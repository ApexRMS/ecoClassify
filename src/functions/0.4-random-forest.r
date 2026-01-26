## -------------------------------
## ecoClassify - Random Forest Functions
## ApexRMS, November 2024
## -------------------------------

#' Predict presence probability using a Random Forest model ----
#'
#' @description
#' `predictRanger` applies a trained Random Forest model to a multi-layer raster
#' and produces a continuous probability raster representing the likelihood of presence.
#' It handles categorical predictors robustly by aligning factor levels with those seen
#' during model training.
#'
#' @param raster A SpatRaster object containing the input predictor layers.
#' @param model A trained Random Forest model object returned by `getRandomForestModel()`,
#' including `$model`, `$cat_vars`, and `$factor_levels`.
#'
#' @return A SpatRaster with predicted presence probabilities, with the same extent
#' and resolution as the input raster. NA values are preserved.
#'
#' @details
#' The function first extracts all raster values as a matrix and removes rows with NA values.
#' Categorical variables are coerced into factors using levels stored in the model object.
#' Unseen levels are safely handled by introducing a synthetic `"unseen"` level.
#'
#' The trained `ranger` model is then used to predict probabilities of presence for
#' each valid cell. Predicted values are inserted back into the original raster structure.
#'
#' This function is typically called within `getPredictionRasters()` to generate prediction maps
#' for full raster extents.
#' @noRd
predictRanger <- function(raster, model, filename = "", memfrac = 0.7) {
  # Validate that raster has all required variables
  model_vars <- c(model$cat_vars, model$num_vars)
  raster_vars <- names(raster)
  missing_vars <- setdiff(model_vars, raster_vars)

  if (length(missing_vars) > 0) {
    stop(
      "Raster is missing required variables for prediction.\n",
      "Missing: ", paste(missing_vars, collapse = ", "), "\n",
      "Expected: ", paste(model_vars, collapse = ", "), "\n",
      "Found: ", paste(raster_vars, collapse = ", ")
    )
  }

  # prediction function for terra::predict
  predictFn <- function(m, data, ...) {
    # m is the model object passed in by terra::predict (same as 'model' in outer scope)
    # Handle categorical variables
    cat_vars_present <- intersect(m$cat_vars, names(data))

    if (length(cat_vars_present) > 0) {
      # Work on a local copy so we don't touch terra's internal object
      data <- as.data.frame(data, stringsAsFactors = FALSE)

      for (var in cat_vars_present) {
        var_levels <- m$factor_levels[[var]]

        # Add an "unseen" level for unknown categories
        all_levels <- c(var_levels, "unseen")
        f <- factor(as.character(data[[var]]), levels = all_levels)
        f[is.na(f)] <- "unseen"

        data[[var]] <- f
      }
    }

    # ranger prediction: force single thread to avoid per-thread copies
    preds <- predict(m$model, data = data, num.threads = 1, verbose = FALSE)

    # Extract probabilities (second column)
    result <- if (is.data.frame(preds)) {
      preds[, 2]
    } else if (is.list(preds) && "predictions" %in% names(preds)) {
      preds$predictions[, 2]
    } else {
      preds[, 2]
    }

    as.numeric(result)
  }

  # terra::predict handles chunking; memfrac controls block size.
  # filename = "" => in-memory; set a path to write to disk.
  predictionRaster <- terra::predict(
    raster,
    model = model,
    fun = predictFn,
    na.rm = TRUE,
    cores = 1,
    memfrac = memfrac,
    filename = filename
  )

  names(predictionRaster) <- "present"
  predictionRaster
}

#' Train a Random Forest Model with Hyperparameter Tuning ----
#'
#' @description
#' This function trains a random forest model using the `ranger` package with optional hyperparameter tuning.
#' It evaluates multiple hyperparameter combinations in parallel and selects the best model based on the Out-of-Bag (OOB) error.
#'
#' @param allTrainData A dataframe containing the training data. The dataframe should include a column named 'presence' indicating the target variable.
#' @param nCores An integer specifying the number of cores to use for parallel processing.
#' @param isTuningOn A logical value indicating whether hyperparameter tuning should be performed.
#' @return A list containing the best random forest model and its variable importance.
#'
#' @details
#' The function first constructs a formula for the random forest model using all columns in `allTrainData` except 'presence' and 'kfold'.
#' If `isTuningOn` is TRUE, it evaluates multiple combinations of hyperparameters (`mtry`, `maxDepth`, and `nTrees`) in parallel using the `foreach` and `doParallel` packages.
#' The best model is selected based on the lowest OOB error. If `isTuningOn` is FALSE, default hyperparameters are used.
#'
#' @import ranger
#' @import foreach
#' @import doParallel
#' @export
getRandomForestModel <- function(allTrainData, nCores, isTuningOn) {
  # ------------------- prep -------------------
  trainingVariables <- grep(
    "presence|kfold",
    colnames(allTrainData),
    invert = TRUE,
    value = TRUE
  )

  cat_vars <- names(allTrainData[, trainingVariables, drop = FALSE])[sapply(
    allTrainData[, trainingVariables, drop = FALSE],
    is.factor
  )]
  num_vars <- setdiff(trainingVariables, cat_vars)

  if (!is.factor(allTrainData$presence)) {
    allTrainData$presence <- factor(
      allTrainData$presence,
      levels = c(0, 1),
      labels = c("absence", "presence")
    )
  }

  df <- allTrainData[, c("presence", trainingVariables), drop = FALSE]
  p <- length(trainingVariables)

  # ------------------- tuning grid -------------------
  mtry_center <- max(1L, round(sqrt(p)))
  mtry_grid <- sort(unique(pmax(
    1L,
    round(c(mtry_center * c(0.5, 0.75, 1, 1.25, 1.5)))
  )))
  maxDepth_grid <- c(0L, 6L, 12L, 18L) # 0 = unlimited
  minNode_grid <- c(1L, 5L, 10L, 20L)
  trees_stage1 <- if (isTuningOn) 300L else 1000L
  trees_stage2 <- if (isTuningOn) 1500L else 2000L
  bestK <- 5L

  # ------------------- optional subsample for tuning -------------------
  tune_nmax <- if (isTuningOn) min(nrow(df), 100000L) else nrow(df)
  if (tune_nmax < nrow(df)) {
    set.seed(1)
    idx0 <- which(df$presence == "absence")
    idx1 <- which(df$presence == "presence")
    k0 <- round(as.numeric(tune_nmax) * length(idx0) / nrow(df))
    k1 <- tune_nmax - k0
    tune_idx <- c(
      sample(idx0, min(k0, length(idx0))),
      sample(idx1, min(k1, length(idx1)))
    )
    df_tune <- df[tune_idx, , drop = FALSE]
  } else {
    df_tune <- df
  }

  # ------------------- build candidate set -------------------
  if (isTuningOn) {
    tuneArgsGrid <- expand.grid(
      mtry = mtry_grid,
      maxDepth = maxDepth_grid,
      minNode = minNode_grid,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    tuneArgsGrid <- data.frame(
      mtry = mtry_center,
      maxDepth = 0L,
      minNode = 5L,
      stringsAsFactors = FALSE
    )
  }

  # ------------------- stage 1: cheap screening -------------------
  if (isTuningOn) {
    doParallel::registerDoParallel(cores = nCores)
    on.exit(doParallel::stopImplicitCluster(), add = TRUE)

    results1 <- foreach::foreach(
      i = seq_len(nrow(tuneArgsGrid)),
      .combine = rbind,
      .packages = "ranger"
    ) %dopar%
      {
        g <- tuneArgsGrid[i, ]
        rf1 <- ranger::ranger(
          dependent.variable.name = "presence",
          data = df_tune,
          mtry = g$mtry,
          num.trees = trees_stage1,
          max.depth = g$maxDepth,
          min.node.size = g$minNode,
          classification = TRUE,
          probability = FALSE, # faster for OOB error
          importance = "none",
          write.forest = FALSE,
          num.threads = 1 # avoid nested parallelism
        )
        data.frame(
          mtry = g$mtry,
          maxDepth = g$maxDepth,
          minNode = g$minNode,
          oobError = rf1$prediction.error,
          stringsAsFactors = FALSE
        )
      }

    ord <- order(results1$oobError, decreasing = FALSE)
    topK <- head(results1[ord, , drop = FALSE], bestK)
  } else {
    topK <- tuneArgsGrid
  }

  # ------------------- stage 2: refit top-K on full data -------------------
  if (nrow(topK) > 1) {
    results2 <- foreach::foreach(
      i = seq_len(nrow(topK)),
      .combine = rbind,
      .packages = "ranger"
    ) %dopar%
      {
        g <- topK[i, ]
        rf2 <- ranger::ranger(
          dependent.variable.name = "presence",
          data = df,
          mtry = g$mtry,
          num.trees = trees_stage2,
          max.depth = g$maxDepth,
          min.node.size = g$minNode,
          classification = TRUE,
          probability = FALSE,
          importance = "none",
          write.forest = FALSE,
          num.threads = 1
        )
        data.frame(
          mtry = g$mtry,
          maxDepth = g$maxDepth,
          minNode = g$minNode,
          oobError = rf2$prediction.error,
          stringsAsFactors = FALSE
        )
      }

    best_idx <- which.min(results2$oobError)
    best_hyp <- results2[best_idx, ]
  } else {
    best_hyp <- topK
  }

  # ------------------- final model (compute importance once) -------------------
  bestModel <- ranger::ranger(
    dependent.variable.name = "presence",
    data = df,
    mtry = best_hyp$mtry,
    num.trees = max(2000L, trees_stage2),
    max.depth = best_hyp$maxDepth,
    min.node.size = best_hyp$minNode,
    classification = TRUE,
    probability = TRUE, # needed downstream
    importance = "impurity", # compute once here
    write.forest = TRUE,
    num.threads = nCores # safe: no outer parallel work now
  )

  # ------------------- metadata for downstream prediction -------------------
  factor_levels <- lapply(
    allTrainData[, trainingVariables, drop = FALSE],
    function(x) if (is.factor(x)) levels(x) else NULL
  )

  list(
    model = bestModel,
    vimp = bestModel$variable.importance,
    factor_levels = factor_levels,
    cat_vars = cat_vars,
    num_vars = num_vars
  )
}
