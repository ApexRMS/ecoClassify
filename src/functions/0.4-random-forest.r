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
predictRanger <- function(raster, model) {
  # Pre-allocate output raster
  predictionRaster <- raster[[1]]
  names(predictionRaster) <- "present"
  rasterMatrix <- terra::values(raster, mat = TRUE, na.rm = FALSE)

  # Find valid cases once
  valid_idx <- complete.cases(rasterMatrix)
  n_valid <- sum(valid_idx)

  if (n_valid == 0) {
    return(predictionRaster) # Return empty raster if no valid data
  }

  # Subset to valid cases only
  validMatrix <- rasterMatrix[valid_idx, , drop = FALSE]

  # Convert to data.frame only for valid data
  validDF <- as.data.frame(validMatrix)

  # Optimize factor handling - only process categorical variables that exist
  cat_vars_present <- intersect(model$cat_vars, names(validDF))

  if (length(cat_vars_present) > 0) {
    # Vectorized factor processing
    for (var in cat_vars_present) {
      var_levels <- model$factor_levels[[var]]

      # Fast factor creation with unseen level handling
      char_vals <- as.character(validDF[[var]])

      # Create factor with original levels + unseen
      all_levels <- c(var_levels, "unseen")
      f <- factor(char_vals, levels = all_levels)

      # Set unseen values (NAs from factor creation) to "unseen"
      f[is.na(f)] <- "unseen"

      validDF[[var]] <- f
    }
  }

  # Make prediction on valid data only
  predictions <- predict(model$model, data = validDF)

  # Handle different ranger prediction output formats
  if (is.data.frame(predictions)) {
    predictedValues <- predictions[, 2]
  } else if (is.list(predictions) && "predictions" %in% names(predictions)) {
    predictedValues <- predictions$predictions[, 2]
  } else {
    predictedValues <- predictions[, 2]
  }

  # Assign predictions back to raster efficiently
  predictionRaster[valid_idx] <- predictedValues

  return(predictionRaster)
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
  # Expect tictoc loaded by caller; feel free to clear log before calling:
  tictoc::tic.clearlog()

  # ------------------- prep -------------------
  tic("Identify predictor columns")
  trainingVariables <- grep("presence|kfold", colnames(allTrainData), invert = TRUE, value = TRUE)
  toc(log = TRUE, quiet = TRUE)

  tic("Detect categorical / numeric vars")
  cat_vars <- names(allTrainData[, trainingVariables, drop = FALSE])[sapply(
    allTrainData[, trainingVariables, drop = FALSE], is.factor)]
  num_vars <- setdiff(trainingVariables, cat_vars)
  toc(log = TRUE, quiet = TRUE)

  tic("Coerce presence to factor")
  if (!is.factor(allTrainData$presence)) {
    allTrainData$presence <- factor(allTrainData$presence, levels = c(0, 1),
                                    labels = c("absence", "presence"))
  }
  toc(log = TRUE, quiet = TRUE)

  tic("Build model frame (no formula)")
  df <- allTrainData[, c("presence", trainingVariables), drop = FALSE]
  p  <- length(trainingVariables)
  toc(log = TRUE, quiet = TRUE)

  # ------------------- tuning grid -------------------
  tic("Construct tuning grid")
  mtry_center   <- max(1L, round(sqrt(p)))
  mtry_grid     <- sort(unique(pmax(1L, round(c(mtry_center * c(0.5, 0.75, 1, 1.25, 1.5))))))
  maxDepth_grid <- c(0L, 6L, 12L, 18L)   # 0 = unlimited
  minNode_grid  <- c(1L, 5L, 10L, 20L)
  trees_stage1  <- if (isTuningOn) 300L  else 1000L
  trees_stage2  <- if (isTuningOn) 1500L else 2000L
  bestK         <- 5L
  toc(log = TRUE, quiet = TRUE)

  # ------------------- optional subsample for tuning -------------------
  tic("Create stratified subsample for tuning")
  tune_nmax <- if (isTuningOn) min(nrow(df), 100000L) else nrow(df)
  if (tune_nmax < nrow(df)) {
    set.seed(1)
    idx0 <- which(df$presence == "absence")
    idx1 <- which(df$presence == "presence")
    k0 <- round(tune_nmax * length(idx0) / nrow(df))
    k1 <- tune_nmax - k0
    tune_idx <- c(sample(idx0, min(k0, length(idx0))), sample(idx1, min(k1, length(idx1))))
    df_tune <- df[tune_idx, , drop = FALSE]
  } else {
    df_tune <- df
  }
  toc(log = TRUE, quiet = TRUE)

  # ------------------- build candidate set -------------------
  tic("Expand tuning grid")
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
      mtry = mtry_center, maxDepth = 0L, minNode = 5L, stringsAsFactors = FALSE
    )
  }
  toc(log = TRUE, quiet = TRUE)

  # ------------------- stage 1: cheap screening -------------------
  if (isTuningOn) {
    tic("Register parallel backend")
    doParallel::registerDoParallel(cores = nCores)
    on.exit(doParallel::stopImplicitCluster(), add = TRUE)
    toc(log = TRUE, quiet = TRUE)

    tic("Stage-1 tuning (cheap trees; no importance; single thread per fit)")
    results1 <- foreach::foreach(
      i = seq_len(nrow(tuneArgsGrid)), .combine = rbind, .packages = "ranger"
    ) %dopar% {
      g <- tuneArgsGrid[i, ]
      rf1 <- ranger::ranger(
        dependent.variable.name = "presence",
        data          = df_tune,
        mtry          = g$mtry,
        num.trees     = trees_stage1,
        max.depth     = g$maxDepth,
        min.node.size = g$minNode,
        classification = TRUE,
        probability   = FALSE,     # faster for OOB error
        importance    = "none",
        write.forest  = FALSE,
        num.threads   = 1          # avoid nested parallelism
      )
      data.frame(
        mtry = g$mtry, maxDepth = g$maxDepth, minNode = g$minNode,
        oobError = rf1$prediction.error,
        stringsAsFactors = FALSE
      )
    }
    toc(log = TRUE, quiet = TRUE)

    tic("Select top-K configs by OOB error")
    ord  <- order(results1$oobError, decreasing = FALSE)
    topK <- head(results1[ord, , drop = FALSE], bestK)
    toc(log = TRUE, quiet = TRUE)
  } else {
    topK <- tuneArgsGrid
  }

  # ------------------- stage 2: refit top-K on full data -------------------
  if (nrow(topK) > 1) {
    tic("Stage-2 tuning on full data (still no importance)")
    results2 <- foreach::foreach(
      i = seq_len(nrow(topK)), .combine = rbind, .packages = "ranger"
    ) %dopar% {
      g <- topK[i, ]
      rf2 <- ranger::ranger(
        dependent.variable.name = "presence",
        data          = df,
        mtry          = g$mtry,
        num.trees     = trees_stage2,
        max.depth     = g$maxDepth,
        min.node.size = g$minNode,
        classification = TRUE,
        probability   = FALSE,
        importance    = "none",
        write.forest  = FALSE,
        num.threads   = 1
      )
      data.frame(
        mtry = g$mtry, maxDepth = g$maxDepth, minNode = g$minNode,
        oobError = rf2$prediction.error,
        stringsAsFactors = FALSE
      )
    }
    toc(log = TRUE, quiet = TRUE)

    tic("Pick best hyperparameters (min OOB error)")
    best_idx <- which.min(results2$oobError)
    best_hyp <- results2[best_idx, ]
    toc(log = TRUE, quiet = TRUE)
  } else {
    best_hyp <- topK
  }

  # ------------------- final model (compute importance once) -------------------
  tic("Train final model with probabilities & importance")
  bestModel <- ranger::ranger(
    dependent.variable.name = "presence",
    data          = df,
    mtry          = best_hyp$mtry,
    num.trees     = max(2000L, trees_stage2),
    max.depth     = best_hyp$maxDepth,
    min.node.size = best_hyp$minNode,
    classification = TRUE,
    probability   = TRUE,          # needed downstream
    importance    = "impurity",    # compute once here
    write.forest  = TRUE,
    num.threads   = nCores         # safe: no outer parallel work now
  )
  toc(log = TRUE, quiet = TRUE)

  # ------------------- metadata for downstream prediction -------------------
  tic("Capture factor levels for categorical predictors")
  factor_levels <- lapply(
    allTrainData[, trainingVariables, drop = FALSE],
    function(x) if (is.factor(x)) levels(x) else NULL
  )
  toc(log = TRUE, quiet = TRUE)

  # Optionally print the timing log from inside the function:
  print(tictoc::tic.log())

  list(
    model = bestModel,
    vimp  = bestModel$variable.importance,
    factor_levels = factor_levels,
    cat_vars = cat_vars,
    num_vars = num_vars
  )
}
