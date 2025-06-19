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
  predictionRaster <- raster[[1]]
  names(predictionRaster) <- "present"

  rasterMatrix <- terra::values(raster, mat = TRUE)
  valid_idx <- complete.cases(rasterMatrix)
  rasterMatrix <- as.data.frame(rasterMatrix[valid_idx, ])

  # Handle factor levels
  for (var in model$cat_vars) {
    if (!var %in% names(rasterMatrix)) next

    # Safely coerce with unseen handling
    f <- factor(as.character(rasterMatrix[[var]]), levels = model$factor_levels[[var]])
    f <- addNA(f)
    if (!"unseen" %in% levels(f)) {
      levels(f) <- c(levels(f), "unseen")
    }
    f[is.na(f)] <- "unseen"
    rasterMatrix[[var]] <- f
  }

  predictedValues <- data.frame(predict(model$model, data = rasterMatrix))[, 2]

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
  trainingVariables <- grep(
    "presence|kfold",
    colnames(allTrainData),
    invert = TRUE,
    value = TRUE
  )

  # Identify categorical and numeric variables
  cat_vars <- names(allTrainData[, trainingVariables, drop = FALSE])[sapply(allTrainData[, trainingVariables, drop = FALSE], is.factor)]
  num_vars <- setdiff(trainingVariables, cat_vars)

  mainModel <- formula(sprintf(
    "%s ~ %s",
    "presence",
    paste(trainingVariables, collapse = " + ")
  ))

  if (isTuningOn) {
    tuneArgs <- list(
      mtry = seq_len(min(6, length(trainingVariables))),
      maxDepth = seq(0, 1, 0.2),
      nTrees = c(1000, 2000, 3000, 4000, 5000)
    )
    tuneArgsGrid <- expand.grid(tuneArgs)
  } else {
    tuneArgs <- list(
      mtry = round(sqrt(length(trainingVariables)), 0),
      maxDepth = 0,
      nTrees = 2000
    )
    tuneArgsGrid <- expand.grid(tuneArgs)
  }

  registerDoParallel(cores = nCores)

  results <- foreach(
    i = seq_len(nrow(tuneArgsGrid)),
    .combine = rbind,
    .packages = "ranger"
  ) %dopar%
    {
      rf1 <- ranger(
        mainModel,
        data = allTrainData,
        mtry = tuneArgsGrid$mtry[i],
        num.trees = tuneArgsGrid$nTrees[i],
        max.depth = tuneArgsGrid$maxDepth[i],
        probability = TRUE,
        importance = "impurity"
      )

      oobError <- rf1$prediction.error
      modelResults <- tuneArgsGrid[i, ]
      modelResults[, "oobError"] <- oobError
      modelResults
    }

  bestModel <- ranger(
    mainModel,
    data = allTrainData,
    mtry = results[which.min(results$oobError), "mtry"],
    num.trees = results[which.min(results$oobError), "nTrees"],
    max.depth = results[which.min(results$oobError), "maxDepth"],
    num.threads = 1,
    probability = TRUE,
    importance = "impurity"
  )

  factor_levels <- lapply(
    allTrainData[, trainingVariables, drop = FALSE],
    function(x) {
      if (is.factor(x)) levels(x) else NULL
    }
  )

  return(list(
    model = bestModel,
    vimp = bestModel$variable.importance,
    factor_levels = factor_levels,
    cat_vars = cat_vars,
    num_vars = num_vars
  ))
}
