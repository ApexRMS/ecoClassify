## -------------------------------
## ecoClassify - MaxEnt Functions
## ApexRMS, November 2024
## -------------------------------

# Default FALSE; set to TRUE by loadModelPackages() when modelType == "MaxEnt"
MAXENT_AVAILABLE <- FALSE

#' Train a Maxent Model with Hyperparameter Tuning ----
#'
#' @description
#' This function trains a Maxent model using the `ENMevaluate` function with optional hyperparameter tuning.
#' It evaluates multiple hyperparameter combinations and selects the best model based on the Continuous Boyce Index (CBI).
#'
#' @param allTrainData A dataframe containing the training data. The dataframe should include a column named 'presence' indicating the target variable.
#' @param nCores An integer specifying the number of cores to use for parallel processing.
#' @param isTuningOn A logical value indicating whether hyperparameter tuning should be performed.
#' @return A list containing the best Maxent model and its variable importance.
#'
#' @details
#' The function first splits the training data into presence and absence data based on the 'presence' column.
#' If `isTuningOn` is TRUE, it evaluates multiple combinations of feature classes (`fc`) and regularization multipliers (`rm`).
#' The best model is selected based on the highest Continuous Boyce Index (CBI). If `isTuningOn` is FALSE, default hyperparameters are used.
#'
#'
#' @import ENMeval
getMaxentModel <- function(allTrainData, nCores, isTuningOn) {
  # Check if MaxEnt dependencies are available
  if (!MAXENT_AVAILABLE) {
    stop(
      "MaxEnt functionality is not available. Please ensure Java is properly configured and both rJava and ENMeval packages are installed."
    )
  }
  # Identify predictor variables
  predictorVars <- grep(
    "presence|kfold",
    colnames(allTrainData),
    invert = TRUE,
    value = TRUE
  )

  # Split into categorical and numeric
  predictorData <- allTrainData[, predictorVars, drop = FALSE]
  cat_vars <- names(predictorData)[sapply(predictorData, is.factor)]
  num_vars <- setdiff(predictorVars, cat_vars)

  ## Specifying feature classes and regularization parameters for Maxent
  if (isTuningOn) {
    tuneArgs <- list(
      fc = c("L", "Q", "P", "LQ", "H", "LQH", "LQHP"),
      rm = seq(0.5, 3, 0.5)
    )
  } else {
    tuneArgs <- list(fc = c("LQH"), rm = 1)
  }

  # Without rasters, ENMevaluate() reads the first two columns of occs/bg as
  # longitude/latitude and fits on the remaining columns only. Prepend
  # placeholder coordinates so every predictor is used. They are unique per
  # row and are not used by the "randomkfold" partitioning.
  addPlaceholderCoordinates <- function(df, offset) {
    cbind(
      data.frame(
        ecoClassifyX = offset + seq_len(nrow(df)),
        ecoClassifyY = 0
      ),
      df
    )
  }
  absenceTrainData <- allTrainData[
    allTrainData$presence == 0, predictorVars, drop = FALSE
  ]
  presenceTrainData <- allTrainData[
    allTrainData$presence == 1, predictorVars, drop = FALSE
  ]
  presenceTrainData <- addPlaceholderCoordinates(presenceTrainData, 0)
  absenceTrainData <- addPlaceholderCoordinates(
    absenceTrainData, nrow(presenceTrainData)
  )

  # Respect the user's core setting, but cap at 2 to limit Java memory usage
  # (each parallel worker starts its own JVM with the -Xmx heap)
  nCores <- min(2L, as.integer(nCores))

  usableCores <- max(1, min(nCores, parallel::detectCores() - 1))

  runEnmevaluate <- function(parallel) {
    tryCatch(
      ENMeval::ENMevaluate(
        occs = presenceTrainData,
        bg = absenceTrainData,
        tune.args = tuneArgs,
        partitions = "randomkfold",
        algorithm = "maxent.jar",
        parallel = parallel,
        numCores = if (parallel) usableCores else 1,
        quiet = TRUE
      ),
      error = function(e) {
        # Argument errors usually mean ENMeval < 2.0, which this code does
        # not support
        if (grepl("unused argument|unexpected.*argument|object.*not found",
                  conditionMessage(e))) {
          updateRunLog(
            "ENMevaluate failed; update the ENMeval package to version 2.0",
            type = "warning"
          )
        }
        stop(e)
      }
    )
  }

  # Run in parallel when more than one core is usable; if that fails, retry
  # once in serial mode
  max1 <- tryCatch(
    runEnmevaluate(parallel = usableCores > 1),
    error = function(e) {
      warning(
        "Parallel Maxent failed. Retrying in serial mode...\n",
        "Original error: ", conditionMessage(e)
      )
      runEnmevaluate(parallel = FALSE)
    }
  )

  bestMax <- which.max(max1@results$cbi.val.avg)
  varImp <- max1@variable.importance[bestMax] %>% data.frame()
  names(varImp) <- c(
    "variable",
    "percent.contribution",
    "permutation.importance"
  )
  maxentImportance <- getMaxentImportance(varImp)

  # Levels of each categorical predictor, in the order ENMevaluate() used to
  # code them (see encodeMaxentCategoricals()); NULL for numeric predictors
  cat_levels <- lapply(
    allTrainData[, predictorVars, drop = FALSE],
    function(x) if (is.factor(x)) levels(x) else NULL
  )

  model <- max1@models[[bestMax]]

  # No prediction closure is stored: it would carry this function's whole
  # environment (all training data and every candidate model) into model.rds.
  # Use predictMaxentProbability() instead.
  return(list(
    model       = model,
    vimp        = maxentImportance,
    cat_vars    = cat_vars,
    num_vars    = num_vars,
    cat_levels  = cat_levels
  ))

}

#' Encode Categorical Predictors as MaxEnt Was Trained ----
#'
#' @description
#' For maxent.jar, `ENMevaluate()` replaces each categorical predictor with
#' its level position (1, 2, 3, ...) in the training factor, while
#' `dismo::predict()` converts factors using their labels (e.g. land-cover
#' classes 11, 21, 41). This converts each categorical column to the training
#' positions, so predictions see the same values the model was trained on.
#' Categories not seen in training become NA (no prediction for that row).
#'
#' @param data Data frame of predictors.
#' @param catLevels Named list of training levels (NULL for numeric
#'   predictors), as saved in the model's `cat_levels`.
#'
#' @return `data` with categorical columns encoded as factors of positions.
#'
#' @noRd
encodeMaxentCategoricals <- function(data, catLevels) {
  catLevels <- Filter(Negate(is.null), catLevels)
  for (v in intersect(names(catLevels), names(data))) {
    trainLevels <- catLevels[[v]]
    positions <- match(as.character(data[[v]]), trainLevels)
    data[[v]] <- factor(positions, levels = seq_along(trainLevels))
  }
  data
}

#' Predict MaxEnt Presence Probability for a Data Frame ----
#'
#' @param model A MaxEnt model list from `getMaxentModel()` (or a saved
#'   model.rds), with `$model` and `$cat_levels`.
#' @param newdata Data frame of predictors.
#'
#' @return Numeric vector of P(presence) in maxent.jar's default output
#'   format (cloglog for MaxEnt 3.4+); NA where a row cannot be predicted.
#'
#' @noRd
predictMaxentProbability <- function(model, newdata) {
  newdata <- as.data.frame(newdata, stringsAsFactors = FALSE)
  newdata <- encodeMaxentCategoricals(newdata, model$cat_levels)

  # Output is maxent.jar's default format (cloglog for MaxEnt 3.4+).
  # dismo::predict() has no `type` argument; passing type = "logistic", as
  # earlier versions did, was silently ignored.
  p1 <- as.numeric(dismo::predict(model$model, newdata))
  p1[!is.finite(p1)] <- NA_real_
  p1
}

#' Predict presence probability using a Maxent model ----
#'
#' @description
#' `predictMaxent` applies a trained Maxent model to a multi-layer raster
#' and produces a continuous probability raster representing the likelihood of presence.
#' It handles categorical predictors by aligning factor levels with those seen during training.
#'
#' @param raster A SpatRaster object containing the input predictor layers.
#' @param model A trained Maxent model object returned by `getMaxentModel()`,
#' including `$model`, `$cat_vars`, and `$cat_levels`.
#' @param filename Character. Output filename. If "", returns in-memory raster.
#' @param memfrac Numeric. Memory fraction for chunking (0-1). Default 0.5.
#'
#' @return A SpatRaster with predicted presence probabilities, with the same extent
#' and resolution as the input raster. NA values are preserved.
#'
#' @details
#' This function uses `terra::predict()` with automatic chunking to process large rasters
#' memory-efficiently. When `filename` is provided, predictions are written directly to
#' disk to minimize memory usage.
#'
#' @noRd
predictMaxent <- function(raster, model, filename = "", memfrac = 0.5) {
  # Check if MaxEnt dependencies are available
  if (!MAXENT_AVAILABLE) {
    stop(
      "MaxEnt functionality is not available. Please ensure Java is properly configured and both rJava and ENMeval packages are installed."
    )
  }

  # prediction function for terra::predict; categorical predictors are
  # encoded as at training time (unseen categories become NA)
  predictFn <- function(m, data, ...) {
    predictMaxentProbability(m, data)
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

#' Extract Maxent Variable Importance ----
#'
#' @description
#' Extracts and formats variable importance from a Maxent model.
#'
#' @param varImp A data.frame with columns 'variable' and 'percent.contribution'.
#'
#' @return A named numeric vector of variable importance values.
#'
#' @noRd
getMaxentImportance <- function(varImp) {
  maxentImportance <- as.numeric(varImp$percent.contribution)
  attr(maxentImportance, "names") <- varImp$variable
  return(maxentImportance)
}
