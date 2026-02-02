## -------------------------------
## ecoClassify - MaxEnt Functions
## ApexRMS, November 2024
## -------------------------------

# Load MaxEnt-specific dependencies - skip MaxEnt if Java not available
MAXENT_AVAILABLE <- FALSE

# Try to load rJava
rJavaLoaded <- tryCatch(
  {
    if (!requireNamespace("rJava", quietly = TRUE)) {
      warning(
        "rJava package not available. MaxEnt functionality will be skipped."
      )
      FALSE
    } else {
      suppressPackageStartupMessages(library("rJava", character.only = TRUE))
      TRUE
    }
  },
  error = function(e) {
    warning(
      "rJava failed to load (Java configuration issue). MaxEnt functionality will be skipped."
    )
    FALSE
  }
)

# Try to load ENMeval only if rJava loaded successfully
if (rJavaLoaded) {
  enmevalLoaded <- tryCatch(
    {
      if (!requireNamespace("ENMeval", quietly = TRUE)) {
        warning(
          "ENMeval package not available. MaxEnt functionality will be skipped."
        )
        FALSE
      } else {
        suppressPackageStartupMessages(library(
          "ENMeval",
          character.only = TRUE
        ))
        TRUE
      }
    },
    error = function(e) {
      warning("ENMeval failed to load. MaxEnt functionality will be skipped.")
      FALSE
    }
  )

  MAXENT_AVAILABLE <- enmevalLoaded
}

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

  absenceTrainData <- allTrainData[allTrainData$presence == 0, predictorVars]
  presenceTrainData <- allTrainData[allTrainData$presence == 1, predictorVars]

  # limit java memory usage
  nCores <- min(2, parallel::detectCores() - 1)

  max1 <- tryCatch(
    {
      usableCores <- max(1, min(nCores, parallel::detectCores() - 1))
      useParallel <- usableCores > 1
      ENMevaluate(
        occ = presenceTrainData,
        bg.coords = absenceTrainData,
        tune.args = tuneArgs,
        progbar = FALSE,
        partitions = "randomkfold",
        parallel = useParallel,
        numCores = usableCores,
        quiet = TRUE,
        algorithm = 'maxent.jar'
      )
    },
    error = function(e) {
      warning(
        "Parallel Maxent failed due to memory issue. Retrying in serial mode..."
      )
      ENMevaluate(
        occ = presenceTrainData,
        bg.coords = absenceTrainData,
        tune.args = tuneArgs,
        progbar = FALSE,
        partitions = "randomkfold",
        parallel = FALSE,
        quiet = TRUE,
        algorithm = 'maxent.jar'
      )
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

  # --- factor/level metadata like RF ---
  cat_levels <- lapply(
    allTrainData[, predictorVars, drop = FALSE],
    function(x) if (is.factor(x)) levels(x) else NULL
  )

  # --- wrapper: consistent 2-col prob output ---
  predictMaxentDataframe <- function(mxModel, newdata) {
    # align factor levels for any categorical predictors seen at train-time
    if (length(cat_levels)) {
      for (v in names(cat_levels)) {
        if (v %in% names(newdata)) {
          lv <- cat_levels[[v]]
          if (!is.null(lv)) {
            # coerce to character first to avoid level drop warnings
            f <- factor(as.character(newdata[[v]]), levels = lv)
            # unseen → NA (MaxEnt ignores; alternative is to set first level)
            newdata[[v]] <- f
          }
        }
      }
    }

    # ENMeval/MaxEnt logistic output is a numeric vector of P(presence)
    p1 <- tryCatch(
      as.numeric(predict(mxModel, newdata, type = "logistic")),
      error = function(e) {
        # some MaxEnt builds require 'dismo::predict' signature
        as.numeric(dismo::predict(mxModel, newdata, args = "logistic"))
      }
    )

    p1[!is.finite(p1)] <- NA_real_
    cbind(absence = 1 - p1, presence = p1)
  }

  model <- max1@models[[bestMax]]

  return(list(
    model       = model,
    vimp        = maxentImportance,
    cat_vars    = cat_vars,
    num_vars    = num_vars,
    cat_levels  = cat_levels,
    predict_df  = predictMaxentDataframe
  ))

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

  # prediction function for terra::predict
  predictFn <- function(m, data, ...) {
    # Handle categorical variables if present
    cat_vars_present <- intersect(names(m$cat_levels), names(data))

    if (length(cat_vars_present) > 0) {
      # Work on a local copy
      data <- as.data.frame(data, stringsAsFactors = FALSE)

      for (var in cat_vars_present) {
        var_levels <- m$cat_levels[[var]]
        if (!is.null(var_levels)) {
          # Convert to factor, coercing unseen levels to NA
          f <- factor(as.character(data[[var]]), levels = var_levels)
          data[[var]] <- f
        }
      }
    }

    # MaxEnt logistic prediction
    p1 <- tryCatch(
      as.numeric(predict(m$model, data, type = "logistic")),
      error = function(e) {
        # some MaxEnt builds require 'dismo::predict' signature
        as.numeric(dismo::predict(m$model, data, args = "logistic"))
      }
    )

    # Clean invalid values
    p1[!is.finite(p1)] <- NA_real_

    return(as.numeric(p1))
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
