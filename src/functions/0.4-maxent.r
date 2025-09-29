## -------------------------------
## ecoClassify - MaxEnt Functions
## ApexRMS, November 2024
## -------------------------------

# Load MaxEnt-specific dependencies
if (!requireNamespace("rJava", quietly = TRUE)) {
  install.packages("rJava", repos = 'http://cran.us.r-project.org')
}
suppressPackageStartupMessages(library(rJava, character.only = TRUE))

if (!requireNamespace("ENMeval", quietly = TRUE)) {
  install.packages("ENMeval", repos = 'http://cran.us.r-project.org')
}
suppressPackageStartupMessages(library(ENMeval, character.only = TRUE))

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
  # Identify predictor variables
  predictorVars <- grep("presence|kfold", colnames(allTrainData), invert = TRUE, value = TRUE)

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

  absenceTrainData <- allTrainData[
    allTrainData$presence == 0,
    predictorVars
  ]
  presenceTrainData <- allTrainData[
    allTrainData$presence == 1,
    predictorVars
  ]

  # limit java memory usage
  nCores <- min(2, parallel::detectCores() - 1)

  max1 <- tryCatch({
    ENMevaluate(
      occ = presenceTrainData,
      bg.coords = absenceTrainData,
      tune.args = tuneArgs,
      progbar = FALSE,
      partitions = "randomkfold",
      parallel = TRUE,
      numCores = nCores,
      quiet = TRUE,
      algorithm = 'maxent.jar'
  )}, error = function(e) {
    warning("Parallel Maxent failed due to memory issue. Retrying in serial mode...")
    ENMevaluate(
      occ = presenceTrainData,
      bg.coords = absenceTrainData,
      tune.args = tuneArgs,
      progbar = FALSE,
      partitions = "randomkfold",
      parallel = FALSE,
      numCores = nCores,
      quiet = TRUE,
      algorithm = 'maxent.jar'
    )
  })

  bestMax <- which.max(max1@results$cbi.val.avg)
  varImp <- max1@variable.importance[bestMax] %>% data.frame()
  names(varImp) <- c(
    "variable",
    "percent.contribution",
    "permutation.importance"
  )
  maxentImportance <- getMaxentImportance(varImp)

  model <- max1@models[[bestMax]]

  return(list(
    model = model,
    vimp = maxentImportance,
    cat_vars = cat_vars,
    num_vars = num_vars
  ))
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
