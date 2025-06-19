## -------------------------------
## ecoClassify - Post-procesing Functions
## ApexRMS, November 2024
## -------------------------------

#' Filter prediction raster ----
#'
#' @description
#' 'filterFun' filters out presence cells in input raster based on the classification
#' of surrounding cells.
#'
#' @param raster prediction raster to filter (spatRaster)
#' @param resolution resolution to apply filtering (numeric)
#' @param percent threshold for filtering (numeric)
#' @return filtered raster (spatRaster)
#'
#' @details
#' Used in generateRasterDataframe wrapper function if filtering is selected.
#' @noRd
filterFun <- function(raster, resolution, percent) {
  # define parameters
  npixels <- resolution^2
  midPixel <- (npixels + 1) / 2
  threshold <- round(npixels * percent, 0)

  # filter
  if (is.na(raster[midPixel])) {
    return(NA)
  } else if (
    raster[midPixel] == 1 &&
      sum(raster[-midPixel] == 0, na.rm = TRUE) > threshold
  ) {
    return(0)
  } else {
    return(raster[midPixel])
  }
}

#' Compute classification metrics and generate confusion matrix plot ----
#'
#' @description
#' `calculateStatistics` evaluates model performance on test data by generating
#' predicted probabilities, classifying outcomes based on a threshold, and computing
#' classification metrics and a formatted confusion matrix.
#'
#' @param model A model object, such as the output from `getRandomForestModel()`,
#' `getMaxentModel()`, or `getCNNModel()`; must contain a `$model` element.
#' @param testData A dataframe containing test predictors and a `presence` column
#' with true class labels (0/1).
#' @param threshold Numeric threshold between 0 and 1 used to convert predicted probabilities
#' into binary presence/absence classes.
#' @param confusionOutputDataFrame An existing dataframe to which confusion matrix results
#' will be appended.
#' @param modelOutputDataFrame An existing dataframe to which accuracy, precision, recall,
#' and other metrics will be appended.
#'
#' @return A list with three components:
#' \describe{
#'   \item{[[1]]}{Updated confusionOutputDataFrame including prediction/reference counts.}
#'   \item{[[2]]}{Updated modelOutputDataFrame with classification metrics (accuracy, sensitivity, etc.).}
#'   \item{[[3]]}{A ggplot2 object containing a visual confusion matrix plot.}
#' }
#'
#' @details
#' The function supports Random Forest (ranger), MaxEnt (maxnet), and CNN (torch) models.
#' Predictions are converted to binary classifications using the provided threshold.
#' Outputs are suitable for export to SyncroSim `ConfusionMatrix` and `ModelStatistics` datasheets.
#' A high-resolution `ggplot` visualization of the confusion matrix is returned for reporting.
#' @noRd
calculateStatistics <- function(
  model,
  testData,
  threshold,
  confusionOutputDataFrame,
  modelOutputDataFrame
) {
  # Predict probabilities
  if (inherits(model[[1]], "ranger")) {
    probs <- predict(model[[1]], testData)$predictions[, 2]
  } else if (inherits(model[[1]], "torchCNN")) {
    probs <- predictCNN(model, testData, isRaster = FALSE)
  } else {
    probs <- predict(model[[1]], testData, type = "logistic")
  }

  # Binary classification based on threshold
  predicted <- ifelse(probs >= threshold, 1, 0)

  # Prepare evaluation data
  evalData <- tibble(
    truth = factor(testData$presence, levels = c(0, 1)),
    prediction = factor(predicted, levels = c(0, 1))
  )

  # Confusion matrix
  confMatrix <- conf_mat(evalData, truth = truth, estimate = prediction)

  # Format confusion matrix table
  confusionMatrix <- as.data.frame(confMatrix$table)
  colnames(confusionMatrix) <- c("Prediction", "Reference", "Frequency")

  # Collect metrics
  metricsData <- bind_rows(
    accuracy(evalData, truth, prediction),
    sens = sensitivity(evalData, truth, prediction),
    spec = specificity(evalData, truth, prediction),
    precision(evalData, truth, prediction),
    recall(evalData, truth, prediction),
    f_meas(evalData, truth, prediction)
  ) %>%
    select(.metric, .estimate) %>%
    rename(Statistic = .metric, Value = .estimate)

  # Append to output dataframes
  confusionOutputDataFrame <- addRow(confusionOutputDataFrame, confusionMatrix)
  modelOutputDataFrame <- addRow(modelOutputDataFrame, metricsData)

  # Confusion matrix plot
  confusionMatrixPlot <- plot_confusion_matrix(
    confusionMatrix,
    target_col = "Reference",
    prediction_col = "Prediction",
    counts_col = "Frequency",
    font_counts = font(size = 15),
    font_normalized = font(size = 6),
    font_row_percentages = font(size = 6),
    font_col_percentages = font(size = 6)
  ) +
    ggplot2::theme(
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 25)
    )

  return(list(
    confusionOutputDataFrame,
    modelOutputDataFrame,
    confusionMatrixPlot
  ))
}

#' Plot variable importance ----
#'
#' @description
#' 'plotVariableImportance' creates and writes variable importance plot
#' from the trained model
#'
#' @param importanceData vector of importance values (numeric) with names attribute
#' @param transferDir filepath for exporting the plot
#' @return variable importance plot (ggplot) and dataframe with filepath
#' to where the plot was written
#'
#' @details
#' transferDir parameter is defined based on the ssim session.
#' @noRd
plotVariableImportance <- function(importanceData, transferDir) {
  if (is.null(names(importanceData))) {
    stop("`importanceData` must be a named numeric vector.")
  }
  df <- tibble::tibble(
    variable = names(importanceData),
    value = as.numeric(importanceData)
  )

  n_vars <- nrow(df)

  # Adjust font size and image height based on number of variables
  font_size <- if (n_vars <= 10) 20 else if (n_vars <= 20) 18 else if (
    n_vars <= 40
  )
    16 else 14
  plot_height <- max(4, min(10, 0.3 * n_vars)) # height scales with variable count, capped at 10 in

  p <- ggplot2::ggplot(
    df,
    aes(
      x = reorder(variable, value),
      y = value,
      fill = value
    )
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Variable",
      y = "Variable Importance",
      title = "Information Value Summary"
    ) +
    ggplot2::theme_classic(base_size = font_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
      ),
      plot.title.position = "plot"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = "#424352",
      guide = "none"
    )

  outfile <- file.path(transferDir, "VariableImportance.png")
  ggplot2::ggsave(
    filename = outfile,
    plot = p,
    width = 7,
    height = plot_height,
    units = "in",
    dpi = 300
  )

  return(list(
    plot = p,
    dataFrame = data.frame(
      VariableImportance = outfile,
      stringsAsFactors = FALSE
    )
  ))
}
