## -------------------------------
## ecoClassify - Post-procesing Functions
## ApexRMS, November 2024
## -------------------------------

#' Filter prediction raster ----
#'
#' @description
#' Applies binary presence/absence filtering to a prediction raster. Can filter
#' isolated presence cells and/or fill absence cells surrounded by presence.
#'
#' @param r Input prediction raster (SpatRaster)
#' @param filterValue Minimum neighbor count to retain presence cells (numeric, optional)
#' @param fillValue Minimum neighbor count to fill absence cells (numeric, optional)
#'
#' @return Filtered and/or filled raster (SpatRaster)
#'
#' @details
#' Used in generateRasterDataframe wrapper function if filtering is selected.
#' Applies `filterValue` to remove isolated presence cells, and `fillValue` to
#' convert central absence pixels to presence if surrounded by nearby presence cells.
#'
#' @noRd

filterPredictionRaster <- function(r, filterValue = NULL, fillValue = NULL) {
  rBin <- classify(
    r,
    matrix(c(-Inf, 0.5, 0, 0.5, Inf, 1), ncol = 3, byrow = TRUE)
  )

  w <- matrix(1, 3, 3)
  w[2, 2] <- 0

  if (!is.null(filterValue)) {
    neighborCount <- focal(
      rBin,
      w = w,
      fun = sum,
      na.policy = "omit",
      filename = "",
      overwrite = TRUE
    )
    rBin <- ifel(neighborCount >= filterValue, rBin, 0)
  }

  if (!is.null(fillValue)) {
    neighborSum <- focal(
      rBin,
      w = w,
      fun = sum,
      na.policy = "omit",
      filename = "",
      overwrite = TRUE
    )
    rBin <- ifel(rBin == 0 & neighborSum >= fillValue, 1, rBin)
  }

  return(rBin)
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

  # Sort by importance
  df <- dplyr::arrange(df, desc(abs(value)))

  # Limit number of variables shown (adjustable)
  max_display_vars <- 40
  df_display <- if (nrow(df) > max_display_vars) {
    df[1:max_display_vars, ]
  } else {
    df
  }

  n_vars <- nrow(df_display)

  # Adjust font size and height based on number of vars shown
  font_size <- if (n_vars <= 10) {
    20
  } else if (n_vars <= 20) {
    18
  } else if (n_vars <= 30) {
    16
  } else {
    14
  }
  plot_height <- max(4, min(10, 0.3 * n_vars))

  # Build plot
  p <- ggplot2::ggplot(
    df_display,
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

  # Save to file
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

#' Filter predicted presence raster and write to file ----
#'
#' @description
#' 'filterRasterDataframe' filters a predicted presence raster based on
#' spatial neighborhood and writes the filtered raster to file.
#' It returns a dataframe containing the path to the written file.
#'
#' @param applyFiltering logical. Whether filtering should be applied.
#' @param predictedPresence a SpatRaster object representing predicted presence.
#' @param filterValue numeric. Minimum number of neighboring presence cells required to retain a presence cell.
#' @param fillValue numeric. Value to assign to removed presence pixels.
#' @param category character. Label used to define output filename.
#' @param timestep integer or character. Time step identifier used in the output filename.
#' @param transferDir filepath. Directory for writing the filtered raster file.
#'
#' @return A dataframe with columns:
#' \itemize{
#'   \item \code{Timestep}: the timestep value.
#'   \item \code{PredictedFiltered}: full filepath of the filtered raster file.
#' }
#'
#' @details
#' When \code{applyFiltering} is \code{FALSE}, the function skips raster filtering and
#' returns a dataframe with \code{NA} in the \code{PredictedFiltered} column.
#' Otherwise, the function filters the raster using a spatial neighborhood-based
#' algorithm (via \code{filterPredictionRaster}), writes the result to a GeoTIFF file,
#' and returns its location.
#'
#' The output filename follows the pattern:
#' \code{filteredPredictedPresence-<category>-t<timestep>.tif}
#'
#' @noRd
filterRasterDataframe <- function(
  applyFiltering,
  predictedPresence,
  filterValue,
  fillValue,
  category,
  timestep,
  transferDir
) {
  if (!applyFiltering) {
    return(data.frame(
      Timestep = timestep,
      PredictedFiltered = NA_character_
    ))
  }

  # Filter out presence pixels surrounded by non-presence
  filteredPredictedPresence <- filterPredictionRaster(
    predictedPresence,
    filterValue = filterValue,
    fillValue = fillValue
  )

  # File path
  filteredPath <- file.path(paste0(
    transferDir,
    "/filteredPredictedPresence-",
    category,
    "-t",
    timestep,
    ".tif"
  ))

  # Save raster
  writeRaster(
    filteredPredictedPresence,
    filename = filteredPath,
    overwrite = TRUE
  )

  # Build dataframe
  rasterDataframe <- data.frame(
    Timestep = timestep,
    PredictedFiltered = filteredPath
  )

  return(rasterDataframe)
}
