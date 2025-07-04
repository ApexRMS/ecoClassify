## -------------------------------
## ecoClassify - Histogram Functions
## ApexRMS, November 2024
## -------------------------------

#' Get Raster Layer Value Range ----
#'
#' @description
#' Samples raster values to compute a value range histogram.
#'
#' @param rasterList List of SpatRasters.
#' @param layerName Character; name of the layer.
#' @param nBins Number of bins.
#' @param nSample Number of values to sample.
#'
#' @return A dataframe of histogram bins and percentages.
#'
#' @noRd
getValueRange <- function(rasterList, layerName, nBins = 20, nSample = 5000) {
  mins <- vapply(
    rasterList,
    function(r) terra::global(r[[layerName]], "min", na.rm = TRUE)[1, 1],
    numeric(1)
  )
  maxs <- vapply(
    rasterList,
    function(r) terra::global(r[[layerName]], "max", na.rm = TRUE)[1, 1],
    numeric(1)
  )
  rastMin <- min(mins, na.rm = TRUE)
  rastMax <- max(maxs, na.rm = TRUE)

  brks <- seq(rastMin, rastMax, length.out = nBins + 1)

  ## sample each raster’s layer, flatten into one vector
  perRaster <- ceiling(nSample / length(rasterList))
  sampVals <- unlist(
    lapply(rasterList, function(r) {
      as.vector(spatSample(r[[layerName]], perRaster, na.rm = TRUE))
    }),
    use.names = FALSE
  )

  ## Calc histogram
  h <- hist(sampVals, breaks = brks, plot = FALSE)

  ## return a clean data.frame
  data.frame(
    layer = layerName,
    bin_lower = head(h$breaks, -1),
    bin_upper = tail(h$breaks, -1),
    count = h$counts,
    pct = h$counts / sum(h$counts)
  )
}

#' Get Histograms for Raster Layers ----
#'
#' @description
#' Computes value distributions for numeric raster layers.
#'
#' @param rasterList List of SpatRasters.
#' @param nBins Number of bins.
#' @param nSample Number of samples per raster.
#'
#' @return A combined dataframe of histograms for all layers.
#'
#' @noRd
getRastLayerHistogram <- function(
  rasterList,
  model,
  nBins = 20,
  nSample = 10000
) {
  first_raster <- rasterList[[1]]
  layerNames <- names(first_raster)
  if (is.null(layerNames) || length(layerNames) == 0) {
    stop("rasterList[[1]] must have at least one named layer.")
  }

  # Use declared model variables to determine layer type
  numeric_layers <- intersect(model$num_vars, layerNames)
  categorical_layers <- intersect(model$cat_vars, layerNames)

  # Histogram for numeric layers
  numeric_df <- foreach(
    layerName = numeric_layers,
    .combine = rbind
  ) %do% {
    getValueRange(rasterList, layerName, nBins, nSample)
  }

  # Placeholder for categorical layers
  categorical_df <- tibble::tibble(
    layer = categorical_layers,
    bin_lower = NA_real_,
    bin_upper = NA_real_,
    count = NA_real_,
    pct = NA_real_
  )

  bind_rows(numeric_df, categorical_df)
}

#' Predict Response Across Value Ranges ----
#'
#' @description
#' Predicts the response for values across the observed range of raster layers.
#'
#' @param rastLayerHistogram A histogram dataframe from `getRastLayerHistogram()`.
#' @param model A trained model (CNN, RF, MaxEnt).
#' @param modelType Character string specifying model type.
#'
#' @return A dataframe with predicted response values.
#'
#' @noRd
predictResponseHistogram <- function(rastLayerHistogram, model, modelType) {
  # Separate numeric and categorical variables in the model
  numeric_layers <- intersect(unique(rastLayerHistogram$layer), model$num_vars)
  categorical_layers <- intersect(model$cat_vars, unique(rastLayerHistogram$layer))

  # Prepare prediction grid for numeric layers
  rastHistogramPredict <- rastLayerHistogram %>%
    filter(layer %in% numeric_layers) %>%
    select(layer, bin_lower) %>%
    group_by(layer) %>%
    mutate(row = row_number()) %>%
    ungroup() %>%
    tidyr::spread(layer, bin_lower) %>%
    select(-row)

  # Predict for numeric layers
  numeric_predictions <- purrr::map_dfr(numeric_layers, function(layerName) {
    predictLayerTemp <- rastHistogramPredict
    fixed_cols <- setdiff(names(predictLayerTemp), layerName)
    predictLayerTemp[fixed_cols] <- purrr::map_dfc(
      predictLayerTemp[fixed_cols],
      ~ mean(.x, na.rm = TRUE)
    )

    preds <- if (modelType == "CNN") {
      # Add back missing categorical variables with a fixed level
      missing_cat_vars <- setdiff(model$cat_vars, names(predictLayerTemp))
      for (v in missing_cat_vars) {
        num_levels <- model$cat_levels[[v]]
        # Assign integer index directly, NOT factor
        predictLayerTemp[[v]] <- rep(1L, nrow(predictLayerTemp))
      }

      # Ensure column order is correct
      predictLayerTemp <- predictLayerTemp[, c(model$num_vars, model$cat_vars), drop = FALSE]

      predict_cnn_dataframe(model, predictLayerTemp, "prob")
    } else {
      # Add back missing categorical variables with a fixed level
      missing_cat_vars <- setdiff(model$cat_vars, names(predictLayerTemp))
      for (v in missing_cat_vars) {
      num_levels <- model$cat_levels[[v]]
      # Use index 1 for all rows (assuming it corresponds to a valid level)
      predictLayerTemp[[v]] <- as.integer(1L)
    }
      predict(model$model, predictLayerTemp, type = "response")$predictions
    }

    tibble::tibble(
      layer = layerName,
      predictor = predictLayerTemp[[layerName]],
      response = preds[, 2]
    )
  })

  # Generate empty rows for categorical variables
  cat_predictions <- purrr::map_dfr(categorical_layers, function(layerName) {
    tibble::tibble(
      layer = layerName,
      predictor = NA,
      response = NA_real_
    )
  })

  # Combine both
  predictedLayers <- bind_rows(numeric_predictions, cat_predictions)
  return(predictedLayers)
}

#' Plot Histogram and Predicted Response ----
#'
#' @description
#' Creates a faceted plot showing histograms and predicted responses.
#'
#' @param histogramData Output from `predictResponseHistogram()`.
#' @param transferDir Directory to save PNG.
#'
#' @noRd
plotLayerHistogram <- function(histogramData, transferDir) {
  # Only keep numeric predictors
  histogramData <- histogramData %>%
    dplyr::filter(!is.na(predictor))

  # Skip plotting if there's nothing to show
  if (nrow(histogramData) == 0) {
    warning("No numeric data available to plot.")
    return(NULL)
  }

  n_vars <- unique(histogramData$layer) %>% length()

  # Adjust font size and image height based on number of variables
  font_size <- if (n_vars <= 6) {
    20
  } else if (n_vars <= 12) {
    16
  } else if (n_vars <= 18) {
    12
  } else {
    10
  }

  width_per_facet <- 1
  height_per_facet <- 0.66
  plot_width <- max(10, n_vars * width_per_facet)
  plot_height <- max(10, n_vars * height_per_facet)

  p <- ggplot2::ggplot(histogramData, aes(x = predictor, y = response)) +
    ggplot2::geom_col(
      aes(y = pct * max(response, na.rm = TRUE)),
      fill = "gray80",
      width = 0.05
    ) +
    ggplot2::geom_line(linewidth = 1.2, color = "Grey20") +
    ggplot2::facet_wrap(~layer, scales = "free") +
    ggplot2::labs(
      x = "Layer values",
      y = "Predicted response",
      title = "Training layer histogram and response"
    ) +
    ggplot2::theme_classic(base_size = font_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
      ),
      plot.title.position = "plot"
    )

  outfile <- file.path(transferDir, "LayerHistogramResponse.png")
  ggplot2::ggsave(
    filename = outfile,
    plot = p,
    height = plot_height,
    width = plot_width,
    units = "in",
    dpi = 300
  )
}
