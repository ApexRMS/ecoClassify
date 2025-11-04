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
    dplyr::filter(layer %in% numeric_layers) %>%
    dplyr::select(layer, bin_lower) %>%
    dplyr::group_by(layer) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = layer, values_from = bin_lower)

  if (nrow(rastHistogramPredict) == 0) {
    warning("predictResponseHistogram: No data to predict from — raster histogram is empty.")
    return(tibble::tibble(layer = character(), predictor = numeric(), response = numeric()))
  }

  # Predict for numeric layers
  numeric_predictions <- purrr::map_dfr(numeric_layers, function(layerName) {
    predictLayerTemp <- rastHistogramPredict

    if (nrow(predictLayerTemp) == 0) {
      warning(sprintf("Skipping response prediction for layer '%s' due to empty prediction grid.", layerName))
      return(NULL)
    }

    # Fix non-focal predictors to their mean
    fixed_cols <- setdiff(names(predictLayerTemp), layerName)
    predictLayerTemp[fixed_cols] <- purrr::map_dfc(
      predictLayerTemp[fixed_cols],
      ~ mean(.x, na.rm = TRUE)
    )

    # Handle categorical predictors
    if (modelType %in% c("CNN", "MaxEnt")) {
      missing_cat_vars <- setdiff(model$cat_vars, names(predictLayerTemp))
      for (v in missing_cat_vars) {
        lvl <- model$cat_levels[[v]]
        if (nrow(predictLayerTemp) > 0) {
          predictLayerTemp[[v]] <- factor(rep(lvl[1], nrow(predictLayerTemp)), levels = lvl)
        }
      }
      for (v in model$cat_vars) {
        if (!is.factor(predictLayerTemp[[v]])) {
          lvl <- model$cat_levels[[v]]
          predictLayerTemp[[v]] <- factor(predictLayerTemp[[v]], levels = lvl)
        }
      }
    }
    else if (modelType == "Random Forest") {
      missing_cat_vars <- setdiff(model$cat_vars, names(predictLayerTemp))
      for (v in missing_cat_vars) {
        lvl <- model$factor_levels[[v]]
        if (nrow(predictLayerTemp) > 0) {
          predictLayerTemp[[v]] <- factor(rep(lvl[1], nrow(predictLayerTemp)), levels = lvl)
        }
      }
      for (v in model$cat_vars) {
        if (!is.factor(predictLayerTemp[[v]])) {
          lvl <- model$factor_levels[[v]]
          predictLayerTemp[[v]] <- factor(predictLayerTemp[[v]], levels = lvl)
        }
      }
    }

    # Align column order
    predictLayerTemp <- predictLayerTemp[, c(model$num_vars, model$cat_vars), drop = FALSE]

    # Perform prediction
    preds <- tryCatch({
      if (modelType == "CNN") {
        predict_cnn_dataframe(model, predictLayerTemp, "prob")
      } else if (modelType == "Random Forest") {
        predict(model$model, predictLayerTemp, type = "response")$predictions
      } else if (modelType == "MaxEnt") {
        model$predict_df(model$model, predictLayerTemp)
      } else {
        stop("Model type not recognized.")
      }
    }, error = function(e) {
      warning(sprintf("Prediction failed for layer '%s': %s", layerName, e$message))
      return(rep(NA_real_, nrow(predictLayerTemp)))
    })

    # Build output
    tibble::tibble(
      layer = layerName,
      predictor = predictLayerTemp[[layerName]],
      response = preds[, 2]
    )
  })

  # Create placeholder rows for categorical variables
  cat_predictions <- purrr::map_dfr(categorical_layers, function(layerName) {
    tibble::tibble(
      layer = layerName,
      predictor = NA,
      response = NA_real_
    )
  })

  # Combine and return
  predictedLayers <- dplyr::bind_rows(numeric_predictions, cat_predictions)

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
plotLayerHistogram <- function(histogramData, transferDir, max_vars = 48) {
  # Only keep numeric predictors
  histogramData <- histogramData %>%
    dplyr::filter(!is.na(predictor))

  # Skip plotting if there's nothing to show
  if (nrow(histogramData) == 0) {
    warning("No numeric data available to plot.")
    return(NULL)
  }

  # Limit to top 12 variables (or fewer)
  unique_layers <- unique(histogramData$layer)
  selected_layers <- head(unique_layers, max_vars)
  histogramData <- histogramData %>%
    dplyr::filter(layer %in% selected_layers)

  n_vars <- length(unique(histogramData$layer))

  # Adjust font size based on number of variables
  font_size <- if (n_vars <= 6) {
    20
  } else if (n_vars <= 12) {
    16
  } else if (n_vars <= 18) {
    12
  } else {
    10
  }

  # Adjust plot dimensions based on number of facets
  width_per_facet <- 1
  height_per_facet <- 0.66
  plot_width <- min(50, max(10, n_vars * width_per_facet))
  plot_height <- min(30, max(10, n_vars * height_per_facet))

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
    dpi = 300,
    limitsize = FALSE  # ← allows larger plots if needed
  )
}
