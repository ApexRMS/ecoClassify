## -------------------------------
## ecoClassify - Contextualization Functions
## ApexRMS, November 2024
## -------------------------------

#' Fit the contextualization PCA ----
#'
#' @description
#' Fits one principal component analysis on a pooled random sample of pixels
#' from every raster in the list. The same fitted PCA is then used for every
#' training raster, every predicting raster and every spatial tile, so PC1 and
#' PC2 mean the same thing everywhere. (Previously a separate PCA was fitted on
#' each raster; component signs and loadings then differed between rasters.)
#'
#' @param rasterList List of SpatRasters with the same layers.
#' @param pcaSample Total number of pixels to sample across all rasters.
#' @return A `prcomp` object (without the sample scores, to keep it small).
#' @noRd
fitContextPca <- function(rasterList, pcaSample = 100000) {
  nLayers <- terra::nlyr(rasterList[[1]])
  if (nLayers < 2) {
    stop(
      "Contextualization needs rasters with at least two bands (it derives two principal ",
      "components from the bands). Turn off 'Add neighbourhood covariates' under Advanced ",
      "for single-band imagery."
    )
  }

  perRaster <- ceiling(pcaSample / length(rasterList))
  samples <- lapply(rasterList, function(r) {
    terra::spatSample(r, size = min(perRaster, terra::ncell(r)), method = "random",
                      na.rm = TRUE, as.df = TRUE, values = TRUE)
  })
  vals <- do.call(rbind, samples)
  vals <- vals[stats::complete.cases(vals), , drop = FALSE]
  if (nrow(vals) < nLayers + 1) {
    stop("Not enough valid pixels to fit the contextualization PCA.")
  }

  pcaMod <- stats::prcomp(vals, scale. = TRUE)
  pcaMod$x <- NULL # sample scores are not needed for predict()
  pcaMod
}

#' Add spatial context and PCA layers to raster stack ----
#'
#' @description
#' `addRasterAdjacencyValues` augments a multi-layer raster by computing local spatial
#' statistics (mean, standard deviation, min, max, range) over a square moving window,
#' and by appending the first two principal components.
#'
#' @param rasterIn A SpatRaster with N predictor layers to contextualize.
#' @param adjacencyWindow Size of the square focal window (default = `contextualizationWindowSize`).
#' @param pcaModel A `prcomp` fit from `fitContextPca()`. If NULL (models trained
#'   before ecoClassify 2.5), a PCA is fitted on this raster alone.
#' @param pcaSample Pixels to sample when `pcaModel` is NULL.
#'
#' @return A SpatRaster with the following layers concatenated:
#' \itemize{
#'   \item Original predictor layers
#'   \item Local means, standard deviations, minimums, maximums and ranges (per layer)
#'   \item First two PCA components (`PC1`, `PC2`)
#' }
#'
#' @noRd
addRasterAdjacencyValues <- function(
  rasterIn,
  adjacencyWindow = contextualizationWindowSize,
  pcaModel = NULL,
  pcaSample = 100000
) {
  # 1) Build the window
  w <- matrix(1, adjacencyWindow, adjacencyWindow)

  # 2) PCA (fitted once on all training rasters, then reused)
  if (is.null(pcaModel)) {
    pcaModel <- fitContextPca(list(rasterIn), pcaSample)
  }
  pcs <- terra::predict(rasterIn, pcaModel, index = 1:2)
  names(pcs) <- c("PC1", "PC2")

  # 3) Focal statistics
  rasterNames <- names(rasterIn)
  adjMean <- focal(rasterIn, w, fun = "mean", na.rm = TRUE)
  names(adjMean) <- paste0(rasterNames, "_mean")

  adjSd <- focal(rasterIn, w, fun = "sd", na.rm = TRUE)
  names(adjSd) <- paste0(rasterNames, "_sd")

  adjMin <- focal(rasterIn, w, fun = "min", na.rm = TRUE)
  names(adjMin) <- paste0(rasterNames, "_min")

  adjMax <- focal(rasterIn, w, fun = "max", na.rm = TRUE)
  names(adjMax) <- paste0(rasterNames, "_max")

  # 4) Range = max – min
  adjRange <- adjMax - adjMin
  names(adjRange) <- paste0(rasterNames, "_range")

  # 5) Stack & return
  return(c(
    rasterIn,
    adjMean,
    adjSd,
    adjMin,
    adjMax,
    adjRange,
    pcs
  ))
}

#' Contextualize Raster List ----
#'
#' @description
#' `contextualizeRaster` applies spatial contextualization to a list of raster objects
#' by computing local statistics and principal components for each raster.
#'
#' @param rasterList A list of SpatRaster objects.
#' @param pcaModel A `prcomp` fit from `fitContextPca()`, shared by all rasters.
#'   If NULL, each raster gets its own PCA (old behaviour, kept for old models).
#'
#' @return A list of contextualized SpatRaster objects.
#'
#' @noRd
contextualizeRaster <- function(rasterList, pcaModel = NULL) {
  lapply(rasterList, function(raster) addRasterAdjacencyValues(raster, pcaModel = pcaModel))
}
