## -------------------------------
## ecoClassify - Contextualization Functions
## ApexRMS, November 2024
## -------------------------------

#' Add spatial context and PCA layers to raster stack ----
#'
#' @description
#' `addRasterAdjacencyValues` augments a multi-layer raster by computing local spatial
#' statistics (mean, standard deviation, range, entropy) over a square moving window,
#' and by appending the first two PCA components derived from sampled pixel values.
#'
#' @param rasterIn A SpatRaster with N predictor layers to contextualize.
#' @param adjacencyWindow Size of the square focal window (default = `contextualizationWindowSize`).
#' @param pcaSample Maximum number of pixels to use when computing PCA (default = 100000).
#' @param nBins Number of bins to use when calculating entropy (default = 16).
#'
#' @return A SpatRaster with the following layers concatenated:
#' \itemize{
#'   \item Original predictor layers
#'   \item Local means (per layer)
#'   \item Local standard deviations
#*   \item Local ranges
#'   \item Local entropy values
#'   \item First two PCA components (`PC1`, `PC2`)
#' }
#'
#' @details
#' Spatial statistics are computed using `terra::focal()` with a square kernel defined
#' by `adjacencyWindow`. PCA components are derived using `prcomp()` on a random sample
#' of complete pixel rows, and applied to the raster using `terra::predict()`.
#' This function supports contextualization of image data prior to classifier training.
#'
#' @noRd
addRasterAdjacencyValues <- function(
  rasterIn,
  adjacencyWindow = contextualizationWindowSize,
  pcaSample = 100000
) {
  # 1) Build the window
  w <- matrix(1, adjacencyWindow, adjacencyWindow)

  # 2) PCA
  vals <- values(rasterIn, mat = TRUE)
  keep <- complete.cases(vals)
  samp <- sample(which(keep), min(pcaSample, sum(keep)))
  pcaMod <- prcomp(vals[samp, ], scale. = TRUE)
  pcs <- predict(rasterIn, pcaMod, index = 1:2)
  names(pcs) <- c("PC1", "PC2")

  # 3) Four threaded focal calls
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
#'
#' @return A list of contextualized SpatRaster objects.
#'
#' @noRd
contextualizeRaster <- function(rasterList) {
  contextualizedRasterList <- c()

  for (r in seq_along(rasterList)) {
    raster <- rasterList[[r]]
    combinedRaster <- addRasterAdjacencyValues(raster)

    contextualizedRasterList <- c(contextualizedRasterList, combinedRaster)
  }

  return(contextualizedRasterList)
}
