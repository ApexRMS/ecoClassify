## -------------------------------
## ecoClassify - Contextualization Functions
## ApexRMS, November 2024
## -------------------------------

#' Add spatial context and PCA layers to raster stack ----
#'
#' @description
#' `addRasterAdjacencyValues` augments a multi-layer raster by computing local spatial
#' statistics (mean, standard deviation, range) over a square moving window,
#' and by appending the first two PCA components derived from sampled pixel values.
#'
#' @param rasterIn A SpatRaster with N predictor layers to contextualize.
#' @param adjacencyWindow Size of the square focal window (default = `contextualizationWindowSize`).
#' @param pcaSample Maximum number of pixels to use when computing PCA (default = 100000).
#'
#' @return A SpatRaster with the following layers concatenated:
#' \itemize{
#'   \item Original predictor layers
#'   \item Local means (per layer)
#'   \item Local standard deviations
#*   \item Local ranges
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
addRasterAdjacencyValues <- function(rasterIn, adjacencyWindow = contextualizationWindowSize,
                                     pcaSample = 100000) {
  w <- matrix(1, adjacencyWindow, adjacencyWindow)

  # ---- PCA sampling WITHOUT values(..., mat=TRUE) ----
  samp_df <- terra::spatSample(rasterIn,
                               size  = min(pcaSample, 100000),
                               method = "random",
                               as.df  = TRUE,
                               na.rm  = TRUE)         # small df only (sampled)
  pcaMod <- stats::prcomp(samp_df, scale. = TRUE)

  # Apply PCs in a streamed way (this was already good)
  pcs <- terra::predict(rasterIn, pcaMod, index = 1:2,
                        wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW","PREDICTOR=2")))
  names(pcs) <- c("PC1","PC2")

  # ---- FOCAL stats: stream & thread; avoid big in-RAM stacks ----
  terraOptions(progress = 0)  # set threads globally elsewhere if desired
  wopt <- list(datatype = "FLT4S", gdal = c("COMPRESS=LZW","PREDICTOR=2"))

  adjMean <- terra::focal(rasterIn, w, fun = "mean", na.rm = TRUE, filename = tempfile(fileext=".tif"), wopt = wopt)
  adjSd   <- terra::focal(rasterIn, w, fun = "sd",   na.rm = TRUE, filename = tempfile(fileext=".tif"), wopt = wopt)

  # Consider dropping min/max to save two passes; if you keep them, write to disk:
  adjMin  <- terra::focal(rasterIn, w, fun = "min",  na.rm = TRUE, filename = tempfile(fileext=".tif"), wopt = wopt)
  adjMax  <- terra::focal(rasterIn, w, fun = "max",  na.rm = TRUE, filename = tempfile(fileext=".tif"), wopt = wopt)
  adjRange <- adjMax - adjMin
  names(adjMean)  <- paste0(names(rasterIn), "_mean")
  names(adjSd)    <- paste0(names(rasterIn), "_sd")
  names(adjMin)   <- paste0(names(rasterIn), "_min")
  names(adjMax)   <- paste0(names(rasterIn), "_max")
  names(adjRange) <- paste0(names(rasterIn), "_range")

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
