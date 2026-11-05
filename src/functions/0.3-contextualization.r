## -------------------------------
## ecoClassify - Contextualization Functions
## ApexRMS, November 2024
## -------------------------------

#' Fit the Contextualization PCA ----
#'
#' @description
#' Fits the PCA used for the `PC1`/`PC2` contextualization layers once, on
#' pixels sampled from all rasters in the list. Training fits it and saves it
#' with the model; prediction reuses that fit so PC1/PC2 have the same meaning
#' (axes and sign) in every raster and every tile.
#'
#' @param rasterList List of SpatRasters (after rounding/normalization).
#' @param pcaSample Total number of pixels to sample across all rasters.
#'
#' @return A `prcomp` object.
#'
#' @noRd
fitContextPca <- function(rasterList, pcaSample = 100000) {
  perRaster <- max(1L, ceiling(pcaSample / length(rasterList)))
  # spatSample avoids loading all pixels into memory (complete.cases fails on
  # long vectors for large rasters on Linux)
  sampleVals <- do.call(rbind, lapply(rasterList, function(r) {
    as.matrix(terra::spatSample(
      r,
      size = min(perRaster, terra::ncell(r)),
      method = "random",
      na.rm = TRUE
    ))
  }))
  stats::prcomp(sampleVals, scale. = TRUE)
}

#' Add spatial context and PCA layers to raster stack ----
#'
#' @description
#' `addRasterAdjacencyValues` augments a multi-layer raster by computing local
#' spatial statistics (mean, standard deviation, min, max, range) over a square
#' moving window, and by appending the first two components of `pcaModel`.
#'
#' @param rasterIn A SpatRaster with N predictor layers to contextualize.
#' @param adjacencyWindow Size of the square focal window.
#' @param pcaModel A `prcomp` object from `fitContextPca()`.
#'
#' @return A SpatRaster with the following layers concatenated:
#' \itemize{
#'   \item Original predictor layers
#'   \item Local means, standard deviations, minimums, maximums and ranges
#'   \item First two PCA components (`PC1`, `PC2`)
#' }
#'
#' @noRd
addRasterAdjacencyValues <- function(rasterIn, adjacencyWindow, pcaModel) {
  # 1) Build the window
  w <- matrix(1, adjacencyWindow, adjacencyWindow)

  # 2) Apply the shared PCA fit
  pcs <- terra::predict(rasterIn, pcaModel, index = 1:2)
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

  # 4) Range = max - min
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
#' `contextualizeRaster` applies spatial contextualization to a list of raster
#' objects, using one shared PCA fit for every raster.
#'
#' @param rasterList A named list of SpatRaster objects (names are timestep
#'   strings).
#' @param windowSize Size of the square focal window.
#' @param pcaModel A `prcomp` object from `fitContextPca()`.
#'
#' @return A list of contextualized SpatRaster objects, retaining the timestep
#'   names of `rasterList`.
#'
#' @details
#' The timestep names are preserved because downstream functions such as
#' `addCovariates()` match rasters to per-timestep covariates by name.
#'
#' @noRd
contextualizeRaster <- function(rasterList, windowSize, pcaModel) {
  contextualizedRasterList <- vector("list", length(rasterList))
  names(contextualizedRasterList) <- names(rasterList)

  for (r in seq_along(rasterList)) {
    contextualizedRasterList[[r]] <- addRasterAdjacencyValues(
      rasterList[[r]],
      adjacencyWindow = windowSize,
      pcaModel = pcaModel
    )
  }

  return(contextualizedRasterList)
}
