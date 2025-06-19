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
  pcaSample = 100000,
  nBins = 16
) {
  terraOptions(threads = 7)
  # helper to compute local range
  rangeFun <- function(vals, na.rm = TRUE) {
    if (na.rm) vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NA_real_)
    max(vals) - min(vals)
  }

  # helper to compute local entropy
  entropyFun <- function(vals, na.rm = TRUE) {
    if (na.rm) vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NA_real_)
    h <- hist(vals, breaks = nBins, plot = FALSE)
    p <- h$counts / sum(h$counts)
    p <- p[p > 0]
    -sum(p * log2(p))
  }

  # build moving-window weight matrix
  w <- matrix(1, adjacencyWindow, adjacencyWindow)

  # compute PCA on a random sample of pixels
  vals <- values(rasterIn, mat = TRUE)
  keep <- complete.cases(vals)
  samp <- sample(which(keep), min(pcaSample, sum(keep)))
  pcaMod <- prcomp(vals[samp, ], scale. = TRUE)

  pcs <- predict(
    rasterIn,
    model = pcaMod,
    index = 1:2,
    filename = "",
    overwrite = TRUE
  )
  names(pcs) <- c("PC1", "PC2")

  # compute local mean
  adjMean <- focal(
    rasterIn,
    w = w,
    fun = "mean",
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjMean) <- paste0(names(rasterIn), "_mean")

  # compute local sd
  adjSd <- focal(
    rasterIn,
    w = w,
    fun = "sd",
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjSd) <- paste0(names(rasterIn), "_sd")

  # compute local range
  adjRange <- focal(
    rasterIn,
    w = w,
    fun = rangeFun,
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjRange) <- paste0(names(rasterIn), "_range")

  # compute local entropy
  adjEntropy <- focal(
    rasterIn,
    w = w,
    fun = entropyFun,
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjEntropy) <- paste0(names(rasterIn), "_entropy")

  # stack everything: original → mean → sd → range → entropy → PCs
  rasterOut <- c(rasterIn, adjMean, adjSd, adjRange, adjEntropy, pcs)

  return(rasterOut)
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
