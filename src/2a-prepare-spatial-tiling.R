## -------------------------------------------
## ecoClassify - Prepare Spatial Tiling
## ApexRMS, 2026
## -------------------------------------------

# Set up workspace -------------------------------------------------------------

packageDir <- (Sys.getenv("ssim_package_directory"))

source(file.path(packageDir, "installDependencies.r"))

sourceScripts <- list.files(
  path = file.path(packageDir, "/functions"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

invisible(lapply(sourceScripts, source))

progressBar(
  type = "message",
  message = "Loading input data and setting up scenario"
)

# Get the SyncroSim Scenario that is currently running
myScenario <- scenario()

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory


# Load input datasheets --------------------------------------------------------

predictingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputPredictingRasters"
)

tilingOptionsDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_TilingOptions"
)

multiprocessingSheet <- datasheet(myScenario, "core_Multiprocessing")


# Resolve tiling parameters ----------------------------------------------------

autoTiling <- FALSE
numTilesX  <- 2L
numTilesY  <- 2L

if (nrow(tilingOptionsDataframe) > 0) {
  if (!is.null(tilingOptionsDataframe$AutoTiling) &&
        isTRUE(tilingOptionsDataframe$AutoTiling[1])) {
    autoTiling <- TRUE
  }
  if (!is.null(tilingOptionsDataframe$NumTilesX) &&
        !is.na(tilingOptionsDataframe$NumTilesX[1])) {
    numTilesX <- as.integer(tilingOptionsDataframe$NumTilesX[1])
  }
  if (!is.null(tilingOptionsDataframe$NumTilesY) &&
        !is.na(tilingOptionsDataframe$NumTilesY[1])) {
    numTilesY <- as.integer(tilingOptionsDataframe$NumTilesY[1])
  }
}

if (autoTiling) {
  # Read MaximumJobs directly rather than using setCores(), because this
  # transformer runs with isMultiprocessing="False" so SyncroSim sets
  # EnableMultiprocessing=FALSE in the execution context, causing setCores()
  # to always return 1.
  nCores <- if (
    nrow(multiprocessingSheet) > 0 &&
    !is.null(multiprocessingSheet$MaximumJobs) &&
    !is.na(multiprocessingSheet$MaximumJobs[1]) &&
    multiprocessingSheet$MaximumJobs[1] > 0L
  ) {
    as.integer(multiprocessingSheet$MaximumJobs[1])
  } else {
    parallel::detectCores()
  }
  nTiles <- max(nCores, 1L)
  numTilesX <- ceiling(sqrt(nTiles))
  numTilesY <- floor(nTiles / numTilesX)
  updateRunLog(paste0(
    "Auto-tiling: ", nCores, " job(s) configured -> ",
    numTilesX, " x ", numTilesY, " tile grid (",
    numTilesX * numTilesY, " tiles)."
  ))
} else {
  if (numTilesX < 1L || numTilesY < 1L) {
    stop(paste0(
      "TilingOptions: NumTilesX and NumTilesY must each be >= 1. ",
      "Got NumTilesX=", numTilesX, ", NumTilesY=", numTilesY, "."
    ))
  }
  updateRunLog(paste0(
    "Tiling configuration: ", numTilesX, " tile(s) along X, ",
    numTilesY, " tile(s) along Y (", numTilesX * numTilesY, " total tiles)."
  ))
  configuredJobs <- if (
    nrow(multiprocessingSheet) > 0 &&
    !is.null(multiprocessingSheet$MaximumJobs) &&
    !is.na(multiprocessingSheet$MaximumJobs[1])
  ) as.integer(multiprocessingSheet$MaximumJobs[1]) else 1L
  if (numTilesX * numTilesY > configuredJobs) {
    updateRunLog(paste0(
      "Number of tiles (", numTilesX * numTilesY, ") exceeds the number of configured jobs (", configuredJobs, "). ",
      "Tiles will run in batches of ", configuredJobs, ". ",
      "To run all tiles simultaneously, set Maximum Jobs to ", numTilesX * numTilesY, " or more."
    ), type = "info")
  }
}


# Resolve template raster path -------------------------------------------------

progressBar(type = "message", message = "Resolving template raster")

templateRasterPath <- NA_character_

if (nrow(predictingRasterDataframe) > 0) {
  candidatePaths <- predictingRasterDataframe$predictingRasterFile
  candidatePaths <- candidatePaths[!is.na(candidatePaths) & nzchar(candidatePaths)]
  if (length(candidatePaths) > 0) {
    templateRasterPath <- candidatePaths[1]
    updateRunLog(paste0("Using predicting raster as template: ", templateRasterPath))
  }
}

if (is.na(templateRasterPath)) {
  stop(paste0(
    "PrepareSpatialTiling requires at least one predicting raster to be specified. ",
    "Please configure InputPredictingRasters."
  ))
}

if (!file.exists(templateRasterPath)) {
  stop(paste0("Template raster file does not exist: ", templateRasterPath))
}


# Check for mismatched extents across all input rasters ------------------------

allRasterPaths <- predictingRasterDataframe$predictingRasterFile
allRasterPaths <- allRasterPaths[!is.na(allRasterPaths) & nzchar(allRasterPaths)]
allRasterPaths <- allRasterPaths[file.exists(allRasterPaths)]

if (length(allRasterPaths) > 1) {
  templateExt <- terra::ext(terra::rast(templateRasterPath))
  mismatchedPaths <- Filter(function(p) {
    !isTRUE(all.equal(
      as.vector(terra::ext(terra::rast(p))),
      as.vector(templateExt)
    ))
  }, allRasterPaths[-1])

  if (length(mismatchedPaths) > 0) {
    updateRunLog(paste0(
      "Warning: the following raster(s) have a different spatial extent than the ",
      "template raster (", basename(templateRasterPath), ") used to generate the tile grid. ",
      "Tiles will be based on the first timestep extent only, which may cause issues ",
      "when predicting on rasters with a different extent:\n  - ",
      paste(basename(mismatchedPaths), collapse = "\n  - "), "\n",
      "Consider using separate scenarios for rasters with different spatial extents."
    ), type = "warning")
  }
}


# Build tile mask raster -------------------------------------------------------

progressBar(type = "message", message = "Generating tile mask raster")

templateRast <- terra::rast(templateRasterPath)

nRows <- terra::nrow(templateRast)
nCols <- terra::ncol(templateRast)

updateRunLog(paste0(
  "Template raster dimensions: ", nCols, " cols x ", nRows, " rows."
))

# Compute breakpoints that evenly divide rows and columns across tiles
tileRowBreaks <- round(seq(0, nRows, length.out = numTilesY + 1L))
tileColBreaks <- round(seq(0, nCols, length.out = numTilesX + 1L))

# Build integer matrix where each cell value = tile ID (row-major, 1-based)
tileMatrix <- matrix(NA_integer_, nrow = nRows, ncol = nCols)

tileId <- 1L
for (iy in seq_len(numTilesY)) {
  rowStart <- tileRowBreaks[iy] + 1L
  rowEnd   <- tileRowBreaks[iy + 1L]
  for (ix in seq_len(numTilesX)) {
    colStart <- tileColBreaks[ix] + 1L
    colEnd   <- tileColBreaks[ix + 1L]
    tileMatrix[rowStart:rowEnd, colStart:colEnd] <- tileId
    tileId <- tileId + 1L
  }
}

# Create SpatRaster from matrix, copying spatial metadata from template
tileRast <- terra::rast(
  nrows = nRows,
  ncols = nCols,
  xmin  = terra::xmin(templateRast),
  xmax  = terra::xmax(templateRast),
  ymin  = terra::ymin(templateRast),
  ymax  = terra::ymax(templateRast),
  crs   = terra::crs(templateRast)
)
terra::values(tileRast) <- as.integer(tileMatrix)
names(tileRast) <- "TileID"

updateRunLog(paste0(
  "Tile mask created with ", numTilesX * numTilesY, " tile(s) ",
  "(values 1 to ", numTilesX * numTilesY, ")."
))


# Write tile mask raster to disk -----------------------------------------------

progressBar(type = "message", message = "Writing tile mask raster")

gridFilename <- paste0(
  "smpGrid-", numTilesX, "x", numTilesY,
  "-", numTilesX * numTilesY, "tiles.tif"
)
gridPath <- file.path(transferDir, gridFilename)

terra::writeRaster(
  tileRast,
  filename  = gridPath,
  datatype  = "INT4S",
  NAflag    = -9999L,
  gdal      = c("COMPRESS=LZW"),
  overwrite = TRUE
)

updateRunLog(paste0("Tile grid raster saved to: ", gridPath))


# Save path to core_SpatialMultiprocessing -------------------------------------

saveDatasheet(
  myScenario,
  data = data.frame(MaskFileName = gridPath, stringsAsFactors = FALSE),
  name = "core_SpatialMultiprocessing"
)

updateRunLog("core_SpatialMultiprocessing datasheet updated.")

totalTiles <- as.integer(numTilesX * numTilesY)
saveDatasheet(
  myScenario,
  data = data.frame(EnableMultiprocessing = TRUE, MaximumJobs = totalTiles,
                    stringsAsFactors = FALSE),
  name = "core_Multiprocessing"
)

updateRunLog(paste0("core_Multiprocessing set to ", totalTiles, " job(s) to match tile count."))

progressBar(type = "message", message = "Spatial tiling preparation complete")
