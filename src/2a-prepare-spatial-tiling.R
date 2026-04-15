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

multiprocessingSheet        <- datasheet(myScenario, "core_Multiprocessing")
advancedOptionsDataframe    <- datasheet(myScenario, "ecoClassify_AdvancedClassifierOptions")

mpEnabled <- nrow(multiprocessingSheet) > 0 &&
  !is.null(multiprocessingSheet$EnableMultiprocessing) &&
  isTRUE(multiprocessingSheet$EnableMultiprocessing[1])

if (!mpEnabled) {
  updateRunLog(
    paste0(
      "Tiling was selected but multiprocessing is not enabled. ",
      "The tile grid will be generated but tiling will not be applied during prediction. ",
      "Enable multiprocessing in the scenario run settings to use spatial tiling."
    ),
    type = "warning"
  )
}


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


# Compute per-tile buffer for contextualization edge correction ----------------
# Tiles are pre-cropped with an extra border of floor(window/2) pixels so the
# focal neighbourhood step in 2-predict.r sees valid values at tile edges.
# The buffer is stripped back to the original tile extent after contextualization.

buffer_px <- 0L
if (nrow(advancedOptionsDataframe) > 0 &&
    !is.null(advancedOptionsDataframe$applyContextualization) &&
    isTRUE(advancedOptionsDataframe$applyContextualization[1])) {
  .windowSize <- if (
    !is.null(advancedOptionsDataframe$contextualizationWindowSize) &&
    !is.na(advancedOptionsDataframe$contextualizationWindowSize[1])
  ) {
    as.integer(advancedOptionsDataframe$contextualizationWindowSize[1])
  } else {
    3L  # matches assignVariables() default
  }
  buffer_px <- floor(.windowSize / 2L)
  updateRunLog(paste0(
    "Contextualization enabled (window size ", .windowSize, "): ",
    "tiles will include a ", buffer_px, "-pixel overlap buffer per side ",
    "to eliminate edge artifacts."
  ))
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


# Load template raster and compute grid parameters ----------------------------

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

.resX       <- terra::xres(templateRast)
.resY       <- terra::yres(templateRast)
fullExt     <- as.vector(terra::ext(templateRast))  # named: xmin, xmax, ymin, ymax
.crs        <- terra::crs(templateRast)
nTotalTiles <- as.integer(numTilesX * numTilesY)

# Initialise tile assignment matrix (NA = empty/skipped tile)
tileMatrix <- matrix(NA_integer_, nrow = nRows, ncol = nCols)


# Pre-crop predicting rasters per tile, skipping tiles with no valid pixels ----

progressBar(type = "message", message = "Pre-cropping rasters per tile")

allRasterFiles <- predictingRasterDataframe$predictingRasterFile
allRasterFiles <- unique(allRasterFiles[
  !is.na(allRasterFiles) & nzchar(allRasterFiles) & file.exists(allRasterFiles)
])

newId         <- 1L   # contiguous job ID assigned to each non-empty tile
manifestTiles <- list()
tilesDataDir  <- NULL

if (length(allRasterFiles) > 0) {
  # Use e$LibraryFilePath rather than ssim_library env var, which can contain
  # surrounding quotes on Windows causing path construction to fail.
  sid_match   <- regmatches(transferDir, regexpr("Scenario-\\d+", transferDir))
  scenario_id <- as.integer(sub("Scenario-", "", sid_match))

  tilesDataDir <- file.path(
    paste0(e$LibraryFilePath, ".data"),
    paste0("Scenario-", scenario_id),
    "ecoClassifyTiles"
  )
  dir.create(tilesDataDir, recursive = TRUE, showWarnings = FALSE)

  for (iy in seq_len(numTilesY)) {
    rowStart <- tileRowBreaks[iy] + 1L
    rowEnd   <- tileRowBreaks[iy + 1L]
    for (ix in seq_len(numTilesX)) {
      colStart <- tileColBreaks[ix] + 1L
      colEnd   <- tileColBreaks[ix + 1L]
      origId   <- (iy - 1L) * numTilesX + ix

      # Derive tile spatial extent directly from row/col breaks
      tileXmin <- fullExt["xmin"] + (colStart - 1L) * .resX
      tileXmax <- fullExt["xmin"] +  colEnd         * .resX
      tileYmax <- fullExt["ymax"] - (rowStart - 1L) * .resY
      tileYmin <- fullExt["ymax"] -  rowEnd         * .resY
      tileExt  <- terra::ext(tileXmin, tileXmax, tileYmin, tileYmax)

      # Skip tiles with no valid pixels (e.g. corners outside the mosaic boundary)
      tileSample <- terra::crop(templateRast[[1]], tileExt)
      validCount <- terra::global(tileSample, "notNA")[[1]]
      rm(tileSample)

      if (validCount == 0L) {
        updateRunLog(sprintf(
          "Tile at grid position (row %d, col %d): skipped — no valid pixels.",
          iy, ix
        ), type = "info")
        next
      }

      # Expand tile extent by contextualization buffer (clamped to full raster bounds)
      cropExt <- if (buffer_px > 0L) {
        terra::ext(
          max(tileXmin - buffer_px * .resX, fullExt["xmin"]),
          min(tileXmax + buffer_px * .resX, fullExt["xmax"]),
          max(tileYmin - buffer_px * .resY, fullExt["ymin"]),
          min(tileYmax + buffer_px * .resY, fullExt["ymax"])
        )
      } else {
        tileExt
      }

      # Assign this grid cell the next contiguous job ID
      tileMatrix[rowStart:rowEnd, colStart:colEnd] <- newId

      tileDirPath <- file.path(tilesDataDir, paste0("tile_", newId))
      dir.create(tileDirPath, recursive = TRUE, showWarnings = FALSE)

      rasterMap <- list()
      for (j in seq_along(allRasterFiles)) {
        rPath   <- allRasterFiles[j]
        outName <- paste0("tile_", newId, "_r", j, "_", basename(rPath))
        outPath <- file.path(tileDirPath, outName)
        terra::writeRaster(
          terra::crop(terra::rast(rPath), cropExt),
          filename  = outPath,
          overwrite = TRUE,
          gdal      = c("COMPRESS=LZW")
        )
        rasterMap[[rPath]] <- outPath
      }

      manifestTiles[[newId]] <- list(
        tile_id     = newId,
        tile_extent = list(
          xmin = tileXmin,
          xmax = tileXmax,
          ymin = tileYmin,
          ymax = tileYmax
        ),
        raster_map  = rasterMap
      )

      updateRunLog(sprintf(
        "Tile %d: pre-cropped %d predicting raster(s).",
        newId, length(allRasterFiles)
      ))

      newId <- newId + 1L
    }
  }
} else {
  updateRunLog(
    "No valid predicting rasters found; skipping per-tile pre-cropping.",
    type = "warning"
  )
}

nValidTiles <- newId - 1L
rm(templateRast)

if (nTotalTiles > nValidTiles) {
  updateRunLog(sprintf(
    "%d of %d tile(s) contained no valid pixels and were skipped; %d tile(s) will be processed.",
    nTotalTiles - nValidTiles, nTotalTiles, nValidTiles
  ), type = "info")
}


# Build and write tile mask raster ---------------------------------------------

progressBar(type = "message", message = "Writing tile mask raster")

tileRast <- terra::rast(
  nrows = nRows,
  ncols = nCols,
  xmin  = fullExt["xmin"],
  xmax  = fullExt["xmax"],
  ymin  = fullExt["ymin"],
  ymax  = fullExt["ymax"],
  crs   = .crs
)
terra::values(tileRast) <- as.integer(t(tileMatrix))
rm(tileMatrix)
names(tileRast) <- "TileID"

updateRunLog(paste0(
  "Tile mask created with ", nValidTiles, " tile(s) ",
  "(values 1 to ", nValidTiles, ")."
))

gridFilename <- paste0(
  "smpGrid-", numTilesX, "x", numTilesY,
  "-", nValidTiles, "tiles.tif"
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
rm(tileRast)


# Write tile manifest ----------------------------------------------------------

if (!is.null(tilesDataDir) && length(manifestTiles) > 0) {
  manifestData <- list(
    version    = "1.0",
    tile_count = as.integer(nValidTiles),
    buffer_px  = buffer_px,
    full_extent = list(
      xmin  = fullExt["xmin"],
      xmax  = fullExt["xmax"],
      ymin  = fullExt["ymin"],
      ymax  = fullExt["ymax"],
      nrows = nRows,
      ncols = nCols,
      crs   = .crs
    ),
    tiles = manifestTiles
  )

  manifestPath <- file.path(tilesDataDir, "tile_manifest.json")
  jsonlite::write_json(manifestData, manifestPath, auto_unbox = TRUE, pretty = TRUE)
  updateRunLog(paste0("Tile manifest saved to: ", manifestPath))
  rm(manifestData)
}


# Save path to core_SpatialMultiprocessing -------------------------------------

saveDatasheet(
  myScenario,
  data = data.frame(MaskFileName = gridPath, stringsAsFactors = FALSE),
  name = "core_SpatialMultiprocessing"
)

updateRunLog("core_SpatialMultiprocessing datasheet updated.")

saveDatasheet(
  myScenario,
  data = data.frame(EnableMultiprocessing = TRUE, MaximumJobs = nValidTiles,
                    stringsAsFactors = FALSE),
  name = "core_Multiprocessing"
)

updateRunLog(paste0("core_Multiprocessing set to ", nValidTiles, " job(s) to match tile count."))

terra::tmpFiles(remove = TRUE)

progressBar(type = "message", message = "Spatial tiling preparation complete")
