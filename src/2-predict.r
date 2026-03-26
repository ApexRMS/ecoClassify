## ---------------------------------------
## ecoClassify - Predict Using Classifier
## ApexRMS, November 2024
## ---------------------------------------

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
myProject <- project(myScenario)

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# --- Tile job detection -------------------------------------------------------
# SyncroSim names each spatial MP job library "Job-N.ssim". Detect that here
# and load the tile manifest written by the prep transformer.
tileJobId     <- NULL
tileRasterMap <- NULL
tileExtent    <- NULL
fullExtent    <- NULL
buffer_px     <- 0L
tilesDataDir  <- NULL
tileCount     <- NULL

# Use e$LibraryFilePath rather than the ssim_library env var, which can contain
# surrounding quotes on Windows causing path construction to fail.
.lib_path <- e$LibraryFilePath
.lib_name <- basename(.lib_path)

if (grepl("^Job-\\d+\\.ssim$", .lib_name)) {
  tileJobId <- as.integer(sub("^Job-(\\d+)\\.ssim$", "\\1", .lib_name))

  # Reconstruct parent library's .ssim.data directory
  .parent_lib_base <- sub("\\.ssim\\.temp.*$", "", .lib_path)
  .parent_data_dir <- paste0(.parent_lib_base, ".ssim.data")

  # Parse scenario ID from the transfer directory path
  .sid_match   <- regmatches(transferDir, regexpr("Scenario-\\d+", transferDir))
  .scenario_id <- as.integer(sub("Scenario-", "", .sid_match))

  .tilesDataDir <- file.path(.parent_data_dir,
                             paste0("Scenario-", .scenario_id),
                             "ecoClassifyTiles")
  .manifestPath <- file.path(.tilesDataDir, "tile_manifest.json")

  if (file.exists(.manifestPath)) {
    .manifest  <- jsonlite::read_json(.manifestPath, simplifyVector = FALSE)
    .tileInfo  <- .manifest$tiles[[tileJobId]]

    tileRasterMap <- .tileInfo$raster_map   # named list: orig_path -> cropped_path
    .te <- .tileInfo$tile_extent
    tileExtent <- terra::ext(.te$xmin, .te$xmax, .te$ymin, .te$ymax)
    .fe <- .manifest$full_extent
    fullExtent <- terra::ext(.fe$xmin, .fe$xmax, .fe$ymin, .fe$ymax)
    buffer_px    <- if (!is.null(.manifest$buffer_px)) as.integer(.manifest$buffer_px) else 0L
    tilesDataDir <- .tilesDataDir
    tileCount    <- as.integer(.manifest$tile_count)

    updateRunLog(sprintf("Running as spatial tile job %d.", tileJobId), type = "info")
  } else {
    updateRunLog(
      paste0("Tile manifest not found at: ", .manifestPath,
             ". Running prediction on full raster."),
      type = "warning"
    )
    tileJobId <- NULL
  }
}


# Load project-level datasheets ------------------------------------------------
terminologyDataframe <- datasheet(myProject, name = "ecoClassify_Terminology")

bandLabelFile <- if (
  nrow(terminologyDataframe) > 0 &&
  !is.null(terminologyDataframe$bandNames) &&
  !is.na(terminologyDataframe$bandNames[1]) &&
  nchar(terminologyDataframe$bandNames[1]) > 0
) terminologyDataframe$bandNames[1] else NULL

rgbBands <- if (
  nrow(terminologyDataframe) > 0 &&
  !is.null(terminologyDataframe$redBand) && !is.na(terminologyDataframe$redBand[1]) &&
  !is.null(terminologyDataframe$greenBand) && !is.na(terminologyDataframe$greenBand[1]) &&
  !is.null(terminologyDataframe$blueBand) && !is.na(terminologyDataframe$blueBand[1])
) {
  list(
    red   = terminologyDataframe$redBand[1],
    green = terminologyDataframe$greenBand[1],
    blue  = terminologyDataframe$blueBand[1]
  )
} else {
  NULL
}

# Load raster input datasheets -------------------------------------------------

predictingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputPredictingRasters"
)

predictingCovariateDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputPredictingCovariates"
)

modelObjectDataframe <- datasheet(myScenario, name = "ecoClassify_ModelObject")

# Check if multiprocessing is selected
mulitprocessingSheet <- datasheet(myScenario, "core_Multiprocessing")
nCores <- setCores(mulitprocessingSheet)

# When spatial tiling is active, SyncroSim runs one prediction process per tile
# concurrently. Using multiple ranger threads per tile causes nTiles * nThreads
# total threads, far exceeding available cores and exhausting virtual memory.
# Limit per-tile threads to 1 so total parallelism = nTiles (matching nCores).
spatialMPSheet <- datasheet(myScenario, "core_SpatialMultiprocessing")
isSpatialMP <- nrow(spatialMPSheet) > 0 &&
  !is.na(spatialMPSheet$MaskFileName[1]) &&
  nzchar(spatialMPSheet$MaskFileName[1])
nCoresForPrediction <- if (isSpatialMP) 1L else nCores

# Assign variables -------------------------------------------------------------

inputVariables <- assignVariables(
  myScenario,
  predictingRasterDataframe,
  predictingRasterDataframe$predictingRasterFile
)
timestepList <- inputVariables[[1]]
nObs <- inputVariables[[2]]
applyContextualization <- inputVariables[[3]]
contextualizationWindowSize <- inputVariables[[4]]
modelType <- inputVariables[[5]]
modelTuning <- inputVariables[[6]]
setManualThreshold <- inputVariables[[7]]
manualThreshold <- inputVariables[[8]]
normalizeRasters <- inputVariables[[9]]
rasterDecimalPlaces <- inputVariables[[10]]
tuningObjective <- inputVariables[[11]]
overrideBandnames <- inputVariables[[12]]

# Read TargetClassOptions ------------------------------------------------------

targetClassSheet <- datasheet(myScenario, "ecoClassify_TargetClassOptions")
targetClassValue <- NA_integer_
targetClassLabel <- NA_character_
if (nrow(targetClassSheet) > 0) {
  if (!is.null(targetClassSheet$targetClassValue) && isTRUE(!is.na(targetClassSheet$targetClassValue[1]))) targetClassValue <- as.integer(targetClassSheet$targetClassValue[1])
  if (!is.null(targetClassSheet$targetClassLabel) && isTRUE(!is.na(targetClassSheet$targetClassLabel[1]))) targetClassLabel <- as.character(targetClassSheet$targetClassLabel[1])
}

# Load model and threshold
if (modelType == "CNN") {
  model <- loadCNNModel(
    weights_path = modelObjectDataframe$Weights, # e.g. "model_weights.pt"
    metadata_path = modelObjectDataframe$Model # e.g. "model_metadata.rds"
  )
} else if (modelType == "Random Forest" || modelType == "MaxEnt") {
  model <- readRDS(modelObjectDataframe$Model)
}

if (setManualThreshold == FALSE) {
  threshold <- modelObjectDataframe$Threshold
} else {
  threshold <- manualThreshold
}

# Extract list of testing rasters ----------------------------------------------

# Preserve original raster paths (before tile substitution) so tile job 1 can
# generate the full-extent RGB from the un-cropped source rasters.
originalPredictingRasterDataframe <- predictingRasterDataframe

# For tile jobs, substitute pre-cropped raster paths before loading
if (!is.null(tileJobId) && !is.null(tileRasterMap)) {
  for (.i in seq_len(nrow(predictingRasterDataframe))) {
    .orig <- predictingRasterDataframe$predictingRasterFile[.i]
    if (!is.null(.orig) && !is.na(.orig) && nzchar(.orig)) {
      .cropped <- tileRasterMap[[.orig]]
      if (!is.null(.cropped) && nzchar(.cropped) && file.exists(.cropped)) {
        predictingRasterDataframe$predictingRasterFile[.i] <- .cropped
      }
    }
  }
  updateRunLog(sprintf("Tile %d: loaded pre-cropped raster paths.", tileJobId), type = "info")
}

predictRasterList <- extractRasters(predictingRasterDataframe, column = 2)

# Override band names if selected
if (isTRUE(overrideBandnames)) {
  if (!is.null(bandLabelFile)) {
    updateRunLog("Applying band label override to predicting rasters.", type = "info")
    predictRasterList <- overrideBandNames(predictRasterList, bandLabelFile)
  } else {
    updateRunLog(
      "Override band names is enabled but no band label file was supplied; band names will not be changed.",
      type = "warning"
    )
  }
} else {
  if (!is.null(bandLabelFile)) {
    updateRunLog(
      "A band label file was supplied but 'Override band names' is not enabled; band names will not be changed.",
      type = "info"
    )
  }
  updateRunLog(
    paste0(
      "Using original band names: ",
      paste(names(predictRasterList[[1]]), collapse = ", ")
    ),
    type = "info"
  )
}

# Pre-processing ---------------------------------------------------------------

progressBar(type = "message", message = "Pre-processing input data")

# Round rasters to integer, if selected
if (
  is.numeric(rasterDecimalPlaces) &&
    length(rasterDecimalPlaces) > 0 &&
    !is.na(rasterDecimalPlaces)
) {
  roundedRasters <- lapply(predictRasterList, function(r) {
    return(app(r, fun = function(x) round(x, rasterDecimalPlaces)))
  })
  predictRasterList <- roundedRasters
}

# Normalize predicting rasters, if selected
if (isTRUE(normalizeRasters)) {
  predictRasterList <- normalizeRaster(predictRasterList)
}

# Apply contextualization to prediction rasters, if selected
if (isTRUE(applyContextualization)) {
  predictRasterList <- contextualizeRaster(predictRasterList)
}

# Strip overlap buffer from tile rasters after contextualization.
# The buffer was added in 2a so the focal window has valid neighbours at tile
# edges; now we crop back to the original tile extent before prediction so the
# output aligns with the mosaic grid.
if (!is.null(tileJobId) && buffer_px > 0L && !is.null(tileExtent)) {
  predictRasterList <- lapply(predictRasterList, function(r) terra::crop(r, tileExtent))
  updateRunLog(
    sprintf("Tile %d: stripped %d-pixel overlap buffer; rasters cropped to tile extent.",
            tileJobId, buffer_px),
    type = "info"
  )
}

# Extract covariate rasters and convert to correct data type
predictingCovariateRaster <- processCovariates(
  predictingCovariateDataframe,
  modelType
)

# Crop covariate raster to tile extent so it aligns with the pre-cropped predicting rasters
if (!is.null(tileJobId) && !is.null(tileExtent) && !is.null(predictingCovariateRaster)) {
  predictingCovariateRaster <- terra::crop(predictingCovariateRaster, tileExtent)
}

# Add covariate data to predicting rasters.
# Pass transferDir so combined files are written there rather than R's tempdir().
predictRasterList <- addCovariates(predictRasterList, predictingCovariateRaster, transferDir)

# Check and mask NA values in predicting rasters
checkNA(predictRasterList)


# Setup empty dataframes to accept output in SyncroSim datasheet format --------
classifiedRasterOutputDataframe <- data.frame(
  Timestep = numeric(0),
  ClassifiedUnfiltered = character(0),
  ClassifiedFiltered = character(0),
  ClassifiedProbability = character(0)
)

classifiedRgbOutputDataframe <- data.frame(
  Timestep = numeric(0),
  RGBImage = character(0)
)

summaryRows <- list()

# Predict presence for rasters to classify -------------------------------------

progressBar(type = "message", message = "Predicting")

for (t in seq_along(predictRasterList)) {
  # Get timestep for the current raster
  timestep <- timestepList[t]

  classifiedRasters <- getPredictionRasters(
    predictRasterList[[t]],
    model,
    threshold,
    modelType,
    transferDir,
    category = "predicting",
    timestep,
    nCores = nCoresForPrediction
  )
  classifiedPresencePath <- classifiedRasters$presencePath
  classifiedProbabilityPath <- classifiedRasters$probabilityPath

  # Generate rasterDataframe based on filtering argument
  # Note: generateRasterDataframe constructs paths internally and doesn't use the first argument
  classifiedRasterOutputDataframe <- generateRasterDataframe(
    classifiedPresencePath,
    category = "predicting",
    timestep,
    transferDir,
    classifiedRasterOutputDataframe,
    hasGroundTruth = FALSE
  )

  # Define RGB data frame (skipped for tile jobs; merged PNG is created in post-processing)
  if (is.null(tileJobId)) {
    classifiedRgbOutputDataframe <- getRgbDataframe(
      classifiedRgbOutputDataframe,
      category = "predicting",
      timestep,
      transferDir
    )
  }

  # Save files (ground truth and RGB only - predictions already saved)
  # Tile jobs skip RGB here; tile job 1 generates the full-extent RGB below.
  if (is.null(tileJobId)) {
    saveFiles(
      classifiedPresencePath,
      classifiedProbabilityPath,
      predictRasterList[[t]],
      groundTruth = NULL,
      category = "predicting",
      timestep,
      transferDir,
      rgbBands = rgbBands
    )
  }

  # Accumulate summary row for this timestep (before extending, so counts reflect tile only)
  tryCatch({
    summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
      predictionRaster  = terra::rast(classifiedPresencePath),
      probabilityRaster = terra::rast(classifiedProbabilityPath),
      timestep          = timestep,
      predictionType    = "predicting",
      targetClassValue  = targetClassValue,
      targetClassLabel  = targetClassLabel
    )
  }, error = function(e) {
    updateRunLog(paste0("Could not build summary row for timestep ", timestep, ": ", conditionMessage(e)), type = "warning")
  })

  # Extend tile outputs to full raster extent so SyncroSim can mosaic tiles
  if (!is.null(tileJobId) && !is.null(fullExtent)) {
    for (.outPath in c(classifiedPresencePath, classifiedProbabilityPath)) {
      .r_extended <- terra::extend(terra::rast(.outPath), fullExtent)
      terra::writeRaster(.r_extended, .outPath, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
    }
    updateRunLog(
      sprintf("Tile %d: extended prediction outputs to full extent (timestep %s).",
              tileJobId, timestep),
      type = "info"
    )
  }
}

# Tile job 1: generate full-extent RGB from original (un-cropped) rasters -----
# All other tile jobs skip RGB; only tile 1 produces the RGB so the output
# covers the full spatial extent without needing a separate post-processing run.
if (!is.null(tileJobId) && tileJobId == 1L) {
  tryCatch({
    origRasterList <- extractRasters(originalPredictingRasterDataframe, column = 2)
    if (isTRUE(overrideBandnames) && !is.null(bandLabelFile)) {
      origRasterList <- overrideBandNames(origRasterList, bandLabelFile)
    }
    for (.t in seq_along(origRasterList)) {
      .ts <- timestepList[.t]
      classifiedRgbOutputDataframe <- getRgbDataframe(
        classifiedRgbOutputDataframe,
        category = "predicting",
        timestep = .ts,
        transferDir
      )
      saveFiles(
        classifiedPresencePath,
        classifiedProbabilityPath,
        origRasterList[[.t]],
        groundTruth = NULL,
        category = "predicting",
        timestep = .ts,
        transferDir,
        rgbBands = rgbBands
      )
    }
    updateRunLog(
      "Tile 1: generated full-extent RGB image from original predicting rasters.",
      type = "info"
    )
  }, error = function(e) {
    updateRunLog(
      paste0("Tile 1: could not generate full-extent RGB image: ", conditionMessage(e)),
      type = "warning"
    )
  })
}

# Save dataframes back to SyncroSim library's output datasheets ----------------

saveDatasheet(
  myScenario,
  data = classifiedRasterOutputDataframe,
  name = "ecoClassify_ClassifiedRasterOutput"
)

if (is.null(tileJobId) || tileJobId == 1L) {
  saveDatasheet(
    myScenario,
    data = classifiedRgbOutputDataframe,
    name = "ecoClassify_ClassifiedRgbOutput"
  )
}

if (length(summaryRows) > 0) {
  if (!is.null(tileJobId) && !is.null(tilesDataDir) && !is.null(tileCount)) {
    # --- Tiling: save intermediate per-tile stats and aggregate in tile job 1 ---

    # Write this tile's summary rows to an intermediate RDS file in tilesDataDir
    # so that tile job 1 can read and aggregate all tiles' stats.
    .tileStatsPath <- file.path(
      tilesDataDir, paste0("tile_", tileJobId),
      "summary_predicting.rds"
    )
    dir.create(dirname(.tileStatsPath), recursive = TRUE, showWarnings = FALSE)
    saveRDS(do.call(rbind, summaryRows), .tileStatsPath)
    updateRunLog(
      sprintf("Tile %d: saved per-tile summary stats.", tileJobId),
      type = "info"
    )

    if (tileJobId == 1L) {
      # Tile job 1 waits for all other tiles to write their intermediate files,
      # then aggregates and saves the combined result to SummaryOutput.
      # Poll every 2 seconds up to 120 seconds.
      .maxWait <- 120L
      .waited  <- 0L
      .allFiles <- character(0)
      repeat {
        .allFiles <- list.files(
          tilesDataDir,
          pattern    = "^summary_predicting\\.rds$",
          recursive  = TRUE,
          full.names = TRUE
        )
        if (length(.allFiles) >= tileCount || .waited >= .maxWait) break
        Sys.sleep(2)
        .waited <- .waited + 2L
      }

      if (length(.allFiles) < tileCount) {
        updateRunLog(
          sprintf(
            "Tile 1: only %d of %d tile summary file(s) found after %ds; saving available stats.",
            length(.allFiles), tileCount, .maxWait
          ),
          type = "warning"
        )
      }

      .allStats <- do.call(rbind, lapply(.allFiles, readRDS))

      .sum_na  <- function(x) if (all(is.na(x))) NA_real_ else sum(x,  na.rm = TRUE)
      .mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

      .aggregated <- do.call(rbind, lapply(
        split(.allStats, list(.allStats$Timestep, .allStats$PredictionType), drop = TRUE),
        function(grp) {
          data.frame(
            Timestep          = grp$Timestep[1],
            TargetClassValue  = if ("TargetClassValue"  %in% names(grp)) grp$TargetClassValue[1]  else NA,
            TargetClassLabel  = if ("TargetClassLabel"  %in% names(grp)) grp$TargetClassLabel[1]  else NA,
            PredictionType    = grp$PredictionType[1],
            PredictedPixels   = if ("PredictedPixels"   %in% names(grp)) .sum_na(grp$PredictedPixels)    else NA_real_,
            PredictedArea     = if ("PredictedArea"     %in% names(grp)) .sum_na(grp$PredictedArea)      else NA_real_,
            MeanProbability   = if ("MeanProbability"   %in% names(grp)) .mean_na(grp$MeanProbability)   else NA_real_,
            MedianProbability = if ("MedianProbability" %in% names(grp)) .mean_na(grp$MedianProbability) else NA_real_,
            MinProbability    = if ("MinProbability"    %in% names(grp)) .mean_na(grp$MinProbability)    else NA_real_,
            MaxProbability    = if ("MaxProbability"    %in% names(grp)) .mean_na(grp$MaxProbability)    else NA_real_,
            stringsAsFactors  = FALSE
          )
        }
      ))
      rownames(.aggregated) <- NULL

      saveDatasheet(myScenario, data = .aggregated, name = "ecoClassify_SummaryOutput")
      updateRunLog(
        sprintf(
          "Tile 1: aggregated summary from %d tile(s) and saved to SummaryOutput.",
          length(.allFiles)
        ),
        type = "info"
      )
    }
    # Tiles 2-N do not write to SummaryOutput; tile 1 handles the combined result.

  } else {
    # Non-tiling: save directly as before
    saveDatasheet(
      myScenario,
      data = do.call(rbind, summaryRows),
      name = "ecoClassify_SummaryOutput"
    )
  }
}
