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

# Send temporary files to a folder that is removed when the run ends
runTempDir <- createRunTempDir(transferDir)

# --- Tile job detection -------------------------------------------------------
# SyncroSim names each spatial MP job library "Job-N.ssim". Detect that here
# and load the tile manifest written by the prep transformer.
tileJobId     <- NULL
tileRasterMap <- NULL
tileExtent    <- NULL
tileCropExtent <- NULL
fullExtent    <- NULL
buffer_px     <- 0L

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

    # Manifest v2 gives the buffered crop extent; v1 manifests listed
    # pre-cropped copies of each raster instead (orig_path -> cropped_path)
    tileRasterMap <- .tileInfo$raster_map
    .te <- .tileInfo$tile_extent
    tileExtent <- terra::ext(.te$xmin, .te$xmax, .te$ymin, .te$ymax)
    .ce <- .tileInfo$crop_extent
    if (!is.null(.ce)) {
      tileCropExtent <- terra::ext(.ce$xmin, .ce$xmax, .ce$ymin, .ce$ymax)
    }
    .fe <- .manifest$full_extent
    fullExtent <- terra::ext(.fe$xmin, .fe$xmax, .fe$ymin, .fe$ymax)
    buffer_px    <- if (!is.null(.manifest$buffer_px)) as.integer(.manifest$buffer_px) else 0L

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
terminology <- parseTerminology(terminologyDataframe)
bandLabelFile <- terminology$bandLabelFile
rgbBands <- terminology$rgbBands

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

# Load model-type specific packages
loadModelPackages(modelType)

modelTuning <- inputVariables[[6]]
setManualThreshold <- inputVariables[[7]]
manualThreshold <- inputVariables[[8]]
normalizeRasters <- inputVariables[[9]]
rasterDecimalPlaces <- inputVariables[[10]]
tuningObjective <- inputVariables[[11]]
overrideBandnames <- inputVariables[[12]]

# Read TargetClassOptions ------------------------------------------------------

targetClassOptions <- parseTargetClassOptions(
  datasheet(myScenario, "ecoClassify_TargetClassOptions")
)
targetClassValue <- targetClassOptions$targetClassValue
targetClassLabel <- targetClassOptions$targetClassLabel

# Load model and threshold (first row; validateModelObject() warns if more)
validateModelObject(modelObjectDataframe, modelType)
modelObjectDataframe <- modelObjectDataframe[1, , drop = FALSE]

if (modelType == "CNN") {
  model <- loadCNNModel(
    weights_path = modelObjectDataframe$Weights, # e.g. "model_weights.pt"
    metadata_path = modelObjectDataframe$Model # e.g. "model_metadata.rds"
  )
} else if (modelType == "Random Forest" || modelType == "MaxEnt") {
  model <- readRDS(modelObjectDataframe$Model)
} else {
  stop("Model type not recognized: ", modelType)
}
checkModelType(model, modelType)

if (modelType == "CNN" && is.null(model$num_center)) {
  updateRunLog(
    paste0(
      "This CNN model was trained with an earlier ecoClassify version that ",
      "did not standardize its inputs. It will predict as before; retrain ",
      "it for better-fitting models and comparable variable importance."
    ),
    type = "warning"
  )
}

if (!isTRUE(setManualThreshold)) {
  threshold <- modelObjectDataframe$Threshold
} else {
  threshold <- manualThreshold
}

# Extract list of testing rasters ----------------------------------------------

# Preserve original raster paths (before tile substitution) so tile job 1 can
# generate the full-extent RGB from the un-cropped source rasters.
originalPredictingRasterDataframe <- predictingRasterDataframe

# For tile jobs with a v1 manifest, substitute the pre-cropped raster paths
if (!is.null(tileJobId) && is.null(tileCropExtent) && !is.null(tileRasterMap)) {
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

# Tile jobs read only their own (buffered) part of the original rasters
if (!is.null(tileJobId) && !is.null(tileCropExtent)) {
  predictRasterList <- cropRastersToTile(predictRasterList, tileCropExtent)
  updateRunLog(
    sprintf("Tile %d: cropped predicting rasters to the tile extent.",
            tileJobId),
    type = "info"
  )
}

# extractRasters() skips rows with no raster file, so re-align timestepList with
# the rasters that were actually loaded. Outputs below are labelled positionally
# (timestepList[t] for predictRasterList[[t]]).
timestepList <- alignTimestepList(
  timestepList,
  predictRasterList,
  context = "predicting rasters"
)

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
  # terra's native round() (as in training) avoids calling back into R per
  # block the way app() does
  predictRasterList <- lapply(predictRasterList, function(r) {
    round(r, digits = rasterDecimalPlaces)
  })
}

# Pre-processing fitted at training time (saved with the model). Models trained
# before v2.5 have no `preprocessing` element; they fall back to fitting the
# normalization/PCA on the prediction rasters, which reproduces the old
# behaviour but can shift features relative to training.
modelPreprocessing <- model$preprocessing

# Normalize predicting rasters, if selected
if (isTRUE(normalizeRasters)) {
  normalizationStats <- modelPreprocessing$normalizationStats
  if (is.null(normalizationStats)) {
    updateRunLog(
      paste0(
        "The trained model has no saved normalization statistics (trained ",
        "with an earlier ecoClassify version, or without normalization). ",
        "Normalizing with statistics from the predicting rasters instead; ",
        "retrain the model for consistent results."
      ),
      type = "warning"
    )
    normalizationStats <- computeNormalizationStats(predictRasterList)
  }
  predictRasterList <- normalizeRaster(predictRasterList, normalizationStats)
}

# Apply contextualization to prediction rasters, if selected
if (isTRUE(applyContextualization)) {
  contextPcaModel <- modelPreprocessing$contextPcaModel
  if (is.null(contextPcaModel)) {
    updateRunLog(
      paste0(
        "The trained model has no saved contextualization PCA (trained with ",
        "an earlier ecoClassify version, or without contextualization). ",
        "Fitting the PCA on the predicting rasters instead; retrain the model ",
        "for consistent results."
      ),
      type = "warning"
    )
    contextPcaModel <- fitContextPca(predictRasterList)
  }
  predictRasterList <- contextualizeRaster(
    predictRasterList,
    windowSize = contextualizationWindowSize,
    pcaModel = contextPcaModel
  )
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
predictingCovariateList <- processCovariates(
  predictingCovariateDataframe,
  modelType
)

# Crop all per-timestep covariates to the tile extent so they align with
# the pre-cropped predicting rasters.
if (!is.null(tileJobId) && !is.null(tileExtent) && !is.null(predictingCovariateList)) {
  predictingCovariateList <- lapply(
    predictingCovariateList,
    function(r) terra::crop(r, tileExtent)
  )
}

# Add covariate data to predicting rasters.
# Combined files go to the run's temporary folder rather than R's tempdir().
predictRasterList <- addCovariates(predictRasterList, predictingCovariateList, runTempDir)

# Fail early, with a clear message, if a predictor the model needs is missing
checkPredictorNames(predictRasterList, model, timesteps = timestepList)

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

  # Accumulate summary row for this timestep. Tile jobs skip this: a tile only
  # sees part of the map, so the summary for tiled runs is computed from the
  # merged rasters by the 2c-Summarize Tiles transformer instead.
  if (is.null(tileJobId)) tryCatch({
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
      rm(.r_extended)
    }
    updateRunLog(
      sprintf("Tile %d: extended prediction outputs to full extent (timestep %s).",
              tileJobId, timestep),
      type = "info"
    )
  }
  predictRasterList[t] <- list(NULL)
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
      origRasterList[.t] <- list(NULL)
    }
    rm(origRasterList)
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

# Tile jobs build no summary rows (see above), so this only runs for
# non-tiled predictions
if (length(summaryRows) > 0) {
  summaryDf <- do.call(rbind, summaryRows)
  saveDatasheet(myScenario, data = summaryDf, name = "ecoClassify_SummaryOutput")
  saveDatasheet(myScenario, data = summaryDf, name = "ecoClassify_SummaryOutputChart")
}

removeRunTempDir()
