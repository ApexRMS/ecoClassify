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

# Extract covariate rasters and convert to correct data type
predictingCovariateRaster <- processCovariates(
  predictingCovariateDataframe,
  modelType
)

# Add covariate data to predicting rasters
predictRasterList <- addCovariates(predictRasterList, predictingCovariateRaster)

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

  # Define RGB data frame
  classifiedRgbOutputDataframe <- getRgbDataframe(
    classifiedRgbOutputDataframe,
    category = "predicting",
    timestep,
    transferDir
  )

  # Save files (ground truth and RGB only - predictions already saved)
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

  # Accumulate summary row for this timestep
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
}

# Save dataframes back to SyncroSim library's output datasheets ----------------

saveDatasheet(
  myScenario,
  data = classifiedRasterOutputDataframe,
  name = "ecoClassify_ClassifiedRasterOutput"
)

saveDatasheet(
  myScenario,
  data = classifiedRgbOutputDataframe,
  name = "ecoClassify_ClassifiedRgbOutput"
)

if (length(summaryRows) > 0) {
  saveDatasheet(
    myScenario,
    data = do.call(rbind, summaryRows),
    name = "ecoClassify_SummaryOutput"
  )
}
