## ---------------------------------------
## ecoClassify - Predict Using Classifier
## ApexRMS, November 2024
## ---------------------------------------

# Set up workspace -------------------------------------------------------------

packageDir <- (Sys.getenv("ssim_package_directory"))

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
if (normalizeRasters == TRUE) {
  predictRasterList <- normalizeRaster(predictRasterList)
}

# Apply contextualization to prediction rasters, if selected
if (applyContextualization == TRUE) {
  predictRasterList <- contextualizeRaster(predictRasterList)
}

# Extract covariate rasters and convert to correct data type
predictingCovariateRaster <- processCovariates(
  predictingCovariateDataframe,
  modelType
)

# Add covariate data to predicting rasters
predictRasterList <- addCovariates(
  predictRasterList,
  predictingCovariateRaster
)

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

# Predict presence for rasters to classify -------------------------------------

progressBar(type = "message", message = "Predicting")

for (t in seq_along(predictRasterList)) {
  # Get timestep for the current raster
  timestep <- timestepList[t]

  classifiedRasters <- getPredictionRasters(
    predictRasterList[[t]],
    model,
    threshold,
    modelType
  )
  classifiedPresence <- classifiedRasters[[1]]
  classifiedProbability <- classifiedRasters[[2]]

  # Generate rasterDataframe based on filtering argument
  classifiedRasterOutputDataframe <- generateRasterDataframe(
    classifiedPresence,
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

  # Save files
  saveFiles(
    classifiedPresence,
    groundTruth = NULL,
    classifiedProbability,
    predictRasterList,
    category = "predicting",
    timestep,
    transferDir
  )
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
