## ---------------------------------------
## ecoClassify - Predict Using Classifier
## ApexRMS, November 2024
## ---------------------------------------

# set up workspace ---------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))

sourceScripts <- list.files(
  path = file.path(packageDir, "/functions"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

invisible(lapply(sourceScripts, source))

# Set up -------------------------------------------------------------------
myScenario <- scenario() # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load raster input datasheets -----------------------------------------------
predictingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputPredictingRasters"
)

predictingCovariateDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputPredictingCovariates"
)

modelObjectDataframe <- datasheet(myScenario, name = "ecoClassify_ModelObject")

# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(
  myScenario,
  predictingRasterDataframe,
  predictingRasterDataframe$predictingRasterFile
)
timestepList <- inputVariables[[1]]
nObs <- inputVariables[[2]]
filterResolution <- inputVariables[[3]] # TO DO: give warnings for lower limits (must be >=1?)
filterPercent <- inputVariables[[4]]
applyFiltering <- inputVariables[[5]]
applyContextualization <- inputVariables[[6]]
contextualizationWindowSize <- inputVariables[[7]]
modelType <- inputVariables[[8]]
modelTuning <- inputVariables[[9]]
setManualThreshold <- inputVariables[[10]]
manualThreshold <- inputVariables[[11]]
normalizeRasters <- inputVariables[[12]]
rasterDecimalPlaces <- inputVariables[[13]]

# load model and threshold
if (modelType == "CNN") {
  model <- loadCNNModel(
    weights_path = modelObjectDataframe$Weights,     # e.g. "model_weights.pt"
    metadata_path = modelObjectDataframe$Model    # e.g. "model_metadata.rds"
  )
} else if (modelType == "Random Forest" || modelType == "MaxEnt") {
  model <- readRDS(modelObjectDataframe$Model)
}

if (setManualThreshold == FALSE) {
  threshold <- modelObjectDataframe$Threshold
} else {
  threshold <- manualThreshold
}

# extract list of testing rasters --------------------------------------------
predictRasterList <- extractRasters(predictingRasterDataframe, column = 2)

# round rasters to integer if selected ----------------------------------
if (is.numeric(rasterDecimalPlaces) && length(rasterDecimalPlaces) > 0 && !is.na(rasterDecimalPlaces)) {
  roundedRasters <- lapply(predictRasterList, function(r) {
    return(app(r, fun = function(x) round(x, rasterDecimalPlaces)))
  })
  predictRasterList <- roundedRasters
}

# normalize predicting rasters if selected -------------------------------------
if (normalizeRasters == TRUE) {
  predictRasterList <- normalizeRaster(predictRasterList)
}

# apply contextualization to prediction rasters if selected ---------------------
if (applyContextualization == TRUE) {
  predictRasterList <- contextualizeRaster(predictRasterList)
}

# extract covariate rasters and convert to correct data type -----------------
predictingCovariateRaster <- processCovariates(predictingCovariateDataframe,
                                               modelType)

# add covariate data to predicting rasters -------------------------------------
predictRasterList <- addCovariates(
  predictRasterList,
  predictingCovariateRaster
)

# check and mask NA values in predicting rasters -------------------
checkNA(predictRasterList)

# Setup empty dataframes to accept output in SyncroSim datasheet format ------
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

## Predict presence for rasters to classify ------------------------------------
for (t in seq_along(predictRasterList)) {
  # get timestep for the current raster
  timestep <- timestepList[t]

  classifiedRasters <- getPredictionRasters(
    predictRasterList[[t]],
    model,
    threshold,
    modelType
  )
  classifiedPresence <- classifiedRasters[[1]]
  classifiedProbability <- classifiedRasters[[2]]

  # generate rasterDataframe based on filtering argument
  classifiedRasterOutputDataframe <- generateRasterDataframe(
    applyFiltering,
    classifiedPresence,
    filterResolution,
    filterPercent,
    category = "predicting",
    timestep,
    transferDir,
    classifiedRasterOutputDataframe,
    hasGroundTruth = FALSE
  )

  # define RGB data frame
  classifiedRgbOutputDataframe <- getRgbDataframe(
    classifiedRgbOutputDataframe,
    category = "predicting",
    timestep,
    transferDir
  )

  # save files
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
