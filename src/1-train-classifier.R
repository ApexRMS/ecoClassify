## -------------------------------
## ecoClassify - Train Classifier
## ApexRMS, November 2024
## -------------------------------

# set up workspace ---------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))

sourceScripts <- list.files(
  path = file.path(packageDir, "/functions"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

invisible(lapply(sourceScripts, source))

myScenario <- scenario() # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load raster input datasheets -----------------------------------------------
trainingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputTrainingRasters"
)

trainingCovariateDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputTrainingCovariates"
)

classifierOptions <- datasheet(
  myScenario,
  name = "ecoClassify_ClassifierOptions"
)

# Set the random seed if provided
if (!is.null(classifierOptions$setSeed) && !is.na(classifierOptions$setSeed)) {
  set.seed(classifierOptions$setSeed)
}

# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(
  myScenario,
  trainingRasterDataframe,
  trainingRasterDataframe$TrainingRasterFile
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


## check if multiprocessing is selected
mulitprocessingSheet <- datasheet(myScenario, "core_Multiprocessing")
nCores <- setCores(mulitprocessingSheet)

# extract list of training and ground truth rasters ----------------
trainingRasterList <- extractRasters(trainingRasterDataframe, column = 2)

groundTruthRasterList <- extractRasters(trainingRasterDataframe, column = 3)

# normalize training rasters if selected -------------------------------------
if (normalizeRasters == TRUE) {
  trainingRasterList <- normalizeRaster(trainingRasterList)
}

# round rasters to integer if selected ----------------------------------
if (
  is.numeric(rasterDecimalPlaces) &&
    length(rasterDecimalPlaces) > 0 &&
    !is.na(rasterDecimalPlaces)
) {
  roundedRasters <- lapply(trainingRasterList, function(r) {
    return(app(r, fun = function(x) round(x, rasterDecimalPlaces)))
  })
  trainingRasterList <- roundedRasters
}

# apply contextualization to training rasters if selected ---------------------
if (applyContextualization == TRUE) {
  trainingRasterList <- contextualizeRaster(trainingRasterList)
}

# reclassify ground truth rasters --------------------------------------------
groundTruthRasterList <- reclassifyGroundTruth(groundTruthRasterList)

# extract covariate rasters and convert to correct data type -----------------
trainingCovariateRaster <- processCovariates(
  trainingCovariateDataframe,
  modelType
)

# add covariate data to training rasters -------------------------------------
trainingRasterList <- addCovariates(
  trainingRasterList,
  trainingCovariateRaster
)

# check and mask NA values in training rasters -------------------
trainingRasterList <- checkAndMaskNA(trainingRasterList)

# Extract raster values for diagnostics
rastLayerHistogram <- getRastLayerHistogram(trainingRasterList)

# round rasters to integer if selected ----------------------------------
if (
  is.numeric(rasterDecimalPlaces) &&
    length(rasterDecimalPlaces) > 0 &&
    !is.na(rasterDecimalPlaces)
) {
  roundedRasters <- lapply(trainingRasterList, function(r) {
    return(app(r, fun = function(x) round(x, rasterDecimalPlaces)))
  })
  trainingRasterList <- roundedRasters
}

# Setup empty dataframes to accept output in SyncroSim datasheet format ------
rasterOutputDataframe <- data.frame(
  Timestep = numeric(0),
  PredictedUnfiltered = character(0),
  PredictedFiltered = character(0),
  GroundTruth = character(0),
  Probability = character(0)
)

confusionOutputDataframe <- data.frame(
  Prediction = numeric(0),
  Reference = numeric(0),
  Frequency = numeric(0)
)

modelOutputDataframe <- data.frame(Statistic = character(0), Value = numeric(0))

rgbOutputDataframe <- data.frame(Timestep = numeric(0), RGBImage = character(0))

# separate training and testing data -------------------------------------------
splitData <- splitTrainTest(
  trainingRasterList,
  groundTruthRasterList,
  nObs
)
allTrainData <- splitData[[1]]
allTestData <- splitData[[2]]

## Train model -----------------------------------------------------------------
if (modelType == "MaxEnt") {
  modelOut <- getMaxentModel(allTrainData, nCores, modelTuning)
  if (setManualThreshold == FALSE) {
    threshold <- getOptimalThreshold(modelOut, allTestData, "MaxEnt")
  } else {
    threshold <- manualThreshold
  }
} else if (modelType == "Random Forest") {
  modelOut <- getRandomForestModel(allTrainData, nCores, modelTuning)
  if (setManualThreshold == FALSE) {
    threshold <- getOptimalThreshold(
      modelOut,
      allTestData,
      "Random Forest"
    )
  } else {
    threshold <- manualThreshold
  }
} else if (modelType == "CNN") {
  modelOut <- getCNNModel(allTrainData, nCores, modelTuning)
  if (setManualThreshold == FALSE) {
    threshold <- getOptimalThreshold(
      modelOut,
      allTestData,
      "CNN"
    )
  } else {
    threshold <- manualThreshold
  }
} else {
  stop("Model type not recognized")
}
model <- modelOut[[1]]
variableImportance <- modelOut[[2]]

if (modelType == "CNN") {
  # Save Torch weights separately
  model_weights_path <- file.path(transferDir, "model_weights.pt")
  torch::torch_save(modelOut$model$state_dict(), model_weights_path)

  # Save non-torch metadata
  metadata <- modelOut[names(modelOut) != "model"]
  metadata_path <- file.path(transferDir, "model_metadata.rds")
  saveRDS(metadata, metadata_path)
} else {
  model_path <- file.path(transferDir, "model.rds")
  saveRDS(modelOut, model_path)
}
# add to output datasheet
if (modelType == "CNN") {
  modelObjectOutputDataframe <- data.frame(
    Model = metadata_path,
    Threshold = threshold,
    Weights = model_weights_path
  )
} else {
  modelObjectOutputDataframe <- data.frame(
    # TODO: add warning if missing present/absent and threshold == 0
    Model = model_path,
    Threshold = threshold,
    Weights = ""
  )
}
# save model object to output datasheet
variableImportanceOutput <- plotVariableImportance(
  variableImportance,
  transferDir
)

variableImportancePlot <- variableImportanceOutput[[1]]
varImportanceOutputImage <- variableImportanceOutput[[2]]

# generate dataframe
varImportanceOutputDataframe <- as.data.frame(variableImportance) %>%
  tibble::rownames_to_column("Variable") %>%
  rename(Importance = "variableImportance")

## Predict presence for training rasters in each timestep group ----------------
for (t in seq_along(trainingRasterList)) {
  # get timestep for the current raster
  timestep <- timestepList[t]

  if (modelType == "CNN") {
    predictionRasters <- getPredictionRasters(
      trainingRasterList[[t]],
      modelOut,
      threshold,
      modelType
    )
  } else if (modelType == "Random Forest" || modelType == "MaxEnt") {
    predictionRasters <- getPredictionRasters(
      trainingRasterList[[t]],
      modelOut,
      threshold,
      modelType
    )
  }
  predictedPresence <- predictionRasters[[1]]
  probabilityRaster <- predictionRasters[[2]]

  # generate rasterDataframe based on filtering argument
  rasterOutputDataframe <- generateRasterDataframe(
    applyFiltering,
    predictedPresence,
    filterResolution,
    filterPercent,
    category = "training",
    timestep,
    transferDir,
    rasterOutputDataframe,
    hasGroundTruth = TRUE
  )

  # define GroundTruth raster
  groundTruth <- groundTruthRasterList[[t]]

  # define RGB data frame
  rgbOutputDataframe <- getRgbDataframe(
    rgbOutputDataframe,
    category = "training",
    timestep,
    transferDir
  )

  # save files
  saveFiles(
    predictedPresence,
    groundTruth,
    probabilityRaster,
    trainingRasterList,
    category = "training",
    timestep,
    transferDir
  )
}

# Predict response based on range of values
responseHistogram <- predictResponseHistogram(
  rastLayerHistogram,
  modelOut,
  modelType
)

histogramJoin <- responseHistogram %>%
  left_join(rastLayerHistogram, by = c("layer", "predictor" = "bin_lower"))

plotLayerHistogram(histogramJoin, transferDir)

# add to output datasheet
layerHistogramPlotOutputDataframe <- data.frame(
  LayerHistogramPlot = file.path(paste0(
    transferDir,
    "/LayerHistogramResponse.png"
  ))
)

# calculate mean values for model statistics -----------------------------------
outputDataframes <- calculateStatistics(
  modelOut,
  allTestData,
  threshold,
  confusionOutputDataframe,
  modelOutputDataframe
)

confusionOutputDataframe <- outputDataframes[[1]]
modelOutputDataframe <- outputDataframes[[2]]
confusionMatrixPlot <- outputDataframes[[3]]

# generate model chart dataframe ----------------------------------------------
modelChartDataframe <- data.frame(
  Accuracy = modelOutputDataframe %>%
    filter(Statistic == "accuracy") %>%
    pull(Value),
  Precision = modelOutputDataframe %>%
    filter(Statistic == "precision") %>%
    pull(Value),
  Sensitivity = modelOutputDataframe %>%
    filter(Statistic == "sensitivity") %>%
    pull(Value),
  Specificity = modelOutputDataframe %>%
    filter(Statistic == "specificity") %>%
    pull(Value)
)

# make a confusion matrix output dataframe
ggsave(
  filename = file.path(paste0(transferDir, "/ConfusionMatrixPlot.png")),
  confusionMatrixPlot
)

# add to output datasheet
confusionMatrixPlotOutputDataframe <- data.frame(
  ConfusionMatrixPlot = file.path(paste0(
    transferDir,
    "/ConfusionMatrixPlot.png"
  ))
)

# save filter resolution and threshold to input datasheet ----------------------
filterOutputDataframe <- data.frame(
  applyFiltering = applyFiltering,
  filterResolution = filterResolution,
  filterPercent = filterPercent
)

if (is.null(rasterDecimalPlaces)) {
  rasterDecimalPlaces <- ""
}
if (is.null(contextualizationWindowSize)) {
  contextualizationWindowSize <- ""
}

classifierOptionsOutputDataframe <- data.frame(
  nObs = format(nObs, scientific = FALSE),
  normalizeRasters = normalizeRasters,
  rasterDecimalPlaces = rasterDecimalPlaces,
  modelType = modelType,
  modelTuning = modelTuning,
  setManualThreshold = setManualThreshold,
  manualThreshold = threshold,
  applyContextualization = applyContextualization,
  contextualizationWindowSize = contextualizationWindowSize
)

# Save dataframes back to SyncroSim library's output datasheets ----------------

saveDatasheet(
  myScenario,
  data = filterOutputDataframe,
  name = "ecoClassify_PostProcessingOptions"
)

saveDatasheet(
  myScenario,
  data = classifierOptionsOutputDataframe,
  name = "ecoClassify_ClassifierOptions"
)

saveDatasheet(
  myScenario,
  data = rasterOutputDataframe,
  name = "ecoClassify_RasterOutput"
)

saveDatasheet(
  myScenario,
  data = confusionOutputDataframe,
  name = "ecoClassify_ConfusionMatrix"
)

saveDatasheet(
  myScenario,
  data = modelOutputDataframe,
  name = "ecoClassify_ModelStatistics"
)


saveDatasheet(
  myScenario,
  data = varImportanceOutputImage,
  name = "ecoClassify_VariableImportanceOutput"
)

saveDatasheet(
  myScenario,
  data = rgbOutputDataframe,
  name = "ecoClassify_RgbOutput"
)

saveDatasheet(
  myScenario,
  data = modelObjectOutputDataframe,
  name = "ecoClassify_ModelObject"
)

saveDatasheet(
  myScenario,
  data = confusionMatrixPlotOutputDataframe,
  name = "ecoClassify_ConfusionMatrixPlotOutput"
)

saveDatasheet(
  myScenario,
  data = layerHistogramPlotOutputDataframe,
  name = "ecoClassify_LayerHistogramPlotOutput"
)

saveDatasheet(
  myScenario,
  data = varImportanceOutputDataframe,
  name = "ecoClassify_VariableImportanceOutputDataframe"
)

saveDatasheet(
  myScenario,
  data = modelChartDataframe,
  name = "ecoClassify_ModelChartData"
)
