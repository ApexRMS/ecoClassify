## -------------------------------
## ecoClassify - Train Classifier
## ApexRMS, November 2024
## -------------------------------

# set up workspace ---------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))
source(file.path(packageDir, "0-helper-functions.r"))

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
modelType <- inputVariables[[7]]
modelTuning <- inputVariables[[8]]
setManualThreshold <- inputVariables[[9]]
manualThreshold <- inputVariables[[10]]
normalizeRasters <- inputVariables[[11]]

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

# reclassify ground truth rasters --------------------------------------------
groundTruthRasterList <- reclassifyGroundTruth(groundTruthRasterList)

# add covariate data to training rasters -------------------------------------
trainingRasterList <- addCovariates(
  trainingRasterList,
  trainingCovariateDataframe
)

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
  ifelse(
    applyContextualization == FALSE,
    trainingRasterList,
    contextualizeRaster(trainingRasterList) ## add contextualization if selected
  ),
  groundTruthRasterList,
  nObs
)
allTrainData <- splitData[[1]]
allTestData <- splitData[[2]]
head(allTestData)
## Train model -----------------------------------------------------------------
if (modelType == "MaxEnt") {
  modelOut <- getMaxentModel(allTrainData, nCores, modelTuning)
  if (setManualThreshold == FALSE) {
    threshold <- getOptimalThreshold(modelOut[[1]], allTestData, "MaxEnt")
  } else {
    threshold <- manualThreshold
  }
} else if (modelType == "Random Forest") {
  modelOut <- getRandomForestModel(allTrainData, nCores, modelTuning)
  if (setManualThreshold == FALSE) {
    threshold <- getOptimalThreshold(
      modelOut[[1]],
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
      modelOut[[1]],
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

# save model
if (modelType == "CNN") {
  torch::torch_save(net, file.path(transferDir, "model.pt"))
} else {
  saveRDS(model, file.path(transferDir, "model.rds"))
}
# add to output datasheet
modelObjectOutputDataframe <- data.frame(
  Model = file.path(transferDir, "model.rds"),
  Threshold = threshold
)

# extract variable importance plot and data frame ------------------------------
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

  predictionRasters <- getPredictionRasters(
    trainingRasterList[[t]],
    model,
    threshold,
    modelType
  )
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

# calculate mean values for model statistics -----------------------------------
outputDataframes <- calculateStatistics(
  model,
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
    filter(Statistic == "Accuracy") %>%
    pull(Value),
  Precision = modelOutputDataframe %>%
    filter(Statistic == "Precision") %>%
    pull(Value),
  Sensitivity = modelOutputDataframe %>%
    filter(Statistic == "Sensitivity") %>%
    pull(Value),
  Specificity = modelOutputDataframe %>%
    filter(Statistic == "Specificity") %>%
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

classifierOptionsOutputDataframe <- data.frame(
  nObs = nObs,
  normalizeRasters = normalizeRasters,
  modelType = modelType,
  modelTuning = modelTuning,
  setManualThreshold = setManualThreshold,
  manualThreshold = threshold,
  applyContextualization = applyContextualization
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
  data = varImportanceOutputDataframe,
  name = "ecoClassify_VariableImportanceOutputDataframe"
)

saveDatasheet(
  myScenario,
  data = modelChartDataframe,
  name = "ecoClassify_ModelChartData"
)
