# # set up library (remove after testing) -----------------------------------
# library(rsyncrosim)
# mySession <- session("C:/Program Files/SyncroSim Studio")
# libPath <- "library/image_classifier_testing.ssim"
# # libPath <- "C:/Users/HannahAdams/Documents/Projects/A332 UofT - UPA Mapping/UPA-testing.ssim"

# myLibrary <- ssimLibrary(name = libPath,
#                          session = mySession)

# # define project
# myProject <- rsyncrosim::project(myLibrary, project = 1)

# # define scenario
# scenario(myProject)
# myScenario <- scenario(myProject, scenario = 202)

# # view datasheets
# datasheet(myScenario)
# source("imageclassifier/workspace.r")

# # transferDir <- ""
# transferDir <- "C:/Users/HannahAdams/OneDrive - Apex Resource Management Solutions Ltd/Desktop/watchtower-testing"

# START OF MODEL SCRIPT:
## SKIP OUTSIDE GUI
# set up workspace ---------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))
source(file.path(packageDir, "workspace.r"))

# Set up -------------------------------------------------------------------
myScenario <- scenario()  # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
## CONTINUE HERE
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load raster input datasheets -----------------------------------------------
trainingRasterDataframe <- datasheet(myScenario,
                                     name = "imageclassifier_InputTrainingRasters")

trainingCovariateDataframe <- datasheet(myScenario,
                                        name = "imageclassifier_InputTrainingCovariates")
 {
  print("empty")
}
# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(myScenario,
                                  trainingRasterDataframe,
                                  trainingRasterDataframe$TrainingRasterFile)
timestepList <- inputVariables[[1]]
nObs <- inputVariables[[2]]
filterResolution <- inputVariables[[3]] # TO DO: give warnings for lower limits (must be >=1?)
filterPercent <- inputVariables[[4]]
applyFiltering <- inputVariables[[5]]
applyContextualization <- inputVariables[[6]]
modelType <- inputVariables[[7]]
modelTuning <- inputVariables[[8]]

## check if multiprocessing is selected
mulitprocessingSheet <- datasheet(myScenario, "core_Multiprocessing")
nCores <- setCores(mulitprocessingSheet)

# check timesteps were input correctly ---------------------------------------
# checkTimesteps(timesteps,
#                rasterTrainingDataframe,
#                rasterGroundTruthDataframe)

# extract list of training, testing, and ground truth rasters ----------------
trainingRasterList <- extractRasters(trainingRasterDataframe,
                                     trainingCovariateDataframe,
                                     column = 2)
groundTruthRasterList <- extractRasters(trainingRasterDataframe,
                                        trainingCovariateDataframe,
                                        column = 3)

# reclassify ground truth rasters --------------------------------------------
groundTruthRasterList <- reclassifyGroundTruth(groundTruthRasterList)

# Setup empty dataframes to accept output in SyncroSim datasheet format ------
rasterOutputDataframe <- data.frame(Timestep = numeric(0),
                                    PredictedUnfiltered = character(0),
                                    PredictedFiltered = character(0),
                                    GroundTruth = character(0),
                                    Probability = character(0))

confusionOutputDataframe <- data.frame(Prediction = numeric(0),
                                       Reference = numeric(0),
                                       Frequency = numeric(0))

modelOutputDataframe <- data.frame(Statistic = character(0),
                                   Value = numeric(0))

rgbOutputDataframe <- data.frame(Timestep = numeric(0),
                                 RGBImage = character(0))

# add contextualization if selected --------------------------------------------

if (applyContextualization == TRUE) {

  trainingRasterList <- contextualizeRaster(trainingRasterList) # change naming to avoid this

}

# separate training and testing data -------------------------------------------
splitData <- splitTrainTest(trainingRasterList,
                            groundTruthRasterList,
                            nObs)
allTrainData <- splitData[[1]]
allTestData <- splitData[[2]]

## Train model -----------------------------------------------------------------
if (modelType == "MaxEnt") {
  modelOut <- getMaxentModel(allTrainData, nCores, modelTuning)
  optimalThreshold <-  getOptimalThreshold(modelOut[[1]], allTestData, "MaxEnt")
} else if (modelType == "Random Forest") {
  modelOut <- getRandomForestModel(allTrainData, nCores, modelTuning)
  optimalThreshold <-  getOptimalThreshold(modelOut[[1]], allTestData, "Random Forest")
} else {
  stop("Model type not recognized")
}
model <- modelOut[[1]]
variableImportance <- modelOut[[2]]

# save model
saveRDS(model, file.path(transferDir, "model.rds"))

# add to output datasheet
modelObjectOutputDataframe <- data.frame(Model = file.path(transferDir, "model.rds"),
                                         OptimalThreshold = optimalThreshold)

# extract variable importance plot and data frame ------------------------------
variableImportanceOutput <- plotVariableImportance(variableImportance,
                                                   transferDir)

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

  predictionRasters <- getPredictionRasters(trainingRasterList[[t]],
                                            model,
                                            optimalThreshold,
                                            modelType)
  predictedPresence <- predictionRasters[[1]]
  probabilityRaster <- predictionRasters[[2]]

  # generate rasterDataframe based on filtering argument
  rasterOutputDataframe <- generateRasterDataframe(applyFiltering,
                                                   predictedPresence,
                                                   filterResolution,
                                                   filterPercent,
                                                   category = "training",
                                                   timestep,
                                                   transferDir,
                                                   rasterOutputDataframe,
                                                   hasGroundTruth = TRUE)

  # define GroundTruth raster
  groundTruth <- groundTruthRasterList[[t]]

  # define RGB data frame
  rgbOutputDataframe <- getRgbDataframe(rgbOutputDataframe,
                                        category = "training",
                                        timestep,
                                        transferDir)

  # save files
  saveFiles(predictedPresence,
            groundTruth,
            probabilityRaster,
            trainingRasterList,
            category = "training",
            timestep,
            transferDir)
}

# calculate mean values for model statistics -----------------------------------
outputDataframes <- calculateStatistics(model,
                                        allTestData,
                                        optimalThreshold,
                                        confusionOutputDataframe,
                                        modelOutputDataframe)

confusionOutputDataframe <- outputDataframes[[1]]
modelOutputDataframe <- outputDataframes[[2]]
confusionMatrixPlot <- outputDataframes[[3]]

# generate model chart dataframe ----------------------------------------------
modelChartDataframe <- data.frame(Accuracy = modelOutputDataframe %>% filter(Statistic == "Accuracy") %>% pull(Value),
                                  Precision = modelOutputDataframe %>% filter(Statistic == "Precision") %>% pull(Value),
                                  Sensitivity = modelOutputDataframe %>% filter(Statistic == "Sensitivity") %>% pull(Value),
                                  Specificity = modelOutputDataframe %>% filter(Statistic == "Specificity") %>% pull(Value))

# make a confusion matrix output dataframe
ggsave(filename = file.path(paste0(transferDir, "/ConfusionMatrixPlot.png")),
       confusionMatrixPlot)

# add to output datasheet
confusionMatrixPlotOutputDataframe <- data.frame(ConfusionMatrixPlot = file.path(paste0(transferDir, "/ConfusionMatrixPlot.png")))

# save filter resolution and threshold to input datasheet ----------------------
filterOutputDataframe <- data.frame(applyFiltering = applyFiltering,
                                    filterResolution = filterResolution,
                                    filterPercent = filterPercent)

# check data type for output dataframes before saving --------------------------
checkOutputDataframes(rasterOutputDataframe,
                      confusionOutputDataframe,
                      modelOutputDataframe,
                      rgbOutputDataframe)

# Save dataframes back to SyncroSim library's output datasheets ----------------

saveDatasheet(myScenario,
              data = filterOutputDataframe,
              name = "imageclassifier_PostProcessingOptions")

saveDatasheet(myScenario,
              data = rasterOutputDataframe,
              name = "imageclassifier_RasterOutput")

saveDatasheet(myScenario,
              data = confusionOutputDataframe,
              name = "imageclassifier_ConfusionMatrix")

saveDatasheet(myScenario,
              data = modelOutputDataframe,
              name = "imageclassifier_ModelStatistics")

saveDatasheet(myScenario,
              data = varImportanceOutputImage,
              name = "imageclassifier_VariableImportanceOutput")

saveDatasheet(myScenario,
              data = rgbOutputDataframe,
              name = "imageclassifier_RgbOutput")

saveDatasheet(myScenario,
              data = modelObjectOutputDataframe,
              name = "imageclassifier_ModelObject")

saveDatasheet(myScenario,
              data = confusionMatrixPlotOutputDataframe,
              name = "imageclassifier_ConfusionMatrixPlotOutput")

saveDatasheet(myScenario,
              data = varImportanceOutputDataframe,
              name = "imageclassifier_VariableImportanceOutputDataframe")

saveDatasheet(myScenario,
              data = modelChartDataframe,
              name = "imageclassifier_ModelChartData")
