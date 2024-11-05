# # set up library (remove after testing) -----------------------------------
# library(rsyncrosim)
# mySession <- session("C:/Program Files/SyncroSim Studio")
# libPath <- "library/image_classifier_testing.ssim"

# myLibrary <- ssimLibrary(name = libPath,
#                          session = mySession)

# # define project
# myProject <- rsyncrosim::project(myLibrary, project = 1)

# # define scenario
# scenario(myProject)
# myScenario <- scenario(myProject, scenario = 89)

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

# Load raster input datasheet ------------------------------------------------
inputRasterDataframe <- datasheet(myScenario,
                                  name = "imageclassifier_InputRasters")

# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(myScenario,
                                  inputRasterDataframe,
                                  inputRasterDataframe$TrainingRasterFile)
timestepList <- inputVariables[[1]]
nObs <- inputVariables[[2]]
filterResolution <- inputVariables[[3]] # TO DO: give warnings for lower limits (must be >=1?)
filterPercent <- inputVariables[[4]]
applyFiltering <- inputVariables[[5]]
applyContextualization <- inputVariables[[6]]
modelType <- inputVariables[[7]]

# check timesteps were input correctly ---------------------------------------
# checkTimesteps(timesteps,
#                rasterTrainingDataframe,
#                rasterGroundTruthDataframe)

# extract list of training, testing, and ground truth rasters ----------------
trainingRasterList <- extractRasters(inputRasterDataframe, column = 2)
groundTruthRasterList <- extractRasters(inputRasterDataframe, column = 3)

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
  modelOut <- getMaxentModel(allTrainData)
  optimalThreshold <-  getOptimalThreshold(modelOut[[1]], allTestData, "MaxEnt")
} else if (modelType == "Random Forest") {
  modelOut <- getRandomForestModel(allTrainData)
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

# extract variable importance plot ---------------------------------------------
variableImportanceOutput <- plotVariableImportance(variableImportance,
                                                   transferDir)

variableImportancePlot <- variableImportanceOutput[[1]]
varImportanceOutputDataframe <- variableImportanceOutput[[2]]

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
              data = varImportanceOutputDataframe,
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
