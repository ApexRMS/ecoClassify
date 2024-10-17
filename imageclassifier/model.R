# set up library (remove after testing) -----------------------------------
library(rsyncrosim)
mySession <- session("C:/Program Files/SyncroSim Studio")
libPath <- "library/image_classifier_testing.ssim"

myLibrary <- ssimLibrary(name = libPath,
                         session = mySession)

# define project
myProject <- project(myLibrary, project = 1)

# define scenario
scenario(myProject)
myScenario <- scenario(myProject, scenario = 89)

# view datasheets
datasheet(myScenario)
source("imageclassifier/workspace.r")
transferDir <- ""

# set transferDir filepath if exporting
# transferDir <- "C:/Users/HannahAdams/OneDrive - Apex Resource Management Solutions Ltd/Desktop/watchtower-testing"

# testing ground truth raster with multiple classes
# groundTruthRasterList <- list(rast("C:/Users/HannahAdams/Documents/Projects/Image classifier/multiple-class-ground-truth.tif"))
# plot(groundTruthRasterList[[1]])
# trainingRasterList <- trainingRasterList[1]
# plot(trainingRasterList[[1]])

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

# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(myScenario)
timesteps <- inputVariables[[1]]
nObs <- inputVariables[[2]]
filterResolution <- inputVariables[[3]]
filterPercent <- inputVariables[[4]]
applyFiltering <- inputVariables[[5]]
applyContextualization <- inputVariables[[6]]

# Load raster input datasheets
rasterTrainingDataframe <- datasheet(myScenario,
                                     name = "imageclassifier_TrainingData")

rasterGroundTruthDataframe <- datasheet(myScenario,
                                        name = "imageclassifier_GroundTruthData")

rasterToClassifyDataframe <- datasheet(myScenario,
                                       name = "imageclassifier_DataToClassify")

# check timesteps were input correctly ---------------------------------------
checkTimesteps(timesteps,
               rasterTrainingDataframe,
               rasterGroundTruthDataframe)

# extract list of training, testing, and ground truth rasters ----------------
extractedRasters <- extractAllRasters(rasterTrainingDataframe,
                                      rasterGroundTruthDataframe,
                                      rasterToClassifyDataframe)

trainingRasterList <- extractedRasters[[1]]
groundTruthRasterList <- extractedRasters[[2]]
toClassifyRasterList <- extractedRasters[[3]]

# Setup empty dataframes to accept output in SyncroSim datasheet format ------
rasterOutputDataframe <- data.frame(Iteration = numeric(0),
                                    Timestep = numeric(0),
                                    PredictedUnfiltered = character(0),
                                    PredictedFiltered = character(0),
                                    GroundTruth = character(0),
                                    Probability = character(0))

classifiedRasterOutputDataframe <- data.frame(Iteration = numeric(0),
                                              Timestep = numeric(0),
                                              ClassifiedUnfiltered = character(0),
                                              ClassifiedFiltered = character(0),
                                              ClassifiedProbability = character(0))

confusionOutputDataframe <- data.frame(Prediction = numeric(0),
                                       Reference = numeric(0),
                                       Frequency = numeric(0))

modelOutputDataframe <- data.frame(Statistic = character(0),
                                   Value = numeric(0))

rgbOutputDataframe <- data.frame(Iteration = numeric(0),
                                 Timestep = numeric(0),
                                 RGBImage = character(0))

classifiedRgbOutputDataframe <- data.frame(Iteration = numeric(0),
                                           Timestep = numeric(0),
                                           RGBImage = character(0))

filterOutputDataframe <- data.frame(filterResolutionOutput = filterResolution,
                                    filterThresholdOutput = filterPercent)

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
modelType = "randomForest"
if(modelType == "MaxEnt") {
  modelOut <- getMaxentModel(allTrainData)
  optimalThreshold <-  getOptimalThreshold(modelOut[[1]], allTestData, "MaxEnt")
} else if(modelType == "randomForest") {
  modelOut <- getRandomForestModel(allTrainData)
  optimalThreshold <-  getOptimalThreshold(modelOut[[1]], allTestData, "randomForest")
} else {
  stop("Model type not recognized")
}


 
 
# extract variable importance plot ---------------------------------------------
variableImportanceOutput <- plotVariableImportance(modelOut[[2]],
                                                   transferDir)

variableImportancePlot <- variableImportanceOutput[[1]]
varImportanceOutputDataframe <- variableImportanceOutput[[2]]


## Predict presence for training rasters in each timestep group ----------------
for (t in seq_along(trainingRasterList)) {

  predictionRasters <- getPredictionRasters(trainingRasterList[[t]],
                                            modelOut[[1]],
                                            optimalThreshold,
                                            modelType)
  predictedPresence <- predictionRasters[[1]]
  probabilityRaster <- predictionRasters[[2]]

  # generate rasterDataframe based on filtering argument
  rasterOutputDataframe <- generateRasterDataframe(applyFiltering,
                                                   predictedPresence,
                                                   filterResolution,
                                                   filterPercent,
                                                   iteration = 1,
                                                   t,
                                                   transferDir,
                                                   rasterOutputDataframe,
                                                   hasGroundTruth = TRUE)

  # define GroundTruth raster
  groundTruth <- groundTruthRasterList[[t]]

  # define RGB data frame
  rgbOutputDataframe <- getRgbDataframe(rgbOutputDataframe,
                                        iteration = 1,
                                        t,
                                        transferDir)

  # save files
  saveFiles(predictedPresence,
            groundTruth,
            probabilityRaster,
            trainingRasterList,
            iteration = 1,
            t,
            transferDir)
}

# add contextualization for toclassify rasters if selected ---------------------
if (applyContextualization == TRUE) {

  toClassifyRasterList <- contextualizeRaster(toClassifyRasterList) # change naming to avoid this

}

## Predict presence for rasters to classify ------------------------------------
for (t in seq_along(toClassifyRasterList)) {

  classifiedRasters <- getPredictionRasters(toClassifyRasterList[[t]],
                                            rf1,
                                            rf2)
  classifiedPresence <- classifiedRasters[[1]]
  classifiedProbability <- classifiedRasters[[2]]

  # generate rasterDataframe based on filtering argument
  classifiedRasterOutputDataframe <- generateRasterDataframe(applyFiltering,
                                                             classifiedPresence,
                                                             filterResolution,
                                                             filterPercent,
                                                             iteration = 2,
                                                             t,
                                                             transferDir,
                                                             classifiedRasterOutputDataframe,
                                                             hasGroundTruth = FALSE)

  # define RGB data frame
  classifiedRgbOutputDataframe <- getRgbDataframe(classifiedRgbOutputDataframe,
                                                  iteration = 2,
                                                  t,
                                                  transferDir)

  # save files
  saveFiles(classifiedPresence,
            groundTruth = NULL,
            classifiedProbability,
            toClassifyRasterList,
            iteration = 2,
            t,
            transferDir)
}

# calculate mean values for model statistics -----------------------------------
outputDataframes <- calculateStatistics(rf1,
                                        allTestData,
                                        confusionOutputDataframe,
                                        modelOutputDataframe)

confusionOutputDataframe <- outputDataframes[[1]]
modelOutputDataframe <- outputDataframes[[2]]

# check data type for output dataframes before saving --------------------------
checkOutputDataframes(rasterOutputDataframe,
                      confusionOutputDataframe,
                      modelOutputDataframe,
                      rgbOutputDataframe)

# Save dataframes back to SyncroSim library's output datasheets ----------------
saveDatasheet(myScenario,
              data = rasterOutputDataframe,
              name = "imageclassifier_RasterOutput")

saveDatasheet(myScenario,
              data = classifiedRasterOutputDataframe,
              name = "imageclassifier_ClassifiedRasterOutput")

saveDatasheet(myScenario,
              data = confusionOutputDataframe,
              name = "imageclassifier_ConfusionMatrix")

saveDatasheet(myScenario,
              data = modelOutputDataframe,
              name = "imageclassifier_ModelStatistics")

saveDatasheet(myScenario,
              data = filterOutputDataframe,
              name = "imageclassifier_FilterStatistics")

saveDatasheet(myScenario,
              data = varImportanceOutputDataframe,
              name = "imageclassifier_ModelOutput")

saveDatasheet(myScenario,
              data = rgbOutputDataframe,
              name = "imageclassifier_RgbOutput")

saveDatasheet(myScenario,
              data = classifiedRgbOutputDataframe,
              name = "imageclassifier_ClassifiedRgbOutput")
