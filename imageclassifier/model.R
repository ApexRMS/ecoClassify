# set up library (remove after testing) -----------------------------------
library(rsyncrosim)
mySession <- session("C:/Program Files/SyncroSim Studio")
libPath <- "C:/Users/HannahAdams/Documents/Projects/Image classifier/image_classifier_testing.ssim"

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
transferDir <- "C:/Users/HannahAdams/OneDrive - Apex Resource Management Solutions Ltd/Desktop/watchtower-testing"

# set up workspace ---------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))
source(file.path(packageDir, "workspace.r"))

# Set up -------------------------------------------------------------------
myScenario <- scenario()  # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(myScenario)
timesteps <- inputVariables[[1]]
nObs <- inputVariables[[2]]
filterResolution <- inputVariables[[3]]
filterPercent <- inputVariables[[4]]
applyFiltering <- inputVariables[[5]]

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

# separate training and testing data -------------------------------------------
splitData <- splitTrainTest(trainingRasterList,
                            groundTruthRasterList,
                            nObs)
allTrainData <- splitData[[1]]
allTestData <- splitData[[2]]

## Train model -----------------------------------------------------------------
mainModel <- formula(sprintf("%s ~ %s",
                             "presence",
                             paste(names(trainingRasterList[[1]]),
                                   collapse = " + ")))

rf1 <-  ranger(mainModel,
               data = allTrainData,
               mtry = 2,
               importance = "impurity")

rf2 <-  ranger(mainModel,
               data = allTrainData,
               mtry = 2,
               probability = TRUE,
               importance = "impurity")

# extract variable importance plot ---------------------------------------------
variableImportanceOutput <- plotVariableImportance(rf1,
                                                   transferDir)

variableImportancePlot <- variableImportanceOutput[[1]]
varImportanceOutputDataframe <- variableImportanceOutput[[2]]

## Predict presence for training rasters in each timestep group ----------------
for (t in seq_along(trainingRasterList)) {

  predictionRasters <- getPredictionRasters(trainingRasterList[[t]],
                                            rf1,
                                            rf2)
  predictedPresence <- predictionRasters[[1]]
  probabilityRaster <- predictionRasters[[2]]

  # generate rasterDataframe based on filtering argument
  rasterOutputDataframe <- generateRasterDataframe(applyFiltering,
                                                   predictedPresence,
                                                   filterResolution,
                                                   filterPercent,
                                                   t,
                                                   transferDir,
                                                   rasterOutputDataframe)

  # define GroundTruth raster
  groundTruth <- groundTruthRasterList[[t]]

  # define RGB data frame
  rgbOutputDataframe <- getRgbDataframe(rgbOutputDataframe,
                                        t,
                                        transferDir)

  # save files
  saveFiles(predictedPresence,
            groundTruth,
            probabilityRaster,
            trainingRasterList,
            t,
            t,
            transferDir)
}

## Predict presence for rasters to classify ------------------------------------
for (t in seq_along(toClassifyRasterList)) {

  classifiedRasters <- getPredictionRasters(toClassifyRasterList[[t]],
                                            rf1,
                                            rf2)
  classifiedPresence <- classifiedRasters[[1]]
  classifiedProbability <- classifiedRasters[[2]]

  # generate rasterDataframe based on filtering argument
  classifiedRasterOutputDataframe <- generateClassifiedRasterDataframe(applyFiltering,
                                                                       classifiedPresence,
                                                                       filterResolution,
                                                                       filterPercent,
                                                                       t + max(timesteps),
                                                                       transferDir,
                                                                       classifiedRasterOutputDataframe)

  # define RGB data frame
  classifiedRgbOutputDataframe <- getRgbDataframe(classifiedRgbOutputDataframe,
                                                  t + max(timesteps),
                                                  transferDir)

  # save files
  saveFiles(predictedPresence,
            groundTruth = NULL,
            probabilityRaster,
            toClassifyRasterList,
            t + max(timesteps),
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
