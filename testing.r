# script to help test image classifier

# set up workspace --------------------------------------------------------
source(file.path("imageclassifier/workspace.r"))

## Connect to SyncroSim Library ----
mySession <- session("C:/Program Files/SyncroSim Studio")
libraryName <- "C:/Users/HannahAdams/Documents/Projects/Image classifier/image_classifier_testing.ssim"
myLibrary <- ssimLibrary(name = libraryName, session = mySession)
myProject <- rsyncrosim::project(ssimObject = myLibrary, project = "Definitions")
scenario(myProject)

# Load testing scenario
myScenario <- scenario(ssimObject = myProject, scenario = "classifier testing")
datasheet(myScenario)

# view input datasheets
datasheet(myScenario, name = "imageclassifier_TrainingData")
datasheet(myScenario, name = "imageclassifier_GroundTruthData")
datasheet(myScenario, name = "imageclassifier_DataToClassify")
datasheet(myScenario, name = "imageclassifier_RasterOutput")

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

# Set timesteps
# timesteps <- seq(1, 10)

# # Load (or create) input Datasheets
# modelInputDataframe <- data.frame(Nobs = 1000, filterResolution = 5, filterPercent = 0.25)

# # assign nob value from model input datasheet
# Nobs <- modelInputDataframe$Nobs
# filterResolution <- modelInputDataframe$filterResolution
# filterPercent <- modelInputDataframe$filterPercent
# ApplyFiltering <- TRUE

# rasterTrainingDataframe <- data.frame(Timesteps = seq(1, 10),
#                                       PredictorRasterFile = c("C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_1.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_2.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_3.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_4.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_5.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_6.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_7.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_8.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_9.tif",
#                                                               "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_10.tif"))

# rasterGroundTruthDataframe <- data.frame(Timesteps = seq(1, 10),
#                                       ResponseRasterFile = c("C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_1.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_2.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_3.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_4.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_5.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_6.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_7.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_8.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_9.tif",
#                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_10.tif"))

# rasterTestingDataframe <- data.frame(Timesteps = numeric(0),
#                                       TestingRasterFile = character(0)) # may not need timesteps?

# doing this here only, names should be different when using package
# newNames <- c("band.1", "band.2", "band.3", "band.4", "band.5", "band.6", "band.7", "band.8", "band.9", "band.10", "band.11", "band.12")
# names(predictorRasterList[[1]]) <- newNames
# names(predictorRasterList[[2]]) <- newNames


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

confusionOutputDataframe <- data.frame(Prediction = numeric(0),
                                       Reference = numeric(0),
                                       Frequency = numeric(0))

modelOutputDataframe <- data.frame(Timestep = numeric(0),
                                   Statistic = character(0),
                                   Value = numeric(0))

rgbOutputDataframe <- data.frame(Iteration = numeric(0),
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

variableImportancePlot <- variableImportanceOutput[1]
varImportanceOutputDataframe <- variableImportanceOutput[2]

## Predict presence for each timestep group ------------------------------------
for (t in seq_along(trainingRasterList)) {

  predictionRasters <- getPredictionRasters(trainingRasterList,
                                            t,
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
                                                   transferDir)

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
            variableImportancePlot,
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
