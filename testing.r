# script to help test image classifier

# set up workspace --------------------------------------------------------
source(file.path("imageclassifier/workspace.r"))

## Connect to SyncroSim Library ----
mySession <- session("C:/Program Files/SyncroSim Studio")
libraryName <- "C:/Users/HannahAdams/Documents/Projects/A331 Snowpack/image_classifier_testing.ssim"
myLibrary <- ssimLibrary(name = libraryName, session = mySession)
myProject <- rsyncrosim::project(ssimObject = myLibrary, project = "Definitions")
scenario(myProject)

# Load testing scenario
myScenario <- scenario(ssimObject = myProject, scenario = "package testing")
datasheet(myScenario)

# view input datasheets
datasheet(myScenario, name = "imageclassifier_TrainingData")
datasheet(myScenario, name = "imageclassifier_GroundTruthData")
datasheet(myScenario, name = "imageclassifier_TestingData")
datasheet(myScenario, name = "imageclassifier_RasterOutput")

# Set timesteps
timesteps <- seq(1, 10)

# Load (or create) input Datasheets
modelInputDataframe <- data.frame(Nobs = 1000, filterResolution = 5, filterPercent = 0.25)

# assign nob value from model input datasheet
Nobs <- modelInputDataframe$Nobs
filterResolution <- modelInputDataframe$filterResolution
filterPercent <- modelInputDataframe$filterPercent
ApplyFiltering <- TRUE

rasterTrainingDataframe <- data.frame(Timesteps = seq(1, 10),
                                      PredictorRasterFile = c("C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_1.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_2.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_3.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_4.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_5.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_6.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_7.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_8.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_9.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/train/Landsat_Predictor_10.tif"))

rasterGroundTruthDataframe <- data.frame(Timesteps = seq(1, 10),
                                      ResponseRasterFile = c("C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_1.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_2.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_3.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_4.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_5.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_6.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_7.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_8.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_9.tif",
                                                             "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_10.tif"))

rasterTestingDataframe <- data.frame(Timesteps = numeric(0),
                                      TestingRasterFile = character(0)) # may not need timesteps?

# doing this here only, names should be different when using package
# newNames <- c("band.1", "band.2", "band.3", "band.4", "band.5", "band.6", "band.7", "band.8", "band.9", "band.10", "band.11", "band.12")
# names(predictorRasterList[[1]]) <- newNames
# names(predictorRasterList[[2]]) <- newNames


# extract list of predictor, testing, and ground truth rasters
trainingRasterList <- extractRasters(rasterTrainingDataframe)

groundTruthRasterList <- extractRasters(rasterGroundTruthDataframe)

if (length(rasterTestingDataframe$TestingRasterFile) > 1) {
  testingRasterList <- extractRasters(rasterTestingeDataframe)
}

# Setup empty dataframes to accept output in SyncroSim datasheet format ------
rasterOutputDataframe <- data.frame(Iteration = numeric(0),
                                    Timestep = numeric(0),
                                    PredictedUnfiltered = character(0),
                                    PredictedFiltered = character(0),
                                    GroundTruth = character(0),
                                    FilterResolution = numeric(0),
                                    FilterThreshold = numeric(0))

confusionOutputDataframe <- data.frame(Timestep = numeric(0),
                                       Prediction = numeric(0),
                                       Reference = numeric(0),
                                       Frequency = numeric(0),
                                       ConfusionSD = numeric(0))

modelOutputDataframe <- data.frame(Timestep = numeric(0),
                                   Statistic = character(0),
                                   Value = numeric(0),
                                   ModelSD = numeric(0))

# create empty lists for binding data
allTrainData <- c()
allTestData <- c()

# For loop through each raster pair
for (i in seq_along(predictorRasterList)) {

  ## Decompose satellite image raster
  modelData <- decomposedRaster(predictorRasterList[[i]],
                                groundTruthRasterList[[i]],
                                nobs = Nobs)

  modelDataSampled <- modelData %>%
    mutate(presence = as.factor(response)) %>%
    dplyr::select(-ID, -response) %>%
    mutate(kfold = sample(1:10, nrow(.), replace = TRUE)) %>%
    drop_na()

  # split into training and testing data
  train <- modelDataSampled %>% filter(kfold != 1)
  test <- modelDataSampled %>% filter(kfold == 1)

  # bind to list
  allTrainData <- rbind(allTrainData, train)
  allTestData <- rbind(allTestData, test)

}

## Train model
mainModel <- formula(sprintf("%s ~ %s",
                             "presence",
                             paste(names(predictorRasterList[[1]]),
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

# generate probabilities for each raster
probabilityRaster <- 1- (predictRanger(trainingRasterList[[1]],
                                       rf2))

testRast <- 1- probabilityRaster
plot(probabilityRaster)

 # extract random forest output
rf1$prediction.error
rf1$num.trees
rf1$num.independent.variables
rf1$mtry
rf1$treetype
rf1$forest
rf1$predictions


# extract variable importance and plot -----------------------------------------
variableImportance <- melt(rf1$variable.importance) %>%
  rownames_to_column("variable")

variableImportancePlot <- ggplot(variableImportance, aes(x = reorder(variable, value),
                                                         y = value,
                                                         fill = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("Variable") +
  ggtitle("Information Value Summary") +
  theme_classic() +
  scale_fill_gradientn(colours = c("#3f4885"), guide = "none")

## Predict for each timestep group ---------------------------------------------
for (t in seq_along(predictorRasterList)) {

  # predict presence for each raster
  PredictedPresence <- predictRanger(predictorRasterList[[t]],
                                     rf1)
  # --------------------------------------------------------------
  predictionRaster <- raster[[1]]
  probabilityRaster <- raster[[1]]
  names(predictionRaster) <- "presence"

  ## predict over raster decomposition
  rasterMatrix <- data.frame(raster)
  rasterMatrix <- na.omit(rasterMatrix)

  predictedOutput <- predict(rf1, rasterMatrix)

  predictedValues <- data.frame(predictedOutput)[, 1]
  values(predictionRaster) <- predictedValues

  # extract probability values
  probabilities <- predictedOutput$predictions
  values(probabilityRaster) <- probabilities

  rfOutput <- list(predictionRaster, probabilities)
  # --------------------------------------------------------------

  # assign values
  values(PredictedPresence) <- ifelse(values(PredictedPresence) == 2, 1, 0)

  if (ApplyFiltering == TRUE) {

    if (OptimizeFiltering == TRUE) {

      # optimize the filtering threshold
      # optimizedParams <- optim(par = c(5, 0.25),
      #                          fn = filterFit,
      #                          PredictedPresence = PredictedPresence,
      #                          groundTruthRaster = groundTruthRasterList[[t]],
      #                          lower = c(0.1, 0.01),
      #                          upper = c(10, 1),
      #                          method = "L-BFGS-B",
      #                          control = list(fnscale = -1))

      optimizedParams <- optim(par = c(5, 0.25),
                               fn = filterFit,
                               PredictedPresence = PredictedPresence,
                               groundTruthRaster = groundTruthRasterList[[t]],
                               control = list(fnscale = -1))

      filteredPredictedPresence <- focal(PredictedPresence,
                                         w = matrix(1, 5, 5),
                                         fun = filterFun,
                                         resolution = optimizedParams$par[1],
                                         percent = optimizedParams$par[2])

      # # save raster
      # writeRaster(filteredPredictedPresence,
      #             filename = file.path(paste0(transferDir,
      #                                         "/filteredPredictedPresence",
      #                                         t,
      #                                         ".tif")),
      #             overwrite = TRUE)

      rasterDataframe <- data.frame(Iteration = 1,
                                    Timestep = t,
                                    PredictedUnfiltered = file.path(paste0(transferDir, "/PredictedPresence", t, ".tif")),
                                    PredictedFiltered = file.path(paste0(transferDir, "/filteredPredictedPresence", t, ".tif")),
                                    GroundTruth = file.path(paste0(transferDir, "/GroundTruth", t, ".tif")),
                                    FilterResolution = optimizedParams$par[1],
                                    FilterThreshold = optimizedParams$par[2])

    } else {
      # filter out presence pixels surrounded by non-presence
      filteredPredictedPresence <- focal(PredictedPresence,
                                         w = matrix(1, 5, 5),
                                         fun = filterFun,
                                         resolution = filterResolution,
                                         percent = filterPercent)

      # # save raster
      # writeRaster(filteredPredictedPresence,
      #             filename = file.path(paste0(transferDir,
      #                                         "/filteredPredictedPresence",
      #                                         t,
      #                                         ".tif")),
      #             overwrite = TRUE)

      rasterDataframe <- data.frame(Iteration = 1,
                                    Timestep = t,
                                    PredictedUnfiltered = file.path(paste0(transferDir, "/PredictedPresence", t, ".tif")),
                                    PredictedFiltered = file.path(paste0(transferDir, "/filteredPredictedPresence", t, ".tif")),
                                    GroundTruth = file.path(paste0(transferDir, "/GroundTruth", t, ".tif")),
                                    FilterResolution = filterResolution,
                                    FilterThreshold = filterPercent)
    }
  } else {
    rasterDataframe <- data.frame(Iteration = 1,
                                  Timestep = t,
                                  PredictedUnfiltered = file.path(paste0(transferDir, "/PredictedPresence", t, ".tif")),
                                  PredictedFiltered = "",
                                  GroundTruth = file.path(paste0(transferDir, "/GroundTruth", t, ".tif")),
                                  FilterResolution = "",
                                  FilterThreshold = "")
  }

  # define GroundTruth (binary) raster output
  groundTruth <- groundTruthRasterList[[t]]

  # calculate statistics using the test data
  prediction <- predict(rf1, allTestData)
  confusionMatrix <- confusionMatrix(data.frame(prediction)[, 1], allTestData$presence)

  # reformat and add to output datasheets
  confusion_matrix <- data.frame(confusionMatrix$table) %>%
    rename("Frequency" = "Freq") %>%
    mutate(Timestep = t)

  overall_stats <- data.frame(confusionMatrix$overall) %>%
    rename(Value = 1) %>%
    drop_na(Value)
  class_stats <- data.frame(confusionMatrix$byClass) %>%
    rename(Value = 1) %>%
    drop_na(Value)
  model_stats <- rbind(overall_stats, class_stats) %>%
    tibble::rownames_to_column("Statistic") %>%
    mutate(Timestep = t)

  # save raster
  # writeRaster(PredictedPresence,
  #             filename = file.path(paste0(transferDir,
  #                                         "/PredictedPresence",
  #                                         t,
  #                                         ".tif")),
  #             overwrite = TRUE)

  # writeRaster(groundTruth,
  #             filename = file.path(paste0(transferDir,
  #                                         "/GroundTruth",
  #                                         t,
  #                                         ".tif")),
  #             overwrite = TRUE)

  # Store the relevant outputs from both rasters in a temporary dataframe
  rasterOutputDataframe <- addRow(rasterOutputDataframe,
                                  rasterDataframe)

  # now what to do with the confusion matrix outputs? Average from all of them?
  confusionOutputDataframe <- addRow(confusionOutputDataframe,
                                     confusion_matrix)

  modelOutputDataframe <- addRow(modelOutputDataframe,
                                 model_stats)
}
# calculate mean values for model statistics -----------------------------------
if (length(timesteps) > 1) {

  modelOutputDataframe <- modelOutputDataframe %>%
    select(-Timestep) %>%
    group_by(Statistic) %>%
    summarise(mean = mean(Value),
              sd = sd(Value)) %>%
    ungroup() %>%
    select(Statistic,
           mean,
           sd) %>%
    rename(Value = mean,
           ModelSD = sd) %>%
    drop_na(ModelSD)

  confusionOutputDataframe <- confusionOutputDataframe %>%
    select(-Timestep) %>%
    group_by(Prediction, Reference) %>%
    summarise(mean = mean(Frequency),
              sd = sd(Frequency)) %>%
    ungroup() %>%
    rename(Frequency = mean,
           ConfusionSD = sd) %>%
    drop_na(ConfusionSD)
}

# Save dataframes back to SyncroSim library's output datasheets ----------------
saveDatasheet(myScenario,
              data = rasterOutputDataframe,
              name = "imageclassifier_RasterOutput")

saveDatasheet(myScenario,
              data = confusionOutputDataframe,
              name = "imageclassifier_ConfusionMatrix")

saveDatasheet(myScenario,
              data = modelOutputDataframe,
              name = "imageclassifier_ModelStatistics")

# add filter threshold to output (make a separate non-RF stats output)
# add variable importance plot to output
# add probability raster to the output

# removed from ModelInput in xml file
# <column name="OptimizeFiltering" dataType="Boolean" displayName="Optimize filtering"/>

# removed from model outputs datasheet