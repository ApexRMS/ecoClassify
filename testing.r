# script to help test image classifier

# set up workspace --------------------------------------------------------
source(file.path("imageclassifier/workspace.r"))

## Connect to SyncroSim Library ----
libraryName <- "C:/Users/HannahAdams/Documents/Projects/Image classifier/image_classifier_testing.ssim"
myLibrary <- ssimLibrary(name = libraryName)
myProject <- rsyncrosim::project(ssimObject = myLibrary, project = "Definitions")
scenario(myProject)

# Load testing scenario
myScenario <- scenario(ssimObject = myProject, scenario = 89)
datasheet(myScenario)

# view input datasheets
datasheet(myScenario, name = "imageclassifier_TrainingData")
datasheet(myScenario, name = "imageclassifier_ResponseData")
datasheet(myScenario, name = "imageclassifier_TestingData")

# Set timesteps
timesteps <- seq(1, 2)

# Load (or create) input Datasheets
modelInputDataframe <- data.frame(Nobs = 1000, filterResolution = 5, filterPercent = 0.25)

# assign nob value from model input datasheet
Nobs <- modelInputDataframe$Nobs
filterResolution <- modelInputDataframe$filterResolution
filterPercent <- modelInputDataframe$filterPercent

rasterTrainingDataframe <- data.frame(Timesteps = c(1, 1, 2, 2),
                                      PredictorRasterFile = c("C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/test/Landsat_Predictor_LC08_042025_20141014.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/test/Landsat_Predictor_LC08_042025_20151001.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/test/Landsat_Predictor_LC08_042025_20160901.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/test/Landsat_Predictor_LC08_042025_20180227.tif"))

rasterResponseDataframe <- data.frame(Timesteps = c(1, 1, 2, 2),
                                      ResponseRasterFile = c("C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_1.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_2.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_3.tif",
                                                              "C:/Users/HannahAdams/Documents/Projects/Image classifier/A300 Western University - 2023 Snowpack/SyncroSim Library Data/response/Sentinel_Snow_4.tif"))

rasterTestingDataframe <- data.frame(Timesteps = numeric(0),
                                      TestingRasterFile = character(0)) # may not need timesteps?
# old extraction function
extractRasters <- function(column) {

  allFiles <- as.vector(column)
  rasterList <- c()

  for (file in allFiles) {
    Raster <- rast(file)
    rasterList <- c(rasterList, Raster)
  }

  return(rasterList)
}

extractRastersV2 <- function(dataframe) {

  # define timesteps
  timesteps <- unique(dataframe[,1])

  # create an empty list
  rasterList <- c()

  # loop through timesteps, reading and combining rasters
  for (t in timesteps) {
    
    # subset based on timestep
    subsetData <- dataframe %>% filter(Timesteps == t)

    # list all files
    allFiles <- as.vector(subsetData[, 2])
    
    # read in all files as a single raster
    subsetRaster <- rast(allFiles)

    # add to main raster list
    rasterList <- c(rasterList, subsetRaster)
  }

  return(rasterList)
}

extractionTest <- extractRastersV2(rasterTrainingDataframe)

# extract list of predictor, testing, and response rasters
extractRasters <- function(column) {

  allFiles <- as.vector(column)
  rasterList <- c()

  for (file in allFiles) {
    Raster <- rast(file)
    rasterList <- c(rasterList, Raster)
  }

  return(rasterList)
}

predictorRasterList <- extractRasters(rasterTrainingDataframe$PredictorRasterFile)

responseRasterList <- extractRasters(rasterResponseDataframe,
                                     rasterResponseDataframe$ResponseRasterFile)

testingRasterList <- extractRasters(rasterTestingDataframe,
                                    rasterTestingDataframe$TestingRasterFile)

# Setup empty dataframes to accept output in SyncroSim datasheet format ------
rasterOutputDataframe <- data.frame(Iteration = numeric(0),
                                    Timestep = numeric(0),
                                    PredictedUnfiltered = character(0),
                                    PredictedFiltered = character(0),
                                    Response = character(0))

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
                                responseRasterList[[i]],
                                nobs = Nobs)

  modelDataSampled <- modelData %>%
      mutate(presence = as.factor(response)) %>%
      select(-ID, -response) %>%
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

# extract variable importance and plot -----------------------------------------
varImportance <- melt(rf1$variable.importance) %>%
  rownames_to_column("variable")

ggplot(varImportance, aes(x = reorder(variable, value),
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
# EVENTUALLY REPLACE WITH ACTUAL TESTING DATA
for (t in seq_along(predictorRasterList)) {

  # predict presence for each raster
  PredictedPresence <- predictRanger(predictorRasterList[[t]],
                                     rf1)

  # assign values
  values(PredictedPresence) <- ifelse(values(PredictedPresence) == 2, 1, 0)

  # ADD IF STATEMENT HERE - IF FILTERING WAS REQUESTED
  # filter out presence pixels surrounded by non-presence
  filteredPredictedPresence <- focal(PredictedPresence,
                                     w = matrix(1, 5, 5),
                                     fun = filterFun,
                                     resolution = filterResolution,
                                     percent = filterPercent)

  # define response (binary) raster output
  Response <- responseRasterList[[t]]

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

  writeRaster(PredictedPresence,
              filename = file.path(paste0(transferDir,
                                          "/PredictedPresence",
                                          t,
                                          ".tif")),
              overwrite = TRUE)

  writeRaster(filteredPredictedPresence,
              filename = file.path(paste0(transferDir,
                                          "/filteredPredictedPresence",
                                          t,
                                          ".tif")),
              overwrite = TRUE)

    writeRaster(Response,
                filename = file.path(paste0(transferDir,
                                            "/filteredPredictedPresence",
                                            t,
                                            ".tif")),
                overwrite = TRUE)

  # export both rasters to an external folder (eventually remove)
  writeRaster(PredictedPresence,
              filename = file.path(paste0("C:/Users/HannahAdams/Documents/Projects/A333 UMU Tamarisk Pilot/output/PredictedPresence", t, ".tif")),
              overwrite = TRUE)

  writeRaster(filteredPredictedPresence,
              filename = file.path(paste0("C:/Users/HannahAdams/Documents/Projects/A333 UMU Tamarisk Pilot/output/filteredPredictedPresence", t, ".tif")),
              overwrite = TRUE)

  # Store the relevant outputs from both rasters in a temporary dataframe
  # ADD BINARY OUTPUT (RESPONSE RASTER) TO OUTPUT DATAFRAME - HERE AND IN XML FILE
  rasterDataframe <- data.frame(Iteration = 1,
                                Timestep = t,
                                PredictedUnfiltered = file.path(paste0(transferDir, "/PredictedPresence", t, ".tif")),
                                PredictedFiltered = file.path(paste0(transferDir, "/filteredPredictedPresence", t, ".tif")),
                                Response = file.path(paste0(transferDir, "/Response", t, ".tif")))

  rasterOutputDataframe <- addRow(rasterOutputDataframe,
                                  rasterDataframe)

  # now what to do with the confusion matrix outputs? Average from all of them?
  confusionOutputDataframe <- addRow(confusionOutputDataframe,
                                     confusion_matrix)

  modelOutputDataframe <- addRow(modelOutputDataframe,
                                 model_stats)
  # ADD BINARY OUTPUT
  # REPORT FILTERING
}
