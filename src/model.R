# set up workspace --------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))
source(file.path(packageDir, "workspace.r"))

# Load files --------------------------------------------------------------
myScenario <- scenario()  # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load RunControl datasheet to be able to set timesteps
runSettings <- datasheet(myScenario, name = "imageclassifier_RunControl")

# Set timesteps - can set to different frequencies if desired
timesteps <- seq(runSettings$MinimumTimestep, runSettings$MaximumTimestep)

# Load input Datasheets
modelInputDataframe <- datasheet(myScenario,
                                 name = "imageclassifier_ModelInput")

# assign nob value from model input datasheet
Nobs <- modelInputDataframe$Nobs
filterResolution <- modelInputDataframe$filterResolution
filterPercent <- modelInputDataframe$filterPercent

# Load raster input
rasterInputDataframe <- datasheet(myScenario,
                                  name = "imageclassifier_RasterInput")

# extract list of predictor and response rasters using their respective filepaths
# this could be turned into a function

# predictor raster list
allPredictorFiles <- as.vector(rasterInputDataframe$PredictorRasterFile)

predictorRasterList <- c()

for (file in allPredictorFiles) {
  predictorRaster <- rast(file)
  predictorRasterList <- c(predictorRasterList, predictorRaster)
}

# response raster list
allResponseFiles <- as.vector(rasterInputDataframe$ResponseRasterFile)

responseRasterList <- c()

for (file in allResponseFiles) {
  responseRaster <- rast(file)
  responseRasterList <- c(responseRasterList, responseRaster)
}

# Setup empty dataframes to accept output in SyncroSim datasheet format ------
rasterOutputDataframe <- data.frame(Iteration = numeric(0),
                                    Timestep = numeric(0),
                                    PredictedUnfiltered = character(0),
                                    PredictedFiltered = character(0))

confusionOutputDataframe <- data.frame(Prediction = numeric(0),
                                       Reference = numeric(0),
                                       Frequency = numeric(0),
                                       ConfusionSD = numeric(0))

modelOutputDataframe <- data.frame(Statistic = character(0),
                                   Value = numeric(0),
                                   ModelSD = numeric(0))

# For loop through iterations
for (t in timesteps) {

  ## Decompose satellite image raster
  modelData <- decomposedRaster(predictorRasterList[[t]],
                                responseRasterList[[t]],
                                nobs = Nobs)

  modelDataSampled <- modelData %>%
      mutate(presence = as.factor(response)) %>%
      select(-ID, -response) %>%
      mutate(kfold = sample(1:10, nrow(.), replace = TRUE)) %>%
      drop_na()

  # split into training and testing data
  train <- modelDataSampled %>% filter(kfold != 1)
  test <- modelDataSampled %>% filter(kfold == 1)

  ## Train model
  mainModel <- formula(sprintf("%s ~ %s",
                               "presence",
                               paste(names(predictorRasterList[[t]]),
                                     collapse = " + ")))

  rf1 <-  ranger(mainModel,
                 data = train,
                 mtry = 2)

  ## Predict over area
  PredictedPresence <- predictRanger(predictorRasterList[[t]],
                                     rf1)

  # assign values
  values(PredictedPresence) <- ifelse(values(PredictedPresence) == 2, 1, 0)

  # filter out presence pixels surrounded by non-presence
  filteredPredictedPresence <- focal(PredictedPresence,
                                     w = matrix(1, 5, 5),
                                     fun = filterFun,
                                     resolution = filterResolution,
                                     percent = filterPercent)

  # calculate statistics using the test data
  prediction <- predict(rf1, test)
  confusionMatrix <- confusionMatrix(data.frame(prediction)[, 1], test$presence)

  # reformat and add to output datasheets
  confusion_matrix <- data.frame(confusionMatrix$table) %>%
    rename("Frequency" = "Freq")

  overall_stats <- data.frame(confusionMatrix$overall) %>%
    rename(Value = 1) %>%
    drop_na(Value)
  class_stats <- data.frame(confusionMatrix$byClass) %>%
    rename(Value = 1) %>%
    drop_na(Value)
  model_stats <- rbind(overall_stats, class_stats) %>%
    tibble::rownames_to_column("Statistic")

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

  # export both rasters to an external folder (eventually remove)
  writeRaster(PredictedPresence,
              filename = file.path(paste0("C:/Users/HannahAdams/OneDrive - Apex Resource Management Solutions Ltd/Documents/Projects/A333 UMU Tamarisk Pilot/output/PredictedPresence", t, ".tif")),
              overwrite = TRUE)

  writeRaster(filteredPredictedPresence,
              filename = file.path(paste0("C:/Users/HannahAdams/OneDrive - Apex Resource Management Solutions Ltd/Documents/Projects/A333 UMU Tamarisk Pilot/output/filteredPredictedPresence", t, ".tif")),
              overwrite = TRUE)

  # Store the relevant outputs from both rasters in a temporary dataframe
  rasterDataframe <- data.frame(Iteration = 1,
                                Timestep = t,
                                PredictedUnfiltered = file.path(paste0(transferDir, "/PredictedPresence", t, ".tif")),
                                PredictedFiltered = file.path(paste0(transferDir, "/filteredPredictedPresence", t, ".tif")))

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
