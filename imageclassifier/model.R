# set up workspace --------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))
source(file.path(packageDir, "workspace.r"))

# Load files --------------------------------------------------------------
myScenario <- scenario()  # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load RunControl datasheet to set timesteps
runSettings <- datasheet(myScenario, name = "imageclassifier_RunControl")

# Set timesteps - can set to different frequencies if desired
timesteps <- seq(runSettings$MinimumTimestep, runSettings$MaximumTimestep)

# Load input Datasheets
modelInputDataframe <- datasheet(myScenario,
                                 name = "imageclassifier_ModelInput")

# Extract model input values from model input datasheet
nObs <- modelInputDataframe$nObs
filterResolution <- modelInputDataframe$filterResolution
filterPercent <- modelInputDataframe$filterPercent
applyFiltering <- modelInputDataframe$applyFiltering

# Load raster input datasheets
rasterTrainingDataframe <- datasheet(myScenario,
                                     name = "imageclassifier_TrainingData")

rasterGroundTruthDataframe <- datasheet(myScenario,
                                        name = "imageclassifier_GroundTruthData")

rasterToClassifyDataframe <- datasheet(myScenario,
                                       name = "imageclassifier_DataToClassify")

# extract list of predictor, testing, and ground truth rasters
trainingRasterList <- extractRasters(rasterTrainingDataframe)

groundTruthRasterList <- extractRasters(rasterGroundTruthDataframe)

if (length(rasterToClassifyDataframe$RasterFileToClassify) > 1) {
  toClassifyRasterList <- extractRasters(rasterToClassifyDataframe)
}

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

imageOutputDataframe <- data.frame(Iteration = numeric(0),
                                   Timestep = numeric(0),
                                   RGBImage = character(0))

# create empty lists for binding data
allTrainData <- c()
allTestData <- c()

# For loop through each raster pair
for (i in seq_along(trainingRasterList)) {

  ## Decompose satellite image raster
  modelData <- decomposedRaster(trainingRasterList[[i]],
                                groundTruthRasterList[[i]],
                                nobs = nObs)

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

# extract variable importance and plot -----------------------------------------
variableImportance <- melt(rf1$variable.importance) %>%
  tibble::rownames_to_column("variable")

variableImportancePlot <- ggplot(variableImportance, aes(x = reorder(variable, value),
                                                         y = value,
                                                         fill = value)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("Variable") +
  ggtitle("Information Value Summary") +
  theme_classic(base_size = 26) +
  scale_fill_gradientn(colours = c("#424352"), guide = "none")

## Predict for each timestep group ---------------------------------------------
for (t in seq_along(trainingRasterList)) {

  # generate probabilities for each raster
  probabilityRaster <- 1 - (predictRanger(trainingRasterList[[t]],
                                          rf2))

  # predict presence for each raster
  predictedPresence <- predictRanger(trainingRasterList[[t]],
                                     rf1)

  # assign values
  values(predictedPresence) <- ifelse(values(predictedPresence) == 2, 1, 0)

  if (applyFiltering == TRUE) {

    # filter out presence pixels surrounded by non-presence
    filteredPredictedPresence <- focal(predictedPresence,
                                       w = matrix(1, 5, 5),
                                       fun = filterFun,
                                       resolution = filterResolution,
                                       percent = filterPercent)

    # save raster
    writeRaster(filteredPredictedPresence,
                filename = file.path(paste0(transferDir,
                                            "/filteredPredictedPresence",
                                            t,
                                            ".tif")),
                overwrite = TRUE)

    rasterDataframe <- data.frame(Iteration = 1,
                                  Timestep = t,
                                  PredictedUnfiltered = file.path(paste0(transferDir, "/PredictedPresence", t, ".tif")),
                                  PredictedFiltered = file.path(paste0(transferDir, "/filteredPredictedPresence", t, ".tif")),
                                  GroundTruth = file.path(paste0(transferDir, "/GroundTruth", t, ".tif")),
                                  Probability = file.path(paste0(transferDir, "/Probability", t, ".tif")))
  } else {
    rasterDataframe <- data.frame(Iteration = 1,
                                  Timestep = t,
                                  PredictedUnfiltered = file.path(paste0(transferDir, "/PredictedPresence", t, ".tif")),
                                  PredictedFiltered = "",
                                  GroundTruth = file.path(paste0(transferDir, "/GroundTruth", t, ".tif")),
                                  Probability = file.path(paste0(transferDir, "/Probability", t, ".tif")))
  }

  # define GroundTruth (binary) raster output
  groundTruth <- groundTruthRasterList[[t]]

  # define RGB image
  # rgbImage <- plotRGB(trainingRasterList[[t]],
  #                     r = 3,
  #                     g = 2,
  #                     b = 1,
  #                     stretch = "lin")

  rgbDataframe <- data.frame(Iteration = 1,
                             Timestep = t,
                             RGBImage = file.path(paste0(transferDir, "/RGBImage", t, ".png")))

  # save raster
  writeRaster(predictedPresence,
              filename = file.path(paste0(transferDir,
                                          "/PredictedPresence",
                                          t,
                                          ".tif")),
              overwrite = TRUE)

  writeRaster(groundTruth,
              filename = file.path(paste0(transferDir,
                                          "/GroundTruth",
                                          t,
                                          ".tif")),
              overwrite = TRUE)

  writeRaster(probabilityRaster,
              filename = file.path(paste0(transferDir,
                                          "/Probability",
                                          t,
                                          ".tif")),
              overwrite = TRUE)

  # save RBG Image
  # Step 1: Call the pdf command to start the plot
  # pdf(file = file.path(paste0(transferDir,
  #                             "/RGBImage",
  #                             t,
  #                             ".pdf")),
  #   width = 4,
  #   height = 4)

  # # Step 2: Create the plot with R code
  # plotRGB(trainingRasterList[[t]],
  #                     r = 3,
  #                     g = 2,
  #                     b = 1,
  #                     stretch = "lin")

  # # Step 3: Run dev.off() to create the file!
  # dev.off()

  # transferDir <- "C:/Users/HannahAdams/Documents"
  # ggsave(filename = file.path(paste0(transferDir,
  #                                    "/RGBImage",
  #                                    1,
  #                                    ".png")),
  #        rgbImage)

  # save image
  png(file = file.path(paste0(transferDir,
                              "/RGBImage",
                              t,
                              ".png")))
  plotRGB(trainingRasterList[[t]],
          r = 3,
          g = 2,
          b = 1,
          stretch = "lin")
  dev.off()

  # Store the relevant outputs from both rasters in a temporary dataframe
  rasterOutputDataframe <- addRow(rasterOutputDataframe,
                                  rasterDataframe)

  imageOutputDataframe <- addRow(imageOutputDataframe,
                                 rgbDataframe)
}

# calculate mean values for model statistics -----------------------------------
prediction <- predict(rf1, allTestData)
confusionMatrix <- confusionMatrix(data.frame(prediction)[, 1], allTestData$presence)

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

# now what to do with the confusion matrix outputs? Average from all of them?
confusionOutputDataframe <- addRow(confusionOutputDataframe,
                                   confusion_matrix)

modelOutputDataframe <- addRow(modelOutputDataframe,
                               model_stats)

# add variable importance to output datasheet ---------------------------------
ggsave(filename = file.path(paste0(transferDir, "/VariableImportance.png")),
       variableImportancePlot)

varImportanceOutputDataframe <- data.frame(VariableImportance = file.path(paste0(transferDir, "/VariableImportance.png")))

# add filtering values to output datasheet
filteringOutputDataframe <- data.frame(filterResolutionOutput = filterResolution,
                                       filterThresholdOutput = filterPercent)

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

saveDatasheet(myScenario,
              data = filteringOutputDataframe,
              name = "imageclassifier_FilterStatistics")

saveDatasheet(myScenario,
              data = varImportanceOutputDataframe,
              name = "imageclassifier_ModelOutput")

saveDatasheet(myScenario,
              data = imageOutputDataframe,
              name = "imageclassifier_ImageOutput")

# remove excess code
# troubleshoot timesteps for image outputs
