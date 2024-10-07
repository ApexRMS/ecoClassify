# load packages ---------------------------------------------------
library(rsyncrosim)
library(tidyverse)
library(terra)
library(sf)
library(ranger)
library(caret)
library(gtools)
library(reshape2)
library(roxygen2)
library(codetools)

# define functions ------------------------------------------------

#' Assign objects froma a datasheet of variables
#'
#' @description
#' 'assignVariables' extracts variables from datashseets in the specified
#' syncrosim scenario and assigns them to an object.
#'
#' @param myScenario syncrosim scenario object
#' @return list of objects (timesteps = numeric, nObs = numeric,
#' filterresolution = numeric, filterPercent = numeric,
#' applyFiltering = boolean) that have been extracted from the syncrosim
#' datasheet
#'
#' @details
#' This function is specifically designed for the the watchtower package
#' @noRd
assignVariables <- function(myScenario) {

  # Load RunControl datasheet
  runSettings <- datasheet(myScenario, name = "imageclassifier_RunControl")

  # Extract timesteps based on min and max input values
  timesteps <- seq(runSettings$MinimumTimestep, runSettings$MaximumTimestep)

  # Load model input datasheet
  modelInputDataframe <- datasheet(myScenario,
                                   name = "imageclassifier_ModelInput")

  # Extract model input values
  nObs <- modelInputDataframe$nObs
  filterResolution <- modelInputDataframe$filterResolution
  filterPercent <- modelInputDataframe$filterPercent
  applyFiltering <- modelInputDataframe$applyFiltering

  # return as a list
  return(list(timesteps,
              nObs,
              filterResolution,
              filterPercent,
              applyFiltering))
}

#' Extract rasters from filepaths in a dataframe
#'
#' @description
#' 'extractRasters' takes a dataframe of raster filepaths and creates
#' a list with one raster for each timestep
#'
#' @param dataframe dataframe containing timesteps in column 1 and
#' raster filepaths in column 2
#' @return list of rasters (spatRaster), one for each timestep
#'
#' @details
#' The dataframe is first subset based on timestep. Rasters from the same timestep
#' are combined into one raster using the terra package, and added to a list.
#' @noRd
extractRasters <- function(dataframe) {

  # define timesteps
  timesteps <- unique(dataframe[, 1])

  # create an empty list
  rasterList <- c()

  # loop through timesteps, combining rasters
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

#' Extract all rasters from the syncrosim library
#'
#' @description
#' 'extractAllRasters' is a wrapper function to extract rasters from
#' all input datasheets using the extractRasters function
#'
#' @param rasterTrainingDataframe dataframe of timesteps and filepaths
#' for training data
#' @param rasterGroundTruthDataframe dataframe of timesteps and filepaths
#' for ground truth data
#' @param rasterToClassifyDataframe dataframe of timesteps and filepaths
#' for rasters to classify
#' @return list of raster lists for each input dataframe (spatRasters)
#'
#' @details
#' rasterToClassifyDataframe can be an empty dataframe and will return
#' an empty list
#' @noRd
extractAllRasters <- function(rasterTrainingDataframe,
                              rasterGroundTruthDataframe,
                              rasterToClassifyDataframe) {

  # extract training rasters
  trainingRasterList <- extractRasters(rasterTrainingDataframe)

  # extract ground truth rasters
  groundTruthRasterList <- extractRasters(rasterGroundTruthDataframe)

  # extract rasters to classify
  if (length(rasterToClassifyDataframe$RasterFileToClassify) > 1) {
    toClassifyRasterList <- extractRasters(rasterToClassifyDataframe)
  } else {
    toClassifyRasterList <- ""
  }

  return(list(trainingRasterList,
              groundTruthRasterList,
              toClassifyRasterList))
}

#' Decompose rasters for image classification
#'
#' @description
#' 'decomposedRaster' samples data from a training and ground truth raster,
#' returning a dataframe for training the image classifier model
#'
#' @param predRast training raster (spatRaster)
#' @param responseRast ground truth raster (spatRaster)
#' @param nobs number of observations to sample (integer)
#' @return dataframe of data sampled from the rasters
#'
#' @details
#' Returned dataframe will be split into training and testing data.
#' @noRd
decomposedRaster <- function(predRast,
                             responseRast,
                             nobs) {

  # randomly sample points in the training raster
  randomPoints <- spatSample(responseRast,
                             size = nobs,
                             na.rm = TRUE,
                             as.points = TRUE,
                             replace = FALSE,
                             method = "stratified")

  # extract values from the training and ground truth rasters
  randomPoints <- unique(randomPoints)
  responseValues <- terra::extract(responseRast, randomPoints)
  predValues <- terra::extract(predRast, randomPoints)

  # bind into single dataframe
  trainData <- cbind(predValues, response = responseValues[, 2])

  return(trainData)
}

#' Plot variable importance from random forest model
#'
#' @description
#' 'plotVariableImportance' creates and writes variable importance plot
#' from the random forest model
#'
#' @param model random forest model (output from ranger)
#' @param transferDir filepath for exporting the plot
#' @return variable importance plot (ggplot) and dataframe with filepath
#' to where the plot was written
#'
#' @details
#' transferDir is defined based on the ssim session.
#' @noRd
plotVariableImportance <- function(model,
                                   transferDir) {

  # extract variable importance
  variableImportance <- melt(model$variable.importance) %>%
    tibble::rownames_to_column("variable")

  # make a variable importance plot for specified model
  variableImportancePlot <- ggplot(variableImportance,
                                   aes(x = reorder(variable, value),
                                       y = value,
                                       fill = value)) +
    geom_bar(stat = "identity", width = 0.8) +
    coord_flip() +
    ylab("Variable Importance") +
    xlab("Variable") +
    ggtitle("Information Value Summary") +
    theme_classic(base_size = 26) +
    scale_fill_gradientn(colours = c("#424352"), guide = "none")

  # save variable importance plot
  ggsave(filename = file.path(paste0(transferDir, "/VariableImportance.png")),
         variableImportancePlot)

  # Generate dataframe
  varImportanceOutputDataframe <- data.frame(VariableImportance = file.path(paste0(transferDir, "/VariableImportance.png")))

  return(list(variableImportancePlot, varImportanceOutputDataframe))
}

#' Split image data for training and testing
#'
#' @description
#' 'splitTrainTest' is a wrapper for the decomposeRaster function,
#' splitting the output into testing and training data.
#'
#' @param trainingRasterList list of training rasters (spatRasters)
#' @param groundTruthRasterList list of ground truth rasters (spatRasters)
#' @param nObs number of observations to sample from the training raster
#' @return separate dataframes from testing and training data
#'
#' @details
#' Both input rasters lists must be the same length.
#' @noRd
splitTrainTest <- function(trainingRasterList,
                           groundTruthRasterList,
                           nObs) {

  # create empty lists for binding data
  allTrainData <- c()
  allTestData <- c()

  # For loop through each raster pair
  for (i in seq_along(trainingRasterList)) {

    ## Decompose satellite image raster
    modelData <- decomposedRaster(trainingRasterList[[i]],
                                  groundTruthRasterList[[i]],
                                  nobs = nObs)

    # format sampled data
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

  return(list(allTrainData, allTestData))
}

predictRanger <- function(raster, model) {
  ## generate blank raster
  predictionRaster <- raster[[1]]
  names(predictionRaster) <- "present"

  ## predict over raster decomposition
  rasterMatrix <- data.frame(raster)
  rasterMatrix <- na.omit(rasterMatrix)
  predictedValues <- data.frame(predict(model, rasterMatrix))[, 1]
  values(predictionRaster) <- predictedValues

  return(predictionRaster)
}

getPredictionRasters <- function(trainingRasterList,
                                 t,
                                 model1,
                                 model2) {
  # predict presence for each raster
  predictedPresence <- predictRanger(trainingRasterList[[t]],
                                     model1)

  # generate probabilities for each raster
  probabilityRaster <- 1 - (predictRanger(trainingRasterList[[t]],
                                          model2))
  # assign values
  values(predictedPresence) <- ifelse(values(predictedPresence) == 2, 1, 0)

  return(list(predictedPresence,
              probabilityRaster))
}

filterFun <- function(raster, resolution, percent) {
  npixels <- resolution^2
  midPixel <- (npixels + 1) / 2
  threshold <- round(npixels * percent, 0)
  if (is.na(raster[midPixel])) {
    return(NA)
  } else if (raster[midPixel] == 1 && sum(raster[-midPixel] == 0, na.rm = TRUE) > threshold) {
    return(0)
  } else {
    return(raster[midPixel])
  }
}

generateRasterDataframe <- function(applyFiltering,
                                    predictedPresence,
                                    filterResolution,
                                    filterPercent,
                                    t,
                                    transferDir) {

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

    rasterDataframe <- data.frame(
      Iteration = 1,
      Timestep = t,
      PredictedUnfiltered = file.path(paste0(transferDir,
                                             "/PredictedPresence",
                                             t,
                                             ".tif")),
      PredictedFiltered = file.path(paste0(transferDir,
                                           "/filteredPredictedPresence",
                                           t,
                                           ".tif")),
      GroundTruth = file.path(paste0(transferDir,
                                     "/GroundTruth",
                                     t,
                                     ".tif")),
      Probability = file.path(paste0(transferDir,
                                     "/Probability",
                                     t,
                                     ".tif"))
    )
  } else {
    rasterDataframe <- data.frame(
      Iteration = 1,
      Timestep = t,
      PredictedUnfiltered = file.path(paste0(transferDir,
                                             "/PredictedPresence",
                                             t,
                                             ".tif")),
      PredictedFiltered = "",
      GroundTruth = file.path(paste0(transferDir,
                                     "/GroundTruth",
                                     t,
                                     ".tif")),
      Probability = file.path(paste0(transferDir,
                                     "/Probability",
                                     t,
                                     ".tif"))
    )
  }

  # add to output dataframe
  rasterOutputDataframe <- addRow(rasterOutputDataframe,
                                  rasterDataframe)

  return(rasterOutputDataframe)
}

getRgbDataframe <- function(rgbOutputDataframe,
                            t,
                            transferDir) {

  rgbDataframe <- data.frame(Iteration = 1,
                             Timestep = t,
                             RGBImage = file.path(paste0(transferDir,
                                                         "/RGBImage",
                                                         t,
                                                         ".png")))

  rgbOutputDataframe <- addRow(rgbOutputDataframe,
                               rgbDataframe)

  return(rgbOutputDataframe)

}

saveFiles <- function(predictedPresence,
                      groundTruth,
                      probabilityRaster,
                      trainingRasterList,
                      variableImportancePlot,
                      t,
                      transferDir) {

  # save rasters
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

}

calculateStatistics <- function(model,
                                testData,
                                confusionOutputDataframe,
                                modelOutputDataframe) {

  prediction <- predict(model, testData)
  confusionMatrix <- confusionMatrix(data.frame(prediction)[, 1],
                                     testData$presence)

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

  # add confusion matrix and model statistics to dataframe
  confusionOutputDataframe <- addRow(confusionOutputDataframe,
                                     confusion_matrix)

  modelOutputDataframe <- addRow(modelOutputDataframe,
                                 model_stats)

  return(list(confusionOutputDataframe, modelOutputDataframe))
}
