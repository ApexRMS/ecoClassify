# load packages ---------------------------------------------------
update.packages(repos='http://cran.us.r-project.org', ask = FALSE, oldPkgs = c("terra"))

install.packages("reshape2", repos='http://cran.us.r-project.org')

library(rsyncrosim)
library(tidyverse)
library(terra)
library(sf)
library(ranger)
library(caret)
library(gtools)
library(codetools)
library(ENMeval)
library(rJava)
library(ecospat)
library(ENMeval)
library(cvms)
library(foreach)
library(doParallel)

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
assignVariables <- function(myScenario,
                            inputRasterDataframe,
                            column) {

  # extract unique timesteps from inputRasterDataframe --------------------------
  timestepList <- inputRasterDataframe %>%
    filter(!is.na(column)) %>%
    pull(Timesteps) %>%
    unique()

  # Load classifier options datasheet
  classifierOptionsDataframe <- datasheet(myScenario,
                                          name = "imageclassifier_ClassifierOptions")

  # Extract model input values
  nObs <- classifierOptionsDataframe$nObs
  applyContextualization <- classifierOptionsDataframe$applyContextualization
  modelType <- as.character(classifierOptionsDataframe$modelType)

  # Load post-processing options datasheet
  postProcessingDataframe <- datasheet(myScenario,
                                       name = "imageclassifier_PostProcessingOptions")

  # Extract post-processing values
  filterResolution <- postProcessingDataframe$filterResolution
  filterPercent <- postProcessingDataframe$filterPercent
  applyFiltering <- postProcessingDataframe$applyFiltering

  # apply default filtering values if not specified
  if (is.na(filterResolution) && applyFiltering == TRUE) {
    filterResolution <- 5
  }

  if (is.na(filterPercent) && applyFiltering == TRUE) {
    filterPercent <- 0.25
  }

  # return as a list
  return(list(timestepList,
              nObs,
              filterResolution,
              filterPercent,
              applyFiltering,
              applyContextualization,
              modelType))
}

## Set cores

#' Set the number of cores for multiprocessing
#'
#' @description
#' 'setCores' determines the number of cores to use for multiprocessing based on the available cores and user settings.
#'
#' @param mulitprocessingSheet A dataframe containing multiprocessing settings. It should have the columns 'EnableMultiprocessing' (logical) and 'MaximumJobs' (integer).
#' @return The number of cores to use for multiprocessing (integer).
#'
#' @details
#' The function first detects the number of available cores on the system. If multiprocessing is enabled in the 'mulitprocessingSheet', it checks if the requested number of cores exceeds the available cores. If so, it sets the number of cores to one less than the available cores and issues a warning. Otherwise, it sets the number of cores to the requested number. If multiprocessing is not enabled, it sets the number of cores to 1.
#' @noRd
setCores <- function(mulitprocessingSheet) {
  availableCores <- parallel::detectCores()
  if(mulitprocessingSheet$EnableMultiprocessing) {
    requestedCores <- mulitprocessingSheet$MaximumJobs
    if(requestedCores > availableCores) {
      warning(paste0("Requested number of jobs exceeds available cores. Continuing run with ",availableCores," jobs."))
      nCores <- availableCores - 1
    } else {
      nCores <- requestedCores
    }
  } else {
    nCores <- 1
  }
  return(nCores)
}



#' Extract rasters from filepaths in a dataframe
#'
#' @description
#' 'extractRasters' takes a dataframe of raster filepaths and creates
#' a list with one raster for each timestep
#'
#' @param dataframe column 1 = timestep, column 2 = filepath (dataframe)
#' @return list of rasters (spatRaster), one for each timestep
#'
#' @details
#' The dataframe is first subset based on timestep. Rasters from the same
#' timestep are combined into one raster using the terra package, and added
#' to a list.
#' @noRd
extractRasters <- function(dataframe,
                           column) {

  # remove rows with NA values in the second column
  dataframe <- dataframe %>% filter(!is.na(dataframe[, column]))

  # define timesteps
  timesteps <- unique(dataframe[, 1])

  # create an empty list
  rasterList <- c()

  # loop through timesteps, combining rasters
  for (t in timesteps) {

    # subset based on timestep
    subsetData <- dataframe %>% filter(Timesteps == t)
    # list all files
    allFiles <- as.vector(subsetData[, column])
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
extractAllRasters <- function(inputRasterDataframe) {

  if (modelType == "Random Forest") {
    # extract training rasters
    trainingRasterList <- extractRasters(inputRasterDataframe,
                                         column = 2)

    # extract ground truth rasters
    groundTruthRasterList <- extractRasters(inputRasterDataframe,
                                            column = 3)

    # warning if training and ground truth rasters are different lengths
    # if(length(rasterTrainingDataframe) != length(rasterGroundTruthDataframe)) stop('must have equal number of training and ground truth rasters')

    return(list(trainingRasterList,
                groundTruthRasterList))
  } else if (modelType == "MaxEnt") {

    # extract rasters to classify
    toClassifyRasterList <- extractRasters(inputRasterDataframe,
                                           column = 4)

    return(toClassifyRasterList)

  }

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
#' @param importanceData vector of importance values (numeric) with names attribute
#' @param transferDir filepath for exporting the plot
#' @return variable importance plot (ggplot) and dataframe with filepath
#' to where the plot was written
#'
#' @details
#' transferDir is defined based on the ssim session.
#' @noRd
plotVariableImportance <- function(importanceData,
                                   transferDir) {

  # extract variable importance
  variableImportance <- data.frame(importanceData) %>%
    tibble::rownames_to_column("variable") %>%
    rename(value = 2)

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

  return(list(allTrainData,
              allTestData))
}

#' Predict presence over area
#'
#' @description
#' 'predictRanger' uses a random forest model to predict presence
#' accross the full extent of the training rasters
#'
#' @param raster training raster (spatRaster)
#' @param model random forest model (random forest object)
#' @return raster of predicted presence (spatRaster)
#'
#' @details
#' Used inside getPredictionRasters wrapper function
#' @noRd
predictRanger <- function(raster,
                          model) {
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

#' Predict presence and generate probability values over area
#'
#' @description
#' 'getPredictionRasters' is a wrapper function for predictRanger,
#' generating a raster of predicted presence and a raster
#' of probabilities
#'
#' @param trainingRaster training raster for predictRanger (spatRaster)
#' @param model predictive model for generating probability values (random forest or maxent object)
#' @param threshold threshold for converted results into binary outcomes (numeric)
#' @param modelType type of model used (character)
#' @return raster of predicted presence and probability values (spatRaster)
#'
#' @details
#'
#' @noRd
getPredictionRasters <- function(raster,
                                 model,
                                 threshold,
                                 modelType = "Random Forest") {
  # predict presence for each raster
  if (modelType == "Random Forest") {
    # generate probabilities for each raster
    probabilityRaster <- 1 - predictRanger(raster,
                                           model)
  } else if (modelType == "MaxEnt") {
    probabilityRaster <- predict(model, raster, type = "logistic")
  }  else {
    stop("Model type not recognized")
  }

  predictedPresence <- reclassifyRaster(probabilityRaster, threshold)

  return(list(predictedPresence = predictedPresence,
              probabilityRaster = probabilityRaster))
}

reclassifyRaster <- function(raster, threshold) {
  raster[raster >= threshold] <- 1
  raster[raster < threshold] <- 0
  return(raster)
}

#' Filter prediction raster
#'
#' @description
#' 'filterFun' filters out presence cells in input raster based on the classification
#' of surrounding cells.
#'
#' @param raster prediction raster to filter (spatRaster)
#' @param resolution resolution to apply filtering (numeric)
#' @param percent threshold for filtering (numeric)
#' @return filtered raster (spatRaster)
#'
#' @details
#' Used in generateRasterDataframe wrapper function if filtering is selected.
#' @noRd
filterFun <- function(raster,
                      resolution,
                      percent) {
  # define parameters
  npixels <- resolution^2
  midPixel <- (npixels + 1) / 2
  threshold <- round(npixels * percent, 0)

  # filter
  if (is.na(raster[midPixel])) {
    return(NA)
  } else if (raster[midPixel] == 1 && sum(raster[-midPixel] == 0, na.rm = TRUE) > threshold) {
    return(0)
  } else {
    return(raster[midPixel])
  }
}

#' Generate raster output dataframe
#'
#' @description
#' 'generateRasterDataframe' saves output raster files and generates output dataframe
#' for rasterOutput syncrosim datasheet.
#'
#' @param applyFiltering determines whether filtering is to be applied (boolean)
#' @param predictedPresence raster of predicted presence from predictRanger (spatRaster)
#' @param filterResolution resolution to apply filtering (numeric)
#' @param filterResolution threshold for filtering (numeric)
#' @param t iteration (integer)
#' @param trandferDir directory for saving files (character)
#' @return filtered raster (spatRaster)
#'
#' @details
#' Uses filterFun if applyFiltering is TRUE. If filtering is not applied the filtered output 
#' raster cell in the dataframe is left empty
#' @noRd
generateRasterDataframe <- function(applyFiltering,
                                    predictedPresence,
                                    filterResolution,
                                    filterPercent,
                                    category,
                                    timestep,
                                    transferDir,
                                    OutputDataframe,
                                    hasGroundTruth) {
  if (hasGroundTruth == TRUE) {
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
                                              "/filteredPredictedPresence-",
                                              category,
                                              "-t",
                                              timestep,
                                              ".tif")),
                  overwrite = TRUE)

      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        PredictedUnfiltered = file.path(paste0(transferDir,
                                               "/PredictedPresence-",
                                               category,
                                               "-t",
                                               timestep,
                                               ".tif")),
        PredictedFiltered = file.path(paste0(transferDir,
                                             "/filteredPredictedPresence-",
                                             category,
                                             "-t",
                                             timestep,
                                             ".tif")),
        GroundTruth = file.path(paste0(transferDir,
                                       "/GroundTruth-t",
                                       timestep,
                                       ".tif")),
        Probability = file.path(paste0(transferDir,
                                       "/Probability-",
                                       category,
                                       "-t",
                                       timestep,
                                       ".tif"))
      )
    } else {
      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        PredictedUnfiltered = file.path(paste0(transferDir,
                                               "/PredictedPresence-",
                                               category,
                                               "-t",
                                               timestep,
                                               ".tif")),
        PredictedFiltered = "",
        GroundTruth = file.path(paste0(transferDir,
                                       "/GroundTruth-t",
                                       timestep,
                                       ".tif")),
        Probability = file.path(paste0(transferDir,
                                       "/Probability-",
                                       category,
                                       "-t",
                                       timestep,
                                       ".tif"))
      )
    }

    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe,
                              rasterDataframe)
  } else {
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
                                              "/filteredPredictedPresence-",
                                              category,
                                              "-t",
                                              timestep,
                                              ".tif")),
                  overwrite = TRUE)

      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        ClassifiedUnfiltered = file.path(paste0(transferDir,
                                                "/PredictedPresence-",
                                                category,
                                                "-t",
                                                timestep,
                                                ".tif")),
        ClassifiedFiltered = file.path(paste0(transferDir,
                                              "/filteredPredictedPresence-",
                                              category,
                                              "-t",
                                              timestep,
                                              ".tif")),
        ClassifiedProbability = file.path(paste0(transferDir,
                                                 "/Probability-",
                                                 category,
                                                 "-t",
                                                 timestep,
                                                 ".tif"))
      )
    } else {
      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        ClassifiedUnfiltered = file.path(paste0(transferDir,
                                                "/PredictedPresence-",
                                                category,
                                                "-t",
                                                timestep,
                                                ".tif")),
        ClassifiedFiltered = "",
        ClassifiedProbability = file.path(paste0(transferDir,
                                                 "/Probability-",
                                                 category,
                                                 "-t",
                                                 timestep,
                                                 ".tif"))
      )
    }

    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe,
                              rasterDataframe)
  }

  return(OutputDataframe)
}

#' Generate RGB output dataframe
#'
#' @description
#' 'getRgbDataframe' generates output dataframe for RgbOutput syncrosim datasheet.
#'
#' @param rgbOutputDataframe RGB dataframe to be added to (dataframe)
#' @param t iteration (integer)
#' @param trandferDir directory for saving files (character)
#' @return filtered raster (spatRaster)
#'
#' @details
#'
#' @noRd
getRgbDataframe <- function(rgbOutputDataframe,
                            category,
                            timestep,
                            transferDir) {

  rgbDataframe <- data.frame(Timestep = timestep,
                             RGBImage = file.path(paste0(transferDir,
                                                         "/RGBImage-",
                                                         category,
                                                         "-t",
                                                         timestep,
                                                         ".png")))

  rgbOutputDataframe <- addRow(rgbOutputDataframe,
                               rgbDataframe)

  return(rgbOutputDataframe)

}

#' Save raster and image files
#'
#' @description
#' 'saveFiles' saves raster and images files to transfer directory so they can be
#' referenced in the syncrosim datasheets.
#'
#' @param predictedPresence predicted presence raster (spatRaster)
#' @param groundTruth ground truth raster (spatRaster)
#' @param probabilityRaster probability raster (spatRaster)
#' @param trainingRasterList list of training rasters for generating RGB image ( list of spatRasters)
#' @param t iteration (integer)
#' @param trandferDir directory for saving files (character)
#'
#' @noRd
saveFiles <- function(predictedPresence,
                      groundTruth = NULL,
                      probabilityRaster,
                      trainingRasterList,
                      category,
                      timestep,
                      transferDir) {

  # save rasters
  writeRaster(predictedPresence,
              filename = file.path(paste0(transferDir,
                                          "/PredictedPresence-",
                                          category,
                                          "-t",
                                          timestep,
                                          ".tif")),
              overwrite = TRUE)

  if (!is.null(groundTruth)) {
    writeRaster(groundTruth,
                filename = file.path(paste0(transferDir,
                                            "/GroundTruth-t",
                                            timestep,
                                            ".tif")),
                overwrite = TRUE)
  }
  writeRaster(probabilityRaster,
              filename = file.path(paste0(transferDir,
                                          "/Probability-",
                                          category,
                                          "-t",
                                          timestep,
                                          ".tif")),
              overwrite = TRUE)

  # save RBG Image
  png(file = file.path(paste0(transferDir,
                              "/RGBImage-",
                              category,
                              "-t",
                              timestep,
                              ".png")))
  plotRGB(trainingRasterList[[t]],
          r = 3,
          g = 2,
          b = 1,
          stretch = "lin")
  dev.off()
}

#' Calculate statistics from random forest model predictions
#'
#' @description
#' 'calculateStatistics' predicts presence based on all test data and 
#' calculates a confusion matrix and other key statistics
#'
#' @param model random forest model (random forest object)
#' @param testData test data to make prediction with (dataframe)
#' @param threshold threshold for converted results into binary outcomes (numeric)
#' @param confusionOutputDataframe empty dataframe for confusion matrix results (dataframe)
#' @param modelOutputDataframe empty dataframe for model statistics (dataframe)
#' @return data frames with confusion matrix results and model statistics
#'
#' @details
#' Output dataframes are saved to ConfusionMatrix and modelStatistics output datasheets
#' @noRd
calculateStatistics <- function(model,
                                testData,
                                threshold,
                                confusionOutputDataframe,
                                modelOutputDataframe) {
  if (inherits(model, "ranger")) {
    prediction <- predict(model, testData)$predictions[, 2]
  } else {
    prediction <- predict(model, testData, type = "logistic")
  }

  prediction <- as.factor(ifelse(prediction >= threshold, 2, 1))

  confusionMatrix <- confusionMatrix(prediction,
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
    tibble::rownames_to_column("Statistics") %>%
    mutate(Statistic = case_when(
      Statistics == "AccuracyLower" ~ "Accuracy (lower)",
      Statistics == "AccuracyUpper" ~ "Accuracy (upper)",
      Statistics == "AccuracyNull" ~ "Accuracy (null)",
      Statistics == "AccuracyPValue" ~ "Accuracy P Value",
      Statistics == "McnemarPValue" ~ "Mcnemar P value",
      Statistics == "Neg Pred Value" ~ "Negative Predictive Value",
      Statistics == "Pos Pred Value" ~ "Positive Predictive Value",
      TRUE ~ Statistics
    )) %>%
    select(-Statistics)

  # add confusion matrix and model statistics to dataframe
  confusionOutputDataframe <- addRow(confusionOutputDataframe,
                                     confusion_matrix)

  modelOutputDataframe <- addRow(modelOutputDataframe,
                                 model_stats)

  # make a confusion matrix plot
  confusionMatrixPlot <- plot_confusion_matrix(
    as_tibble(confusion_matrix),
    target_col = "Reference",
    prediction_col = "Prediction",
    counts_col = "Frequency",
    font_counts = font(size = 15),
    font_normalized = font(size = 6),
    font_row_percentages = font(size = 6),
    font_col_percentages = font(size = 6),) +
    ggplot2::theme(axis.title = element_text(size = 25),
                   axis.text = element_text(size = 25))

  return(list(confusionOutputDataframe,
              modelOutputDataframe,
              confusionMatrixPlot))
}

# check for issues with data structure -----------------------------
checkTimesteps <- function(timesteps,
                           rasterTrainingDataframe,
                           rasterGroundTruthDataframe) {

  # check if all timesteps are included in both dataframes
  trainingTimesteps <- rasterTrainingDataframe %>%
    pull(Timesteps) %>%
    unique()

  groundTruthTimesteps <- rasterGroundTruthDataframe %>%
    pull(Timesteps) %>%
    unique()

  # convert timesteps to numeric
  timestepsNumeric <- as.numeric(timesteps)

  if (!identical(trainingTimesteps, groundTruthTimesteps)) stop("must have same timesteps for training and ground truth raster input datasheets")
  if (!identical(timestepsNumeric, trainingTimesteps)) warning('timestep range does not match training raster input datasheet')
  if (!identical(timestepsNumeric, groundTruthTimesteps)) warning('timestep range does not match ground truth raster input datasheet')

}

checkOutputDataframes <- function(rasterOutputDataframe,
                                  confusionOutputDataframe,
                                  modelOutputDataframe,
                                  rgbOutputDataframe) {

  # check that rasterOutputDataframe has the correct data types
  if (!is.numeric(rasterOutputDataframe$Timestep)) warning('Incorrect data type for Timestep in raster output datasheet')
  if (!is.character(rasterOutputDataframe$PredictedUnfiltered)) warning('Incorrect data type for unfiltered prediction filepath in raster output datasheet')
  if (!is.character(rasterOutputDataframe$PredictedFiltered)) warning('Incorrect data type for filtered prediction filepath in raster output datasheet')
  if (!is.character(rasterOutputDataframe$GroundTruth)) warning('Incorrect data type for ground truth filepath in raster output datasheet')
  if (!is.character(rasterOutputDataframe$Probability)) warning('Incorrect data type for probability filepath in raster output datasheet')

  # check that confusionOutputDatafram has the correct data types
  if (!is.numeric(confusionOutputDataframe$Prediction)) warning('Incorrect data type for Prediction in confusion matrix output')
  if (!is.numeric(confusionOutputDataframe$Reference)) warning('Incorrect data type for Reference in confusion matrix output')
  if (!is.numeric(confusionOutputDataframe$Frequency)) warning('Incorrect data type for Frequency in confusion matrix output')

  # check that modelOutputDataframe has the correct data types
  if (!is.character(modelOutputDataframe$Statistic)) warning('Incorrect data type for Statistic in model statistics output')
  if (!is.numeric(modelOutputDataframe$Value)) warning('Incorrect data type for Value in model statistics output')

  # check that rgbOutputDataframe has the correct data types
  if (!is.numeric(rgbOutputDataframe$Timestep)) warning('Incorrect data type for Timestep in RBG raster output')
  if (!is.character(rgbOutputDataframe$RGBImage)) warning('Incorrect data type for RGBImage in RBG raster output')
}

# context functions ------------------------------
getRasterAdjacencyValues <- function(rasterIn) {
    adjacentPixels <- adjacent(rasterIn, 1:ncell(rasterIn), directions = 8)
    contextColourData <- data.frame()
  for (i in 1:nrow(adjacentPixels)) {
    adjacentPixelMeans <- colMeans(rasterIn[adjacentPixels[i,]], na.rm=T)
    adjacentPixelMeansDF <- data.frame(t(adjacentPixelMeans))
    adjacentPixelMeansDF[, "ID"] <- i
    contextColourData <- rbind(contextColourData, adjacentPixelMeansDF)
  }
names(contextColourData)[1:4] <- paste0(names(contextColourData)[1:4], "_adjacent")
return(contextColourData)
}

addRasterAdjacencyValues <- function(rasterIn) {
  contextColourData <- getRasterAdjacencyValues(rasterIn)
  extendedRaster <- rasterIn
  for (i in 1:nlyr(rasterIn)) {
    values(extendedRaster[[i]]) <- contextColourData[, i]
  }
  names(extendedRaster) <- paste0(names(extendedRaster), "_adjacent")
  return(extendedRaster)
}

contextualizeRaster <- function(rasterList) {

  contextualizedRasterList <- c()

  for (r in seq_along(rasterList)) {
    raster <- rasterList[[r]]
    adjacentRaster <- addRasterAdjacencyValues(raster)
    combinedRaster <- c(raster, adjacentRaster)

    contextualizedRasterList <- c(contextualizedRasterList,
                                  combinedRaster)
  }

  return(contextualizedRasterList)

}

## MaxEnt model and training

getMaxentModel <- function(allTrainData, nCores, isTuningOn) {

  ## Specifying feature classes and regularization parameters for Maxent
if (isTuningOn) {
  tuneArgs <- list(fc = c("L", "Q", "P", "LQ", "H", "LQH", "LQHP"), 
                   rm = seq(0.5, 3, 0.5))
} else {
  tuneArgs <- list(fc = c("LQH"), 
                   rm = 1)
}

  absenceTrainData <- allTrainData[allTrainData$presence == 1, grep("presence|kfold", colnames(allTrainData), invert=T)]
  presenceTrainData <- allTrainData[allTrainData$presence == 2, grep("presence|kfold", colnames(allTrainData), invert=T)]
  max1 <- ENMevaluate(occ = presenceTrainData, # absenceTrainData,
                      bg.coords = absenceTrainData, # presenceTrainData,
                      tune.args = tuneArgs,
                      progbar = F,
                      partitions = "randomkfold",
                      parallel = T,
                      numCores = nCores,
                      quiet = T, ## silence messages but not errors
                      algorithm = 'maxent.jar')

  bestMax <- which.max(max1@results$cbi.val.avg)
  varImp <- max1@variable.importance[bestMax] %>% data.frame()
  names(varImp) <- c("variable","percent.contribution","permutation.importance")
  maxentImportance <- getMaxentImportance(varImp)

  model <- max1@models[[bestMax]]
  modelOut <- max1@results[bestMax,]

  return(list(model, maxentImportance))

}

getMaxentImportance <- function(varImp) {
  maxentImportance <- as.numeric(varImp$percent.contribution)
  attr(maxentImportance, "names") <- varImp$variable
  return(maxentImportance)

}

### Random forest training

## find sensitivity and specificity values
getSensSpec <- function(probs, actual, threshold) {
  predicted <- ifelse(probs >= threshold, 1, 0)
  confMatrix <- confusionMatrix(as.factor(predicted), as.factor(actual))

  sensitivity <- confMatrix$byClass['Sensitivity']
  specificity <- confMatrix$byClass['Specificity']

  return(c(sensitivity, specificity))
}

## find optimal threshold between sensitivity and specificity
getOptimalThreshold <- function(model, testingData, modelType = "Random Forest") {

  thresholds <- seq(0.01, 0.99, by = 0.01)
  testingObservations <- as.numeric(testingData$presence) - 1

  ## predicting data
  if (modelType == "Random Forest") {
    testingPredictions <- predict(model, testingData)$predictions[, 2]
  } else if (modelType == "MaxEnt") {
    testingPredictions <- predict(model, testingData, type = "logistic")
  } else {
    stop("Model type not recognized")
  }

  # Calculate sensitivity and specificity for each threshold
  metrics <- t(sapply(thresholds, getSensSpec, probs = testingPredictions, actual = testingObservations))
  youdenIndex  <- metrics[, 1] + metrics[, 2] - 1
  optimalYouden <- thresholds[which.max(youdenIndex)]

  return(optimalYouden)
}

getRandomForestModel <- function(allTrainData, nCores, isTuningOn) {
  trainingVariables <-  grep("presence|kfold", colnames(allTrainData), invert=T, value=T)

  mainModel <- formula(sprintf("%s ~ %s",
                              "presence",
                              paste(trainingVariables,
                                    collapse = " + ")))

   ## Specifying feature classes and regularization parameters for Maxent
if (isTuningOn) {
  tuneArgs <- list(mtry = c(1:6),  ## number of splits
                   maxDepth = seq(0, 1, 0.2), ## regulariation amount
                   nTrees = c(500, 1000, 2000)) ## number of trees
  tuneArgsGrid <- expand.grid(tuneArgs)
} else {
  tuneArgs <- list(mtry = round(sqrt(length(trainingVariables)),0),  ## defaults
                   maxDepth = 0,
                   nTrees = 500)
  tuneArgsGrid <- expand.grid(tuneArgs)
}

  results <- foreach(i = 1:nrow(tuneArgsGrid), .combine = rbind, .packages = "ranger") %dopar% {
  rf1 <-  ranger(mainModel,
                data = allTrainData,
                mtry = tuneArgsGrid$mtry[i],
                num.trees = tuneArgsGrid$nTrees[i],
                max.depth = tuneArgsGrid$maxDepth[i],
                probability = TRUE,
                importance = "impurity")

  oobError <- rf1$prediction.error
  
  modelResults <- tuneArgsGrid[i,]
  modelResults[,"oobError"] <- oobError
  modelResults
 
}

# Find the best model and train off of it
bestModel <- ranger(mainModel,
                    data = allTrainData,
                    mtry = results[which.min(results$oobError), "mtry"],
                    num.trees = results[which.min(results$oobError), "nTrees"],
                    max.depth = results[which.min(results$oobError), "maxDepth"],
                    num.threads = nCores,
                    probability = TRUE,
                    importance = "impurity")

return(list(bestModel, bestModel$variable.importance))
stopCluster(cl)
}
