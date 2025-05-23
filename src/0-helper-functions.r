## -------------------------------
## ecoClassify - Helper Functions
## ApexRMS, November 2024
## -------------------------------

# suppress additional warnings -------------------------------------
quiet <- function(expr) {
  sink(tempfile())  # divert output to temporary file
  on.exit(sink())   # reset on exit
  invisible(capture.output(result <- tryCatch(expr, error = function(e) stop(e$message))))
  result
}

quiet({
  suppressPackageStartupMessages({
    library(terra)
    library(tidyverse)
    library(caret)
    library(magrittr)
    library(ENMeval)
    library(foreach)
    library(iterators)
    library(parallel)
  })

## load packages ---------------------------------------------------
update.packages(repos='http://cran.us.r-project.org', ask = FALSE, oldPkgs = c("terra"))

if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2", repos='http://cran.us.r-project.org')
}

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
})

## define functions ------------------------------------------------

#' Assign objects froma a datasheet of input variables ---
#'
#' @description
#' 'assignVariables' extracts variables from datashseets in the specified
#' syncrosim scenario and assigns them to an object.
#'
#' @param myScenario syncrosim scenario object
#' @param trainingRasterDataframe dataframe with input variables
#' @param column integer specifying the column to extract
#' @return list of objects (timesteps = numeric, nObs = numeric,
#' filterresolution = numeric, filterPercent = numeric,
#' applyFiltering = boolean, applyContextualization = boolean,
#' modelType = String ("Random Forest" or "MaxENt"), modelTuning = boolean)
#' that have been extracted from the syncrosim datasheet
#'
#' @details
#' This function is specifically designed for the the watchtower package
#' @noRd
assignVariables <- function(myScenario,
                            trainingRasterDataframe,
                            column) {

  # extract unique timesteps from trainingRasterDataframe --------------------------
  timestepList <- trainingRasterDataframe %>%
    filter(!is.na(column)) %>%
    pull(Timesteps) %>%
    unique()

  # Load classifier options datasheet
  classifierOptionsDataframe <- datasheet(myScenario,
                                          name = "ecoClassify_ClassifierOptions")

  # Extract model input values
  nObs <- classifierOptionsDataframe$nObs
  applyContextualization <- classifierOptionsDataframe$applyContextualization
  modelType <- as.character(classifierOptionsDataframe$modelType)
  modelTuning <- classifierOptionsDataframe$modelTuning
  setManualThreshold <- classifierOptionsDataframe$setManualThreshold
  manualThreshold <- classifierOptionsDataframe$manualThreshold
  normalizeRasters <- classifierOptionsDataframe$normalizeRasters

  # Load post-processing options datasheet
  postProcessingDataframe <- datasheet(myScenario,
                                       name = "ecoClassify_PostProcessingOptions")

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

  # stop if manual threshold is missing or outside of possible range
  if (setManualThreshold == TRUE) {
    if (is.null(manualThreshold) || length(manualThreshold) == 0 || is.na(manualThreshold)) {
      stop("Set probability threshold was selected but probability threshold value is missing")
    } else if (manualThreshold < 0 || manualThreshold > 1) {
      stop("Manual threshold outside of acceptable range; select a value between 0 and 1")
    }
  }
  # return as a list
  return(list(timestepList,
              nObs,
              filterResolution,
              filterPercent,
              applyFiltering,
              applyContextualization,
              modelType,
              modelTuning,
              setManualThreshold,
              manualThreshold,
              normalizeRasters))
}

#' Set the number of cores for multiprocessing ---
#'
#' @description
#' 'setCores' determines the number of cores to use for multiprocessing
#' based on the available cores and user settings.
#'
#' @param mulitprocessingSheet A dataframe containing multiprocessing settings.
#' It should have the columns 'EnableMultiprocessing' (logical) and
#' 'MaximumJobs' (integer).
#' @return The number of cores to use for multiprocessing (integer).
#'
#' @details
#' The function first detects the number of available cores on the system.
#' If multiprocessing is enabled in the 'mulitprocessingSheet', it checks if
#' the requested number of cores exceeds the available cores. If so, it sets
#' the number of cores to one less than the available cores and issues a warning.
#' Otherwise, it sets the number of cores to the requested number. If
#' multiprocessing is not enabled, it sets the number of cores to 1.
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

#' Extract rasters from filepaths in a dataframe ---
#'
#' @description
#' 'extractRasters' takes a dataframe of raster filepaths and creates
#' a list with one raster for each timestep
#'
#' @param dataframe column 1 = timestep, column 2 = filepath (dataframe)
#' @param column integer specifying the column to extract
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

    if (column == 3) {
      # remove duplicated layers
      subsetRaster <- subsetRaster[[1]]
    }

    # add to main raster list
    rasterList <- c(rasterList, subsetRaster)
  }

  return(rasterList)
}

#' Decompose rasters for image classification ---
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
                             method = "random")

  # extract values from the training and ground truth rasters
  randomPoints <- unique(randomPoints)
  responseValues <- terra::extract(responseRast, randomPoints)
  predValues <- terra::extract(predRast, randomPoints)

  # bind into single dataframe
  trainData <- cbind(predValues, response = responseValues[, 2])

  return(trainData)
}

#' Plot variable importance from random forest model ---
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

#' Split image data for training and testing ---
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

#' Predict presence over area ---
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
  predictedValues <- data.frame(predict(model, rasterMatrix))[, 2]

  # assing values where raster is not NA
  predictionRaster[!is.na(raster[[1]])] <- predictedValues

  return(predictionRaster)
}

#' Predict presence and generate probability values over area ---
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
    # generate probabilities for each raster using ranger
    probabilityRaster <- predictRanger(raster,
                                       model)
  } else if (modelType == "MaxEnt") {
    probabilityRaster <- predict(model, raster, type = "logistic")
  }  else {
    stop("Model type not recognized")
  }

  predictedPresence <- reclassifyRaster(probabilityRaster, threshold)

  return(list(predictedPresence,
              probabilityRaster))
}

reclassifyRaster <- function(raster, threshold) {

  raster[raster >= threshold] <- 1
  raster[raster < threshold] <- 0

  return(raster)
}

#' Filter prediction raster ---
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

#' Generate raster output dataframe ---
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

#' Generate RGB output dataframe ---
#'
#' @description
#' 'getRgbDataframe' generates output dataframe for RgbOutput syncrosim datasheet.
#'
#' @param rgbOutputDataframe RGB dataframe to be added to (dataframe)
#' @param category category for RGB image for file naming ("predicting" or "training" (character) 
#' @param timestep timestep value (integer)
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

#' Save raster and image files ---
#'
#' @description
#' 'saveFiles' saves raster and images files to transfer directory so they can be
#' referenced in the syncrosim datasheets.
#'
#' @param predictedPresence predicted presence raster (spatRaster)
#' @param groundTruth ground truth raster (spatRaster)
#' @param probabilityRaster probability raster (spatRaster)
#' @param trainingRasterList list of training rasters for generating RGB image ( list of spatRasters)
#' @param timestep timestep (integer)
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

#' Calculate statistics from random forest model predictions ---
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
    prediction <- predict(model, testData)$predictions[, 2] # TODO update for multiclass
  } else {
    prediction <- predict(model, testData, type = "logistic")
  }

  prediction <- as.factor(ifelse(prediction >= threshold, 1, 0))

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

#' Train a Maxent Model with Hyperparameter Tuning
#'
#' @description
#' This function trains a Maxent model using the `ENMevaluate` function with optional hyperparameter tuning.
#' It evaluates multiple hyperparameter combinations and selects the best model based on the Continuous Boyce Index (CBI).
#'
#' @param allTrainData A dataframe containing the training data. The dataframe should include a column named 'presence' indicating the target variable.
#' @param nCores An integer specifying the number of cores to use for parallel processing.
#' @param isTuningOn A logical value indicating whether hyperparameter tuning should be performed.
#' @return A list containing the best Maxent model and its variable importance.
#'
#' @details
#' The function first splits the training data into presence and absence data based on the 'presence' column.
#' If `isTuningOn` is TRUE, it evaluates multiple combinations of feature classes (`fc`) and regularization multipliers (`rm`).
#' The best model is selected based on the highest Continuous Boyce Index (CBI). If `isTuningOn` is FALSE, default hyperparameters are used.
#'
#'
#' @import ENMeval
getMaxentModel <- function(allTrainData, nCores, isTuningOn) {

  ## Specifying feature classes and regularization parameters for Maxent
  if (isTuningOn) {
    tuneArgs <- list(fc = c("L", "Q", "P", "LQ", "H", "LQH", "LQHP"),
                     rm = seq(0.5, 3, 0.5))
  } else {
    tuneArgs <- list(fc = c("LQH"),
                     rm = 1)
  }

  absenceTrainData <- allTrainData[allTrainData$presence == 0, grep("presence|kfold", colnames(allTrainData), invert=T)]
  presenceTrainData <- allTrainData[allTrainData$presence == 1, grep("presence|kfold", colnames(allTrainData), invert=T)]

  max1 <- ENMevaluate(occ = presenceTrainData,
                      bg.coords = absenceTrainData,
                      tune.args = tuneArgs,
                      progbar = F,
                      partitions = "randomkfold",
                      parallel = T,
                      numCores = nCores,
                      quiet = T, ## silence messages but not errors
                      algorithm = 'maxent.jar')

  bestMax <- which.max(max1@results$cbi.val.avg)
  varImp <- max1@variable.importance[bestMax] %>% data.frame()
  names(varImp) <- c("variable","percent.contribution", "permutation.importance")
  maxentImportance <- getMaxentImportance(varImp)

  model <- max1@models[[bestMax]]
  # modelOut <- max1@results[bestMax,]

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

  # define thresholds
  thresholds <- seq(0.01, 0.99, by = 0.01)

  # define testing observations (subtract 1 for factor level)
  testingObservations <- as.numeric(testingData$presence) - 1

  ## predicting data
  if (modelType == "Random Forest") {
    testingPredictions <- predict(model, testingData)$predictions[, 2] # TO DO: use value instead of index?
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

#' Train a Random Forest Model with Hyperparameter Tuning
#'
#' @description
#' This function trains a random forest model using the `ranger` package with optional hyperparameter tuning.
#' It evaluates multiple hyperparameter combinations in parallel and selects the best model based on the Out-of-Bag (OOB) error.
#'
#' @param allTrainData A dataframe containing the training data. The dataframe should include a column named 'presence' indicating the target variable.
#' @param nCores An integer specifying the number of cores to use for parallel processing.
#' @param isTuningOn A logical value indicating whether hyperparameter tuning should be performed.
#' @return A list containing the best random forest model and its variable importance.
#'
#' @details
#' The function first constructs a formula for the random forest model using all columns in `allTrainData` except 'presence' and 'kfold'.
#' If `isTuningOn` is TRUE, it evaluates multiple combinations of hyperparameters (`mtry`, `maxDepth`, and `nTrees`) in parallel using the `foreach` and `doParallel` packages.
#' The best model is selected based on the lowest OOB error. If `isTuningOn` is FALSE, default hyperparameters are used.
#'
#' @import ranger
#' @import foreach
#' @import doParallel
#' @export
getRandomForestModel <- function(allTrainData, nCores, isTuningOn) {
  trainingVariables <-  grep("presence|kfold", colnames(allTrainData), invert=T, value=T)

  mainModel <- formula(sprintf("%s ~ %s",
                               "presence",
                               paste(trainingVariables,
                                     collapse = " + ")))

  ## Specifying feature classes and regularization parameters for Maxent
  if (isTuningOn) {
    tuneArgs <- list(mtry = seq_len(min(6, length(trainingVariables))),  ## number of splits
                    maxDepth = seq(0, 1, 0.2), ## regulariation amount
                    nTrees = c(500, 1000, 2000)) ## number of trees
    tuneArgsGrid <- expand.grid(tuneArgs)
  } else {
    tuneArgs <- list(mtry = round(sqrt(length(trainingVariables)),0),  ## defaults
                    maxDepth = 0,
                    nTrees = 500)
    tuneArgsGrid <- expand.grid(tuneArgs)
  }
  
  registerDoParallel(cores=nCores) # TO DO: confirm this is a good solution
  
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

# function to reclassify ground truth rasters
reclassifyGroundTruth <- function(groundTruthRasterList) {

  reclassifiedGroundTruthList <- c()

  for (i in seq_along(groundTruthRasterList)) {
    groundTruthRaster <- groundTruthRasterList[[i]]
    groundTruthRaster[groundTruthRaster == min(values(groundTruthRaster), na.rm = TRUE)] <- 0
    groundTruthRaster[groundTruthRaster == max(values(groundTruthRaster), na.rm = TRUE)] <- 1

    reclassifiedGroundTruthList <- c(reclassifiedGroundTruthList, groundTruthRaster)

  }

  return(reclassifiedGroundTruthList)
}

# add covariate rasters to training data
addCovariates <- function(rasterList,
                          covariateDataframe) {

  # filter for NA values
  covariateDataframe <- covariateDataframe %>% filter(!is.na(covariateDataframe[, 1]))

  # list all covariate files
  covariateFiles <- as.vector(covariateDataframe[, 1])

  if (length(covariateFiles) > 0) {

    # read in covariate rasters
    covariateRaster <- rast(covariateFiles)

    # Merge each raster in covariateFiles with each raster in trainingRasterList
    for (i in seq_along(rasterList)) {
      rasterList[[i]] <- c(rasterList[[i]], covariateRaster)
    }
  }

  return(rasterList)
}

# normalize raster values between 0 and 1 ------------------------------
# Normalize bands ---
normalizeBand <- function(band) {
  min_val <- min(values(band), na.rm = TRUE)
  max_val <- max(values(band), na.rm = TRUE)
  (band - min_val) / (max_val - min_val)
}

# use normalizeBand function to normalize all bands in a raster --------
normalizeRaster <- function(rasterList) {

  # make an empty list for normalized rasters
  normalizedRasterList <- c()

  for (raster in rasterList) {
    # normalize bands for each raster in rasterList
    normalizedRaster <- lapply(1:nlyr(raster),
                               function(i) normalizeBand(raster[[i]])) %>% rast()
    
    # append to normalizedRasterList
    normalizedRasterList <- c(normalizedRasterList, normalizedRaster)
  }

  return(normalizedRasterList)
}
