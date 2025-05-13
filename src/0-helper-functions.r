## -------------------------------
## ecoClassify - Helper Functions
## ApexRMS, November 2024
## -------------------------------

## load packages ---------------------------------------------------
# suppress additional warnings ----
load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = 'http://cran.us.r-project.org')
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

quiet({
  pkgs <- c("terra", "tidyverse", "caret", "magrittr", "ENMeval", "foreach",
            "iterators", "parallel", "torch", "coro", "reshape2", "rsyncrosim",
            "sf", "ranger", "gtools", "codetools", "rJava", "ecospat", "cvms",
            "doParallel")

  invisible(lapply(pkgs, load_pkg))
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
assignVariables <- function(myScenario, trainingRasterDataframe, column) {
  # extract unique timesteps from trainingRasterDataframe --------------------------
  timestepList <- trainingRasterDataframe %>%
    filter(!is.na(column)) %>%
    pull(Timesteps) %>%
    unique()

  # Load classifier options datasheet
  classifierOptionsDataframe <- datasheet(
    myScenario,
    name = "ecoClassify_ClassifierOptions"
  )

  # Extract model input values
  nObs <- classifierOptionsDataframe$nObs
  applyContextualization <- classifierOptionsDataframe$applyContextualization
  contextualizationWindowSize <- classifierOptionsDataframe$contextualizationWindowSize
  modelType <- as.character(classifierOptionsDataframe$modelType)
  modelTuning <- classifierOptionsDataframe$modelTuning
  setManualThreshold <- classifierOptionsDataframe$setManualThreshold
  manualThreshold <- classifierOptionsDataframe$manualThreshold
  normalizeRasters <- classifierOptionsDataframe$normalizeRasters

  # assign value of 3 to contextualizationWindowSize if not specified
  if (is.null(contextualizationWindowSize) || isTRUE(is.na(contextualizationWindowSize))) {
    contextualizationWindowSize <- 3
  } else if (contextualizationWindowSize %% 2 == 0) {
    stop(
      "Contextualization window size must be an odd number; please specify a odd value greater than 1"
    )
  }

  # give a warning if contextualization window is specified but applyContextualization is FALSE
  if (contextualizationWindowSize > 0 && applyContextualization == FALSE) {
    warning(
      "Contextualization window size was supplied but applyContextualization is set to FALSE; no contextualization will be applied"
    )
  }

  # Load post-processing options datasheet
  postProcessingDataframe <- datasheet(
    myScenario,
    name = "ecoClassify_PostProcessingOptions"
  )

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
    if (
      is.null(manualThreshold) ||
        length(manualThreshold) == 0 ||
        is.na(manualThreshold)
    ) {
      stop(
        "Set probability threshold was selected but probability threshold value is missing"
      )
    } else if (manualThreshold < 0 || manualThreshold > 1) {
      stop(
        "Manual threshold outside of acceptable range; select a value between 0 and 1"
      )
    }
  }
  # return as a list
  return(list(
    timestepList,
    nObs,
    filterResolution,
    filterPercent,
    applyFiltering,
    applyContextualization,
    contextualizationWindowSize,
    modelType,
    modelTuning,
    setManualThreshold,
    manualThreshold,
    normalizeRasters
  ))
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
  if (mulitprocessingSheet$EnableMultiprocessing) {
    requestedCores <- mulitprocessingSheet$MaximumJobs
    if (requestedCores > availableCores) {
      warning(paste0(
        "Requested number of jobs exceeds available cores. Continuing run with ",
        availableCores,
        " jobs."
      ))
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
extractRasters <- function(dataframe, column) {
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
decomposedRaster <- function(predRast, responseRast, nobs) {
  # randomly sample points in the training raster
  randomPoints <- spatSample(
    responseRast,
    size = nobs,
    na.rm = TRUE,
    as.points = TRUE,
    replace = FALSE,
    method = "random"
  )

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
plotVariableImportance <- function(importanceData, transferDir) {
  if (is.null(names(importanceData))) {
    stop("`importanceData` must be a named numeric vector.")
  }
  df <- tibble::tibble(
    variable = names(importanceData),
    value = as.numeric(importanceData)
  )

  p <- ggplot2::ggplot(
    df,
    aes(
      x = reorder(variable, value),
      y = value,
      fill = value
    )
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Variable",
      y = "Variable Importance",
      title = "Information Value Summary"
    ) +
    ggplot2::theme_classic(base_size = 26) +
    ggplot2::scale_fill_gradientn(
      colours = "#424352",
      guide = "none"
    )

  outfile <- file.path(transferDir, "VariableImportance.png")
  ggplot2::ggsave(
    filename = outfile,
    plot = p,
    width = 7,
    height = 7,
    units = "in"
  )

  return(list(
    plot = p,
    dataFrame = data.frame(
      VariableImportance = outfile,
      stringsAsFactors = FALSE
    )
  ))
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
splitTrainTest <- function(trainingRasterList, groundTruthRasterList, nObs) {
  # create empty lists for binding data
  allTrainData <- c()
  allTestData <- c()

  # For loop through each raster pair
  for (i in seq_along(trainingRasterList)) {
    ## Decompose satellite image raster
    modelData <- decomposedRaster(
      trainingRasterList[[i]],
      groundTruthRasterList[[i]],
      nobs = nObs
    )

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

  return(list(allTrainData, allTestData))
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
predictRanger <- function(raster, model) {
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
#' @param model predictive model for generating probability values (random forest, maxent, or CNN object)
#' @param threshold threshold for converted results into binary outcomes (numeric)
#' @param modelType type of model used (character)
#' @return raster of predicted presence and probability values (spatRaster)
#'
#' @details
#'
#' @noRd
getPredictionRasters <- function(
  raster,
  model,
  threshold,
  modelType = "Random Forest"
) {
  # predict presence for each raster
  if (modelType == "Random Forest") {
    # generate probabilities for each raster using ranger
    probabilityRaster <- predictRanger(raster, model)
  } else if (modelType == "CNN") {
    probabilityRaster <- predictCNN(model, raster)
  } else if (modelType == "MaxEnt") {
    probabilityRaster <- predict(model, raster, type = "logistic")
  } else {
    stop("Model type not recognized")
  }

  predictedPresence <- reclassifyRaster(probabilityRaster, threshold)

  return(list(predictedPresence, probabilityRaster))
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
filterFun <- function(raster, resolution, percent) {
  # define parameters
  npixels <- resolution^2
  midPixel <- (npixels + 1) / 2
  threshold <- round(npixels * percent, 0)

  # filter
  if (is.na(raster[midPixel])) {
    return(NA)
  } else if (
    raster[midPixel] == 1 &&
      sum(raster[-midPixel] == 0, na.rm = TRUE) > threshold
  ) {
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
generateRasterDataframe <- function(
  applyFiltering,
  predictedPresence,
  filterResolution,
  filterPercent,
  category,
  timestep,
  transferDir,
  OutputDataframe,
  hasGroundTruth
) {
  if (hasGroundTruth == TRUE) {
    if (applyFiltering == TRUE) {
      # filter out presence pixels surrounded by non-presence
      filteredPredictedPresence <- focal(
        predictedPresence,
        w = matrix(1, 5, 5),
        fun = filterFun,
        resolution = filterResolution,
        percent = filterPercent
      )

      # save raster
      writeRaster(
        filteredPredictedPresence,
        filename = file.path(paste0(
          transferDir,
          "/filteredPredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        overwrite = TRUE
      )

      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        PredictedUnfiltered = file.path(paste0(
          transferDir,
          "/PredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        PredictedFiltered = file.path(paste0(
          transferDir,
          "/filteredPredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        GroundTruth = file.path(paste0(
          transferDir,
          "/GroundTruth-t",
          timestep,
          ".tif"
        )),
        Probability = file.path(paste0(
          transferDir,
          "/Probability-",
          category,
          "-t",
          timestep,
          ".tif"
        ))
      )
    } else {
      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        PredictedUnfiltered = file.path(paste0(
          transferDir,
          "/PredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        PredictedFiltered = "",
        GroundTruth = file.path(paste0(
          transferDir,
          "/GroundTruth-t",
          timestep,
          ".tif"
        )),
        Probability = file.path(paste0(
          transferDir,
          "/Probability-",
          category,
          "-t",
          timestep,
          ".tif"
        ))
      )
    }

    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe, rasterDataframe)
  } else {
    if (applyFiltering == TRUE) {
      # filter out presence pixels surrounded by non-presence
      filteredPredictedPresence <- focal(
        predictedPresence,
        w = matrix(1, 5, 5),
        fun = filterFun,
        resolution = filterResolution,
        percent = filterPercent
      )

      # save raster
      writeRaster(
        filteredPredictedPresence,
        filename = file.path(paste0(
          transferDir,
          "/filteredPredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        overwrite = TRUE
      )

      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        ClassifiedUnfiltered = file.path(paste0(
          transferDir,
          "/PredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        ClassifiedFiltered = file.path(paste0(
          transferDir,
          "/filteredPredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        ClassifiedProbability = file.path(paste0(
          transferDir,
          "/Probability-",
          category,
          "-t",
          timestep,
          ".tif"
        ))
      )
    } else {
      # build dataframe
      rasterDataframe <- data.frame(
        Timestep = timestep,
        ClassifiedUnfiltered = file.path(paste0(
          transferDir,
          "/PredictedPresence-",
          category,
          "-t",
          timestep,
          ".tif"
        )),
        ClassifiedFiltered = "",
        ClassifiedProbability = file.path(paste0(
          transferDir,
          "/Probability-",
          category,
          "-t",
          timestep,
          ".tif"
        ))
      )
    }

    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe, rasterDataframe)
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
getRgbDataframe <- function(
  rgbOutputDataframe,
  category,
  timestep,
  transferDir
) {
  rgbDataframe <- data.frame(
    Timestep = timestep,
    RGBImage = file.path(paste0(
      transferDir,
      "/RGBImage-",
      category,
      "-t",
      timestep,
      ".png"
    ))
  )

  rgbOutputDataframe <- addRow(rgbOutputDataframe, rgbDataframe)

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
saveFiles <- function(
  predictedPresence,
  groundTruth = NULL,
  probabilityRaster,
  trainingRasterList,
  category,
  timestep,
  transferDir
) {
  # save rasters
  writeRaster(
    predictedPresence,
    filename = file.path(paste0(
      transferDir,
      "/PredictedPresence-",
      category,
      "-t",
      timestep,
      ".tif"
    )),
    overwrite = TRUE
  )

  if (!is.null(groundTruth)) {
    writeRaster(
      groundTruth,
      filename = file.path(paste0(
        transferDir,
        "/GroundTruth-t",
        timestep,
        ".tif"
      )),
      overwrite = TRUE
    )
  }
  writeRaster(
    probabilityRaster,
    filename = file.path(paste0(
      transferDir,
      "/Probability-",
      category,
      "-t",
      timestep,
      ".tif"
    )),
    overwrite = TRUE
  )

  # save RBG Image
  png(
    file = file.path(paste0(
      transferDir,
      "/RGBImage-",
      category,
      "-t",
      timestep,
      ".png"
    ))
  )
  plotRGB(trainingRasterList[[t]], r = 3, g = 2, b = 1, stretch = "lin")
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
calculateStatistics <- function(
  model,
  testData,
  threshold,
  confusionOutputDataframe,
  modelOutputDataframe
) {
  if (inherits(model, "ranger")) {
    prediction <- predict(model, testData)$predictions[, 2]
  } else if (inherits(model, "torchCNN")) {
    prediction <- predictCNN(model, testData, isRaster = FALSE)
  } else {
    prediction <- predict(model, testData, type = "logistic")
  }

  prediction <- as.factor(ifelse(prediction >= threshold, 1, 0))

  confusionMatrix <- confusionMatrix(prediction, testData$presence)

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
    mutate(
      Statistic = case_when(
        Statistics == "AccuracyLower" ~ "Accuracy (lower)",
        Statistics == "AccuracyUpper" ~ "Accuracy (upper)",
        Statistics == "AccuracyNull" ~ "Accuracy (null)",
        Statistics == "AccuracyPValue" ~ "Accuracy P Value",
        Statistics == "McnemarPValue" ~ "Mcnemar P value",
        Statistics == "Neg Pred Value" ~ "Negative Predictive Value",
        Statistics == "Pos Pred Value" ~ "Positive Predictive Value",
        TRUE ~ Statistics
      )
    ) %>%
    select(-Statistics)

  # add confusion matrix and model statistics to dataframe
  confusionOutputDataframe <- addRow(confusionOutputDataframe, confusion_matrix)

  modelOutputDataframe <- addRow(modelOutputDataframe, model_stats)

  # make a confusion matrix plot
  confusionMatrixPlot <- plot_confusion_matrix(
    as_tibble(confusion_matrix),
    target_col = "Reference",
    prediction_col = "Prediction",
    counts_col = "Frequency",
    font_counts = font(size = 15),
    font_normalized = font(size = 6),
    font_row_percentages = font(size = 6),
    font_col_percentages = font(size = 6),
  ) +
    ggplot2::theme(
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 25)
    )

  return(list(
    confusionOutputDataframe,
    modelOutputDataframe,
    confusionMatrixPlot
  ))
}

getPCAFromRaster <- function(r, pcaSample = 100000) {
  if (nlyr(r) < 2) {
    stop("Need ≥2 layers for PCA")
  }
  # sample a data.frame of values
  sampleDF <- spatSample(
    r,
    size = min(pcaSample, ncell(r)),
    method = "random",
    na.rm = TRUE
  )
  # run PCA
  prcomp(sampleDF, scale. = TRUE)
}

# context functions ------------------------------
#’ Compute 8‐neighbour mean and first 2 PCA components for a multi‐layer image
#’
#’ @param rasterIn A SpatRaster with N predictor layers
#’ @param pcaSample Number of pixels to sample for building the PCA model
#’ @return A SpatRaster with N adjacent‐means plus 2 PC layers
addRasterAdjacencyValues <- function(
  rasterIn,
  adjacencyWindow = contextualizationWindowSize,
  pcaSample = 100000
) {
  # 1) 8‐neighbour mean
  w <- matrix(1, adjacencyWindow, adjacencyWindow)

  adj <- focal(
    rasterIn,
    w = w,
    fun = "mean",
    na.rm = TRUE,
    filename = "", # empty means process in blocks
    overwrite = TRUE
  )
  names(adj) <- paste0(names(rasterIn), "_adj")

  # 2) build PCA model on a random sample of pixels
  vals <- values(rasterIn, mat = TRUE)
  keep <- complete.cases(vals)
  samp <- sample(which(keep), min(pcaSample, sum(keep)))
  pcaMod <- prcomp(vals[samp, ])

  # 3) predict PC1 & PC2 back onto the full Raster in blocks
  pcs <- predict(
    rasterIn,
    model = pcaMod,
    index = 1:2,
    filename = "",
    overwrite = TRUE
  )
  names(pcs) <- c("PC1", "PC2")

  # 4) combine all layers
  rasterOut <- c(rasterIn, adj, pcs)
  return(rasterOut)
}

contextualizeRaster <- function(rasterList) {
  contextualizedRasterList <- c()

  for (r in seq_along(rasterList)) {
    raster <- rasterList[[r]]
    combinedRaster <- addRasterAdjacencyValues(raster)

    contextualizedRasterList <- c(contextualizedRasterList, combinedRaster)
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
    tuneArgs <- list(
      fc = c("L", "Q", "P", "LQ", "H", "LQH", "LQHP"),
      rm = seq(0.5, 3, 0.5)
    )
  } else {
    tuneArgs <- list(fc = c("LQH"), rm = 1)
  }

  absenceTrainData <- allTrainData[
    allTrainData$presence == 0,
    grep("presence|kfold", colnames(allTrainData), invert = T)
  ]
  presenceTrainData <- allTrainData[
    allTrainData$presence == 1,
    grep("presence|kfold", colnames(allTrainData), invert = T)
  ]

  max1 <- ENMevaluate(
    occ = presenceTrainData,
    bg.coords = absenceTrainData,
    tune.args = tuneArgs,
    progbar = F,
    partitions = "randomkfold",
    parallel = T,
    numCores = nCores,
    quiet = T, ## silence messages but not errors
    algorithm = 'maxent.jar'
  )

  bestMax <- which.max(max1@results$cbi.val.avg)
  varImp <- max1@variable.importance[bestMax] %>% data.frame()
  names(varImp) <- c(
    "variable",
    "percent.contribution",
    "permutation.importance"
  )
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
getOptimalThreshold <- function(
  model,
  testingData,
  modelType = "Random Forest"
) {
  # define thresholds
  thresholds <- seq(0.01, 0.99, by = 0.01)

  # define testing observations (subtract 1 for factor level)
  testingObservations <- as.numeric(testingData$presence) - 1

  ## predicting data
  if (modelType == "Random Forest") {
    testingPredictions <- predict(model, testingData)$predictions[, 2] # TO DO: use value instead of index?
  } else if (modelType == "MaxEnt") {
    testingPredictions <- predict(model, testingData, type = "logistic")
  } else if (modelType == "CNN") {
    testingPredictions <- predictCNN(model, testingData, isRaster = FALSE)
  } else {
    stop("Model type not recognized")
  }

  # Calculate sensitivity and specificity for each threshold
  metrics <- t(sapply(
    thresholds,
    getSensSpec,
    probs = testingPredictions,
    actual = testingObservations
  ))
  youdenIndex <- metrics[, 1] + metrics[, 2] - 1
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
  trainingVariables <- grep(
    "presence|kfold",
    colnames(allTrainData),
    invert = T,
    value = T
  )

  mainModel <- formula(sprintf(
    "%s ~ %s",
    "presence",
    paste(trainingVariables, collapse = " + ")
  ))

  ## Specifying feature classes and regularization parameters for Maxent
  if (isTuningOn) {
    tuneArgs <- list(
      mtry = seq_len(min(6, length(trainingVariables))), ## number of splits
      maxDepth = seq(0, 1, 0.2), ## regulariation amount
      nTrees = c(500, 1000, 2000)
    ) ## number of trees
    tuneArgsGrid <- expand.grid(tuneArgs)
  } else {
    tuneArgs <- list(
      mtry = round(sqrt(length(trainingVariables)), 0), ## defaults
      maxDepth = 0,
      nTrees = 500
    )
    tuneArgsGrid <- expand.grid(tuneArgs)
  }

  registerDoParallel(cores = nCores) # TO DO: confirm this is a good solution

  results <- foreach(
    i = seq_len(nrow(tuneArgsGrid)),
    .combine = rbind,
    .packages = "ranger"
  ) %dopar%
    {
      rf1 <- ranger(
        mainModel,
        data = allTrainData,
        mtry = tuneArgsGrid$mtry[i],
        num.trees = tuneArgsGrid$nTrees[i],
        max.depth = tuneArgsGrid$maxDepth[i],
        probability = TRUE,
        importance = "impurity"
      )

      oobError <- rf1$prediction.error

      modelResults <- tuneArgsGrid[i, ]
      modelResults[, "oobError"] <- oobError
      modelResults
    }

  # Find the best model and train off of it
  bestModel <- ranger(
    mainModel,
    data = allTrainData,
    mtry = results[which.min(results$oobError), "mtry"],
    num.trees = results[which.min(results$oobError), "nTrees"],
    max.depth = results[which.min(results$oobError), "maxDepth"],
    num.threads = 1,
    probability = TRUE,
    importance = "impurity"
  )

  return(list(bestModel, bestModel$variable.importance))
  stopCluster(cl)
}


getCNNModel <- function(allTrainData, nCores, isTuningOn) {
  torch_set_num_threads(nCores)

  # 1) pull out data
  X_df <- subset(allTrainData, select = -c(presence, kfold))
  y_raw <- allTrainData$presence
  y_int <- if (is.factor(y_raw)) as.integer(y_raw) - 1L else as.integer(y_raw)

  X <- as.matrix(X_df) # [n_samples, n_features]
  n_feat <- ncol(X)

  # 2) dataset + loader
  ds <- dataset(
    initialize = function(X, y) {
      self$X <- torch_tensor(X, dtype = torch_float())
      self$y <- torch_tensor(y + 1L, dtype = torch_long())
    },
    .getitem = function(i) list(x = self$X[i, ], y = self$y[i]),
    .length = function() self$X$size()[1]
  )(X, y_int)

  batch_size <- if (isTuningOn) 64 else 32
  epochs <- if (isTuningOn) 50 else 20
  dl <- dataloader(ds, batch_size = batch_size, shuffle = TRUE)

  # 3) define a 1×1 “CNN”
  net <- nn_module(
    "OneByOneCNN",
    initialize = function(n_feat) {
      # treat each predictor as its own channel, but length=1
      self$conv1 <- nn_conv1d(
        in_channels = n_feat,
        out_channels = 16,
        kernel_size = 1
      )
      self$fc1 <- nn_linear(16, 2)
    },
    forward = function(x) {
      x <- x$unsqueeze(3)
      x <- self$conv1(x)
      x <- nnf_relu(x)
      x <- x$squeeze(3)
      x <- self$fc1(x)
      x
    }
  )(n_feat)

  # 4) train it
  device <- torch_device("cpu")
  net <- net$to(device = device)
  opt <- optim_adam(net$parameters, lr = 1e-3)
  lossf <- nn_cross_entropy_loss()

  for (ep in seq_len(epochs)) {
    net$train()
    coro::loop(
      for (b in dl) {
        opt$zero_grad()
        x_b <- b$x$to(device = device)
        y_b <- b$y$to(device = device)

        logits <- net(x_b)
        loss <- lossf(logits, y_b)
        loss$backward()
        opt$step()
      }
    )
  }

  # 5) variable importance (abs sum of conv1 weights)
  w1 <- net$conv1$weight$data()$abs()$sum(dim = c(1, 3))$to(device = "cpu")
  vimp <- as.numeric(w1)
  names(vimp) <- colnames(X_df)

  class(net) <- c("torchCNN", class(net))
  list(net, vimp)
}

## Predict presence using a trained CNN model
#' @param model A trained CNN model (torchCNN object).
#' @param newdata A SpatRaster object containing the data to predict on.
#' @param ... Additional arguments (not used).
predictCNN <- function(model, newdata, isRaster = TRUE, ...) {
  # 1) pull out the raw feature matrix
  if (isRaster) {
    # get a numeric matrix ncell × nfeat
    vals <- terra::values(newdata, mat = TRUE)
  } else if (is.data.frame(newdata) || is.matrix(newdata)) {
    df <- as.data.frame(newdata, stringsAsFactors = FALSE)
    # drop any 'presence' or 'kfold' columns if they exist
    drop <- intersect(c("presence", "kfold"), names(df))
    if (length(drop)) df <- df[, setdiff(names(df), drop), drop = FALSE]
    # coerce everything to numeric
    vals <- as.matrix(df)
    storage.mode(vals) <- "double"
  } else {
    stop(
      "`newdata` must be a SpatRaster or a data.frame / matrix of predictors"
    )
  }

  ## check input layers
  conv1_wt <- model$conv1$weight$data()
  n_in <- conv1_wt$size()[2]
  if (ncol(vals) != n_in) {
    stop(sprintf(
      "Wrong number of predictors: model expects %d but you passed %d",
      n_in,
      ncol(vals)
    ))
  }

  X <- torch_tensor(vals, dtype = torch_float())
  dev <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")
  model <- model$to(device = dev)$eval()
  X <- X$to(device = dev)

  probs_t <- with_no_grad({
    logits <- model(X) # [N, 2]
    nnf_softmax(logits, dim = 2)$to(device = torch::torch_device("cpu"))
  })
  mat <- as.matrix(probs_t) # base R matrix
  colnames(mat) <- c("absent", "presence")

  if (isRaster) {
    outR <- newdata[[1]]
    terra::values(outR) <- mat[, "presence"]
    names(outR) <- "presence"
    return(outR)
  } else {
    return(mat[, "presence"])
  }
}


# function to reclassify ground truth rasters
reclassifyGroundTruth <- function(groundTruthRasterList) {
  reclassifiedGroundTruthList <- c()

  for (i in seq_along(groundTruthRasterList)) {
    groundTruthRaster <- groundTruthRasterList[[i]]
    groundTruthRaster[
      groundTruthRaster == min(values(groundTruthRaster), na.rm = TRUE)
    ] <- 0
    groundTruthRaster[
      groundTruthRaster == max(values(groundTruthRaster), na.rm = TRUE)
    ] <- 1

    reclassifiedGroundTruthList <- c(
      reclassifiedGroundTruthList,
      groundTruthRaster
    )
  }

  return(reclassifiedGroundTruthList)
}

# add covariate rasters to training data
addCovariates <- function(rasterList, covariateDataframe) {
  # filter for NA values
  covariateDataframe <- covariateDataframe %>%
    filter(!is.na(covariateDataframe[, 1]))

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
    normalizedRaster <- lapply(
      1:nlyr(raster),
      function(i) normalizeBand(raster[[i]])
    ) %>%
      rast()

    # append to normalizedRasterList
    normalizedRasterList <- c(normalizedRasterList, normalizedRaster)
  }

  return(normalizedRasterList)
}


# Save only the model’s state_dict (weights) as plain R arrays
saveTorchCNNasRDS <- function(model, path) {
  # 1a) pull out the state dict (a named list of torch_tensors)
  sd <- model$state_dict()
  # 1b) move everything to CPU and convert to plain R arrays
  sd_r <- lapply(sd, function(x) as.array(x$to(device = torch_device("cpu"))))
  # 1c) write to disk
  saveRDS(sd_r, path)
}


loadTorchCNNfromRDS <- function(path, n_feat, hidden_chs = 16, device = "cpu") {
  # 2a) read the plain‐R list of arrays
  sd_r <- readRDS(path)

  # 2b) rebuild your module skeleton
  model <- OneByOneCNN(n_feat = n_feat, hidden_chs = hidden_chs)

  # 2c) turn the arrays back into torch_tensors
  sd_t <- lapply(sd_r, function(a) torch_tensor(a, dtype = torch_float()))
  # ensure names line up
  names(sd_t) <- names(sd_r)

  # 2d) load into the model
  model$load_state_dict(sd_t)

  # 2e) move to desired device & eval mode
  dev <- if (device == "cpu") torch_device("cpu") else torch_device(device)
  model <- model$to(device = dev)$eval()

  return(model)
}
