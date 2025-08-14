## -------------------------------
## ecoClassify - Training and Prediction Functions
## ApexRMS, November 2024
## -------------------------------

#' Split raster data into spatially stratified training and testing sets ----
#'
#' @description
#' `splitTrainTest` performs spatially stratified sampling of points from raster data,
#' assigning them to training and testing sets using a grid-based block design. It ensures
#' balanced sampling by block and class (presence/absence) and supports multi-temporal raster stacks.
#'
#' @param trainingRasterList A list of SpatRaster stacks representing training covariates for each timestep.
#' @param groundTruthRasterList A list of SpatRasters representing ground truth (presence/absence) for each timestep.
#' @param nObs Total number of points to sample across the spatial extent (integer).
#' @param nBlocks The number of spatial blocks to divide the area into (must be a perfect square, default = 25).
#' @param proportionTraining Proportion of blocks to assign to the training set (between 0 and 1, default = 0.8).
#'
#' @return A list containing two dataframes:
#' \describe{
#'   \item{train}{Dataframe with training predictors and `presence` column.}
#'   \item{test}{Dataframe with testing predictors and `presence` column.}
#' }
#'
#' @details
#' The spatial extent is divided into equal-sized blocks using `sf::st_make_grid`, then sampling
#' is performed to ensure spatial stratification and class balance. A warning is issued if >50%
#' of points fall outside valid blocks due to NA masking. The function then extracts predictor values
#' at sampled points for all timesteps, combines results across stacks, and returns cleaned datasets
#' with complete cases only.
#'
#' This function supports downstream model training and evaluation by promoting spatial independence
#' between training and test data and reducing spatial autocorrelation bias.
#'
#' @noRd
splitTrainTest <- function(
  trainingRasterList,
  groundTruthRasterList,
  nObs,
  nBlocks = 25,
  proportionTraining = 0.8,
  # Enhanced sampling parameters
  minMinorityProportion = 0.15, # Minimum proportion of minority class
  maxMinorityProportion = 0.35, # Maximum proportion of minority class
  edgeEnrichment = FALSE, # Sample more near class boundaries
  spatialBalance = TRUE # Use spatial blocks for train/test split
) {
  blockDim <- sqrt(nBlocks)
  if (blockDim != floor(blockDim)) {
    stop("`nBlocks` must be a perfect square, e.g. 100, 144, 256.")
  }
  if (proportionTraining <= 0 || proportionTraining >= 1) {
    stop("`proportionTraining` must be between 0 and 1 (exclusive).")
  }
  if (minMinorityProportion >= maxMinorityProportion) {
    stop("`minMinorityProportion` must be less than `maxMinorityProportion`.")
  }

  # Get reference raster and establish valid mask
  r0 <- trainingRasterList[[1]][[1]]
  for (r in trainingRasterList) {
    for (i in 1:nlyr(r)) {
      r0 <- mask(r0, r[[i]])
    }
  }

  validMask <- !is.na(r0)
  validCells <- which(values(validMask) == 1)

  if (length(validCells) < nObs) {
    stop("Not enough valid cells to sample the requested number of points.")
  }

  # Fast extraction of ground truth values
  gtValues <- values(groundTruthRasterList[[1]])[validCells]
  gtValues <- gtValues[!is.na(gtValues)]

  # Identify majority and minority classes
  classCounts <- table(gtValues)
  minorityClass <- as.numeric(names(classCounts)[which.min(classCounts)])
  majorityClass <- as.numeric(names(classCounts)[which.max(classCounts)])

  minorityCells <- validCells[which(
    values(groundTruthRasterList[[1]])[validCells] == minorityClass
  )]
  majorityCells <- validCells[which(
    values(groundTruthRasterList[[1]])[validCells] == majorityClass
  )]

  currentMinorityProp <- length(minorityCells) / length(validCells)

  updateRunLog(
    sprintf(
      "Natural class distribution: %.1f%% minority class (%d), %.1f%% majority class (%d)",
      currentMinorityProp * 100,
      minorityClass,
      (1 - currentMinorityProp) * 100,
      majorityClass
    ),
    type = "info"
  )

  # Determine target sampling proportions
  targetMinorityProp <- pmin(
    pmax(currentMinorityProp * 2.5, minMinorityProportion),
    maxMinorityProportion
  )
  targetMinorityN <- round(nObs * targetMinorityProp)
  targetMajorityN <- nObs - targetMinorityN

  updateRunLog(
    sprintf(
      "Target sampling: %d minority (%.1f%%), %d majority (%.1f%%)",
      targetMinorityN,
      targetMinorityProp * 100,
      targetMajorityN,
      (1 - targetMinorityProp) * 100
    ),
    type = "info"
  )

  # Fast edge detection if requested
  edgeWeights <- NULL
  if (
    edgeEnrichment && length(minorityCells) > 0 && length(majorityCells) > 0
  ) {
    # Simple 3x3 edge detection - much faster than focal()
    gtRaster <- rast(r0)
    values(gtRaster) <- 0 # Default to majority class
    values(gtRaster)[minorityCells] <- 1

    # Fast edge detection using adjacent cells
    dims <- dim(gtRaster)
    nrowR <- dims[1]
    ncolR <- dims[2]

    # Get row/col positions for valid cells
    validCoords <- rowColFromCell(gtRaster, validCells)

    # Simple edge detection: check if any of 4-connected neighbors differ
    edgeScores <- sapply(seq_along(validCells), function(i) {
      row <- validCoords[i, 1]
      col <- validCoords[i, 2]
      currentVal <- values(gtRaster)[validCells[i]]

      # Check 4-connected neighbors
      neighbors <- c()
      if (row > 1) {
        neighbors <- c(
          neighbors,
          values(gtRaster)[cellFromRowCol(gtRaster, row - 1, col)]
        )
      }
      if (row < nrowR) {
        neighbors <- c(
          neighbors,
          values(gtRaster)[cellFromRowCol(gtRaster, row + 1, col)]
        )
      }
      if (col > 1) {
        neighbors <- c(
          neighbors,
          values(gtRaster)[cellFromRowCol(gtRaster, row, col - 1)]
        )
      }
      if (col < ncolR) {
        neighbors <- c(
          neighbors,
          values(gtRaster)[cellFromRowCol(gtRaster, row, col + 1)]
        )
      }

      # Return 1 if any neighbor differs, 0 otherwise
      any(neighbors != currentVal, na.rm = TRUE)
    })

    edgeWeights <- 1 + edgeScores * 1.5 # Boost edge pixels by 1.5x
  }

  # Stratified sampling
  sampledMinorityIdx <- c()
  sampledMajorityIdx <- c()

  # Sample minority class
  if (targetMinorityN > 0 && length(minorityCells) > 0) {
    minorityIndices <- which(validCells %in% minorityCells)

    if (edgeEnrichment && !is.null(edgeWeights)) {
      minWeights <- edgeWeights[minorityIndices]
      minProbs <- minWeights / sum(minWeights)
    } else {
      minProbs <- NULL
    }

    sampledMinorityIdx <- sample(
      minorityIndices,
      size = min(targetMinorityN, length(minorityIndices)),
      prob = minProbs,
      replace = FALSE
    )
  }

  # Sample majority class
  if (targetMajorityN > 0 && length(majorityCells) > 0) {
    majorityIndices <- which(validCells %in% majorityCells)

    if (edgeEnrichment && !is.null(edgeWeights)) {
      majWeights <- edgeWeights[majorityIndices]
      majProbs <- majWeights / sum(majWeights)
    } else {
      majProbs <- NULL
    }

    sampledMajorityIdx <- sample(
      majorityIndices,
      size = min(targetMajorityN, length(majorityIndices)),
      prob = majProbs,
      replace = FALSE
    )
  }

  # Combine samples
  sampledIndices <- c(sampledMinorityIdx, sampledMajorityIdx)
  sampledCells <- validCells[sampledIndices]

  # Convert to spatial points - vectorized for speed
  sampledCoords <- xyFromCell(r0, sampledCells)
  sampledSf <- st_as_sf(
    data.frame(sampledCoords),
    coords = c("x", "y"),
    crs = crs(r0)
  )

  # Fast block assignment
  sampledRowcol <- rowColFromCell(r0, sampledCells)
  rowBins <- cut(sampledRowcol[, 1], breaks = blockDim, labels = FALSE)
  colBins <- cut(sampledRowcol[, 2], breaks = blockDim, labels = FALSE)
  blockIds <- (rowBins - 1) * blockDim + colBins

  # Create final dataset
  classValues <- c(
    rep(minorityClass, length(sampledMinorityIdx)),
    rep(majorityClass, length(sampledMajorityIdx))
  )

  ptsSf <- sampledSf
  ptsSf$block <- blockIds
  ptsSf$presence <- classValues
  ptsSf <- filter(ptsSf, !is.na(block))

  updateRunLog(
    sprintf(
      "Final sample: %d points (%.1f%% minority class)",
      nrow(ptsSf),
      mean(ptsSf$presence == minorityClass) * 100
    ),
    type = "info"
  )

  # Train/test split
  if (spatialBalance) {
    # Spatial block-based split
    uniqueBlocks <- unique(ptsSf$block)
    nTrainBlocks <- round(length(uniqueBlocks) * proportionTraining)
    trainBlocks <- sample(uniqueBlocks, nTrainBlocks)

    trainPts <- filter(ptsSf, block %in% trainBlocks)
    testPts <- filter(ptsSf, !block %in% trainBlocks)

    # Ensure both classes in train and test
    if (
      length(unique(trainPts$presence)) < 2 ||
        length(unique(testPts$presence)) < 2
    ) {
      updateRunLog(
        "Spatial split resulted in class imbalance, using random split",
        type = "warning"
      )
      trainIdx <- sample(
        seq_len(nrow(ptsSf)),
        size = round(nrow(ptsSf) * proportionTraining)
      )
      trainPts <- ptsSf[trainIdx, ]
      testPts <- ptsSf[-trainIdx, ]
    }
  } else {
    # Random split
    trainIdx <- sample(
      seq_len(nrow(ptsSf)),
      size = round(nrow(ptsSf) * proportionTraining)
    )
    trainPts <- ptsSf[trainIdx, ]
    testPts <- ptsSf[-trainIdx, ]
  }

  # Fast predictor extraction
  extractAtPts <- function(rStack, pts) {
    terra::extract(rStack, vect(pts), df = TRUE)[, -1]
  }

  trainList <- lapply(trainingRasterList, extractAtPts, pts = trainPts)
  testList <- lapply(trainingRasterList, extractAtPts, pts = testPts)

  trainDf <- do.call(
    rbind,
    lapply(trainList, function(df) {
      cbind(df, presence = trainPts$presence)
    })
  )
  testDf <- do.call(
    rbind,
    lapply(testList, function(df) {
      cbind(df, presence = testPts$presence)
    })
  )

  # Clean up NA values
  trainDf <- trainDf[complete.cases(trainDf), ]
  testDf <- testDf[complete.cases(testDf), ]

  # Final validation
  if (nrow(trainDf) < 2) {
    stop(
      "Insufficient training data (< 2 rows). Check class balance or NA filtering."
    )
  }
  if (length(unique(trainDf$presence)) < 2) {
    stop("Training data must include both classes.")
  }

  updateRunLog(
    sprintf(
      "Final datasets - Training: %d samples, Testing: %d samples",
      nrow(trainDf),
      nrow(testDf)
    ),
    type = "info"
  )

  return(list(
    train = trainDf,
    test = testDf,
    samplingInfo = list(
      minorityClass = minorityClass,
      majorityClass = majorityClass,
      targetMinorityProportion = targetMinorityProp,
      actualTrainMinorityProportion = mean(trainDf$presence == minorityClass),
      actualTestMinorityProportion = mean(testDf$presence == minorityClass)
    )
  ))
}

#' Calculate Sensitivity and Specificity ----
#'
#' @description
#' Calculates sensitivity and specificity based on predicted probabilities and actual labels.
#'
#' @param probs Numeric vector of predicted probabilities.
#' @param actual Numeric vector of actual class labels.
#' @param threshold Numeric threshold to convert probabilities to binary predictions.
#'
#' @return A numeric vector with sensitivity and specificity.
#'
#' @noRd
getSensSpec <- function(probs, actual, threshold) {
  predicted <- ifelse(probs >= threshold, 1, 0)

  evalData <- tibble(
    truth = factor(actual, levels = c(0, 1)),
    prediction = factor(predicted, levels = c(0, 1))
  )

  sens <- sensitivity(evalData, truth, prediction)$.estimate
  spec <- specificity(evalData, truth, prediction)$.estimate

  return(c(sens, spec))
}

#' Determine Optimal Classification Threshold ----
#'
#' @description
#' Selects a classification threshold by maximizing the Youden index.
#'
#' @param model The trained model object (CNN, RF, or MaxEnt).
#' @param testingData The data to make predictions on.
#' @param modelType Model type: "CNN", "Random Forest", or "MaxEnt".
#'
#' @return Optimal threshold value as a numeric.
#'
#' @noRd
getOptimalThreshold <- function(
  model,
  testingData,
  modelType
) {
  # define thresholds
  thresholds <- seq(0.01, 0.99, by = 0.01)

  # define testing observations (subtract 1 for factor level)
  testingObservations <- as.numeric(testingData$presence) - 1

  ## predicting data
  if (modelType == "Random Forest") {
    testingPredictions <- predict(model$model, testingData)$predictions[, "presence"]
  } else if (modelType == "MaxEnt") {
    testingPredictions <- predict(model$model, testingData, type = "logistic")
  } else if (modelType == "CNN") {
    testingPredictions <- predictCNN(model, testingData, isRaster = FALSE)
  } else {
    stop("Model type not recognized")
  }

  # remove NAs in predictions or observations
  valid_idx <- complete.cases(testingPredictions, testingObservations)
  testingPredictions <- testingPredictions[valid_idx]
  testingObservations <- testingObservations[valid_idx]

  if (length(testingPredictions) == 0) {
    stop(
      "All testing predictions were dropped due to NA — possibly from unseen factor levels."
    )
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

#' Generate binary presence and probability rasters from model predictions ----
#'
#' @description
#' `getPredictionRasters` applies a trained model to a multi-band raster input and returns
#' both a binary presence/absence raster and a continuous probability raster. It supports
#' multiple model types including Random Forest, MaxEnt, and CNN.
#'
#' @param raster A SpatRaster containing predictor layers to classify.
#' @param model A trained model object, such as the output from `getRandomForestModel()`,
#' `getMaxentModel()`, or `getCNNModel()`. The structure must include at least `$model`.
#' @param threshold A numeric value (0–1) used to convert predicted probabilities into binary
#' presence/absence classifications.
#' @param modelType A string specifying the type of model to apply: `"Random Forest"`,
#' `"MaxEnt"`, or `"CNN"`. Case sensitive.
#'
#' @return A list containing two SpatRaster objects:
#' \describe{
#'   \item{[[1]]}{Binary presence raster based on thresholded predictions.}
#'   \item{[[2]]}{Continuous probability raster from the model output.}
#' }
#'
#' @details
#' Internally, this function dispatches to the appropriate prediction method based on `modelType`:
#' \itemize{
#'   \item Random Forest: calls `predictRanger()` and handles factor alignment.
#'   \item MaxEnt: calls `predict()` from the `dismo` or `ENMeval` package.
#'   \item CNN: calls `predictCNN()` to run forward passes through a torch-based model.
#' }
#' The output probability raster is reclassified into binary form using the supplied threshold via `reclassifyRaster()`.
#' This function is commonly used when generating per-timestep prediction maps across a study area.
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
    probabilityRaster <- predict(model$model, raster, type = "logistic")
  } else {
    stop("Model type not recognized")
  }

  predictedPresence <- reclassifyRaster(probabilityRaster, threshold)

  return(list(predictedPresence, probabilityRaster))
}

#' Reclassify raster to binary presence/absence ----
#'
#' @description
#' 'reclassifyRaster' converts a continuous raster of probabilities into
#' a binary presence/absence raster based on a given threshold.
#'
#' @param raster Raster to reclassify (spatRaster).
#' @param threshold Threshold above which presence is assigned (numeric).
#' @return Binary raster (spatRaster).
#'
#' @details
#' Used to convert probability outputs from classifiers into discrete predictions.
#' @noRd
reclassifyRaster <- function(raster, threshold) {
  raster[raster >= threshold] <- 1
  raster[raster < threshold] <- 0

  return(raster)
}

#' Generate and document prediction raster outputs ----
#'
#' @description
#' `generateRasterDataframe` saves predicted presence and probability rasters to disk
#' and builds a structured dataframe row referencing these files for SyncroSim output
#' datasheets. Optionally applies spatial filtering to reduce spurious presence pixels
#' based on neighborhood context.
#'
#' @param predictedPresence A SpatRaster object representing predicted binary presence values.
#' @param category Category label used in file naming (e.g., "predicting" or "training").
#' @param timestep Integer representing the current model timestep.
#' @param transferDir File path to the directory where rasters will be saved.
#' @param OutputDataframe Existing dataframe to which output file references will be appended.
#' @param hasGroundTruth Logical; whether ground truth data is available for this timestep.
#'
#' @return A dataframe with paths to raster output files, including predicted presence
#' (filtered and unfiltered), probability raster, and optionally ground truth raster. The
#' structure of the dataframe differs depending on whether `hasGroundTruth` is `TRUE`.
#'
#' @details
#' If `applyFiltering` is `TRUE`, a focal filter is applied using `filterFun()` with a square
#' window (default 5x5) to suppress isolated presence pixels. Filtered rasters are saved with
#' `filteredPredictedPresence-*.tif` naming. If ground truth is available, the resulting
#' dataframe includes columns: `Timestep`, `PredictedUnfiltered`, `PredictedFiltered`,
#' `GroundTruth`, and `Probability`. If not, column names are `ClassifiedUnfiltered`,
#' `ClassifiedFiltered`, and `ClassifiedProbability` instead.
#'
#' Used when preparing outputs for the `ecoClassify_RasterOutput` SyncroSim datasheet.
#' @noRd
generateRasterDataframe <- function(
  predictedPresence,
  category,
  timestep,
  transferDir,
  OutputDataframe,
  hasGroundTruth
) {
  if (hasGroundTruth == TRUE) {
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
    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe, rasterDataframe)
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

    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe, rasterDataframe)
  }

  return(OutputDataframe)
}


#' Append RGB image path to output dataframe ---
#'
#' @description
#' `getRgbDataframe` creates a new row referencing an RGB image file for a given
#' timestep and appends it to the output dataframe used by the SyncroSim `RgbOutput`
#' datasheet.
#'
#' @param rgbOutputDataframe A dataframe used to collect output paths for RGB images
#' across timesteps.
#' @param category A string specifying the image category (e.g., `"predicting"` or `"training"`).
#' This is used in the image filename.
#' @param timestep Integer value indicating the timestep of the image.
#' @param transferDir Directory path where the image is stored.
#'
#' @return A dataframe with a new row containing the `Timestep` and corresponding
#' `RGBImage` file path.
#'
#' @details
#' This function assumes that the corresponding RGB image has already been saved
#' to the specified `transferDir` using a consistent filename format:
#' `"RGBImage-{category}-t{timestep}.png"`. It is used in workflows that require
#' visual inspection of RGB representations of predictor data at each timestep.
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

#' Save raster and RGB image files to disk ----
#'
#' @description
#' `saveFiles` writes out the predicted presence raster, probability raster,
#' optional ground truth raster, and a PNG RGB image to the specified
#' transfer directory. This prepares output artifacts for linkage with
#' SyncroSim datasheets or visual inspection.
#'
#' @param predictedPresence A SpatRaster representing binary presence/absence predictions.
#' @param groundTruth Optional SpatRaster containing ground truth presence values
#' (can be NULL if unavailable).
#' @param probabilityRaster A SpatRaster containing continuous probability predictions.
#' @param trainingRasterList A list of SpatRaster stacks used to generate the RGB image.
#' @param category A character string used to label file outputs (e.g., "training" or "predicting").
#' @param timestep Integer indicating the current timestep for file naming.
#' @param transferDir File path to the directory where outputs will be written.
#'
#' @return None. This function performs file I/O only.
#'
#' @details
#' The function saves three GeoTIFF rasters:
#' \itemize{
#'   \item `PredictedPresence-{category}-t{timestep}.tif`
#'   \item `Probability-{category}-t{timestep}.tif`
#'   \item `GroundTruth-t{timestep}.tif` (optional)
#' }
#' Additionally, a PNG RGB image is generated using bands 3 (R), 2 (G), and 1 (B)
#' from the training raster and saved as `RGBImage-{category}-t{timestep}.png`.
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
