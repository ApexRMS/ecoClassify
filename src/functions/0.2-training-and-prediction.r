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
  minMinorityProportion = 0.15,
  maxMinorityProportion = 0.35,
  edgeEnrichment = FALSE,
  spatialBalance = TRUE
) {
  # ---- validation ----
  blockDim <- sqrt(nBlocks)
  if (blockDim != floor(blockDim)) stop("`nBlocks` must be a perfect square, e.g. 25, 100, 144.")
  if (proportionTraining <= 0 || proportionTraining >= 1) stop("`proportionTraining` must be in (0,1).")
  if (minMinorityProportion >= maxMinorityProportion) stop("`minMinorityProportion` must be < `maxMinorityProportion`.")
  if (length(trainingRasterList) != length(groundTruthRasterList)) stop("trainingRasterList and groundTruthRasterList must have same length.")

  # helper: fast extraction by cell indexes (version-agnostic: drop ID if returned)
  extractAtCells <- function(rStack, cells) {
    xy <- terra::xyFromCell(rStack, cells)
    df <- terra::extract(rStack, xy)  # terra 1.8-29: no ID arg
    if ("ID" %in% names(df)) df <- df[, setdiff(names(df), "ID"), drop = FALSE]
    df
  }

  trainDfs <- vector("list", length(trainingRasterList))
  testDfs  <- vector("list", length(trainingRasterList))
  samplingInfoRows <- vector("list", length(trainingRasterList))

  # ---- loop over timesteps ----
  for (t in seq_along(trainingRasterList)) {
    r_pred <- trainingRasterList[[t]]
    r_gt   <- groundTruthRasterList[[t]]

    # 1) Build valid mask for THIS timestep: all predictor layers + GT non-NA
    r_all <- c(r_pred, r_gt)
    validMask <- terra::app(
      r_all,
      fun = function(x) as.integer(all(!is.na(x))),
      cores = 1  # rely on terraOptions() if set globally
    )
    validCells <- which(terra::values(validMask) == 1L)

    if (length(validCells) < 2) {
      stop(sprintf("Timestep %d has <2 valid cells after masking.", t))
    }

    # 2) Class info from THIS timestep (extract only needed cells)
    gt_vals_df <- terra::extract(r_gt, terra::xyFromCell(r_gt, validCells))
    if ("ID" %in% names(gt_vals_df)) gt_vals_df <- gt_vals_df[, setdiff(names(gt_vals_df), "ID"), drop = FALSE]
    gt_vals <- as.vector(gt_vals_df[[1]])
    gt_vals <- gt_vals[!is.na(gt_vals)]
    if (!length(gt_vals)) stop(sprintf("No valid ground truth after masking at timestep %d.", t))

    classCounts <- table(gt_vals)
    if (length(classCounts) < 2) {
      stop(sprintf("Timestep %d has only one class present in valid cells.", t))
    }
    minorityClass <- as.numeric(names(classCounts)[which.min(classCounts)])
    majorityClass <- as.numeric(names(classCounts)[which.max(classCounts)])

    # indexes within validCells
    gt_vals_full <- as.vector(terra::extract(r_gt, terra::xyFromCell(r_gt, validCells))[, setdiff(names(gt_vals_df), "ID"), drop = FALSE][[1]])
    minorityIdx_all <- which(gt_vals_full == minorityClass)
    majorityIdx_all <- which(gt_vals_full == majorityClass)

    currentMinorityProp <- length(minorityIdx_all) / length(validCells)

    updateRunLog(sprintf(
      "[t=%d] Natural class distribution: %.1f%% minority (%d), %.1f%% majority (%d)",
      t, 100*currentMinorityProp, minorityClass, 100*(1-currentMinorityProp), majorityClass
    ), type = "info")

    # 3) Target sampling proportions (bounded)
    targetMinorityProp <- pmin(pmax(currentMinorityProp * 2.5, minMinorityProportion), maxMinorityProportion)
    targetMinorityN    <- round(nObs * targetMinorityProp)

    # Edge weights (optional) from GT boundaries (extract only needed cells)
    minProbs <- majProbs <- NULL
    if (edgeEnrichment && length(minorityIdx_all) && length(majorityIdx_all)) {
      edges  <- terra::boundaries(r_gt, type = "outer")
      e_df   <- terra::extract(edges, terra::xyFromCell(edges, validCells))
      if ("ID" %in% names(e_df)) e_df <- e_df[, setdiff(names(e_df), "ID"), drop = FALSE]
      e_vals <- as.vector(e_df[[1]])
      w_all  <- ifelse(!is.na(e_vals) & e_vals == 1, 2.5, 1.0)
      if (length(minorityIdx_all)) {
        w_min    <- w_all[minorityIdx_all]
        minProbs <- w_min / sum(w_min)
      }
      if (length(majorityIdx_all)) {
        w_maj    <- w_all[majorityIdx_all]
        majProbs <- w_maj / sum(w_maj)
      }
    }

    # 4) Stratified sampling for THIS timestep
    sampledMinorityIdx <- integer(0)
    sampledMajorityIdx <- integer(0)

    if (targetMinorityN > 0 && length(minorityIdx_all) > 0) {
      sampledMinorityIdx <- sample(
        minorityIdx_all,
        size = min(targetMinorityN, length(minorityIdx_all)),
        replace = FALSE,
        prob = minProbs
      )
    }

    # Fill remainder with majority to reach nObs (as much as possible)
    targetMajorityN <- max(0, nObs - length(sampledMinorityIdx))
    if (targetMajorityN > 0 && length(majorityIdx_all) > 0) {
      sampledMajorityIdx <- sample(
        majorityIdx_all,
        size = min(targetMajorityN, length(majorityIdx_all)),
        replace = FALSE,
        prob = majProbs
      )
    }

    sampledIdx_within_valid <- c(sampledMinorityIdx, sampledMajorityIdx)
    if (length(sampledIdx_within_valid) < 2) {
      stop(sprintf("Timestep %d: insufficient sampled points (<2). Try lowering nObs or constraints.", t))
    }

    sampledCells <- validCells[sampledIdx_within_valid]

    # Labels for the sampled cells only
    sampledGT_df <- terra::extract(r_gt, terra::xyFromCell(r_gt, sampledCells))
    if ("ID" %in% names(sampledGT_df)) sampledGT_df <- sampledGT_df[, setdiff(names(sampledGT_df), "ID"), drop = FALSE]
    sampledGT <- as.vector(sampledGT_df[[1]])

    # 5) Assign spatial blocks (by row/col bins)
    rc <- terra::rowColFromCell(r_pred, sampledCells)
    rowBins <- cut(rc[,1], breaks = blockDim, labels = FALSE)
    colBins <- cut(rc[,2], breaks = blockDim, labels = FALSE)
    blockIds <- (rowBins - 1L) * blockDim + colBins

    pts <- data.frame(
      cell = sampledCells,
      block = as.integer(blockIds),
      presence = as.integer(sampledGT),
      stringsAsFactors = FALSE
    )
    pts <- pts[!is.na(pts$block), , drop = FALSE]

    updateRunLog(sprintf(
      "[t=%d] Final sample: %d pts (%.1f%% minority).",
      t, nrow(pts), 100*mean(pts$presence == minorityClass)
    ), type = "info")

    # 6) Train/test split (spatial blocks or random) for THIS timestep
    if (spatialBalance) {
      uniqueBlocks   <- unique(pts$block)
      nTrainBlocks   <- round(length(uniqueBlocks) * proportionTraining)
      trainBlocks    <- sample(uniqueBlocks, nTrainBlocks)
      trainPts <- pts[pts$block %in% trainBlocks, , drop = FALSE]
      testPts  <- pts[!(pts$block %in% trainBlocks), , drop = FALSE]

      # ensure both classes in train & test; otherwise fallback to random split
      if (length(unique(trainPts$presence)) < 2 || length(unique(testPts$presence)) < 2) {
        updateRunLog(sprintf("[t=%d] Spatial split class-imbalanced; using random split.", t), type = "warning")
        tr_idx  <- sample(seq_len(nrow(pts)), size = round(nrow(pts) * proportionTraining))
        trainPts <- pts[tr_idx, , drop = FALSE]
        testPts  <- pts[-tr_idx, , drop = FALSE]
      }
    } else {
      tr_idx  <- sample(seq_len(nrow(pts)), size = round(nrow(pts) * proportionTraining))
      trainPts <- pts[tr_idx, , drop = FALSE]
      testPts  <- pts[-tr_idx, , drop = FALSE]
    }

    # 7) Extract predictors by CELL INDEX (fast) for THIS timestep
    trainX <- extractAtCells(r_pred, trainPts$cell)
    testX  <- extractAtCells(r_pred, testPts$cell)

    trainDf_t <- cbind(trainX, presence = trainPts$presence)
    testDf_t  <- cbind(testX,  presence = testPts$presence)

    # Clean NAs
    trainDf_t <- trainDf_t[complete.cases(trainDf_t), , drop = FALSE]
    testDf_t  <- testDf_t[complete.cases(testDf_t),  , drop = FALSE]

    if (nrow(trainDf_t) < 2 || length(unique(trainDf_t$presence)) < 2) {
      stop(sprintf("Timestep %d: insufficient/bad training data after NA filtering.", t))
    }

    trainDfs[[t]] <- trainDf_t
    testDfs[[t]]  <- testDf_t

    samplingInfoRows[[t]] <- data.frame(
      timestep = t,
      minorityClass = minorityClass,
      majorityClass = majorityClass,
      targetMinorityProportion = targetMinorityProp,
      actualTrainMinorityProportion = mean(trainDf_t$presence == minorityClass),
      actualTestMinorityProportion  = mean(testDf_t$presence == minorityClass),
      nTrain = nrow(trainDf_t),
      nTest  = nrow(testDf_t)
    )
  }

  # ---- combine across timesteps ----
  trainDf <- do.call(rbind, trainDfs)
  testDf  <- do.call(rbind, testDfs)
  samplingInfo <- do.call(rbind, samplingInfoRows)

  updateRunLog(sprintf(
    "Final datasets (all timesteps) — Training: %d, Testing: %d",
    nrow(trainDf), nrow(testDf)
  ), type = "info")

  # final validation
  if (nrow(trainDf) < 2) stop("Insufficient training data (<2 rows) overall.")
  if (length(unique(trainDf$presence)) < 2) stop("Training data must include both classes overall.")

  return(list(
    train = trainDf,
    test  = testDf,
    samplingInfo = samplingInfo
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

  if (modelType == "Random Forest") {
    testingObservations <- as.numeric(testingData$presence) - 1
    testingPredictions <- predict(model$model, testingData)$predictions[, "presence"]
  } else if (modelType == "MaxEnt") {
    testingObservations <- as.numeric(testingData$presence)
    testingPredictions <- predict(model$model, testingData, type = "logistic")
  } else if (modelType == "CNN") {
    testingObservations <- as.numeric(testingData$presence)
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
