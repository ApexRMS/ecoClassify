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
    proportionTraining = 0.8) {
  blockDim <- sqrt(nBlocks)
  if (blockDim != floor(blockDim)) {
    stop("`nBlocks` must be a perfect square, e.g.  100, 144, 256.")
  }
  if (proportionTraining <= 0 || proportionTraining >= 1) {
    stop("`proportionTraining` must be between 0 and 1 (exclusive).")
  }
  nTrainBlocks <- floor(nBlocks * proportionTraining)
  if (nTrainBlocks < 1) {
    stop(
      "With that `proportionTraining`, you end up with zero training blocks."
    )
  }

  ## Build spatial grid over first raster
  r0 <- trainingRasterList[[1]][[1]] # pick one layer just to get extent/CRS
  bbox_sf <- st_as_sfc(st_bbox(r0))
  grid <- st_make_grid(bbox_sf, n = c(blockDim, blockDim), square = TRUE)
  gridSf <- st_sf(block = seq_along(grid), geometry = grid)

  # Sample points & assign to blocks
  pts <- terra::spatSample(r0, size = nObs, as.points = TRUE, na.rm = TRUE)
  ptsSf <- st_as_sf(pts) %>% st_join(gridSf, join = st_intersects)
  ptsSf <- filter(ptsSf, !is.na(block))
  if (nrow(ptsSf) < nObs * 0.5) {
    updateRunLog(
      "Lost >50% of points to NA blocks; you may want a finer grid or more samples.",
      type = "warning"
    )
  }

  # Extract true presence/absence
  resp_df <- terra::extract(groundTruthRasterList[[1]], vect(ptsSf), df = TRUE)
  ptsSf$presence <- resp_df[[2]] # assume second column is your 0/1 band

  # # Stratified sampling by block & class
  # # roughly equal points per block, half presence/half absence
  # samplesPerBlock <- ceiling(nObs / nBlocks)
  # samplesPerClass <- ceiling(samplesPerBlock / 2)

  # balancedPts <- ptsSf %>%
  #   group_by(block, presence) %>%
  #   slice_sample(n = samplesPerClass, replace = TRUE) %>%
  #   ungroup()

  # # Split blocks into train vs test
  # trainBlocks <- sample(unique(balancedPts$block), nTrainBlocks)
  # trainPts <- filter(balancedPts, block %in% trainBlocks)
  # testPts <- filter(balancedPts, !block %in% trainBlocks)

  # split into train vs test
  trainPts <- ptsSf %>%
    group_by(presence) %>%
    slice_sample(prop = proportionTraining) %>%
    ungroup()

  testPts <- anti_join(ptsSf, st_drop_geometry(trainPts), by = colnames(trainPts)[!colnames(trainPts) %in% c("geometry")])

  # Extract predictors for each time step
  extract_at_pts <- function(rStack, pts) {
    terra::extract(rStack, vect(pts), df = TRUE)[, -1] # drop ID col
  }

  train_list <- lapply(trainingRasterList, extract_at_pts, pts = trainPts)
  test_list <- lapply(trainingRasterList, extract_at_pts, pts = testPts)

  # Combine & return
  train_df <- do.call(
    rbind,
    lapply(train_list, function(df) {
      cbind(df, presence = trainPts$presence)
    })
  )
  test_df <- do.call(
    rbind,
    lapply(test_list, function(df) {
      cbind(df, presence = testPts$presence)
    })
  )

  # Drop rows with NA in predictors or presence
  n_train_before <- nrow(train_df)
  train_df <- train_df[stats::complete.cases(train_df), ]
  n_train_dropped <- n_train_before - nrow(train_df)
  if (n_train_dropped > 0) {
    updateRunLog(sprintf("%d rows dropped from training data due to NA values.", n_train_dropped), type = "warning")
  }

  n_test_before <- nrow(test_df)
  test_df <- test_df[stats::complete.cases(test_df), ]
  n_test_dropped <- n_test_before - nrow(test_df)
  if (n_test_dropped > 0) {
    updateRunLog(sprintf("%d rows dropped from testing data due to NA values.", n_test_dropped), type = "warning")
  }

  list(train = train_df, test = test_df)
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
    modelType) {
  # define thresholds
  thresholds <- seq(0.01, 0.99, by = 0.01)

  # define testing observations (subtract 1 for factor level)
  testingObservations <- as.numeric(testingData$presence)

  # handle categorical variables by aligning factor levels
  if (modelType == "CNN") {
    if (!is.null(model$cat_vars) && length(model$cat_vars) > 0) {
      for (i in seq_along(model$cat_vars)) {
        col <- model$cat_vars[i]
        if (col %in% names(testingData)) {
          f <- factor(
            testingData[[col]],
            levels = seq_len(model$cat_levels[[i]])
          )
          x <- as.integer(f)
          x[is.na(x)] <- model$cat_levels[[i]] + 1 # assign 'unknown' category index
          testingData[[col]] <- x
        }
      }
    }
  } else if (modelType == "Random Forest") {
    rf_model <- model$model
    rf_levels <- model$factor_levels
    rf_vars <- names(rf_levels)
    if (is.null(rf_levels) || length(rf_levels) == 0) {
      warning(
        "Random Forest model does not include categorical level info. Skipping factor alignment."
      )
    } else {
      cat_vars <- names(testingData)[
        sapply(testingData, is.factor) & names(testingData) %in% rf_vars
      ]
      for (col in cat_vars) {
        levels_train <- rf_levels[[col]]
        if (!is.null(levels_train)) {
          f <- factor(as.character(testingData[[col]]), levels = levels_train)
          f[is.na(f)] <- "__unknown__"
          levels(f) <- c(levels_train, "__unknown__")
          testingData[[col]] <- f
        } else {
          warning(sprintf(
            "Skipping factor alignment for '%s': not found in trained RF model levels",
            col
          ))
          testingData[[col]] <- NA
        }
      }
    }
  } else if (modelType == "MaxEnt") {
    # Optional: warn if factor levels in testing data don't match
    cat_vars <- names(testingData)[sapply(testingData, is.factor)]
    for (col in cat_vars) {
      # Try aligning factor levels to training data if you have access
      # For now, we just drop NA-producing levels
      if (any(is.na(testingData[[col]]))) {
        updateRunLog(
          sprintf(
            "Column '%s' in testing data contains unknown levels. NA values will be introduced in prediction.",
            col
          ),
          type = "warning"
        )
      }
    }
  }

  ## predicting data
  if (modelType == "Random Forest") {
    testingPredictions <- predict(model$model, testingData)$predictions[, 2]
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
    modelType = "Random Forest") {

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
#' @param applyFiltering Logical; whether to apply spatial filtering to the presence raster.
#' @param predictedPresence A SpatRaster object representing predicted binary presence values.
#' @param filterResolution Size of the moving window used in filtering (numeric, e.g., 5 = 5x5).
#' @param filterPercent Proportion threshold of non-presence cells required to flip presence (numeric).
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
    applyFiltering,
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
    transferDir) {
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
    transferDir) {
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
