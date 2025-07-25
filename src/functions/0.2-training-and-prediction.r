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
  proportionTraining = 0.8
) {
  blockDim <- sqrt(nBlocks)
  if (blockDim != floor(blockDim)) {
    stop("`nBlocks` must be a perfect square, e.g.  100, 144, 256.")
  }
  if (proportionTraining <= 0 || proportionTraining >= 1) {
    stop("`proportionTraining` must be between 0 and 1 (exclusive).")
  }

  # Get a reference raster (single layer from the first stack)
  r0 <- trainingRasterList[[1]][[1]]

  # Mask of valid (non-NA) cells
  valid_mask <- !is.na(r0)
  valid_cells <- which(values(valid_mask) == 1)

  if (length(valid_cells) < nObs) {
    stop("Not enough valid cells to sample the requested number of points.")
  }

  # Get raster dimensions
  dims <- dim(valid_mask)
  nrow_r <- dims[1]
  ncol_r <- dims[2]

  # Create row and column indices for each cell
  row_indices <- rep(1:nrow_r, times = ncol_r)
  col_indices <- rep(1:ncol_r, each = nrow_r)

  # Assign each cell to a grid block using equal-area binning
  row_bins <- cut(row_indices, breaks = blockDim, labels = FALSE)
  col_bins <- cut(col_indices, breaks = blockDim, labels = FALSE)
  block_ids <- (row_bins - 1) * blockDim + col_bins

  # Build block ID raster
  block_raster <- rast(r0)
  values(block_raster) <- NA
  values(block_raster)[valid_cells] <- block_ids[valid_cells]

  # Sample nObs points randomly from valid cells
  sampled_pts <- spatSample(r0, size = nObs, as.points = TRUE, na.rm = TRUE)
  sampled_sf <- st_as_sf(sampled_pts)

  # Extract block ID and presence values
  block_vals <- terra::extract(block_raster, vect(sampled_sf), df = TRUE)[, 2]
  presence_vals <- terra::extract(
    groundTruthRasterList[[1]],
    vect(sampled_sf),
    df = TRUE
  )[, 2]

  # Combine into sf object
  ptsSf <- sampled_sf
  ptsSf$block <- block_vals
  ptsSf$presence <- presence_vals
  ptsSf <- filter(ptsSf, !is.na(block) & !is.na(presence))

  if (nrow(ptsSf) < nObs * 0.5) {
    updateRunLog(
      "Lost >50% of points due to invalid (NA) values. Consider more samples or fewer blocks.",
      type = "warning"
    )
  }

  # Stratified sampling by presence
  trainPts <- ptsSf %>%
    group_by(presence) %>%
    slice_sample(prop = proportionTraining) %>%
    ungroup()

  testPts <- anti_join(
    ptsSf,
    st_drop_geometry(trainPts),
    by = colnames(trainPts)[!colnames(trainPts) %in% c("geometry")]
  )

  # Extract predictors at each time step
  extract_at_pts <- function(rStack, pts) {
    terra::extract(rStack, vect(pts), df = TRUE)[, -1]
  }

  train_list <- lapply(trainingRasterList, extract_at_pts, pts = trainPts)
  test_list <- lapply(trainingRasterList, extract_at_pts, pts = testPts)

  train_df <- do.call(
    rbind,
    lapply(train_list, function(df) cbind(df, presence = trainPts$presence))
  )
  test_df <- do.call(
    rbind,
    lapply(test_list, function(df) cbind(df, presence = testPts$presence))
  )

  # Drop NA rows
  n_train_before <- nrow(train_df)
  train_df <- train_df[complete.cases(train_df), ]
  if ((n_train_before - nrow(train_df)) > 0) {
    updateRunLog(
      sprintf(
        "%d rows dropped from training data due to NA values.",
        n_train_before - nrow(train_df)
      ),
      type = "warning"
    )
  }

  n_test_before <- nrow(test_df)
  test_df <- test_df[complete.cases(test_df), ]
  if ((n_test_before - nrow(test_df)) > 0) {
    updateRunLog(
      sprintf(
        "%d rows dropped from testing data due to NA values.",
        n_test_before - nrow(test_df)
      ),
      type = "warning"
    )
  }

  if (nrow(train_df) < 2) {
    stop(
      "Insufficient training data (< 2 rows). Check presence balance or NA filtering."
    )
  }
  if (length(unique(train_df$presence)) < 2) {
    stop(
      "Training data must include at least one presence (1) and one absence (0)."
    )
  }

  return(list(train = train_df, test = test_df))
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
  testingObservations <- as.numeric(testingData$presence)

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
