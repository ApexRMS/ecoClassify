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

quiet <- function(expr) {
  suppressPackageStartupMessages(expr)
}

quiet({
  pkgs <- c(
    "terra",
    "tidyverse",
    "magrittr",
    "ENMeval",
    "foreach",
    "iterators",
    "parallel",
    "coro",
    "rsyncrosim",
    "sf",
    "ranger",
    "gtools",
    "codetools",
    "rJava",
    "ecospat",
    "cvms",
    "doParallel",
    "tidymodels"
  )

  invisible(lapply(pkgs, load_pkg))
})

install_and_load_torch <- function(max_attempts = 3, wait_seconds = 5) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    install.packages("torch", repos = 'http://cran.us.r-project.org')
  }

  Sys.setenv(TORCH_INSTALL = "1")

  if (!torch::torch_is_installed()) {
    torch::install_torch()
  }

  # Try loading backend up to `max_attempts` times
  attempt <- 1
  while (attempt <= max_attempts) {
    try(
      {
        suppressPackageStartupMessages(library(torch))
        torch::torch_tensor(1) # forces backend to load
        return(invisible(TRUE))
      },
      silent = TRUE
    )

    updateRunLog(
      sprintf(
        "Attempt %d failed to load torch backend. Retrying in %d seconds...",
        attempt,
        wait_seconds
      ),
      type = "info"
    )
    Sys.sleep(wait_seconds)
    attempt <- attempt + 1
  }

  updateRunLog("Torch backend still not ready. Reinstalling...", type = "info")
  unlink(
    get("torch_home", envir = asNamespace("torch"))(),
    recursive = TRUE,
    force = TRUE
  )
  torch::install_torch()
  suppressPackageStartupMessages(library(torch))
  torch::torch_tensor(1)
}

install_and_load_torch()

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
  rasterDecimalPlaces <- classifierOptionsDataframe$rasterDecimalPlaces

  # assign value of 3 to contextualizationWindowSize if not specified
  if (applyContextualization == TRUE) {
    if (
      is.null(contextualizationWindowSize) ||
        isTRUE(is.na(contextualizationWindowSize))
    ) {
      contextualizationWindowSize <- 3
      updateRunLog(
        "Contextualization window size was not supplied; using default value of 3",
        type = "info"
      )
    } else if (contextualizationWindowSize %% 2 == 0) {
      stop(
        "Contextualization window size must be an odd number; please specify a odd value greater than 1"
      )
    } else if (contextualizationWindowSize == 1) {
      stop(
        "Contextualization window size must be greater than 1 for contextualization to be applied"
      )
    }
  }

  # give a warning if contextualization window is specified but applyContextualization is FALSE
  if (
    !is.null(contextualizationWindowSize) && applyContextualization == FALSE
  ) {
    updateRunLog(
      "Contextualization window size was supplied but applyContextualization is set to FALSE; no contextualization will be applied",
      type = "info"
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
    updateRunLog(
      "Filter resolution was not supplied; using default value of 5",
      type = "info"
    )
  }

  if (is.na(filterPercent) && applyFiltering == TRUE) {
    filterPercent <- 0.25
    updateRunLog(
      "Filter percent was not supplied; using default value of 0.25",
      type = "info"
    )
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
    normalizeRasters,
    rasterDecimalPlaces
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
      updateRunLog(
        paste0(
          "Requested number of jobs exceeds available cores. Continuing run with ",
          availableCores,
          " jobs."
        ),
        type = "warning"
      )
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

  n_vars <- nrow(df)

  # Adjust font size and image height based on number of variables
  font_size <- if (n_vars <= 10) 20 else if (n_vars <= 20) 18 else if (
    n_vars <= 40
  )
    16 else 14
  plot_height <- max(4, min(10, 0.3 * n_vars)) # height scales with variable count, capped at 10 in

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
    ggplot2::theme_classic(base_size = font_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
      ),
      plot.title.position = "plot"
    ) +
    ggplot2::scale_fill_gradientn(
      colours = "#424352",
      guide = "none"
    )

  outfile <- file.path(transferDir, "VariableImportance.png")
  ggplot2::ggsave(
    filename = outfile,
    plot = p,
    width = 7,
    height = plot_height,
    units = "in",
    dpi = 300
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

splitTrainTest <- function(
  trainingRasterList, # list of SpatRaster stacks (one per time step)
  groundTruthRasterList, # same structure, but single-band (0/1) rasters
  nObs, # total number of pixels to sample
  nBlocks = 25, # must be a perfect square
  proportionTraining = 0.8 # fraction of blocks to use for training
) {
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

  ## Build spatial grid over first raster -------------------------------
  r0 <- trainingRasterList[[1]][[1]] # pick one layer just to get extent/CRS
  bbox_sf <- st_as_sfc(st_bbox(r0))
  grid <- st_make_grid(bbox_sf, n = c(blockDim, blockDim), square = TRUE)
  gridSf <- st_sf(block = seq_along(grid), geometry = grid)

  # Sample points & assign to blocks -----------------------------------
  pts <- terra::spatSample(r0, size = nObs, as.points = TRUE)
  ptsSf <- st_as_sf(pts) %>% st_join(gridSf, join = st_within)
  ptsSf <- filter(ptsSf, !is.na(block))
  if (nrow(ptsSf) < nObs * 0.5) {
    warning(
      "Lost >50% of points to NA blocks; you may want a finer grid or more samples."
    )
  }

  # 4. Extract true presence/absence --------------------------------------
  resp_df <- terra::extract(groundTruthRasterList[[1]], vect(ptsSf), df = TRUE)
  ptsSf$presence <- resp_df[[2]] # assume second column is your 0/1 band

  # 5. Stratified sampling by block & class -------------------------------
  # roughly equal points per block, half presence/half absence
  samplesPerBlock <- ceiling(nObs / nBlocks)
  samplesPerClass <- ceiling(samplesPerBlock / 2)

  balancedPts <- ptsSf %>%
    group_by(block, presence) %>%
    slice_sample(n = samplesPerClass, replace = TRUE) %>%
    ungroup()

  # Split blocks into train vs test
  trainBlocks <- sample(unique(balancedPts$block), nTrainBlocks)
  trainPts <- filter(balancedPts, block %in% trainBlocks)
  testPts <- filter(balancedPts, !block %in% trainBlocks)

  # 7. Extract predictors for each time step ------------------------------
  extract_at_pts <- function(rStack, pts) {
    terra::extract(rStack, vect(pts), df = TRUE)[, -1] # drop ID col
  }

  train_list <- lapply(trainingRasterList, extract_at_pts, pts = trainPts)
  test_list <- lapply(trainingRasterList, extract_at_pts, pts = testPts)

  # 8. Combine & return ----------------------------------------------------
  train_df <- do.call(
    rbind,
    lapply(train_list, function(df) {
      cbind(df, presence = trainPts$presence)
    })
  )
  train_df <- train_df[!is.na(train_df$presence), ]
  test_df <- do.call(
    rbind,
    lapply(test_list, function(df) {
      cbind(df, presence = testPts$presence)
    })
  )
  test_df <- test_df[!is.na(test_df$presence), ]

  list(train = train_df, test = test_df)
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
  rasterMatrix <- terra::values(raster, mat = TRUE)
  valid_idx <- complete.cases(rasterMatrix)
  rasterMatrix <- rasterMatrix[valid_idx, ]
  predictedValues <- data.frame(predict(model, rasterMatrix))[, 2]

  # assing values where raster is not NA
  predictionRaster[valid_idx] <- predictedValues

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
  confusionOutputDataFrame,
  modelOutputDataFrame
) {
  # Predict probabilities
  if (inherits(model[[1]], "ranger")) {
    probs <- predict(model[[1]], testData)$predictions[, 2]
  } else if (inherits(model[[1]], "torchCNN")) {
    probs <- predictCNN(model, testData, isRaster = FALSE)
  } else {
    probs <- predict(model[[1]], testData, type = "logistic")
  }

  # Binary classification based on threshold
  predicted <- ifelse(probs >= threshold, 1, 0)

  # Prepare evaluation data
  evalData <- tibble(
    truth = factor(testData$presence, levels = c(0, 1)),
    prediction = factor(predicted, levels = c(0, 1))
  )

  # Confusion matrix
  confMatrix <- conf_mat(evalData, truth = truth, estimate = prediction)

  # Format confusion matrix table
  confusionMatrix <- as.data.frame(confMatrix$table)
  colnames(confusionMatrix) <- c("Prediction", "Reference", "Frequency")

  # Collect metrics
  metricsData <- bind_rows(
    accuracy(evalData, truth, prediction),
    sens = sensitivity(evalData, truth, prediction),
    spec = specificity(evalData, truth, prediction),
    precision(evalData, truth, prediction),
    recall(evalData, truth, prediction),
    f_meas(evalData, truth, prediction)
  ) %>%
    select(.metric, .estimate) %>%
    rename(Statistic = .metric, Value = .estimate)

  # Append to output dataframes
  confusionOutputDataFrame <- addRow(confusionOutputDataFrame, confusionMatrix)
  modelOutputDataFrame <- addRow(modelOutputDataFrame, metricsData)

  # Confusion matrix plot
  confusionMatrixPlot <- plot_confusion_matrix(
    confusionMatrix,
    target_col = "Reference",
    prediction_col = "Prediction",
    counts_col = "Frequency",
    font_counts = font(size = 15),
    font_normalized = font(size = 6),
    font_row_percentages = font(size = 6),
    font_col_percentages = font(size = 6)
  ) +
    ggplot2::theme(
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 25)
    )

  return(list(
    confusionOutputDataFrame,
    modelOutputDataFrame,
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
  pcaSample = 100000,
  nBins = 16
) {
  terraOptions(threads = 7)
  # helper to compute local range
  rangeFun <- function(vals, na.rm = TRUE) {
    if (na.rm) vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NA_real_)
    max(vals) - min(vals)
  }

  # helper to compute local entropy
  entropyFun <- function(vals, na.rm = TRUE) {
    if (na.rm) vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NA_real_)
    h <- hist(vals, breaks = nBins, plot = FALSE)
    p <- h$counts / sum(h$counts)
    p <- p[p > 0]
    -sum(p * log2(p))
  }

  # build moving-window weight matrix
  w <- matrix(1, adjacencyWindow, adjacencyWindow)

  # compute PCA on a random sample of pixels
  vals <- values(rasterIn, mat = TRUE)
  keep <- complete.cases(vals)
  samp <- sample(which(keep), min(pcaSample, sum(keep)))
  pcaMod <- prcomp(vals[samp, ], scale. = TRUE)

  pcs <- predict(
    rasterIn,
    model = pcaMod,
    index = 1:2,
    filename = "",
    overwrite = TRUE
  )
  names(pcs) <- c("PC1", "PC2")

  # compute local mean
  adjMean <- focal(
    rasterIn,
    w = w,
    fun = "mean",
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjMean) <- paste0(names(rasterIn), "_mean")

  # compute local sd
  adjSd <- focal(
    rasterIn,
    w = w,
    fun = "sd",
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjSd) <- paste0(names(rasterIn), "_sd")

  # compute local range
  adjRange <- focal(
    rasterIn,
    w = w,
    fun = rangeFun,
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjRange) <- paste0(names(rasterIn), "_range")

  # compute local entropy
  adjEntropy <- focal(
    rasterIn,
    w = w,
    fun = entropyFun,
    na.rm = TRUE,
    filename = "",
    overwrite = TRUE
  )
  names(adjEntropy) <- paste0(names(rasterIn), "_entropy")

  # stack everything: original → mean → sd → range → entropy → PCs
  rasterOut <- c(rasterIn, adjMean, adjSd, adjRange, adjEntropy, pcs)

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
  # Identify predictor variables
  predictorVars <- grep("presence|kfold", colnames(allTrainData), invert = TRUE, value = TRUE)

  # Split into categorical and numeric
  predictorData <- allTrainData[, predictorVars, drop = FALSE]
  cat_vars <- names(predictorData)[sapply(predictorData, is.factor)]
  num_vars <- setdiff(predictorVars, cat_vars)

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
    predictorVars
  ]
  presenceTrainData <- allTrainData[
    allTrainData$presence == 1,
    predictorVars
  ]

  max1 <- ENMevaluate(
    occ = presenceTrainData,
    bg.coords = absenceTrainData,
    tune.args = tuneArgs,
    progbar = FALSE,
    partitions = "randomkfold",
    parallel = TRUE,
    numCores = nCores,
    quiet = TRUE,
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

  return(list(
    model = model,
    vimp = maxentImportance,
    cat_vars = cat_vars,
    num_vars = num_vars
  ))
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

  evalData <- tibble(
    truth = factor(actual, levels = c(0, 1)),
    prediction = factor(predicted, levels = c(0, 1))
  )

  sens <- sensitivity(evalData, truth, prediction)$.estimate
  spec <- specificity(evalData, truth, prediction)$.estimate

  return(c(sens, spec))
}


getOptimalThreshold <- function(
  model,
  testingData,
  modelType
) {
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
    maxent_model <- model[[1]]
    cat_vars <- names(testingData)[sapply(testingData, is.factor)]
    for (col in cat_vars) {
      if (col %in% names(maxent_model@data@presence)) {
        levels_train <- levels(maxent_model@data@presence[[col]])
        if (!is.null(levels_train)) {
          f <- factor(testingData[[col]], levels = levels_train)
          f[is.na(f)] <- "__unknown__"
          levels(f) <- c(levels_train, "__unknown__")
          testingData[[col]] <- f
        }
      } else {
        warning(sprintf(
          "Skipping factor alignment for '%s': not found in trained MaxEnt model data",
          col
        ))
        testingData[[col]] <- NA
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
    invert = TRUE,
    value = TRUE
  )

  # Identify categorical and numeric variables
  cat_vars <- names(allTrainData[, trainingVariables, drop = FALSE])[sapply(allTrainData[, trainingVariables, drop = FALSE], is.factor)]
  num_vars <- setdiff(trainingVariables, cat_vars)

  mainModel <- formula(sprintf(
    "%s ~ %s",
    "presence",
    paste(trainingVariables, collapse = " + ")
  ))

  if (isTuningOn) {
    tuneArgs <- list(
      mtry = seq_len(min(6, length(trainingVariables))),
      maxDepth = seq(0, 1, 0.2),
      nTrees = c(1000, 2000, 3000, 4000, 5000)
    )
    tuneArgsGrid <- expand.grid(tuneArgs)
  } else {
    tuneArgs <- list(
      mtry = round(sqrt(length(trainingVariables)), 0),
      maxDepth = 0,
      nTrees = 2000
    )
    tuneArgsGrid <- expand.grid(tuneArgs)
  }

  registerDoParallel(cores = nCores)

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

  factor_levels <- lapply(
    allTrainData[, trainingVariables, drop = FALSE],
    function(x) {
      if (is.factor(x)) levels(x) else NULL
    }
  )

  return(list(
    model = bestModel,
    vimp = bestModel$variable.importance,
    factor_levels = factor_levels,
    cat_vars = cat_vars,
    num_vars = num_vars
  ))
}

# Updated getCNNModel function to use embeddings for specified categorical variables, with dynamic embedding dimension
getCNNModel <- function(allTrainData, nCores, isTuningOn) {
  torch_set_num_threads(nCores)

  # Identify categorical and numeric variables
  predictors <- allTrainData[, grep(
    "presence|kfold",
    colnames(allTrainData),
    invert = T
  )]
  cat_vars <- names(predictors)[sapply(predictors, is.factor)]
  num_vars <- setdiff(names(predictors), cat_vars)

  # Map factor levels to integers
  cat_indices <- lapply(predictors[cat_vars], function(x) as.integer(x))
  cat_levels <- if (length(cat_vars))
    lapply(predictors[cat_vars], function(x) length(levels(x))) else list()
  embedding_dims <- if (length(cat_levels))
    lapply(cat_levels, function(l) min(50, floor(l / 2) + 1)) else list()
  cat_indices <- as.data.frame(cat_indices)
  X_num <- as.matrix(predictors[num_vars])

  y_raw <- allTrainData$presence
  y_int <- if (is.factor(y_raw)) as.integer(y_raw) - 1L else as.integer(y_raw)

  # Combine categorical and numeric tensors in custom dataset
  ds <- dataset(
    initialize = function(X_num, X_cat, y) {
      self$X_num <- torch_tensor(X_num, dtype = torch_float())
      self$X_cat <- lapply(
        X_cat,
        function(x) torch_tensor(x, dtype = torch_long())
      )
      self$y <- torch_tensor(y + 1L, dtype = torch_long())
    },
    .getitem = function(i) {
      cat_feats <- lapply(self$X_cat, function(x) x[i])
      list(x_num = self$X_num[i, ], x_cat = cat_feats, y = self$y[i])
    },
    .length = function() self$X_num$size()[1]
  )(X_num, cat_indices, y_int)

  batch_size <- if (isTuningOn) 64 else 32
  epochs <- if (isTuningOn) 100 else 20
  dl <- dataloader(ds, batch_size = batch_size, shuffle = TRUE)

  net <- nn_module(
    "CNNWithEmbeddings",
    initialize = function(n_num, cat_levels, embedding_dims) {
      self$has_cat <- length(cat_levels) > 0
      if (self$has_cat) {
        self$embeddings <- nn_module_list(
          mapply(
            function(l, d)
              nn_embedding(num_embeddings = l + 2, embedding_dim = d),
            cat_levels,
            embedding_dims,
            SIMPLIFY = FALSE
          )
        )
        embed_dim <- sum(unlist(embedding_dims))
      } else {
        embed_dim <- 0
        self$embeddings <- NULL
      }
      self$fc1 <- nn_linear(embed_dim + n_num, 16)
      self$fc2 <- nn_linear(16, 2)
    },
    forward = function(x_num, x_cat) {
      if (self$has_cat) {
        embeds <- lapply(
          seq_along(x_cat),
          function(i) self$embeddings[[i]](x_cat[[i]])
        )
        x_cat_emb <- torch_cat(embeds, dim = 2)
        x <- torch_cat(list(x_num, x_cat_emb), dim = 2)
      } else {
        x <- x_num
      }
      x <- nnf_relu(self$fc1(x))
      self$fc2(x)
    }
  )(ncol(X_num), unlist(cat_levels), unlist(embedding_dims))

  device <- torch_device("cpu")
  net <- net$to(device = device)
  opt <- optim_adam(net$parameters, lr = 1e-3)
  lossf <- nn_cross_entropy_loss()

  for (ep in seq_len(epochs)) {
    net$train()
    coro::loop(
      for (b in dl) {
        opt$zero_grad()
        x_num <- b$x_num$to(device = device)
        x_cat <- lapply(b$x_cat, function(x) x$to(device = device))
        y <- b$y$to(device = device)

        logits <- net(x_num, x_cat)
        loss <- lossf(logits, y)
        loss$backward()
        opt$step()
      }
    )
  }

  # Compute variable importance
  vimp <- numeric(length(num_vars) + length(cat_vars))
  names(vimp) <- c(num_vars, cat_vars)

  # For numeric vars: use sum of absolute weights from fc1
  if (length(num_vars)) {
    weights <- net$fc1$weight$data()[, 1:length(num_vars)]$abs()$sum(dim = 1)
    vimp[num_vars] <- as.numeric(weights)
  }

  # For categorical vars: use average norm of embedding weights
  if (length(cat_vars)) {
    emb_norms <- sapply(net$embeddings, function(e) {
      torch_mean(torch_norm(e$weight$data(), p = 2, dim = 2))$item()
    })
    vimp[cat_vars] <- emb_norms
  }

  class(net) <- c("torchCNN", class(net))
  list(
    model = net,
    vimp = vimp,
    feature_names = names(predictors),
    cat_vars = cat_vars,
    num_vars = num_vars,
    cat_levels = cat_levels,
    embedding_dims = embedding_dims
  )
}


## Predict presence using a trained CNN model
#' @param model A trained CNN model (torchCNN object).
#' @param newdata A SpatRaster object containing the data to predict on.
#' @param ... Additional arguments (not used).
predictCNN <- function(model, newdata, isRaster = TRUE, ...) {
  if (isRaster) {
    df_full <- as.data.frame(newdata, xy = FALSE, cells = FALSE, na.rm = FALSE)
    valid_idx <- complete.cases(df_full)
    df <- df_full[valid_idx, , drop = FALSE]
  } else if (is.data.frame(newdata) || is.matrix(newdata)) {
    df <- as.data.frame(newdata, stringsAsFactors = TRUE)
    drop <- intersect(c("presence", "kfold"), names(df))
    if (length(drop)) df <- df[, setdiff(names(df), drop), drop = FALSE]
    valid_idx <- rep(TRUE, nrow(df))
  } else {
    stop(
      "`newdata` must be a SpatRaster or a data.frame / matrix of predictors"
    )
  }

  # Extract numeric and categorical variables
  num_vars <- model$num_vars
  cat_vars <- model$cat_vars
  cat_levels <- model$cat_levels

  X_num <- as.matrix(df[, num_vars, drop = FALSE])
  storage.mode(X_num) <- "double"

  # Convert categorical variables to integer indices
  X_cat <- lapply(seq_along(cat_vars), function(i) {
    f <- factor(df[[cat_vars[i]]], levels = seq_len(cat_levels[[i]]))
    x <- as.integer(f)
    x[is.na(x)] <- cat_levels[[i]] + 1
    x
  })

  # Prepare tensors
  X_num_tensor <- torch_tensor(X_num, dtype = torch_float())
  X_cat_tensor <- lapply(
    X_cat,
    function(x) torch_tensor(x, dtype = torch_long())
  )

  # Predict
  dev <- if (cuda_is_available()) torch_device("cuda") else torch_device("cpu")
  mdl <- model$model$to(device = dev)$eval()

  X_num_tensor <- X_num_tensor$to(device = dev)
  X_cat_tensor <- lapply(X_cat_tensor, function(x) x$to(device = dev))

  probs_t <- with_no_grad({
    logits <- mdl(X_num_tensor, X_cat_tensor)
    nnf_softmax(logits, dim = 2)$to(device = torch_device("cpu"))
  })

  mat <- as.matrix(probs_t)
  colnames(mat) <- c("absent", "presence")

  if (isRaster) {
    outR <- newdata[[1]]
    pred_vals <- rep(NA, terra::ncell(outR))
    pred_vals[valid_idx] <- mat[, "presence"]
    terra::values(outR) <- pred_vals
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

processCovariates <- function(trainingCovariateDataframe, modelType) {
  # filter for NA values
  trainingCovariateDataframe <- trainingCovariateDataframe %>%
    filter(!is.na(trainingCovariateDataframe[, 1]))

  covariateRasterList <- c()

  if (nrow(trainingCovariateDataframe) > 0) {
    if (
      modelType == "Random Forest" ||
        modelType == "MaxEnt" ||
        modelType == "CNN"
    ) {
      for (row in seq(1, nrow(trainingCovariateDataframe), by = 1)) {
        covariateRaster <- rast(trainingCovariateDataframe[row, 1])
        dataType <- as.character(trainingCovariateDataframe[row, 2])

        if (dataType == "Categorical") {
          covariateRaster <- as.factor(covariateRaster)
        } else if (dataType == "Continuous") {
          covariateRaster <- as.numeric(covariateRaster)
        } else {
          stop("Data type not recognized")
        }
        covariateRasterList <- c(covariateRasterList, covariateRaster)
      }

      mergedCovariateRaster <- rast(covariateRasterList)
    }

    return(mergedCovariateRaster)
  }
}

# add covariate rasters to training data
addCovariates <- function(rasterList, covariateRaster) {
  # Merge covariateFiles with each raster in trainingRasterList
  for (i in seq_along(rasterList)) {
    rasterList[[i]] <- c(rasterList[[i]], covariateRaster)
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

# preprocess rasters to ensure each layer has the same pattern of NA values
checkAndMaskNA <- function(rasterList) {
  processedRasterList <- list()

  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]

    # Check if each layer has the same number of non NA cells
    cellCounts <- sapply(1:nlyr(raster), function(j) {
      sum(!is.na(terra::values(raster[[j]])))
    })

    if (length(unique(cellCounts)) != 1) {
      msg <- sprintf(
        "Input raster %d: Layers have unequal number of cells. Applying NA mask to standardize NA pattern.",
        i
      )
      if (exists("updateRunLog")) {
        updateRunLog(msg, type = "info")
      } else {
        message(msg)
      }

      # Create a mask of cells that are NA in any layer
      naMask <- terra::app(raster, fun = function(x) any(is.na(x)))

      # Apply mask so all layers share the same NA pattern
      processedRaster <- terra::mask(raster, naMask, maskvalues = 1)
    } else {
      processedRaster <- raster
    }

    processedRasterList[[i]] <- processedRaster
  }

  return(processedRasterList)
}

preprocessTrainingData <- function(
  allTrainData,
  response = "presence",
  exclude = c("kfold")
) {
  predictorVars <- setdiff(names(allTrainData), c(response, exclude))

  initialN <- nrow(allTrainData)
  completeRows <- !is.na(allTrainData[[response]]) &
    complete.cases(allTrainData[, predictorVars])
  cleanedData <- allTrainData[completeRows, ]
  rowsRemoved <- initialN - nrow(cleanedData)

  if (rowsRemoved > 0) {
    warning(sprintf(
      "Preprocessing: Removed %d rows with NA in '%s' or predictors.",
      rowsRemoved,
      response
    ))
  }

  return(cleanedData)
}

# Function to load a CNN model from separate .pt and .rds files
loadCNNModel <- function(weights_path, metadata_path) {
  # Define CNNWithEmbeddings inside the function scope
  CNNWithEmbeddings <- nn_module(
    "CNNWithEmbeddings",
    initialize = function(n_num, cat_levels, embedding_dims) {
      self$has_cat <- length(cat_levels) > 0
      if (self$has_cat) {
        self$embeddings <- nn_module_list(
          mapply(
            function(l, d)
              nn_embedding(num_embeddings = l + 2, embedding_dim = d),
            cat_levels,
            embedding_dims,
            SIMPLIFY = FALSE
          )
        )
        embed_dim <- sum(unlist(embedding_dims))
      } else {
        embed_dim <- 0
        self$embeddings <- NULL
      }
      self$fc1 <- nn_linear(embed_dim + n_num, 16)  # <-- Match here
      self$fc2 <- nn_linear(16, 2)                  # <-- Match here
    },
    forward = function(x_num, x_cat) {
      if (self$has_cat) {
        embeds <- lapply(
          seq_along(x_cat),
          function(i) self$embeddings[[i]](x_cat[[i]])
        )
        x_cat_emb <- torch_cat(embeds, dim = 2)
        x <- torch_cat(list(x_num, x_cat_emb), dim = 2)
      } else {
        x <- x_num
      }
      x <- nnf_relu(self$fc1(x))
      x <- self$fc2(x)
    }
  )

  # Load metadata (cat_levels, feature_names, etc.)
  metadata <- readRDS(metadata_path)

  # Reconstruct model architecture
  embedding_dims <- metadata$embedding_dims
  net <- CNNWithEmbeddings(
    n_num = length(metadata$num_vars),
    cat_levels = unlist(metadata$cat_levels),
    embedding_dims = unlist(embedding_dims)
  )
  class(net) <- c("torchCNN", class(net))

  # Load weights
  net$load_state_dict(torch_load(weights_path))

  # Return full modelOut object
  c(list(model = net), metadata)
}


### Get the range of raster values in the raster
getValueRange <- function(rasterList, layerName, nBins = 20, nSample = 5000) {
  mins <- vapply(
    rasterList,
    function(r) terra::global(r[[layerName]], "min", na.rm = TRUE)[1, 1],
    numeric(1)
  )
  maxs <- vapply(
    rasterList,
    function(r) terra::global(r[[layerName]], "max", na.rm = TRUE)[1, 1],
    numeric(1)
  )
  rastMin <- min(mins, na.rm = TRUE)
  rastMax <- max(maxs, na.rm = TRUE)

  brks <- seq(rastMin, rastMax, length.out = nBins + 1)

  ## sample each raster’s layer, flatten into one vector
  perRaster <- ceiling(nSample / length(rasterList))
  sampVals <- unlist(
    lapply(rasterList, function(r) {
      as.vector(spatSample(r[[layerName]], perRaster, na.rm = TRUE))
    }),
    use.names = FALSE
  )

  ## Calc histogram
  h <- hist(sampVals, breaks = brks, plot = FALSE)

  ## return a clean data.frame
  data.frame(
    layer = layerName,
    bin_lower = head(h$breaks, -1),
    bin_upper = tail(h$breaks, -1),
    count = h$counts,
    pct = h$counts / sum(h$counts)
  )
}

getRastLayerHistogram <- function(
  rasterList,
  nBins = 20,
  nSample = 10000
) {
  first_raster <- rasterList[[1]]
  layerNames <- names(first_raster)
  if (is.null(layerNames) || length(layerNames) == 0) {
    stop("rasterList[[1]] must have at least one named layer.")
  }

  # Determine layer types
  is_numeric_layer <- sapply(layerNames, function(name) {
    vals <- terra::values(first_raster[[name]], mat = FALSE)
    is.numeric(vals) && length(unique(vals)) > 15
  })

  numeric_layers <- layerNames[is_numeric_layer]
  categorical_layers <- setdiff(layerNames, numeric_layers)

  # Histogram for numeric layers
  numeric_df <- foreach(
    layerName = numeric_layers,
    .combine = rbind
  ) %do% {
    getValueRange(rasterList, layerName, nBins, nSample)
  }

  # Placeholder for categorical layers
  categorical_df <- tibble::tibble(
    layer = categorical_layers,
    bin_lower = NA_real_,
    bin_upper = NA_real_,
    count = NA_real_,
    pct = NA_real_
  )

  return(bind_rows(numeric_df, categorical_df))
}

## Predict the response across the range of values based on the histogram
predictResponseHistogram <- function(rastLayerHistogram, model, modelType) {
  # Separate numeric and categorical variables in the model
  numeric_layers <- intersect(unique(rastLayerHistogram$layer), model$num_vars)
  categorical_layers <- intersect(model$cat_vars, unique(rastLayerHistogram$layer))

  # Prepare prediction grid for numeric layers
  rastHistogramPredict <- rastLayerHistogram %>%
    filter(layer %in% numeric_layers) %>%
    select(layer, bin_lower) %>%
    group_by(layer) %>%
    mutate(row = row_number()) %>%
    ungroup() %>%
    tidyr::spread(layer, bin_lower) %>%
    select(-row)

  # Predict for numeric layers
  numeric_predictions <- purrr::map_dfr(numeric_layers, function(layerName) {
    predictLayerTemp <- rastHistogramPredict
    fixed_cols <- setdiff(names(predictLayerTemp), layerName)
    predictLayerTemp[fixed_cols] <- purrr::map_dfc(
      predictLayerTemp[fixed_cols],
      ~ mean(.x, na.rm = TRUE)
    )

    preds <- if (modelType == "CNN") {
      # Add back missing categorical variables with a fixed level
      missing_cat_vars <- setdiff(model$cat_vars, names(predictLayerTemp))
      for (v in missing_cat_vars) {
        num_levels <- model$cat_levels[[v]]
        # Assign integer index directly, NOT factor
        predictLayerTemp[[v]] <- rep(1L, nrow(predictLayerTemp))
      }

      # Ensure column order is correct
      predictLayerTemp <- predictLayerTemp[, c(model$num_vars, model$cat_vars), drop = FALSE]

      predict_cnn_dataframe(model, predictLayerTemp, "prob")
    } else {
      # Add back missing categorical variables with a fixed level
      missing_cat_vars <- setdiff(model$cat_vars, names(predictLayerTemp))
      for (v in missing_cat_vars) {
      num_levels <- model$cat_levels[[v]]
      # Use index 1 for all rows (assuming it corresponds to a valid level)
      predictLayerTemp[[v]] <- as.integer(1L)
    }
      predict(model$model, predictLayerTemp, type = "response")$predictions
    }

    tibble::tibble(
      layer = layerName,
      predictor = predictLayerTemp[[layerName]],
      response = preds[, 2]
    )
  })

  # Generate empty rows for categorical variables
  cat_predictions <- purrr::map_dfr(categorical_layers, function(layerName) {
    tibble::tibble(
      layer = layerName,
      predictor = NA,
      response = NA_real_
    )
  })

  # Combine both
  predictedLayers <- bind_rows(numeric_predictions, cat_predictions)
  return(predictedLayers)
}

## plot histogram and responses to raster layers

plotLayerHistogram <- function(histogramData, transferDir) {
  # Only keep numeric predictors
  histogramData <- histogramData %>%
    dplyr::filter(!is.na(predictor))

  # Skip plotting if there's nothing to show
  if (nrow(histogramData) == 0) {
    warning("No numeric data available to plot.")
    return(NULL)
  }

  n_vars <- unique(histogramData$layer) %>% length()

  # Adjust font size and image height based on number of variables
  font_size <- if (n_vars <= 6) {
    20
  } else if (n_vars <= 12) {
    16
  } else if (n_vars <= 18) {
    12
  } else {
    10
  }

  width_per_facet <- 1
  height_per_facet <- 0.66
  plot_width <- max(10, n_vars * width_per_facet)
  plot_height <- max(10, n_vars * height_per_facet)

  p <- ggplot2::ggplot(histogramData, aes(x = predictor, y = response)) +
    ggplot2::geom_col(
      aes(y = pct * max(response, na.rm = TRUE)),
      fill = "gray80",
      width = 0.05
    ) +
    ggplot2::geom_line(linewidth = 1.2, color = "Grey20") +
    ggplot2::facet_wrap(~layer, scales = "free") +
    ggplot2::labs(
      x = "Layer values",
      y = "Predicted response",
      title = "Training layer histogram and response"
    ) +
    ggplot2::theme_classic(base_size = font_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10)
      ),
      plot.title.position = "plot"
    )

  outfile <- file.path(transferDir, "LayerHistogramResponse.png")
  ggplot2::ggsave(
    filename = outfile,
    plot = p,
    height = plot_height,
    width = plot_width,
    units = "in",
    dpi = 300
  )
}

predict_cnn_dataframe <- function(model, newdata, return = c("class", "prob")) {
  return <- match.arg(return)

  if (!inherits(model$model, "nn_module")) {
    stop("model$model must be a torch nn_module object")
  }

  if (!is.data.frame(newdata)) {
    stop("newdata must be a data.frame")
  }

  # Extract model metadata
  num_vars <- model$num_vars
  cat_vars <- model$cat_vars
  cat_levels <- model$cat_levels

  # Handle numeric input
  X_num <- as.matrix(newdata[, num_vars, drop = FALSE])
  if (anyNA(X_num)) stop("NA values in numeric predictors not allowed.")
  input_num <- torch_tensor(X_num, dtype = torch_float())

  # Handle categorical input
  input_cat <- lapply(cat_vars, function(var) {
    levels_train <- cat_levels[[var]]
    x <- newdata[[var]]
    if (!is.factor(x)) x <- factor(x, levels = levels_train)
    idx <- as.integer(factor(x, levels = levels_train))
    idx[is.na(idx)] <- length(levels_train) + 1  # handle unknowns
    torch_tensor(idx, dtype = torch_long())
  })

  # Predict using model$model
  net <- model$model
  net$eval()
  with_no_grad({
    output <- net(input_num, input_cat)
  })

  if (return == "prob") {
    probs <- output$softmax(dim = 2)
    return(as_array(probs))
  } else {
    classes <- output$argmax(dim = 2)$to(dtype = torch_int())$cpu()
    return(as_array(classes))
  }
}
