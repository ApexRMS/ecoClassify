## -------------------------------
## ecoClassify - Pre-processing Functions
## ApexRMS, November 2024
## -------------------------------

#' Extract rasters from filepaths in a dataframe ----
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

#' Assign objects froma a datasheet of input variables ----
#'
#' @description
#' 'assignVariables' extracts variables from datashseets in the specified
#' syncrosim scenario and assigns them to an object.
#'
#' @param myScenario syncrosim scenario object
#' @param trainingRasterDataframe dataframe with input variables
#' @param column integer specifying the column to extract
#' @return list of objects (timestepList = numeric, nObs = numeric,
#' contextualizationWindowSize = numeric,
#' modelType = String ("Random Forest", "MaxENt", or "CNN"),
#' modelTuning = boolean, setManualThreshold = boolean,
#' manualThreshold = numeric, normalizeRasters = boolean,
#' rasterDecimalPlaces = numeric) that have been extracted from the
#' syncrosim datasheet.
#'
#' @details
#' This function extracts variables from the syncrosim datasheets.
#' @noRd
assignVariables <- function(myScenario, trainingRasterDataframe, column) {
  # extract unique timesteps from trainingRasterDataframe
  timestepList <- trainingRasterDataframe %>%
    filter(!is.na(column)) %>%
    pull(Timesteps) %>%
    unique()

  # Load classifier options datasheet
  classifierOptionsDataframe <- datasheet(
    myScenario,
    name = "ecoClassify_ClassifierOptions"
  )
  advClassifierOptionsDataframe <- datasheet(
    myScenario,
    name = "ecoClassify_AdvancedClassifierOptions"
  )

  # Extract model input values
  nObs <- classifierOptionsDataframe$nObs
  applyContextualization <- advClassifierOptionsDataframe$applyContextualization
  contextualizationWindowSize <- advClassifierOptionsDataframe$contextualizationWindowSize
  modelType <- as.character(classifierOptionsDataframe$modelType)
  modelTuning <- advClassifierOptionsDataframe$modelTuning
  setManualThreshold <- advClassifierOptionsDataframe$setManualThreshold
  manualThreshold <- advClassifierOptionsDataframe$manualThreshold
  normalizeRasters <- advClassifierOptionsDataframe$normalizeRasters
  rasterDecimalPlaces <- advClassifierOptionsDataframe$rasterDecimalPlaces

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

  if (
    !is.null(contextualizationWindowSize) && applyContextualization == FALSE
  ) {
    updateRunLog(
      "Contextualization window size was supplied but applyContextualization is set to FALSE; no contextualization will be applied",
      type = "info"
    )
  }

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

  return(list(
    timestepList,
    nObs,
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

#' Reclassify Ground Truth Rasters ----
#'
#' @description
#' Converts ground truth raster values to binary 0/1.
#'
#' @param groundTruthRasterList List of SpatRasters.
#'
#' @return List of reclassified SpatRasters.
#'
#' @noRd
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

#' Load and Process Covariate Rasters ----
#'
#' @description
#' Reads raster covariates and converts them to factor or numeric types.
#'
#' @param trainingCovariateDataframe Dataframe with paths and data types.
#' @param modelType Character, used for conditional processing.
#'
#' @return A SpatRaster with all covariates merged.
#'
#' @noRd
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

#' Add Covariates to Raster List ----
#'
#' @description
#' Adds covariate rasters to each raster in a list.
#'
#' @param rasterList List of SpatRasters.
#' @param covariateRaster SpatRaster of covariates to append.
#'
#' @return Updated list of SpatRasters.
#'
#' @noRd
addCovariates <- function(rasterList, covariateRaster) {
  # Merge covariateFiles with each raster in trainingRasterList
  for (i in seq_along(rasterList)) {
    rasterList[[i]] <- c(rasterList[[i]], covariateRaster)
  }

  return(rasterList)
}

#' Normalize Raster Band ----
#'
#' @description
#' Scales raster values to 0–1 range.
#'
#' @param band A SpatRaster layer.
#'
#' @return A normalized SpatRaster.
#'
#' @noRd
normalizeBand <- function(band) {
  min_val <- min(values(band), na.rm = TRUE)
  max_val <- max(values(band), na.rm = TRUE)
  (band - min_val) / (max_val - min_val)
}

#' Normalize All Bands in Raster List ----
#'
#' @description
#' Applies 0–1 normalization to each band in a list of raster stacks.
#'
#' @param rasterList List of SpatRasters.
#'
#' @return List of normalized SpatRasters.
#'
#' @noRd
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

#' Ensure Consistent NA Masking ----
#'
#' @description
#' Applies a consistent NA mask across all layers in a raster.
#'
#' @param rasterList List of SpatRasters.
#'
#' @return List of SpatRasters with uniform NA pattern.
#'
#' @noRd
checkNA <- function(rasterList, verbose = TRUE) {
  if (!is.list(rasterList)) {
    rasterList <- list(rasterList)
  }

  issues <- FALSE

  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]

    if (terra::nlyr(raster) == 1) {
      next
    }

    # Fast check using global function - correct syntax
    cellCounts <- terra::global(raster, fun = function(x) sum(!is.na(x)))
    cellCounts <- as.numeric(cellCounts[, 1])

    if (length(unique(cellCounts)) != 1) {
      issues <- TRUE
      if (verbose) {
        message(sprintf(
          "Raster %d: Inconsistent NA patterns (range: %d-%d valid cells)",
          i,
          min(cellCounts),
          max(cellCounts)
        ))
      }
    }
  }

  return(issues)
}
