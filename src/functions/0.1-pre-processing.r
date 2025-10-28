## -------------------------------
## ecoClassify - Pre-processing Functions
## ApexRMS, November 2024
## -------------------------------

#' Assign Generic CRS to Raster ----
#'
#' @description
#' Assigns a generic planar coordinate reference system to rasters
#' that don't have a CRS defined.
#'
#' @param raster A SpatRaster object
#' @return A SpatRaster with CRS assigned if it was missing
#'
#' @details
#' This function checks if a raster has a defined CRS. If not, it assigns
#' a generic planar coordinate system suitable for non-geographic data
#' (like DSLR camera images) where coordinates are in arbitrary units.
#' @noRd
assignGenericCRS <- function(raster) {
  # Check if raster has a CRS
  if (is.na(terra::crs(raster)) || terra::crs(raster) == "") {
    # Assign a generic planar CRS suitable for non-geographic data
    # This is essentially a Cartesian coordinate system with no specific projection
    terra::crs(raster) <- "local"

    updateRunLog(
      "No CRS found for input raster; assigned generic 2D coordinate system",
      type = "info"
    )
  }

  return(raster)
}

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
#' to a list. If rasters don't have a CRS defined, a generic planar CRS is assigned.
#' @noRd
extractRasters <- function(dataframe, column) {
  # drop rows with NA in the selected column
  dataframe <- dataframe[!is.na(dataframe[[column]]), , drop = FALSE]

  # timesteps (assumes column 1 holds them)
  timesteps <- unique(dataframe[[1]])

  # preallocate a proper list and keep names by timestep
  rasterList <- vector("list", length(timesteps))
  names(rasterList) <- as.character(timesteps)

  # build one SpatRaster per timestep
  for (i in seq_along(timesteps)) {
    t <- timesteps[i]
    subsetData <- dataframe[dataframe[[1]] == t, , drop = FALSE]
    allFiles <- as.vector(subsetData[[column]])

    subsetRaster <- terra::rast(allFiles)

    # assign generic CRS if none exists
    subsetRaster <- assignGenericCRS(subsetRaster)

    # for ground truth column (3): keep the single/first layer only
    if (column == 3) {
      subsetRaster <- subsetRaster[[1]]
    }

    rasterList[[i]] <- subsetRaster
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
  tuningObjective <- as.character(advClassifierOptionsDataframe$tuningObjective)
  setManualThreshold <- advClassifierOptionsDataframe$setManualThreshold
  manualThreshold <- advClassifierOptionsDataframe$manualThreshold
  normalizeRasters <- advClassifierOptionsDataframe$normalizeRasters
  rasterDecimalPlaces <- advClassifierOptionsDataframe$rasterDecimalPlaces
  setSeed <- advClassifierOptionsDataframe$setSeed

  # assign value of 3 to contextualizationWindowSize if not specified
  if (isTRUE(applyContextualization)) {
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

  if (isTRUE(setManualThreshold)) {
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

  # Extract override band names option
  overrideBandnames <- advClassifierOptionsDataframe$overrideBandnames
  if (is.null(overrideBandnames) || is.na(overrideBandnames)) {
    overrideBandnames <- FALSE
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
    rasterDecimalPlaces,
    tuningObjective,
    overrideBandnames,
    setSeed
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

        # assign generic CRS if none exists
        covariateRaster <- assignGenericCRS(covariateRaster)

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
    normalizedRaster <- lapply(1:nlyr(raster), function(i) {
      normalizeBand(raster[[i]])
    }) %>%
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


validateAndAlignRasters <- function(trainingRasterList, groundTruthRasterList) {
  #' Validate training rasters consistency and align ground truth rasters
  #'
  #' @param trainingRasterList List of training rasters
  #' @param groundTruthRasterList List of ground truth rasters
  #' @return List with aligned ground truth rasters
  #' @description This function:
  #'   1. Validates that all training rasters have matching spatial properties
  #'   2. Resamples ground truth rasters to match training rasters if needed

  if (length(trainingRasterList) == 0 || length(groundTruthRasterList) == 0) {
    stop(
      "Both training and ground truth raster lists must contain at least one raster"
    )
  }

  if (length(trainingRasterList) != length(groundTruthRasterList)) {
    stop("Training and ground truth raster lists must have the same length")
  }

  # Type checks to fail fast with helpful errors
  if (!all(vapply(trainingRasterList, inherits, logical(1), "SpatRaster"))) {
    stop("All elements of 'trainingRasterList' must be SpatRaster objects")
  }
  if (!all(vapply(groundTruthRasterList, inherits, logical(1), "SpatRaster"))) {
    stop("All elements of 'groundTruthRasterList' must be SpatRaster objects")
  }

  # Function to extract raster properties
  getRasterProperties <- function(raster) {
    list(
      extent = terra::ext(raster),
      resolution = terra::res(raster),
      crs = terra::crs(raster, proj = TRUE),
      nrows = terra::nrow(raster),
      ncols = terra::ncol(raster)
    )
  }

  # Function to compare two raster properties
  compareRasterProperties <- function(props1, props2, tolerance = 1e-6) {
    # Compare extent
    extent_match <- all(
      abs(as.vector(props1$extent) - as.vector(props2$extent)) < tolerance
    )

    # Compare resolution
    res_match <- all(abs(props1$resolution - props2$resolution) < tolerance)

    # Compare CRS (convert to character for comparison)
    crs_match <- identical(as.character(props1$crs), as.character(props2$crs))

    # Compare dimensions
    dim_match <- (props1$nrows == props2$nrows) &&
      (props1$ncols == props2$ncols)

    return(list(
      extent = extent_match,
      resolution = res_match,
      crs = crs_match,
      dimensions = dim_match,
      overall = extent_match && res_match && crs_match && dim_match
    ))
  }

  # Step 1: Validate training rasters consistency
  cat("Validating training raster consistency...\n")

  # Get properties of the first training raster as reference
  reference_props <- getRasterProperties(trainingRasterList[[1]])

  # Check all training rasters against the reference
  for (i in seq_along(trainingRasterList)) {
    current_props <- getRasterProperties(trainingRasterList[[i]])
    comparison <- compareRasterProperties(reference_props, current_props)

    if (!comparison$overall) {
      error_msg <- paste0(
        "Training raster ",
        i,
        " does not match reference raster properties:\n"
      )

      if (!comparison$extent) {
        error_msg <- paste0(
          error_msg,
          "  - Extent mismatch: ",
          "Reference: ",
          paste(as.vector(reference_props$extent), collapse = ", "),
          " vs Current: ",
          paste(as.vector(current_props$extent), collapse = ", "),
          "\n"
        )
      }

      if (!comparison$resolution) {
        error_msg <- paste0(
          error_msg,
          "  - Resolution mismatch: ",
          "Reference: ",
          paste(reference_props$resolution, collapse = ", "),
          " vs Current: ",
          paste(current_props$resolution, collapse = ", "),
          "\n"
        )
      }

      if (!comparison$crs) {
        error_msg <- paste0(
          error_msg,
          "  - CRS mismatch: ",
          "Reference: ",
          as.character(reference_props$crs),
          " vs Current: ",
          as.character(current_props$crs),
          "\n"
        )
      }

      if (!comparison$dimensions) {
        error_msg <- paste0(
          error_msg,
          "  - Dimension mismatch: ",
          "Reference: ",
          reference_props$nrows,
          "x",
          reference_props$ncols,
          " vs Current: ",
          current_props$nrows,
          "x",
          current_props$ncols,
          "\n"
        )
      }

      stop(error_msg)
    }
  }

  cat("✓ All training rasters have consistent spatial properties\n")

  # Step 2: Align ground truth rasters with training rasters
  cat("Checking and aligning ground truth rasters...\n")

  aligned_groundTruthRasterList <- vector("list", length(groundTruthRasterList))
  resampled_count <- 0

  for (i in seq_along(groundTruthRasterList)) {
    training_props <- getRasterProperties(trainingRasterList[[i]])
    groundtruth_props <- getRasterProperties(groundTruthRasterList[[i]])

    comparison <- compareRasterProperties(training_props, groundtruth_props)

    if (comparison$overall) {
      # Ground truth already matches training raster
      aligned_groundTruthRasterList[[i]] <- groundTruthRasterList[[i]]
      cat("  Raster", i, ": Already aligned\n")
    } else {
      # Need to resample ground truth to match training raster
      cat("  Raster", i, ": Resampling ground truth raster...\n")

      # Log the differences
      if (!comparison$extent || !comparison$dimensions) {
        cat("    - Spatial extent/dimensions differ\n")
      }
      if (!comparison$resolution) {
        cat("    - Resolution differs\n")
      }
      if (!comparison$crs) {
        cat("    - CRS differs\n")
      }

      # Resample using nearest neighbor for categorical data (typical for ground truth)
      aligned_groundTruthRasterList[[i]] <- tryCatch(
        {
          if (!comparison$crs) {
            terra::project(
              groundTruthRasterList[[i]],
              trainingRasterList[[i]],
              method = "near"
            )
          } else {
            terra::resample(
              groundTruthRasterList[[i]],
              trainingRasterList[[i]],
              method = "near"
            )
          }
        },
        error = function(e) {
          op <- if (!comparison$crs) "project" else "resample"
          stop(sprintf(
            "Failed to %s ground truth raster %d: %s",
            op,
            i,
            conditionMessage(e)
          ))
        }
      )

      resampled_count <- resampled_count + 1
    }
  }

  if (resampled_count > 0) {
    cat(
      "✓ Resampled",
      resampled_count,
      "ground truth raster(s) to match training rasters\n"
    )
  } else {
    cat("✓ All ground truth rasters already aligned with training rasters\n")
  }

  # Final validation
  cat("Performing final validation...\n")
  for (i in seq_along(aligned_groundTruthRasterList)) {
    training_props <- getRasterProperties(trainingRasterList[[i]])
    aligned_props <- getRasterProperties(aligned_groundTruthRasterList[[i]])

    comparison <- compareRasterProperties(training_props, aligned_props)

    if (!comparison$overall) {
      msg <- sprintf("Final validation failed for raster pair %d:\n", i)
      if (!comparison$extent) {
        msg <- paste0(msg, "  - Extent mismatch\n")
      }
      if (!comparison$resolution) {
        msg <- paste0(msg, "  - Resolution mismatch\n")
      }
      if (!comparison$crs) {
        msg <- paste0(msg, "  - CRS mismatch\n")
      }
      if (!comparison$dimensions) {
        msg <- paste0(msg, "  - Dimension mismatch\n")
      }
      stop(msg)
    }
  }

  cat("✓ All rasters successfully validated and aligned\n")

  return(aligned_groundTruthRasterList)
}

#' Override Band Names with Standardized Names ----
#'
#' @description
#' Renames all bands in raster stacks to band1, band2, band3, etc.
#' This prevents errors when using multiple JPEGs or other images with
#' filename-based band names.
#'
#' @param rasterList List of SpatRasters.
#'
#' @return List of SpatRasters with standardized band names.
#'
#' @noRd
overrideBandNames <- function(rasterList) {
  renamedRasterList <- c()

  for (raster in rasterList) {
    # Get number of bands
    nBands <- terra::nlyr(raster)

    # Generate standardized band names
    newNames <- paste0("band", seq_len(nBands))

    # Assign new names
    names(raster) <- newNames

    # Append to list
    renamedRasterList <- c(renamedRasterList, raster)
  }

  return(renamedRasterList)
}
