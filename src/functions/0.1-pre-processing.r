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

#' Align a Timestep List with a Loaded Raster List ----
#'
#' @description
#' `extractRasters()` drops dataframe rows whose raster file path is NA, so the
#' timesteps it loads can be a subset of those returned by `assignVariables()`.
#' Downstream code labels outputs positionally (`timestepList[t]` against
#' `rasterList[[t]]`), so a dropped timestep would shift the labels of every
#' output after it. This reorders and filters `timestepList` to correspond
#' element-for-element with `rasterList`, and warns about any dropped timesteps.
#'
#' @param timestepList Vector of timesteps from `assignVariables()`.
#' @param rasterList Named list of SpatRasters from `extractRasters()`.
#' @param context Character label describing the rasters, used in messages.
#'
#' @return `timestepList` filtered and ordered to match `names(rasterList)`,
#'   preserving the original vector's type.
#'
#' @noRd
alignTimestepList <- function(timestepList, rasterList, context = "rasters") {
  loadedKeys <- names(rasterList)

  if (is.null(loadedKeys)) {
    updateRunLog(
      paste0(
        "Could not align timesteps with the loaded ", context,
        ": the raster list has no timestep names. Output timestep labels may be ",
        "incorrect if any rows were missing a raster file."
      ),
      type = "warning"
    )
    return(timestepList)
  }

  matchIdx <- match(loadedKeys, as.character(timestepList))

  # Defensive: every loaded key originates from the same Timesteps column as
  # timestepList, so this should be unreachable. Leave timestepList untouched
  # rather than introducing NA labels.
  if (anyNA(matchIdx)) {
    updateRunLog(
      paste0(
        "Loaded ", context, " include timestep(s) ",
        paste(loadedKeys[is.na(matchIdx)], collapse = ", "),
        " that are absent from the scenario timestep list; timestep labels were ",
        "left unchanged."
      ),
      type = "warning"
    )
    return(timestepList)
  }

  droppedIdx <- setdiff(seq_along(timestepList), matchIdx)
  if (length(droppedIdx) > 0) {
    updateRunLog(
      paste0(
        "No ", context, " were loaded for timestep(s) ",
        paste(timestepList[droppedIdx], collapse = ", "),
        " (missing raster file path); these timesteps were dropped."
      ),
      type = "warning"
    )
  }

  return(timestepList[matchIdx])
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

  # Extract model input values with fallback defaults
  nObs <- classifierOptionsDataframe$nObs
  if (is.null(nObs) || length(nObs) == 0 || is.na(nObs)) nObs <- 10000L

  modelType_raw <- classifierOptionsDataframe$modelType
  if (is.null(modelType_raw) || length(modelType_raw) == 0 || is.na(modelType_raw)) {
    modelType <- "Random Forest"
  } else {
    modelType <- as.character(modelType_raw)
  }

  normalizeRasters <- advClassifierOptionsDataframe$normalizeRasters
  if (is.null(normalizeRasters) || length(normalizeRasters) == 0 || is.na(normalizeRasters)) normalizeRasters <- FALSE

  rasterDecimalPlaces <- advClassifierOptionsDataframe$rasterDecimalPlaces
  if (is.null(rasterDecimalPlaces) || length(rasterDecimalPlaces) == 0 || is.na(rasterDecimalPlaces)) rasterDecimalPlaces <- 2L

  modelTuning <- advClassifierOptionsDataframe$modelTuning
  if (is.null(modelTuning) || length(modelTuning) == 0 || is.na(modelTuning)) modelTuning <- TRUE

  tuningObjective_raw <- advClassifierOptionsDataframe$tuningObjective
  if (is.null(tuningObjective_raw) || length(tuningObjective_raw) == 0 || is.na(tuningObjective_raw)) {
    tuningObjective <- "Youden"
  } else {
    tuningObjective <- as.character(tuningObjective_raw)
  }

  setManualThreshold <- advClassifierOptionsDataframe$setManualThreshold
  if (is.null(setManualThreshold) || length(setManualThreshold) == 0 || is.na(setManualThreshold)) setManualThreshold <- FALSE

  manualThreshold <- advClassifierOptionsDataframe$manualThreshold
  if (is.null(manualThreshold) || length(manualThreshold) == 0 || is.na(manualThreshold)) manualThreshold <- 0.5

  applyContextualization <- advClassifierOptionsDataframe$applyContextualization
  if (is.null(applyContextualization) || length(applyContextualization) == 0 || is.na(applyContextualization)) applyContextualization <- FALSE

  contextualizationWindowSize <- advClassifierOptionsDataframe$contextualizationWindowSize

  setSeed <- advClassifierOptionsDataframe$setSeed

  # assign value of 3 to contextualizationWindowSize if not specified
  if (isTRUE(applyContextualization)) {
    if (
      is.null(contextualizationWindowSize) || is.na(contextualizationWindowSize)
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
    !is.null(contextualizationWindowSize) &&
      isTRUE(applyContextualization) == FALSE
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
#' Converts ground truth raster values to binary 0/1. When useTargetClass is
#' TRUE, maps a specific class value to 1 and all others (except ignored values)
#' to 0. Otherwise falls back to the original min→0 / max→1 behaviour.
#'
#' @param groundTruthRasterList List of SpatRasters.
#' @param useTargetClass Logical. If TRUE, apply target-class selection logic.
#' @param targetClassValue Integer. The class value to map to 1.
#' @param backgroundValues Character. Comma-separated integers to map to 0.
#'   If NULL, all non-target non-ignored non-NA values become 0.
#' @param ignoreValues Character. Comma-separated integers to leave as NA.
#'
#' @return List of reclassified SpatRasters.
#'
#' @noRd
reclassifyGroundTruth <- function(groundTruthRasterList,
                                   useTargetClass = FALSE,
                                   targetClassValue = NA,
                                   backgroundValues = NA,
                                   ignoreValues = NA) {
  lapply(groundTruthRasterList, function(raster) {
    if (isTRUE(useTargetClass) && !is.na(targetClassValue)) {
      result <- raster
      result[] <- NA
      vals <- terra::values(raster)

      # target class → 1
      result[vals == targetClassValue] <- 1

      if (!is.na(backgroundValues) && nchar(trimws(backgroundValues)) > 0) {
        bgVals <- as.integer(trimws(strsplit(backgroundValues, ",")[[1]]))
        result[vals %in% bgVals] <- 0
      } else {
        ignoreVals <- if (!is.na(ignoreValues) && nchar(trimws(ignoreValues)) > 0)
          as.integer(trimws(strsplit(ignoreValues, ",")[[1]]))
        else integer(0)
        result[!is.na(vals) & vals != targetClassValue & !(vals %in% ignoreVals)] <- 0
      }

      return(result)
    } else {
      # Original binary logic: min → 0, max → 1 (in-place, preserves any intermediate values)
      raster[raster == min(terra::values(raster), na.rm = TRUE)] <- 0
      raster[raster == max(terra::values(raster), na.rm = TRUE)] <- 1
      return(raster)
    }
  })
}

#' Load and Process Covariate Rasters ----
#'
#' @description
#' Reads raster covariates and converts them to factor or numeric types.
#' Returns a named list of SpatRasters keyed by timestep (as character) or
#' "global" for rows with no timestep. Returns NULL if no covariates are
#' provided. Rows with NA Timesteps are treated as global covariates applied
#' to any timestep that has no specific covariate entry.
#'
#' @param trainingCovariateDataframe Dataframe with optional Timesteps column,
#'   raster file paths, and data types.
#' @param modelType Character, used for conditional processing.
#'
#' @return Named list of SpatRasters, or NULL if no covariates are provided.
#'
#' @noRd
processCovariates <- function(trainingCovariateDataframe, modelType) {
  # Determine the file path and data type column names (training vs predicting)
  fileCol <- if ("trainingCovariateRasterFile" %in% names(trainingCovariateDataframe)) {
    "trainingCovariateRasterFile"
  } else {
    "predictingCovariateRasterFile"
  }
  typeCol <- if ("trainingCovariateType" %in% names(trainingCovariateDataframe)) {
    "trainingCovariateType"
  } else {
    "predictingCovariateType"
  }

  trainingCovariateDataframe <- trainingCovariateDataframe %>%
    filter(!is.na(.data[[fileCol]]))

  if (nrow(trainingCovariateDataframe) == 0) {
    return(NULL)
  }

  # Read one row's raster, apply type conversion, and return a file-backed SpatRaster.
  # as.factor()/as.numeric() can produce in-memory rasters; writing to a temp file
  # ensures the raster remains valid after any gc() calls later in the session.
  readCovariateRow <- function(filePath, dataType) {
    covariateRaster <- rast(filePath)
    covariateRaster <- assignGenericCRS(covariateRaster)
    dataTypeStr <- as.character(dataType)
    if (dataTypeStr == "Categorical") {
      covariateRaster <- as.factor(covariateRaster)
    } else if (dataTypeStr == "Continuous") {
      covariateRaster <- as.numeric(covariateRaster)
    } else {
      stop("Data type not recognized")
    }
    tmpFile <- tempfile(fileext = ".tif")
    terra::writeRaster(covariateRaster, tmpFile, overwrite = TRUE)
    terra::rast(tmpFile)
  }

  # Assign a group key: NA or absent Timesteps → "global"; numeric → character.
  hasTimestepsCol <- "Timesteps" %in% names(trainingCovariateDataframe)
  trainingCovariateDataframe$groupKey <- if (hasTimestepsCol) {
    ifelse(
      is.na(trainingCovariateDataframe$Timesteps),
      "global",
      as.character(trainingCovariateDataframe$Timesteps)
    )
  } else {
    "global"
  }

  groupKeys <- unique(trainingCovariateDataframe$groupKey)
  covariateList <- vector("list", length(groupKeys))
  names(covariateList) <- groupKeys

  for (k in groupKeys) {
    groupRows <- trainingCovariateDataframe[
      trainingCovariateDataframe$groupKey == k, , drop = FALSE
    ]
    groupRasters <- c()
    for (row in seq_len(nrow(groupRows))) {
      r <- readCovariateRow(groupRows[[fileCol]][row], groupRows[[typeCol]][row])
      groupRasters <- c(groupRasters, r)
    }
    covariateList[[k]] <- rast(groupRasters)
  }

  return(covariateList)
}

#' Add Covariates to Raster List ----
#'
#' @description
#' Adds covariate rasters to each raster in a list. `covariateList` is a named
#' list returned by `processCovariates()`, keyed by timestep (as character) or
#' "global". For each raster in `rasterList`, its timestep key is looked up in
#' `covariateList`; if no match exists the "global" entry is used as a fallback.
#' A warning is logged if neither a timestep-specific nor a global covariate is
#' found for a particular timestep.
#'
#' outDir should be the SyncroSim transferDir rather than R's tempdir() so that
#' the combined files survive for the full transformer run. On Windows, doParallel
#' PSOCK workers can scan and remove orphaned Rtmp* directories from tempdir(),
#' which would delete these files and trigger [readValues] errors.
#'
#' @param rasterList Named list of SpatRasters (names are timestep strings).
#' @param covariateList Named list of SpatRasters from processCovariates(), or NULL.
#' @param outDir Directory for writing combined file-backed rasters.
#'
#' @return Updated list of SpatRasters.
#'
#' @noRd
addCovariates <- function(rasterList, covariateList, outDir = NULL) {
  if (is.null(covariateList)) return(rasterList)

  for (i in seq_along(rasterList)) {
    timestepKey <- names(rasterList)[i]

    # Prefer timestep-specific covariates; fall back to global
    covRaster <- if (!is.null(timestepKey) && !is.null(covariateList[[timestepKey]])) {
      covariateList[[timestepKey]]
    } else if (!is.null(covariateList[["global"]])) {
      covariateList[["global"]]
    } else {
      NULL
    }

    if (is.null(covRaster)) {
      # Report both the key that was looked up and the keys that are available,
      # so a genuine timestep mismatch is distinguishable from a rasterList that
      # arrived without timestep names.
      timestepLabel <- if (is.null(timestepKey) || is.na(timestepKey) || !nzchar(timestepKey)) {
        paste0("<unnamed raster ", i, ">")
      } else {
        timestepKey
      }
      updateRunLog(
        paste0(
          "No covariates found for timestep ", timestepLabel,
          "; skipping covariate addition for this timestep. Available covariate ",
          "timesteps: ", paste(names(covariateList), collapse = ", "), "."
        ),
        type = "warning"
      )
      next
    }

    combined <- c(rasterList[[i]], covRaster)
    outFile <- if (!is.null(outDir)) {
      file.path(outDir, paste0("covCombined_", i, ".tif"))
    } else {
      tempfile(fileext = ".tif")
    }
    terra::writeRaster(combined, outFile, overwrite = TRUE)
    rasterList[[i]] <- terra::rast(outFile)
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
  # Preallocate and carry over the timestep names of rasterList; downstream
  # functions such as addCovariates() match rasters to per-timestep covariates
  # by name, so dropping the names silently discards covariate data.
  normalizedRasterList <- vector("list", length(rasterList))
  names(normalizedRasterList) <- names(rasterList)

  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]

    # normalize bands for each raster in rasterList
    normalizedRasterList[[i]] <- lapply(1:nlyr(raster), function(b) {
      normalizeBand(raster[[b]])
    }) %>%
      rast()
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
  #' Align ground truth rasters with their corresponding training rasters
  #'
  #' @param trainingRasterList List of training rasters (may have different extents)
  #' @param groundTruthRasterList List of ground truth rasters
  #' @return List with ground truth rasters resampled to match their training raster

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

  # Align ground truth rasters with training rasters
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

#' Override Band Names Using a Label File ----
#'
#' @description
#' Renames all bands in raster stacks using a user-supplied CSV file that maps
#' band position to a new label. The file must have one band name per row (in
#' band order), with an optional header. The number of names in the file must
#' match the number of bands in the rasters.
#'
#' @param rasterList List of SpatRasters.
#' @param bandLabelFile Path to a CSV file with one column of band names in order.
#'
#' @return List of SpatRasters with renamed bands.
#'
#' @noRd
overrideBandNames <- function(rasterList, bandLabelFile) {
  # Read band names from file
  bandLabels <- tryCatch(
    read.csv(bandLabelFile, header = TRUE, stringsAsFactors = FALSE),
    error = function(e) stop(paste0(
      "Could not read band label file '", bandLabelFile, "': ", conditionMessage(e)
    ))
  )
  newNames <- bandLabels[[1]]

  # Validate against first raster
  nBands <- terra::nlyr(rasterList[[1]])
  if (length(newNames) != nBands) {
    stop(paste0(
      "Band label file has ", length(newNames), " entries but rasters have ",
      nBands, " bands. The file must have one name per band."
    ))
  }

  # Log the renaming using first raster's original names as example
  oldNames <- names(rasterList[[1]])
  updateRunLog(
    paste0(
      "Renaming ", nBands, " bands using label file: ",
      paste(oldNames, "->", newNames, collapse = ", ")
    ),
    type = "info"
  )

  # Preallocate and carry over the timestep names of rasterList (see
  # normalizeRaster() for why the names matter downstream)
  renamedRasterList <- vector("list", length(rasterList))
  names(renamedRasterList) <- names(rasterList)

  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]
    names(raster) <- newNames
    renamedRasterList[[i]] <- raster
  }

  return(renamedRasterList)
}
