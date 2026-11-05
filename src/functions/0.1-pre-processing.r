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
  # Built from terra operations, which stream from disk, rather than by
  # pulling every cell into R with values(). The output is the same as the
  # earlier cell-by-cell version.
  parseValues <- function(x) {
    if (is.na(x) || nchar(trimws(x)) == 0) return(integer(0))
    as.integer(trimws(strsplit(x, ",")[[1]]))
  }

  lapply(groundTruthRasterList, function(raster) {
    layerNames <- names(raster)
    # Compare on the stored cell values, as values() did, not on category
    # labels
    if (any(terra::is.factor(raster))) levels(raster) <- NULL

    if (isTRUE(useTargetClass) && !is.na(targetClassValue)) {
      isTarget <- raster == targetClassValue
      bgVals <- parseValues(backgroundValues)

      if (length(bgVals) > 0) {
        # Background values -> 0 (taking precedence over the target, as
        # before), target -> 1, everything else NA
        result <- terra::ifel(
          raster %in% bgVals, 0, terra::ifel(isTarget, 1, NA)
        )
      } else {
        # Target -> 1, ignored values -> NA, every other value -> 0
        ignoreVals <- parseValues(ignoreValues)
        result <- if (length(ignoreVals) > 0) {
          terra::ifel(isTarget, 1, terra::ifel(raster %in% ignoreVals, NA, 0))
        } else {
          terra::ifel(isTarget, 1, 0)
        }
      }
    } else {
      # Original binary logic: min -> 0, then max -> 1, where the max is
      # taken after the first step (so a single-valued raster becomes all 1,
      # and an all-negative raster's max can be the new 0). Values in
      # between are kept.
      rangeStats <- terra::global(raster, c("min", "max"), na.rm = TRUE)
      rasterMin <- suppressWarnings(min(rangeStats$min, na.rm = TRUE))
      rasterMax <- suppressWarnings(max(rangeStats$max, na.rm = TRUE))
      # All-NA raster: nothing to reclassify (the old version left it as is)
      if (!is.finite(rasterMin) || !is.finite(rasterMax)) return(raster)
      maxAfterMin <- if (rasterMin == rasterMax) 0 else max(rasterMax, 0)

      result <- terra::ifel(raster == rasterMin, 0, raster)
      result <- terra::ifel(result == maxAfterMin, 1, result)
    }

    # NA cells stay NA in every branch, as before (ifel() does not always
    # carry an NA condition through)
    result <- terra::mask(result, raster)
    names(result) <- layerNames
    result
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

    checkCovariateGeometry(rasterList[[i]], covRaster, timestepKey, i)

    combined <- c(rasterList[[i]], covRaster)

    # Only write the combined stack when a layer lives in a temporary file
    # that could be cleaned up before it is read (see Details). When every
    # layer is already a persistent file, or in memory, the stacked
    # SpatRaster refers to the sources directly.
    if (!hasTemporarySources(combined)) {
      rasterList[[i]] <- combined
      next
    }

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

#' Check a Covariate Raster Matches its Raster's Grid ----
#'
#' @description
#' Stops with a message naming the timestep and the property that differs
#' (CRS, extent, resolution or dimensions) instead of terra's generic
#' "extents do not match" error from `c()`.
#'
#' @noRd
checkCovariateGeometry <- function(raster, covRaster, timestepKey = NULL,
                                   index = NULL) {
  if (terra::compareGeom(raster, covRaster, stopOnError = FALSE)) {
    return(invisible(TRUE))
  }

  problems <- c(
    if (!terra::same.crs(raster, covRaster)) "CRS",
    if (!isTRUE(all.equal(as.vector(terra::ext(raster)),
                          as.vector(terra::ext(covRaster))))) "extent",
    if (!isTRUE(all.equal(terra::res(raster), terra::res(covRaster)))) {
      "resolution"
    },
    if (terra::nrow(raster) != terra::nrow(covRaster) ||
          terra::ncol(raster) != terra::ncol(covRaster)) "dimensions"
  )
  if (length(problems) == 0) problems <- "grid"

  label <- if (!is.null(timestepKey) && !is.na(timestepKey) &&
                 nzchar(timestepKey)) timestepKey else index
  stop(
    "Covariate raster(s) for timestep ", label, " do not match the image ",
    "raster's ", paste(problems, collapse = ", "), ". Covariates must share ",
    "the CRS, extent and resolution of the rasters they are added to."
  )
}

#' Test Whether a SpatRaster Reads From Temporary Files ----
#'
#' @description
#' TRUE if any source file is under R's or terra's temporary directory.
#' In-memory layers (empty source) count as persistent.
#'
#' @noRd
hasTemporarySources <- function(r) {
  sourceFiles <- terra::sources(r)
  sourceFiles <- sourceFiles[nzchar(sourceFiles)]
  if (length(sourceFiles) == 0) return(FALSE)

  normalizeDir <- function(path) {
    tolower(gsub("\\\\", "/", normalizePath(path, mustWork = FALSE)))
  }
  # `print = FALSE` is not available in older terra versions
  terraOpts <- tryCatch(
    terra::terraOptions(print = FALSE),
    error = function(e) terra::terraOptions()
  )
  tempDirs <- unique(normalizeDir(c(tempdir(), terraOpts$tempdir)))
  sourceFiles <- normalizeDir(sourceFiles)

  any(vapply(tempDirs, function(d) {
    any(startsWith(sourceFiles, paste0(sub("/$", "", d), "/")))
  }, logical(1)))
}

#' Compute Band Normalization Statistics ----
#'
#' @description
#' Computes one min and max per band, pooled across every raster in the list,
#' using terra's on-disk `global()` (no values are pulled into R memory).
#' Training computes these once and saves them with the model so prediction
#' rescales bands on exactly the same scale.
#'
#' @param rasterList List of SpatRasters with identical band names.
#'
#' @return A list with named numeric vectors `min` and `max` (one per band).
#'
#' @noRd
computeNormalizationStats <- function(rasterList) {
  perRaster <- lapply(rasterList, function(r) {
    terra::global(r, c("min", "max"), na.rm = TRUE)
  })
  bandNames <- rownames(perRaster[[1]])
  mins <- do.call(cbind, lapply(perRaster, function(x) x[bandNames, "min"]))
  maxs <- do.call(cbind, lapply(perRaster, function(x) x[bandNames, "max"]))

  list(
    min = stats::setNames(apply(mins, 1, min, na.rm = TRUE), bandNames),
    max = stats::setNames(apply(maxs, 1, max, na.rm = TRUE), bandNames)
  )
}

#' Normalize All Bands in Raster List ----
#'
#' @description
#' Rescales each band to 0-1 using the supplied `normalizationStats` (from
#' `computeNormalizationStats()`, normally saved with the model at training
#' time). Bands are matched by name. Values outside the training range fall
#' outside 0-1, which is intended: the model sees them on the training scale.
#'
#' @param rasterList Named list of SpatRasters.
#' @param normalizationStats List with named `min` and `max` vectors.
#'
#' @return List of normalized SpatRasters, retaining the names of `rasterList`.
#'
#' @noRd
normalizeRaster <- function(rasterList, normalizationStats) {
  statMin <- normalizationStats$min
  statMax <- normalizationStats$max

  # Constant bands would divide by zero; leave them unscaled apart from the
  # offset so they become 0 instead of NaN.
  statRange <- statMax - statMin
  statRange[!is.finite(statRange) | statRange == 0] <- 1

  # Preallocate and carry over the timestep names of rasterList; downstream
  # functions such as addCovariates() match rasters to per-timestep covariates
  # by name, so dropping the names silently discards covariate data.
  normalizedRasterList <- vector("list", length(rasterList))
  names(normalizedRasterList) <- names(rasterList)

  for (i in seq_along(rasterList)) {
    raster <- rasterList[[i]]
    bandNames <- names(raster)

    missingBands <- setdiff(bandNames, names(statMin))
    if (length(missingBands) > 0) {
      stop(
        "Cannot normalize raster: no training normalization statistics for ",
        "band(s) ", paste(missingBands, collapse = ", "), ". Band names must ",
        "match those used to train the model."
      )
    }

    # Vectorised over layers: one lazy terra expression for the whole stack
    normalizedRasterList[[i]] <- (raster - statMin[bandNames]) /
      statRange[bandNames]
    names(normalizedRasterList[[i]]) <- bandNames
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

    # Built-in "notNA" counts in C++; an R function here is called per block
    cellCounts <- terra::global(raster, fun = "notNA")
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

#' Validate the Model Object Datasheet Before Prediction ----
#'
#' @description
#' Checks that a trained model exists for the scenario and that its files are
#' present, so prediction fails early with a clear message rather than inside
#' `readRDS()` or `torch_load()`.
#'
#' @param modelObjectDataframe The `ecoClassify_ModelObject` datasheet.
#' @param modelType Character; the model type selected in ClassifierOptions.
#'
#' @return Invisibly TRUE; stops with an informative error otherwise.
#'
#' @noRd
validateModelObject <- function(modelObjectDataframe, modelType) {
  if (is.null(modelObjectDataframe) || nrow(modelObjectDataframe) == 0) {
    stop(
      "No trained model was found for this scenario. Run 1-Train Classifier ",
      "first, or add the training scenario as a dependency."
    )
  }
  if (nrow(modelObjectDataframe) > 1) {
    updateRunLog(
      paste0(
        "More than one trained model was found; using the first. Check the ",
        "scenario's dependencies if this is unexpected."
      ),
      type = "warning"
    )
  }

  isMissingFile <- function(path) {
    length(path) == 0 || is.na(path) || !nzchar(path) || !file.exists(path)
  }

  modelPath <- modelObjectDataframe$Model[1]
  if (isMissingFile(modelPath)) {
    stop("The trained model file could not be found: ", modelPath)
  }
  if (modelType == "CNN" && isMissingFile(modelObjectDataframe$Weights[1])) {
    stop(
      "Model type is CNN but the trained model has no CNN weights file. ",
      "Check that 'Model Type' matches the model that was trained."
    )
  }

  invisible(TRUE)
}

#' Check a Loaded Model Matches the Selected Model Type ----
#'
#' @param model A loaded model list (from `readRDS()` or `loadCNNModel()`).
#' @param modelType Character; the model type selected in ClassifierOptions.
#'
#' @return Invisibly TRUE; stops with an informative error otherwise.
#'
#' @noRd
checkModelType <- function(model, modelType) {
  fitted <- model$model
  trainedType <- if (inherits(fitted, "ranger")) {
    "Random Forest"
  } else if (inherits(fitted, "torchCNN")) {
    "CNN"
  } else {
    "MaxEnt"
  }
  if (!identical(trainedType, modelType)) {
    stop(
      "Model type is set to '", modelType, "' but the trained model is a ",
      trainedType, " model. Set 'Model Type' to match the trained model, or ",
      "retrain."
    )
  }
  invisible(TRUE)
}

#' Check Predicting Rasters Contain the Model's Predictors ----
#'
#' @description
#' Compares the layer names of each predicting raster (after band renaming,
#' contextualization and covariates) with the predictors the model was trained
#' on, and stops listing any that are missing.
#'
#' @param rasterList List of SpatRasters to predict on.
#' @param model A loaded model list with `cat_vars` and `num_vars`.
#' @param timesteps Optional timestep labels for messages.
#'
#' @return Invisibly TRUE; stops with an informative error otherwise.
#'
#' @noRd
checkPredictorNames <- function(rasterList, model, timesteps = NULL) {
  expected <- c(model$cat_vars, model$num_vars)
  if (length(expected) == 0) return(invisible(TRUE))

  for (i in seq_along(rasterList)) {
    missingVars <- setdiff(expected, names(rasterList[[i]]))
    if (length(missingVars) > 0) {
      label <- if (!is.null(timesteps)) timesteps[i] else i
      stop(
        "Predicting raster for timestep ", label, " is missing predictor(s) ",
        "used to train the model: ", paste(missingVars, collapse = ", "),
        ". Band names, covariates and pre-processing options must match ",
        "the training scenario."
      )
    }
  }
  invisible(TRUE)
}

#' Write the Spatial Tiling Mask Raster ----
#'
#' @description
#' Writes the TileID mask used by SyncroSim spatial multiprocessing, block by
#' block, so memory holds one block of cells rather than the whole grid. Each
#' grid cell (tile row `iy`, tile column `ix`) covers rows
#' `rowBreaks[iy] + 1` to `rowBreaks[iy + 1]` and the matching columns.
#'
#' @param template SpatRaster defining the full grid (values are not used).
#' @param tileIdGrid Integer matrix (numTilesY x numTilesX) of tile IDs, NA
#'   for skipped tiles.
#' @param rowBreaks,colBreaks Cumulative row/column breakpoints, starting at 0.
#' @param filename Output GeoTIFF path.
#'
#' @return The written SpatRaster.
#'
#' @noRd
writeTileMask <- function(template, tileIdGrid, rowBreaks, colBreaks,
                          filename) {
  out <- terra::rast(template, nlyrs = 1)
  names(out) <- "TileID"

  # Tile row of every raster row, and tile column of every raster column
  tileColOfCol <- findInterval(seq_len(terra::ncol(out)) - 1, colBreaks)
  rowPatterns <- tileIdGrid[, tileColOfCol, drop = FALSE]

  blockInfo <- terra::writeStart(
    out, filename,
    overwrite = TRUE, datatype = "INT4S", NAflag = -9999L,
    gdal = c("COMPRESS=LZW")
  )
  on.exit(try(terra::writeStop(out), silent = TRUE), add = TRUE)

  for (b in seq_len(blockInfo$n)) {
    rows <- blockInfo$row[b] + seq_len(blockInfo$nrows[b]) - 1L
    tileRowOfRow <- findInterval(rows - 1, rowBreaks)
    vals <- as.integer(t(rowPatterns[tileRowOfRow, , drop = FALSE]))
    terra::writeValues(out, vals, blockInfo$row[b], blockInfo$nrows[b])
  }
  out <- terra::writeStop(out)
  on.exit()

  terra::rast(filename)
}

#' Crop Rasters to a Spatial Tile ----
#'
#' @description
#' Used by tile jobs in 2b-predict.R: crops each predicting raster to the
#' tile's (buffered) extent, reading only that part of the source file. The
#' list's timestep names are kept.
#'
#' @param rasterList Named list of SpatRasters.
#' @param cropExtent SpatExtent to crop to.
#'
#' @return Named list of cropped SpatRasters.
#'
#' @noRd
cropRastersToTile <- function(rasterList, cropExtent) {
  croppedList <- lapply(rasterList, function(r) terra::crop(r, cropExtent))
  names(croppedList) <- names(rasterList)
  croppedList
}

#' Parse the TargetClassOptions Datasheet ----
#'
#' @description
#' Reads the first row of `ecoClassify_TargetClassOptions` into a list, with
#' NA (or FALSE) for anything missing. Shared by every transformer.
#'
#' @param targetClassSheet The datasheet as a data frame (may have no rows).
#'
#' @return A list with `useTargetClass`, `targetClassValue`,
#'   `backgroundValues`, `ignoreValues` and `targetClassLabel`.
#'
#' @noRd
parseTargetClassOptions <- function(targetClassSheet) {
  firstValue <- function(column, convert) {
    value <- targetClassSheet[[column]]
    if (nrow(targetClassSheet) == 0 || is.null(value) || is.na(value[1])) {
      return(convert(NA))
    }
    convert(value[1])
  }

  list(
    useTargetClass = nrow(targetClassSheet) > 0 &&
      isTRUE(as.logical(targetClassSheet$useTargetClass[1])),
    targetClassValue = firstValue("targetClassValue", as.integer),
    backgroundValues = firstValue("backgroundValues", as.character),
    ignoreValues = firstValue("ignoreValues", as.character),
    targetClassLabel = firstValue("targetClassLabel", as.character)
  )
}

#' Parse the Terminology Datasheet ----
#'
#' @description
#' Reads the band label file and RGB band choices from the first row of
#' `ecoClassify_Terminology`.
#'
#' @param terminologyDataframe The datasheet as a data frame.
#'
#' @return A list with `bandLabelFile` (path or NULL) and `rgbBands` (a list
#'   of `red`, `green`, `blue`, or NULL unless all three are set).
#'
#' @noRd
parseTerminology <- function(terminologyDataframe) {
  hasRow <- nrow(terminologyDataframe) > 0
  isSet <- function(column) {
    hasRow && !is.null(terminologyDataframe[[column]]) &&
      !is.na(terminologyDataframe[[column]][1])
  }

  bandLabelFile <- if (isSet("bandNames") &&
                         nchar(terminologyDataframe$bandNames[1]) > 0) {
    terminologyDataframe$bandNames[1]
  } else {
    NULL
  }

  rgbBands <- if (isSet("redBand") && isSet("greenBand") &&
                    isSet("blueBand")) {
    list(
      red   = terminologyDataframe$redBand[1],
      green = terminologyDataframe$greenBand[1],
      blue  = terminologyDataframe$blueBand[1]
    )
  } else {
    NULL
  }

  list(bandLabelFile = bandLabelFile, rgbBands = rgbBands)
}
