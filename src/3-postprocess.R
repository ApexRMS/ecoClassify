## ecoClassify - Filtering training or predicting steps
## ApexRMS, November 2024

# Set up workspace -------------------------------------------------------------

packageDir <- (Sys.getenv("ssim_package_directory"))

source(file.path(packageDir, "installDependencies.r"))

sourceScripts <- list.files(
  path = file.path(packageDir, "/functions"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

invisible(lapply(sourceScripts, source))

progressBar(
  type = "message",
  message = "Loading input data and setting up scenario"
)

# Get the SyncroSim Scenario that is currently running
myScenario <- scenario()

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Set terra options to minimize memory use ----
terra_tmp_dir <- file.path(transferDir, "terra_tmp")
dir.create(terra_tmp_dir, showWarnings = FALSE, recursive = TRUE)
terraOptions(
  todisk = TRUE,
  memfrac = 0.6,
  tempdir = terra_tmp_dir
)
wopt_int <- list(
  datatype = "INT2S",
  NAflag = -32768,
  gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES")
)
# Load post-processing settings ------------------------------------------------

### Filtering -----------------------------------------------------

postProcessingDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_PostProcessingFilter"
)

filterValue <- postProcessingDataframe$filterValue
fillValue <- postProcessingDataframe$fillValue
applyFiltering <- postProcessingDataframe$applyFiltering

# Apply default filtering values if not specified
if (dim(postProcessingDataframe)[1] != 0) {
  if (is.na(filterValue) && applyFiltering == TRUE) {
    filterValue <- 8
    updateRunLog(
      "Number of neighbours for filtering was not supplied; using default value of 8",
      type = "info"
    )
  }
  if (is.na(fillValue) && applyFiltering == TRUE) {
    fillValue <- 8
    updateRunLog(
      "Number of neighbours for filling was not supplied; using default value of 8",
      type = "info"
    )
  }
}


### Rule-based reclassification -----------------------------------

ruleReclassDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_PostProcessingRule"
)


### TargetClassOptions -------------------------------------------

targetClassSheet <- datasheet(myScenario, "ecoClassify_TargetClassOptions")
targetClassValue <- NA_integer_
targetClassLabel <- NA_character_
if (nrow(targetClassSheet) > 0) {
  if (!is.null(targetClassSheet$targetClassValue) && isTRUE(!is.na(targetClassSheet$targetClassValue[1]))) targetClassValue <- as.integer(targetClassSheet$targetClassValue[1])
  if (!is.null(targetClassSheet$targetClassLabel) && isTRUE(!is.na(targetClassSheet$targetClassLabel[1]))) targetClassLabel <- as.character(targetClassSheet$targetClassLabel[1])
}


### Output datasheets ---------------------------------------------

trainingOutputDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_RasterOutput"
)

predictingOutputDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_ClassifiedRasterOutput"
)


### Existing summary output (to be preserved and overwritten) -----

existingSummaryOutput <- datasheet(myScenario, name = "ecoClassify_SummaryOutput")
# Keep only the schema-defined columns to avoid internal rsyncosim columns causing rbind failures
summaryOutputSchemaCols <- c(
  "Timestep", "TargetClassValue", "TargetClassLabel", "PredictionType",
  "PredictedPixels", "PredictedArea", "MeanProbability", "MedianProbability",
  "MinProbability", "MaxProbability"
)
if (nrow(existingSummaryOutput) > 0) {
  existingSummaryOutput <- existingSummaryOutput[,
    intersect(names(existingSummaryOutput), summaryOutputSchemaCols), drop = FALSE
  ]
}
postProcessingTypes <- c("filtered", "restricted", "filtered_restricted")
if (nrow(existingSummaryOutput) > 0 && "PredictionType" %in% names(existingSummaryOutput)) {
  existingSummaryOutput <- existingSummaryOutput[
    !existingSummaryOutput$PredictionType %in% postProcessingTypes, , drop = FALSE
  ]
}

# Aggregate per-tile "predicting" rows into one row per timestep.
# When spatial tiling is used, 2-predict.r writes one summary row per tile to
# SummaryOutput. Collapse those into a single row: PredictedPixels and
# PredictedArea are summed; all probability columns are averaged across tiles.
# This is a safety net — tile job 1 already attempts this in 2-predict.r, but
# running post-processing guarantees a clean aggregated result.
if (nrow(existingSummaryOutput) > 0 &&
    "PredictionType" %in% names(existingSummaryOutput) &&
    any(existingSummaryOutput$PredictionType == "predicting", na.rm = TRUE)) {

  .predRows    <- existingSummaryOutput[existingSummaryOutput$PredictionType == "predicting", , drop = FALSE]
  .nonPredRows <- existingSummaryOutput[existingSummaryOutput$PredictionType != "predicting", , drop = FALSE]

  .sum_na  <- function(x) if (all(is.na(x))) NA_real_ else sum(x,  na.rm = TRUE)
  .mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

  .aggregated <- do.call(rbind, lapply(
    split(.predRows, .predRows$Timestep),
    function(grp) {
      data.frame(
        Timestep          = grp$Timestep[1],
        TargetClassValue  = if ("TargetClassValue"  %in% names(grp)) grp$TargetClassValue[1]  else NA,
        TargetClassLabel  = if ("TargetClassLabel"  %in% names(grp)) grp$TargetClassLabel[1]  else NA,
        PredictionType    = "predicting",
        PredictedPixels   = if ("PredictedPixels"   %in% names(grp)) .sum_na(grp$PredictedPixels)    else NA_real_,
        PredictedArea     = if ("PredictedArea"     %in% names(grp)) .sum_na(grp$PredictedArea)      else NA_real_,
        MeanProbability   = if ("MeanProbability"   %in% names(grp)) .mean_na(grp$MeanProbability)   else NA_real_,
        MedianProbability = if ("MedianProbability" %in% names(grp)) .mean_na(grp$MedianProbability) else NA_real_,
        MinProbability    = if ("MinProbability"    %in% names(grp)) .mean_na(grp$MinProbability)    else NA_real_,
        MaxProbability    = if ("MaxProbability"    %in% names(grp)) .mean_na(grp$MaxProbability)    else NA_real_,
        stringsAsFactors  = FALSE
      )
    }
  ))
  rownames(.aggregated) <- NULL
  existingSummaryOutput <- rbind(.nonPredRows, .aggregated)
}


### Unique timesteps ----------------------------------------------

trainingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputTrainingRasters"
)
predictingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputPredictingRasters"
)

trainTimestepList <- trainingRasterDataframe %>%
  filter(!is.na(TrainingRasterFile)) %>%
  pull(Timesteps) %>%
  unique()
predTimestepList <- predictingRasterDataframe %>%
  filter(!is.na(predictingRasterFile)) %>%
  pull(Timesteps) %>%
  unique()


# Filter raster ----------------------------------------------------------------

progressBar(type = "message", message = "Filtering")

summaryRows <- list()

### Training step -------------------------------------------------

for (t in trainTimestepList) {
  # Get file paths (take first match in case of duplicate timestep rows)
  predictedPresenceFilepath <- trainingOutputDataframe$PredictedUnfiltered[
    trainingOutputDataframe$Timestep == t
  ][1]

  if (!is.na(predictedPresenceFilepath)) {
    # Load raster
    predictedPresence <- rast(predictedPresenceFilepath)

    # Filter
    filteredTraining <- filterRasterDataframe(
      applyFiltering,
      predictedPresence,
      filterValue,
      fillValue,
      "training",
      t,
      transferDir,
      fileName = "filteredPredictedPresence"
    )

    # Combine results
    trainingOutputDataframe$PredictedFiltered[
      trainingOutputDataframe$Timestep == t
    ] <- filteredTraining$PredictedFiltered

    # Accumulate filtered summary row
    if (isTRUE(applyFiltering) && !is.na(filteredTraining$PredictedFiltered)) {
      probPath <- trainingOutputDataframe$Probability[trainingOutputDataframe$Timestep == t]
      if (!is.na(probPath) && file.exists(probPath)) {
        summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
          predictionRaster  = terra::rast(filteredTraining$PredictedFiltered),
          probabilityRaster = terra::rast(probPath),
          timestep          = t,
          predictionType    = "filtered",
          targetClassValue  = targetClassValue,
          targetClassLabel  = targetClassLabel
        )
      }
    }
  }
}

### Predicting step -----------------------------------------------

for (t in predTimestepList) {
  # Get file paths (take first match in case of duplicate timestep rows)
  classifiedPresenceFilepath <- predictingOutputDataframe$ClassifiedUnfiltered[
    predictingOutputDataframe$Timestep == t
  ][1]

  if (!is.na(classifiedPresenceFilepath) && file.exists(classifiedPresenceFilepath)) {
    # Load raster
    predictedPresence <- rast(classifiedPresenceFilepath)

    # Filter
    filteredPredicting <- filterRasterDataframe(
      applyFiltering,
      predictedPresence,
      filterValue,
      fillValue,
      "predicting",
      t,
      transferDir,
      fileName = "filteredPredictedPresence"
    )

    # Combine results
    predictingOutputDataframe$ClassifiedFiltered[
      predictingOutputDataframe$Timestep == t
    ] <- filteredPredicting$PredictedFiltered

    # Accumulate filtered summary row
    if (isTRUE(applyFiltering) && !is.na(filteredPredicting$PredictedFiltered)) {
      probPath <- predictingOutputDataframe$ClassifiedProbability[predictingOutputDataframe$Timestep == t]
      if (!is.na(probPath) && file.exists(probPath)) {
        summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
          predictionRaster  = terra::rast(filteredPredicting$PredictedFiltered),
          probabilityRaster = terra::rast(probPath),
          timestep          = t,
          predictionType    = "filtered",
          targetClassValue  = targetClassValue,
          targetClassLabel  = targetClassLabel
        )
      }
    }
  }
}


# Reclassify probability raster ------------------------------------------------

progressBar(type = "message", message = "Reclassifying")

if (nrow(ruleReclassDataframe) != 0) {
  restrictedTmpDir <- file.path(transferDir, "restrictedTmpDir")
  dir.create(restrictedTmpDir, showWarnings = FALSE, recursive = TRUE)

  # ---- Apply Reclassification Rules (training: unfiltered + filtered) ----
  if (length(trainTimestepList) > 0) {
    for (t in trainTimestepList) {

      # Get file paths
      unfilteredTrainFilepath <- trainingOutputDataframe$PredictedUnfiltered[
        trainingOutputDataframe$Timestep == t
      ]

      # Skip if no unfiltered path
      if (length(unfilteredTrainFilepath) == 0 ||
            is.na(unfilteredTrainFilepath) ||
            !file.exists(unfilteredTrainFilepath)) next

      # Load base rasters
      unfiltered <- rast(unfilteredTrainFilepath)

      # Disk-backed working copies
      reclassedUnf <- writeRaster(
        unfiltered, filename = tempfile(fileext = ".tif"),
        overwrite = TRUE, wopt = wopt_int
      )

      # Rules
      for (i in seq_len(nrow(ruleReclassDataframe))) {

        # Disable disk-backing for this iteration
        old_todisk <- terraOptions()$todisk
        terraOptions(todisk = FALSE)

          # Load rule raster
          rulePath <- ruleReclassDataframe$ruleRasterFile[i]
          if (length(rulePath) == 0 || is.na(rulePath) || !file.exists(rulePath)) {
            updateRunLog(
              paste0("Rule raster missing for rule index ", i, "; skipping."),
              type = "warning"
            )
            # Restore before next
            terraOptions(todisk = old_todisk)
            next
          }
          ruleRaster <- rast(rulePath)

          # Ensure geometry match; skip (or resample here if desired)
          if (!compareGeom(ruleRaster, reclassedUnf, stopOnError = FALSE)) {
            updateRunLog(
              "Rule raster extent does not match unfiltered training raster extent. Skipping reclassification.",
              type = "warning"
            )
            # Restore before next
            terraOptions(todisk = old_todisk)
            next
          }

          vmin <- ruleReclassDataframe$ruleMinValue[i]
          vmax <- ruleReclassDataframe$ruleMaxValue[i]
          rval <- as.numeric(ruleReclassDataframe$ruleReclassValue[i]) - 1

          if (any(is.na(c(vmin, vmax, rval)))) {
            updateRunLog(
              paste0("Rule values (min/max/reclass) contain NA for rule index ", i, "; skipping."),
              type = "warning"
            )
            # Restore before next
            terraOptions(todisk = old_todisk)
            next
          }
          if (vmin > vmax) {
            tmp <- vmin; vmin <- vmax; vmax <- tmp
            updateRunLog(
              paste0("Swapped vmin/vmax for rule index ", i, " to maintain [min,max]."),
              type = "info"
            )
          }

          if (isTRUE(vmin == vmax)) {
            # ---------- CATEGORICAL: apply where ruleRaster == vmin ----------
            tmpUnf <- tempfile(fileext = ".tif")
            reclassedUnf <- ifel(
              ruleRaster == vmin,
              rval,
              reclassedUnf,
              filename = tmpUnf, overwrite = TRUE
            )

          } else {
            # ---------- CONTINUOUS: classify range [vmin, vmax] ----------
            rtab <- matrix(c(vmin, vmax, rval), ncol = 3, byrow = TRUE)
            classedMask <- classify(
              ruleRaster, rtab, others = NA,
              filename = tempfile(fileext = ".tif"), overwrite = TRUE,
              right = FALSE, include.lowest = TRUE
            )

            # Update where mask is not NA
            tmpUnf <- tempfile(fileext = ".tif")
            reclassedUnf <- ifel(
              !is.na(classedMask),
              rval,
              reclassedUnf,
              filename = tmpUnf, overwrite = TRUE
            )

            rm(classedMask); gc()
          }

          # Restore setting at end of iteration
          terraOptions(todisk = old_todisk)
          gc()
        }

      # ---- Final writes for timestep t (restricted) ----
      reclassedPathTrain <- file.path(
        transferDir,
        paste0("PredictedPresenceRestricted-","training","-t",t,".tif")
      )
      reclassedUnf <- writeRaster(
        reclassedUnf, filename = reclassedPathTrain,
        overwrite = TRUE, wopt = wopt_int
      )
      trainingOutputDataframe$PredictedUnfilteredRestricted[
        trainingOutputDataframe$Timestep == t
      ] <- reclassedPathTrain

      # Accumulate restricted summary row
      probPath <- trainingOutputDataframe$Probability[trainingOutputDataframe$Timestep == t]
      if (!is.na(probPath) && file.exists(probPath)) {
        summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
          predictionRaster  = terra::rast(reclassedPathTrain),
          probabilityRaster = terra::rast(probPath),
          timestep          = t,
          predictionType    = "restricted",
          targetClassValue  = targetClassValue,
          targetClassLabel  = targetClassLabel
        )
      }

      # ---- filter after reclassification to create FilteredRestricted ----
      if (isTRUE(applyFiltering)) {
        filteredRestricted <- filterRasterDataframe(
          applyFiltering,
          rast(reclassedPathTrain),
          filterValue,
          fillValue,
          "training",
          t,
          restrictedTmpDir,
          fileName = "PredictedPresenceFilteredRestricted"
        )

        reclassedFilteredPathTrain <- file.path(
          transferDir,
          paste0("PredictedPresenceFilteredRestricted-","training","-t",t,".tif")
        )

        writeRaster(
          rast(filteredRestricted$PredictedFiltered),
          filename = reclassedFilteredPathTrain,
          overwrite = TRUE, wopt = wopt_int
        )

        trainingOutputDataframe$PredictedFilteredRestricted[
          trainingOutputDataframe$Timestep == t
        ] <- reclassedFilteredPathTrain

        # Accumulate filtered_restricted summary row
        if (!is.na(probPath) && file.exists(probPath)) {
          summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
            predictionRaster  = terra::rast(reclassedFilteredPathTrain),
            probabilityRaster = terra::rast(probPath),
            timestep          = t,
            predictionType    = "filtered_restricted",
            targetClassValue  = targetClassValue,
            targetClassLabel  = targetClassLabel
          )
        }
      }

      # Cleanup per-timestep
      rm(unfiltered, reclassedUnf); gc()

    }
  }

  # ---- Apply Reclassification Rules (predicting: unfiltered + filtered) ----
  if (length(predTimestepList) > 0) {
    for (t in predTimestepList) {

      # Get file paths
      unfilteredPredFilepath <- predictingOutputDataframe$ClassifiedUnfiltered[
        predictingOutputDataframe$Timestep == t
      ]

      # Skip if no unfiltered path
      if (length(unfilteredPredFilepath) == 0 ||
            is.na(unfilteredPredFilepath) ||
            !file.exists(unfilteredPredFilepath)) next

      # Load rasters
      unfiltered <- rast(unfilteredPredFilepath)

      # Start disk-backed working copies
      reclassedUnf <- writeRaster(
        unfiltered, filename = tempfile(fileext = ".tif"),
        overwrite = TRUE, wopt = wopt_int
      )

      # Apply rules (if any)
      for (i in seq_len(nrow(ruleReclassDataframe))) {

        # Disable disk-backing for this iteration
        old_todisk <- terraOptions()$todisk
        terraOptions(todisk = FALSE)

          # Load rule raster
          rulePath <- ruleReclassDataframe$ruleRasterFile[i]
          if (length(rulePath) == 0 || is.na(rulePath) || !file.exists(rulePath)) {
            updateRunLog(
              paste0("Rule raster missing for rule index ", i, "; skipping."),
              type = "warning"
            )
            # Restore before next
            terraOptions(todisk = old_todisk)
            next
          }
          ruleRaster <- rast(rulePath)

          # Ensure same geometry; otherwise skip (or resample if you prefer)
          if (!compareGeom(ruleRaster, reclassedUnf, stopOnError = FALSE)) {
            updateRunLog(
              "Rule raster extent does not match predicting raster extent. Skipping reclassification.",
              type = "warning"
            )
            # Restore before next
            terraOptions(todisk = old_todisk)
            next
          }

          vmin <- ruleReclassDataframe$ruleMinValue[i]
          vmax <- ruleReclassDataframe$ruleMaxValue[i]
          rval <- as.numeric(ruleReclassDataframe$ruleReclassValue[i]) - 1

          if (any(is.na(c(vmin, vmax, rval)))) {
            updateRunLog(
              paste0("Rule values (min/max/reclass) contain NA for rule index ", i, "; skipping."),
              type = "warning"
            )
            # Restore before next
            terraOptions(todisk = old_todisk)
            next
          }
          if (vmin > vmax) {
            tmp <- vmin; vmin <- vmax; vmax <- tmp
            updateRunLog(
              paste0("Swapped vmin/vmax for rule index ", i, " to maintain [min,max]."),
              type = "info"
            )
          }

          if (isTRUE(vmin == vmax)) {
            # ---------- CATEGORICAL: apply where ruleRaster == vmin ----------
            tmpUnf <- tempfile(fileext = ".tif")
            reclassedUnf <- ifel(
              ruleRaster == vmin,
              rval,                 # set to rval where condition true
              reclassedUnf,         # otherwise keep
              filename = tmpUnf, overwrite = TRUE
            )

          } else {
            # ---------- CONTINUOUS: classify range [vmin, vmax] ----------
            rtab <- matrix(c(vmin, vmax, rval), ncol = 3, byrow = TRUE)
            classedMask <- classify(
              ruleRaster, rtab, others = NA,
              filename = tempfile(fileext = ".tif"), overwrite = TRUE,
              right = FALSE, include.lowest = TRUE
            )

            # Update where mask is not NA (i.e., cells under the rule)
            tmpUnf <- tempfile(fileext = ".tif")
            reclassedUnf <- ifel(
              !is.na(classedMask),
              rval,
              reclassedUnf,
              filename = tmpUnf, overwrite = TRUE
            )

            rm(classedMask); gc()
          }

          # Restore setting at end of iteration
          terraOptions(todisk = old_todisk)
          gc()
        }

      # ---- Final writes for timestep t ----
      reclassedPathPred <- file.path(
        transferDir,
        paste0("PredictedPresenceRestricted-","predicting","-t",t,".tif")
      )
      reclassedUnf <- writeRaster(
        reclassedUnf, filename = reclassedPathPred,
        overwrite = TRUE, wopt = wopt_int
      )
      predictingOutputDataframe$ClassifiedUnfilteredRestricted[
        predictingOutputDataframe$Timestep == t
      ] <- reclassedPathPred

      # Accumulate restricted summary row
      predProbPath <- predictingOutputDataframe$ClassifiedProbability[predictingOutputDataframe$Timestep == t]
      if (!is.na(predProbPath) && file.exists(predProbPath)) {
        summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
          predictionRaster  = terra::rast(reclassedPathPred),
          probabilityRaster = terra::rast(predProbPath),
          timestep          = t,
          predictionType    = "restricted",
          targetClassValue  = targetClassValue,
          targetClassLabel  = targetClassLabel
        )
      }

      # ---- filter after reclassification to create FilteredRestricted ----
      if (isTRUE(applyFiltering)) {
        filteredRestricted <- filterRasterDataframe(
          applyFiltering,
          rast(reclassedPathPred),
          filterValue,
          fillValue,
          "predicting",
          t,
          restrictedTmpDir,
          fileName = "PredictedPresenceFilteredRestricted"
        )

        reclassedFilteredPathPred <- file.path(
          transferDir,
          paste0("PredictedPresenceFilteredRestricted-","predicting","-t",t,".tif")
        )

        writeRaster(
          rast(filteredRestricted$PredictedFiltered),
          filename = reclassedFilteredPathPred,
          overwrite = TRUE, wopt = wopt_int
        )

        predictingOutputDataframe$ClassifiedFilteredRestricted[
          predictingOutputDataframe$Timestep == t
        ] <- reclassedFilteredPathPred

        # Accumulate filtered_restricted summary row
        if (!is.na(predProbPath) && file.exists(predProbPath)) {
          summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
            predictionRaster  = terra::rast(reclassedFilteredPathPred),
            probabilityRaster = terra::rast(predProbPath),
            timestep          = t,
            predictionType    = "filtered_restricted",
            targetClassValue  = targetClassValue,
            targetClassLabel  = targetClassLabel
          )
        }
      }

      # Cleanup per-timestep
      rm(unfiltered, reclassedUnf); gc()

    }
  }
} else {
  updateRunLog(
    "No rule-based reclassification rules found. Skipping reclassification.",
    type = "warning"
  )
}

# Save datasheets --------------------------------------------------------------

# Save datasheets back to SyncroSim
if (dim(trainingOutputDataframe)[1] != 0) {
  saveDatasheet(
    myScenario,
    data = trainingOutputDataframe,
    name = "ecoClassify_RasterOutput"
  )
}
if (dim(predictingOutputDataframe)[1] != 0) {
  saveDatasheet(
    myScenario,
    data = predictingOutputDataframe,
    name = "ecoClassify_ClassifiedRasterOutput"
  )
}

# Combine post-processing summary rows with preserved existing rows and overwrite
if (length(summaryRows) > 0) {
  newSummaryRows <- do.call(rbind, summaryRows)
  combinedSummary <- if (nrow(existingSummaryOutput) > 0) {
    dplyr::bind_rows(existingSummaryOutput, newSummaryRows)
  } else {
    newSummaryRows
  }
} else {
  combinedSummary <- existingSummaryOutput
}

if (nrow(combinedSummary) > 0) {
  saveDatasheet(
    myScenario,
    data = combinedSummary,
    name = "ecoClassify_SummaryOutput"
  )
}

# Recalculate model statistics and metrics from post-processed rasters ----------
if (length(trainTimestepList) > 0) {
  allTp <- 0; allTn <- 0; allFp <- 0; allFn <- 0

  # Helper: first non-NA, non-empty path from a possibly multi-row subset
  first_valid_path <- function(paths) {
    paths <- paths[!is.na(paths) & nzchar(paths)]
    if (length(paths) == 0) NA_character_ else paths[1]
  }

  for (t in trainTimestepList) {
    row_idx <- trainingOutputDataframe$Timestep == t

    frc_path  <- first_valid_path(trainingOutputDataframe$PredictedFilteredRestricted[row_idx])
    urc_path  <- first_valid_path(trainingOutputDataframe$PredictedUnfilteredRestricted[row_idx])
    flt_path  <- first_valid_path(trainingOutputDataframe$PredictedFiltered[row_idx])
    unf_path  <- first_valid_path(trainingOutputDataframe$PredictedUnfiltered[row_idx])
    truthPath <- first_valid_path(trainingOutputDataframe$GroundTruth[row_idx])

    predPath <- NA_character_
    if (!is.na(frc_path) && file.exists(frc_path)) {
      predPath <- frc_path
    } else if (!is.na(urc_path) && file.exists(urc_path)) {
      predPath <- urc_path
    } else if (!is.na(flt_path) && file.exists(flt_path)) {
      predPath <- flt_path
    } else if (!is.na(unf_path) && file.exists(unf_path)) {
      predPath <- unf_path
    }

    if (is.na(predPath) || is.na(truthPath) || !file.exists(truthPath)) next

    # Use terra::crosstab (long=TRUE) for disk-backed confusion matrix counts.
    # long=TRUE always returns a data.frame regardless of how many class
    # combinations are present, avoiding the 1-D table issue when a class
    # is absent from one of the rasters.
    ct <- tryCatch(
      terra::crosstab(c(terra::rast(truthPath), terra::rast(predPath)), long = TRUE),
      error = function(e) {
        updateRunLog(paste0("crosstab failed for timestep ", t, ": ", conditionMessage(e)), type = "warning")
        NULL
      }
    )
    if (is.null(ct)) next

    # Normalise column names: layer names may vary; always rename to truth/pred/Freq
    names(ct) <- c("truth", "pred", "Freq")
    ct$truth  <- as.numeric(as.character(ct$truth))
    ct$pred   <- as.numeric(as.character(ct$pred))

    get_ct <- function(truth_val, pred_val) {
      val <- ct$Freq[ct$truth == truth_val & ct$pred == pred_val]
      if (length(val) == 0 || is.na(val)) 0 else as.numeric(val)
    }
    allTp <- allTp + get_ct(1, 1)
    allTn <- allTn + get_ct(0, 0)
    allFp <- allFp + get_ct(0, 1)
    allFn <- allFn + get_ct(1, 0)
  }

  tp <- allTp; tn <- allTn; fp <- allFp; fn <- allFn
  n  <- tp + tn + fp + fn

  if (n > 0) {

    acc      <- (tp + tn) / n
    sens     <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    spec     <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    ppv      <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    npv      <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
    prec     <- ppv
    rec      <- sens
    f1       <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) 2 * prec * rec / (prec + rec) else NA_real_
    p_pos    <- (tp + fn) / n
    nir      <- max(p_pos, 1 - p_pos)
    pe       <- (((tp + fn) * (tp + fp)) + ((fp + tn) * (fn + tn))) / (n ^ 2)
    kappa_val <- if ((1 - pe) > 0) (acc - pe) / (1 - pe) else NA_real_
    bal_acc  <- if (!is.na(sens) && !is.na(spec)) (sens + spec) / 2 else NA_real_
    det_rate <- tp / n
    det_prev <- (tp + fp) / n

    # Wilson score CI — works correctly at raster-scale n where binom.test is
    # unreliable (requires integer inputs and is slow for very large counts)
    z95      <- qnorm(0.975)
    centre   <- acc + z95^2 / (2 * n)
    margin   <- z95 * sqrt(acc * (1 - acc) / n + z95^2 / (4 * n^2))
    denom    <- 1 + z95^2 / n
    acc_lower <- (centre - margin) / denom
    acc_upper <- (centre + margin) / denom

    # One-sided z-test: accuracy vs no-information rate
    acc_p <- pnorm((acc - nir) / sqrt(nir * (1 - nir) / n), lower.tail = FALSE)

    mcnemar_p <- tryCatch(
      stats::mcnemar.test(matrix(c(tn, fp, fn, tp), nrow = 2))$p.value,
      error = function(e) NA_real_
    )

    updatedModelOutputDataframe <- data.frame(
      Statistic = c(
        "Accuracy", "Kappa",
        "Accuracy (lower)", "Accuracy (upper)",
        "Accuracy (null)", "Accuracy P Value",
        "Mcnemar P value",
        "Sensitivity", "Specificity",
        "Positive Predictive Value", "Negative Predictive Value",
        "Precision", "Recall", "F1",
        "Prevalence", "Detection Rate",
        "Detection Prevalence", "Balanced Accuracy"
      ),
      Value = c(
        acc, kappa_val,
        acc_lower, acc_upper,
        nir, acc_p, mcnemar_p,
        sens, spec, ppv, npv,
        prec, rec, f1,
        p_pos, det_rate, det_prev, bal_acc
      ),
      stringsAsFactors = FALSE
    )

    saveDatasheet(
      myScenario,
      data = updatedModelOutputDataframe,
      name = "ecoClassify_ModelStatistics"
    )

    tryCatch({
      updatedMetricsRow <- buildMetricsRow(
        statsDataframe   = updatedModelOutputDataframe,
        targetClassValue = targetClassValue,
        targetClassLabel = targetClassLabel,
        auc              = NA_real_
      )
      saveDatasheet(
        myScenario,
        data = updatedMetricsRow,
        name = "ecoClassify_ModelMetricsByClass"
      )
    }, error = function(e) {
      updateRunLog(
        paste0("Could not build model metrics row: ", conditionMessage(e)),
        type = "warning"
      )
    })
  } else {
    updateRunLog(
      "No valid confusion matrix counts found for training rasters; model statistics and metrics will not be updated.",
      type = "warning"
    )
  }
}
