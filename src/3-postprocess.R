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
  # Get file paths
  predictedPresenceFilepath <- trainingOutputDataframe$PredictedUnfiltered[
    trainingOutputDataframe$Timestep == t
  ]

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
  # Get file paths
  classifiedPresenceFilepath <- predictingOutputDataframe$ClassifiedUnfiltered[
    predictingOutputDataframe$Timestep == t
  ]

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

if (length(summaryRows) > 0) {
  saveDatasheet(
    myScenario,
    data = do.call(rbind, summaryRows),
    name = "ecoClassify_SummaryOutput"
  )
}
