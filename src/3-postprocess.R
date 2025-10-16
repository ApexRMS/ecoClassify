## ecoClassify - Filtering training or predicting steps
## ApexRMS, November 2024

# Set up workspace -------------------------------------------------------------

packageDir <- (Sys.getenv("ssim_package_directory"))

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
      transferDir
    )

    # Combine results
    trainingOutputDataframe$PredictedFiltered[
      trainingOutputDataframe$Timestep == t
    ] <- filteredTraining$PredictedFiltered
  }
}

### Predicting step -----------------------------------------------

for (t in predTimestepList) {
  # Get file paths
  classifiedPresenceFilepath <- predictingOutputDataframe$ClassifiedUnfiltered[
    predictingOutputDataframe$Timestep == t
  ]

  if (!is.null(classifiedPresenceFilepath)) {
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
      transferDir
    )

    # Combine results
    predictingOutputDataframe$ClassifiedFiltered[
      predictingOutputDataframe$Timestep == t
    ] <- filteredPredicting$PredictedFiltered
  }
}


# Reclassify probability raster ------------------------------------------------

progressBar(type = "message", message = "Reclassifying")


# ---- Apply Reclassification Rules (training: unfiltered + filtered) ----
if (!is_empty(trainTimestepList)) {
  for (t in trainTimestepList) {

    # Get file paths
    unfilteredTrainFilepath <- trainingOutputDataframe$PredictedUnfiltered[
      trainingOutputDataframe$Timestep == t
    ]
    filteredTrainFilepath <- trainingOutputDataframe$PredictedFiltered[
      trainingOutputDataframe$Timestep == t
    ]

    # Skip if no unfiltered path
    if (is.na(unfilteredTrainFilepath)) next

    # Load base rasters
    unfiltered <- rast(unfilteredTrainFilepath)

    # Disk-backed working copies
    reclassedUnf <- writeRaster(
      unfiltered, filename = tempfile(fileext = ".tif"),
      overwrite = TRUE, wopt = wopt_int
    )

    hasFiltered <- !is.na(filteredTrainFilepath)
    if (hasFiltered) {
      filtered <- rast(filteredTrainFilepath)
      reclassedFil <- writeRaster(
        filtered, filename = tempfile(fileext = ".tif"),
        overwrite = TRUE, wopt = wopt_int
      )
    } else {
      filtered <- reclassedFil <- NULL
    }

    # Rules
    if (nrow(ruleReclassDataframe) == 0) {
      updateRunLog(
        "No rule-based reclassification rules found. Skipping reclassification.",
        type = "warning"
      )
    } else {
      for (i in seq_len(nrow(ruleReclassDataframe))) {

        # Load rule raster
        ruleRaster <- rast(ruleReclassDataframe$ruleRasterFile[i])

        # Ensure geometry match; skip (or resample here if desired)
        if (!compareGeom(ruleRaster, reclassedUnf, stopOnError = FALSE)) {
          updateRunLog(
            "Rule raster extent does not match unfiltered training raster extent. Skipping reclassification.",
            type = "warning"
          )
          next
        }

        vmin <- ruleReclassDataframe$ruleMinValue[i]
        vmax <- ruleReclassDataframe$ruleMaxValue[i]
        rval <- as.numeric(ruleReclassDataframe$ruleReclassValue[i])

        if (isTRUE(vmin == vmax)) {
          # ---------- CATEGORICAL: apply where ruleRaster == vmin ----------
          tmpUnf <- tempfile(fileext = ".tif")
          reclassedUnf <- ifel(
            ruleRaster == vmin,
            rval,
            reclassedUnf,
            filename = tmpUnf, overwrite = TRUE
          )

          if (hasFiltered) {
            tmpFil <- tempfile(fileext = ".tif")
            reclassedFil <- ifel(
              ruleRaster == vmin,
              rval,
              reclassedFil,
              filename = tmpFil, overwrite = TRUE
            )
          }

        } else {
          # ---------- CONTINUOUS: classify range [vmin, vmax] ----------
          rtab <- matrix(c(vmin, vmax, rval), ncol = 3, byrow = TRUE)
          classedMask <- classify(
            ruleRaster, rtab, others = NA,
            filename = tempfile(fileext = ".tif"), overwrite = TRUE
          )

          # Update where mask is not NA
          tmpUnf <- tempfile(fileext = ".tif")
          reclassedUnf <- ifel(
            !is.na(classedMask),
            rval,
            reclassedUnf,
            filename = tmpUnf, overwrite = TRUE
          )

          if (hasFiltered) {
            tmpFil <- tempfile(fileext = ".tif")
            reclassedFil <- ifel(
              !is.na(classedMask),
              rval,
              reclassedFil,
              filename = tmpFil, overwrite = TRUE
            )
          }

          rm(classedMask); gc()
        }

        gc()
      }
    }

    # ---- Final writes for timestep t ----
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

    if (!is.null(reclassedFil)) {
      reclassedFilteredPathTrain <- file.path(
        transferDir,
        paste0("PredictedPresenceFilteredRestricted-","training","-t",t,".tif")
      )
      reclassedFil <- writeRaster(
        reclassedFil, filename = reclassedFilteredPathTrain,
        overwrite = TRUE, wopt = wopt_int
      )
      trainingOutputDataframe$PredictedFilteredRestricted[
        trainingOutputDataframe$Timestep == t
      ] <- reclassedFilteredPathTrain
    }

    # Cleanup per-timestep
    rm(unfiltered, filtered, reclassedUnf, reclassedFil); gc()
  }
}

# ---- Apply Reclassification Rules (predicting: unfiltered + filtered) ----
if (!is_empty(predTimestepList)) {
  for (t in predTimestepList) {

    # Get file paths
    unfilteredPredFilepath <- predictingOutputDataframe$ClassifiedUnfiltered[
      predictingOutputDataframe$Timestep == t
    ]
    filteredPredFilepath <- predictingOutputDataframe$ClassifiedFiltered[
      predictingOutputDataframe$Timestep == t
    ]

    # Skip if no unfiltered path
    if (is.na(unfilteredPredFilepath)) next

    # Load rasters
    unfiltered <- rast(unfilteredPredFilepath)

    # Start disk-backed working copies
    reclassedUnf <- writeRaster(
      unfiltered, filename = tempfile(fileext = ".tif"),
      overwrite = TRUE, wopt = wopt_int
    )

    hasFiltered <- !is.na(filteredPredFilepath)
    if (hasFiltered) {
      filtered <- rast(filteredPredFilepath)
      reclassedFil <- writeRaster(
        filtered, filename = tempfile(fileext = ".tif"),
        overwrite = TRUE, wopt = wopt_int
      )
    } else {
      filtered <- reclassedFil <- NULL
    }

    # Apply rules (if any)
    if (nrow(ruleReclassDataframe) != 0) {
      for (i in seq_len(nrow(ruleReclassDataframe))) {

        # Load rule raster
        ruleRaster <- rast(ruleReclassDataframe$ruleRasterFile[i])

        # Ensure same geometry; otherwise skip (or resample if you prefer)
        if (!compareGeom(ruleRaster, reclassedUnf, stopOnError = FALSE)) {
          updateRunLog(
            "Rule raster extent does not match predicting raster extent. Skipping reclassification.",
            type = "warning"
          )
          next
        }

        vmin <- ruleReclassDataframe$ruleMinValue[i]
        vmax <- ruleReclassDataframe$ruleMaxValue[i]
        rval <- as.numeric(ruleReclassDataframe$ruleReclassValue[i])

        if (isTRUE(vmin == vmax)) {
          # ---------- CATEGORICAL: apply where ruleRaster == vmin ----------
          tmpUnf <- tempfile(fileext = ".tif")
          reclassedUnf <- ifel(
            ruleRaster == vmin,
            rval,                 # set to rval where condition true
            reclassedUnf,         # otherwise keep
            filename = tmpUnf, overwrite = TRUE
          )

          if (hasFiltered) {
            tmpFil <- tempfile(fileext = ".tif")
            reclassedFil <- ifel(
              ruleRaster == vmin,
              rval,
              reclassedFil,
              filename = tmpFil, overwrite = TRUE
            )
          }

        } else {
          # ---------- CONTINUOUS: classify range [vmin, vmax] ----------
          rtab <- matrix(c(vmin, vmax, rval), ncol = 3, byrow = TRUE)
          classedMask <- classify(
            ruleRaster, rtab, others = NA,
            filename = tempfile(fileext = ".tif"), overwrite = TRUE
          )

          # Update where mask is not NA (i.e., cells under the rule)
          tmpUnf <- tempfile(fileext = ".tif")
          reclassedUnf <- ifel(
            !is.na(classedMask),
            rval,
            reclassedUnf,
            filename = tmpUnf, overwrite = TRUE
          )

          if (hasFiltered) {
            tmpFil <- tempfile(fileext = ".tif")
            reclassedFil <- ifel(
              !is.na(classedMask),
              rval,
              reclassedFil,
              filename = tmpFil, overwrite = TRUE
            )
          }

          rm(classedMask); gc()
        }

        gc()
      }
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

    if (!is.null(reclassedFil)) {
      reclassedFilteredPathPred <- file.path(
        transferDir,
        paste0("PredictedPresenceFilteredRestricted-","predicting","-t",t,".tif")
      )
      reclassedFil <- writeRaster(
        reclassedFil, filename = reclassedFilteredPathPred,
        overwrite = TRUE, wopt = wopt_int
      )
      predictingOutputDataframe$ClassifiedFilteredRestricted[
        predictingOutputDataframe$Timestep == t
      ] <- reclassedFilteredPathPred
    }

    # Cleanup per-timestep
    rm(unfiltered, filtered, reclassedUnf, reclassedFil); gc()
  }
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
