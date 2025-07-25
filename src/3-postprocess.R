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


# Function ---------------------------------------------------------------------

filterRasterDataframe <- function(
  applyFiltering,
  predictedPresence,
  filterValue,
  fillValue,
  category,
  timestep,
  transferDir
) {
  if (!applyFiltering) {
    return(data.frame(
      Timestep = timestep,
      PredictedFiltered = NA_character_
    ))
  }

  # Filter out presence pixels surrounded by non-presence
  filteredPredictedPresence <- filterPredictionRaster(
    predictedPresence,
    filterValue = filterValue,
    fillValue = fillValue
  )

  # File path
  filteredPath <- file.path(paste0(
    transferDir,
    "/filteredPredictedPresence-",
    category,
    "-t",
    timestep,
    ".tif"
  ))

  # Save raster
  writeRaster(
    filteredPredictedPresence,
    filename = filteredPath,
    overwrite = TRUE
  )

  # Build dataframe
  rasterDataframe <- data.frame(
    Timestep = timestep,
    PredictedFiltered = filteredPath
  )

  return(rasterDataframe)
}

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


## Training ------------------------------------------------------

if (!is_empty(trainTimestepList)) {
  for (t in trainTimestepList) {
    # Get file paths
    unfilteredTrainFilepath <- trainingOutputDataframe$PredictedUnfiltered[
      trainingOutputDataframe$Timestep == t
    ]

    # Load unfiltered raster
    unfilteredTrainRaster <- reclassedUnfilteredTrain <- rast(
      unfilteredTrainFilepath
    )

    if (nrow(ruleReclassDataframe) == 0) {
      updateRunLog(
        "No rule-based reclassification rules found. Skipping reclassification.",
        type = "warning"
      )
    } else {
      for (i in 1:dim(ruleReclassDataframe)[1]) {
        if (
          !is.null(unfilteredTrainFilepath) && !is.na(unfilteredTrainFilepath)
        ) {
          # Load rule raster
          ruleRaster <- rast(ruleReclassDataframe$ruleRasterFile[i])

          # Categorical vs. Continuous
          if (
            ruleReclassDataframe$ruleMinValue[i] ==
              ruleReclassDataframe$ruleMaxValue[i]
          ) {
            # Reclass table
            reclassTable <- matrix(
              c(
                ruleReclassDataframe$ruleMinValue[i],
                as.numeric(paste(ruleReclassDataframe$ruleReclassValue[i]))
              ),
              ncol = 2,
              byrow = TRUE
            )

            # Reclassify categorical
            reclassedUnfilteredTrain[ruleRaster == reclassTable[, 1]] <-
              reclassTable[, 2]
          } else {
            # Reclass table
            reclassTable <- matrix(
              c(
                ruleReclassDataframe$ruleMinValue[i],
                ruleReclassDataframe$ruleMaxValue[i],
                as.numeric(paste(ruleReclassDataframe$ruleReclassValue[i]))
              ),
              ncol = 3,
              byrow = T
            )
            # Reclassify continuous
            ruleReclassRaster <- classify(ruleRaster, reclassTable, others = NA)
            reclassedUnfilteredTrain[] <- mask(
              unfilteredTrainRaster,
              ruleReclassRaster,
              maskvalue = as.numeric(paste(ruleReclassDataframe$ruleReclassValue[
                i
              ])),
              updatevalue = as.numeric(paste(ruleReclassDataframe$ruleReclassValue[
                i
              ]))
            )
          }
        }
      }
    }

    # File path for timestep t
    reclassedPathTrain <- file.path(paste0(
      transferDir,
      "/PredictedPresenceRestricted-",
      "training",
      "-t",
      t,
      ".tif"
    ))
    # Save raster for timestep t
    writeRaster(
      reclassedUnfilteredTrain,
      filename = reclassedPathTrain,
      overwrite = TRUE
    )

    # Add raster for timestep t to results
    trainingOutputDataframe$PredictedUnfilteredRestricted[
      trainingOutputDataframe$Timestep == t
    ] <- reclassedPathTrain
  }
}


## Predicting ----------------------------------------------------

if (!is_empty(predTimestepList)) {
  for (t in predTimestepList) {
    # Get file paths
    unfilteredPredFilepath <- predictingOutputDataframe$ClassifiedUnfiltered[
      predictingOutputDataframe$Timestep == t
    ]

    # Load unfiltered raster
    unfilteredPredRaster <- reclassedUnfilteredPred <- rast(
      unfilteredPredFilepath
    )

    for (i in 1:dim(ruleReclassDataframe)[1]) {
      if (!is.na(unfilteredPredFilepath)) {
        # Load rule raster
        ruleRaster <- rast(ruleReclassDataframe$ruleRasterFile[i])

        # Categorical vs. Continuous
        if (
          ruleReclassDataframe$ruleMinValue[i] ==
            ruleReclassDataframe$ruleMaxValue[i]
        ) {
          # Reclass table
          reclassTable <- matrix(
            c(
              ruleReclassDataframe$ruleMinValue[i],
              as.numeric(paste(ruleReclassDataframe$ruleReclassValue[i]))
            ),
            ncol = 2,
            byrow = TRUE
          )

          # Reclassify categorical
          reclassedUnfilteredPred[ruleRaster == reclassTable[, 1]] <-
            reclassTable[, 2]
        } else {
          # Reclass table
          reclassTable <- matrix(
            c(
              ruleReclassDataframe$ruleMinValue,
              ruleReclassDataframe$ruleMaxValue,
              as.numeric(paste(ruleReclassDataframe$ruleReclassValue[i]))
            ),
            ncol = 3,
            byrow = T
          )
          # Reclassify continuous
          ruleReclassRaster <- classify(ruleRaster, reclassTable, others = NA)
          reclassedUnfilteredPred[] <- mask(
            unfilteredPredRaster,
            ruleReclassRaster,
            maskvalue = as.numeric(paste(ruleReclassDataframe$ruleReclassValue[
              i
            ])),
            updatevalue = as.numeric(paste(ruleReclassDataframe$ruleReclassValue[
              i
            ]))
          )
        }
      }
    }

    # File path for timestep t
    reclassedPathPred <- file.path(paste0(
      transferDir,
      "/PredictedPresenceRestricted-",
      "predicting",
      "-t",
      t,
      ".tif"
    ))
    # Save raster for timestep t
    writeRaster(
      reclassedUnfilteredPred,
      filename = reclassedPathPred,
      overwrite = TRUE
    )

    # Add raster for timestep t to results
    predictingOutputDataframe$ClassifiedUnfilteredRestricted[
      predictingOutputDataframe$Timestep == t
    ] <- reclassedPathPred
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
