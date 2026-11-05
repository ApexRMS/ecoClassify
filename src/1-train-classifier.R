## -------------------------------
## ecoClassify - Train Classifier
## ApexRMS, November 2024
## -------------------------------

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
myProject <- project(myScenario)

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load project-level datasheets ------------------------------------------------
terminologyDataframe <- datasheet(myProject, name = "ecoClassify_Terminology")
terminology <- parseTerminology(terminologyDataframe)
bandLabelFile <- terminology$bandLabelFile
rgbBands <- terminology$rgbBands

# Load raster input datasheets -------------------------------------------------
trainingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputTrainingRasters"
)

trainingCovariateDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputTrainingCovariates"
)

# Assign variables -------------------------------------------------------------

inputVariables <- assignVariables(
  myScenario,
  trainingRasterDataframe,
  trainingRasterDataframe$TrainingRasterFile
)
timestepList <- inputVariables[[1]]
nObs <- inputVariables[[2]]
applyContextualization <- inputVariables[[3]]
contextualizationWindowSize <- inputVariables[[4]]
modelType <- inputVariables[[5]]
modelTuning <- inputVariables[[6]]
setManualThreshold <- inputVariables[[7]]
manualThreshold <- inputVariables[[8]]
normalizeRasters <- inputVariables[[9]]
rasterDecimalPlaces <- inputVariables[[10]]
tuningObjective <- inputVariables[[11]]
overrideBandnames <- inputVariables[[12]]
setSeed <- inputVariables[[13]]

# Load model-type specific packages
loadModelPackages(modelType)

# Set the random seed if provided
if (!is.null(setSeed) && !is.na(setSeed)) {
  set.seed(setSeed)
}

# Check if multiprocessing is selected
mulitprocessingSheet <- datasheet(myScenario, "core_Multiprocessing")
nCores <- setCores(mulitprocessingSheet)

# Read TargetClassOptions ------------------------------------------------------

targetClassOptions <- parseTargetClassOptions(
  datasheet(myScenario, "ecoClassify_TargetClassOptions")
)
useTargetClass   <- targetClassOptions$useTargetClass
targetClassValue <- targetClassOptions$targetClassValue
backgroundValues <- targetClassOptions$backgroundValues
ignoreValues     <- targetClassOptions$ignoreValues
targetClassLabel <- targetClassOptions$targetClassLabel


# Extract list of training and ground truth rasters ----------------------------

trainingRasterList <- extractRasters(trainingRasterDataframe, column = 2)
groundTruthRasterList <- extractRasters(trainingRasterDataframe, column = 3)

# extractRasters() skips rows with no raster file, so re-align timestepList with
# the rasters that were actually loaded. Outputs and log messages below are
# labelled positionally (timestepList[t] for trainingRasterList[[t]]).
timestepList <- alignTimestepList(
  timestepList,
  trainingRasterList,
  context = "training rasters"
)

# Override band names if selected
if (isTRUE(overrideBandnames)) {
  if (!is.null(bandLabelFile)) {
    updateRunLog("Applying band label override to training rasters.", type = "info")
    trainingRasterList <- overrideBandNames(trainingRasterList, bandLabelFile)
  } else {
    updateRunLog(
      "Override band names is enabled but no band label file was supplied; band names will not be changed.",
      type = "warning"
    )
  }
} else {
  if (!is.null(bandLabelFile)) {
    updateRunLog(
      "A band label file was supplied but 'Override band names' is not enabled; band names will not be changed.",
      type = "info"
    )
  }
  updateRunLog(
    paste0(
      "Using original band names: ",
      paste(names(trainingRasterList[[1]]), collapse = ", ")
    ),
    type = "info"
  )
}

# Validate and align rasters ---------------------------------------------------
progressBar(type = "message", message = "Validating and aligning rasters")

# Check that all training rasters have consistent spatial properties. Reasmple groundTruth as needed
groundTruthRasterList <- validateAndAlignRasters(
  trainingRasterList,
  groundTruthRasterList
)

# Validate target class value is present in ground truth rasters ---------------

if (isTRUE(useTargetClass) && !is.na(targetClassValue)) {
  timestepsWithTarget <- vapply(groundTruthRasterList, function(r) {
    targetClassValue %in% terra::unique(r)[[1]]
  }, logical(1))

  if (!any(timestepsWithTarget)) {
    stop(
      "Target class value (", targetClassValue, ") was not found in any user classified raster. ",
      "Check that 'Target Class Value' matches the values in your user classified rasters."
    )
  } else if (!all(timestepsWithTarget)) {
    missingIdx <- which(!timestepsWithTarget)
    updateRunLog(
      paste0(
        "Target class value (", targetClassValue, ") was not found in user classified raster(s) for timestep(s): ",
        paste(timestepList[missingIdx], collapse = ", "), ". ",
        "These timesteps will have no presence pixels and may be dropped."
      ),
      type = "warning"
    )
  }
}

# Pre-processing ---------------------------------------------------------------

progressBar(type = "message", message = "Pre-processing input data")

# Round rasters to integer, if selected
if (
  is.numeric(rasterDecimalPlaces) &&
    length(rasterDecimalPlaces) > 0 &&
    !is.na(rasterDecimalPlaces)
) {
  trainingRasterList <- lapply(trainingRasterList, function(r) {
    round(r, digits = rasterDecimalPlaces)
  })
}

# Normalize training rasters, if selected. The per-band min/max are pooled
# across all training rasters and saved with the model, so prediction rescales
# on exactly the same scale.
normalizationStats <- NULL
if (isTRUE(normalizeRasters)) {
  normalizationStats <- computeNormalizationStats(trainingRasterList)
  trainingRasterList <- normalizeRaster(trainingRasterList, normalizationStats)
}

# Apply contextualization to training rasters, if selected. The PCA is fitted
# once across all training rasters and saved with the model, so PC1/PC2 mean
# the same thing at prediction time.
contextPcaModel <- NULL
if (isTRUE(applyContextualization)) {
  contextPcaModel <- fitContextPca(trainingRasterList)
  trainingRasterList <- contextualizeRaster(
    trainingRasterList,
    windowSize = contextualizationWindowSize,
    pcaModel = contextPcaModel
  )
}

# Reclassify ground truth rasters
groundTruthRasterList <- reclassifyGroundTruth(
  groundTruthRasterList,
  useTargetClass   = useTargetClass,
  targetClassValue = targetClassValue,
  backgroundValues = backgroundValues,
  ignoreValues     = ignoreValues
)

# Extract covariate rasters and convert to correct data type
trainingCovariateList <- processCovariates(
  trainingCovariateDataframe,
  modelType
)

## filtered out timesteps with issues
flt <- skipBadTimesteps(
  trainingRasterList,
  groundTruthRasterList,
  timesteps = timestepList
)
## using filtered datasets going forward
trainingRasterList <- flt$trainingRasterList
groundTruthRasterList <- flt$groundTruthRasterList
if (!is.null(flt$keptTimesteps)) {
  timestepList <- flt$keptTimesteps
}
## guard: if all timesteps were dropped, abort early with a clear message
if (length(trainingRasterList) == 0) {
  warning(
    "All timesteps were dropped because of missing data or projection issues; no data available for training."
  )
  stop("Aborting: no valid timesteps remain after filtering.")
}


# Add covariate data to training rasters.
# Pass transferDir so combined files are written there rather than R's
# tempdir(), which doParallel PSOCK workers can clean up on Windows.
trainingRasterList <- addCovariates(trainingRasterList, trainingCovariateList, transferDir)

# Check for NA values in training rasters
# NOTE: Masking is not applied, but a message is returned if there are an uneven
#       number of NA values across the training data
checkNA(trainingRasterList)

# Separate training and testing data
splitData <- splitTrainTest(trainingRasterList, groundTruthRasterList, nObs)
allTrainData <- splitData[[1]]
allTestData <- splitData[[2]]

if (modelType == "Random Forest") {
  allTrainData$presence <- factor(
    allTrainData$presence,
    levels = c(0, 1),
    labels = c("absence", "presence")
  )
  allTestData$presence <- factor(
    allTestData$presence,
    levels = c(0, 1),
    labels = c("absence", "presence")
  )
}

# Setup empty dataframes to accept output in SyncroSim datasheet format --------

rasterOutputDataframe <- data.frame(
  Timestep = numeric(0),
  PredictedUnfiltered = character(0),
  PredictedFiltered = character(0),
  GroundTruth = character(0),
  Probability = character(0)
)

confusionOutputDataframe <- data.frame(
  Prediction = numeric(0),
  Reference = numeric(0),
  Frequency = numeric(0)
)

modelOutputDataframe <- data.frame(Statistic = character(0), Value = numeric(0))

rgbOutputDataframe <- data.frame(Timestep = numeric(0), RGBImage = character(0))

summaryRows <- list()
metricsRows <- list()


# Train model ------------------------------------------------------------------

progressBar(type = "message", message = "Training model")

if (modelType == "MaxEnt") {
  modelOut <- getMaxentModel(allTrainData, nCores, isTRUE(modelTuning))
  if (isTRUE(setManualThreshold)) {
    threshold <- manualThreshold
  } else {
    threshold <- getOptimalThreshold(
      modelOut,
      allTestData,
      "MaxEnt",
      tuningObjective
    )
  }
} else if (modelType == "Random Forest") {
  modelOut <- getRandomForestModel(allTrainData, nCores, isTRUE(modelTuning))
  if (isTRUE(setManualThreshold)) {
    threshold <- manualThreshold
  } else {
    threshold <- getOptimalThreshold(
      modelOut,
      allTestData,
      "Random Forest",
      tuningObjective
    )
  }
} else if (modelType == "CNN") {
  modelOut <- getCNNModel(allTrainData, nCores, isTRUE(modelTuning))
  if (isTRUE(setManualThreshold)) {
    threshold <- manualThreshold
  } else {
    threshold <- getOptimalThreshold(
      modelOut,
      allTestData,
      "CNN",
      tuningObjective
    )
  }
} else {
  stop("Model type not recognized")
}
model <- modelOut[[1]]
variableImportance <- modelOut[[2]]

# Extract raster values for diagnostics
rastLayerHistogram <- getRastLayerHistogram(
  trainingRasterList,
  modelOut,
  nBins = 20,
  nSample = 10000
)

# Save the fitted pre-processing with the model so 2b-predict.R can apply the
# same normalization and PCA (appended last so positional access to modelOut
# elements is unaffected)
modelOut$preprocessing <- list(
  normalizationStats = normalizationStats,
  contextPcaModel = contextPcaModel
)

if (modelType == "CNN") {
  # Save Torch weights separately
  model_weights_path <- file.path(transferDir, "model_weights.pt")
  torch::torch_save(modelOut$model$state_dict(), model_weights_path)

  # Save non-torch metadata
  metadata <- modelOut[names(modelOut) != "model"]
  metadata_path <- file.path(transferDir, "model_metadata.rds")
  saveRDS(metadata, metadata_path)
} else {
  model_path <- file.path(transferDir, "model.rds")
  saveRDS(modelOut, model_path)
}

# Add to output datasheet
if (modelType == "CNN") {
  modelObjectOutputDataframe <- data.frame(
    Model = metadata_path,
    Threshold = threshold,
    Weights = model_weights_path
  )
} else {
  modelObjectOutputDataframe <- data.frame(
    # TODO: add warning if missing present/absent and threshold == 0
    Model = model_path,
    Threshold = threshold,
    Weights = ""
  )
}

# Save model object to output datasheet
variableImportanceOutput <- plotVariableImportance(
  variableImportance,
  transferDir
)

variableImportancePlot <- variableImportanceOutput[[1]]
varImportanceOutputImage <- variableImportanceOutput[[2]]

# Generate dataframe
varImportanceOutputDataframe <- as.data.frame(variableImportance) %>%
  tibble::rownames_to_column("Variable") %>%
  rename(Importance = "variableImportance")

# Predict presence for training rasters in each timestep group -----------------

progressBar(type = "message", message = "Predict training rasters")

for (t in seq_along(trainingRasterList)) {
  # Get timestep for the current raster
  timestep <- timestepList[t]

  # Get prediction rasters (written directly to disk for memory efficiency)
  predictionRasters <- getPredictionRasters(
    trainingRasterList[[t]],
    modelOut,
    threshold,
    modelType,
    transferDir,
    category = "training",
    timestep,
    nCores = nCores
  )
  predictedPresencePath <- predictionRasters$presencePath
  probabilityPath <- predictionRasters$probabilityPath

  # Generate rasterDataframe based on filtering argument
  # Note: generateRasterDataframe constructs paths internally and doesn't use the first argument
  rasterOutputDataframe <- generateRasterDataframe(
    predictedPresencePath,
    category = "training",
    timestep,
    transferDir,
    rasterOutputDataframe,
    hasGroundTruth = TRUE
  )

  # Define GroundTruth raster
  groundTruth <- groundTruthRasterList[[t]]

  # Define RGB data frame
  rgbOutputDataframe <- getRgbDataframe(
    rgbOutputDataframe,
    category = "training",
    timestep,
    transferDir
  )

  # Save files (ground truth and RGB only - predictions already saved)
  saveFiles(
    predictedPresencePath,
    probabilityPath,
    trainingRasterList[[t]],
    groundTruth,
    category = "training",
    timestep,
    transferDir,
    rgbBands = rgbBands
  )

  # Accumulate summary row for this timestep
  tryCatch({
    summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
      predictionRaster  = terra::rast(predictedPresencePath),
      probabilityRaster = terra::rast(probabilityPath),
      timestep          = timestep,
      predictionType    = "training",
      targetClassValue  = targetClassValue,
      targetClassLabel  = targetClassLabel
    )
  }, error = function(e) {
    updateRunLog(paste0("Could not build summary row for timestep ", timestep, ": ", conditionMessage(e)), type = "warning")
  })
  groundTruthRasterList[t] <- list(NULL)
}

# Free training data now that all raster reads are complete.
# gc() is deferred to here so it does not run while trainingRasterList is still
# being read (getRastLayerHistogram and the prediction loop above). Calling gc()
# earlier — including inside getRandomForestModel — can invalidate terra's
# internal raster state when covariate rasters are present, causing
# [readValues] errors.
rm(allTrainData, model, variableImportance, trainingRasterList, trainingCovariateList)
gc()

progressBar(type = "message", message = "Calculating summary statistics")

# Predict response based on range of values
responseHistogram <- predictResponseHistogram(
  rastLayerHistogram,
  modelOut,
  modelType
)

histogramJoin <- responseHistogram %>%
  left_join(rastLayerHistogram, by = c("layer", "predictor" = "bin_lower"))

plotLayerHistogram(histogramJoin, transferDir)

# Add to output datasheet
layerHistogramPlotOutputDataframe <- data.frame(
  LayerHistogramPlot = file.path(paste0(
    transferDir,
    "/LayerHistogramResponse.png"
  ))
)

# Calculate mean values for model statistics --------------------

outputDataframes <- calculateStatistics(
  modelOut,
  allTestData,
  threshold,
  confusionOutputDataframe,
  modelOutputDataframe
)

confusionOutputDataframe <- outputDataframes[[1]]
modelOutputDataframe <- outputDataframes[[2]]
confusionMatrixPlot <- outputDataframes[[3]]

# Accumulate metrics row
tryCatch({
  metricsRows[[1]] <- buildMetricsRow(
    statsDataframe   = modelOutputDataframe,
    targetClassValue = targetClassValue,
    targetClassLabel = targetClassLabel
  )
}, error = function(e) {
  updateRunLog(paste0("Could not build metrics row: ", conditionMessage(e)), type = "warning")
})

# Make a confusion matrix output dataframe
ggsave(
  filename = file.path(paste0(transferDir, "/ConfusionMatrixPlot.png")),
  confusionMatrixPlot
)

# Add to output datasheet
confusionMatrixPlotOutputDataframe <- data.frame(
  ConfusionMatrixPlot = file.path(paste0(
    transferDir,
    "/ConfusionMatrixPlot.png"
  ))
)

# Save filter resolution and threshold to input datasheet ----------------------

progressBar(type = "message", message = "Saving results")


if (is.null(rasterDecimalPlaces)) {
  rasterDecimalPlaces <- ""
}
if (is.null(contextualizationWindowSize)) {
  contextualizationWindowSize <- ""
}
if (is.null(manualThreshold)) {
  manualThreshold <- ""
}
if (is.null(tuningObjective)) {
  tuningObjective <- ""
}
if (is.null(setSeed)) {
  setSeed <- ""
}

classifierOptionsOutputDataframe <- data.frame(
  nObs = format(nObs, scientific = FALSE),
  modelType = modelType
)

advClassifierOptionsOutputDataframe <- data.frame(
  normalizeRasters = isTRUE(normalizeRasters),
  overrideBandnames = isTRUE(overrideBandnames),
  rasterDecimalPlaces = rasterDecimalPlaces,
  modelTuning = isTRUE(modelTuning),
  tuningObjective = tuningObjective,
  setManualThreshold = isTRUE(setManualThreshold),
  manualThreshold = manualThreshold,
  applyContextualization = isTRUE(applyContextualization),
  contextualizationWindowSize = contextualizationWindowSize,
  setSeed = setSeed
)

# Save dataframes back to SyncroSim library's output datasheets ----------------
saveDatasheet(
  myScenario,
  data = classifierOptionsOutputDataframe,
  name = "ecoClassify_ClassifierOptions"
)

saveDatasheet(
  myScenario,
  data = advClassifierOptionsOutputDataframe,
  name = "ecoClassify_AdvancedClassifierOptions"
)

saveDatasheet(
  myScenario,
  data = rasterOutputDataframe,
  name = "ecoClassify_RasterOutput"
)

saveDatasheet(
  myScenario,
  data = confusionOutputDataframe,
  name = "ecoClassify_ConfusionMatrix"
)

saveDatasheet(
  myScenario,
  data = modelOutputDataframe,
  name = "ecoClassify_ModelStatistics"
)


saveDatasheet(
  myScenario,
  data = varImportanceOutputImage,
  name = "ecoClassify_VariableImportanceOutput"
)

saveDatasheet(
  myScenario,
  data = rgbOutputDataframe,
  name = "ecoClassify_RgbOutput"
)

saveDatasheet(
  myScenario,
  data = modelObjectOutputDataframe,
  name = "ecoClassify_ModelObject"
)

saveDatasheet(
  myScenario,
  data = confusionMatrixPlotOutputDataframe,
  name = "ecoClassify_ConfusionMatrixPlotOutput"
)

saveDatasheet(
  myScenario,
  data = layerHistogramPlotOutputDataframe,
  name = "ecoClassify_LayerHistogramPlotOutput"
)

saveDatasheet(
  myScenario,
  data = varImportanceOutputDataframe,
  name = "ecoClassify_VariableImportanceOutputDataframe"
)

if (length(summaryRows) > 0) {
  summaryDf <- do.call(rbind, summaryRows)
  saveDatasheet(myScenario, data = summaryDf, name = "ecoClassify_SummaryOutput")
  saveDatasheet(myScenario, data = summaryDf, name = "ecoClassify_SummaryOutputChart")
}

if (length(metricsRows) > 0) {
  saveDatasheet(
    myScenario,
    data = do.call(rbind, metricsRows),
    name = "ecoClassify_ModelMetricsByClass"
  )
}

terra::tmpFiles(remove = TRUE)
