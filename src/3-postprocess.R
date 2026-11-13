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
# Deduplicate: spatial tiling causes RasterOutput rows to be replicated once
# per tile job when SyncroSim merges job libraries back into the parent
trainingOutputDataframe <- dplyr::distinct(trainingOutputDataframe)
# Clear restricted columns so metrics only use paths written in the current run
trainingOutputDataframe$PredictedUnfilteredRestricted <- NA_character_
trainingOutputDataframe$PredictedFilteredRestricted   <- NA_character_

predictingOutputDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_ClassifiedRasterOutput"
)
# Clear restricted columns so metrics only use paths written in the current run
predictingOutputDataframe$ClassifiedUnfilteredRestricted <- NA_character_
predictingOutputDataframe$ClassifiedFilteredRestricted   <- NA_character_




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
      probPath <- trainingOutputDataframe$Probability[trainingOutputDataframe$Timestep == t][1]
      if (!is.na(probPath) && file.exists(probPath)) {
        summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
          predictionRaster  = terra::rast(filteredTraining$PredictedFiltered),
          probabilityRaster = terra::rast(probPath),
          timestep          = t,
          predictionType    = "filtered - training",
          targetClassValue  = targetClassValue,
          targetClassLabel  = targetClassLabel
        )
      }
    }
    rm(predictedPresence, filteredTraining); gc()
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
      probPath <- predictingOutputDataframe$ClassifiedProbability[predictingOutputDataframe$Timestep == t][1]
      if (!is.na(probPath) && file.exists(probPath)) {
        summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
          predictionRaster  = terra::rast(filteredPredicting$PredictedFiltered),
          probabilityRaster = terra::rast(probPath),
          timestep          = t,
          predictionType    = "filtered - predicting",
          targetClassValue  = targetClassValue,
          targetClassLabel  = targetClassLabel
        )
      }
    }
    rm(predictedPresence, filteredPredicting); gc()
  }
}


# Reclassify probability raster ------------------------------------------------

progressBar(type = "message", message = "Reclassifying")

# Applies the restriction rules to one timestep's unfiltered prediction, writes
# the restricted (and, if filtering is on, filtered + restricted) rasters
# straight to transferDir, and returns their paths plus summary rows.
restrictTimestep <- function(unfilteredPath, probPath, category, t, labelSuffix) {
  restrictedPath <- file.path(
    transferDir,
    paste0("PredictedPresenceRestricted-", category, "-t", t, ".tif")
  )
  applyRestrictionRules(unfilteredPath, ruleReclassDataframe, restrictedPath, wopt = binaryWopt())

  rows <- list()
  hasProb <- !is.na(probPath) && file.exists(probPath)
  if (hasProb) {
    rows[[length(rows) + 1]] <- buildSummaryRow(
      predictionRaster  = terra::rast(restrictedPath),
      probabilityRaster = terra::rast(probPath),
      timestep          = t,
      predictionType    = paste0("reclassified - ", labelSuffix),
      targetClassValue  = targetClassValue,
      targetClassLabel  = targetClassLabel
    )
  }

  filteredRestrictedPath <- NA_character_
  if (isTRUE(applyFiltering)) {
    # Written directly to its final location (no temporary copy)
    filteredRestricted <- filterRasterDataframe(
      applyFiltering,
      terra::rast(restrictedPath),
      filterValue,
      fillValue,
      category,
      t,
      transferDir,
      fileName = "PredictedPresenceFilteredRestricted"
    )
    filteredRestrictedPath <- filteredRestricted$PredictedFiltered

    if (hasProb) {
      rows[[length(rows) + 1]] <- buildSummaryRow(
        predictionRaster  = terra::rast(filteredRestrictedPath),
        probabilityRaster = terra::rast(probPath),
        timestep          = t,
        predictionType    = paste0("filtered_reclassified - ", labelSuffix),
        targetClassValue  = targetClassValue,
        targetClassLabel  = targetClassLabel
      )
    }
  }

  list(restricted = restrictedPath, filteredRestricted = filteredRestrictedPath, summaryRows = rows)
}

firstPath <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (length(paths) == 0) NA_character_ else paths[1]
}

if (nrow(ruleReclassDataframe) != 0) {
  # ---- training rasters ----
  for (t in trainTimestepList) {
    rowIdx <- trainingOutputDataframe$Timestep == t
    unfilteredPath <- firstPath(trainingOutputDataframe$PredictedUnfiltered[rowIdx])
    if (is.na(unfilteredPath) || !file.exists(unfilteredPath)) next

    res <- restrictTimestep(
      unfilteredPath,
      firstPath(trainingOutputDataframe$Probability[rowIdx]),
      "training", t, "training"
    )
    trainingOutputDataframe$PredictedUnfilteredRestricted[rowIdx] <- res$restricted
    if (!is.na(res$filteredRestricted)) {
      trainingOutputDataframe$PredictedFilteredRestricted[rowIdx] <- res$filteredRestricted
    }
    summaryRows <- c(summaryRows, res$summaryRows)
  }

  # ---- predicting rasters ----
  for (t in predTimestepList) {
    rowIdx <- predictingOutputDataframe$Timestep == t
    unfilteredPath <- firstPath(predictingOutputDataframe$ClassifiedUnfiltered[rowIdx])
    if (is.na(unfilteredPath) || !file.exists(unfilteredPath)) next

    res <- restrictTimestep(
      unfilteredPath,
      firstPath(unique(predictingOutputDataframe$ClassifiedProbability[rowIdx])),
      "predicting", t, "predicting"
    )
    predictingOutputDataframe$ClassifiedUnfilteredRestricted[rowIdx] <- res$restricted
    if (!is.na(res$filteredRestricted)) {
      predictingOutputDataframe$ClassifiedFilteredRestricted[rowIdx] <- res$filteredRestricted
    }
    summaryRows <- c(summaryRows, res$summaryRows)
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

# Save post-processing summary rows (overwrites any previous post-processing output)
if (length(summaryRows) > 0) {
  allSummaryDf <- dplyr::bind_rows(summaryRows)
  saveDatasheet(myScenario, data = allSummaryDf, name = "ecoClassify_SummaryOutput")

  # For the chart, keep only the best post-processing type per context:
  # filtered_reclassified > reclassified > filtered
  bestRowsList <- lapply(c("training", "predicting"), function(ctx) {
    ctxDf <- allSummaryDf[grepl(paste0(" - ", ctx, "$"), allSummaryDf$PredictionType), ]
    if (nrow(ctxDf) == 0) return(NULL)

    types <- ctxDf$PredictionType
    bestType <- if (any(grepl("^filtered_reclassified", types))) {
      paste0("filtered_reclassified - ", ctx)
    } else if (any(grepl("^reclassified", types))) {
      paste0("reclassified - ", ctx)
    } else {
      paste0("filtered - ", ctx)
    }

    updateRunLog(
      paste0("Prediction summary chart: using '", bestType, "' rows for SummaryOutputChart"),
      type = "info"
    )
    ctxDf[ctxDf$PredictionType == bestType, ]
  })

  bestSummaryDf <- dplyr::bind_rows(Filter(Negate(is.null), bestRowsList))
  if (nrow(bestSummaryDf) > 0) {
    saveDatasheet(myScenario, data = bestSummaryDf, name = "ecoClassify_SummaryOutputChart")
  }
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
    predPathType <- NA_character_
    if (!is.na(frc_path) && file.exists(frc_path)) {
      predPath <- frc_path
      predPathType <- "filtered + reclassified"
    } else if (!is.na(urc_path) && file.exists(urc_path)) {
      predPath <- urc_path
      predPathType <- "reclassified"
    } else if (!is.na(flt_path) && file.exists(flt_path)) {
      predPath <- flt_path
      predPathType <- "filtered"
    } else if (!is.na(unf_path) && file.exists(unf_path)) {
      predPath <- unf_path
      predPathType <- "unfiltered"
    }

    if (is.na(predPath) || is.na(truthPath) || !file.exists(truthPath)) next

    updateRunLog(
      paste0("Timestep ", t, ": using ", predPathType, " training raster for model evaluation metrics"),
      type = "info"
    )

    # Use terra::crosstab (long=TRUE) for disk-backed confusion matrix counts.
    # long=TRUE always returns a data.frame regardless of how many class
    # combinations are present, avoiding the 1-D table issue when a class
    # is absent from one of the rasters.
    # The GroundTruth raster written by the training step is already 0/1 (the
    # target class was mapped to 1 by reclassifyGroundTruth), so it is used
    # as-is. Re-applying `== targetClassValue` here zeroed every truth cell
    # whenever the target class value was not 1.
    truth_r <- terra::rast(truthPath)
    ct <- tryCatch(
      terra::crosstab(c(truth_r, terra::rast(predPath)), long = TRUE),
      error = function(e) {
        updateRunLog(paste0("crosstab failed for timestep ", t, ": ", conditionMessage(e)), type = "warning")
        NULL
      }
    )
    rm(truth_r); gc()
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

terra::tmpFiles(remove = TRUE)
