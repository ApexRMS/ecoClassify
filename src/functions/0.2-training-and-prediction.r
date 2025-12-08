## -------------------------------
## ecoClassify - Training and Prediction Functions
## ApexRMS, November 2024
## -------------------------------

skipBadTimesteps <- function(
  trainingRasterList,
  groundTruthRasterList,
  timesteps = NULL, # optional labels for messages
  reprojectForCheck = TRUE, # try to align GT to training grid in-memory
  resampleMethod = "near", # "near" for categorical GT
  requireTwoClasses = TRUE, # enforce >= 2 classes after masking
  verbose = TRUE
) {
  stopifnot(length(trainingRasterList) == length(groundTruthRasterList))
  n <- length(trainingRasterList)

  # Helper to safely coerce to SpatRaster
  asRaster <- function(x) {
    if (inherits(x, "SpatRaster")) {
      return(x)
    }
    try(terra::rast(x), silent = TRUE)
  }

  keptIdx <- logical(n)
  reasons <- character(n)
  nValidCells <- integer(n)
  classStr <- character(n)

  for (i in seq_len(n)) {
    ls <- asRaster(trainingRasterList[[i]])
    gt <- asRaster(groundTruthRasterList[[i]])
    if (inherits(ls, "try-error") || inherits(gt, "try-error")) {
      reasons[i] <- "readFailed"
      next
    }

    # Attempt to align GT to the training grid (no I/O)
    gtCheck <- gt
    if (reprojectForCheck && !terra::compareGeom(ls, gt, stopOnError = FALSE)) {
      gtCheck <- try(
        terra::project(gt, ls, method = resampleMethod),
        silent = TRUE
      )
      if (inherits(gtCheck, "try-error")) {
        reasons[i] <- "projectFailed"
        next
      }
    }

    # Build "valid predictors" mask: non-NA across all training layers
    validMask <- !is.na(ls[[1]])
    if (terra::nlyr(ls) > 1) {
      for (b in 2:terra::nlyr(ls)) {
        validMask <- validMask & !is.na(ls[[b]])
      }
    }

    nValid <- as.integer(terra::global(validMask, "sum", na.rm = TRUE)[[1]])
    nValidCells[i] <- ifelse(is.na(nValid), 0L, nValid)
    if (nValidCells[i] == 0L) {
      reasons[i] <- "noValidPredictorCells"
      next
    }

    # Mask GT to valid predictor cells and check class richness
    gtMasked <- terra::mask(gtCheck, validMask)
    vals <- unique(terra::values(gtMasked))
    vals <- vals[!is.na(vals)]
    classStr[i] <- if (length(vals)) paste(vals, collapse = ",") else ""

    if (requireTwoClasses && length(vals) < 2) {
      reasons[i] <- sprintf(
        "oneClassOnly (%s)",
        ifelse(length(vals), classStr[i], "none")
      )
      next
    }

    keptIdx[i] <- TRUE
  }

  # Assemble outputs
  keptTraining <- trainingRasterList[keptIdx]
  keptGroundTruth <- groundTruthRasterList[keptIdx]
  keptTimesteps <- if (!is.null(timesteps)) timesteps[keptIdx] else NULL

  droppedIdx <- which(!keptIdx)
  dropped <- if (length(droppedIdx)) {
    data.frame(
      index = droppedIdx,
      timestep = if (!is.null(timesteps)) timesteps[droppedIdx] else droppedIdx,
      reason = reasons[droppedIdx],
      nValid = nValidCells[droppedIdx],
      classes = classStr[droppedIdx],
      row.names = NULL
    )
  } else {
    data.frame(
      index = integer(0),
      timestep = integer(0),
      reason = character(0),
      nValid = integer(0),
      classes = character(0)
    )
  }

  if (verbose) {
    message(sprintf(
      "Keeping %d/%d timesteps. Dropped: %d",
      length(keptTraining),
      n,
      nrow(dropped)
    ))
    if (nrow(dropped)) {
      msg <- paste0(
        " - t=",
        dropped$timestep,
        " -> ",
        dropped$reason,
        " [nValid=",
        dropped$nValid,
        "; classes=",
        dropped$classes,
        "]"
      )
      message(paste(msg, collapse = "\n"))
    }
  }

  list(
    trainingRasterList = keptTraining,
    groundTruthRasterList = keptGroundTruth,
    dropped = dropped,
    keptTimesteps = keptTimesteps
  )
}


#' Split raster data into spatially stratified training and testing sets ----
#'
#' @description
#' `splitTrainTest` performs spatially stratified sampling of points from raster data,
#' assigning them to training and testing sets using a grid-based block design. It ensures
#' balanced sampling by block and class (presence/absence) and supports multi-temporal raster stacks.
#'
#' @param trainingRasterList A list of SpatRaster stacks representing training covariates for each timestep.
#' @param groundTruthRasterList A list of SpatRasters representing ground truth (presence/absence) for each timestep.
#' @param nObs Total number of points to sample across the spatial extent (integer).
#' @param nBlocks The number of spatial blocks to divide the area into (must be a perfect square, default = 25).
#' @param proportionTraining Proportion of blocks to assign to the training set (between 0 and 1, default = 0.8).
#'
#' @return A list containing two dataframes:
#' \describe{
#'   \item{train}{Dataframe with training predictors and `presence` column.}
#'   \item{test}{Dataframe with testing predictors and `presence` column.}
#' }
#'
#' @details
#' The spatial extent is divided into equal-sized blocks using `sf::st_make_grid`, then sampling
#' is performed to ensure spatial stratification and class balance. A warning is issued if >50%
#' of points fall outside valid blocks due to NA masking. The function then extracts predictor values
#' at sampled points for all timesteps, combines results across stacks, and returns cleaned datasets
#' with complete cases only.
#'
#' This function supports downstream model training and evaluation by promoting spatial independence
#' between training and test data and reducing spatial autocorrelation bias.
#'
#' @noRd
splitTrainTest <- function(
  trainingRasterList,
  groundTruthRasterList,
  nObs,
  nBlocks = 25,
  proportionTraining = 0.8,
  minMinorityProportion = 0.15,
  maxMinorityProportion = 0.35,
  edgeEnrichment = FALSE,
  spatialBalance = TRUE,
  # tuning knobs for sampling:
  chunk_factor = 5L, # candidates per class per iteration ~ chunk_factor * nObs
  quick_prop_sample = 50000L # how many GT cells to sample to estimate class proportion
) {
  # ---- validation ----
  blockDim <- sqrt(nBlocks)
  if (blockDim != floor(blockDim)) {
    stop("`nBlocks` must be a perfect square, e.g. 25, 100, 144.")
  }
  if (proportionTraining <= 0 || proportionTraining >= 1) {
    stop("`proportionTraining` must be in (0,1).")
  }
  if (minMinorityProportion >= maxMinorityProportion) {
    stop("`minMinorityProportion` must be < `maxMinorityProportion`.")
  }
  if (length(trainingRasterList) != length(groundTruthRasterList)) {
    stop("trainingRasterList and groundTruthRasterList must have same length.")
  }

  # ---- small helpers ----

  # Drop 'ID' column if present (terra::extract quirks)
  strip_id <- function(df) {
    if (!is.null(df) && "ID" %in% names(df)) {
      df <- df[, setdiff(names(df), "ID"), drop = FALSE]
    }
    df
  }

  # Generic "extract at cell ids" for a stack
  extract_df_at_cells <- function(r, cells) {
    xy <- terra::xyFromCell(r, cells)
    strip_id(terra::extract(r, xy))
  }

  # Predictors only
  extractAtCells <- function(rStack, cells) {
    extract_df_at_cells(rStack, cells)
  }

  # Convenience: first-layer values as a vector
  extract_vals_at_cells <- function(r, cells) {
    df <- extract_df_at_cells(r, cells)
    as.vector(df[[1]])
  }

  # ---- estimate class balance from a sample of GT ----
  estimate_classes <- function(r_gt, sample_n = quick_prop_sample) {
    n_total <- terra::ncell(r_gt)

    # sample size bounds
    target_valid <- min(sample_n, max(1000L, floor(0.001 * n_total)), 10000L)

    updateRunLog(
      sprintf(
        "    Estimating class proportions: sampling from %d total cells (target: %d valid samples)...",
        n_total,
        target_valid
      ),
      type = "info"
    )

    oversample_factor <- 3L
    sample_size <- min(n_total, target_valid * oversample_factor)

    cell_ids <- sample.int(n_total, size = sample_size, replace = FALSE)
    vals <- extract_vals_at_cells(r_gt, cell_ids)

    vals_clean <- vals[!is.na(vals)]

    if (length(vals_clean) < 100) {
      stop(
        sprintf(
          "Insufficient valid ground truth samples: only %d valid cells found from %d sampled. ",
          length(vals_clean),
          sample_size
        ),
        "The raster may have too many NA values."
      )
    }

    if (length(vals_clean) > target_valid) {
      vals_clean <- sample(vals_clean, target_valid)
    }

    updateRunLog(
      sprintf("    Successfully obtained %d valid samples", length(vals_clean)),
      type = "info"
    )

    uv <- sort(unique(vals_clean))
    if (length(uv) == 1) {
      stop("Ground truth has only one class in sampled cells.")
    }

    # Map arbitrary 2-level GT to 0/1 if needed
    v <- vals_clean
    if (!all(uv %in% c(0, 1))) {
      v[v == min(uv)] <- 0
      v[v == max(uv)] <- 1
    }

    prop1 <- mean(v == 1, na.rm = TRUE)

    list(
      minorityClass = if (prop1 < 0.5) 1 else 0,
      majorityClass = if (prop1 < 0.5) 0 else 1,
      currentMinorityProp = min(prop1, 1 - prop1)
    )
  }

  # ---- accept–reject sampling for ONE class ----
  sample_valid_for_class <- function(
    r_pred,
    r_gt,
    class_value,
    n_target,
    chunk_size
  ) {
    if (n_target <= 0) {
      return(integer(0))
    }

    got <- integer(0)
    tried <- 0L
    stalled <- 0L
    last_count <- 0L
    n_total <- terra::ncell(r_gt)
    max_iter <- 50L

    while (length(got) < n_target && tried < max_iter) {
      tried <- tried + 1L

      # Adaptive sampling: start small, get more aggressive if needed
      multiplier <- if (tried > 5L && length(got) < n_target * 0.5) 10L else 5L
      size_i <- min(chunk_size, max(n_target * multiplier, 5000L), n_total)

      cand_cells <- sample.int(n_total, size = size_i, replace = FALSE)

      # Filter by class
      gt_vals <- extract_vals_at_cells(r_gt, cand_cells)
      match_class <- !is.na(gt_vals) & (gt_vals == class_value)

      if (!any(match_class)) {
        stalled <- stalled + 1L
        if (stalled >= 10L) {
          updateRunLog(
            sprintf(
              "    Early exit: no class %d cells found after %d attempts",
              class_value,
              tried
            ),
            type = "warning"
          )
          break
        }
        next
      }

      cand_cells <- cand_cells[match_class]

      # Validate predictors at those cells
      pred_vals <- extract_df_at_cells(r_pred, cand_cells)
      keep <- stats::complete.cases(pred_vals)

      acc <- unique(cand_cells[keep])
      if (length(acc)) {
        got <- unique(c(got, acc))
        stalled <- 0L
      }

      if (length(got) == last_count) {
        stalled <- stalled + 1L
        if (stalled >= 10L) {
          updateRunLog(
            sprintf(
              "    Early exit: sampling stalled at %d/%d valid cells after %d iterations",
              length(got),
              n_target,
              tried
            ),
            type = "warning"
          )
          break
        }
      } else {
        last_count <- length(got)
      }
    }

    if (!length(got)) {
      warning(
        "No valid cells found for class ",
        class_value,
        " after sampling."
      )
    }

    got[seq_len(min(length(got), n_target))]
  }

  # ---- edge weights on candidate pool only ----
  edge_weights_for_pool <- function(r_gt, pool_cells) {
    if (!length(pool_cells)) {
      return(numeric(0))
    }

    pairs <- terra::adjacent(r_gt, pool_cells, directions = 4, pairs = TRUE)
    if (is.null(dim(pairs)) || nrow(pairs) == 0) {
      return(rep(1.0, length(pool_cells)))
    }

    uniq_cells <- unique(c(pool_cells, pairs[, 2]))
    values <- extract_vals_at_cells(r_gt, uniq_cells)

    from_vals <- values[match(pairs[, 1], uniq_cells)]
    to_vals <- values[match(pairs[, 2], uniq_cells)]

    diff_pair <- (from_vals != to_vals) & !is.na(from_vals) & !is.na(to_vals)

    any_diff <- tapply(diff_pair, pairs[, 1], any, na.rm = TRUE)

    edge_flag <- rep(FALSE, length(pool_cells))
    m <- match(as.integer(names(any_diff)), pool_cells)
    edge_flag[m[!is.na(m)]] <- any_diff[!is.na(m)]

    ifelse(edge_flag, 2.5, 1.0)
  }

  # ---- storage ----
  n_t <- length(trainingRasterList)
  trainDfs <- vector("list", n_t)
  testDfs <- vector("list", n_t)
  samplingInfoRows <- vector("list", n_t)

  # ---- main loop over timesteps ----
  for (t in seq_len(n_t)) {
    r_pred <- trainingRasterList[[t]]
    r_gt <- groundTruthRasterList[[t]]

    # (1) Estimate classes & current minority proportion
    cls <- estimate_classes(r_gt, sample_n = quick_prop_sample)
    minorityClass <- cls$minorityClass
    majorityClass <- cls$majorityClass
    currentMinorityProp <- cls$currentMinorityProp

    updateRunLog(
      sprintf(
        "[t=%d] (sampled) class distribution: %.1f%% minority (%d), %.1f%% majority (%d)",
        t,
        100 * currentMinorityProp,
        minorityClass,
        100 * (1 - currentMinorityProp),
        majorityClass
      ),
      type = "info"
    )

    # (2) Target sampling proportions with feasibility bounds
    maxFeasibleMinorityProp <- min(
      currentMinorityProp * 2.5, # oversample up to 2.5x
      0.4 # and never > 40% minority
    )

    if (minMinorityProportion > maxFeasibleMinorityProp) {
      updateRunLog(
        sprintf(
          "[t=%d] Warning: Requested min minority %.1f%% exceeds feasible %.1f%% (actual: %.1f%%). Using feasible limit.",
          t,
          100 * minMinorityProportion,
          100 * maxFeasibleMinorityProp,
          100 * currentMinorityProp
        ),
        type = "warning"
      )
    }

    targetMinorityProp <- pmin(
      pmax(currentMinorityProp * 2.5, minMinorityProportion),
      maxMinorityProportion,
      maxFeasibleMinorityProp
    )

    targetMinorityN <- round(nObs * targetMinorityProp)
    targetMajorityN <- max(0, nObs - targetMinorityN)

    updateRunLog(
      sprintf(
        "[t=%d] Target sampling: %d minority (%.1f%%), %d majority (%.1f%%)",
        t,
        targetMinorityN,
        100 * targetMinorityProp,
        targetMajorityN,
        100 * (1 - targetMinorityProp)
      ),
      type = "info"
    )

    # (3) Sample candidates per class (validation against predictor stack inside)
    chunk_size <- max(5000L, chunk_factor * nObs)

    minor_pool <- sample_valid_for_class(
      r_pred,
      r_gt,
      class_value = minorityClass,
      n_target = targetMinorityN,
      chunk_size = chunk_size
    )

    major_pool <- sample_valid_for_class(
      r_pred,
      r_gt,
      class_value = majorityClass,
      n_target = targetMajorityN,
      chunk_size = chunk_size
    )

    if (length(minor_pool) < targetMinorityN) {
      updateRunLog(
        sprintf(
          "[t=%d] Warning: minority pool underfilled (%d/%d).",
          t,
          length(minor_pool),
          targetMinorityN
        ),
        type = "warning"
      )
    }
    if (length(major_pool) < targetMajorityN) {
      updateRunLog(
        sprintf(
          "[t=%d] Warning: majority pool underfilled (%d/%d).",
          t,
          length(major_pool),
          targetMajorityN
        ),
        type = "warning"
      )
    }

    # (4) Optional edge enrichment computed only on pooled candidates
    minProbs <- majProbs <- NULL
    if (edgeEnrichment) {
      updateRunLog(
        sprintf(
          "[t=%d] Computing edge enrichment (minority pool: %d, majority pool: %d)...",
          t,
          length(minor_pool),
          length(major_pool)
        ),
        type = "info"
      )
      if (length(minor_pool)) {
        w_min <- edge_weights_for_pool(r_gt, minor_pool)
        if (!any(is.finite(w_min)) || sum(w_min, na.rm = TRUE) == 0) {
          w_min <- rep(1, length(w_min))
        }
        minProbs <- w_min / sum(w_min)
      }
      if (length(major_pool)) {
        w_maj <- edge_weights_for_pool(r_gt, major_pool)
        if (!any(is.finite(w_maj)) || sum(w_maj, na.rm = TRUE) == 0) {
          w_maj <- rep(1, length(w_maj))
        }
        majProbs <- w_maj / sum(w_maj)
      }
    }

    # (5) Draw the final samples from the validated pools
    sampledMinority <- if (length(minor_pool)) {
      sample(
        minor_pool,
        size = min(targetMinorityN, length(minor_pool)),
        replace = FALSE,
        prob = minProbs
      )
    } else {
      integer(0)
    }

    sampledMajority <- if (length(major_pool)) {
      sample(
        major_pool,
        size = min(targetMajorityN, length(major_pool)),
        replace = FALSE,
        prob = majProbs
      )
    } else {
      integer(0)
    }

    sampledCells <- c(sampledMinority, sampledMajority)

    if (length(sampledCells) < 2) {
      stop(sprintf(
        "Timestep %d: insufficient sampled points (<2). Try lowering nObs or constraints.",
        t
      ))
    }

    # Labels for the sampled cells
    sampledGT <- extract_vals_at_cells(r_gt, sampledCells)

    # (6) Assign spatial blocks
    rc <- terra::rowColFromCell(r_pred, sampledCells)
    rowBins <- cut(rc[, 1], breaks = blockDim, labels = FALSE)
    colBins <- cut(rc[, 2], breaks = blockDim, labels = FALSE)
    blockIds <- (rowBins - 1L) * blockDim + colBins

    pts <- data.frame(
      cell = sampledCells,
      block = as.integer(blockIds),
      presence = as.integer(sampledGT),
      stringsAsFactors = FALSE
    )
    pts <- pts[!is.na(pts$block), , drop = FALSE]

    updateRunLog(
      sprintf(
        "[t=%d] Final sample: %d pts (%.1f%% minority).",
        t,
        nrow(pts),
        100 * mean(pts$presence == minorityClass, na.rm = TRUE)
      ),
      type = "info"
    )

    # (7) Train/test split (spatial blocks or random)
    if (spatialBalance) {
      uniqueBlocks <- unique(pts$block)
      nTrainBlocks <- round(length(uniqueBlocks) * proportionTraining)
      trainBlocks <- if (length(uniqueBlocks)) {
        sample(uniqueBlocks, nTrainBlocks)
      } else {
        integer(0)
      }

      trainPts <- pts[pts$block %in% trainBlocks, , drop = FALSE]
      testPts <- pts[!(pts$block %in% trainBlocks), , drop = FALSE]

      # If either split ends up single-class, fall back to random
      if (
        length(unique(trainPts$presence)) < 2 ||
          length(unique(testPts$presence)) < 2
      ) {
        updateRunLog(
          sprintf(
            "[t=%d] Spatial split class-imbalanced; using random split.",
            t
          ),
          type = "warning"
        )
        tr_idx <- sample(
          seq_len(nrow(pts)),
          size = round(nrow(pts) * proportionTraining)
        )
        trainPts <- pts[tr_idx, , drop = FALSE]
        testPts <- pts[-tr_idx, , drop = FALSE]
      }
    } else {
      tr_idx <- sample(
        seq_len(nrow(pts)),
        size = round(nrow(pts) * proportionTraining)
      )
      trainPts <- pts[tr_idx, , drop = FALSE]
      testPts <- pts[-tr_idx, , drop = FALSE]
    }

    # (8) Extract predictors only at sampled cells
    trainX <- extractAtCells(r_pred, trainPts$cell)
    testX <- extractAtCells(r_pred, testPts$cell)

    trainDf_t <- cbind(trainX, presence = trainPts$presence)
    testDf_t <- cbind(testX, presence = testPts$presence)

    # Clean NAs
    trainDf_t <- trainDf_t[stats::complete.cases(trainDf_t), , drop = FALSE]
    testDf_t <- testDf_t[stats::complete.cases(testDf_t), , drop = FALSE]

    if (nrow(trainDf_t) < 2 || length(unique(trainDf_t$presence)) < 2) {
      stop(sprintf(
        "Timestep %d: insufficient/bad training data after NA filtering.",
        t
      ))
    }

    trainDfs[[t]] <- trainDf_t
    testDfs[[t]] <- testDf_t

    samplingInfoRows[[t]] <- data.frame(
      timestep = t,
      minorityClass = minorityClass,
      majorityClass = majorityClass,
      targetMinorityProportion = targetMinorityProp,
      actualTrainMinorityProportion = mean(trainDf_t$presence == minorityClass),
      actualTestMinorityProportion = mean(testDf_t$presence == minorityClass),
      nTrain = nrow(trainDf_t),
      nTest = nrow(testDf_t)
    )
  }

  # ---- combine across timesteps ----
  trainDf <- do.call(rbind, trainDfs)
  testDf <- do.call(rbind, testDfs)
  samplingInfo <- do.call(rbind, samplingInfoRows)

  updateRunLog(
    sprintf(
      "Final datasets (all timesteps) — Training: %d, Testing: %d",
      nrow(trainDf),
      nrow(testDf)
    ),
    type = "info"
  )

  # final validation
  if (nrow(trainDf) < 2) {
    stop("Insufficient training data (<2 rows) overall.")
  }
  if (length(unique(trainDf$presence)) < 2) {
    stop("Training data must include both classes overall.")
  }

  list(train = trainDf, test = testDf, samplingInfo = samplingInfo)
}


#' Compute metrics at a threshold
#'
#' @description
#' Computes sensitivity, specificity, precision, accuracy, and balanced accuracy
#' for binary labels given a probability threshold.
#'
#' @param probs Numeric vector of predicted probabilities (class 1).
#' @param actual Numeric or integer vector of 0/1 ground-truth labels.
#' @param threshold Numeric scalar in [0, 1]; predict 1 if \code{probs >= threshold}.
#'
#' @return Named numeric vector: \code{sens}, \code{spec}, \code{prec}, \code{acc}, \code{bal}.
#'
#' @noRd
getSensSpec <- function(probs, actual, threshold) {
  # keep only paired, non-NA entries
  ok <- !is.na(probs) & !is.na(actual)
  if (!any(ok)) {
    return(c(sens = NA, spec = NA, prec = NA, acc = NA, bal = NA))
  }

  p <- as.integer(probs[ok] >= threshold)
  a <- as.integer(actual[ok]) # assumes actual is 0/1 already

  TP <- sum(p == 1L & a == 1L)
  TN <- sum(p == 0L & a == 0L)
  FP <- sum(p == 1L & a == 0L)
  FN <- sum(p == 0L & a == 1L)

  denom <- function(x) if (x > 0) x else NA_real_

  sens <- TP / denom(TP + FN) # recall / TPR
  spec <- TN / denom(TN + FP) # TNR
  prec <- TP / denom(TP + FP) # PPV
  acc <- (TP + TN) / (TP + TN + FP + FN)
  bal <- mean(c(sens, spec), na.rm = TRUE) # tolerate one undefined rate

  c(sens = sens, spec = spec, prec = prec, acc = acc, bal = bal)
}

#' Determine optimal classification threshold
#'
#' @description
#' Selects a threshold over \code{0.01..0.99} maximizing an objective
#' (\code{"Youden"}, \code{"Accuracy"}, \code{"Specificity"},
#' \code{"Sensitivity"}, \code{"Precision"}, \code{"Balanced"}),
#' with optional minimum metric constraints.
#'
#' @param model Trained model object (CNN, RF, or MaxEnt).
#' @param testingData Data frame with \code{presence} and predictors.
#' @param modelType One of \code{"Random Forest"}, \code{"MaxEnt"}, \code{"CNN"}.
#' @param objective Objective to maximize; default \code{"Youden"}.
#' @param min_sensitivity Optional minimum sensitivity in [0, 1].
#' @param min_specificity Optional minimum specificity in [0, 1].
#' @param min_precision Optional minimum precision in [0, 1].
#' @param min_accuracy Optional minimum accuracy in [0, 1].
#' @param min_balanced Optional minimum balanced accuracy in [0, 1].
#'
#' @return Numeric threshold in [0, 1].
#'
#' @noRd
getOptimalThreshold <- function(
  model,
  testingData,
  modelType,
  objective = c(
    "Youden",
    "Accuracy",
    "Specificity",
    "Sensitivity",
    "Precision",
    "Balanced"
  ),
  # optional constraints (set to NULL to ignore)
  min_sensitivity = 0.5,
  min_specificity = 0.5,
  min_precision = 0.5,
  min_accuracy = 0.5,
  min_balanced = 0.5
) {
  objective <- match.arg(objective)
  thresholds <- seq(0.01, 0.99, by = 0.01)

  # --- get labels and probabilities (kept close to your original) ---
  if (modelType == "Random Forest") {
    testingObservations <- as.numeric(testingData$presence) - 1L
    p <- predict(model$model, testingData)$predictions
    if (is.data.frame(p) || is.matrix(p)) {
      if ("presence" %in% colnames(p)) {
        testingPredictions <- p[, "presence"]
      } else if (ncol(p) >= 2) {
        testingPredictions <- p[, 2]
      } else {
        testingPredictions <- as.numeric(p)
      }
    } else {
      testingPredictions <- as.numeric(p)
    }
  } else if (modelType == "MaxEnt") {
    testingObservations <- as.numeric(testingData$presence)
    testingPredictions <- predict(model$model, testingData, type = "logistic")
  } else if (modelType == "CNN") {
    testingObservations <- as.numeric(testingData$presence)
    testingPredictions <- predictCNN(model, testingData, isRaster = FALSE)
  } else {
    stop("Model type not recognized")
  }

  valid_idx <- stats::complete.cases(testingPredictions, testingObservations)
  testingPredictions <- as.numeric(testingPredictions[valid_idx])
  testingObservations <- as.numeric(testingObservations[valid_idx])
  if (!length(testingPredictions)) {
    stop("All testing predictions were dropped due to NA.")
  }

  # --- evaluate metric vectors across the same grid (minimal change) ---
  metrics <- t(sapply(
    thresholds,
    getSensSpec,
    probs = testingPredictions,
    actual = testingObservations
  ))
  # metrics has columns: "sens","spec","prec","acc","bal"

  # constraints mask
  keep <- rep(TRUE, length(thresholds))
  if (!is.null(min_sensitivity)) {
    keep <- keep & (metrics[, "sens"] >= min_sensitivity)
  }
  if (!is.null(min_specificity)) {
    keep <- keep & (metrics[, "spec"] >= min_specificity)
  }
  if (!is.null(min_precision)) {
    keep <- keep & (metrics[, "prec"] >= min_precision)
  }
  if (!is.null(min_accuracy)) {
    keep <- keep & (metrics[, "acc"] >= min_accuracy)
  }
  if (!is.null(min_balanced)) {
    keep <- keep & (metrics[, "bal"] >= min_balanced)
  }

  # objective scores (unchanged math; just selection differs)
  score <- switch(
    objective,
    Youden = metrics[, "sens"] + metrics[, "spec"] - 1,
    Accuracy = metrics[, "acc"],
    Specificity = metrics[, "spec"],
    Sensitivity = metrics[, "sens"],
    Precision = metrics[, "prec"],
    Balanced = metrics[, "bal"]
  )

  # apply constraints
  score_constrained <- score
  score_constrained[!keep] <- -Inf

  if (all(!is.finite(score_constrained))) {
    warning(
      "No threshold satisfies the provided constraints; returning unconstrained optimum."
    )
    best_idx <- which.max(score) # fallback
  } else {
    best_idx <- which.max(score_constrained)
  }

  thresholds[best_idx]
}

#' Generate binary presence and probability rasters from model predictions ----
#'
#' @description
#' `getPredictionRasters` applies a trained model to a multi-band raster input and returns
#' both a binary presence/absence raster and a continuous probability raster. It supports
#' multiple model types including Random Forest, MaxEnt, and CNN.
#'
#' @param raster A SpatRaster containing predictor layers to classify.
#' @param model A trained model object, such as the output from `getRandomForestModel()`,
#' `getMaxentModel()`, or `getCNNModel()`. The structure must include at least `$model`.
#' @param threshold A numeric value (0–1) used to convert predicted probabilities into binary
#' presence/absence classifications.
#' @param modelType A string specifying the type of model to apply: `"Random Forest"`,
#' `"MaxEnt"`, or `"CNN"`. Case sensitive.
#'
#' @return A list containing two SpatRaster objects:
#' \describe{
#'   \item{[[1]]}{Binary presence raster based on thresholded predictions.}
#'   \item{[[2]]}{Continuous probability raster from the model output.}
#' }
#'
#' @details
#' Internally, this function dispatches to the appropriate prediction method based on `modelType`:
#' \itemize{
#'   \item Random Forest: calls `predictRanger()` and handles factor alignment.
#'   \item MaxEnt: calls `predict()` from the `dismo` or `ENMeval` package.
#'   \item CNN: calls `predictCNN()` to run forward passes through a torch-based model.
#' }
#' The output probability raster is reclassified into binary form using the supplied threshold via `reclassifyRaster()`.
#' This function is commonly used when generating per-timestep prediction maps across a study area.
#' @noRd
getPredictionRasters <- function(
  raster,
  model,
  threshold,
  modelType = "Random Forest"
) {
  # predict presence for each raster
  if (modelType == "Random Forest") {
    # generate probabilities for each raster using ranger
    probabilityRaster <- predictRanger(raster, model)
  } else if (modelType == "CNN") {
    probabilityRaster <- predictCNN(model, raster)
  } else if (modelType == "MaxEnt") {
    probabilityRaster <- predict(model$model, raster, type = "logistic")
  } else {
    stop("Model type not recognized")
  }

  predictedPresence <- reclassifyRaster(probabilityRaster, threshold)

  return(list(predictedPresence, probabilityRaster))
}

#' Reclassify raster to binary presence/absence ----
#'
#' @description
#' 'reclassifyRaster' converts a continuous raster of probabilities into
#' a binary presence/absence raster based on a given threshold.
#'
#' @param raster Raster to reclassify (spatRaster).
#' @param threshold Threshold above which presence is assigned (numeric).
#' @return Binary raster (spatRaster).
#'
#' @details
#' Used to convert probability outputs from classifiers into discrete predictions.
#' @noRd
reclassifyRaster <- function(raster, threshold) {
  raster[raster >= threshold] <- 1
  raster[raster < threshold] <- 0

  return(raster)
}

#' Generate and document prediction raster outputs ----
#'
#' @description
#' `generateRasterDataframe` saves predicted presence and probability rasters to disk
#' and builds a structured dataframe row referencing these files for SyncroSim output
#' datasheets. Optionally applies spatial filtering to reduce spurious presence pixels
#' based on neighborhood context.
#'
#' @param predictedPresence A SpatRaster object representing predicted binary presence values.
#' @param category Category label used in file naming (e.g., "predicting" or "training").
#' @param timestep Integer representing the current model timestep.
#' @param transferDir File path to the directory where rasters will be saved.
#' @param OutputDataframe Existing dataframe to which output file references will be appended.
#' @param hasGroundTruth Logical; whether ground truth data is available for this timestep.
#'
#' @return A dataframe with paths to raster output files, including predicted presence
#' (filtered and unfiltered), probability raster, and optionally ground truth raster. The
#' structure of the dataframe differs depending on whether `hasGroundTruth` is `TRUE`.
#'
#' @details
#' If `applyFiltering` is `TRUE`, a focal filter is applied using `filterFun()` with a square
#' window (default 5x5) to suppress isolated presence pixels. Filtered rasters are saved with
#' `filteredPredictedPresence-*.tif` naming. If ground truth is available, the resulting
#' dataframe includes columns: `Timestep`, `PredictedUnfiltered`, `PredictedFiltered`,
#' `GroundTruth`, and `Probability`. If not, column names are `ClassifiedUnfiltered`,
#' `ClassifiedFiltered`, and `ClassifiedProbability` instead.
#'
#' Used when preparing outputs for the `ecoClassify_RasterOutput` SyncroSim datasheet.
#' @noRd
generateRasterDataframe <- function(
  predictedPresence,
  category,
  timestep,
  transferDir,
  OutputDataframe,
  hasGroundTruth
) {
  if (hasGroundTruth == TRUE) {
    # build dataframe
    rasterDataframe <- data.frame(
      Timestep = timestep,
      PredictedUnfiltered = file.path(paste0(
        transferDir,
        "/PredictedPresence-",
        category,
        "-t",
        timestep,
        ".tif"
      )),
      PredictedFiltered = "",
      GroundTruth = file.path(paste0(
        transferDir,
        "/GroundTruth-t",
        timestep,
        ".tif"
      )),
      Probability = file.path(paste0(
        transferDir,
        "/Probability-",
        category,
        "-t",
        timestep,
        ".tif"
      ))
    )
    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe, rasterDataframe)
  } else {
    # build dataframe
    rasterDataframe <- data.frame(
      Timestep = timestep,
      ClassifiedUnfiltered = file.path(paste0(
        transferDir,
        "/PredictedPresence-",
        category,
        "-t",
        timestep,
        ".tif"
      )),
      ClassifiedFiltered = "",
      ClassifiedProbability = file.path(paste0(
        transferDir,
        "/Probability-",
        category,
        "-t",
        timestep,
        ".tif"
      ))
    )

    # add to output dataframe
    OutputDataframe <- addRow(OutputDataframe, rasterDataframe)
  }

  return(OutputDataframe)
}


#' Append RGB image path to output dataframe ---
#'
#' @description
#' `getRgbDataframe` creates a new row referencing an RGB image file for a given
#' timestep and appends it to the output dataframe used by the SyncroSim `RgbOutput`
#' datasheet.
#'
#' @param rgbOutputDataframe A dataframe used to collect output paths for RGB images
#' across timesteps.
#' @param category A string specifying the image category (e.g., `"predicting"` or `"training"`).
#' This is used in the image filename.
#' @param timestep Integer value indicating the timestep of the image.
#' @param transferDir Directory path where the image is stored.
#'
#' @return A dataframe with a new row containing the `Timestep` and corresponding
#' `RGBImage` file path.
#'
#' @details
#' This function assumes that the corresponding RGB image has already been saved
#' to the specified `transferDir` using a consistent filename format:
#' `"RGBImage-{category}-t{timestep}.png"`. It is used in workflows that require
#' visual inspection of RGB representations of predictor data at each timestep.
#' @noRd
getRgbDataframe <- function(
  rgbOutputDataframe,
  category,
  timestep,
  transferDir
) {
  rgbDataframe <- data.frame(
    Timestep = timestep,
    RGBImage = file.path(paste0(
      transferDir,
      "/RGBImage-",
      category,
      "-t",
      timestep,
      ".png"
    ))
  )

  rgbOutputDataframe <- addRow(rgbOutputDataframe, rgbDataframe)

  return(rgbOutputDataframe)
}

#' Save raster and RGB image files to disk ----
#'
#' @description
#' `saveFiles` writes out the predicted presence raster, probability raster,
#' optional ground truth raster, and a PNG RGB image to the specified
#' transfer directory. This prepares output artifacts for linkage with
#' SyncroSim datasheets or visual inspection.
#'
#' @param predictedPresence A SpatRaster representing binary presence/absence predictions.
#' @param groundTruth Optional SpatRaster containing ground truth presence values
#' (can be NULL if unavailable).
#' @param probabilityRaster A SpatRaster containing continuous probability predictions.
#' @param trainingRaster A SpatRaster used to generate the RGB image.
#' @param category A character string used to label file outputs (e.g., "training" or "predicting").
#' @param timestep Integer indicating the current timestep for file naming.
#' @param transferDir File path to the directory where outputs will be written.
#'
#' @return None. This function performs file I/O only.
#'
#' @details
#' The function saves three GeoTIFF rasters:
#' \itemize{
#'   \item `PredictedPresence-{category}-t{timestep}.tif`
#'   \item `Probability-{category}-t{timestep}.tif`
#'   \item `GroundTruth-t{timestep}.tif` (optional)
#' }
#' Additionally, a PNG RGB image is generated using bands 3 (R), 2 (G), and 1 (B)
#' from the training raster and saved as `RGBImage-{category}-t{timestep}.png`.
#' @noRd
saveFiles <- function(
  predictedPresence,
  groundTruth = NULL,
  probabilityRaster,
  trainingRaster,
  category,
  timestep,
  transferDir
) {
  # save rasters
  writeRaster(
    predictedPresence,
    filename = file.path(paste0(
      transferDir,
      "/PredictedPresence-",
      category,
      "-t",
      timestep,
      ".tif"
    )),
    overwrite = TRUE
  )

  if (!is.null(groundTruth)) {
    writeRaster(
      groundTruth,
      filename = file.path(paste0(
        transferDir,
        "/GroundTruth-t",
        timestep,
        ".tif"
      )),
      overwrite = TRUE
    )
  }
  writeRaster(
    probabilityRaster,
    filename = file.path(paste0(
      transferDir,
      "/Probability-",
      category,
      "-t",
      timestep,
      ".tif"
    )),
    overwrite = TRUE
  )

  # save RBG Image
  # prefer B03,B02,B01 if present; otherwise fall back to first three layers
  rgb_idx <- c(3, 2, 1)
  if (terra::nlyr(trainingRaster) >= 3) {
    rgb_rast <- trainingRaster[[rgb_idx]]
  } else if (terra::nlyr(trainingRaster) == 2) {
    rgb_rast <- c(trainingRaster[[2]], trainingRaster[[1]], trainingRaster[[1]])
  } else if (terra::nlyr(trainingRaster) == 1) {
    rgb_rast <- c(trainingRaster[[1]], trainingRaster[[1]], trainingRaster[[1]])
  } else {
    updateRunLog(
      sprintf(
        "t=%s: training raster has 0 layers; skipping RGB PNG.",
        timestep
      ),
      type = "warning"
    )
    return(invisible(NULL))
  }

  # convert factors to numeric for plotting (plotRGB needs numeric values)
  if (any(terra::is.factor(rgb_rast))) {
    rgb_rast <- terra::as.numeric(rgb_rast)
  }

  # sanity: ensure exactly 3 layers
  stopifnot(terra::nlyr(rgb_rast) == 3)

  # --- plot PNG safely ---
  png_file <- file.path(paste0(
    transferDir,
    "/RGBImage-",
    category,
    "-t",
    timestep,
    ".png"
  ))
  grDevices::png(filename = png_file)
  tryCatch(
    {
      terra::plotRGB(rgb_rast, r = 1, g = 2, b = 3, stretch = "lin")
    },
    error = function(e) {
      updateRunLog(
        sprintf(
          "t=%s: Failed to write RGB PNG: %s",
          timestep,
          conditionMessage(e)
        ),
        type = "warning"
      )
    }
  )
}
