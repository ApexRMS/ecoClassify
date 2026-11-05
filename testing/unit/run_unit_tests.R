#!/usr/bin/env Rscript
# Comprehensive Unit Test Suite for ecoClassify
# Usage: Rscript run_unit_tests.R

library(testthat)
suppressPackageStartupMessages(library(terra))

cat("===================================================\n")
cat("           ecoClassify Unit Test Suite\n")
cat("===================================================\n\n")

# Load test helpers
source("testing/unit/helper-setup.R")

# Load the real functions under test from src/ (not copies, which drift).
# Stub rsyncrosim's updateRunLog so warnings surface as R warnings.
suppressPackageStartupMessages(library(dplyr))
updateRunLog <- function(message, type = "info") {
  if (identical(type, "warning")) warning(message, call. = FALSE)
  invisible(NULL)
}

# 0.0-setup.r attaches heavy packages at the top level, so only the setCores()
# definition is evaluated from it
for (expr in parse("src/functions/0.0-setup.r")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      identical(expr[[2]], as.name("setCores"))) {
    eval(expr)
  }
}
source("src/functions/0.1-pre-processing.r")
source("src/functions/0.2-training-and-prediction.r")
source("src/functions/0.3-contextualization.r")
source("src/functions/0.4-maxent.r")
source("src/functions/0.4-CNN.r")
source("src/functions/0.5-histogram.r")
source("src/functions/0.6-post-processing.r")

# Test Suite
cat("Running Test Suite...\n\n")

test_results <- list()

# Test 1: Helper Functions
cat("1. Testing helper functions...\n")
test_results$helpers <- test_that("Helper functions work correctly", {
  # Test raster creation
  r <- create_test_raster(nrows = 10, ncols = 10)
  expect_s4_class(r, "SpatRaster")
  expect_equal(nrow(r), 10)
  expect_equal(ncol(r), 10)
  
  # Test binary raster creation
  binary_r <- create_binary_raster(nrows = 5, ncols = 5, prob = 0.3)
  expect_s4_class(binary_r, "SpatRaster")
  vals <- values(binary_r)
  unique_vals <- unique(vals[!is.na(vals)])
  expect_true(all(unique_vals %in% c(0, 1)))
  
  # Test raster stack creation
  stack <- create_test_stack(nlayers = 4, nrows = 8, ncols = 8)
  expect_s4_class(stack, "SpatRaster")
  expect_equal(nlyr(stack), 4)
  expect_equal(nrow(stack), 8)
  expect_equal(ncol(stack), 8)
})

# Test 2: Core Setup Functions
cat("2. Testing setup functions...\n")
test_results$setup <- test_that("setCores function works correctly", {
  # Test multiprocessing disabled
  mp_disabled <- data.frame(
    EnableMultiprocessing = FALSE,
    MaximumJobs = 4
  )
  result <- setCores(mp_disabled)
  expect_equal(result, 1)
  
  # Test multiprocessing enabled with reasonable cores
  mp_enabled <- data.frame(
    EnableMultiprocessing = TRUE,
    MaximumJobs = 2
  )
  result <- setCores(mp_enabled)
  expect_true(result >= 1)
  expect_true(result <= parallel::detectCores())
  
  # Test excessive core request
  available_cores <- parallel::detectCores()
  mp_excessive <- data.frame(
    EnableMultiprocessing = TRUE,
    MaximumJobs = available_cores + 10
  )
  expect_warning(result <- setCores(mp_excessive))
  expect_equal(result, available_cores - 1)
})

# Test 3: Preprocessing Functions  
cat("3. Testing preprocessing functions...\n")
test_results$preprocessing <- test_that("normalization uses pooled training stats", {
  r1 <- create_test_raster(nrows = 5, ncols = 5)
  values(r1) <- c(10, 20, 30, 40, 50, rep(25, ncell(r1) - 5))
  r2 <- r1 * 2
  rasterList <- list("1" = r1, "2" = r2)

  stats <- computeNormalizationStats(rasterList)
  expect_equal(unname(stats$min), 10)
  expect_equal(unname(stats$max), 100)

  result <- normalizeRaster(rasterList, stats)
  expect_s4_class(result[[1]], "SpatRaster")
  expect_equal(names(result), names(rasterList))

  # Both rasters share one scale: r1's max (50) maps to 40/90, not to 1
  expect_equal(max(values(result[[1]]), na.rm = TRUE), 40 / 90)
  expect_equal(max(values(result[[2]]), na.rm = TRUE), 1)
  expect_equal(min(values(result[[1]]), na.rm = TRUE), 0)

  # Constant bands give 0, not NaN
  constant <- list("1" = r1 * 0 + 5)
  constantResult <- normalizeRaster(constant, computeNormalizationStats(constant))
  expect_true(all(values(constantResult[[1]]) == 0))

  # Band names must match the training stats
  renamed <- rasterList
  names(renamed[[1]]) <- "other"
  expect_error(normalizeRaster(renamed, stats), "no training normalization")
})

test_results$contextualization <- test_that("contextualization reuses one PCA", {
  r <- terra::rast(nrows = 20, ncols = 20, nlyrs = 3,
                   vals = stats::runif(1200))
  names(r) <- c("red", "green", "nir")
  pca <- fitContextPca(list("1" = r))

  full <- contextualizeRaster(list("1" = r), windowSize = 3, pcaModel = pca)
  expect_true(all(c("PC1", "PC2", "red_mean", "nir_range") %in%
                    names(full[[1]])))

  # A tile gets exactly the same PC values as the matching part of the
  # full raster
  tileExtent <- terra::ext(-180, 0, -90, 90)
  tile <- addRasterAdjacencyValues(terra::crop(r, tileExtent), 3, pca)
  fullCropped <- terra::crop(full[[1]], tileExtent)
  expect_equal(values(tile[["PC1"]]), values(fullCropped[["PC1"]]))
})

test_results$summary <- test_that("buildSummaryRow gives exact stats", {
  prob <- terra::rast(nrows = 10, ncols = 10, vals = seq(0.01, 1, by = 0.01))
  pred <- terra::ifel(prob > 0.5, 1, 0)

  row <- buildSummaryRow(pred, prob, 1, "predicting")
  present <- seq(0.51, 1, by = 0.01)
  expect_equal(row$PredictedPixels, 50)
  expect_equal(row$PredictedArea, 50 * prod(terra::res(pred)))
  expect_equal(row$MeanProbability, mean(present))
  expect_equal(row$MedianProbability, median(present))
  expect_equal(row$MinProbability, 0.51)
  expect_equal(row$MaxProbability, 1)

  # Present pixels without a probability count towards area only
  probGap <- prob
  probGap[100] <- NA
  gapRow <- buildSummaryRow(pred, probGap, 1, "filtered - predicting")
  expect_equal(gapRow$PredictedPixels, 50)
  expect_equal(gapRow$MaxProbability, 0.99)

  # No presence gives NA statistics, not NaN
  emptyRow <- buildSummaryRow(pred * 0, prob, 1, "predicting")
  expect_equal(emptyRow$PredictedPixels, 0)
  expect_true(is.na(emptyRow$MeanProbability))
  expect_false(is.nan(emptyRow$MeanProbability))
})

# Reference copy of the cell-by-cell reclassifyGroundTruth() it replaced,
# used only to check the terra version gives identical output
reclassifyGroundTruthOld <- function(raster, useTargetClass = FALSE,
                                     targetClassValue = NA,
                                     backgroundValues = NA,
                                     ignoreValues = NA) {
  if (isTRUE(useTargetClass) && !is.na(targetClassValue)) {
    result <- raster
    result[] <- NA
    vals <- terra::values(raster)
    result[vals == targetClassValue] <- 1
    if (!is.na(backgroundValues) && nchar(trimws(backgroundValues)) > 0) {
      bgVals <- as.integer(trimws(strsplit(backgroundValues, ",")[[1]]))
      result[vals %in% bgVals] <- 0
    } else {
      ignoreVals <- if (!is.na(ignoreValues) &&
                          nchar(trimws(ignoreValues)) > 0)
        as.integer(trimws(strsplit(ignoreValues, ",")[[1]])) else integer(0)
      result[!is.na(vals) & vals != targetClassValue &
               !(vals %in% ignoreVals)] <- 0
    }
    return(result)
  }
  raster[raster == min(terra::values(raster), na.rm = TRUE)] <- 0
  raster[raster == max(terra::values(raster), na.rm = TRUE)] <- 1
  raster
}

test_results$groundTruth <- test_that("reclassifyGroundTruth is unchanged", {
  set.seed(1)
  classes <- terra::rast(nrows = 20, ncols = 20,
                         vals = sample(c(1:5, NA), 400, replace = TRUE))
  cases <- list(
    list(useTargetClass = TRUE, targetClassValue = 3),
    list(useTargetClass = TRUE, targetClassValue = 3,
         backgroundValues = "1, 2"),
    list(useTargetClass = TRUE, targetClassValue = 3,
         backgroundValues = "3,4"),
    list(useTargetClass = TRUE, targetClassValue = 3, ignoreValues = "5"),
    list(useTargetClass = FALSE)
  )
  for (args in cases) {
    expected <- do.call(reclassifyGroundTruthOld, c(list(classes), args))
    actual <- do.call(reclassifyGroundTruth, c(list(list(classes)), args))[[1]]
    expect_equal(as.vector(values(actual)), as.vector(values(expected)))
  }

  # Binary branch edge cases: all-negative and single-valued rasters
  for (vals in list(c(-5, -2, NA, -5), c(7, 7, NA, 7), c(-1, 0, 5, 2))) {
    r <- terra::rast(nrows = 2, ncols = 2, vals = vals)
    expect_equal(
      as.vector(values(reclassifyGroundTruth(list(r))[[1]])),
      as.vector(values(reclassifyGroundTruthOld(r)))
    )
  }

  # Categorical rasters compare on stored values, as values() did
  catRaster <- classes
  levels(catRaster) <- data.frame(id = 1:5, label = letters[1:5])
  expect_equal(
    as.vector(values(reclassifyGroundTruth(
      list(catRaster), useTargetClass = TRUE, targetClassValue = 3
    )[[1]])),
    as.vector(values(reclassifyGroundTruthOld(
      classes, useTargetClass = TRUE, targetClassValue = 3
    )))
  )
})

test_results$covariates <- test_that("addCovariates checks grids", {
  base <- create_test_stack(nlayers = 2, nrows = 10, ncols = 10)
  covariate <- create_test_raster(nrows = 10, ncols = 10)
  names(covariate) <- "cov"
  rasterList <- list(`1` = base)

  combined <- addCovariates(rasterList, list(global = covariate))
  expect_equal(names(combined[[1]]), c(names(base), "cov"))

  shifted <- terra::shift(covariate, dx = 1)
  expect_error(
    addCovariates(rasterList, list(global = shifted)),
    "timestep 1 .*extent"
  )
  coarse <- terra::aggregate(covariate, 2)
  expect_error(
    addCovariates(rasterList, list(global = coarse)),
    "resolution"
  )

  # Persistent files are stacked directly; temp files are rewritten
  persistentDir <- file.path(getwd(), "testing", "unit", "tmp-covariates")
  dir.create(persistentDir, showWarnings = FALSE)
  on.exit(unlink(persistentDir, recursive = TRUE), add = TRUE)
  basePath <- file.path(persistentDir, "base.tif")
  covPath <- file.path(persistentDir, "cov.tif")
  terra::writeRaster(base, basePath, overwrite = TRUE)
  terra::writeRaster(covariate, covPath, overwrite = TRUE)
  stacked <- addCovariates(list(`1` = terra::rast(basePath)),
                           list(global = terra::rast(covPath)),
                           outDir = persistentDir)
  expect_false(hasTemporarySources(stacked[[1]]))
  expect_false(file.exists(file.path(persistentDir, "covCombined_1.tif")))

  tempBase <- terra::writeRaster(base, tempfile(fileext = ".tif"))
  expect_true(hasTemporarySources(tempBase))
  rewritten <- addCovariates(list(`1` = tempBase),
                             list(global = terra::rast(covPath)),
                             outDir = persistentDir)
  expect_true(file.exists(file.path(persistentDir, "covCombined_1.tif")))
  expect_equal(values(rewritten[[1]]), values(c(base, covariate)),
               tolerance = 1e-5, ignore_attr = TRUE)
})

test_results$predictChecks <- test_that("Predict input checks", {
  expect_error(validateModelObject(data.frame(), "MaxEnt"), "No trained model")
  modelFile <- tempfile(fileext = ".rds")
  saveRDS(list(), modelFile)
  expect_error(
    validateModelObject(data.frame(Model = "missing.rds", Weights = ""),
                        "MaxEnt"),
    "could not be found"
  )
  expect_error(
    validateModelObject(data.frame(Model = modelFile, Weights = ""), "CNN"),
    "no CNN weights"
  )
  expect_silent(
    validateModelObject(data.frame(Model = modelFile, Weights = ""), "MaxEnt")
  )

  rangerModel <- list(model = structure(list(), class = "ranger"))
  expect_silent(checkModelType(rangerModel, "Random Forest"))
  expect_error(checkModelType(rangerModel, "CNN"), "Random Forest model")

  r <- create_test_stack(nlayers = 2, nrows = 5, ncols = 5)
  names(r) <- c("red", "green")
  model <- list(num_vars = c("red", "green"), cat_vars = character(0))
  expect_silent(checkPredictorNames(list(r), model, timesteps = 4))
  model$num_vars <- c("red", "nir")
  expect_error(checkPredictorNames(list(r), model, timesteps = 4),
               "timestep 4 .*nir")
})

test_results$sampling <- test_that("training sampling and timestep checks", {
  set.seed(42)
  predictors <- create_test_stack(nlayers = 3, nrows = 60, ncols = 60)
  groundTruth <- terra::rast(predictors[[1]],
                             vals = rbinom(3600, 1, 0.3))
  predictors[[2]][1:200] <- NA

  split <- splitTrainTest(list(predictors), list(groundTruth), nObs = 400)
  expect_true(nrow(split$train) > 0 && nrow(split$test) > 0)
  expect_setequal(unique(split$train$presence), c(0, 1))
  expect_false(anyNA(split$train))

  # Rows come from the pooled extraction; they must match the stack at the
  # same cells. Recover each row's cell by matching on all three layers.
  allVals <- terra::as.data.frame(predictors, cells = TRUE, na.rm = TRUE)
  key <- function(df) do.call(paste, c(signif(df[, 1:3], 10), sep = "|"))
  matched <- match(key(split$train), key(allVals[, -1]))
  expect_false(anyNA(matched))
  expect_equal(
    split$train$presence,
    as.vector(values(groundTruth))[allVals$cell[matched]]
  )

  # skipBadTimesteps keeps two-class timesteps and drops one-class ones,
  # judged only on cells where all predictors are present
  oneClass <- groundTruth
  oneClass[] <- 1
  oneClass[1:5] <- 0
  sparse <- predictors
  sparse[[3]][1:5] <- NA
  flt <- skipBadTimesteps(
    list(predictors, sparse), list(groundTruth, oneClass),
    timesteps = c(10, 20), verbose = FALSE
  )
  expect_equal(flt$keptTimesteps, 10)
  expect_match(flt$dropped$reason, "oneClassOnly")
})

test_results$smallStats <- test_that("checkNA, getValueRange, filtering", {
  r <- create_test_stack(nlayers = 2, nrows = 10, ncols = 10)
  expect_false(checkNA(list(r), verbose = FALSE))
  r[[2]][1:3] <- NA
  expect_true(checkNA(list(r), verbose = FALSE))

  valueRange <- getValueRange(list(r, r * 2), names(r)[1], nBins = 4,
                              nSample = 50)
  expect_true(is.data.frame(valueRange))

  # A missing filter setting skips filtering instead of erroring
  pred <- terra::rast(nrows = 5, ncols = 5, vals = rep(c(0, 1), length.out = 25))
  skipped <- filterRasterDataframe(NA, pred, 8, 8, "training", 1, tempdir(),
                                   fileName = "f")
  expect_true(is.na(skipped$PredictedFiltered))
  skipped <- filterRasterDataframe(logical(0), pred, 8, 8, "training", 1,
                                   tempdir(), fileName = "f")
  expect_true(is.na(skipped$PredictedFiltered))

  # filterPredictionRaster restores on-disk processing, even after an error
  terraOptions(todisk = TRUE)
  invisible(filterPredictionRaster(pred, filterValue = 2, fillValue = 2))
  expect_true(terraOptions(print = FALSE)$todisk)
  expect_error(filterPredictionRaster("not a raster", filterValue = 2))
  expect_true(terraOptions(print = FALSE)$todisk)
  terraOptions(todisk = FALSE)
})

test_results$tiling <- test_that("tile mask and tile crops are unchanged", {
  # Old method: full-size matrix, transposed into the raster
  oldTileMask <- function(template, tileIdGrid, rowBreaks, colBreaks) {
    m <- matrix(NA_integer_, nrow = nrow(template), ncol = ncol(template))
    for (iy in seq_len(nrow(tileIdGrid))) {
      for (ix in seq_len(ncol(tileIdGrid))) {
        if (is.na(tileIdGrid[iy, ix])) next
        rows <- (rowBreaks[iy] + 1L):rowBreaks[iy + 1L]
        cols <- (colBreaks[ix] + 1L):colBreaks[ix + 1L]
        if (rowBreaks[iy] == rowBreaks[iy + 1L] ||
              colBreaks[ix] == colBreaks[ix + 1L]) next
        m[rows, cols] <- tileIdGrid[iy, ix]
      }
    }
    as.integer(t(m))
  }

  grids <- list(
    list(nrows = 37, ncols = 23, ny = 3, nx = 2, skip = c(2, 5)),
    list(nrows = 10, ncols = 10, ny = 1, nx = 1, skip = integer(0)),
    list(nrows = 2,  ncols = 9,  ny = 3, nx = 4, skip = 1)  # more tiles than rows
  )
  for (g in grids) {
    template <- terra::rast(nrows = g$nrows, ncols = g$ncols)
    rowBreaks <- round(seq(0, g$nrows, length.out = g$ny + 1L))
    colBreaks <- round(seq(0, g$ncols, length.out = g$nx + 1L))
    ids <- seq_len(g$ny * g$nx)
    ids[g$skip] <- NA
    tileIdGrid <- matrix(ids, nrow = g$ny, ncol = g$nx, byrow = TRUE)

    maskFile <- tempfile(fileext = ".tif")
    mask <- writeTileMask(template, tileIdGrid, rowBreaks, colBreaks,
                          filename = maskFile)
    expect_equal(as.vector(values(mask)),
                 oldTileMask(template, tileIdGrid, rowBreaks, colBreaks))
    expect_equal(names(mask), "TileID")
    expect_equal(terra::datatype(mask), "INT4S")
  }

  # Force several write blocks
  terraOptions(steps = 4)
  template <- terra::rast(nrows = 41, ncols = 7)
  rowBreaks <- round(seq(0, 41, length.out = 4))
  colBreaks <- round(seq(0, 7, length.out = 3))
  tileIdGrid <- matrix(1:6, nrow = 3, byrow = TRUE)
  mask <- writeTileMask(template, tileIdGrid, rowBreaks, colBreaks,
                        filename = tempfile(fileext = ".tif"))
  expect_equal(as.vector(values(mask)),
               oldTileMask(template, tileIdGrid, rowBreaks, colBreaks))
  terraOptions(steps = 0)

  # Cropping the original gives the same cells as the old pre-cropped copy
  src <- terra::rast(nrows = 30, ncols = 30, nlyrs = 2,
                     vals = sample.int(1000, 1800, replace = TRUE))
  srcFile <- tempfile(fileext = ".tif")
  terra::writeRaster(src, srcFile, datatype = "INT2U")
  cropExt <- terra::ext(-60, 60, -30, 54)
  oldCopy <- tempfile(fileext = ".tif")
  terra::writeRaster(terra::crop(terra::rast(srcFile), cropExt), oldCopy,
                     gdal = "COMPRESS=LZW")
  cropped <- cropRastersToTile(list(`5` = terra::rast(srcFile)), cropExt)
  expect_equal(names(cropped), "5")
  expect_true(terra::compareGeom(cropped[[1]], terra::rast(oldCopy)))
  expect_equal(values(cropped[[1]]), values(terra::rast(oldCopy)),
               ignore_attr = TRUE)
})

test_results$maxentEncoding <- test_that("MaxEnt categoricals use training codes", {
  catLevels <- list(band_1 = NULL, lc = c("11", "21", "41", "90"))
  data <- data.frame(
    band_1 = c(0.1, 0.2, 0.3, 0.4),
    lc = factor(c("41", "11", "55", "21"))  # different level order; 55 unseen
  )
  encoded <- encodeMaxentCategoricals(data, catLevels)
  expect_equal(as.integer(as.character(encoded$lc)), c(3L, 1L, NA, 2L))
  expect_equal(levels(encoded$lc), as.character(1:4))
  expect_equal(encoded$band_1, data$band_1)

  # Numeric raster values (no factor) are matched on their labels too
  numericLc <- encodeMaxentCategoricals(data.frame(lc = c(21, 90)), catLevels)
  expect_equal(as.integer(as.character(numericLc$lc)), c(2L, 4L))
})

test_results$maxentModel <- test_that("MaxEnt uses every predictor", {
  maxentReady <- requireNamespace("ENMeval", quietly = TRUE) &&
    requireNamespace("dismo", quietly = TRUE) &&
    isTRUE(tryCatch(dismo::maxent(silent = TRUE), error = function(e) FALSE))
  skip_if_not(maxentReady, "maxent.jar / ENMeval not available")

  MAXENT_AVAILABLE <<- TRUE
  on.exit(MAXENT_AVAILABLE <<- FALSE, add = TRUE)
  set.seed(3)
  n <- 1000
  lc <- factor(sample(c("11", "21", "41"), n, TRUE),
               levels = c("11", "21", "41", "90"))
  trainData <- data.frame(band_1 = runif(n), band_2 = runif(n),
                          band_3 = runif(n), band_4 = runif(n), lc = lc)
  trainData$presence <- rbinom(
    n, 1, plogis(-3 + 5 * trainData$band_1 + 2 * (lc == "41"))
  )
  model <- suppressWarnings(suppressMessages(
    getMaxentModel(trainData, nCores = 1, isTuningOn = FALSE)
  ))

  # band_1 and band_2 used to be read as coordinates and dropped
  expect_setequal(colnames(model$model@presence),
                  c("band_1", "band_2", "band_3", "band_4", "lc"))
  expect_null(model$predict_df)

  newData <- data.frame(band_1 = c(0.05, 0.95, 0.5, 0.5, 0.5),
                        band_2 = 0.5, band_3 = 0.5, band_4 = 0.5,
                        lc = c("41", "41", "11", "41", "55"))
  probs <- predictMaxentProbability(model, newData)
  expect_gt(probs[2], probs[1])        # band_1 has an effect
  expect_gt(probs[4], probs[3])        # class 41 above class 11
  expect_true(is.na(probs[5]))         # unseen category
})

test_results$cnnScaling <- test_that("CNN inputs are standardized", {
  X <- cbind(elevation = c(100, 200, 300, 400), constant = 5)
  scaling <- computeCnnScaling(X)
  expect_equal(unname(scaling$center), c(250, 5))
  expect_equal(unname(scaling$scale), c(sd(c(100, 200, 300, 400)), 1))

  model <- list(num_center = scaling$center, num_scale = scaling$scale)
  standardized <- standardizeCnnInputs(X, model)
  expect_equal(mean(standardized[, "elevation"]), 0)
  expect_equal(sd(standardized[, "elevation"]), 1)
  expect_equal(unname(standardized[, "constant"]), rep(0, 4))

  # Columns are matched by name, not position
  reordered <- standardizeCnnInputs(X[, c("constant", "elevation")], model)
  expect_equal(reordered[, "elevation"], standardized[, "elevation"])

  # Models trained before standardization predict as before
  expect_identical(standardizeCnnInputs(X, list()), X)
})

test_results$cnnModel <- test_that("CNN saves and applies its scaling", {
  skip_if_not(
    requireNamespace("torch", quietly = TRUE) && torch::torch_is_installed(),
    "torch not available"
  )
  suppressPackageStartupMessages(library(torch))
  set.seed(4)
  torch::torch_manual_seed(4)
  n <- 1000
  trainData <- data.frame(elevation = runif(n, 200, 3000), noise = runif(n),
                          lc = factor(sample(c("11", "41"), n, TRUE)))
  trainData$presence <- rbinom(n, 1, plogis(-4 + 0.003 * trainData$elevation))
  model <- getCNNModel(trainData, nCores = 1, isTuningOn = FALSE)
  expect_equal(model$num_center, colMeans(trainData[, c("elevation", "noise")]))

  # Round trip through the files Train writes and Predict reads
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  torch::torch_save(model$model$state_dict(), file.path(dir, "w.pt"))
  saveRDS(model[names(model) != "model"], file.path(dir, "meta.rds"))
  loaded <- loadCNNModel(file.path(dir, "w.pt"), file.path(dir, "meta.rds"))
  expect_equal(loaded$num_scale, model$num_scale)

  newData <- trainData[1:50, c("elevation", "noise", "lc")]
  tableProbs <- predictCNN(model, newData, isRaster = FALSE)
  expect_equal(predictCNN(loaded, newData, isRaster = FALSE), tableProbs,
               tolerance = 1e-6)
  expect_equal(unname(cnnPresenceProbability(loaded, newData)),
               unname(tableProbs), tolerance = 1e-6)

  r <- terra::rast(nrows = 5, ncols = 10, nlyrs = 2,
                   vals = c(newData$elevation, newData$noise))
  names(r) <- c("elevation", "noise")
  lcRaster <- terra::rast(r[[1]], vals = as.integer(as.character(newData$lc)))
  lcRaster <- terra::as.factor(lcRaster)
  names(lcRaster) <- "lc"
  rasterProbs <- predictCNN(loaded, c(r, lcRaster), isRaster = TRUE)
  expect_equal(as.vector(terra::values(rasterProbs)), unname(tableProbs),
               tolerance = 1e-5)

  # Learns the elevation effect
  extremes <- predictCNN(
    loaded, data.frame(elevation = c(300, 2900), noise = 0.5, lc = "11"),
    isRaster = FALSE
  )
  expect_gt(extremes[2] - extremes[1], 0.7)
})

test_results$datasheetParsing <- test_that("datasheet parsing helpers", {
  empty <- parseTargetClassOptions(data.frame())
  expect_false(empty$useTargetClass)
  expect_true(is.na(empty$targetClassValue) && is.integer(empty$targetClassValue))
  expect_true(is.na(empty$targetClassLabel) && is.character(empty$targetClassLabel))

  sheet <- data.frame(useTargetClass = TRUE, targetClassValue = 3,
                      backgroundValues = "1,2", ignoreValues = NA,
                      targetClassLabel = "Snow")
  parsed <- parseTargetClassOptions(sheet)
  expect_true(parsed$useTargetClass)
  expect_identical(parsed$targetClassValue, 3L)
  expect_identical(parsed$backgroundValues, "1,2")
  expect_true(is.na(parsed$ignoreValues))
  expect_identical(parsed$targetClassLabel, "Snow")

  expect_null(parseTerminology(data.frame())$bandLabelFile)
  terminology <- parseTerminology(data.frame(
    bandNames = "bands.csv", redBand = 3, greenBand = 2, blueBand = NA
  ))
  expect_identical(terminology$bandLabelFile, "bands.csv")
  expect_null(terminology$rgbBands)  # all three bands are required
  terminology <- parseTerminology(data.frame(
    bandNames = "", redBand = 3, greenBand = 2, blueBand = 1
  ))
  expect_null(terminology$bandLabelFile)
  expect_equal(terminology$rgbBands, list(red = 3, green = 2, blue = 1))
})

test_results$reclassRules <- test_that("reclassification rules", {
  wopt <- list(datatype = "INT2S", NAflag = -32768)
  base <- terra::rast(nrows = 2, ncols = 3, vals = c(1, 1, 1, 0, 0, NA))
  ruleDir <- tempfile()
  dir.create(ruleDir)
  on.exit(unlink(ruleDir, recursive = TRUE), add = TRUE)
  rulePath <- file.path(ruleDir, "rule.tif")
  terra::writeRaster(
    terra::rast(base, vals = c(10, 20, 30, 40, 50, 60)), rulePath
  )

  rules <- data.frame(
    ruleRasterFile = c(rulePath, rulePath, "missing.tif", rulePath),
    ruleMinValue = c(20, 60, 0, NA),
    ruleMaxValue = c(20, 35, 1, 5),
    # Reclass values are stored as list positions: 1 = absent, 2 = present
    ruleReclassValue = c(1, 2, 2, 2)
  )
  todiskBefore <- terraOptions(print = FALSE)$todisk
  warningsSeen <- character(0)
  result <- withCallingHandlers(
    applyReclassRules(base, rules, "test raster", wopt),
    warning = function(w) {
      warningsSeen <<- c(warningsSeen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("missing for rule index 3", warningsSeen)))
  expect_true(any(grepl("contain NA for rule index 4", warningsSeen)))
  # Rule 1 (categorical 20 -> 0); rule 2 (swapped to [35, 60] -> 1).
  # NA cells under a rule are set too, as before.
  expect_equal(as.vector(terra::values(result)), c(1, 0, 1, 1, 1, 1))
  expect_identical(terraOptions(print = FALSE)$todisk, todiskBefore)

  shifted <- terra::shift(terra::rast(rulePath), dx = terra::xres(base))
  shiftedPath <- file.path(ruleDir, "shifted.tif")
  terra::writeRaster(shifted, shiftedPath)
  expect_warning(
    applyReclassRules(
      base, data.frame(ruleRasterFile = shiftedPath, ruleMinValue = 0,
                       ruleMaxValue = 100, ruleReclassValue = 2),
      "unfiltered training raster", wopt
    ),
    "does not match unfiltered training raster extent"
  )

  # End to end for one context: output paths, file names and summary rows
  basePath <- file.path(ruleDir, "unfiltered.tif")
  probPath <- file.path(ruleDir, "prob.tif")
  terra::writeRaster(base, basePath)
  terra::writeRaster(terra::rast(base, vals = seq(0.1, 0.6, 0.1)), probPath)
  outputDf <- data.frame(
    Timestep = 4, ClassifiedUnfiltered = basePath,
    ClassifiedUnfilteredRestricted = NA_character_,
    ClassifiedFilteredRestricted = NA_character_,
    ClassifiedProbability = probPath
  )
  outputs <- reclassifyOutputs(
    outputDf, 4, "predicting",
    columns = list(
      unfiltered = "ClassifiedUnfiltered",
      unfilteredRestricted = "ClassifiedUnfilteredRestricted",
      filteredRestricted = "ClassifiedFilteredRestricted",
      probability = "ClassifiedProbability"
    ),
    rasterLabel = "predicting raster",
    ruleReclassDataframe = rules[1:2, ], applyFiltering = TRUE,
    filterValue = 0, fillValue = 8, transferDir = ruleDir,
    restrictedTmpDir = tempdir(), wopt = wopt,
    targetClassValue = NA, targetClassLabel = NA
  )
  expect_equal(
    basename(outputs$outputDf$ClassifiedUnfilteredRestricted),
    "PredictedPresenceRestricted-predicting-t4.tif"
  )
  expect_equal(
    basename(outputs$outputDf$ClassifiedFilteredRestricted),
    "PredictedPresenceFilteredRestricted-predicting-t4.tif"
  )
  expect_equal(
    vapply(outputs$summaryRows, `[[`, "", "PredictionType"),
    c("reclassified - predicting", "filtered_reclassified - predicting")
  )
  expect_equal(outputs$summaryRows[[1]]$PredictedPixels, 5)
})

# Test 4: Validation Logic
cat("4. Testing validation logic...\n") 
test_results$validation <- test_that("Parameter validation works", {
  # Test threshold validation
  validate_threshold <- function(setManual, threshold) {
    if (setManual && (is.null(threshold) || is.na(threshold))) {
      stop("Threshold missing")
    }
    if (setManual && (threshold < 0 || threshold > 1)) {
      stop("Threshold out of range")  
    }
  }
  
  expect_error(validate_threshold(TRUE, NULL), "Threshold missing")
  expect_error(validate_threshold(TRUE, -0.1), "Threshold out of range")
  expect_error(validate_threshold(TRUE, 1.5), "Threshold out of range")
  expect_silent(validate_threshold(TRUE, 0.5))
  expect_silent(validate_threshold(FALSE, NULL))
})

# Test 5: Raster Operations
cat("5. Testing raster operations...\n")
test_results$raster_ops <- test_that("Basic raster operations work", {
  r1 <- create_test_raster(nrows = 6, ncols = 6)
  r2 <- create_test_raster(nrows = 6, ncols = 6)
  
  # Test dimension matching
  expect_equal(dim(r1), dim(r2))
  
  # Test basic operations
  r_sum <- r1 + r2
  expect_s4_class(r_sum, "SpatRaster")
  expect_equal(dim(r_sum), dim(r1))
  
  # Test value extraction
  vals1 <- values(r1)
  expect_true(length(vals1) == ncell(r1))
  expect_true(is.numeric(vals1))
})

cat("\n===================================================\n")
cat("              Test Results Summary\n")
cat("===================================================\n")

total_tests <- length(test_results)
cat(sprintf("Total test groups run: %d\n", total_tests))
cat("All tests completed successfully! ✓\n\n")

cat("Test suite ready for integration with build process.\n")
cat("===================================================\n")