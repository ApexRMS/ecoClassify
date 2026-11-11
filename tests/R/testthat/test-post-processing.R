## Unit tests for src/functions/0.6-post-processing.r

# 5x5 binary raster (row-major) for filter tests.
binary_raster <- function(vals) {
  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 5,
                   crs = "EPSG:3857")
  terra::values(r) <- vals
  r
}

test_that("filterPredictionRaster removes isolated presence pixels", {
  source_ecoclassify_r("0.6-post-processing.r")

  vals <- rep(0, 25)
  vals[13] <- 1 # lone presence pixel in the centre
  out <- filterPredictionRaster(binary_raster(vals), filterValue = 1)

  expect_equal(sum(terra::values(out)), 0)
})

test_that("filterPredictionRaster fills a hole surrounded by presence", {
  source_ecoclassify_r("0.6-post-processing.r")

  vals <- rep(1, 25)
  vals[13] <- 0 # single absence hole in the centre
  out <- filterPredictionRaster(binary_raster(vals), fillValue = 8)

  expect_equal(terra::values(out)[13], 1)
})

test_that("filterPredictionRaster leaves the raster unchanged with no settings", {
  source_ecoclassify_r("0.6-post-processing.r")

  set.seed(8)
  vals <- stats::rbinom(25, 1, 0.5)
  out <- filterPredictionRaster(binary_raster(vals))

  expect_equal(as.vector(terra::values(out)), vals)
})

test_that("buildSummaryRow summarizes predicted presence and probabilities", {
  source_ecoclassify_r("0.6-post-processing.r")

  pred <- terra::rast(nrows = 1, ncols = 4, xmin = 0, xmax = 40, ymin = 0, ymax = 10,
                      crs = "EPSG:3857")
  prob <- pred
  terra::values(pred) <- c(1, 1, 0, NA)
  terra::values(prob) <- c(0.9, 0.7, 0.2, NA)

  row <- buildSummaryRow(pred, prob, timestep = 3, predictionType = "Unfiltered",
                         targetClassValue = 2L, targetClassLabel = "Pine")

  expect_equal(row$Timestep, 3)
  expect_equal(row$PredictedPixels, 2)
  expect_equal(row$PredictedArea, 2 * 100) # 10 x 10 map-unit cells
  expect_equal(row$MeanProbability, 0.8)
  expect_equal(row$MinProbability, 0.7)
  expect_equal(row$MaxProbability, 0.9)
  expect_equal(row$TargetClassLabel, "Pine")
})

test_that("buildSummaryRow returns NA probabilities when nothing is predicted", {
  source_ecoclassify_r("0.6-post-processing.r")

  pred <- terra::rast(nrows = 1, ncols = 2, crs = "EPSG:3857")
  prob <- pred
  terra::values(pred) <- c(0, 0)
  terra::values(prob) <- c(0.1, 0.2)

  row <- buildSummaryRow(pred, prob, timestep = 1, predictionType = "Unfiltered")
  expect_equal(row$PredictedPixels, 0)
  expect_true(is.na(row$MinProbability))
  expect_true(is.na(row$MaxProbability))
})

# The previous implementation of the restriction rules: one classify()/ifel()
# pass per rule (from 3-postprocess.R before the single-pass rewrite).
reference_restrict <- function(pred, rules) {
  cur <- pred
  for (i in seq_len(nrow(rules))) {
    ruleRaster <- terra::rast(rules$ruleRasterFile[i])
    vmin <- rules$ruleMinValue[i]
    vmax <- rules$ruleMaxValue[i]
    rval <- as.numeric(rules$ruleReclassValue[i]) - 1
    if (vmin > vmax) { tmp <- vmin; vmin <- vmax; vmax <- tmp }
    if (isTRUE(vmin == vmax)) {
      cur <- terra::ifel(ruleRaster == vmin, rval, cur)
    } else {
      classedMask <- terra::classify(ruleRaster, matrix(c(vmin, vmax, rval), ncol = 3),
                                     others = NA, right = FALSE, include.lowest = TRUE)
      cur <- terra::ifel(!is.na(classedMask), rval, cur)
    }
  }
  cur
}

test_that("applyRestrictionRules matches the per-rule classify/ifel chain", {
  source_ecoclassify_r("0.6-post-processing.r")

  set.seed(9)
  dir <- file.path(tempdir(), "restrict-test")
  dir.create(dir, showWarnings = FALSE)

  pred <- binary_raster(rbinom(25, 1, 0.5))
  pred[c(2, 8)] <- NA
  predPath <- file.path(dir, "pred.tif")
  terra::writeRaster(pred, predPath, overwrite = TRUE)

  continuous <- binary_raster(seq(0, 96, by = 4))
  continuous[c(5, 17)] <- NA
  categorical <- binary_raster(rep(c(1, 2, 3, 4, 5), 5))
  categorical[c(3, 20)] <- NA
  paths <- file.path(dir, c("continuous.tif", "categorical.tif"))
  terra::writeRaster(continuous, paths[1], overwrite = TRUE)
  terra::writeRaster(categorical, paths[2], overwrite = TRUE)

  # Reclass values come from a 0/1 list, read back as factor codes 1/2 -> 0/1.
  rules <- data.frame(
    ruleRasterFile = c(paths[1], paths[2], paths[1]),
    ruleMinValue = c(40, 3, 80),     # third rule: min > max, gets swapped
    ruleMaxValue = c(10, 3, 60),
    ruleReclassValue = factor(c("0", "1", "1"), levels = c("0", "1"))
  )

  outPath <- file.path(dir, "restricted.tif")
  applyRestrictionRules(predPath, rules, outPath)

  expect_equal(
    as.vector(terra::values(terra::rast(outPath))),
    as.vector(terra::values(reference_restrict(terra::rast(predPath), rules)))
  )
  expect_equal(terra::datatype(terra::rast(outPath)), "FLT4S")
})

test_that("raster write options only use datatypes SyncroSim can read", {
  source_ecoclassify_r("0.6-post-processing.r")

  # SyncroSim (Spatial.OSGeoDataTypeToSSim) accepts Int32, Float32 and Float64
  # only; Byte/Int16 outputs fail when it merges spatial tiles or extracts bands.
  supported <- c("INT4S", "FLT4S", "FLT8S")
  expect_true(binaryWopt()$datatype %in% supported)
  expect_true(floatWopt()$datatype %in% supported)
})

test_that("applyRestrictionRules skips rules whose raster does not match", {
  source_ecoclassify_r("0.6-post-processing.r")

  dir <- file.path(tempdir(), "restrict-test-2")
  dir.create(dir, showWarnings = FALSE)
  pred <- binary_raster(rep(c(0, 1), length.out = 25))
  predPath <- file.path(dir, "pred.tif")
  terra::writeRaster(pred, predPath, overwrite = TRUE)

  other <- terra::rast(nrows = 3, ncols = 3, crs = "EPSG:3857", vals = 1)
  otherPath <- file.path(dir, "other.tif")
  terra::writeRaster(other, otherPath, overwrite = TRUE)

  rules <- data.frame(ruleRasterFile = otherPath, ruleMinValue = 0, ruleMaxValue = 2,
                      ruleReclassValue = factor("0", levels = c("0", "1")))
  outPath <- file.path(dir, "restricted.tif")
  applyRestrictionRules(predPath, rules, outPath)

  expect_equal(as.vector(terra::values(terra::rast(outPath))), as.vector(terra::values(pred)))
})

test_that("buildMetricsRow maps statistics and fills missing ones with NA", {
  source_ecoclassify_r("0.6-post-processing.r")

  stats <- data.frame(
    Statistic = c("Accuracy", "Precision", "Sensitivity", "Specificity"),
    Value = c(0.9, 0.8, 0.7, 0.6)
  )
  row <- buildMetricsRow(stats, targetClassValue = 1L, auc = 0.95)

  expect_equal(row$Accuracy, 0.9)
  expect_equal(row$Recall, 0.7)
  expect_equal(row$Specificity, 0.6)
  expect_true(is.na(row$F1))
  expect_equal(row$AUC, 0.95)
})
