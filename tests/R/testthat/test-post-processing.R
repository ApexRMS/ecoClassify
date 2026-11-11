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
