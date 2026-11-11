## Unit tests for src/functions/0.3-contextualization.r

test_that("addRasterAdjacencyValues appends focal statistics and two PCA layers", {
  source_ecoclassify_r("0.3-contextualization.r")

  set.seed(5)
  r <- make_test_stack(nlayers = 3, nrows = 12, ncols = 12)
  out <- addRasterAdjacencyValues(r, adjacencyWindow = 3)

  # original + mean + sd + min + max + range per band, plus PC1 and PC2
  expect_equal(terra::nlyr(out), 3 * 6 + 2)
  expect_true(all(c("band_1_mean", "band_2_sd", "band_3_range", "PC1", "PC2") %in% names(out)))
})

test_that("focal range equals focal max minus focal min", {
  source_ecoclassify_r("0.3-contextualization.r")

  # Two bands: the function always derives PC1 and PC2, so it needs at least two.
  set.seed(6)
  r <- make_test_stack(nlayers = 2, nrows = 8, ncols = 8)
  out <- addRasterAdjacencyValues(r, adjacencyWindow = 3)

  expect_equal(
    as.vector(terra::values(out[["band_1_range"]])),
    as.vector(terra::values(out[["band_1_max"]] - out[["band_1_min"]]))
  )
})

test_that("contextualizeRaster uses the contextualizationWindowSize setting for every raster", {
  source_ecoclassify_r("0.3-contextualization.r")

  # In a transformer this comes from the AdvancedClassifierOptions datasheet.
  contextualizationWindowSize <- 5

  set.seed(7)
  out <- contextualizeRaster(list(make_test_stack(2), make_test_stack(2)))

  expect_length(out, 2)
  for (r in out) {
    expect_equal(terra::nlyr(r), 2 * 6 + 2)
  }
})
