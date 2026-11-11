## Unit tests for src/functions/0.1-pre-processing.r

test_that("assignGenericCRS adds a CRS only when one is missing", {
  source_ecoclassify_r("0.1-pre-processing.r")

  noCrs <- make_test_raster(crs = "")
  expect_true(terra::crs(noCrs) == "")
  expect_false(terra::crs(assignGenericCRS(noCrs)) == "")

  withCrs <- make_test_raster(crs = "EPSG:3857")
  expect_equal(terra::crs(assignGenericCRS(withCrs)), terra::crs(withCrs))
})

test_that("normalizeBand rescales values to 0-1", {
  source_ecoclassify_r("0.1-pre-processing.r")

  set.seed(1)
  out <- normalizeBand(make_test_raster(min = 50, max = 500))
  v <- terra::values(out)

  expect_equal(min(v), 0)
  expect_equal(max(v), 1)
})

test_that("normalizeRaster normalizes every band of every raster", {
  source_ecoclassify_r("0.1-pre-processing.r")

  set.seed(2)
  out <- normalizeRaster(list(make_test_stack(3), make_test_stack(3)))

  expect_length(out, 2)
  for (r in out) {
    expect_equal(terra::nlyr(r), 3)
    mm <- terra::minmax(r)
    expect_equal(unname(mm[1, ]), rep(0, 3))
    expect_equal(unname(mm[2, ]), rep(1, 3))
  }
})

test_that("checkNA flags rasters whose bands have different NA patterns", {
  source_ecoclassify_r("0.1-pre-processing.r")

  consistent <- make_test_stack(2)
  expect_false(checkNA(list(consistent), verbose = FALSE))

  inconsistent <- consistent
  inconsistent[[1]][1:5] <- NA
  expect_true(checkNA(list(inconsistent), verbose = FALSE))
})

test_that("reclassifyGroundTruth keeps binary rasters as min -> 0, max -> 1", {
  source_ecoclassify_r("0.1-pre-processing.r")

  r <- make_test_raster(nrows = 2, ncols = 2)
  terra::values(r) <- c(1, 2, 2, 1)

  out <- reclassifyGroundTruth(list(r))[[1]]
  expect_equal(as.vector(terra::values(out)), c(0, 1, 1, 0))
})

test_that("reclassifyGroundTruth maps the target class to 1 and other classes to 0", {
  source_ecoclassify_r("0.1-pre-processing.r")

  r <- make_test_raster(nrows = 1, ncols = 5)
  terra::values(r) <- c(1, 2, 3, 4, NA)

  out <- reclassifyGroundTruth(list(r), useTargetClass = TRUE, targetClassValue = 2)[[1]]
  expect_equal(as.vector(terra::values(out)), c(0, 1, 0, 0, NA))
})

test_that("reclassifyGroundTruth leaves ignore values as NA", {
  source_ecoclassify_r("0.1-pre-processing.r")

  r <- make_test_raster(nrows = 1, ncols = 4)
  terra::values(r) <- c(1, 2, 3, 4)

  out <- reclassifyGroundTruth(list(r), useTargetClass = TRUE,
                               targetClassValue = 2, ignoreValues = "3, 4")[[1]]
  expect_equal(as.vector(terra::values(out)), c(0, 1, NA, NA))
})

test_that("reclassifyGroundTruth uses only background values as absence when given", {
  source_ecoclassify_r("0.1-pre-processing.r")

  r <- make_test_raster(nrows = 1, ncols = 4)
  terra::values(r) <- c(1, 2, 3, 4)

  # The ignore list is not used when a background list is supplied.
  out <- reclassifyGroundTruth(list(r), useTargetClass = TRUE, targetClassValue = 2,
                               backgroundValues = "1", ignoreValues = "4")[[1]]
  expect_equal(as.vector(terra::values(out)), c(0, 1, NA, NA))
})

test_that("validateAndAlignRasters accepts matching rasters", {
  source_ecoclassify_r("0.1-pre-processing.r")

  pair <- make_separable_pair(nrows = 10, ncols = 10)
  out <- suppressMessages(capture.output(
    aligned <- validateAndAlignRasters(list(pair$image), list(pair$groundTruth))
  ))

  expect_length(aligned, 1)
  expect_equal(terra::ext(aligned[[1]]), terra::ext(pair$image))
})

test_that("validateAndAlignRasters rejects mismatched list lengths", {
  source_ecoclassify_r("0.1-pre-processing.r")

  pair <- make_separable_pair(nrows = 10, ncols = 10)
  expect_error(
    validateAndAlignRasters(list(pair$image, pair$image), list(pair$groundTruth)),
    "same length"
  )
})

test_that("validateAndAlignRasters rejects training rasters with different extents", {
  source_ecoclassify_r("0.1-pre-processing.r")

  a <- make_test_stack(nrows = 10, ncols = 10)
  b <- make_test_stack(nrows = 12, ncols = 12)
  gt <- make_test_raster(nrows = 10, ncols = 10)

  expect_error(
    suppressMessages(capture.output(validateAndAlignRasters(list(a, b), list(gt, gt)))),
    "does not match"
  )
})
