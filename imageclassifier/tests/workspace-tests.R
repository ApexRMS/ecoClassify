
## Load test functions
source("imageclassifier/workspace.r")

extractAllRasters <- function(rasterTrainingDataframe,
                              rasterGroundTruthDataframe,
                              rasterToClassifyDataframe) {

  # extract training rasters
  trainingRasterList <- extractRasters(rasterTrainingDataframe)

  # extract ground truth rasters
  groundTruthRasterList <- extractRasters(rasterGroundTruthDataframe)

  # extract rasters to classify
  if (length(rasterToClassifyDataframe$RasterFileToClassify) > 1) {
    toClassifyRasterList <- extractRasters(rasterToClassifyDataframe)
  } else {
    toClassifyRasterList <- ""
  }

  return(list(trainingRasterList,
              groundTruthRasterList,
              toClassifyRasterList))
}


add <- function(x, y) {
  return(x + y)
}


# Unit tests for the add function
test_that("addition works correctly", {
  expect_equal(add(1, 1), 2)
  expect_equal(add(-1, 1), 0)
  expect_equal(add(0, 0), 0)
  expect_equal(add(1.5, 1.5), 3)
})