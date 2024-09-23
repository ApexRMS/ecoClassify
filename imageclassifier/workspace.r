# load packages
library(rsyncrosim)
library(tidyverse)
library(terra)
library(sf)
library(ranger)
library(caret)
library(gtools)
library(reshape2)
library(Dict)
library(ggplot2)
library(stats)

# define functions ------------------------------------------------

# updated function that combines multiple raster types from each timestep
extractRasters <- function(dataframe) {

  # define timesteps
  timesteps <- unique(dataframe[,1])

  # create an empty list
  rasterList <- c()

  # loop through timesteps, combining rasters
  for (t in timesteps) {

    # subset based on timestep
    subsetData <- dataframe %>% filter(Timesteps == t)

    # list all files
    allFiles <- as.vector(subsetData[, 2])

    # read in all files as a single raster
    subsetRaster <- rast(allFiles)

    # add to main raster list
    rasterList <- c(rasterList, subsetRaster)
  }

  return(rasterList)
}

decomposedRaster <- function(predRast,
                             responseRast,
                             nobs) {
  randomPoints <- spatSample(responseRast,
                             size = nobs,
                             na.rm = TRUE,
                             as.points = TRUE,
                             replace = FALSE,
                             method = "stratified")
  randomPoints <- unique(randomPoints)
  responseValues <- terra::extract(responseRast, randomPoints)
  predValues <- terra::extract(predRast, randomPoints) # RGB-NIR
  trainData <- cbind(predValues, response = responseValues[, 2])
  return(trainData)
}

predictRanger <- function(raster, model) {
  ## generate blank raster
  predictionRaster <- raster[[1]]
  names(predictionRaster) <- "presence"

  ## predict over raster decomposition
  rasterMatrix <- data.frame(raster)
  rasterMatrix <- na.omit(rasterMatrix)
  predictedValues <- data.frame(predict(model, rasterMatrix))[, 1]
  values(predictionRaster) <- predictedValues

  return(predictionRaster)
}

filterFun <- function(raster, resolution, percent) {
  npixels <- resolution^2
  midPixel <- (npixels + 1) / 2
  threshold <- round(npixels * percent, 0)
  if (is.na(raster[midPixel])) {
    return(NA)
  } else if (raster[midPixel] == 1 && sum(raster[-midPixel] == 0, na.rm = TRUE) > threshold) {
    return(0)
  } else {
    return(raster[midPixel])
  }
}

filterRaster <- function(filterPercent,
                         filterResolution,
                         PredictedPresence,
                         groundTruthRaster) {

  # filter out presence pixels surrounded by non-presence
  filteredPredictedPresence <- focal(PredictedPresence,
                                     w = matrix(1, 5, 5),
                                     fun = filterFun,
                                     resolution = filterResolution,
                                     percent = filterPercent)

  # make a raster that is the sum of both layers
  sumRaster <- mosaic(filteredPredictedPresence,
                      groundTruthRaster,
                      fun = "sum")

  # calculate the max value
  maxVal <- minmax(sumRaster)[2]

  # count the number of pixels with the max value
  numPixels <- freq(sumRaster, value = maxVal)[, "count"]

  return(numPixels)
}


# next steps
# 1. finish filterAccuracy function
# 2. add to model script
# 3. add optimize wrapper

# 4. do some testing
# 5. run the package and see if it works!
# 6. confirm variable importance argument
# 7. fix confusion matrix calculation
# 8. eventually find a way to calculate average filter threshold and 
# then apply to test rasters (no ground truth)
