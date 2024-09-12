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

# define functions ------------------------------------------------
extractRasters <- function(column) {

  allFiles <- as.vector(column)
  rasterList <- c()

  for (file in allFiles) {
    Raster <- rast(file)
    rasterList <- c(rasterList, Raster)
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
