# set up workspace ---------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))
source(file.path(packageDir, "workspace.r"))

# Set up -------------------------------------------------------------------
myScenario <- scenario()  # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Load raster input datasheets -----------------------------------------------
inputRasterDataframe <- datasheet(myScenario,
                                  name = "imageclassifier_InputRasters")

modelObjectDataframe <- datasheet(myScenario,
                                  name = "imageclassifier_ModelObject")

# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(myScenario,
                                  inputRasterDataframe,
                                  inputRasterDataframe$RasterFileToClassify)
timestepList <- inputVariables[[1]]
nObs <- inputVariables[[2]]
filterResolution <- inputVariables[[3]]
filterPercent <- inputVariables[[4]]
applyFiltering <- inputVariables[[5]]
applyContextualization <- inputVariables[[6]]
modelType <- inputVariables[[7]]

# load model and threshold
model <- readRDS(modelObjectDataframe$Model)
optimalThreshold <- modelObjectDataframe$OptimalThreshold

# extract list of training, testing, and ground truth rasters ----------------
toClassifyRasterList <- extractRasters(inputRasterDataframe, column = 4)

# Setup empty dataframes to accept output in SyncroSim datasheet format ------

classifiedRasterOutputDataframe <- data.frame(Timestep = numeric(0),
                                              ClassifiedUnfiltered = character(0),
                                              ClassifiedFiltered = character(0),
                                              ClassifiedProbability = character(0))


classifiedRgbOutputDataframe <- data.frame(Timestep = numeric(0),
                                           RGBImage = character(0))

# add contextualization for toclassify rasters if selected ---------------------
if (applyContextualization == TRUE) {

  toClassifyRasterList <- contextualizeRaster(toClassifyRasterList) # change naming to avoid this

}

## Predict presence for rasters to classify ------------------------------------
for (t in seq_along(toClassifyRasterList)) {

  # get timestep for the current raster
  timestep <- timestepList[t]

  classifiedRasters <- getPredictionRasters(toClassifyRasterList[[t]],
                                            model,
                                            optimalThreshold,
                                            modelType)
  classifiedPresence <- classifiedRasters[[1]]
  classifiedProbability <- classifiedRasters[[2]]

  # generate rasterDataframe based on filtering argument
  classifiedRasterOutputDataframe <- generateRasterDataframe(applyFiltering,
                                                             classifiedPresence,
                                                             filterResolution,
                                                             filterPercent,
                                                             category = "forecasting",
                                                             timestep,
                                                             transferDir,
                                                             classifiedRasterOutputDataframe,
                                                             hasGroundTruth = FALSE)

  # define RGB data frame
  classifiedRgbOutputDataframe <- getRgbDataframe(classifiedRgbOutputDataframe,
                                                  category = "forecasting",
                                                  timestep,
                                                  transferDir)

  # save files
  saveFiles(classifiedPresence,
            groundTruth = NULL,
            classifiedProbability,
            toClassifyRasterList,
            category = "forecasting",
            timestep,
            transferDir)
}

# Save dataframes back to SyncroSim library's output datasheets ----------------

saveDatasheet(myScenario,
              data = classifiedRasterOutputDataframe,
              name = "imageclassifier_ClassifiedRasterOutput")

saveDatasheet(myScenario,
              data = classifiedRgbOutputDataframe,
              name = "imageclassifier_ClassifiedRgbOutput")
