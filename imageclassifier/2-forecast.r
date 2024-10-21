# set up workspace ---------------------------------------------------------
packageDir <- (Sys.getenv("ssim_package_directory"))
source(file.path(packageDir, "workspace.r"))

# Set up -------------------------------------------------------------------
myScenario <- scenario()  # Get the SyncroSim Scenario that is currently running

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Assign variables ----------------------------------------------------------
inputVariables <- assignVariables(myScenario)
timesteps <- inputVariables[[1]]
nObs <- inputVariables[[2]]
filterResolution <- inputVariables[[3]]
filterPercent <- inputVariables[[4]]
applyFiltering <- inputVariables[[5]]
applyContextualization <- inputVariables[[6]]
modelType <- inputVariables[[7]]

# Load raster input datasheets
inputRasterDataframe <- datasheet(myScenario,
                                  name = "imageclassifier_InputRasters")

modelObjectDataframe <- datasheet(myScenario,
                                  name = "imageclassifier_ModelObject")

# load model and threshold
model <- readRDS(modelObjectDataframe$Model)
optimalThreshold <- modelObjectDataframe$OptimalThreshold

# extract list of training, testing, and ground truth rasters ----------------
toClassifyRasterList <- extractRasters(inputRasterDataframe, column = 4)

# Setup empty dataframes to accept output in SyncroSim datasheet format ------

classifiedRasterOutputDataframe <- data.frame(Iteration = numeric(0),
                                              Timestep = numeric(0),
                                              ClassifiedUnfiltered = character(0),
                                              ClassifiedFiltered = character(0),
                                              ClassifiedProbability = character(0))


classifiedRgbOutputDataframe <- data.frame(Iteration = numeric(0),
                                           Timestep = numeric(0),
                                           RGBImage = character(0))

# add contextualization for toclassify rasters if selected ---------------------
if (applyContextualization == TRUE) {

  toClassifyRasterList <- contextualizeRaster(toClassifyRasterList) # change naming to avoid this

}

## Predict presence for rasters to classify ------------------------------------
for (t in seq_along(toClassifyRasterList)) {

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
                                                             iteration = 2,
                                                             t,
                                                             transferDir,
                                                             classifiedRasterOutputDataframe,
                                                             hasGroundTruth = FALSE)

  # define RGB data frame
  classifiedRgbOutputDataframe <- getRgbDataframe(classifiedRgbOutputDataframe,
                                                  iteration = 2,
                                                  t,
                                                  transferDir)

  # save files
  saveFiles(classifiedPresence,
            groundTruth = NULL,
            classifiedProbability,
            toClassifyRasterList,
            iteration = 2,
            t,
            transferDir)
}

# Save dataframes back to SyncroSim library's output datasheets ----------------

saveDatasheet(myScenario,
              data = classifiedRasterOutputDataframe,
              name = "imageclassifier_ClassifiedRasterOutput")

saveDatasheet(myScenario,
              data = classifiedRgbOutputDataframe,
              name = "imageclassifier_ClassifiedRgbOutput")
