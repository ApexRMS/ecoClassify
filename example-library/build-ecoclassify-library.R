# Builds a sample ecoClassify library using example image and label rasters
# ApexRMS
# Mar 2026

library(rsyncrosim)
library(tidyverse)

mySession <- session()

pathExampleLibrary <- file.path(getwd(), "example-library")
pathImageRaster <- file.path(pathExampleLibrary, "image_raster.tif")
pathLabelRaster <- file.path(pathExampleLibrary, "label_raster.tif")

missingFiles <- c(pathImageRaster, pathLabelRaster)[
  !file.exists(c(pathImageRaster, pathLabelRaster))
]
if (length(missingFiles) > 0) {
  stop("Missing raster files: ", paste(missingFiles, collapse = ", "))
}

# Create library ---------------------------------------------------------------

myLibrary <- ssimLibrary(
  name = file.path(pathExampleLibrary, "ecoClassify-example.ssim"),
  package = "ecoClassify",
  session = mySession
)

myProject <- rsyncrosim::project(myLibrary, project = "Definitions")

description(myLibrary) <- "Sample ecoClassify library built from example image and label rasters"
owner(myLibrary) <- "ApexRMS"

# Create scenario --------------------------------------------------------------

sc <- scenario(myProject, scenario = "Train and Predict")
description(sc) <- "Train and predict using binary ground truth raster"
owner(sc) <- "ApexRMS"

saveDatasheet(
  sc,
  data.frame(
    Timesteps = 1,
    TrainingRasterFile = pathImageRaster,
    GroundTruthRasterFile = pathLabelRaster
  ),
  "ecoClassify_InputTrainingRasters"
)

saveDatasheet(
  sc,
  data.frame(Timesteps = 1, predictingRasterFile = pathImageRaster),
  "ecoClassify_InputPredictingRasters"
)

saveDatasheet(
  sc,
  data.frame(
    nObs = 10000,
    modelType = 0 # 0 = Random Forest
  ),
  "ecoClassify_ClassifierOptions"
)

saveDatasheet(
  sc,
  data.frame(
    overrideBandnames = FALSE,
    normalizeRasters = TRUE,
    rasterDecimalPlaces = 4,
    modelTuning = FALSE,
    tuningObjective = 5, # Youden
    setManualThreshold = FALSE,
    manualThreshold = 0.5,
    applyContextualization = FALSE,
    contextualizationWindowSize = 3,
    setSeed = 11
  ),
  "ecoClassify_AdvancedClassifierOptions"
)

saveDatasheet(
  sc,
  data.frame(
    StageNameId = c(
      "ecoClassify_TrainClassifier",
      "ecoClassify_Predict",
      "ecoClassify_PostProcessing"
    ),
    RunOrder = c(1, 2, 3)
  ),
  "core_Pipeline",
  append = FALSE
)

# Run --------------------------------------------------------------------------
# run(sc)
