# Builds a sample ecoClassify library using example image and label rasters
# ApexRMS
# Mar 2026

library(rsyncrosim)
library(tidyverse)

mySession <- session()

pathExampleLibrary <- file.path(getwd(), "example-library")
pathImageRaster <- file.path(pathExampleLibrary, "image_raster.tif")
pathLabelRaster <- file.path(pathExampleLibrary, "label_raster.tif")

# Optional: path to a multiclass ground truth raster (3+ classes).
# Set to NULL to skip creating the multiclass scenario.
pathMulticlassLabelRaster <- NULL
# pathMulticlassLabelRaster <- file.path(pathExampleLibrary, "label_raster_multiclass.tif")

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

description(
  myLibrary
) <- "Sample ecoClassify library built from example image and label rasters"
owner(myLibrary) <- "ApexRMS"

# Helper: build one self-contained scenario ------------------------------------

makeScenario <- function(project, name, trainingRaster, labelRaster) {
  sc <- scenario(project, scenario = name)
  description(sc) <- paste(
    "Train and predict using",
    basename(labelRaster),
    "as ground truth"
  )
  owner(sc) <- "ApexRMS"

  saveDatasheet(
    sc,
    data.frame(
      Timesteps = 1,
      TrainingRasterFile = trainingRaster,
      GroundTruthRasterFile = labelRaster
    ),
    "ecoClassify_InputTrainingRasters"
  )

  saveDatasheet(
    sc,
    data.frame(Timesteps = 1, predictingRasterFile = trainingRaster),
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
      tuningObjective = 5, # Youden (binary only)
      setManualThreshold = FALSE,
      manualThreshold = 0.5, # binary only
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

  sc
}

# Binary scenario --------------------------------------------------------------

makeScenario(myProject, "Train and Predict", pathImageRaster, pathLabelRaster)

# Multiclass scenario (optional) -----------------------------------------------

if (
  !is.null(pathMulticlassLabelRaster) && file.exists(pathMulticlassLabelRaster)
) {
  makeScenario(
    myProject,
    "Train and Predict (Multiclass)",
    pathImageRaster,
    pathMulticlassLabelRaster
  )
}

myProject <- rsyncrosim::project(myLibrary, project = "Definitions")
sc <- scenario(myProject, scenario = "Train and Predict")
# Run scenarios (uncomment to execute) -----------------------------------------
# run(myProject, scenario = "Train and Predict")
# run(myProject, scenario = "Train and Predict (Multiclass)")  # requires pathMulticlassLabelRaster
