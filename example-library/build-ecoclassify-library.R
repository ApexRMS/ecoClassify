# Builds a sample ecoClassify library using example image and label rasters
# ApexRMS
# Mar 2026
#
# Structure:
#   Base scenario  — all shared settings (rasters, classifier options, pipeline)
#   6 species scenarios — each depends on Base, overrides only TargetClassOptions
#
# Label raster values:
#   1 = pine, 2 = spruce, 3 = cedar, 4 = maple, 5 = oak, 6 = birch

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
  name = file.path(pathExampleLibrary, "Edmonton-example.ssim"),
  package = "ecoClassify",
  session = mySession
)

myProject <- rsyncrosim::project(myLibrary, project = "Definitions")

description(
  myLibrary
) <- "Tree Classification City of Edmonton — classifies urban tree species from drone imagery using ecoClassify random forest models"
owner(myLibrary) <- "ApexRMS"

# Base scenario ----------------------------------------------------------------
# Contains all shared settings. Species scenarios depend on this.

scBase <- scenario(myProject, scenario = "Base")
description(
  scBase
) <- "Base settings shared across all species scenarios. Contains input rasters, classifier options, and pipeline configuration."
owner(scBase) <- "ApexRMS"

saveDatasheet(
  scBase,
  data.frame(
    Timesteps = 1,
    TrainingRasterFile = pathImageRaster,
    GroundTruthRasterFile = pathLabelRaster
  ),
  "ecoClassify_InputTrainingRasters"
)

saveDatasheet(
  scBase,
  data.frame(Timesteps = 1, predictingRasterFile = pathImageRaster),
  "ecoClassify_InputPredictingRasters"
)

saveDatasheet(
  scBase,
  data.frame(
    nObs = 100,
    modelType = 2 # 0 = random forest | 2 = CNN 
  "ecoClassify_ClassifierOptions"
)

saveDatasheet(
  scBase,
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
saveDatasheet(myScenario, myData, "core_Pipeline", append = FALSE)
saveDatasheet(
  scBase,
  data.frame(StageNameId = c("ecoClassify_TrainClassifier"), RunOrder = 1),
  "core_Pipeline",
  append = FALSE
)

# Species scenarios ------------------------------------------------------------
# Each depends on Base and only overrides TargetClassOptions.

species <- data.frame(
  name = c("Pine", "Spruce", "Cedar", "Maple", "Oak", "Birch"),
  value = c(1L, 2L, 3L, 4L, 5L, 6L)
)

for (i in seq_len(nrow(species))) {
  sp_name <- species$name[i]
  sp_value <- species$value[i]

  sc <- scenario(myProject, scenario = sp_name)
  description(sc) <- paste0(
    sp_name,
    " presence classification using drone imagery over the City of Edmonton. ",
    "Trains a random forest model to distinguish ",
    tolower(sp_name),
    " (label value = ",
    sp_value,
    ") from all other tree species."
  )
  owner(sc) <- "ApexRMS"

  # Inherit all settings from Base
  dependency(sc) <- "Base"

  # Override only the target class for this species
  saveDatasheet(
    sc,
    data.frame(
      useTargetClass = TRUE,
      targetClassValue = sp_value,
      targetClassLabel = sp_name
    ),
    "ecoClassify_TargetClassOptions"
  )
}

# Run --------------------------------------------------------------------------
# run(scBase)  # run base alone to verify shared settings
# run(sc)      # run a single species scenario
