library(rsyncrosim)
library(terra)
library(tidyverse)
library(yaml)

# Settings ---------------------------------------------------------------------
# You can create a config.yaml file or define these directly
# config <- read_yaml("config/config.yaml")

# Direct configuration (modify as needed)
config <- list(
  libraryName = "ecoClassify-Library",
  projectName = "ImageClassification",
  ssimDir = "C:/Program Files/SyncroSim", # Set your SyncroSim directory path here
  ssimJobs = 4
)

# Define your raster data
rasters <- list.files("testing/snow/", pattern = ".tif$", full.names = TRUE)
trainingRasterDataframe <- data.frame(
  Timesteps = 1:3,
  TrainingRasterFile = rasters[1:3],
  GroundTruthRasterFile = rasters[4:6],
  stringsAsFactors = FALSE
)

# Library Level Settings
libraryName <- config$libraryName
projectName <- config$projectName
ssimOwner <- "ApexRMS"

# Descriptions
libraryDescription <- paste0(
  "ecoClassify library for image classification using semantic image segmentation."
)

# Build library ----------------------------------------------------------------
# Ensure ecoClassify is installed
uninstallPackage("ecoClassify")
installPackage("./src")

# Create library, project, and scenario
libraryFolder <- "./scratch"
dir.create(libraryFolder, showWarnings = FALSE)
ssimSession <- session(config$ssimDir)

mylibrary <- ssimLibrary(
  file.path(libraryFolder, libraryName),
  package = "ecoClassify",
  session = ssimSession,
  overwrite = FALSE,
  useConda = FALSE
)

myproject <- rsyncrosim::project(mylibrary, projectName, overwrite = FALSE)

# Set owner
owner(mylibrary) <- ssimOwner
owner(myproject) <- ssimOwner

# Set descriptions
description(mylibrary) <- libraryDescription

# Library Definitions ----------------------------------------------------------

# Project Definitions ----------------------------------------------------------
## Terminology ----
# Set up band names and timestep terminology
# terminology <- data.frame(
#   timestepName = "Timestep"
# )
# saveDatasheet(myproject, terminology, "project_Terminology")

## Multiprocessing ----
multiprocessing <- data.frame(
  EnableMultiprocessing = TRUE,
  MaximumJobs = config$ssimJobs,
  EnableMultiScenario = FALSE
)
saveDatasheet(myproject, multiprocessing, "core_Multiprocessing")

# Build Classification Scenarios ----------------------------------------------
## Create scenario folders
folder(myproject, folder = "Classification-Scenarios")
folder(
  myproject,
  folder = "1 - Training",
  parentFolder = "Classification-Scenarios"
)
folder(
  myproject,
  folder = "2 - Prediction",
  parentFolder = "Classification-Scenarios"
)

## Training Scenario -----------------------------------------------------------
scenarioName <- "Train Classifier"
trainScenario <- scenario(
  myproject,
  scenarioName,
  folder = "1 - Training",
  overwrite = TRUE
)

# Pipeline for training
pipeline <- data.frame(StageNameId = "1-Train Classifier") %>%
  mutate(RunOrder = 1)
saveDatasheet(trainScenario, pipeline, "core_Pipeline")

# Input Training Rasters
# Convert full paths to relative or absolute as needed
rasters <- list.files(
  paste0(getwd(), "/testing/integration/snow/"),
  pattern = ".tif$",
  full.names = TRUE
)
trainingRasterDataframe <- data.frame(
  Timesteps = 1:3,
  TrainingRasterFile = rasters[1:3],
  GroundTruthRasterFile = rasters[4:6],
  stringsAsFactors = FALSE
)

saveDatasheet(
  trainScenario,
  trainingRasterDataframe,
  "ecoClassify_InputTrainingRasters"
)

# Classifier Options (default settings from XML)
classifierOptions <- data.frame(
  nObs = 10000,
  modelType = 0 # 0 = Random Forest, 1 = MaxEnt, 2 = CNN
)
saveDatasheet(trainScenario, classifierOptions, "ecoClassify_ClassifierOptions")

# Advanced Classifier Options (default settings)
advancedOptions <- data.frame(
  normalizeRasters = FALSE,
  modelTuning = FALSE,
  setManualThreshold = FALSE,
  applyContextualization = FALSE
)
saveDatasheet(
  trainScenario,
  advancedOptions,
  "ecoClassify_AdvancedClassifierOptions"
)

# Optional: Add training covariates if you have them
# trainingCovariates <- data.frame(
#   trainingCovariateRasterFile = "path/to/covariate.tif",
#   trainingCovariateType = 0  # 0 = Categorical, 1 = Continuous
# )
# saveDatasheet(trainScenario, trainingCovariates, "InputTrainingCovariates")

## Prediction Scenario ---------------------------------------------------------
scenarioName <- "Predict Classification"
predictScenario <- scenario(
  myproject,
  scenarioName,
  folder = "2 - Prediction",
  overwrite = TRUE
)

# Set this scenario as a dependency of the training scenario
dependency(predictScenario) <- trainScenario

# Pipeline for prediction
pipeline <- data.frame(StageNameId = "2-Predict") %>%
  mutate(RunOrder = 1)
saveDatasheet(predictScenario, pipeline, "core_Pipeline")

# Input Predicting Rasters (you'll need to specify these)
# Example structure - modify with your actual prediction rasters
# predictingRasters <- data.frame(
#   Timesteps = 1:3,
#   predictingRasterFile = c("path/to/predict1.tif",
#                           "path/to/predict2.tif",
#                           "path/to/predict3.tif")
# )
# saveDatasheet(predictScenario, predictingRasters, "InputPredictingRasters")

# Copy classifier options from training scenario
saveDatasheet(
  predictScenario,
  classifierOptions,
  "ecoClassify_ClassifierOptions"
)
saveDatasheet(
  predictScenario,
  advancedOptions,
  "ecoClassify_AdvancedClassifierOptions"
)

## Post-Processing Scenario ----------------------------------------------------
scenarioName <- "Post Process Results"
postProcessScenario <- scenario(
  myproject,
  scenarioName,
  folder = "2 - Prediction",
  overwrite = TRUE
)

# Set dependencies
dependency(postProcessScenario) <- list(trainScenario, predictScenario)

# Pipeline for post-processing
pipeline <- data.frame(StageNameId = "3-PostProcessing") %>%
  mutate(RunOrder = 1)
saveDatasheet(postProcessScenario, pipeline, "core_Pipeline")

# Post-processing filter options
filterOptions <- data.frame(
  applyFiltering = FALSE,
  filterValue = NA_integer_,
  fillValue = NA_integer_
)
saveDatasheet(
  postProcessScenario,
  filterOptions,
  "ecoClassify_PostProcessingFilter"
)

# Optional: Rule-based restrictions
# ruleRestrictions <- data.frame(
#   ruleClass = 1,
#   ruleRasterFile = "path/to/restriction.tif",
#   ruleMinValue = 0,
#   ruleMaxValue = 1,
#   ruleReclassValue = 0
# )
# saveDatasheet(postProcessScenario, ruleRestrictions, "PostProcessingRule")

# Summary ----------------------------------------------------------------------
cat("ecoClassify library created successfully!\n")
cat("Library location:", file.path(libraryFolder, libraryName), "\n")
cat("Project:", projectName, "\n")
cat("Scenarios created:\n")
cat("  1. Train Classifier (Training folder)\n")
cat("  2. Predict Classification (Prediction folder)\n")
cat("  3. Post Process Results (Prediction folder)\n")

# Display scenario info
scenarios <- scenario(myproject)
print(scenarios)

# Next steps message
cat("\n=== Next Steps ===\n")
cat(
  "1. Update the prediction rasters in the 'Predict Classification' scenario\n"
)
cat("2. Add any covariate rasters if needed\n")
cat("3. Adjust classifier options and advanced settings as required\n")
cat(
  "4. Run the scenarios in order: Training -> Prediction -> Post-Processing\n"
)
cat("5. View results in the SyncroSim interface\n")


run(trainScenario)
run(predictScenario)
run(postProcessScenario)
