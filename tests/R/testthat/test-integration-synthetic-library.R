## Integration test -- builds an ecoClassify library from scratch with small
## synthetic rasters and runs Train Classifier -> Predict -> Post-Process
## end-to-end with Random Forest.
##
## Requires SyncroSim + the ecoClassify package to be installed (and an R
## configured in SyncroSim with the package's dependencies); self-skips otherwise.

# Datasheet file columns may come back as bare file names; resolve them against
# the library's data folder for the scenario.
resolve_output_file <- function(path, libraryPath, scenarioId, datasheetName) {
  if (is.na(path) || file.exists(path)) {
    return(path)
  }
  candidates <- list.files(
    paste0(libraryPath, ".data"),
    pattern = paste0("^", basename(path), "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  inScenario <- grep(paste0("Scenario-", scenarioId), candidates, value = TRUE)
  if (length(inScenario) > 0) inScenario[1] else if (length(candidates) > 0) candidates[1] else path
}

expect_output_files <- function(df, columns, libraryPath, scenarioId, datasheetName) {
  for (column in intersect(columns, names(df))) {
    for (path in stats::na.omit(df[[column]])) {
      resolved <- resolve_output_file(path, libraryPath, scenarioId, datasheetName)
      expect_true(file.exists(resolved), info = paste(datasheetName, column, path))
    }
  }
}

test_that("a synthetic library trains, predicts and post-processes with Random Forest", {
  skip_if_not(syncrosim_available(), "SyncroSim + the ecoClassify package are not available")

  library(rsyncrosim)

  workDir <- file.path(tempdir(), paste0("ecoclassify-synthetic-test-", Sys.getpid()))
  dir.create(workDir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(workDir, recursive = TRUE, force = TRUE), add = TRUE)

  # -- synthetic inputs: two training timesteps and one predicting timestep
  set.seed(42)
  rasterFiles <- list()
  for (t in 1:3) {
    pair <- make_separable_pair(nrows = 50, ncols = 50)
    rasterFiles[[t]] <- c(
      image = file.path(workDir, paste0("image-t", t, ".tif")),
      groundTruth = file.path(workDir, paste0("groundtruth-t", t, ".tif"))
    )
    terra::writeRaster(pair$image, rasterFiles[[t]][["image"]], overwrite = TRUE)
    terra::writeRaster(pair$groundTruth, rasterFiles[[t]][["groundTruth"]],
                       overwrite = TRUE, datatype = "INT1U")
  }

  # -- library, project and scenario
  libraryPath <- file.path(workDir, "ecoclassify-synthetic-test.ssim")
  # Set ECOCLASSIFY_TEST_CONDA=true to run the transformers in the package's conda environment
  # (what Studio users get) instead of the R configured in SyncroSim. The first conda run builds
  # the environment and installs every R package, so it can take 20+ minutes.
  useConda <- tolower(Sys.getenv("ECOCLASSIFY_TEST_CONDA", "false")) == "true"
  myLibrary <- ssimLibrary(
    name = libraryPath,
    session = session(),
    packages = "ecoClassify",
    overwrite = TRUE,
    useConda = useConda
  )
  myProject <- rsyncrosim::project(myLibrary, project = "Definitions")
  myScenario <- scenario(myProject, scenario = "Synthetic Random Forest")

  saveDatasheet(myScenario, data.frame(
    Timesteps = 1:2,
    TrainingRasterFile = c(rasterFiles[[1]][["image"]], rasterFiles[[2]][["image"]]),
    GroundTruthRasterFile = c(rasterFiles[[1]][["groundTruth"]], rasterFiles[[2]][["groundTruth"]])
  ), "ecoClassify_InputTrainingRasters")

  saveDatasheet(myScenario, data.frame(
    Timesteps = 3,
    predictingRasterFile = rasterFiles[[3]][["image"]]
  ), "ecoClassify_InputPredictingRasters")

  saveDatasheet(myScenario, data.frame(nObs = 1000, modelType = "Random Forest"),
                "ecoClassify_ClassifierOptions")

  # Tuning is on by default; turn it off to keep the run fast.
  saveDatasheet(myScenario, data.frame(
    normalizeRasters = FALSE,
    modelTuning = FALSE,
    tuningObjective = "Youden",
    setManualThreshold = FALSE,
    applyContextualization = FALSE,
    setSeed = 1
  ), "ecoClassify_AdvancedClassifierOptions")

  saveDatasheet(myScenario, data.frame(applyFiltering = TRUE, filterValue = 4, fillValue = 6),
                "ecoClassify_PostProcessingFilter")

  # StageNameId takes the transformer displayName from src/package.xml.
  saveDatasheet(myScenario, data.frame(
    StageNameId = c("1-Train Classifier", "2-Predict", "3-Post-Process"),
    RunOrder = 1:3
  ), "core_Pipeline")

  # -- run
  myResultScenario <- run(myScenario)
  resultId <- scenarioId(myResultScenario)

  # -- outputs
  modelObject <- datasheet(myResultScenario, "ecoClassify_ModelObject")
  expect_gte(nrow(modelObject), 1)
  expect_true(all(modelObject$Threshold > 0 & modelObject$Threshold < 1))

  rasterOutput <- datasheet(myResultScenario, "ecoClassify_RasterOutput")
  expect_gte(nrow(rasterOutput), 2) # one row per training timestep
  expect_output_files(rasterOutput, c("PredictedUnfiltered", "PredictedFiltered", "Probability"),
                      libraryPath, resultId, "ecoClassify_RasterOutput")

  classified <- datasheet(myResultScenario, "ecoClassify_ClassifiedRasterOutput")
  expect_gte(nrow(classified), 1)
  expect_output_files(classified, c("ClassifiedUnfiltered", "ClassifiedFiltered", "ClassifiedProbability"),
                      libraryPath, resultId, "ecoClassify_ClassifiedRasterOutput")

  summaryOutput <- datasheet(myResultScenario, "ecoClassify_SummaryOutput")
  expect_gte(nrow(summaryOutput), 1)

  # The classes are separable, so the model should do well on held-out data.
  stats <- datasheet(myResultScenario, "ecoClassify_ModelStatistics")
  accuracy <- stats$Value[stats$Statistic == "Accuracy"]
  expect_length(accuracy, 1)
  expect_gt(accuracy, 0.9)
})
