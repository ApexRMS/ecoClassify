## Integration tests -- build ecoClassify libraries from scratch with small
## synthetic rasters and run the transformers end-to-end with Random Forest.
##
## Requires SyncroSim + the ecoClassify package to be installed (and an R
## configured in SyncroSim with the package's dependencies); self-skip otherwise.
## Install your working copy first (src/build.ps1 -Install), otherwise these test
## the released package.

# Datasheet file columns may come back as bare file names; resolve them against
# the library's data folder for the scenario.
resolve_output_file <- function(path, libraryPath, scenarioId) {
  if (is.na(path) || file.exists(path)) {
    return(path)
  }
  candidates <- list.files(
    paste0(libraryPath, ".data"),
    pattern = paste0("^", basename(path), "$"),
    recursive = TRUE,
    full.names = TRUE
  )
  inScenario <- grep(paste0("Scenario-", scenarioId, "[/\\\\]"), candidates, value = TRUE)
  if (length(inScenario) > 0) inScenario[1] else if (length(candidates) > 0) candidates[1] else path
}

expect_output_files <- function(df, columns, libraryPath, scenarioId, datasheetName) {
  for (column in intersect(columns, names(df))) {
    for (path in stats::na.omit(df[[column]])) {
      resolved <- resolve_output_file(path, libraryPath, scenarioId)
      expect_true(file.exists(resolved), info = paste(datasheetName, column, path))
    }
  }
}

# Writes `n` synthetic image/ground-truth pairs. Ground truth uses classes
# 1 (absence) and 2 (target) so the target-class path is exercised.
write_synthetic_rasters <- function(dir, n = 3, size = 50) {
  set.seed(42)
  lapply(seq_len(n), function(t) {
    pair <- make_separable_pair(nrows = size, ncols = size)
    paths <- c(
      image = file.path(dir, paste0("image-t", t, ".tif")),
      groundTruth = file.path(dir, paste0("groundtruth-t", t, ".tif"))
    )
    terra::writeRaster(pair$image, paths[["image"]], overwrite = TRUE)
    terra::writeRaster(pair$groundTruth + 1, paths[["groundTruth"]], overwrite = TRUE, datatype = "INT1U")
    paths
  })
}

# Builds a scenario. `predictFiles` are the predicting images (timesteps 3, 4, ...).
build_scenario <- function(project, name, rasterFiles, predictFiles, stages,
                           normalize = FALSE, contextualize = FALSE, extra = NULL) {
  sc <- rsyncrosim::scenario(project, scenario = name)
  rsyncrosim::saveDatasheet(sc, data.frame(
    Timesteps = 1:2,
    TrainingRasterFile = c(rasterFiles[[1]][["image"]], rasterFiles[[2]][["image"]]),
    GroundTruthRasterFile = c(rasterFiles[[1]][["groundTruth"]], rasterFiles[[2]][["groundTruth"]])
  ), "ecoClassify_InputTrainingRasters")
  rsyncrosim::saveDatasheet(sc, data.frame(
    Timesteps = 2 + seq_along(predictFiles),
    predictingRasterFile = predictFiles
  ), "ecoClassify_InputPredictingRasters")
  rsyncrosim::saveDatasheet(sc, data.frame(nObs = 1000, modelType = "Random Forest"),
                            "ecoClassify_ClassifierOptions")
  # Tuning is on by default; turn it off to keep the run fast.
  rsyncrosim::saveDatasheet(sc, data.frame(
    normalizeRasters = normalize,
    modelTuning = FALSE,
    tuningObjective = "Youden",
    setManualThreshold = FALSE,
    applyContextualization = contextualize,
    contextualizationWindowSize = 3,
    setSeed = 1
  ), "ecoClassify_AdvancedClassifierOptions")
  rsyncrosim::saveDatasheet(sc, data.frame(useTargetClass = TRUE, targetClassValue = 2L,
                                           targetClassLabel = "Target"),
                            "ecoClassify_TargetClassOptions")
  rsyncrosim::saveDatasheet(sc, data.frame(applyFiltering = TRUE, filterValue = 4, fillValue = 6),
                            "ecoClassify_PostProcessingFilter")
  for (ds in names(extra)) rsyncrosim::saveDatasheet(sc, extra[[ds]], ds)
  # StageNameId takes the transformer displayName from src/package.xml.
  rsyncrosim::saveDatasheet(sc, data.frame(StageNameId = stages, RunOrder = seq_along(stages)),
                            "core_Pipeline")
  sc
}

new_test_library <- function(workDir, name) {
  # Set ECOCLASSIFY_TEST_CONDA=true to run the transformers in the package's conda
  # environment (what Studio users get) instead of the R configured in SyncroSim.
  useConda <- tolower(Sys.getenv("ECOCLASSIFY_TEST_CONDA", "false")) == "true"
  libraryPath <- file.path(workDir, paste0(name, ".ssim"))
  lib <- rsyncrosim::ssimLibrary(name = libraryPath, session = rsyncrosim::session(),
                                 packages = "ecoClassify", overwrite = TRUE, useConda = useConda)
  list(path = libraryPath, project = rsyncrosim::project(lib, project = "Definitions"))
}

new_work_dir <- function(label) {
  workDir <- file.path(tempdir(), paste0("ecoclassify-", label, "-", Sys.getpid()))
  dir.create(workDir, showWarnings = FALSE, recursive = TRUE)
  workDir
}

allStages <- c("1-Train Classifier", "2-Predict", "3-Post-Process")

test_that("a synthetic library trains, predicts and post-processes with Random Forest", {
  skip_if_not(syncrosim_available(), "SyncroSim + the ecoClassify package are not available")

  workDir <- new_work_dir("synthetic")
  on.exit(unlink(workDir, recursive = TRUE, force = TRUE), add = TRUE)
  files <- write_synthetic_rasters(workDir)
  lib <- new_test_library(workDir, "synthetic")

  sc <- build_scenario(lib$project, "Synthetic Random Forest", files,
                       files[[3]][["image"]], allStages)
  res <- rsyncrosim::run(sc)
  resultId <- rsyncrosim::scenarioId(res)

  modelObject <- rsyncrosim::datasheet(res, "ecoClassify_ModelObject")
  expect_gte(nrow(modelObject), 1)
  expect_true(all(modelObject$Threshold > 0 & modelObject$Threshold < 1))

  rasterOutput <- rsyncrosim::datasheet(res, "ecoClassify_RasterOutput")
  expect_gte(nrow(rasterOutput), 2) # one row per training timestep
  expect_output_files(rasterOutput, c("PredictedUnfiltered", "PredictedFiltered", "Probability"),
                      lib$path, resultId, "ecoClassify_RasterOutput")

  classified <- rsyncrosim::datasheet(res, "ecoClassify_ClassifiedRasterOutput")
  expect_gte(nrow(classified), 1)
  expect_output_files(classified, c("ClassifiedUnfiltered", "ClassifiedFiltered", "ClassifiedProbability"),
                      lib$path, resultId, "ecoClassify_ClassifiedRasterOutput")

  # Outputs use a datatype SyncroSim can read (it rejects Byte/Int16)
  presence <- terra::rast(resolve_output_file(classified$ClassifiedUnfiltered[1], lib$path, resultId))
  expect_true(terra::datatype(presence) %in% c("INT4S", "FLT4S", "FLT8S"))

  summaryOutput <- rsyncrosim::datasheet(res, "ecoClassify_SummaryOutput")
  expect_gte(nrow(summaryOutput), 1)

  # The classes are separable, so the model should do well. With target class 2,
  # recall must be real: post-processing used to zero every truth cell whenever
  # the target class value was not 1, which made recall NA.
  stats <- rsyncrosim::datasheet(res, "ecoClassify_ModelStatistics")
  getStat <- function(name) stats$Value[stats$Statistic == name]
  expect_length(getStat("Accuracy"), 1)
  expect_gt(getStat("Accuracy"), 0.9)
  expect_false(is.na(getStat("Recall")))
  expect_gt(getStat("Recall"), 0.8)
})

test_that("predicting on a copy of a training image reproduces its training probabilities", {
  skip_if_not(syncrosim_available(), "SyncroSim + the ecoClassify package are not available")

  # Normalization + contextualization on: prediction must reuse the PCA fitted
  # in training (it used to refit one per raster, giving different features).
  workDir <- new_work_dir("reproduce")
  on.exit(unlink(workDir, recursive = TRUE, force = TRUE), add = TRUE)
  files <- write_synthetic_rasters(workDir)
  lib <- new_test_library(workDir, "reproduce")

  sc <- build_scenario(lib$project, "Predict training image", files,
                       files[[1]][["image"]], allStages[1:2],
                       normalize = TRUE, contextualize = TRUE)
  res <- rsyncrosim::run(sc)
  resultId <- rsyncrosim::scenarioId(res)

  trainOut <- rsyncrosim::datasheet(res, "ecoClassify_RasterOutput")
  predOut <- rsyncrosim::datasheet(res, "ecoClassify_ClassifiedRasterOutput")
  trainProb <- terra::rast(resolve_output_file(trainOut$Probability[trainOut$Timestep == 1][1], lib$path, resultId))
  predProb <- terra::rast(resolve_output_file(predOut$ClassifiedProbability[1], lib$path, resultId))

  expect_equal(as.vector(terra::values(predProb)), as.vector(terra::values(trainProb)), tolerance = 1e-6)
})

test_that("tiled prediction matches untiled prediction", {
  skip_if_not(syncrosim_available(), "SyncroSim + the ecoClassify package are not available")

  workDir <- new_work_dir("tiling")
  on.exit(unlink(workDir, recursive = TRUE, force = TRUE), add = TRUE)
  files <- write_synthetic_rasters(workDir, size = 60)
  lib <- new_test_library(workDir, "tiling")

  # Same multiprocessing settings in both, so only the tiling differs
  untiled <- rsyncrosim::run(build_scenario(
    lib$project, "Untiled", files, files[[3]][["image"]], allStages[1:2],
    normalize = TRUE, contextualize = TRUE,
    extra = list(core_Multiprocessing = data.frame(EnableMultiprocessing = TRUE, MaximumJobs = 2L))
  ))
  tiled <- rsyncrosim::run(build_scenario(
    lib$project, "Tiled", files, files[[3]][["image"]],
    c("1-Train Classifier", "2a-Prepare Spatial Tiling", "2-Predict"),
    normalize = TRUE, contextualize = TRUE,
    extra = list(
      ecoClassify_TilingOptions = data.frame(AutoTiling = FALSE, NumTilesX = 2L, NumTilesY = 2L),
      core_Multiprocessing = data.frame(EnableMultiprocessing = TRUE, MaximumJobs = 2L)
    )
  ))

  probOf <- function(res) {
    d <- rsyncrosim::datasheet(res, "ecoClassify_ClassifiedRasterOutput")
    terra::rast(resolve_output_file(d$ClassifiedProbability[1], lib$path, rsyncrosim::scenarioId(res)))
  }
  expect_equal(as.vector(terra::values(probOf(tiled))), as.vector(terra::values(probOf(untiled))),
               tolerance = 1e-6)
})
