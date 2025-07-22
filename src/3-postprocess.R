## ---------------------------------------
## ecoClassify - Filtering training or predicting steps
## ApexRMS, November 2024
## ---------------------------------------

# Set up workspace -------------------------------------------------------------

packageDir <- (Sys.getenv("ssim_package_directory"))

sourceScripts <- list.files(
  path = file.path(packageDir, "/functions"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

invisible(lapply(sourceScripts, source))

progressBar(type = "message", 
            message = "Loading input data and setting up scenario")

# Get the SyncroSim Scenario that is currently running
myScenario <- scenario()

# Retrieve the transfer directory for storing output rasters
e <- ssimEnvironment()
transferDir <- e$TransferDirectory



# Load post-processing settings ------------------------------------------------

# Filtering -----------------------------------------------------

postProcessingDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_PostProcessingFilter")

filterResolution <- postProcessingDataframe$filterResolution
filterPercent <- postProcessingDataframe$filterPercent
applyFiltering <- postProcessingDataframe$applyFiltering

# Apply default filtering values if not specified
if (is.na(filterResolution) && applyFiltering == TRUE) {
  filterResolution <- 5
  updateRunLog(
    "Filter resolution was not supplied; using default value of 5",
    type = "info"
  )
}
if (is.na(filterPercent) && applyFiltering == TRUE) {
  filterPercent <- 0.25
  updateRunLog(
    "Filter percent was not supplied; using default value of 0.25",
    type = "info"
  )
}

# Output datasheets ---------------------------------------------

trainingOutputDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_RasterOutput")

predictingOutputDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_ClassifiedRasterOutput")


# Unique timesteps ----------------------------------------------

trainingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputTrainingRasters")
predictingRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_InputPredictingRasters")

trainTimestepList <- trainingRasterDataframe %>%
  filter(!is.na(TrainingRasterFile)) %>%
  pull(Timesteps) %>%
  unique()
predTimestepList <- predictingRasterDataframe %>%
  filter(!is.na(predictingRasterFile)) %>%
  pull(Timesteps) %>%
  unique()



# Function ---------------------------------------------------------------------

filterRasterDataframe <- function(
    applyFiltering,
    predictedPresence,
    filterResolution,
    filterPercent,
    category,
    timestep,
    transferDir) {
  
  # Filter out presence pixels surrounded by non-presence
  filteredPredictedPresence <- focal(
     predictedPresence,
     w = matrix(1, 5, 5),
     fun = filterFun,
     resolution = filterResolution,
     percent = filterPercent
  )
  
  # File path
  filteredPath <- file.path(paste0(transferDir,
                                   "/filteredPredictedPresence-",
                                   category,
                                   "-t",
                                   timestep,
                                   ".tif"))
  
  # Save raster
  writeRaster(filteredPredictedPresence,
              filename = filteredPath,
              overwrite = TRUE)
 
  # Build dataframe
  rasterDataframe <- data.frame(
    Timestep = timestep,
    PredictedFiltered = filteredPath)

  return(rasterDataframe)
}

# Filter raster ----------------------------------------------------------------

progressBar(type = "message", 
            message = "Filtering")

# Training step -------------------------------------------------

for(t in trainTimestepList){

  # Get file paths
  predictedPresenceFilepath <- trainingOutputDataframe$PredictedUnfiltered[
      trainingOutputDataframe$Timestep == t]
  
  if(!is.na(predictedPresenceFilepath)){

    # Load raster
    predictedPresence <- rast(predictedPresenceFilepath)

    # Filter
    filteredTraining <- filterRasterDataframe(
      applyFiltering,
      predictedPresence,
      filterResolution,
      filterPercent,
      "training",
      t,
      transferDir)

    # Combine results
    trainingOutputDataframe$PredictedFiltered[
      trainingOutputDataframe$Timestep == t] <- filteredTraining$PredictedFiltered
  }
}

# Predicting step -----------------------------------------------

for(t in predTimestepList){
  
  # Get file paths
  classifiedPresenceFilepath <- predictingOutputDataframe$ClassifiedUnfiltered[
    predictingOutputDataframe$Timestep == t]
  
  if(!is.null(classifiedPresenceFilepath)){
    
    # Load raster
    predictedPresence <- rast(classifiedPresenceFilepath)
    
    # Filter
    filteredPredicting <- filterRasterDataframe(
      applyFiltering,
      predictedPresence,
      filterResolution,
      filterPercent,
      "predicting",
      t,
      transferDir)   
    
    # Rename column
    names(filteredPredicting)[2] <- 
    
    # Combine results
    predictingOutputDataframe$ClassifiedFiltered[
      predictingOutputDataframe$Timestep == t] <- filteredPredicting$PredictedFiltered
  }
}

# Save datasheets back to SyncroSim
if(dim(trainingOutputDataframe)[1] != 0){
  saveDatasheet(
    myScenario,
    data = trainingOutputDataframe,
    name = "ecoClassify_RasterOutput"
  )
}
if(dim(predictingOutputDataframe)[1] != 0){
  saveDatasheet(
    myScenario,
    data = predictingOutputDataframe,
    name = "ecoClassify_ClassifiedRasterOutput"
  )
}
