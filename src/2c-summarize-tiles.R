## ---------------------------------------
## ecoClassify - Summarize Tiled Predictions
## ApexRMS, November 2026
## ---------------------------------------

# Builds the Prediction Summary (SummaryOutput / SummaryOutputChart) for
# spatially tiled prediction runs. Tile jobs in 2b-predict.R each see only part
# of the map, so they skip the summary; this transformer runs once after the
# tiles are merged and summarizes the full classified rasters, giving exact
# totals and an exact median.
#
# For non-tiled runs 2b-predict.R has already written the "predicting" summary
# rows, so this transformer detects them and exits without changing anything.

# Set up workspace -------------------------------------------------------------

packageDir <- (Sys.getenv("ssim_package_directory"))

source(file.path(packageDir, "installDependencies.r"))

sourceScripts <- list.files(
  path = file.path(packageDir, "/functions"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)

invisible(lapply(sourceScripts, source))

progressBar(type = "message", message = "Summarizing tiled predictions")

myScenario <- scenario()

# Skip if 2b-Predict already summarized (non-tiled run) ------------------------

existingSummary <- datasheet(myScenario, name = "ecoClassify_SummaryOutput")

if (
  nrow(existingSummary) > 0 &&
    any(existingSummary$PredictionType == "predicting", na.rm = TRUE)
) {
  updateRunLog(
    paste0(
      "Prediction summary already exists (prediction was not tiled); ",
      "nothing to summarize."
    ),
    type = "info"
  )
  progressBar(type = "message", message = "Summarize tiles complete")
  quit(save = "no", status = 0)
}

# Load merged classified rasters -----------------------------------------------

classifiedRasterDataframe <- datasheet(
  myScenario,
  name = "ecoClassify_ClassifiedRasterOutput"
)

if (nrow(classifiedRasterDataframe) == 0) {
  updateRunLog(
    paste0(
      "No classified rasters found; run 2b-Predict before 2c-Summarize Tiles. ",
      "No prediction summary was written."
    ),
    type = "warning"
  )
  quit(save = "no", status = 0)
}

# Read TargetClassOptions (same parsing as 2b-predict.R) ------------------------

targetClassSheet <- datasheet(myScenario, "ecoClassify_TargetClassOptions")
targetClassValue <- NA_integer_
targetClassLabel <- NA_character_
if (nrow(targetClassSheet) > 0) {
  if (
    !is.null(targetClassSheet$targetClassValue) &&
      isTRUE(!is.na(targetClassSheet$targetClassValue[1]))
  ) {
    targetClassValue <- as.integer(targetClassSheet$targetClassValue[1])
  }
  if (
    !is.null(targetClassSheet$targetClassLabel) &&
      isTRUE(!is.na(targetClassSheet$targetClassLabel[1]))
  ) {
    targetClassLabel <- as.character(targetClassSheet$targetClassLabel[1])
  }
}

# Summarize each timestep ------------------------------------------------------

firstValidPath <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) NA_character_ else paths[1]
}

timestepList <- sort(unique(classifiedRasterDataframe$Timestep))
summaryRows <- list()

for (t in timestepList) {
  rowIdx <- which(classifiedRasterDataframe$Timestep == t)
  predictionPath <- firstValidPath(
    classifiedRasterDataframe$ClassifiedUnfiltered[rowIdx]
  )
  probabilityPath <- firstValidPath(
    classifiedRasterDataframe$ClassifiedProbability[rowIdx]
  )

  if (is.na(predictionPath) || is.na(probabilityPath)) {
    updateRunLog(
      paste0(
        "Timestep ", t, ": classified or probability raster missing; ",
        "skipping summary."
      ),
      type = "warning"
    )
    next
  }

  tryCatch(
    {
      summaryRows[[length(summaryRows) + 1]] <- buildSummaryRow(
        predictionRaster = terra::rast(predictionPath),
        probabilityRaster = terra::rast(probabilityPath),
        timestep = t,
        predictionType = "predicting",
        targetClassValue = targetClassValue,
        targetClassLabel = targetClassLabel
      )
    },
    error = function(e) {
      updateRunLog(
        paste0(
          "Could not build summary row for timestep ", t, ": ",
          conditionMessage(e)
        ),
        type = "warning"
      )
    }
  )
}

# Save -------------------------------------------------------------------------

if (length(summaryRows) > 0) {
  summaryDf <- do.call(rbind, summaryRows)
  saveDatasheet(
    myScenario,
    data = summaryDf,
    name = "ecoClassify_SummaryOutput"
  )
  saveDatasheet(
    myScenario,
    data = summaryDf,
    name = "ecoClassify_SummaryOutputChart"
  )
  updateRunLog(
    paste0(
      "Saved prediction summary for ", nrow(summaryDf),
      " timestep(s) from the merged tiled rasters."
    ),
    type = "info"
  )
} else {
  updateRunLog(
    "No prediction summary rows could be built.",
    type = "warning"
  )
}

terra::tmpFiles(remove = TRUE)

progressBar(type = "message", message = "Summarize tiles complete")
