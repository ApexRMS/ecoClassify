source("src/0-helper-functions.r")

rasters <- list.files("testing/snow/", pattern = ".tif$", full.names = TRUE)
trainingRasterDataframe <- data.frame(
    Timesteps = 1:3,
    preds = rasters[1:3],
    resp = rasters[4:6],
    stringsAsFactors = FALSE
)
