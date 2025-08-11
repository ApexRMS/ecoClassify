source("src/0-helper-functions.r")

rasters <- list.files("testing/snow/", pattern = ".tif$", full.names = TRUE)
trainingRasterDataframe <- data.frame(
    Timesteps = 1:3,
    preds = rasters[1:3],
    resp = rasters[4:6],
    stringsAsFactors = FALSE
)


rasters <- list.files(
    "D:/Github/B002-RootTraces/data/train",
    pattern = ".tif$",
    full.names = TRUE
)
trainingRasterDataframe <- data.frame(
    Timesteps = 1,
    preds = rasters[c(1)],
    resp = rasters[c(2)],
    stringsAsFactors = FALSE
)
