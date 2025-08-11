# Helper functions and setup for ecoClassify tests

# Create test rasters
create_test_raster <- function(nrows = 10, ncols = 10, ext = c(0, 10, 0, 10), crs = "EPSG:4326") {
  r <- rast(nrows = nrows, ncols = ncols, ext = ext, crs = crs)
  values(r) <- runif(ncell(r), min = 0, max = 100)
  return(r)
}

# Create binary test raster (for ground truth)
create_binary_raster <- function(nrows = 10, ncols = 10, ext = c(0, 10, 0, 10), crs = "EPSG:4326", prob = 0.3) {
  r <- rast(nrows = nrows, ncols = ncols, ext = ext, crs = crs)
  values(r) <- rbinom(ncell(r), 1, prob)
  return(r)
}

# Create raster stack with multiple bands
create_test_stack <- function(nlayers = 3, nrows = 10, ncols = 10, ext = c(0, 10, 0, 10), crs = "EPSG:4326") {
  raster_list <- lapply(1:nlayers, function(i) {
    r <- rast(nrows = nrows, ncols = ncols, ext = ext, crs = crs)
    values(r) <- runif(ncell(r), min = i * 10, max = (i + 1) * 10)
    names(r) <- paste0("band_", i)
    return(r)
  })
  return(rast(raster_list))
}

# Create mock SyncroSim scenario object
create_mock_scenario <- function() {
  mock_scenario <- structure(list(
    ScenarioId = 1L,
    ProjectId = 1L,
    Name = "test_scenario"
  ), class = "SsimObject")
  return(mock_scenario)
}

# Create mock datasheet
create_mock_datasheet <- function(data) {
  return(data)
}

# Create test data directory if it doesn't exist
setup_test_data_dir <- function() {
  test_data_dir <- file.path(tempdir(), "ecoClassify_test_data")
  if (!dir.exists(test_data_dir)) {
    dir.create(test_data_dir, recursive = TRUE)
  }
  return(test_data_dir)
}