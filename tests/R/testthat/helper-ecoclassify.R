## -------------------------------------------------------------------------
## Shared test helpers: locating src/, sourcing function files, synthetic
## rasters, and detecting a SyncroSim install
## -------------------------------------------------------------------------

# Walk upward from `start` until a directory containing src/package.xml is
# found. Lets tests locate the repo root regardless of the working directory
# testthat happens to run from.
find_ecoclassify_repo_root <- function(start = getwd()) {
  path <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "src", "package.xml"))) {
      return(path)
    }
    parent <- dirname(path)
    if (parent == path) {
      stop("Could not locate the ecoClassify repo root (src/package.xml) above ", start)
    }
    path <- parent
  }
}

ecoclassify_src_dir <- file.path(find_ecoclassify_repo_root(), "src")
ecoclassify_functions_dir <- file.path(ecoclassify_src_dir, "functions")

# Transformers read their own files relative to this, exactly as SyncroSim
# sets it up when invoking a transformer.
Sys.setenv(ssim_package_directory = ecoclassify_src_dir)

# Source one of the src/functions/*.r files into `env` for unit testing.
#
# - Sources 0.0-setup.r into the global environment once first, since it
#   loads the packages (terra, tidyverse, tidymodels, ...) the other files use
#   without loading themselves.
# - Stubs `updateRunLog()` and `progressBar()` (normally supplied by
#   rsyncrosim against a live scenario) so functions that log can run here.
source_ecoclassify_r <- function(filename, env = parent.frame()) {
  if (!exists("loadModelPackages", envir = globalenv(), inherits = FALSE)) {
    sys.source(file.path(ecoclassify_functions_dir, "0.0-setup.r"), envir = globalenv())
  }
  for (stub in c("updateRunLog", "progressBar")) {
    if (!exists(stub, envir = env, inherits = FALSE)) {
      assign(stub, function(...) invisible(NULL), envir = env)
    }
  }
  sys.source(file.path(ecoclassify_functions_dir, filename), envir = env)
}

# ---- synthetic rasters ------------------------------------------------------

# A single-layer raster of uniform random values.
make_test_raster <- function(nrows = 10, ncols = 10, min = 0, max = 100,
                             crs = "EPSG:3857") {
  r <- terra::rast(nrows = nrows, ncols = ncols, xmin = 0, xmax = ncols * 10,
                   ymin = 0, ymax = nrows * 10, crs = crs)
  terra::values(r) <- stats::runif(terra::ncell(r), min = min, max = max)
  r
}

# A multi-band raster with bands named band_1..band_n, each in its own range.
make_test_stack <- function(nlayers = 3, nrows = 10, ncols = 10, crs = "EPSG:3857") {
  layers <- lapply(seq_len(nlayers), function(i) {
    make_test_raster(nrows, ncols, min = i * 10, max = (i + 1) * 10, crs = crs)
  })
  s <- terra::rast(layers)
  names(s) <- paste0("band_", seq_len(nlayers))
  s
}

# A synthetic training pair whose classes are separable: a ground truth raster
# with presence (1) in the left half and absence (0) in the right half, and a
# 3-band image whose band values differ by class (plus noise).
make_separable_pair <- function(nrows = 40, ncols = 40, crs = "EPSG:3857") {
  gt <- terra::rast(nrows = nrows, ncols = ncols, xmin = 0, xmax = ncols * 10,
                    ymin = 0, ymax = nrows * 10, crs = crs)
  cols <- terra::colFromCell(gt, seq_len(terra::ncell(gt)))
  terra::values(gt) <- as.integer(cols <= ncols / 2)
  names(gt) <- "groundTruth"

  gtVals <- terra::values(gt, mat = FALSE)
  bands <- lapply(1:3, function(i) {
    b <- terra::rast(gt)
    terra::values(b) <- gtVals * (20 * i) + stats::rnorm(length(gtVals), mean = 10 * i, sd = 3)
    names(b) <- paste0("band_", i)
    b
  })

  list(image = terra::rast(bands), groundTruth = gt)
}

# ---- SyncroSim -----------------------------------------------------------------

# TRUE if a SyncroSim session is reachable and the ecoClassify package is
# installed in it, so integration tests can self-skip on a machine without
# SyncroSim Studio + the ecoClassify package.
syncrosim_available <- function() {
  if (!requireNamespace("rsyncrosim", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch(
    {
      mySession <- rsyncrosim::session()
      pkgs <- rsyncrosim::packages(mySession)
      "ecoClassify" %in% pkgs$name
    },
    error = function(e) FALSE
  )
}
