## -------------------------------
## ecoClassify - Setup Functions
## ApexRMS, November 2024
## -------------------------------

# suppress additional warnings ----
load_pkg <- function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
}

quiet <- function(expr) suppressPackageStartupMessages(expr)

quiet({
  pkgs <- c(
    "terra",
    "tidyverse",
    "magrittr",
    "foreach",
    "iterators",
    "parallel",
    "rsyncrosim",
    "sf",
    "gtools",
    "codetools",
    "cvms",
    "doParallel",
    "tidymodels"
  )
  invisible(lapply(pkgs, load_pkg))
})

loadModelPackages <- function(modelType) {
  if (modelType == "MaxEnt") {
    options(java.parameters = "-Xmx4g")
    rJavaLoaded <- tryCatch(
      {
        if (!requireNamespace("rJava", quietly = TRUE)) {
          warning("rJava package not available. MaxEnt functionality will be skipped.")
          FALSE
        } else {
          suppressPackageStartupMessages(library("rJava", character.only = TRUE))
          TRUE
        }
      },
      error = function(e) {
        warning("rJava failed to load (Java configuration issue). MaxEnt functionality will be skipped.")
        FALSE
      }
    )
    if (rJavaLoaded) {
      enmevalLoaded <- tryCatch(
        {
          if (!requireNamespace("ENMeval", quietly = TRUE)) {
            warning("ENMeval package not available. MaxEnt functionality will be skipped.")
            FALSE
          } else {
            suppressPackageStartupMessages(library("ENMeval", character.only = TRUE))
            TRUE
          }
        },
        error = function(e) {
          warning("ENMeval failed to load. MaxEnt functionality will be skipped.")
          FALSE
        }
      )
      MAXENT_AVAILABLE <<- enmevalLoaded
    }
  } else if (modelType == "Random Forest") {
    load_pkg("ranger")
  } else if (modelType == "CNN") {
    load_pkg("torch")
    load_pkg("coro")
  }
}

#' Set the number of cores for multiprocessing ----
#'
#' @description
#' 'setCores' determines the number of cores to use for multiprocessing
#' based on the available cores and user settings.
#'
#' @param mulitprocessingSheet A dataframe containing multiprocessing settings.
#' It should have the columns 'EnableMultiprocessing' (logical) and
#' 'MaximumJobs' (integer).
#' @return The number of cores to use for multiprocessing (integer).
#'
#' @details
#' The function first detects the number of available cores on the system.
#' If multiprocessing is enabled in the 'mulitprocessingSheet', it checks if
#' the requested number of cores exceeds the available cores. If so, it sets
#' the number of cores to one less than the available cores and issues a warning.
#' Otherwise, it sets the number of cores to the requested number. If
#' multiprocessing is not enabled, it sets the number of cores to 1.
#' @noRd
setCores <- function(mulitprocessingSheet) {
  availableCores <- parallel::detectCores()
  if (is.na(availableCores)) availableCores <- 1L

  # Treat a missing row or NA settings as "multiprocessing disabled"
  enabled <- nrow(mulitprocessingSheet) > 0 &&
    isTRUE(as.logical(mulitprocessingSheet$EnableMultiprocessing[1]))
  if (!enabled) {
    return(1L)
  }

  requestedCores <- as.integer(mulitprocessingSheet$MaximumJobs[1])
  if (is.na(requestedCores) || requestedCores < 1L) {
    return(1L)
  }

  if (requestedCores > availableCores) {
    nCores <- max(1L, availableCores - 1L)
    updateRunLog(
      paste0(
        "Requested number of jobs exceeds available cores. Continuing run with ",
        nCores,
        " jobs."
      ),
      type = "warning"
    )
  } else {
    nCores <- requestedCores
  }
  return(nCores)
}

#' Create the Run's Temporary Folder ----
#'
#' @description
#' Creates a temporary folder for this transformer run inside the SyncroSim
#' transfer directory and sends every temporary file there:
#' * terra's own temporary rasters (`terraOptions(tempdir = )`)
#' * files named with `runTempFile()`
#' * the session temp folders of child R processes (doParallel/PSOCK workers,
#'   e.g. ENMeval's MaxEnt fits), which read `TMPDIR` when they start. Workers
#'   that are stopped abruptly otherwise leave `Rtmp*` folders in the system
#'   temp folder.
#'
#' The folder is removed by `removeRunTempDir()` at the end of the script and,
#' through an exit finalizer, when R exits after an error or `quit()`.
#' The process ID in the name keeps concurrent jobs from sharing a folder.
#'
#' @param transferDir The SyncroSim transfer directory.
#'
#' @return The path of the temporary folder (invisibly).
#'
#' @noRd
createRunTempDir <- function(transferDir) {
  runTempDir <- file.path(
    transferDir,
    paste0("ecoClassifyTemp-", Sys.getpid())
  )
  dir.create(runTempDir, recursive = TRUE, showWarnings = FALSE)
  runTempDir <- normalizePath(runTempDir, winslash = "/", mustWork = TRUE)

  terra::terraOptions(tempdir = runTempDir)
  options(ecoClassify.runTempDir = runTempDir)
  Sys.setenv(TMPDIR = runTempDir)

  # Exit finalizers run on normal exit, quit() and after an error in Rscript
  guard <- new.env()
  reg.finalizer(guard, function(e) removeRunTempDir(runTempDir), onexit = TRUE)
  assign(".ecoClassifyRunTempGuard", guard, envir = globalenv())

  invisible(runTempDir)
}

#' Temporary File Path in the Run's Temporary Folder ----
#'
#' @description
#' Like `tempfile()`, but inside the folder from `createRunTempDir()` so the
#' file is removed with it. Falls back to R's `tempdir()` when no run folder
#' was created (e.g. in unit tests).
#'
#' @noRd
runTempFile <- function(fileext = ".tif") {
  runTempDir <- getOption("ecoClassify.runTempDir")
  if (is.null(runTempDir) || !dir.exists(runTempDir)) {
    runTempDir <- tempdir()
  }
  tempfile(tmpdir = runTempDir, fileext = fileext)
}

#' Remove the Run's Temporary Folder ----
#'
#' @description
#' Deletes terra's temporary files and the folder from `createRunTempDir()`.
#' Safe to call more than once. Nothing saved to a datasheet is written
#' there, so removing it never affects outputs.
#'
#' @param runTempDir Folder to remove; defaults to the current run's folder.
#'
#' @return TRUE if the folder no longer exists (invisibly).
#'
#' @noRd
removeRunTempDir <- function(
  runTempDir = getOption("ecoClassify.runTempDir")
) {
  try(terra::tmpFiles(remove = TRUE), silent = TRUE)
  if (is.null(runTempDir) || !dir.exists(runTempDir)) {
    return(invisible(TRUE))
  }
  # Release terra file handles so Windows allows the files to be deleted
  gc()
  unlink(runTempDir, recursive = TRUE, force = TRUE)
  invisible(!dir.exists(runTempDir))
}
