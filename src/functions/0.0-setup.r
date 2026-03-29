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
  if (mulitprocessingSheet$EnableMultiprocessing) {
    requestedCores <- mulitprocessingSheet$MaximumJobs
    if (requestedCores > availableCores) {
      updateRunLog(
        paste0(
          "Requested number of jobs exceeds available cores. Continuing run with ",
          availableCores,
          " jobs."
        ),
        type = "warning"
      )
      nCores <- availableCores - 1
    } else {
      nCores <- requestedCores
    }
  } else {
    nCores <- 1
  }
  return(nCores)
}
