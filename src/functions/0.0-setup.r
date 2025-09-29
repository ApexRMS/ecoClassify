## -------------------------------
## ecoClassify - Setup Functions
## ApexRMS, November 2024
## -------------------------------

# suppress additional warnings ----
load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = 'http://cran.us.r-project.org')
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

quiet <- function(expr) {
  suppressPackageStartupMessages(expr)
}

# load required packages ----
quiet({
  pkgs <- c(
    "terra",
    "tidyverse",
    "magrittr",
    "foreach",
    "iterators",
    "parallel",
    "coro",
    "rsyncrosim",
    "sf",
    "ranger",
    "gtools",
    "codetools",
    "ecospat",
    "cvms",
    "doParallel",
    "tidymodels"
  )

  invisible(lapply(pkgs, load_pkg))
})

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
