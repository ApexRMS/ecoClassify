#!/usr/bin/env Rscript
# ecoClassify Integration Test using rsyncrosim

library(rsyncrosim)
library(xml2)
library(optparse)
library(tidyverse)

# Configuration
SSIM_DIR <- "C:/Program Files/SyncroSim"
METADATA_PATH <- "testing/integration/metadata.xml"
SOURCE_FOLDER <- "./src"
TEMP_DIR <- "scratch"

#' Print timestamped messages
log_message <- function(message, type = "info") {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  symbols <- list(
    "info" = "[INFO]",
    "success" = "[OK]",
    "warning" = "[WARN]",
    "error" = "[ERR]"
  )
  cat(sprintf("%s %s %s\n", timestamp, symbols[[type]], message))
}

#' Parse metadata.xml to get package and library info
get_package_info <- function(metadata_path) {
  doc <- read_xml(metadata_path)
  package_name <- xml_attr(doc, "name")
  package_version <- xml_attr(doc, "version")

  lib_nodes <- xml_find_all(doc, ".//onlineLibrary")
  libraries <- list()

  for (node in lib_nodes) {
    libraries[[length(libraries) + 1]] <- list(
      name = xml_attr(node, "name"),
      displayName = xml_attr(node, "displayName"),
      url = xml_attr(node, "libraryLocation")
    )
  }

  log_message(sprintf(
    "Package: %s v%s with %d libraries",
    package_name,
    package_version,
    length(libraries)
  ))

  return(list(
    name = package_name,
    version = package_version,
    libraries = libraries
  ))
}

#' Install/reinstall package from source folder
install_package_from_source <- function(
  ssim_session,
  package_name,
  source_folder
) {
  log_message(sprintf("Installing package %s from source", package_name))

  # Check if package is already installed
  installed_packages <- packages()
  package_installed <- package_name %in% installed_packages$name

  if (package_installed) {
    log_message("Package already installed, removing first")
    tryCatch(
      {
        uninstallPackage(packages = package_name)
        log_message("Existing package removed", "success")
      },
      error = function(e) {
        log_message(
          sprintf("Failed to remove existing package: %s", e$message),
          "error"
        )
        return(FALSE)
      }
    )
  }

  # Install from source
  tryCatch(
    {
      installPackage(source_folder, session = ssim_session)
      log_message("Package installed successfully from source", "success")
      return(TRUE)
    },
    error = function(e) {
      log_message(
        sprintf("Package installation failed: %s", e$message),
        "error"
      )
      return(FALSE)
    }
  )
}

#' Download and extract template library
download_template_library <- function(library_info, temp_dir) {
  log_message(sprintf(
    "Downloading template library: %s",
    library_info$displayName
  ))

  # Create temp directory
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  # Download file
  filename <- basename(library_info$url)
  zip_path <- file.path(temp_dir, filename)

  tryCatch(
    {
      download.file(library_info$url, zip_path, mode = "wb", quiet = TRUE)
      log_message("Download completed", "success")
    },
    error = function(e) {
      log_message(sprintf("Download failed: %s", e$message), "error")
      return(NULL)
    }
  )

  # Extract
  extract_folder <- file.path(temp_dir, paste0(library_info$name, "_unzipped"))
  dir.create(extract_folder, showWarnings = FALSE, recursive = TRUE)

  tryCatch(
    {
      unzip(zip_path, exdir = extract_folder)
      log_message("Extraction completed", "success")
    },
    error = function(e) {
      log_message(sprintf("Extraction failed: %s", e$message), "error")
      return(NULL)
    }
  )

  # Find .ssim file
  ssim_files <- list.files(
    extract_folder,
    pattern = "\\.ssim$",
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(ssim_files) == 0) {
    log_message("No .ssim file found", "error")
    return(NULL)
  }

  lib_path <- ssim_files[1]
  log_message(
    sprintf("Template library extracted: %s", basename(lib_path)),
    "success"
  )
  return(lib_path)
}

#' Run integration test scenarios
run_integration_scenarios <- function(
  ssim_session,
  library_path,
  package_info
) {
  log_message("Setting up library for testing")

  tryCatch(
    {
      # Open the template library
      mylibrary <- ssimLibrary(library_path, session = ssim_session)
      log_message("Template library opened", "success")

      # Add package to library if needed
      removePackage(mylibrary, packages = package_info$name)
      addPackage(mylibrary, packages = package_info$name)

      # Get scenarios
      scenarios_df <- scenario(mylibrary)
      log_message(sprintf("Found %d scenarios in library", nrow(scenarios_df)))

      if (nrow(scenarios_df) > 0) {
        # Print scenario info
        cat("\nAvailable scenarios:\n")
        print(scenarios_df[c("ScenarioId", "Name", "IsResult")])
        cat("\n")

        # Get non-result scenarios (the ones we want to run)
        run_scenarios <- scenarios_df[scenarios_df$IsResult == "No", ]

        if (nrow(run_scenarios) > 0) {
          # Take first 3 scenarios or all if less than 3
          scenarios_to_run <- head(run_scenarios, 3)
          scenario_ids <- scenarios_to_run$ScenarioId
          scenario_names <- scenarios_to_run$Name

          log_message(sprintf(
            "Running %d scenarios: %s",
            length(scenario_ids),
            paste(scenario_names, collapse = " -> ")
          ))

          start_time <- Sys.time()

          # Run scenarios
          tryCatch(
            {
              scenario_objects <- lapply(scenario_ids, function(id) {
                scenario(mylibrary, scenario = id)
              })
              lapply(scenario_objects, function(i) {
                run(i)
              })

              end_time <- Sys.time()
              elapsed <- as.numeric(difftime(
                end_time,
                start_time,
                units = "secs"
              ))

              log_message(
                sprintf("Scenarios completed in %.2f seconds", elapsed),
                "success"
              )

              # Display results
              cat("\n", paste(rep("=", 80), collapse = ""), "\n")
              cat("INTEGRATION TEST RESULTS:\n")
              cat(paste(rep("=", 80), collapse = ""), "\n")
              cat(sprintf("Execution time: %.2f seconds\n", elapsed))
              cat("Success: YES\n")
              cat(
                "Scenarios run:",
                paste(scenario_names, collapse = ", "),
                "\n"
              )
              cat(paste(rep("=", 80), collapse = ""), "\n\n")

              return(TRUE)
            },
            error = function(e) {
              end_time <- Sys.time()
              elapsed <- as.numeric(difftime(
                end_time,
                start_time,
                units = "secs"
              ))

              log_message(
                sprintf(
                  "Scenario execution failed after %.2f seconds: %s",
                  elapsed,
                  e$message
                ),
                "error"
              )

              # Display error results
              cat("\n", paste(rep("=", 80), collapse = ""), "\n")
              cat("INTEGRATION TEST RESULTS:\n")
              cat(paste(rep("=", 80), collapse = ""), "\n")
              cat(sprintf("Execution time: %.2f seconds\n", elapsed))
              cat("Success: NO\n")
              cat("Error:", e$message, "\n")
              cat(paste(rep("=", 80), collapse = ""), "\n\n")

              return(FALSE)
            }
          )
        } else {
          log_message("No runnable scenarios found", "warning")
          return(FALSE)
        }
      } else {
        log_message("No scenarios found in library", "error")
        return(FALSE)
      }
    },
    error = function(e) {
      log_message(sprintf("Library setup failed: %s", e$message), "error")
      return(FALSE)
    }
  )
}

#' Main integration test function
run_integration_test <- function() {
  log_message("Starting ecoClassify integration test with rsyncrosim")

  # Initialize SyncroSim session
  tryCatch(
    {
      ssim_session <- session(SSIM_DIR)
      log_message("SyncroSim session initialized", "success")
    },
    error = function(e) {
      log_message(
        sprintf("Failed to initialize SyncroSim session: %s", e$message),
        "error"
      )
      return(FALSE)
    }
  )

  # Parse metadata
  package_info <- get_package_info(METADATA_PATH)

  # Install package from source
  if (
    !install_package_from_source(ssim_session, package_info$name, SOURCE_FOLDER)
  ) {
    return(FALSE)
  }

  # Process each template library
  overall_success <- TRUE

  for (library_info in package_info$libraries) {
    log_message(sprintf(
      "Processing template library: %s",
      library_info$displayName
    ))

    # Download and extract template library
    lib_path <- download_template_library(library_info, TEMP_DIR)
    if (is.null(lib_path)) {
      overall_success <- FALSE
      next
    }

    # Run integration scenarios
    if (!run_integration_scenarios(ssim_session, lib_path, package_info)) {
      overall_success <- FALSE
    }
  }

  return(overall_success)
}

#' Command line interface
main <- function() {
  option_list <- list(
    make_option(
      c("--meta"),
      type = "character",
      default = METADATA_PATH,
      help = "Path to meta-data.xml [default: %default]",
      metavar = "character"
    ),
    make_option(
      c("--ssimdir"),
      type = "character",
      default = SSIM_DIR,
      help = "Path to SyncroSim installation [default: %default]",
      metavar = "character"
    ),
    make_option(
      c("--source"),
      type = "character",
      default = SOURCE_FOLDER,
      help = "Source folder path [default: %default]",
      metavar = "character"
    ),
    make_option(
      c("--tempdir"),
      type = "character",
      default = TEMP_DIR,
      help = "Temporary directory [default: %default]",
      metavar = "character"
    ),
    make_option(
      c("--test"),
      action = "store_true",
      default = FALSE,
      help = "Run integration test"
    )
  )

  opt_parser <- OptionParser(
    option_list = option_list,
    description = "ecoClassify Integration Test using rsyncrosim"
  )
  opt <- parse_args(opt_parser)

  # Update global variables
  SSIM_DIR <<- opt$ssimdir
  METADATA_PATH <<- opt$meta
  SOURCE_FOLDER <<- opt$source
  TEMP_DIR <<- opt$tempdir

  # Run test
  success <- run_integration_test()

  if (success) {
    log_message("INTEGRATION TEST COMPLETED SUCCESSFULLY", "success")
  } else {
    log_message("INTEGRATION TEST FAILED", "error")
  }

  quit(status = if (success) 0 else 1)
}

# Run based on how script is called
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 0) {
    main()
  } else {
    # Run default integration test
    success <- run_integration_test()
    quit(status = if (success) 0 else 1)
  }
}
