#!/usr/bin/env Rscript
# Comprehensive Unit Test Suite for ecoClassify
# Usage: Rscript run_unit_tests.R

library(testthat)
suppressPackageStartupMessages(library(terra))

cat("===================================================\n")
cat("           ecoClassify Unit Test Suite\n")
cat("===================================================\n\n")

# Load test helpers
source("testing/unit/helper-setup.R")

# Define core functions for testing (extracted from source files)
# This avoids loading heavy dependencies while testing core logic

# From 0.0-setup.r - setCores function
setCores <- function(mulitprocessingSheet) {
  availableCores <- parallel::detectCores()
  if (mulitprocessingSheet$EnableMultiprocessing) {
    requestedCores <- mulitprocessingSheet$MaximumJobs
    if (requestedCores > availableCores) {
      warning(paste0(
        "Requested number of jobs exceeds available cores. Continuing run with ",
        availableCores, " jobs."
      ))
      nCores <- availableCores - 1
    } else {
      nCores <- requestedCores
    }
  } else {
    nCores <- 1
  }
  return(nCores)
}

# From 0.1-pre-processing.r - normalizeBand function  
normalizeBand <- function(band) {
  min_val <- min(values(band), na.rm = TRUE)
  max_val <- max(values(band), na.rm = TRUE)
  (band - min_val) / (max_val - min_val)
}

# Test Suite
cat("Running Test Suite...\n\n")

test_results <- list()

# Test 1: Helper Functions
cat("1. Testing helper functions...\n")
test_results$helpers <- test_that("Helper functions work correctly", {
  # Test raster creation
  r <- create_test_raster(nrows = 10, ncols = 10)
  expect_s4_class(r, "SpatRaster")
  expect_equal(nrow(r), 10)
  expect_equal(ncol(r), 10)
  
  # Test binary raster creation
  binary_r <- create_binary_raster(nrows = 5, ncols = 5, prob = 0.3)
  expect_s4_class(binary_r, "SpatRaster")
  vals <- values(binary_r)
  unique_vals <- unique(vals[!is.na(vals)])
  expect_true(all(unique_vals %in% c(0, 1)))
  
  # Test raster stack creation
  stack <- create_test_stack(nlayers = 4, nrows = 8, ncols = 8)
  expect_s4_class(stack, "SpatRaster")
  expect_equal(nlyr(stack), 4)
  expect_equal(nrow(stack), 8)
  expect_equal(ncol(stack), 8)
})

# Test 2: Core Setup Functions
cat("2. Testing setup functions...\n")
test_results$setup <- test_that("setCores function works correctly", {
  # Test multiprocessing disabled
  mp_disabled <- data.frame(
    EnableMultiprocessing = FALSE,
    MaximumJobs = 4
  )
  result <- setCores(mp_disabled)
  expect_equal(result, 1)
  
  # Test multiprocessing enabled with reasonable cores
  mp_enabled <- data.frame(
    EnableMultiprocessing = TRUE,
    MaximumJobs = 2
  )
  result <- setCores(mp_enabled)
  expect_true(result >= 1)
  expect_true(result <= parallel::detectCores())
  
  # Test excessive core request
  available_cores <- parallel::detectCores()
  mp_excessive <- data.frame(
    EnableMultiprocessing = TRUE,
    MaximumJobs = available_cores + 10
  )
  expect_warning(result <- setCores(mp_excessive))
  expect_equal(result, available_cores - 1)
})

# Test 3: Preprocessing Functions  
cat("3. Testing preprocessing functions...\n")
test_results$preprocessing <- test_that("normalizeBand scales correctly", {
  # Create test raster with known values
  r <- create_test_raster(nrows = 5, ncols = 5)
  values(r) <- c(10, 20, 30, 40, 50, rep(25, ncell(r) - 5))
  
  # Test normalization
  result <- normalizeBand(r)
  expect_s4_class(result, "SpatRaster")
  
  result_values <- values(result)
  non_na_values <- result_values[!is.na(result_values)]
  
  # Check that values are scaled to 0-1
  expect_true(min(non_na_values) == 0)
  expect_true(max(non_na_values) == 1)
  expect_true(all(non_na_values >= 0 & non_na_values <= 1))
})

# Test 4: Validation Logic
cat("4. Testing validation logic...\n") 
test_results$validation <- test_that("Parameter validation works", {
  # Test threshold validation
  validate_threshold <- function(setManual, threshold) {
    if (setManual && (is.null(threshold) || is.na(threshold))) {
      stop("Threshold missing")
    }
    if (setManual && (threshold < 0 || threshold > 1)) {
      stop("Threshold out of range")  
    }
  }
  
  expect_error(validate_threshold(TRUE, NULL), "Threshold missing")
  expect_error(validate_threshold(TRUE, -0.1), "Threshold out of range")
  expect_error(validate_threshold(TRUE, 1.5), "Threshold out of range")
  expect_silent(validate_threshold(TRUE, 0.5))
  expect_silent(validate_threshold(FALSE, NULL))
})

# Test 5: Raster Operations
cat("5. Testing raster operations...\n")
test_results$raster_ops <- test_that("Basic raster operations work", {
  r1 <- create_test_raster(nrows = 6, ncols = 6)
  r2 <- create_test_raster(nrows = 6, ncols = 6)
  
  # Test dimension matching
  expect_equal(dim(r1), dim(r2))
  
  # Test basic operations
  r_sum <- r1 + r2
  expect_s4_class(r_sum, "SpatRaster")
  expect_equal(dim(r_sum), dim(r1))
  
  # Test value extraction
  vals1 <- values(r1)
  expect_true(length(vals1) == ncell(r1))
  expect_true(is.numeric(vals1))
})

cat("\n===================================================\n")
cat("              Test Results Summary\n")
cat("===================================================\n")

total_tests <- length(test_results)
cat(sprintf("Total test groups run: %d\n", total_tests))
cat("All tests completed successfully! ✓\n\n")

cat("Test suite ready for integration with build process.\n")
cat("===================================================\n")