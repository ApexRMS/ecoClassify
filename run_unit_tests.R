#!/usr/bin/env Rscript
# Convenience wrapper for unit tests
# This allows running unit tests from the project root

# Change to the project directory if needed
if (!dir.exists("testing/unit")) {
  stop("Could not find testing/unit directory. Make sure to run this from the project root.")
}

# Source the actual test runner
source("testing/unit/run_unit_tests.R")