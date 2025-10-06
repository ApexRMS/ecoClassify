# Guide.md

This file provides guidance on the development of this package.

## Project Overview

**ecoClassify** is a SyncroSim package for semantic image segmentation and classification. It's built as an R-based package that integrates with the SyncroSim modeling platform to provide machine learning capabilities for ecological image analysis.

## Architecture

### Core Components

- **SyncroSim Package Structure**: This is a SyncroSim package defined by `src/package.xml` which specifies datasheets, transformers, and UI layouts
- **Transformer Scripts**: Main execution scripts in `src/` that process datasheets and run ML workflows:
  - `1-train-classifier.R`: Training pipeline orchestrator
  - `2-predict.r`: Prediction pipeline
  - `3-postprocess.R`: Post-processing and output generation
- **Function Library**: Modular functions in `src/functions/` organized by workflow stage:
  - `0.0-setup.r`: Core setup and initialization
  - `0.1-pre-processing.r`: Data preprocessing utilities
  - `0.2-training-and-prediction.r`: Core ML training/prediction logic
  - `0.3-contextualization.r`: Spatial context handling
  - `0.4-*.r`: Model-specific implementations (CNN, MaxEnt, Random Forest)
  - `0.5-histogram.r`: Data analysis and visualization
  - `0.6-post-processing.r`: Output processing utilities

### Data Flow

1. **Input**: Training/predicting rasters and covariates via SyncroSim datasheets
2. **Processing**: Transformer scripts source functions and execute ML pipelines
3. **Output**: Classified rasters and metrics saved to SyncroSim transfer directory

## Development Commands

### Testing
- **Unit Tests**: `Rscript run_unit_tests.R` (convenience wrapper)
- **Direct Unit Tests**: `Rscript testing/unit/run_unit_tests.R`
- **Integration Tests**: `bash test-build.sh` (requires SyncroSim installation)

### Linting
- **R Linting**: Uses `.lintr` configuration file (run `lintr::lint_dir("src")` in R)

### Build & Package
- **Integration Build**: `bash test-build.sh` (reinstalls package and tests with SyncroSim)
- **Python Integration**: Uses Python scripts in `testing/integration/` for package management

## Key Dependencies

- **Core R**: terra (preferred over raster/rgdal), dplyr, parallel
- **ML Libraries**: randomForest, maxent, tensorflow/keras (for CNN)
- **SyncroSim**: rsyncrosim package for platform integration
- **Testing**: testthat framework

## Code Standards

Following `src/copilot-instructions.md`:

### R Style
- Use `camelCase` for variables and functions
- Use `<-` for assignment (except in function arguments)
- Use `terra` instead of `raster`/`rgdal`
- Use pipes `%>%` for readability
- 80 character line limit, 2-space indentation
- Prefer relative paths

### Python Style
- Use `camelCase` (project uses this instead of snake_case)
- Follow PEP 8 otherwise
- Use f-strings for formatting
- Import modules with aliases (e.g., `import pandas as pd`)

### Documentation
- Use roxygen2 for R functions
- Use docstrings for Python functions
- Minimize comments - prefer self-explanatory names
- Comments should explain "why", not "what"

## Project Structure Context

- `/src/`: Main package source code and transformers
- `/src/functions/`: Modular function library
- `/testing/unit/`: R-based unit tests using testthat
- `/testing/integration/`: Python-based integration tests with SyncroSim
- `/development-guide/`: Technical documentation about SyncroSim patterns

## SyncroSim Integration

This package integrates with SyncroSim through:
- **Datasheets**: Defined in `package.xml`, accessed via `rsyncrosim`
- **Scenarios**: Current scenario accessed via `scenario()` function
- **Transfer Directory**: Output location from `ssimEnvironment()$TransferDirectory`
- **Progress Reporting**: Use `progressBar()` for user feedback

## Testing Strategy

- **Unit Tests**: Test individual functions in isolation using synthetic data
- **Integration Tests**: Test full package installation and template library creation with SyncroSim
- Run unit tests frequently during development
- Run integration tests before major releases or package updates