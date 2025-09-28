# SyncroSim Transformer Development Guide

## Overview
Transformers are the computational engines of SyncroSim packages. They read input datasheets, process data, and write results to output datasheets. This guide covers transformer development in both R and Python.

## Transformer Types

### 1. Processing Transformers
Execute model logic and calculations
```xml
<transformer name="ProcessData"
            displayName="Process Data"
            programArguments="process.R">
    <dataSheet name="InputData" type="Input"/>
    <dataSheet name="OutputData" type="Output"/>
</transformer>
```

### 2. Export Transformers
Export specific columns to files
```xml
<transformer name="ExportResults" 
            dataSheet="Results" 
            column="ResultFile" 
            isFolderExport="True"/>
```

### 3. Multiprocessing Transformers
Handle computationally intensive operations
```xml
<transformer name="ParallelProcess"
            programArguments="parallel.py"
            isMultiprocessing="True">
    <dataSheet name="core_Multiprocessing" type="Input"/>
    <dataSheet name="OutputData" type="Output"/>
</transformer>
```

### 4. Spatial Multiprocessing Transformers
Process large rasters in tiles
```xml
<transformer name="SpatialAnalysis"
            programArguments="spatial.R">
    <dataSheet name="core_SpatialMultiprocessing" type="Input"/>
    <dataSheet name="SpatialOutput" type="Output" 
              spatialMultiProcessing="True"/>
</transformer>
```

## Python Transformer Development

### Basic Structure
```python
## --------------------------------
## Package - Transformer Name
## Author, Date
## --------------------------------

# Import required modules
import pysyncrosim as ps
import pandas as pd
import numpy as np
import os

# Initialize progress reporting
steps = 5
ps.environment.update_run_log('Transformer Name => Begin')
ps.environment.progress_bar(report_type="begin", total_steps=steps)

# Connect to SyncroSim
myScenario = ps.Scenario()
myLibrary = myScenario.library
myProject = myScenario.project

# Create temporary directory
temp_dir = ps.runtime_temp_folder(
    os.path.join("DataTransfer", f"Scenario-{myScenario.sid}")
)

# Load input datasheets
input_data = myScenario.datasheets("PackageName_InputDatasheet")
ps.environment.progress_bar()  # Update progress

# Process data
# ... your processing logic here ...
ps.environment.progress_bar()  # Update progress

# Save output datasheets
output_data = pd.DataFrame({
    'Column1': results1,
    'Column2': results2
})
myScenario.save_datasheet("PackageName_OutputDatasheet", output_data)

# Complete progress reporting
ps.environment.progress_bar(report_type="end")
ps.environment.update_run_log('Transformer Name => Complete')
```

### Working with Files
```python
# Load datasheet with full paths
file_data = myScenario.datasheets("DatasheetName", show_full_paths=True)

# Access file path
file_path = file_data.FilePath.item()

# Save file to temp directory
output_path = os.path.join(temp_dir, "output.tif")

# Reference in output datasheet
output_data = pd.DataFrame({
    'ResultFile': [output_path]
})
```

### Conda Environment Management
```python
# Get conda configuration
mySession = ps.Session()
result = mySession._Session__call_console(["--conda", "--config"])
conda_path = result.stdout.decode('utf-8').strip().split(
    "Conda path is currently: "
)[1]

# Set environment variables for GDAL/PROJ
if myLibrary.datasheets("core_Option").UseConda.item() == "Yes":
    env_path = os.path.join(conda_path, "envs", "env-name")
    gdal_path = os.path.join(env_path, "Library", "share", "gdal")
    proj_path = os.path.join(env_path, "Library", "share", "proj")
    
    os.environ['GDAL_DATA'] = gdal_path
    os.environ['PROJ_DATA'] = proj_path
```

### Multiprocessing with Dask
```python
import dask
from dask.distributed import Client

# Check multiprocessing settings
mp_sheet = myScenario.datasheets("core_Multiprocessing")
if mp_sheet.EnableMultiprocessing.item() == "Yes":
    num_threads = mp_sheet.MaximumJobs.item()
else:
    num_threads = 1

# Configure Dask
dask.config.set(**{
    'temporary-directory': os.path.join(temp_dir, 'dask-worker-space')
})

# Create client
with Client(threads_per_worker=num_threads, n_workers=1, processes=False) as client:
    # Parallel processing here
    pass
```

## R Transformer Development

### Basic Structure
```r
## --------------------------------
## Package - Transformer Name
## Author, Date
## --------------------------------

# Load libraries
library(rsyncrosim)
library(tidyverse)

# Get package directory for sourcing helper functions
packageDir <- Sys.getenv("ssim_package_directory")

# Source helper functions if they exist
if (file.exists(file.path(packageDir, "helper-functions.R"))) {
  source(file.path(packageDir, "helper-functions.R"))
}

# Initialize environment
myScenario <- scenario()
myLibrary <- ssimLibrary() 
myProject <- project()

# Get transfer directory for outputs
e <- ssimEnvironment()
transferDir <- e$TransferDirectory

# Progress reporting with message
updateRunLog('Transformer Name => Begin')
progressBar(type = "begin", totalSteps = 5)
progressBar(type = "message", message = "Loading input data...")

# Load input datasheets
inputData <- datasheet(myScenario, "PackageName_InputDatasheet", optional = TRUE)
progressBar()

# Process data
# ... your processing logic here ...
progressBar()

# Save output datasheets
outputData <- data.frame(
  Column1 = results1,
  Column2 = results2
)
saveDatasheet(myScenario, outputData, "PackageName_OutputDatasheet")

# Complete progress
progressBar(type = "end")
updateRunLog('Transformer Name => Complete')
```

### Working with Rasters
```r
library(terra)

# Load template raster from datasheet
templateSheet <- datasheet(myScenario, "TemplateRaster")
templatePath <- templateSheet$RasterFilePath
templateRaster <- rast(templatePath)

# Load raster with explicit show_full_paths equivalent
covariateSheet <- datasheet(myScenario, "CovariateData", optional = TRUE, 
                           lookupsAsFactors = FALSE)

# Process multiple rasters
for(i in 1:nrow(covariateSheet)) {
  ri <- rast(covariateSheet$RasterFilePath[i])
  
  # Resample to match template
  ri <- resample(ri, templateRaster, method = "bilinear")
  
  # Mask to template extent
  ri <- mask(ri, templateRaster)
  
  # Save processed raster
  outputPath <- file.path(transferDir, 
                         paste0("processed_", basename(covariateSheet$RasterFilePath[i])))
  writeRaster(ri, outputPath, overwrite = TRUE, 
              gdal = c('COMPRESS=LZW', 'TILED=YES'))
  
  progressBar()
}

# Save reference in datasheet
outputData <- data.frame(
  RasterOutput = outputPath
)
saveDatasheet(myScenario, outputData, "OutputRaster")
```

### Helper Functions Pattern
```r
# Source helper functions from package directory
packageDir <- Sys.getenv("ssim_package_directory")

# Source all R files in functions directory
sourceScripts <- list.files(
  path = file.path(packageDir, "functions"),
  pattern = "\\.[rR]$",
  full.names = TRUE
)
invisible(lapply(sourceScripts, source))

# Or source specific helper file
source(file.path(packageDir, "00-helper-functions.R"))

# Use modular functions
result <- processData(inputData, parameters)
metrics <- calculateMetrics(result)
plots <- generatePlots(metrics)
```

## Progress Reporting

### Python
```python
# Initialize progress bar
ps.environment.progress_bar(report_type="begin", total_steps=10)

# Update with message
ps.environment.progress_bar("message", 
    message="Processing step 1 of 10...")

# Increment progress
ps.environment.progress_bar()

# Complete
ps.environment.progress_bar(report_type="end")

# Log messages
ps.environment.update_run_log("Status message")
ps.environment.update_run_log("Warning message", type="warning")
```

### R
```r
# Initialize progress bar
progressBar(type = "begin", totalSteps = 10)

# Update with message
progressBar(message = "Processing step 1 of 10...")

# Increment progress
progressBar()

# Complete
progressBar(type = "end")

# Log messages
updateRunLog("Status message")
updateRunLog("Warning message", type = "warning")
```

## Error Handling

### Python
```python
try:
    # Risky operation
    result = process_data(input_data)
except ValueError as e:
    ps.environment.update_run_log(f"Error: {str(e)}", type="error")
    raise
except Exception as e:
    ps.environment.update_run_log(
        f"Unexpected error: {str(e)}", type="warning"
    )
    # Handle gracefully
```

### R
```r
# Using tryCatch
result <- tryCatch({
    processData(inputData)
}, error = function(e) {
    updateRunLog(paste("Error:", e$message), type = "error")
    stop(e)
}, warning = function(w) {
    updateRunLog(paste("Warning:", w$message), type = "warning")
    # Continue execution
})

# Input validation
if (is.null(inputData$RequiredColumn)) {
    stop("Required column is missing from input data")
}
```

## Spatial Multiprocessing

### Setup Spatial Tiles
```python
# Load spatial multiprocessing configuration
smp_sheet = myScenario.datasheets("core_SpatialMultiprocessing")
tile_raster_path = smp_sheet.MaskFileName.item()

# Process each tile
import rasterio
with rasterio.open(tile_raster_path) as src:
    tiles = np.unique(src.read(1))
    tiles = tiles[tiles != src.nodata]

for tile_id in tiles:
    ps.environment.progress_bar("message", 
        message=f"Processing tile {tile_id}")
    # Process tile
```

### R Implementation
```r
# Load spatial multiprocessing configuration
smpSheet <- datasheet(myScenario, "core_SpatialMultiprocessing")
tileRasterPath <- smpSheet$MaskFileName

# Process tiles
tileRaster <- rast(tileRasterPath)
tiles <- unique(values(tileRaster))
tiles <- tiles[!is.na(tiles)]

for (tileId in tiles) {
    progressBar(message = paste("Processing tile", tileId))
    # Process tile
}
```

## Data Validation

### Input Validation Checklist
```python
# Check required columns exist
required_cols = ['Column1', 'Column2']
missing_cols = [col for col in required_cols 
                 if col not in input_data.columns]
if missing_cols:
    raise ValueError(f"Missing required columns: {missing_cols}")

# Check data types
if not pd.api.types.is_numeric_dtype(input_data['NumericColumn']):
    raise TypeError("NumericColumn must contain numeric values")

# Check value ranges
if (input_data['Probability'] < 0).any() or (input_data['Probability'] > 1).any():
    raise ValueError("Probability values must be between 0 and 1")

# Check file existence
for file_path in input_data['FilePath']:
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"File not found: {file_path}")
```

## Performance Optimization

### Memory Management
```python
# Process in chunks for large datasets
chunk_size = 10000
for chunk_start in range(0, len(data), chunk_size):
    chunk_end = min(chunk_start + chunk_size, len(data))
    chunk = data.iloc[chunk_start:chunk_end]
    # Process chunk
    
# Clean up temporary files
import tempfile
import shutil

with tempfile.TemporaryDirectory() as tmpdir:
    # Use tmpdir for intermediate files
    pass  # Files automatically cleaned up
```

### Parallel Processing
```python
from concurrent.futures import ProcessPoolExecutor
import multiprocessing as mp

# Determine number of workers
n_workers = min(mp.cpu_count(), 
                mp_sheet.MaximumJobs.item())

# Parallel execution
with ProcessPoolExecutor(max_workers=n_workers) as executor:
    results = list(executor.map(process_function, data_chunks))
```

## Best Practices

### 1. Transformer Design
- Keep transformers focused on a single task
- Use clear, descriptive names
- Document expected inputs and outputs
- Implement proper error handling

### 2. Data Management
- Validate all inputs before processing
- Use appropriate data types
- Clean up temporary files
- Handle missing/null values explicitly

### 3. Progress Reporting
- Report progress at logical intervals
- Include time estimates for long operations
- Provide informative status messages
- Log warnings and errors appropriately

### 4. Performance
- Use vectorized operations when possible
- Implement chunking for large datasets
- Enable multiprocessing for intensive operations
- Profile and optimize bottlenecks

### 5. File Handling
- Use temporary directories for intermediate files
- Store outputs in appropriate formats
- Compress large output files
- Clean up temporary files on completion

## Debugging Tips

### Enable Verbose Logging
```python
# Python
import logging
logging.basicConfig(level=logging.DEBUG)

# Add debug messages
ps.environment.update_run_log(f"Debug: variable = {variable}")
```

### R Debugging
```r
# Enable debugging
options(error = browser)

# Add debug output
updateRunLog(paste("Debug: variable =", variable))

# Save intermediate results
saveRDS(intermediateData, 
        file.path(tempDir, "debug_intermediate.rds"))
```

### Common Issues

1. **File Path Issues**
   - Always use `show_full_paths=True` when loading file references
   - Use `os.path.join()` or `file.path()` for path construction
   - Handle both forward and backward slashes

2. **Memory Errors**
   - Process large datasets in chunks
   - Clear variables after use: `del variable` (Python) or `rm(variable)` (R)
   - Use appropriate data types (int16 vs int64)

3. **Multiprocessing Conflicts**
   - Use locks for shared resources
   - Avoid writing to same file from multiple processes
   - Clean up Dask clients properly

4. **Data Type Mismatches**
   - Explicitly convert data types
   - Handle numpy/pandas type conversions
   - Check for integer overflow

This guide provides comprehensive coverage of transformer development for SyncroSim packages.