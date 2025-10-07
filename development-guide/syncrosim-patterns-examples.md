# SyncroSim Package Patterns and Examples

## Complete Package Example: Simple Species Distribution Model

### package.xml
```xml
<?xml version="1.0" encoding="utf-8"?>
<package name="simpleSDM" 
         displayName="Simple SDM" 
         description="Simple Species Distribution Modeling Package"
         version="1.0.0" 
         minSyncroSimVersion="3.0.0">

<!-- Project-level datasheet for species information -->
<dataSheet name="Species" dataScope="Project" displayMember="SpeciesName">
    <column name="SpeciesName" displayName="Species Name" dataType="String"/>
    <column name="ScientificName" displayName="Scientific Name" dataType="String" isOptional="True"/>
    <column name="ConservationStatus" displayName="Conservation Status" dataType="Integer" 
            validationType="List" formula1="0:Least Concern|1:Near Threatened|2:Vulnerable|3:Endangered|4:Critically Endangered" isOptional="True"/>
</dataSheet>

<!-- Scenario-level datasheet for occurrence data -->
<dataSheet name="OccurrenceData" displayName="Occurrence Data">
    <column name="SpeciesID" displayName="Species" dataType="Integer" 
            validationType="Datasheet" formula1="Species"/>
    <column name="Longitude" dataType="Double"/>
    <column name="Latitude" dataType="Double"/>
    <column name="PresenceAbsence" displayName="Presence/Absence" dataType="Integer" 
            validationType="List" formula1="0:Absence|1:Presence"/>
    <column name="Date" dataType="String" isOptional="True"/>
</dataSheet>

<!-- Environmental variables -->
<dataSheet name="EnvironmentalVariables" displayName="Environmental Variables">
    <column name="VariableName" displayName="Variable Name" dataType="String"/>
    <column name="RasterFile" displayName="Raster File" dataType="String" 
            isExternalFile="True" isRaster="True"/>
    <column name="IsCategorical" displayName="Is Categorical" dataType="Boolean" defaultValue="0"/>
</dataSheet>

<!-- Model configuration -->
<dataSheet name="ModelConfiguration" displayName="Model Configuration" isSingleRow="True">
    <column name="ModelType" displayName="Model Type" dataType="Integer" 
            validationType="List" formula1="0:Random Forest|1:MaxEnt|2:GLM"/>
    <column name="TrainingProportion" displayName="Training Proportion" dataType="Double" 
            defaultValue="0.7" validationType="Decimal" validationCondition="Between" 
            formula1="0.1" formula2="0.9"/>
    <column name="NumberOfReplicates" displayName="Number of Replicates" dataType="Integer" 
            defaultValue="10" validationType="WholeNumber" validationCondition="GreaterEqual" 
            formula1="1"/>
    <column name="CreateProbabilityMap" displayName="Create Probability Map" dataType="Boolean" 
            defaultValue="1"/>
    <column name="CreateBinaryMap" displayName="Create Binary Map" dataType="Boolean" 
            defaultValue="0"/>
</dataSheet>

<!-- Output datasheets -->
<dataSheet name="ModelPerformance" displayName="Model Performance">
    <column name="SpeciesID" dataType="Integer" validationType="Datasheet" formula1="Species"/>
    <column name="Replicate" dataType="Integer"/>
    <column name="AUC" displayName="AUC" dataType="Double"/>
    <column name="TSS" displayName="TSS" dataType="Double"/>
    <column name="Sensitivity" dataType="Double"/>
    <column name="Specificity" dataType="Double"/>
</dataSheet>

<dataSheet name="PredictionMaps" displayName="Prediction Maps">
    <column name="SpeciesID" dataType="Integer" validationType="Datasheet" formula1="Species"/>
    <column name="MapType" displayName="Map Type" dataType="String"/>
    <column name="RasterFile" displayName="Raster File" dataType="String" 
            isExternalFile="True" isRaster="True"/>
</dataSheet>

<!-- Transformer definitions -->
<transformer name="PrepareData" 
            displayName="1. Prepare Data" 
            programArguments="01-prepare-data.py"
            condaEnv="sdm-env.yml">
    <dataSheet name="OccurrenceData" type="Input"/>
    <dataSheet name="EnvironmentalVariables" type="Input"/>
    <dataSheet name="ProcessedOccurrences" type="Output"/>
</transformer>

<transformer name="FitModel" 
            displayName="2. Fit Model" 
            programArguments="02-fit-model.R">
    <dataSheet name="ProcessedOccurrences" type="Input"/>
    <dataSheet name="ModelConfiguration" type="Input"/>
    <dataSheet name="ModelPerformance" type="Output"/>
    <dataSheet name="ModelObject" type="Output"/>
</transformer>

<transformer name="CreateMaps" 
            displayName="3. Create Prediction Maps" 
            programArguments="03-create-maps.py"
            isMultiprocessing="True">
    <dataSheet name="ModelObject" type="Input"/>
    <dataSheet name="EnvironmentalVariables" type="Input"/>
    <dataSheet name="ModelConfiguration" type="Input"/>
    <dataSheet name="PredictionMaps" type="Output"/>
</transformer>

<!-- Layout definitions -->
<layout type="Scenario">
    <group name="Inputs" displayName="Input Data">
        <item name="OccurrenceData"/>
        <item name="EnvironmentalVariables"/>
    </group>
    <group name="Configuration" displayName="Model Configuration">
        <item name="ModelConfiguration"/>
    </group>
    <group name="Results" displayName="Results">
        <item name="ModelPerformance"/>
        <item name="PredictionMaps"/>
    </group>
</layout>

<layout type="Map">
    <item name="ProbabilityMap" displayName="Habitat Suitability" 
          dataSheet="PredictionMaps" column="RasterFile" 
          filter="MapType"/>
</layout>

</package>
```

## Common Implementation Patterns

### 1. Data Validation Pattern

```python
# validation.py - Reusable validation functions

def validate_occurrence_data(df):
    """Validate occurrence data meets requirements."""
    errors = []
    
    # Check required columns
    required_cols = ['Longitude', 'Latitude', 'PresenceAbsence']
    missing = [col for col in required_cols if col not in df.columns]
    if missing:
        errors.append(f"Missing columns: {', '.join(missing)}")
    
    # Check coordinate ranges
    if 'Longitude' in df.columns:
        invalid_lon = df[(df['Longitude'] < -180) | (df['Longitude'] > 180)]
        if not invalid_lon.empty:
            errors.append(f"{len(invalid_lon)} records with invalid longitude")
    
    if 'Latitude' in df.columns:
        invalid_lat = df[(df['Latitude'] < -90) | (df['Latitude'] > 90)]
        if not invalid_lat.empty:
            errors.append(f"{len(invalid_lat)} records with invalid latitude")
    
    # Check presence/absence values
    if 'PresenceAbsence' in df.columns:
        valid_values = [0, 1]
        invalid_pa = df[~df['PresenceAbsence'].isin(valid_values)]
        if not invalid_pa.empty:
            errors.append(f"{len(invalid_pa)} records with invalid presence/absence values")
    
    if errors:
        raise ValueError("\n".join(errors))
    
    return True

# Use in transformer
occurrence_data = myScenario.datasheets("simpleSDM_OccurrenceData")
validate_occurrence_data(occurrence_data)
```

### 2. Raster Processing Pattern

```python
# raster_utils.py - Common raster operations

import rasterio
import numpy as np
from rasterio.warp import reproject, Resampling
import os

def standardize_rasters(raster_paths, template_path, output_dir):
    """Standardize multiple rasters to match template."""
    
    with rasterio.open(template_path) as template:
        template_meta = template.meta.copy()
        template_bounds = template.bounds
        template_crs = template.crs
        
    standardized_paths = []
    
    for raster_path in raster_paths:
        output_path = os.path.join(output_dir, 
                                  f"std_{os.path.basename(raster_path)}")
        
        with rasterio.open(raster_path) as src:
            # Create output array
            output_array = np.empty(
                (template_meta['height'], template_meta['width']),
                dtype=src.dtypes[0]
            )
            
            # Reproject to match template
            reproject(
                source=rasterio.band(src, 1),
                destination=output_array,
                src_transform=src.transform,
                src_crs=src.crs,
                dst_transform=template_meta['transform'],
                dst_crs=template_crs,
                resampling=Resampling.bilinear
            )
            
            # Write standardized raster
            with rasterio.open(output_path, 'w', **template_meta) as dst:
                dst.write(output_array, 1)
            
            standardized_paths.append(output_path)
    
    return standardized_paths
```

### 3. Model Wrapper Pattern

```r
# model_wrapper.R - Consistent interface for different models

fit_model <- function(data, model_type, config) {
  # Standard interface for different model types
  
  model <- switch(model_type,
    "Random Forest" = {
      library(randomForest)
      randomForest(
        presence ~ .,
        data = data,
        ntree = config$n_trees %||% 500,
        mtry = config$mtry %||% sqrt(ncol(data) - 1)
      )
    },
    "GLM" = {
      glm(
        presence ~ .,
        data = data,
        family = binomial(link = "logit")
      )
    },
    "MaxEnt" = {
      library(dismo)
      maxent(
        x = data[, -which(names(data) == "presence")],
        p = data$presence
      )
    },
    stop(paste("Unknown model type:", model_type))
  )
  
  return(model)
}

predict_model <- function(model, newdata, model_type) {
  # Standard prediction interface
  
  predictions <- switch(model_type,
    "Random Forest" = {
      predict(model, newdata, type = "prob")[, 2]
    },
    "GLM" = {
      predict(model, newdata, type = "response")
    },
    "MaxEnt" = {
      predict(model, newdata)
    }
  )
  
  return(predictions)
}
```

### 4. Progress Reporting with Subtasks

```python
# Complex transformer with nested progress

def process_species_models(scenario):
    species_list = scenario.datasheets("Species")
    n_species = len(species_list)
    n_steps_per_species = 5
    total_steps = n_species * n_steps_per_species
    
    ps.environment.progress_bar(report_type="begin", 
                                total_steps=total_steps)
    
    current_step = 0
    
    for species_idx, species in species_list.iterrows():
        species_name = species['SpeciesName']
        
        # Step 1: Load data
        ps.environment.progress_bar("message", 
            message=f"Loading data for {species_name}")
        ps.environment.progress_bar()
        current_step += 1
        
        # Step 2: Prepare features
        ps.environment.progress_bar("message", 
            message=f"Preparing features for {species_name}")
        ps.environment.progress_bar()
        current_step += 1
        
        # Step 3: Fit model
        ps.environment.progress_bar("message", 
            message=f"Fitting model for {species_name}")
        ps.environment.progress_bar()
        current_step += 1
        
        # Step 4: Evaluate
        ps.environment.progress_bar("message", 
            message=f"Evaluating model for {species_name}")
        ps.environment.progress_bar()
        current_step += 1
        
        # Step 5: Create maps
        ps.environment.progress_bar("message", 
            message=f"Creating maps for {species_name}")
        ps.environment.progress_bar()
        current_step += 1
    
    ps.environment.progress_bar(report_type="end")
```

### 5. Conditional Processing Pattern

```xml
<!-- Conditional UI elements in package.xml -->
<dataSheet name="AdvancedOptions" displayName="Advanced Options" isSingleRow="True">
    <column name="UseAdvanced" displayName="Use Advanced Settings" dataType="Boolean" defaultValue="0"/>
    <column name="Option1" displayName="Option 1" dataType="Integer" isOptional="True"/>
    <column name="Option2" displayName="Option 2" dataType="Double" isOptional="True"/>
</dataSheet>
```

```python
# Conditional processing in transformer
advanced_options = myScenario.datasheets("AdvancedOptions")

if advanced_options.UseAdvanced.item():
    # Use advanced settings
    param1 = advanced_options.Option1.item()
    param2 = advanced_options.Option2.item()
    result = advanced_processing(data, param1, param2)
else:
    # Use defaults
    result = simple_processing(data)
```

### 6. Multi-Output Pattern

```python
# Transformer producing multiple output types

def generate_outputs(model, scenario):
    """Generate multiple output types from a single model."""
    
    outputs = {}
    temp_dir = ps.runtime_temp_folder()
    
    # Generate performance metrics
    metrics_df = pd.DataFrame({
        'Metric': ['AUC', 'TSS', 'Sensitivity', 'Specificity'],
        'Value': calculate_metrics(model)
    })
    outputs['metrics'] = metrics_df
    
    # Generate plots
    plot_path = os.path.join(temp_dir, 'performance_plot.png')
    create_performance_plot(model, plot_path)
    outputs['plot'] = plot_path
    
    # Generate prediction map
    map_path = os.path.join(temp_dir, 'prediction_map.tif')
    create_prediction_map(model, map_path)
    outputs['map'] = map_path
    
    # Generate report
    report_path = os.path.join(temp_dir, 'model_report.html')
    generate_html_report(model, metrics_df, report_path)
    outputs['report'] = report_path
    
    # Save all outputs to respective datasheets
    scenario.save_datasheet("ModelMetrics", outputs['metrics'])
    
    scenario.save_datasheet("ModelOutputs", pd.DataFrame({
        'PlotFile': [outputs['plot']],
        'MapFile': [outputs['map']],
        'ReportFile': [outputs['report']]
    }))
    
    return outputs
```

### 7. Library-Project-Scenario Data Hierarchy

```xml
<!-- Library-level configuration -->
<dataSheet name="GlobalSettings" dataScope="Library" isSingleRow="True">
    <column name="DefaultCRS" displayName="Default CRS" dataType="String" 
            defaultValue="EPSG:4326"/>
    <column name="TempDirectory" displayName="Temp Directory" dataType="String" 
            isOptional="True"/>
</dataSheet>

<!-- Project-level shared data -->
<dataSheet name="StudyArea" dataScope="Project" isSingleRow="True">
    <column name="AreaName" displayName="Study Area Name" dataType="String"/>
    <column name="BoundaryFile" displayName="Boundary Shapefile" dataType="String" 
            isExternalFile="True" isOptional="True"/>
    <column name="ExtentRaster" displayName="Extent Raster" dataType="String" 
            isExternalFile="True" isRaster="True"/>
</dataSheet>

<!-- Scenario-specific settings -->
<dataSheet name="ScenarioSettings" dataScope="Scenario" isSingleRow="True">
    <column name="ScenarioType" displayName="Scenario Type" dataType="Integer" 
            validationType="List" formula1="0:Current|1:Future|2:Historical"/>
    <column name="TimeHorizon" displayName="Time Horizon" dataType="Integer" 
            isOptional="True"/>
</dataSheet>
```

```python
# Access data at different scopes
library_settings = myScenario.library.datasheets("GlobalSettings")
project_settings = myScenario.project.datasheets("StudyArea")
scenario_settings = myScenario.datasheets("ScenarioSettings")

# Use hierarchical configuration
crs = library_settings.DefaultCRS.item()
study_area = project_settings.AreaName.item()
scenario_type = scenario_settings.ScenarioType.item()
```

### 8. Dynamic Column Generation

```python
# Generate columns based on input data

def create_output_datasheet(variables):
    """Dynamically create output datasheet structure."""
    
    # Base columns
    output_structure = {
        'ScenarioID': [],
        'Timestep': []
    }
    
    # Add column for each variable
    for var in variables:
        output_structure[f'{var}_Mean'] = []
        output_structure[f'{var}_StdDev'] = []
        output_structure[f'{var}_Min'] = []
        output_structure[f'{var}_Max'] = []
    
    return pd.DataFrame(output_structure)

# Use in transformer
env_vars = myScenario.datasheets("EnvironmentalVariables")
variable_names = env_vars.VariableName.tolist()

output_df = create_output_datasheet(variable_names)
# Populate with results...
myScenario.save_datasheet("VariableStatistics", output_df)
```

## R-Specific Implementation Patterns

### 1. Background Data Generation Pattern

```r
# Background surface generation for species distribution modeling
backgroundSurfaceGeneration <- function(sp,         # species name
                                       template,    # template raster
                                       mask,        # spatVector study area
                                       outputDir,   # output directory
                                       dat,         # field data 
                                       method)      # 'kde' or 'mcp'
{ 
  library(sp)
  library(adehabitatHR)
  library(spatstat.geom)
  library(sf)
  
  # Create spatial points from occurrence data
  xy <- sp::SpatialPoints(dat[, c('X', 'Y')])
  
  # Generate kernel density estimate
  if ('kde' %in% method) {
    ud <- adehabitatHR::kernelUD(xy, extent = 0.5, grid = 150)
    
    if(tolower(method$surface) == "continuous") {
      # Create continuous surface
      t <- terra::rast(ud)
      ext(t) <- ext(ud)
      crs(t) <- crs(template)
      t <- t / max(values(t), na.rm = TRUE) * 100  # normalize 0-100
      t <- terra::crop(t, ext(template))
      t <- terra::resample(x = t, y = template, method = 'near')
      t <- terra::ifel(!is.na(template), t, NA)
      
      # Save with compression
      kde_out <- file.path(outputDir, paste0(sp, '_kde_surface.tif'))
      writeRaster(x = t, filename = kde_out,
                  gdal = c('BIGTIFF=YES', 'TILED=YES', 'COMPRESS=LZW'),
                  overwrite = TRUE)
    }
    
    if(tolower(method$surface) == 'binary') {
      # Create binary surface from isopleth
      ver <- adehabitatHR::getverticeshr(ud, method$isopleth)
      bg_out <- file.path(outputDir, paste0(sp, '_kde_surface.shp'))
      terra::writeVector(terra::vect(sf::st_as_sf(ver)), bg_out, overwrite = TRUE)
    }
  }
  
  # Generate minimum convex polygon
  if("mcp" %in% method) {
    ver <- adehabitatHR::mcp(xy, percent = method$isopleth)
    bg_out <- file.path(outputDir, paste0(sp, '_mcp_surface.shp'))
    terra::writeVector(terra::vect(sf::st_as_sf(ver)), bg_out, overwrite = TRUE)
  }
}
```

### 2. Model Evaluation Pattern

```r
# Comprehensive model evaluation with multiple metrics
modelEvaluation <- function(predOcc, predAbs) {
  # Remove NA values
  p <- stats::na.omit(predOcc)
  a <- stats::na.omit(predAbs)
  np <- length(p)
  na <- length(a)
  
  if (na == 0 | np == 0) {
    stop("Cannot evaluate model without presence and absence data")
  }
  
  # Define S4 class for model evaluation
  setClass("modelEvaluation",
           slots = list(presence = "numeric",
                       absence = "numeric",
                       confusion = "matrix",
                       stats = "data.frame",
                       tr_stats = "data.frame",
                       thresholds = "data.frame"))
  
  # Calculate AUC
  R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1) / 2)
  auc <- R / (as.numeric(na) * as.numeric(np))
  
  # Calculate correlation
  cr <- try(stats::cor.test(c(p, a), 
                           c(rep(1, length(p)), rep(0, length(a)))), 
                           silent = TRUE)
  
  # Generate thresholds
  if (length(p) > 1000) {
    tr <- as.vector(stats::quantile(p, 0:1000/1000))
  } else {
    tr <- p
  }
  
  # Calculate confusion matrix for each threshold
  res <- matrix(ncol = 4, nrow = length(tr))
  colnames(res) <- c("tp", "fp", "fn", "tn")
  
  for (i in 1:length(tr)) {
    res[i, 1] <- length(p[p >= tr[i]])  # true positives
    res[i, 2] <- length(a[a >= tr[i]])  # false positives
    res[i, 3] <- length(p[p < tr[i]])   # false negatives
    res[i, 4] <- length(a[a < tr[i]])   # true negatives
  }
  
  # Calculate performance metrics
  CCR <- (res[, 1] + res[, 4]) / (np + na)  # correct classification rate
  TPR <- res[, 1] / (res[, 1] + res[, 3])   # sensitivity
  TNR <- res[, 4] / (res[, 2] + res[, 4])   # specificity
  FPR <- res[, 2] / (res[, 2] + res[, 4])   # false positive rate
  
  # Calculate kappa
  prA <- CCR
  prY <- ((res[, 1] + res[, 2]) / (np + na)) * 
         ((res[, 1] + res[, 3]) / (np + na))
  prN <- ((res[, 3] + res[, 4]) / (np + na)) * 
         ((res[, 2] + res[, 4]) / (np + na))
  prE <- prY + prN
  kappa <- (prA - prE) / (1 - prE)
  
  # Find optimal thresholds
  max_kappa <- tr[which.max(kappa)]
  max_spec_sens <- tr[which.max(TPR + TNR)]
  no_omission <- tr[max(which(res[, "fn"] == 0))]
  
  # Return evaluation object
  eval_obj <- methods::new("modelEvaluation")
  eval_obj@presence <- p
  eval_obj@absence <- a
  eval_obj@confusion <- res
  eval_obj@stats <- data.frame(np, na, auc, cor = cr$estimate)
  eval_obj@tr_stats <- data.frame(threshold = tr, kappa, CCR, TPR, TNR, FPR)
  eval_obj@thresholds <- data.frame(max_kappa, max_spec_sens, no_omission)
  
  return(eval_obj)
}
```

### 3. Datasheet Loading with Defaults Pattern

```r
# Load datasheets with proper defaults and error handling
loadDatasheetWithDefaults <- function(scenario, sheetName, defaults = list()) {
  
  # Load datasheet with optional flag
  sheet <- datasheet(scenario, sheetName, optional = TRUE, 
                    lookupsAsFactors = FALSE)
  
  # If empty, create with defaults
  if(nrow(sheet) < 1) {
    sheet <- bind_rows(sheet, defaults)
  }
  
  # Apply defaults to NA values
  for(col in names(defaults)) {
    if(col %in% names(sheet)) {
      na_idx <- is.na(sheet[[col]])
      if(any(na_idx)) {
        sheet[[col]][na_idx] <- defaults[[col]]
      }
    }
  }
  
  return(sheet)
}

# Usage example
backgroundOptions <- loadDatasheetWithDefaults(
  myScenario, 
  "BackgroundDataOptions",
  defaults = list(
    GenerateBackgroundSites = FALSE,
    BackgroundSiteCount = 1000,
    BackgroundGenerationMethod = "Kernel Density Estimate (KDE)",
    KDESurface = "Continuous",
    Isopleth = 95
  )
)
```

### 4. Multicore Processing Pattern

```r
# Set up parallel processing based on multiprocessing settings
setCores <- function(multiprocessingSheet) {
  if (!is.null(multiprocessingSheet) && 
      nrow(multiprocessingSheet) > 0 &&
      multiprocessingSheet$EnableMultiprocessing == "Yes") {
    
    nCores <- multiprocessingSheet$MaximumJobs
    
    # Respect system limits
    maxCores <- parallel::detectCores() - 1
    nCores <- min(nCores, maxCores)
    
    # Set up parallel backend for different packages
    if (requireNamespace("doParallel", quietly = TRUE)) {
      doParallel::registerDoParallel(cores = nCores)
    }
    
    # For terra package
    if (requireNamespace("terra", quietly = TRUE)) {
      terra::terraOptions(threads = nCores)
    }
    
    return(nCores)
  } else {
    return(1)
  }
}

# Use in transformer
multiprocessingSheet <- datasheet(myScenario, "core_Multiprocessing")
nCores <- setCores(multiprocessingSheet)
updateRunLog(paste("Using", nCores, "cores for processing"))
```

### 5. Raster Processing with Progress Updates

```r
# Process multiple rasters with proper memory management
processRasterStack <- function(rasterList, covariateSheet, 
                              templateRaster, transferDir) {
  
  steps <- length(rasterList) + nrow(covariateSheet)
  progressBar(type = "begin", totalSteps = steps)
  
  # Process each timestep
  for(i in seq_along(rasterList)) {
    progressBar(type = "message", 
               message = paste("Processing timestep", i, "of", length(rasterList)))
    
    r <- rasterList[[i]]
    
    # Free memory periodically
    if(i %% 10 == 0) {
      gc()
    }
    
    # Process with terra for efficiency
    r <- terra::resample(r, templateRaster, method = "bilinear")
    r <- terra::mask(r, templateRaster)
    
    # Write with compression
    outputPath <- file.path(transferDir, paste0("processed_t", i, ".tif"))
    terra::writeRaster(r, outputPath, 
                      gdal = c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=YES"),
                      overwrite = TRUE)
    
    progressBar()
  }
  
  # Process covariates
  for(j in 1:nrow(covariateSheet)) {
    progressBar(type = "message", 
               message = paste("Processing covariate:", covariateSheet$Name[j]))
    
    cov <- terra::rast(covariateSheet$RasterFilePath[j])
    
    # Handle categorical vs continuous
    method <- ifelse(covariateSheet$IsCategorical[j], "near", "bilinear")
    cov <- terra::resample(cov, templateRaster, method = method)
    
    progressBar()
  }
  
  progressBar(type = "end")
}
```

### 6. Safe File Path Handling

```r
# Handle file paths across platforms
safeFilePath <- function(path) {
  # Normalize path separators
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  
  # Handle network paths on Windows
  if (.Platform$OS.type == "windows") {
    path <- gsub("^//", "\\\\\\\\", path)
  }
  
  return(path)
}

# Check file existence with informative errors
checkFileExists <- function(filepath, description = "File") {
  if (!file.exists(filepath)) {
    stop(paste0(description, " not found: ", filepath, "\n",
               "Working directory: ", getwd()))
  }
  return(safeFilePath(filepath))
}

# Use in transformer
templatePath <- checkFileExists(
  templateSheet$RasterFilePath, 
  "Template raster"
)
```

### 7. Datasheet Append vs Replace Pattern

```r
# Careful datasheet saving with append control
updateDatasheet <- function(scenario, data, name, append = TRUE) {
  
  if (append) {
    # Get existing data
    existing <- datasheet(scenario, name, optional = TRUE, 
                         includeKey = TRUE)
    
    if (nrow(existing) > 0) {
      # Preserve key columns if they exist
      if ("ScenarioID" %in% names(existing)) {
        data$ScenarioID <- existing$ScenarioID[1]
      }
      
      # Combine data
      data <- bind_rows(existing, data)
    }
  }
  
  # Save datasheet
  saveDatasheet(scenario, data, name, append = FALSE)
}
```

### 8. Model Training with Automatic Tuning

```r
# Flexible model training with hyperparameter tuning
trainModelWithTuning <- function(trainData, modelType, 
                                tuning = FALSE, nCores = 1) {
  
  if (modelType == "Random Forest") {
    if (tuning) {
      # Grid search for hyperparameters
      library(caret)
      
      tunegrid <- expand.grid(
        mtry = c(2, sqrt(ncol(trainData) - 1), ncol(trainData) / 3),
        ntree = c(100, 500, 1000),
        nodesize = c(1, 5, 10)
      )
      
      # Use cross-validation
      control <- trainControl(method = "cv", number = 5, 
                            allowParallel = nCores > 1)
      
      model <- train(presence ~ ., data = trainData,
                    method = "rf", trControl = control,
                    tuneGrid = tunegrid)
      
      # Extract best model
      bestModel <- model$finalModel
      
    } else {
      # Use default parameters
      library(randomForest)
      bestModel <- randomForest(presence ~ ., data = trainData,
                               ntree = 500, importance = TRUE)
    }
    
  } else if (modelType == "MaxEnt") {
    library(dismo)
    
    # Prepare data for MaxEnt
    presence_data <- trainData[trainData$presence == 1, -which(names(trainData) == "presence")]
    absence_data <- trainData[trainData$presence == 0, -which(names(trainData) == "presence")]
    
    if (tuning) {
      # Test different feature combinations
      features <- c("L", "LQ", "LQP", "LQPT", "LQPTH")
      models <- list()
      
      for (feat in features) {
        models[[feat]] <- maxent(x = rbind(presence_data, absence_data),
                               p = c(rep(1, nrow(presence_data)), 
                                    rep(0, nrow(absence_data))),
                               args = c(paste0("features=", feat)))
      }
      
      # Evaluate and select best
      # (evaluation code here)
      bestModel <- models[[1]]  # placeholder
      
    } else {
      bestModel <- maxent(x = rbind(presence_data, absence_data),
                         p = c(rep(1, nrow(presence_data)), 
                              rep(0, nrow(absence_data))))
    }
  }
  
  return(bestModel)
}
```

### Unit Test Structure
```python
# test_transformer.py

import unittest
import pandas as pd
import numpy as np
from transformer_functions import validate_data, process_model

class TestTransformerFunctions(unittest.TestCase):
    
    def setUp(self):
        """Set up test data."""
        self.test_data = pd.DataFrame({
            'x': [1, 2, 3],
            'y': [4, 5, 6],
            'value': [0.1, 0.5, 0.9]
        })
    
    def test_validate_data(self):
        """Test data validation."""
        self.assertTrue(validate_data(self.test_data))
        
        # Test with invalid data
        invalid_data = self.test_data.copy()
        invalid_data['value'] = [1.5, 0.5, 0.9]  # Out of range
        
        with self.assertRaises(ValueError):
            validate_data(invalid_data)
    
    def test_process_model(self):
        """Test model processing."""
        result = process_model(self.test_data)
        
        self.assertIsInstance(result, pd.DataFrame)
        self.assertEqual(len(result), len(self.test_data))
        self.assertIn('prediction', result.columns)

if __name__ == '__main__':
    unittest.main()
```

### Debug Mode Implementation
```python
# Enable debug mode through datasheet
debug_settings = myScenario.datasheets("DebugSettings")
DEBUG_MODE = debug_settings.EnableDebug.item() if len(debug_settings) > 0 else False

def debug_log(message, data=None):
    """Conditional debug logging."""
    if DEBUG_MODE:
        ps.environment.update_run_log(f"DEBUG: {message}")
        if data is not None:
            # Save debug data
            debug_path = os.path.join(temp_dir, f"debug_{timestamp}.pkl")
            pd.to_pickle(data, debug_path)
            ps.environment.update_run_log(f"Debug data saved to: {debug_path}")

# Use throughout transformer
debug_log("Starting data processing", input_data)
processed = process_data(input_data)
debug_log("Processing complete", processed)
```

## Package Distribution

### conda-env.yml Example
```yaml
name: simpleSDM
channels:
  - conda-forge
  - defaults
dependencies:
  - python=3.9
  - r-base=4.1
  - pandas=1.4
  - numpy=1.22
  - rasterio=1.3
  - geopandas=0.12
  - scikit-learn=1.1
  - r-randomforest=4.7
  - r-dismo=1.3
  - r-rsyncrosim=1.4
  - pysyncrosim=1.0
```

### Package Metadata (package.config)
```ini
[Package]
Name = simpleSDM
DisplayName = Simple Species Distribution Model
Version = 1.0.0
Description = A simple SDM package for demonstration
Authors = Your Name
Company = Your Organization
HomePage = https://github.com/yourorg/simplesdm
IssueTracker = https://github.com/yourorg/simplesdm/issues

[Requirements]
MinSyncroSimVersion = 3.0.0
MinRVersion = 4.1.0
MinPythonVersion = 3.9.0
```

This comprehensive guide provides patterns and examples for building production-ready SyncroSim packages.