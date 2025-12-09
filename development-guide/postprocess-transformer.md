# Post-Processing Transformer (3-postprocess.R)

## Overview

The post-processing transformer is the third and final pipeline stage in ecoClassify. It applies spatial filtering and rule-based reclassification to the predicted/classified rasters generated during training and prediction phases. This transformer operates on outputs from both the training (1-train-classifier.R) and prediction (2-predict.r) transformers.

## Purpose

The post-processor serves two main functions:
1. **Spatial Filtering**: Removes noise and fills gaps in classified rasters using neighborhood analysis
2. **Rule-Based Reclassification**: Applies spatial constraints by reclassifying pixels based on environmental covariates or other rule rasters

## Workflow

### 1. Setup Phase

```r
# Sources all function scripts from src/functions/
# Gets current SyncroSim scenario and transfer directory
# Configures terra for memory-efficient disk-backed operations
```

Key configurations:
- Sets temporary directory within transfer directory
- Configures compression (LZW), tiling, and BIGTIFF support
- Uses INT2S datatype with -32768 as NA flag

### 2. Load Configuration

Loads settings from three primary datasheets:

#### PostProcessingFilter Datasheet
- `filterValue`: Number of neighbors required to retain a pixel (default: 8)
- `fillValue`: Number of neighbors needed to fill an NA pixel (default: 8)
- `applyFiltering`: Boolean flag to enable/disable filtering

#### PostProcessingRule Datasheet
- `ruleRasterFile`: Path to environmental/constraint raster
- `ruleMinValue`: Minimum value for rule condition
- `ruleMaxValue`: Maximum value for rule condition
- `ruleReclassValue`: Target class to assign when rule applies

#### Output Datasheets
- `ecoClassify_RasterOutput`: Training outputs
- `ecoClassify_ClassifiedRasterOutput`: Prediction outputs

### 3. Spatial Filtering

Applies filtering to both training and predicting rasters across all timesteps.

**Training Step**:
```r
for (t in trainTimestepList) {
  # Loads PredictedUnfiltered raster
  # Applies filterRasterDataframe() function
  # Saves to PredictedFiltered column
}
```

**Predicting Step**:
```r
for (t in predTimestepList) {
  # Loads ClassifiedUnfiltered raster
  # Applies filterRasterDataframe() function
  # Saves to ClassifiedFiltered column
}
```

**Filter Logic** (implemented in `filterRasterDataframe`):
- Counts neighbors with same classification
- If count < `filterValue`, sets pixel to NA (removes noise)
- If NA pixel has >= `fillValue` neighbors of same class, fills with that class

### 4. Rule-Based Reclassification

Creates "Restricted" versions of outputs by applying spatial rules. Each rule raster defines where certain classifications are allowed or restricted based on environmental conditions.

**Rule Types**:

1. **Categorical Rules** (vmin == vmax):
   - Applies when rule raster exactly equals a specific value
   - Example: Reclassify all pixels where elevation == 100m

2. **Continuous Rules** (vmin < vmax):
   - Applies when rule raster falls within a range [vmin, vmax]
   - Example: Reclassify all pixels where temperature is 15-25°C
   - Uses `classify()` with `right=FALSE, include.lowest=TRUE`

**Processing Steps**:

For each timestep and each rule:
1. Load the unfiltered raster (training or predicting)
2. Create disk-backed working copy
3. For each rule in the rule datasheet:
   - Load rule raster
   - Validate geometry matches (warns and skips if not)
   - Extract rule parameters (vmin, vmax, reclassValue)
   - Apply categorical or continuous logic using `ifel()`
   - Update working copy with reclassified values
4. Write final UnfilteredRestricted raster
5. If filtering enabled, apply filtering to create FilteredRestricted raster

**Output Columns**:
- Training: `PredictedUnfilteredRestricted`, `PredictedFilteredRestricted`
- Predicting: `ClassifiedUnfilteredRestricted`, `ClassifiedFilteredRestricted`

### 5. Save Results

Saves updated datasheets back to SyncroSim:
- `ecoClassify_RasterOutput` (training outputs)
- `ecoClassify_ClassifiedRasterOutput` (predicting outputs)

## Output Structure

For each timestep, the transformer can produce up to 4 output variants:

### Training Outputs
1. **PredictedUnfiltered**: Raw model predictions
2. **PredictedFiltered**: After spatial filtering
3. **PredictedUnfilteredRestricted**: After rule reclassification
4. **PredictedFilteredRestricted**: After both reclassification and filtering

### Predicting Outputs
1. **ClassifiedUnfiltered**: Raw classifications
2. **ClassifiedFiltered**: After spatial filtering
3. **ClassifiedUnfilteredRestricted**: After rule reclassification
4. **ClassifiedFilteredRestricted**: After both reclassification and filtering

## Memory Management

The transformer implements aggressive memory management strategies:

- **Disk-backed operations**: All intermediate rasters written to disk
- **Temporary file cleanup**: Uses `tempfile()` for intermediate steps
- **Explicit garbage collection**: Calls `gc()` after each major operation
- **Per-timestep cleanup**: Removes large objects after processing each timestep
- **Compression**: All outputs use LZW compression with tiling

## Error Handling

Validates inputs and provides informative warnings:
- Missing rule rasters: Warns and skips rule
- Geometry mismatches: Warns if rule raster extent doesn't match output raster
- NA values in rules: Warns and skips invalid rules
- Missing input files: Skips timesteps with no input files
- Min/max swap: Automatically corrects vmin > vmax and logs info message

## Integration with Pipeline

**Inputs** (from previous transformers):
- Training: PredictedUnfiltered rasters from 1-train-classifier.R
- Predicting: ClassifiedUnfiltered rasters from 2-predict.r

**Outputs** (saved to SyncroSim):
- Filtered and/or restricted rasters for downstream analysis
- Updated output datasheets with file paths

## Configuration Examples

### Example 1: Simple Filtering
```r
# PostProcessingFilter datasheet
filterValue = 8    # Remove isolated pixels with < 8 neighbors
fillValue = 8      # Fill gaps surrounded by >= 8 neighbors
applyFiltering = TRUE
```

### Example 2: Elevation Constraint
```r
# PostProcessingRule datasheet
ruleRasterFile = "path/to/elevation.tif"
ruleMinValue = 0
ruleMaxValue = 1000
ruleReclassValue = 1  # Force to class 0 (rval = 1-1 = 0) outside 0-1000m
```

### Example 3: Land Use Restriction
```r
# PostProcessingRule datasheet (categorical)
ruleRasterFile = "path/to/landuse.tif"
ruleMinValue = 5   # Urban land use code
ruleMaxValue = 5   # Same value = categorical
ruleReclassValue = 1  # Force to class 0 in urban areas
```

## Performance Considerations

- Filtering is relatively fast (neighborhood analysis)
- Reclassification can be memory-intensive with large rasters and many rules
- Multiple rules processed sequentially, not in parallel
- Each timestep processed independently (could parallelize if needed)
- Disk I/O is the primary bottleneck due to disk-backed operations

## Related Functions

- `filterRasterDataframe()`: Located in `src/functions/0.6-post-processing.r`
- Helper functions for datasheet manipulation in setup scripts

## Notes

- Rule reclassification values are decremented by 1 (class IDs are 0-indexed internally)
- Filtering can be applied before or after reclassification (currently: filter → reclass → filter again)
- The transformer handles missing or NA inputs gracefully
- All file paths are stored in SyncroSim datasheets for traceability
