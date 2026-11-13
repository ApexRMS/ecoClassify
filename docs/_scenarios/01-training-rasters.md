---
layout: default
title: Training Rasters and Covariates
---

# Training Rasters and Covariates

[Reference](../reference) > Training Rasters and Covariates

These *Datasheets* hold the inputs to the **1-Train Classifier** stage.

## Training Rasters

Each row pairs a multi-band image with a user-classified raster of the same area. Add one row per image; rows can share a *Timestep* value or use different ones.

| Field | Description |
|---|---|
| **Timestep** | Integer identifying the image (labelled with the **Raster group name** set in [Terminology](../project/01-terminology)). Outputs are reported by this value. |
| **Training Raster** | A multi-band raster (for example, a GeoTIFF of satellite imagery). Each band becomes a predictor. All training and predicting rasters must have the same bands in the same order. |
| **User Classified Raster** | A single-band raster with the same extent, resolution and coordinate reference system as the training raster. By default it is treated as binary: the lowest value becomes absence (0) and the highest becomes presence (1). To use a multi-class raster, set [Target Class Options](04-target-class-options). Cells with no data are not sampled. |

Rasters without a coordinate reference system are given a generic planar one. Training and user-classified rasters that don't match in extent or resolution are checked and aligned before sampling, and an image that can't be read is skipped with a warning.

## Covariates

Optional rasters added to every training raster as extra predictors, such as elevation or land cover.

| Field | Description |
|---|---|
| **Covariate Raster** | A single-band raster covering the training area. |
| **Data Type** | **Continuous** for numeric values, or **Categorical** for class codes (treated as factors by the model). |

The same covariates should also be entered in the [Predicting Rasters and Covariates](05-predicting-rasters) *Datasheets*, so the model sees the same predictors at prediction time.
