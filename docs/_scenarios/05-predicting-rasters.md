---
layout: default
title: Predicting Rasters and Covariates
---

# Predicting Rasters and Covariates

[Reference](../reference) > Predicting Rasters and Covariates

These *Datasheets* hold the inputs to the **2-Predict** stage, which applies the model trained by **1-Train Classifier** to new images. A predicting *Scenario* is usually set up with the training *Scenario* as a *Dependency*, so it can find the trained model.

## Predicting Rasters

| Field | Description |
|---|---|
| **Timestep** | Integer identifying the image (labelled with the **Raster group name** set in [Terminology](../project/01-terminology)). Outputs are reported by this value. |
| **Predicting Raster** | A multi-band raster to classify. It must have the same bands, in the same order, as the training rasters. |

## Covariates

| Field | Description |
|---|---|
| **Covariate Raster** | A single-band raster covering the predicting area. Enter the same covariates, in the same order, as in [Training Rasters and Covariates](01-training-rasters). |
| **Data Type** | **Continuous** for numeric values, or **Categorical** for class codes. Must match the type used in training. |

The pre-processing chosen in [Advanced Classifier Options](03-advanced-classifier-options) (band name override, rounding, normalization and contextualization) is applied to these rasters in the same way as in training.
