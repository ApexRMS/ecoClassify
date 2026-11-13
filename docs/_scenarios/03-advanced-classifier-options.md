---
layout: default
title: Advanced Classifier Options
---

# Advanced Classifier Options

[Reference](../reference) > Advanced Classifier Options

The **Advanced** *Datasheet* (under **Classifier Options**) controls pre-processing, model tuning and how probabilities are converted to presence or absence. The same pre-processing is repeated at prediction time.

| Field | Description |
|---|---|
| **Override band names** | When checked, renames the bands of the training and predicting rasters using the **Band label override file** in [Terminology](../project/01-terminology). |
| **Normalize rasters** | When checked, rescales each band of each raster to the range 0–1 using that raster's own minimum and maximum. Helps when images differ in brightness or units. Off by default. |
| **Raster decimal places (rounding)** | Rounds raster values to this many decimal places before training and prediction. Defaults to `2`. Rounding reduces noise and file size. |
| **Enable automated tuning** | When checked (default), searches for better model settings (hyperparameters) during training. Training takes longer. |
| **Model tuning objective** | The metric maximized when choosing the probability threshold that separates presence from absence. **Youden** (default) maximizes sensitivity + specificity − 1. Other choices are **Accuracy**, **Specificity**, **Sensitivity**, **Precision** and **Balanced** (balanced accuracy). Choose **Sensitivity** to miss fewer presences, or **Specificity** / **Precision** to make fewer false detections. |
| **Set probability threshold** | When checked, uses **Probability threshold** instead of choosing a threshold with the tuning objective. |
| **Probability threshold** | Pixels with a predicted probability at or above this value are classed as presence. Defaults to `0.5`. Used only when **Set probability threshold** is checked. |
| **Apply contextualization** | When checked, adds neighbourhood predictors for every band: the local mean, standard deviation, minimum, maximum and range within a moving window. It also adds the first two principal components (PC1, PC2) of the bands, fitted once across all training rasters and reused at prediction. Helps the model use texture and spatial context, but multiplies the number of predictors. |
| **Contextualization window size** | Width, in pixels, of the square moving window used for contextualization. Defaults to `3` (a 3 × 3 window). Use an odd number. |
| **Random seed** | Optional. An integer that makes sampling and model training repeatable between runs. |
