---
layout: default
title: Terminology
---

# Terminology

[Reference](../reference) > Terminology

The **Terminology** *Datasheet* is *Project* scoped, so its settings apply to every *Scenario* in the *Project*. It sets labels used across the *Library* and identifies which raster bands hold common spectral bands.

| Field | Description |
|---|---|
| **Band label override file** | Optional. A CSV file with one band name per row, in band order (a header row is allowed). The number of names must match the number of bands in the training and predicting rasters. The names are applied only when **Override band names** is checked under [Advanced Classifier Options](../scenarios/03-advanced-classifier-options). Useful when raster band names are missing or differ between images. |
| **Raster group name** | The label used for the grouping of rasters (the *Timestep* column in the raster *Datasheets*). Defaults to `Timestep`. Change it to something more meaningful, such as `Image` or `Year`. |
| **Red band** | Optional. Name of the band holding red reflectance. When red, green and blue are all set, **ecoClassify** creates true-colour RGB previews of the training and predicting rasters. |
| **Green band** | Optional. Name of the band holding green reflectance. Used with **Red band** and **Blue band** for RGB previews. |
| **Blue band** | Optional. Name of the band holding blue reflectance. Used with **Red band** and **Green band** for RGB previews. |
| **NIR band** | Optional. Name of the near-infrared band. Stored for reference; not currently used by the models. |
| **SWIR band** | Optional. Name of the shortwave infrared band. Stored for reference; not currently used by the models. |
