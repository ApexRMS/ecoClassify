---
layout: default
title: Tiling Options
---

# Tiling Options

[Reference](../reference) > Tiling Options

The **Tiling Options** *Datasheet* is used by the optional **2a-Prepare Spatial Tiling** stage. It splits large predicting rasters into a grid of tiles so **2-Predict** can process them in parallel and use less memory. The tiles are merged back into single outputs.

Tiling only takes effect when *Multiprocessing* is enabled in the *Scenario* run settings. Without it, the tile grid is still created but prediction runs on the whole raster, and a warning is written to the run log. When rasters are normalized, each tile is scaled using the ranges of the full image, so tiled and untiled predictions match.

| Field | Description |
|---|---|
| **Auto-calculate tile count from cores** | When checked (default), sets the number of tiles from the maximum number of *Multiprocessing* jobs. |
| **Number of Tiles (X axis)** | Number of tile columns. Used when auto-calculation is off. Must be 1 or more. Defaults to `2`. |
| **Number of Tiles (Y axis)** | Number of tile rows. Used when auto-calculation is off. Must be 1 or more. Defaults to `2`. |
