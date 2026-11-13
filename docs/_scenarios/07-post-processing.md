---
layout: default
title: Post-Processing
---

# Post-Processing

[Reference](../reference) > Post-Processing

The **3-Post-Process** stage cleans the binary presence/absence rasters produced by training and prediction. It uses two *Datasheets*, **Filtering** and **Rule-Based Restrictions**, and produces four versions of each binary map: unfiltered, filtered, restricted, and restricted and filtered.

## Filtering

Filtering removes isolated presence pixels (speckle) and fills small gaps, based on how many of a pixel's 8 neighbours are presence.

| Field | Description |
|---|---|
| **Apply filtering** | When checked (default), applies the filter. |
| **Min neighbors to keep** | A presence pixel stays presence only if at least this many of its 8 neighbours are also presence; otherwise it becomes absence. Value from 0 to 8. Defaults to `8` when blank. Lower values remove less. |
| **Min neighbors to fill** | After the step above, an absence pixel becomes presence if at least this many of its 8 neighbours are presence. Value from 0 to 8. Defaults to `8` when blank. Lower values fill more. |

## Rule-Based Restrictions

Restrictions use other rasters to force pixels to presence or absence where the classifier is known to be wrong, for example removing predicted snow below a given elevation or water on steep slopes. Add one row per rule. Rules are applied in order, and later rules override earlier ones.

| Field | Description |
|---|---|
| **Condition Raster** | A single-band raster on the same grid (extent, resolution and coordinate reference system) as the predictions. Rules whose raster is missing or doesn't match the grid are skipped with a warning. |
| **Minimum Value** | Lower bound of the condition range (inclusive). |
| **Maximum Value** | Upper bound of the condition range (inclusive). To match a single category, set the minimum and maximum to the same value. If the minimum is larger than the maximum, the two are swapped. |
| **Reclassify Value** | The class given to pixels where the condition raster falls within the range: `0` (absence) or `1` (presence). Pixels outside the range, or where the condition raster has no data, are unchanged. |
