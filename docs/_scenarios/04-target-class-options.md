---
layout: default
title: Target Class Options
---

# Target Class Options

[Reference](../reference) > Target Class Options

**ecoClassify** trains a binary (presence/absence) classifier. The **Target Class Options** *Datasheet* lets you train from a multi-class user-classified raster by choosing one class as the target. When it is not used, the lowest value in each user-classified raster is treated as absence (0) and the highest as presence (1).

| Field | Description |
|---|---|
| **Use target class** | When checked, converts the user-classified rasters to presence/absence using the fields below. |
| **Target class value** | The class code to predict. Pixels with this value become presence (1). |
| **Background values (comma-separated)** | Optional, for example `2,5,7`. When given, only these class codes become absence (0); all other codes are excluded from training. If a value is listed here and is also the target, it is treated as background. When left blank, every class other than the target (and the ignore values) becomes absence. |
| **Ignore values (comma-separated)** | Optional. Class codes excluded from training, such as clouds or unclassified pixels. Used only when **Background values** is blank. |
| **Target class label** | Optional. A readable name for the target class, such as `Snow`, used in the evaluation outputs. |
