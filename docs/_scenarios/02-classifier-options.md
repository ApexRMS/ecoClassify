---
layout: default
title: Classifier Options
---

# Classifier Options

[Reference](../reference) > Classifier Options

The **Classifier Options** *Datasheet* sets the model and how many pixels it learns from. It is used by **1-Train Classifier** and read again by **2-Predict**.

| Field | Description |
|---|---|
| **Sample size** | Total number of pixels sampled from the training rasters. Defaults to `10000`. Pixels are sampled across a grid of spatial blocks so the samples are spread out, and the presence class is kept at 15–35% of the sample when it is rare. 80% of the blocks are used for training and 20% for testing, so the evaluation metrics come from areas the model did not see. Larger samples usually improve the model but take longer to train. |
| **Model type** | The classifier. **Random Forest** (default; the `ranger` package) is a robust general choice and reports variable importance. **MaxEnt** (the `maxnet` package) is a presence/background model often used for species and habitat mapping. **CNN** (the `torch` package) is a convolutional neural network that can learn more complex patterns but needs more data and time. |

See [Advanced Classifier Options](03-advanced-classifier-options) for pre-processing, tuning and threshold settings.
