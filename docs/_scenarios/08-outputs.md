---
layout: default
title: Outputs
---

# Outputs

[Reference](../reference) > Outputs

**ecoClassify** writes its results to the following output *Datasheets*. They can be viewed as *Charts*, *Maps* and *Images* in SyncroSim Studio, and most can be exported.

## Training outputs (1-Train Classifier)

| Datasheet | Contents |
|---|---|
| **Training Rasters** | For each training timestep: the user-classified raster, the predicted probability, and the binary prediction (unfiltered, filtered, restricted, and restricted and filtered). |
| **Confusion Matrix** | Counts of correctly and incorrectly classified test pixels, as a table and as an image. |
| **Statistics** | Model evaluation statistics on the test pixels: accuracy (with confidence interval), kappa, sensitivity, specificity, precision, recall, F1 score, balanced accuracy and related measures. |
| **Model Evaluation Metrics** | Accuracy, precision, recall, specificity, F1 score and AUC for the target class, used by the *Charts*. |
| **Variable Importance** | A plot and a table of how much each band or covariate contributed to the model. |
| **Layer Histogram and Response** | For each predictor, the distribution of values for presence and absence pixels and the model's response. |
| **RGB Images** | True-colour previews of the training rasters, when red, green and blue bands are set in [Terminology](../project/01-terminology). |
| **Model** | The trained model and its pre-processing settings, used by **2-Predict**. |
| **Prediction Summary** | For each timestep: number of predicted presence pixels, predicted area, and mean and median probability. |

## Prediction outputs (2-Predict)

| Datasheet | Contents |
|---|---|
| **Classified Rasters** | For each predicting timestep: the predicted probability and the binary prediction (unfiltered, filtered, restricted, and restricted and filtered). |
| **RGB Images** | True-colour previews of the predicting rasters, when red, green and blue bands are set. |
| **Prediction Summary** | The same per-timestep summary as above, for the predicting rasters. |

## Exporting

From the *Scenario* results in SyncroSim Studio, you can export each raster type (probability, binary, filtered, restricted, and restricted and filtered) as GeoTIFFs, and the confusion matrix, statistics and variable importance as tables or images.
