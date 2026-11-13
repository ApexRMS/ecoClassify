---
layout: default
title: Reference
permalink: /reference
description: "Reference guide for the ecoClassify package"
---

# Reference guide for **ecoClassify** SyncroSim *Package*

In SyncroSim, all inputs and outputs associated with a model are stored in a single file (with the extension .ssim), referred to as a SyncroSim *Library*. The inputs and outputs contained within a SyncroSim *Library* are organized into *Datafeeds* (or tabs in the SyncroSim UI). Each *Datafeed* can be made up of one or more tables of data, called *Datasheets*. Each *Datasheet* is associated with one of three scopes: *Library*, *Project* or *Scenario*. See the [SyncroSim documentation](https://docs.syncrosim.com/how_to_guides/library_overview.html) for more details.

**ecoClassify** has one *Project* scoped *Datasheet* and multiple *Scenario* scoped *Datasheets*. The *Scenario* *Datasheets* are used by the package's four pipeline stages:

1. **1-Train Classifier** trains a Random Forest, MaxEnt or CNN model from training rasters and user-classified rasters.
2. **2a-Prepare Spatial Tiling** (optional) splits large predicting rasters into tiles so prediction can run in parallel.
3. **2-Predict** applies the trained model to new rasters.
4. **3-Post-Process** filters the binary predictions and applies rule-based restrictions.

Within this Reference guide, you will find details on the function of each field within **ecoClassify** *Datasheets*. For details on *Datasheets* not covered in this reference guide, such as *Multiprocessing*, see the [SyncroSim Core Datasheets Overview](https://docs.syncrosim.com/reference/ds_overview.html).

<li class="no-bullets">Project <i>Datasheet</i>:</li>
{% for section in site.project %}
  <li> <a href="{{site.baseurl}}{{ section.url }}"> {{ section.title }}</a> </li>
{% endfor %}
<div class="spacer"></div>
<li class="no-bullets">Scenario <i>Datasheets</i>:</li>
{% for section in site.scenarios %}
  <li> <a href="{{site.baseurl}}{{ section.url }}"> {{ section.title }}</a> </li>
{% endfor %}
