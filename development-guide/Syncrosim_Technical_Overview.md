# Technical Overview of SyncroSim Studio and Console

## Introduction

SyncroSim is a scenario-based modeling platform used for ecological forecasting, image classification, and complex data workflows. It provides:

- **SyncroSim Studio**: A GUI for building, managing, and running models.
- **SyncroSim Console**: A CLI for automating workflows and batch runs.
- **SyncroSim Cloud**: A cloud platform for publishing and sharing models online.

This document explains how these tools work together via the `package.xml` configuration, focusing on how the XML structure communicates between the platform and model logic.

---

## Core Terminology

| Term         | Description |
|--------------|-------------|
| **Package**  | A set of XML definitions, transformers (R/Python), and metadata. Packages define new model functionality. |
| **Library**  | A workspace for managing and running scenarios using one or more packages. A Library contains all the data and results for a modeling project. |
| **Scenario** | A single run configuration, defined by a unique combination of inputs. |
| **Datasheet**| A tabular structure used for inputs/outputs. Defined in the package XML. |
| **Transformer** | An executable R/Python script that consumes datasheets and returns results. |
| **Layout**   | A set of rules in XML that control how datasheets and outputs appear in the Studio UI. |
| **Timestep/Iteration** | Optional temporal dimensions that can be built into model logic. |

---

## The Role of `package.xml`

The `package.xml` file is the **primary communication bridge** between a model's code and the SyncroSim platform (Studio, Console, and Cloud).

### Package Development Workflow

1. **Define the structure** in `package.xml`.
2. **Implement the logic** in R or Python scripts (transformers).
3. **Map inputs/outputs** to datasheets.
4. **Expose UI elements** using `<layout>` tags.
5. **Build the package** into `.ssimpkg` using SyncroSim Console or Package Manager.
6. **Deploy** in Studio, Console, or publish to Cloud.

---

## XML Anatomy and Structure

Here’s a simplified overview of the main sections in `package.xml`:

```xml
<package name="ecoClassify" version="1.0.0" displayName="ecoClassify">
  
  <!-- Input and output datasheets -->
  <dataSheet name="InputTrainingRasters" displayName="Training Rasters">
    <column name="RasterFile" dataType="String" isExternalFile="True" isRaster="True"/>
  </dataSheet>

  <!-- Transformers -->
  <transformer name="TrainClassifier" programName="Rscript" programArguments="train.R">
    <dataSheet name="InputTrainingRasters" type="Input"/>
    <dataSheet name="RasterOutput" type="Output"/>
  </transformer>

  <!-- UI Layout for Studio -->
  <layout type="Scenario">
    <item name="InputTrainingRasters"/>
    <item name="RasterOutput"/>
  </layout>

</package>
```

### Key Elements:

- `<dataSheet>`: Defines tabular inputs or outputs, specifying columns, datatypes, and validation rules.
- `<column>`: Declares attributes for UI widgets such as text boxes, dropdowns, file selectors, raster inputs, etc.
- `<transformer>`: Links scripts to inputs and outputs. SyncroSim uses this to call R/Python and pass/load the appropriate datasheets.
- `<layout>`: Specifies how elements should be presented in Studio. Used to organize tabs, charts, maps, and exports.

---

## Communicating with Studio via XML

Studio **automatically generates its user interface** from the XML structure in a package. Here’s how:

| XML Attribute / Tag       | Studio Behavior |
|---------------------------|-----------------|
| `dataType="String"`       | Renders a text input. |
| `isExternalFile="True"`   | Renders a file browser. |
| `validationType="Datasheet"` | Renders a dropdown linked to another datasheet. |
| `isSingleRow="True"`      | Ensures only one row appears in the datasheet tab. |
| `<layout type="Scenario">` | Controls which datasheets appear in scenario configuration. |
| `<layout type="Chart">`   | Specifies output variables for chart plotting. |
| `<layout type="Map">`     | Declares raster output variables as map layers. |
| `hasTimestep="True"`      | Enables per-timestep charting and mapping. |

By customizing these elements, developers build rich, domain-specific interfaces in Studio **without writing UI code**.

---

## Temporal Modeling: Timesteps and Charts

- **RunControl** datasheets define model start/end time (`MinimumTimestep`, `MaximumTimestep`).
- If a datasheet has `hasTimestep="True"` and includes a `Timestep` column, SyncroSim can **visualize temporal trends** in both charts and maps.
- Charts can show line graphs over time, filtered by scenario or model dimensions.
- Maps can render **time-series of classified rasters**, enabling spatial-temporal analysis.

To enable this:

```xml
<dataSheet name="RasterOutput" hasTimestep="True">
  <column name="Probability" dataType="String" isRaster="True" isExternalFile="True"/>
</dataSheet>
```

And define maps:

```xml
<layout type="Map">
  <item name="ProbabilityMap" dataSheet="RasterOutput" column="Probability"/>
</layout>
```

---

## Charts and Maps in Studio

- **Charts** (`<layout type="Chart">`) display time-series, bar plots, or grouped values from datasheets.
- **Maps** (`<layout type="Map">`) visualize rasters with built-in viewers.
- **Images** (`<layout type="Image">`) show things like confusion matrices, RGB previews, and variable importance plots.

Each chart or map is declared as an `<item>` within a `<group>`, allowing developers to group visual outputs logically.

---

## SyncroSim Cloud: Publishing

SyncroSim supports **publishing packages and scenarios** to the cloud for web-based deployment.

### Publishing Workflow:

1. Finalize a scenario in Studio.
2. Right-click > **Publish to Cloud**.
3. Select inputs and outputs to include.
4. SyncroSim compresses and uploads the scenario and associated package.

This enables:
- Cloud execution of models (e.g., CNN classifiers, statistical summaries).
- Shared access to results and inputs.
- Controlled environments (via `condaEnv` specs in `package.xml`).

Transformers published to the cloud must be:
- Deterministic
- Self-contained (e.g., with all required files and environments)
- Declaratively defined in `package.xml`

---

## Summary: How It All Fits Together

| Component          | Role |
|--------------------|------|
| `package.xml`      | Declarative configuration. Defines UI, logic links, and data structure. |
| Studio             | Reads XML to generate user-facing GUI. Executes transformers. |
| Console            | Headless execution of the same workflows as Studio. |
| R/Python Scripts   | Define the core modeling logic, executed via transformers. |
| Layouts            | Control GUI visibility of datasheets, charts, maps, and export options. |
| Cloud Publishing   | Allows sharing and remote execution of configured scenarios. |

---

