# EcoClassify

**EcoClassify** is an open-source [SyncroSim](https://syncrosim.com/) package that uses semantic image segmentation to classify images. Both **ecoClassify** and SyncroSim have been designed and developed by [ApexRMS](https://apexrms.com/).

* See the [Home page](https://apexrms.github.io/ecoClassify/) for an overview of **ecoClassify**
* See the [Getting Started](https://apexrms.github.io/ecoClassify/getting_started.html) page to get up and running quickly

## Development

* `src/` is the SyncroSim package (`package.xml`, transformer scripts and `functions/`).
* `src/ecoclassifyUI/` is the C# project for the custom SyncroSim Studio forms. It builds `src/ecoclassifyUI.dll`, which ships in the package.
* Build the package with `.\src\build.ps1` (add `-Install` to install it into SyncroSim, or `-SkipUI` to reuse the committed DLL). Building the UI needs the .NET SDK and SyncroSim installed.
* Run the tests with `Rscript tests/R/testthat.R`. See [tests/README.md](tests/README.md) for prerequisites.

The ongoing development of **ecoClassify** has been generously supported by several agencies, including the [U.S. Geological Survey](https://www.usgs.gov/), the [Ute Mountain Ute Tribe](https://www.utemountainutetribe.com/), the [University of Toronto](https://www.utoronto.ca/), and [Western University](https://www.uwo.ca/index.html). As a result, **ecoClassify** is available as a free download.
