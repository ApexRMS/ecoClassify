# ecoClassify test suite

## Layout

```
tests/
  R/
    testthat.R                               # entrypoint: Rscript tests/R/testthat.R
    testthat/
      helper-ecoclassify.R                   # shared helpers (not a test file itself)
      test-setup-functions.R                 # unit tests, src/functions/0.0-setup.r
      test-pre-processing.R                  # unit tests, src/functions/0.1-pre-processing.r
      test-training-prediction.R             # unit tests, src/functions/0.2-training-and-prediction.r
      test-contextualization.R               # unit tests, src/functions/0.3-contextualization.r
      test-post-processing.R                 # unit tests, src/functions/0.6-post-processing.r
      test-spatial-tiling.R                  # unit tests, src/functions/0.7-spatial-tiling.r
      test-integration-synthetic-library.R   # builds libraries from synthetic rasters, runs them
```

Some unit tests compare a rewritten function against a copy of the code it replaced
(`reference_restrict()`, `reference_tile_matrix()`), to prove performance rewrites give
identical results.

Unit tests source the real `src/functions/*.r` files (via `source_ecoclassify_r()` in
`helper-ecoclassify.R`) rather than copies of the functions, so they test what ships.
Rasters are generated in the tests with `terra`; there are no committed fixtures.

## Prerequisites

- **Unit tests** need R with the packages that `src/functions/0.0-setup.r` loads at the top
  level (`terra`, `tidyverse`, `tidymodels`, `sf`, `cvms`, `rsyncrosim`, `gtools`,
  `doParallel`, ...) plus `testthat` and `ranger`. The package's conda environment
  (`src/ecoClassify-conda.yml`) has all of these; add `testthat` if you use it. No SyncroSim
  installation is required.
- **Integration tests** (`test-integration-*.R`) additionally require SyncroSim with the
  `ecoClassify` package installed (use `src/build.ps1 -Install` to test your working copy), and an
  R configured in SyncroSim that has the package's dependencies. They self-skip (via
  `skip_if_not(syncrosim_available())`) when SyncroSim or the package can't be found, so it's safe
  to run the whole suite on a machine without SyncroSim.

## Running

From the repo root:

```sh
Rscript tests/R/testthat.R
```

To run one file, e.g. while working on a function:

```r
library(testthat)
source("tests/R/testthat/helper-ecoclassify.R")
test_file("tests/R/testthat/test-pre-processing.R")
```

By default the integration test runs the transformers with the R configured in SyncroSim. To test
the package's conda environment instead (what Studio users get, and what catches dependency
problems in `src/ecoClassify-conda.yml` / `src/installDependencies.r`), set
`ECOCLASSIFY_TEST_CONDA=true` before running it:

```powershell
$env:ECOCLASSIFY_TEST_CONDA = "true"; Rscript tests/R/testthat.R
```

## Runtime expectations

- Unit tests: about 10 seconds.
- Integration tests: about 8 minutes. Each trains a Random Forest with tuning off on small
  synthetic rasters (two training timesteps, one predicting timestep, target class 2):
  - full pipeline (train, predict, post-process) with filtering;
  - normalization + contextualization, predicting on a copy of a training image (must reproduce
    its training probabilities, which checks the saved PCA is reused);
  - spatial tiling (2a + 2 x 2 tiles) against an untiled run (must match).
  They do not cover MaxEnt, CNN or tuning.
- With `ECOCLASSIFY_TEST_CONDA=true`, the first run after a `condaEnvVersion` change builds the
  conda environment and installs every R package from CRAN (including the torch backend), which
  can take 20+ minutes. Later runs reuse the environment.

## Notes for maintainers

- The integration test runs whichever `ecoClassify` version SyncroSim picks for a new library
  (normally the newest installed). Install your working copy with `src/build.ps1 -Install` first,
  otherwise you are testing the released package, not your changes.
- Contextualization derives two principal components, so it needs rasters with at least two
  bands; `fitContextPca()` stops with a clear message otherwise.
- Output rasters must use a datatype SyncroSim can read (Int32, Float32, Float64): it rejects
  Byte/Int16 when merging spatial tiles or extracting bands. A unit test guards `binaryWopt()` /
  `floatWopt()`.
- terra's `ifel()` handles an NA condition inconsistently (keeps a raster "no" argument, gives NA
  for a constant one, and nested calls can turn NA into 0). Prefer `lapp()` with vector logic, or
  check NA handling with a test.
- `normalizeBand()` has no guard for a constant band (max == min); it returns NaN. There is no
  test for that case yet.
