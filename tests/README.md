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
      test-integration-synthetic-library.R   # builds a library from synthetic rasters, runs it
```

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

## Runtime expectations

- Unit tests: about 10 seconds.
- Integration test: a few minutes. It trains a single Random Forest with tuning off on three
  50 x 50 synthetic rasters (two training timesteps, one predicting timestep), then predicts and
  post-processes. It does not cover MaxEnt, CNN, tuning, contextualization or spatial tiling.

## Notes for maintainers

- The integration test runs whichever `ecoClassify` version SyncroSim picks for a new library
  (normally the newest installed). Install your working copy with `src/build.ps1 -Install` first,
  otherwise you are testing the released package, not your changes.
- `addRasterAdjacencyValues()` always derives two principal components, so contextualization
  needs rasters with at least two bands. The contextualization tests use multi-band rasters for
  this reason.
- `normalizeBand()` has no guard for a constant band (max == min); it returns NaN. There is no
  test for that case yet.
