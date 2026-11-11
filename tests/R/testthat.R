## Entrypoint for the ecoClassify R test suite.
## Run from the repo root with: Rscript tests/R/testthat.R

library(testthat)

find_ecoclassify_repo_root <- function(start = getwd()) {
  path <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "src", "package.xml"))) {
      return(path)
    }
    parent <- dirname(path)
    if (parent == path) {
      stop("Could not locate the ecoClassify repo root (src/package.xml) above ", start)
    }
    path <- parent
  }
}

repoRoot <- find_ecoclassify_repo_root()
test_dir(file.path(repoRoot, "tests", "R", "testthat"), stop_on_failure = TRUE)
