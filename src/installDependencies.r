# 0.0-installDependencies.r  — ecoClassify
# Robust installer for conda envs (Windows/Linux/macOS)
# - avoids loading DLLs during checks (prevents cross-version errors)
# - pre-updates 'terra' on R 4.1.* so ecospat won't try to swap DLLs mid-run
# - skips loading packages that were just installed; prompts for fresh session

# -------------------------
# Helpers (no DLL loads)
# -------------------------
get_conda_lib_path <- function() {
  p <- .libPaths()[1]
  p <- gsub("^['\"]|['\"]$", "", p)
  normalizePath(p, winslash = "/", mustWork = FALSE)
}

pkg_is_installed <- function(pkg, lib_path) {
  pkg %in% rownames(utils::installed.packages(lib.loc = lib_path))
}

pkg_version <- function(pkg, lib_path) {
  ip <- utils::installed.packages(lib.loc = lib_path)
  if (pkg %in% rownames(ip)) ip[pkg, "Version"] else NULL
}

install_cran <- function(pkg, lib_path, type = NULL, ...) {
  if (is.null(type)) type <- if (.Platform$OS.type == "windows") "win.binary" else "source"
  if (!pkg_is_installed(pkg, lib_path)) {
    message(sprintf("Installing %s from CRAN (%s)...", pkg, type))
    install.packages(pkg, repos = "https://cran.r-project.org", lib = lib_path, type = type, ...)
    if (!pkg_is_installed(pkg, lib_path)) stop(sprintf("Failed to install %s", pkg))
    return(TRUE)
  } else {
    message(sprintf("✓ %s already installed", pkg))
    return(FALSE)
  }
}

# -------------------------
# Main
# -------------------------
installDependencies <- function() {
  message("=== ecoClassify dependency installation ===")
  message(R.version.string)
  message("Platform: ", .Platform$OS.type, "\n")

  # Show library paths
  message("Current .libPaths():")
  for (i in seq_along(.libPaths())) message(sprintf("  [%d] %s", i, .libPaths()[i]))
  message("")

  lib_path <- get_conda_lib_path()
  message("Target library path: ", lib_path)

  # Packages expected from conda env (do NOT load them here)
  conda_pkgs <- c("terra", "sf", "ranger", "rsyncrosim")

  message("Checking conda-provided packages...")
  missing_conda <- character(0)
  for (pkg in conda_pkgs) {
    if (pkg_is_installed(pkg, lib_path)) {
      message(sprintf("✓ %s found (version %s)", pkg, pkg_version(pkg, lib_path)))
    } else {
      message(sprintf("✗ %s not found in library", pkg))
      missing_conda <- c(missing_conda, pkg)
    }
  }
  if (length(missing_conda) > 0) {
    warning(
      "The following packages are missing but should be supplied by the conda env: ",
      paste(missing_conda, collapse = ", "),
      "\nPlease (re)create the environment from ecoClassifyEnv.yml and rerun this installer."
    )
  }

  # -------------------------
  # Preflight: ensure terra >= 1.7-23 on R 4.1.*
  # -------------------------
  TERRA_MIN <- "1.7-23"
  r_minor <- as.character(getRversion())
  if (grepl("^4\\.1\\.", r_minor)) {
    v <- pkg_version("terra", lib_path)
    needs_terra <- is.null(v) || utils::compareVersion(v, TERRA_MIN) < 0
    if (needs_terra) {
      message(sprintf("Updating 'terra' to >= %s for R %s ...", TERRA_MIN, r_minor))
      install.packages(
        "terra",
        repos = "https://cran.r-project.org",
        lib = lib_path,
        type = if (.Platform$OS.type == "windows") "win.binary" else "source",
        dependencies = TRUE
      )
      message("✓ terra updated; continuing…")
    }
  }

  # -------------------------
  # CRAN installs into the env library
  # -------------------------
  message("\nInstalling CRAN packages into the conda env library...")

  cran_pkgs <- c(
    "tidyverse",
    "magrittr",
    "foreach",
    "iterators",
    "coro",
    "gtools",
    "codetools",
    "ecospat",
    "cvms",
    "doParallel",
    "tidymodels"
  )

  installed_now <- character(0)
  for (pkg in cran_pkgs) {
    did <- tryCatch(
      install_cran(pkg, lib_path),
      error = function(e) stop(sprintf("Failed installing %s: %s", pkg, e$message))
    )
    if (isTRUE(did)) installed_now <- c(installed_now, pkg)
  }

  # -------------------------
  # Torch (package + backend), but avoid hard-loading now
  # -------------------------
  message("\nSetting up torch...")
  if (install_cran("torch", lib_path)) installed_now <- c(installed_now, "torch")
  Sys.setenv(TORCH_INSTALL = "1")
  # Only install backend if missing; don't force-load torch DLLs here
  if (!("torch" %in% installed_now)) {
    # torch package already present; probe backend
    ok <- FALSE
    try(ok <- torch::torch_is_installed(), silent = TRUE)
    if (isFALSE(ok)) {
      torch::install_torch()
      installed_now <- unique(c(installed_now, "torch"))
    }
  } else {
    # torch just installed: install backend
    torch::install_torch()
  }

  # -------------------------
  # Verification (lightweight)
  #   - If a pkg was installed/updated *now*, tell user to verify in a fresh session.
  #   - Otherwise, try loading it.
  # -------------------------
  message("\n=== Verification ===")
  verify_pkgs <- c(
    conda_pkgs,
    "tidyverse","magrittr","foreach","iterators","coro","gtools",
    "codetools","ecospat","cvms","doParallel","tidymodels","torch"
  )

  for (pkg in verify_pkgs) {
    if (!pkg_is_installed(pkg, lib_path)) {
      message(sprintf("✗ %s not installed (expected in %s)", pkg, lib_path))
      next
    }
    if (pkg %in% installed_now) {
      message(sprintf("• %s installed/updated in this run; start a *new R session* to load/verify.", pkg))
      next
    }
    ok <- try({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      TRUE
    }, silent = TRUE)
    if (isTRUE(ok)) {
      message(sprintf("✓ %s loaded", pkg))
    } else {
      message(sprintf("• %s is installed but could not be loaded in this session; start a new R session to verify.", pkg))
    }
  }

  message("\n=== Installation complete ===")
  message("Library location: ", lib_path)
  if (length(installed_now)) {
    message("Note: restart R to verify packages installed/updated in this run: ",
            paste(sort(unique(installed_now)), collapse = ", "))
  }
  invisible(TRUE)
}

# Auto-run if called non-interactively (e.g., via Rscript)
if (!interactive()) {
  ok <- tryCatch(installDependencies(), error = function(e) { message("Installation error: ", e$message); FALSE })
  if (!ok) quit(status = 1)
} else {
  message("Run installDependencies() to start.")
}
