# 0.0-installDependencies.r  — ecoClassify
# Robust installer for conda envs (Windows/Linux/macOS)
# - fixes .libPaths() to the active conda env (no stray quoted entries)
# - pre-updates 'terra' on R 4.1.* to avoid ecospat/raster DLL swaps
# - verifies by presence/version only (no heavy attach in-session)
# - optionally attaches a MINIMAL set (e.g., dplyr/magrittr) at the end

# --- LIB GUARD: force installs into the active conda env library ---
fix_lib_paths <- function() {
  cp <- Sys.getenv("CONDA_PREFIX", unset = "")
  env_lib <- if (nzchar(cp)) file.path(cp, "Lib", "R", "library") else .libPaths()[1]

  # Strip rogue quotes and normalize
  clean <- function(p) normalizePath(gsub('^"+|"+$|^\\\'+|\\\'+$', "", p),
                                     winslash = "/", mustWork = FALSE)
  env_lib <- clean(env_lib)

  # Ensure it exists
  if (!dir.exists(env_lib)) dir.create(env_lib, recursive = TRUE, showWarnings = FALSE)

  # **Neutralize any external library injections BEFORE setting .libPaths**
  Sys.setenv(R_LIBS_SITE = "")   # <- stop site lib from being auto-prepended
  Sys.setenv(R_LIBS      = "")   # <- clear any custom lib chain
  Sys.setenv(R_LIBS_USER = env_lib)

  # Disable user profiles that could re-add paths
  if (.Platform$OS.type == "windows") {
    Sys.setenv(R_PROFILE_USER = "NUL", R_ENVIRON_USER = "NUL")
  } else {
    Sys.setenv(R_PROFILE_USER = "/dev/null", R_ENVIRON_USER = "/dev/null")
  }

  # Set the library paths to exactly the env lib
  .libPaths(env_lib)

  # Final sanity (+ ensure writability)
  if (file.access(.libPaths()[1], 2) != 0) {
    stop("Active library not writable: ", .libPaths()[1])
  }

  # Quiet Windows timezone gripe
  if (nzchar(Sys.getenv("TZ", "")) == FALSE) Sys.setenv(TZ = "UTC")

  message("Using library: ", .libPaths()[1])
}
# --- end LIB GUARD ---

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

# small utility to quietly attach a short list
attach_minimal_pkgs <- function(pkgs) {
  for (p in pkgs) {
    suppressPackageStartupMessages(
      try(library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE), silent = TRUE)
    )
    if (p %in% (.packages())) {
      message(sprintf("✓ %s attached", p))
    } else {
      message(sprintf("• %s could not be attached in this session (but is installed).", p))
    }
  }
}

# -------------------------
# Main
# -------------------------
installDependencies <- function(attach_minimal = c("magrittr","dplyr")) {
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
    ok <- FALSE
    try(ok <- torch::torch_is_installed(), silent = TRUE)
    if (isFALSE(ok)) {
      torch::install_torch()
      installed_now <- unique(c(installed_now, "torch"))
    }
  } else {
    torch::install_torch()
  }

  # -------------------------
  # Verification (lightweight, no attaches)
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
    } else {
      message(sprintf("✓ %s present (version %s)", pkg, pkg_version(pkg, lib_path)))
    }
  }

  message("\n=== Installation complete ===")
  message("Library location: ", lib_path)
  if (length(installed_now)) {
    message("Note: restart R to verify packages installed/updated in this run: ",
            paste(sort(unique(installed_now)), collapse = ", "))
  }

  # --- Attach only the MINIMAL set needed immediately (e.g., pull(), pipes) ---
  if (!is.null(attach_minimal) && length(attach_minimal)) {
    message("\n=== Attaching minimal set ===")
    attach_minimal_pkgs(attach_minimal)
  }

  Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")

  invisible(TRUE)
}

# Auto-run if called non-interactively (e.g., via Rscript)
if (!interactive()) {
  ok <- tryCatch(installDependencies(), error = function(e) { message("Installation error: ", e$message); FALSE })
  if (!ok) quit(status = 1)
} else {
  message("Run installDependencies() to start.")
}
