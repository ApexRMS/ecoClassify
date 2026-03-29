# ecoClassify v2.4.0
# Rocker-based image for running ecoClassify package scripts.
#
# Base: rocker/geospatial:4.4 (R 4.4.x)
# Provides: R 4.4.x, GDAL/GEOS/PROJ system libs, sf, terra, tidyverse, jsonlite.
#
# Build (CPU-only torch backend, ~3 GB image):
#   docker build -t ecoclassify:2.4.0 .
#
# MaxEnt note: maxent.jar cannot be redistributed automatically.
# To enable MaxEnt, copy maxent.jar into the container at build time or mount it:
#   COPY maxent.jar /opt/ecoClassify/java/maxent.jar
# and set MAXENT_JAR=/opt/ecoClassify/java/maxent.jar in the environment,
# OR place it at the path returned by: system.file("java", package="dismo")/maxent.jar

FROM rocker/geospatial:4.4

# --- System dependencies -------------------------------------------------------
# default-jdk: required by rJava (MaxEnt backend)
# libxt-dev:   required by some R graphics devices used in plotting functions
RUN apt-get update && apt-get install -y --no-install-recommends \
        default-jdk \
        libxt-dev \
        libnetcdf-dev \
        libsqlite3-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Register the JDK with R so rJava can find the JVM
RUN R CMD javareconf

# --- R packages not already in rocker/geospatial:4.1 --------------------------
# rocker/geospatial already provides: sf, terra, tidyverse (ggplot2, dplyr,
# tidyr, purrr, tibble, magrittr, etc.), jsonlite, devtools.
# The packages below cover the remainder of ecoClassify's dependency set.
RUN Rscript -e " \
    pkgs <- c( \
        # Core modelling \
        'ranger',        \
        'caret',         \
        # SyncroSim interface \
        'rsyncrosim',    \
        # Parallel / iteration utilities \
        'foreach',       \
        'iterators',     \
        'doParallel',    \
        # Async / generator pattern (CNN data loader) \
        'coro',          \
        # Misc utilities \
        'gtools',        \
        'gower',         \
        # Species distribution modelling (MaxEnt path) \
        'dismo',         \
        'rJava',         \
        'ENMeval',       \
        # Ecological niche tools (installed separately below) \
        # Model evaluation \
        'cvms',          \
        'tidymodels',    \
        # Deep learning (CNN model type) \
        'torch'          \
    ); \
    install.packages(pkgs, repos = 'https://cran.r-project.org', \
                     Ncpus = parallel::detectCores())"



# --- ecospat (separate step to surface install errors) ------------------------
RUN Rscript -e " \
    install.packages('ecospat', repos = 'https://cran.r-project.org', dependencies = TRUE); \
    if (!requireNamespace('ecospat', quietly = TRUE)) \
        stop('ecospat failed to install')"

# --- Torch CPU backend ---------------------------------------------------------
# Downloads and installs the LibTorch C++ library (~1.5 GB).
# Set type='gpu' and use a CUDA base image if GPU support is needed.
ENV TORCH_INSTALL=1
RUN Rscript -e "torch::install_torch(type = 'cpu')"

# --- Package source -----------------------------------------------------------
COPY src/ /opt/ecoClassify/src/

ENV ssim_package_directory=/opt/ecoClassify/src

# Optional: pre-create a directory for maxent.jar
RUN mkdir -p /opt/ecoClassify/java
