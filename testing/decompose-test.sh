#!/bin/bash
set -e

cp ../src/raster-decompose/target/x86_64-pc-windows-msvc/release/raster-decompose.exe ./release/raster-decompose.exe 

export PATH="/d/Github/ecoClassify/src/raster-decompose/gdal/bin:$PATH"

./release/raster-decompose \
  --input ./data/landsatTest.tif \
  --output ./data/landsatDecomposed.csv \
  --pca-components 3 \
  --texture-kernel 5 \
  --jobs 6
