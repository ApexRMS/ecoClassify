## Unit tests for src/functions/0.7-spatial-tiling.r

# The previous implementation: a full-size integer matrix filled tile by tile.
reference_tile_matrix <- function(nRows, nCols, rowBreaks, colBreaks, idLookup) {
  nTilesX <- length(colBreaks) - 1L
  m <- matrix(NA_integer_, nRows, nCols)
  for (iy in seq_len(length(rowBreaks) - 1L)) {
    for (ix in seq_len(nTilesX)) {
      id <- idLookup[(iy - 1L) * nTilesX + ix]
      if (is.na(id)) next
      m[(rowBreaks[iy] + 1L):rowBreaks[iy + 1L], (colBreaks[ix] + 1L):colBreaks[ix + 1L]] <- id
    }
  }
  as.integer(t(m)) # raster cell order (row-major)
}

test_that("buildTileIdRaster matches the matrix-based tile mask", {
  source_ecoclassify_r("0.7-spatial-tiling.r")

  configs <- list(
    list(nRows = 10, nCols = 10, nY = 2, nX = 2, skip = integer(0)),
    list(nRows = 37, nCols = 23, nY = 3, nX = 4, skip = c(1L, 6L, 12L)),
    list(nRows = 50, nCols = 7, nY = 5, nX = 1, skip = 3L)
  )

  for (cfg in configs) {
    template <- terra::rast(nrows = cfg$nRows, ncols = cfg$nCols, crs = "EPSG:3857",
                            xmin = 0, xmax = cfg$nCols, ymin = 0, ymax = cfg$nRows)
    rowBreaks <- round(seq(0, cfg$nRows, length.out = cfg$nY + 1L))
    colBreaks <- round(seq(0, cfg$nCols, length.out = cfg$nX + 1L))

    # Contiguous job IDs, skipping some grid positions like empty tiles
    lookup <- rep(NA_integer_, cfg$nY * cfg$nX)
    keep <- setdiff(seq_along(lookup), cfg$skip)
    lookup[keep] <- seq_along(keep)

    out <- buildTileIdRaster(template, rowBreaks, colBreaks, lookup)
    expect_equal(
      as.integer(terra::values(out, mat = FALSE)),
      reference_tile_matrix(cfg$nRows, cfg$nCols, rowBreaks, colBreaks, lookup)
    )
  }
})
