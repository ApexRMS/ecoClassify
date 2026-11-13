## -------------------------------
## ecoClassify - Spatial Tiling Functions
## ApexRMS, November 2026
## -------------------------------

#' Build the spatial multiprocessing tile-ID raster ----
#'
#' @description
#' Assigns every cell the job ID of the tile it falls in, using block-wise
#' terra operations (no full-size in-memory matrix).
#'
#' Tile (iy, ix) covers rows `rowBreaks[iy] + 1` to `rowBreaks[iy + 1]` and
#' columns `colBreaks[ix] + 1` to `colBreaks[ix + 1]`. Its grid position is
#' `(iy - 1) * nTilesX + ix`, and `idLookup[position]` is its job ID (NA for
#' skipped tiles).
#'
#' @param template SpatRaster giving the grid (values are not used).
#' @param rowBreaks,colBreaks Integer tile breaks, starting at 0 and ending at
#'   nrow / ncol.
#' @param idLookup Integer job ID per grid position (length nTilesX * nTilesY).
#' @param filename Optional output file.
#' @param ... Further arguments for writeRaster (e.g. datatype).
#' @return A single-layer SpatRaster of job IDs.
#' @noRd
buildTileIdRaster <- function(template, rowBreaks, colBreaks, idLookup,
                              filename = "", ...) {
  nTilesY <- length(rowBreaks) - 1L
  nTilesX <- length(colBreaks) - 1L
  grid <- terra::rast(template, nlyr = 1)

  # Tile row/column of every cell: intervals (break[i], break[i + 1]]
  rowBin <- terra::classify(
    terra::init(grid, "row"),
    cbind(rowBreaks[-length(rowBreaks)], rowBreaks[-1], seq_len(nTilesY)),
    right = TRUE
  )
  colBin <- terra::classify(
    terra::init(grid, "col"),
    cbind(colBreaks[-length(colBreaks)], colBreaks[-1], seq_len(nTilesX)),
    right = TRUE
  )

  gridPosition <- (rowBin - 1) * nTilesX + colBin
  terra::classify(
    gridPosition,
    cbind(seq_along(idLookup), idLookup),
    filename = filename,
    ...
  )
}
