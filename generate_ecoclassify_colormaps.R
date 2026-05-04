# generate_ecoclassify_colormaps.R
# Pre-populates SyncroSim colormap .txt files for all ecoClassify map variables.
# Probability maps receive a continuous viridis gradient with breakpoints fixed
# at every 0.1 (0.0–1.0). Binary prediction maps receive a 2-color discrete
# colormap (background + presence class).
#
# Dependencies: DBI, RSQLite, viridis
#
# Usage:
#   source("generate_ecoclassify_colormaps.R")
#   generate_ecoclassify_colormaps("my-library.ssim")
#   generate_ecoclassify_colormaps(
#     "my-library.ssim",
#     palette           = "plasma",
#     reverse           = TRUE,
#     bg_color          = "#C8C8C8",
#     presence_color    = "#E65C00",
#     gt_presence_color = "#21908C"
#   )

generate_ecoclassify_colormaps <- function(
  library_path,
  palette           = "viridis",   # palette for continuous probability maps
  reverse           = FALSE,       # reverse palette direction
  bg_color          = "#DCDCDC",   # colour for value 0 (absent / background)
  presence_color    = "#FF4500",   # colour for value 1 (presence) on prediction maps
  gt_presence_color = "#21908C"    # colour for value 1 on the ground truth map (teal = viridis midpoint)
) {
  # ── Validate inputs ───────────────────────────────────────────────────────────
  if (!file.exists(library_path)) {
    stop("Library file not found: ", library_path)
  }

  # ── Known variable types ──────────────────────────────────────────────────────
  # Maps ALL-CAPS SyncroSim variable name → list(fname, display_name, is_continuous)
  VAR_INFO <- list(
    ECOCLASSIFY_GROUNDTRUTHMAP                    = list("GroundTruthMap",                    "Ground Truth",                   FALSE),
    ECOCLASSIFY_PROBABILITYMAP                    = list("ProbabilityMap",                    "Probability",                    TRUE),
    ECOCLASSIFY_PREDICTEDUNFILTEREDMAP            = list("PredictedUnfilteredMap",            "Predicted",                      FALSE),
    ECOCLASSIFY_PREDICTEDFILTEREDMAP              = list("PredictedFilteredMap",              "Predicted Filtered",             FALSE),
    ECOCLASSIFY_PREDICTEDUNFILTEREDRESTRICTEDMAP  = list("PredictedUnfilteredRestrictedMap",  "Predicted Restricted",           FALSE),
    ECOCLASSIFY_PREDICTEDFILTEREDRESTRICTEDMAP    = list("PredictedFilteredRestrictedMap",    "Predicted Filtered Restricted",  FALSE),
    ECOCLASSIFY_CLASSIFIEDPROBABILITYMAP          = list("ClassifiedProbabilityMap",          "Classified Probability",         TRUE),
    ECOCLASSIFY_CLASSIFIEDUNFILTEREDMAP           = list("ClassifiedUnfilteredMap",           "Classified",                     FALSE),
    ECOCLASSIFY_CLASSIFIEDFILTEREDMAP             = list("ClassifiedFilteredMap",             "Classified Filtered",            FALSE),
    ECOCLASSIFY_CLASSIFIEDUNFILTEREDRESTRICTEDMAP = list("ClassifiedUnfilteredRestrictedMap", "Classified Restricted",          FALSE),
    ECOCLASSIFY_CLASSIFIEDFILTEREDRESTRICTEDMAP   = list("ClassifiedFilteredRestrictedMap",   "Classified Filtered Restricted", FALSE)
  )

  # ── Resolve output directory ──────────────────────────────────────────────────
  data_dir <- paste0(library_path, ".data/Project-1/")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message("Created directory: ", data_dir)
  }

  # ── Probability map breakpoints: fixed at every 0.1 from 0 to 1 ──────────────
  breakpoints <- seq(0, 1, by = 0.1)   # always 0.0, 0.1, 0.2, ..., 1.0
  n_breaks    <- length(breakpoints)   # 11

  # ── Generate continuous palette colours ──────────────────────────────────────
  viridis_options <- c(
    "viridis", "plasma", "magma", "inferno", "cividis", "turbo", "rocket", "mako"
  )
  direction <- if (reverse) -1L else 1L

  hex_continuous <- tryCatch(
    {
      if (tolower(palette) %in% viridis_options) {
        if (!requireNamespace("viridis", quietly = TRUE)) {
          stop("Package 'viridis' is required. Install with: install.packages('viridis')")
        }
        viridis::viridis(n_breaks, option = tolower(palette), direction = direction)
      } else {
        cols <- grDevices::hcl.colors(n_breaks, palette = palette)
        if (reverse) rev(cols) else cols
      }
    },
    error = function(e) {
      stop(
        "Unrecognised palette '", palette, "'.\n",
        "Viridis options: ", paste(viridis_options, collapse = ", "), "\n",
        "Or use any name from grDevices::hcl.pals().\n",
        "Original error: ", conditionMessage(e)
      )
    }
  )
  rgb_continuous <- grDevices::col2rgb(hex_continuous) # 3 x n_breaks matrix

  # ── Parse binary colours ──────────────────────────────────────────────────────
  rgb_bg          <- grDevices::col2rgb(bg_color)
  rgb_presence    <- grDevices::col2rgb(presence_color)
  rgb_gt_presence <- grDevices::col2rgb(gt_presence_color)

  # ── Connect to SQLite database ────────────────────────────────────────────────
  if (
    !requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("RSQLite", quietly = TRUE)
  ) {
    stop(
      "Packages 'DBI' and 'RSQLite' are required. Install with:\n",
      "  install.packages(c('DBI', 'RSQLite'))"
    )
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), library_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)

  # ── Read core_Map (for colormap generation) ───────────────────────────────────
  # If no maps are defined yet the colormap loop is skipped, but visualization
  # creation still runs below.
  maps <- if ("core_Map" %in% tables) {
    DBI::dbGetQuery(con, "SELECT MapId, Name, Criteria FROM core_Map")
  } else {
    data.frame(MapId = integer(0), Name = character(0), Criteria = character(0))
  }

  # ── Look up target class label (used for binary map row labels) ───────────────
  # Use the most common label across all scenarios; fall back to "Presence".
  presence_label   <- "Presence"
  background_label <- "Background"
  if ("ecoClassify_TargetClassOptions" %in% tables) {
    labels <- DBI::dbGetQuery(
      con,
      "SELECT targetClassLabel FROM ecoClassify_TargetClassOptions
       WHERE targetClassLabel IS NOT NULL AND targetClassLabel != ''"
    )$targetClassLabel
    if (length(labels) > 0) {
      presence_label <- names(sort(table(labels), decreasing = TRUE))[1]
    }
  }

  # ── Process each map ──────────────────────────────────────────────────────────
  written <- character(0)
  skipped <- character(0)

  for (i in seq_len(nrow(maps))) {
    map_id   <- maps$MapId[i]
    criteria <- maps$Criteria[i]

    if (is.na(criteria) || criteria == "") next

    variables <- strsplit(criteria, "\\|")[[1]]
    variables <- variables[variables != "-1" & nzchar(variables)]

    for (var_raw in variables) {
      # Strip any trailing metadata suffix (e.g. "-ITEMID-1-ITEMSRC-...")
      var_clean <- sub("-ITEMID-.*$", "", var_raw)

      info <- VAR_INFO[[var_clean]]
      if (is.null(info)) {
        skipped <- c(skipped, sprintf("map%d: %s (unknown variable)", map_id, var_clean))
        next
      }

      fname_component <- info[[1]]
      display_name    <- info[[2]]
      is_continuous   <- info[[3]]

      filename <- sprintf("colormap_map%d_ecoclassify_%s.txt", map_id, fname_component)
      filepath <- file.path(data_dir, filename)

      header <- sprintf(
        "# Syncrosim Generated %s Color Map (QGIS-compatible),,,,,",
        display_name
      )

      if (is_continuous) {
        lines <- c(header, "INTERPOLATION:DISCRETE")
        for (j in seq_along(breakpoints)) {
          val <- breakpoints[j]
          R   <- rgb_continuous[1, j]
          G   <- rgb_continuous[2, j]
          B   <- rgb_continuous[3, j]
          lines <- c(lines, sprintf("%.4f,%d,%d,%d,255,%.1f", val, R, G, B, val))
        }
      } else {
        rgb_p <- if (var_clean == "ECOCLASSIFY_GROUNDTRUTHMAP") rgb_gt_presence else rgb_presence
        lines <- c(header, "INTERPOLATION:DISCRETE")
        lines <- c(lines, sprintf(
          "0.0000,%d,%d,%d,255,%s",
          rgb_bg[1], rgb_bg[2], rgb_bg[3], background_label
        ))
        lines <- c(lines, sprintf(
          "1.0000,%d,%d,%d,255,%s",
          rgb_p[1], rgb_p[2], rgb_p[3], presence_label
        ))
      }

      writeLines(lines, filepath)
      written <- c(written, filename)
    }
  }

  # ── Create visualizations if not already present ─────────────────────────────

  proj_id <- DBI::dbGetQuery(con, "SELECT ProjectId FROM core_Project LIMIT 1")$ProjectId[1]

  # Returns the next available integer ID for a table
  next_id <- function(tbl, id_col) {
    as.integer(DBI::dbGetQuery(
      con, sprintf("SELECT COALESCE(MAX(%s), 0) + 1 AS n FROM [%s]", id_col, tbl)
    )$n)
  }

  # Inserts the 3 standard facets for a new map or image
  add_facets <- function(facet_tbl, facet_id_col, parent_id_col, parent_id) {
    # Variable 0: rows    (ArrangeIn=1, Order=1)
    # Variable 1: columns (ArrangeIn=1, Order=2)
    # Variable 2: overlay (ArrangeIn=0, Order=3)
    for (v in 0:2) {
      DBI::dbExecute(con, sprintf(
        'INSERT INTO [%s] ([%s], ProjectId, [%s], Variable, ArrangeIn, "Order")
         VALUES (?, ?, ?, ?, ?, ?)',
        facet_tbl, facet_id_col, parent_id_col
      ), params = list(
        next_id(facet_tbl, facet_id_col),
        proj_id, parent_id, v,
        if (v == 2L) 0L else 1L,
        v + 1L
      ))
    }
  }

  viz_created <- character(0)
  viz_skipped <- character(0)

  # Warn and skip visualization creation if the required core tables are absent.
  # This can happen with very old or non-standard libraries.
  required_viz_tables <- c("core_Chart", "core_Map", "core_MapFacet",
                            "core_Image", "core_ImageFacet")
  missing_tables <- setdiff(required_viz_tables, tables)
  if (length(missing_tables) > 0) {
    message("Skipping visualization creation — required tables not found: ",
            paste(missing_tables, collapse = ", "))
  } else {

  DBI::dbWithTransaction(con, {

    # ── Chart: Model Evaluation Metrics ────────────────────────────────────────
    existing_charts <- DBI::dbGetQuery(
      con, "SELECT Name FROM core_Chart WHERE ProjectId = ?", params = list(proj_id)
    )$Name

    if (!("Model Evaluation Metrics" %in% existing_charts)) {
      DBI::dbExecute(con,
        "INSERT INTO core_Chart
           (ChartId, ProjectId, Name, ChartType, IterationType, ErrorBarType,
            ChartShowLegend,
            ChartLegendShowScenarioName, ChartLegendShowScenarioId,
            ChartLegendShowStageName, ChartLegendShowTimestamp,
            PanelYAxisSameScale, PanelYAxisMinZero,
            PanelShowToolTips, PanelShowTitles, PanelFixedYAxisIntervals,
            PanelNoDataAsZero, PanelShowDataPoints, PanelShowDataPointsOnly,
            PanelShowNoDataPanels, PanelLineThickness,
            PanelYAxisNumberStyle, PanelYAxisDecimalPlaces, PanelYAxisShowThousandsSep,
            PanelXAxisNumberStyle, PanelXAxisDecimalPlaces, PanelXAxisShowThousandsSep,
            IsPublish, IsReadOnly, Criteria)
         VALUES (?, ?, 'Model Evaluation Metrics', 1, 0, 0,
                 -1,
                 -1, -1,
                 0, 0,
                 0, -1,
                 -1, -1, -1,
                 -1, 0, 0,
                 0, 2,
                 0, 2, -1,
                 0, 0, 0,
                 0, 0,
                 'ECOCLASSIFY_METRICSACCURACY|ECOCLASSIFY_METRICSPRECISION|ECOCLASSIFY_METRICSRECALL|ECOCLASSIFY_METRICSSPECIFICITY|ECOCLASSIFY_METRICSF1|ECOCLASSIFY_METRICSAUC')",
        params = list(next_id("core_Chart", "ChartId"), proj_id)
      )
      viz_created <- c(viz_created, "Chart: Model Evaluation Metrics")
    } else {
      viz_skipped <- c(viz_skipped, "Chart: Model Evaluation Metrics")
    }

    # ── Maps: Training and Predicting ──────────────────────────────────────────
    existing_maps <- DBI::dbGetQuery(
      con, "SELECT Name FROM core_Map WHERE ProjectId = ?", params = list(proj_id)
    )$Name

    maps_to_create <- list(
      list(
        name     = "Training",
        criteria = paste(
          "ECOCLASSIFY_GROUNDTRUTHMAP",
          "ECOCLASSIFY_PROBABILITYMAP",
          "ECOCLASSIFY_PREDICTEDUNFILTEREDMAP",
          "ECOCLASSIFY_PREDICTEDFILTEREDMAP",
          "ECOCLASSIFY_PREDICTEDUNFILTEREDRESTRICTEDMAP",
          "ECOCLASSIFY_PREDICTEDFILTEREDRESTRICTEDMAP",
          sep = "|"
        )
      ),
      list(
        name     = "Predicting",
        criteria = paste(
          "ECOCLASSIFY_CLASSIFIEDPROBABILITYMAP",
          "ECOCLASSIFY_CLASSIFIEDUNFILTEREDMAP",
          "ECOCLASSIFY_CLASSIFIEDUNFILTEREDRESTRICTEDMAP",
          "ECOCLASSIFY_CLASSIFIEDFILTEREDMAP",
          "ECOCLASSIFY_CLASSIFIEDFILTEREDRESTRICTEDMAP",
          sep = "|"
        )
      )
    )

    for (m in maps_to_create) {
      if (!(m$name %in% existing_maps)) {
        mid <- next_id("core_Map", "MapId")
        DBI::dbExecute(con,
          "INSERT INTO core_Map
             (MapId, ProjectId, Name,
              ShowScenarioName, ShowScenarioId, ShowIndicatorLabel,
              ShowTimestepLabel, ShowTimestepPrefix,
              NestLabels, PanelSize, IsPublish, IsReadOnly, Criteria)
           VALUES (?, ?, ?, -1, -1, -1, -1, -1, -1, 100, 0, 0, ?)",
          params = list(mid, proj_id, m$name, m$criteria)
        )
        add_facets("core_MapFacet", "MapFacetId", "MapId", mid)
        viz_created <- c(viz_created, paste("Map:", m$name))
      } else {
        viz_skipped <- c(viz_skipped, paste("Map:", m$name))
      }
    }

    # ── Images ─────────────────────────────────────────────────────────────────
    existing_images <- DBI::dbGetQuery(
      con, "SELECT Name FROM core_Image WHERE ProjectId = ?", params = list(proj_id)
    )$Name

    images_to_create <- list(
      list(name = "Variable Importance Plot",     criteria = "ECOCLASSIFY_VARIABLEIMPORTANCEPLOT"),
      list(name = "Confusion Matrix",             criteria = "ECOCLASSIFY_CONFUSIONMATRIXPLOTOUTPUTIMAGE"),
      list(name = "Layer Histogram and Response", criteria = "ECOCLASSIFY_LAYERHISTOGRAMPLOTOUTPUTIMAGE"),
      list(name = "RGB - Training",               criteria = "ECOCLASSIFY_TRAININGRGBIMAGEPLOTS"),
      list(name = "RGB - Predicting",             criteria = "ECOCLASSIFY_CLASSIFIEDRGBIMAGEPLOTS")
    )

    for (img in images_to_create) {
      if (!(img$name %in% existing_images)) {
        iid <- next_id("core_Image", "ImageId")
        DBI::dbExecute(con,
          "INSERT INTO core_Image
             (ImageId, ProjectId, Name,
              ShowScenarioName, ShowScenarioId, ShowIndicatorLabel,
              ShowTimestepLabel, ShowTimestepPrefix,
              NestLabels, PanelSize, IsPublish, IsReadOnly, Criteria)
           VALUES (?, ?, ?, -1, -1, -1, -1, -1, -1, 100, 0, 0, ?)",
          params = list(iid, proj_id, img$name, img$criteria)
        )
        add_facets("core_ImageFacet", "ImageFacetId", "ImageId", iid)
        viz_created <- c(viz_created, paste("Image:", img$name))
      } else {
        viz_skipped <- c(viz_skipped, paste("Image:", img$name))
      }
    }

  }) # end dbWithTransaction

  } # end if required tables present

  # ── Summary ───────────────────────────────────────────────────────────────────
  cat("\n=== ecoClassify Colormap Generation Complete ===\n")
  cat(sprintf(
    "Probability palette: %s | Breakpoints: 0.0–1.0 (every 0.1) | Reversed: %s\n",
    palette, reverse
  ))
  cat(sprintf(
    "Binary maps — background: %s  |  presence: %s  |  ground truth presence: %s  |  class label: '%s'\n",
    bg_color, presence_color, gt_presence_color, presence_label
  ))
  cat(sprintf("Output directory: %s\n\n", data_dir))

  if (length(written) > 0) {
    cat(sprintf("Colormap files written (%d):\n", length(written)))
    for (f in written) cat("  [OK]", f, "\n")
  } else {
    cat("No colormap files written.\n")
  }

  if (length(skipped) > 0) {
    cat(sprintf("\nColormap files skipped (%d):\n", length(skipped)))
    for (v in skipped) cat("  [--]", v, "\n")
  }

  if (length(viz_created) > 0) {
    cat(sprintf("\nVisualizations created (%d):\n", length(viz_created)))
    for (v in viz_created) cat("  [+]", v, "\n")
  }

  if (length(viz_skipped) > 0) {
    cat(sprintf("\nVisualizations already present (%d):\n", length(viz_skipped)))
    for (v in viz_skipped) cat("  [=]", v, "\n")
  }

  invisible(list(written = written, skipped = skipped,
                 viz_created = viz_created, viz_skipped = viz_skipped))
}

generate_ecoclassify_colormaps(
  "C:/Users/HannahAdams/Downloads/ecoclassify-example.ssim",
  palette           = "viridis",
  bg_color          = "#DCDCDC",
  presence_color    = "#FF4500",
  gt_presence_color = "#21908C"
)
