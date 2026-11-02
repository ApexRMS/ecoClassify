# fix_publish_display_names.R
#
# Patches VariableDisplayName values in MapPanel.json inside a SyncroSim
# publish folder so that each binary map variable has a unique label.
# SyncroSim Cloud uses VariableDisplayName as the layer-selector key, so
# duplicate names (e.g. "Binary" for all binary maps) cause only one to
# appear in the dropdown.
#
# Usage:
#   Rscript tools/fix_publish_display_names.R <publish_folder>
#
# <publish_folder> should be the inner folder that contains MapPanel.json,
# e.g. "C:/path/to/MyLibrary.ssim.publish/MyLibrary"
#
# If no argument is supplied the script exits with a usage message.

# ── Display name lookup ───────────────────────────────────────────────────────
DISPLAY_NAMES <- list(
  # Predicting (ClassifiedRasterOutput)
  ecoClassify_ClassifiedProbabilityMap          = "Probability",
  ecoClassify_ClassifiedUnfilteredMap           = "Binary",
  ecoClassify_ClassifiedUnfilteredRestrictedMap = "Binary (Restricted)",
  ecoClassify_ClassifiedFilteredMap             = "Binary (Filtered)",
  ecoClassify_ClassifiedFilteredRestrictedMap   = "Binary (Restricted and Filtered)",
  # Training (RasterOutput)
  ecoClassify_ProbabilityMap                    = "Probability",
  ecoClassify_GroundTruthMap                    = "User Classification",
  ecoClassify_PredictedUnfilteredMap            = "Binary",
  ecoClassify_PredictedUnfilteredRestrictedMap  = "Binary (Restricted)",
  ecoClassify_PredictedFilteredMap              = "Binary (Filtered)",
  ecoClassify_PredictedFilteredRestrictedMap    = "Binary (Restricted and Filtered)"
)

# ── Parse argument ────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  cat("Usage: Rscript tools/fix_publish_display_names.R <publish_folder>\n")
  cat("  publish_folder: path to the inner publish directory containing MapPanel.json\n")
  quit(status = 1)
}

publish_dir <- args[1]
map_panel_path <- file.path(publish_dir, "MapPanel.json")

if (!file.exists(map_panel_path)) {
  stop("MapPanel.json not found in: ", publish_dir)
}

# ── Read, patch, write ────────────────────────────────────────────────────────
panel <- jsonlite::fromJSON(map_panel_path, simplifyVector = FALSE)

changed <- 0L
for (i in seq_along(panel)) {
  var <- panel[[i]]$Variable
  correct_name <- DISPLAY_NAMES[[var]]
  if (!is.null(correct_name) && !identical(panel[[i]]$VariableDisplayName, correct_name)) {
    old_name <- panel[[i]]$VariableDisplayName
    panel[[i]]$VariableDisplayName <- correct_name
    cat(sprintf("[fixed] %s: '%s' -> '%s'\n", var, old_name, correct_name))
    changed <- changed + 1L
  }
}

if (changed == 0L) {
  cat("All display names are already correct — no changes made.\n")
} else {
  jsonlite::write_json(panel, map_panel_path, auto_unbox = TRUE, pretty = FALSE)
  cat(sprintf("\nUpdated %d entry/entries in %s\n", changed, map_panel_path))
}
