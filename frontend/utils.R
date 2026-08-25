# ============================================================================
# utils.R
# ============================================================================
"
Core color/shape mappings and utility functions for SNV Benchmarking Dashboard.

Main components:
- Color and shape mappings for visualizations
- Helper functions for data processing and JSON parameter handling
"

# ============================================================================
# COLOR AND SHAPE MAPPINGS
# ============================================================================

# --- Persisted reserve assignments ------------------------------------------
# Once a tech/caller is assigned a reserve color/pch code, that assignment is
# written here and reused forever after -- it is never recomputed from live
# enum state, so adding a new tech/caller can never shift an existing one's
# color/shape. See docs/tasks/tech-caller-color-persistence-and-gradient-autoextend.md
#
# Lives under data/, not frontend/: in the container build (Containerfile),
# frontend/ is baked into the image and is wiped on every redeploy, while
# /data is the mounted persistent volume (same place config.py keeps
# benchmarking.db). Mirrors config.py's own IS_CONTAINER detection so this
# stays correct in both local dev and the container, without needing a
# reticulate round-trip to Python's config module.
IS_CONTAINER <- dir.exists("/app") && dir.exists("/app/backend")
COLOR_SHAPE_ASSIGNMENTS_FILE <- if (IS_CONTAINER) {
  "/data/color_shape_assignments.json"
} else {
  file.path("..", "data", "color_shape_assignments.json")
}

load_color_shape_assignments <- function() {
  empty <- list(technologies = list(), callers = list())
  if (!file.exists(COLOR_SHAPE_ASSIGNMENTS_FILE)) return(empty)
  tryCatch({
    raw <- jsonlite::fromJSON(COLOR_SHAPE_ASSIGNMENTS_FILE, simplifyVector = TRUE)
    list(
      technologies = if (!is.null(raw$technologies)) as.list(raw$technologies) else list(),
      callers = if (!is.null(raw$callers)) as.list(raw$callers) else list()
    )
  }, error = function(e) empty)
}

save_color_shape_assignments <- function(assignments) {
  tryCatch({
    jsonlite::write_json(assignments, COLOR_SHAPE_ASSIGNMENTS_FILE, auto_unbox = TRUE, pretty = TRUE)
  }, error = function(e) {
    warning("Could not persist color/shape assignments: ", conditionMessage(e))
  })
}

# --- Technology colors ------------------------------------------------------

# Curated colors — Okabe-Ito palette (colorblind-safe under all common CVD types)
technology_colors <- c(
  "ILLUMINA" = "#D55E00",
  "PACBIO"   = "#CC79A7",
  "ONT"      = "#56B4E9",
  "MGI"      = "#009E73",
  "10X"      = "#E69F00",
  "ULTIMA"   = "#0072B2",
  "Unknown"  = "#999999"
)

# Reserve colors for new technologies. First 3 are the remaining colors from
# the Okabe-Ito CVD-safe palette; the rest are additional high-contrast colors
# (not formally CVD-vetted, but chosen to stay visually distinct on a white
# plot background) used only once the CVD-safe entries run out.
TECHNOLOGY_COLOR_RESERVE <- c(
  "#0072B2", "#F0E442", "#000000",             # remaining Okabe-Ito (CVD-safe)
  "#8B4513", "#4B0082", "#008080", "#B8860B",  # extended, high-contrast
  "#556B2F", "#8B0000", "#2F4F4F"
)

extend_technology_colors <- function(base_colors, known_techs) {
  fallback <- base_colors[["Unknown"]]
  curated <- base_colors[names(base_colors) != "Unknown"]

  assignments <- load_color_shape_assignments()
  persisted <- assignments$technologies
  persisted <- persisted[names(persisted) %in% setdiff(known_techs, names(curated))]

  missing <- sort(setdiff(known_techs, c(names(curated), names(persisted))))
  if (length(missing) > 0) {
    used <- unique(c(unname(curated), unlist(persisted, use.names = FALSE)))
    pool <- TECHNOLOGY_COLOR_RESERVE[!TECHNOLOGY_COLOR_RESERVE %in% used]
    if (length(pool) < length(missing)) pool <- rep(TECHNOLOGY_COLOR_RESERVE, length.out = length(missing))
    persisted <- c(persisted, setNames(as.list(pool[seq_along(missing)]), missing))
    assignments$technologies <- persisted
    save_color_shape_assignments(assignments)
  }

  reserve_colors <- if (length(persisted) > 0) setNames(unlist(persisted), names(persisted)) else character(0)
  c(curated, reserve_colors, "Unknown" = fallback)
}

technology_colors <- extend_technology_colors(technology_colors, enums$VALID_TECHNOLOGIES)

# --- Caller shapes -----------------------------------------------------------

caller_shapes <- c(
  "DEEPVARIANT" = 16, "CLAIR3" = 15, "DRAGEN" = 18, "GATK3" = 17,
  "GATK4" = 4, "LONGRANGER" = 3, "MEGABOLT" = 10, "NANOCALLER" = 12,
  "PARABRICK" = 1, "PEPPER" = 0, "Unknown" = 4
)

shape_symbols <- c(
  "16" = "●", "17" = "▲", "15" = "■", "18" = "◆", "4" = "✕",
  "3" = "+", "10" = "⊕", "12" = "⊞", "1" = "○", "0" = "□"
)

# Reserve pch codes + matching legend symbols for new callers.
RESERVE_CALLER_SHAPES <- c(
  "2" = "△", "5" = "◇", "6" = "▽", "7" = "⊠", "8" = "✶",
  "9" = "◈", "11" = "⚹", "13" = "⊗", "14" = "⬠"
)

extend_caller_shapes <- function(base_shapes, known_callers) {
  fallback <- base_shapes[["Unknown"]]
  curated <- base_shapes[names(base_shapes) != "Unknown"]

  assignments <- load_color_shape_assignments()
  persisted <- assignments$callers
  persisted <- persisted[names(persisted) %in% setdiff(known_callers, names(curated))]

  missing <- sort(setdiff(known_callers, c(names(curated), names(persisted))))
  reserve_codes <- names(RESERVE_CALLER_SHAPES)
  if (length(missing) > 0) {
    used <- as.character(c(unname(curated), unlist(persisted, use.names = FALSE)))
    pool <- reserve_codes[!reserve_codes %in% used]
    if (length(pool) < length(missing)) pool <- rep(reserve_codes, length.out = length(missing))
    persisted <- c(persisted, setNames(as.list(as.integer(pool[seq_along(missing)])), missing))
    assignments$callers <- persisted
    save_color_shape_assignments(assignments)
  }

  reserve_shapes <- if (length(persisted) > 0) setNames(as.integer(unlist(persisted)), names(persisted)) else integer(0)
  c(curated, reserve_shapes, "Unknown" = fallback)
}

caller_shapes <- extend_caller_shapes(caller_shapes, enums$VALID_CALLERS)

shape_symbols <- c(shape_symbols, RESERVE_CALLER_SHAPES[
  setdiff(names(RESERVE_CALLER_SHAPES), names(shape_symbols))
])

# Technology-caller gradient combinations for stratified plots
tech_caller_colors <- c(
  # ILLUMINA family (Red variations)
  "ILLUMINA-DEEPVARIANT" = "#F42D1F",
  "ILLUMINA-CLAIR3" = "#F54A3E",
  "ILLUMINA-DRAGEN" = "#F6584E",
  "ILLUMINA-GATK3" = "#F53B2F",
  "ILLUMINA-GATK4" = "#F8756C",
  "ILLUMINA-LONGRANGER" = "#F8847C",
  "ILLUMINA-MEGABOLT" = "#F9938B",
  "ILLUMINA-NANOCALLER" = "#FAA19B",
  "ILLUMINA-PARABRICK" = "#FAB0AA",
  "ILLUMINA-PEPPER" = "#FBBEBA",
  
  # PACBIO family (Purple variations)
  "PACBIO-DEEPVARIANT" = "#A42AFE",
  "PACBIO-CLAIR3" = "#B24BFF",
  "PACBIO-DRAGEN" = "#B95BFF",
  "PACBIO-GATK3" = "#AB3BFF",
  "PACBIO-GATK4" = "#C77CFF",
  "PACBIO-LONGRANGER" = "#CD8CFE",
  "PACBIO-MEGABOLT" = "#D49CFE",
  "PACBIO-NANOCALLER" = "#DBACFF",
  "PACBIO-PARABRICK" = "#E2BCFF",
  "PACBIO-PEPPER" = "#E9CCFE",
  
  # ONT family (Cyan variations)
  "ONT-DEEPVARIANT" = "#006F72",
  "ONT-CLAIR3" = "#008F93",
  "ONT-DRAGEN" = "#009FA3",
  "ONT-GATK3" = "#007F83",
  "ONT-GATK4" = "#00BEC4",
  "ONT-LONGRANGER" = "#00CED4",
  "ONT-MEGABOLT" = "#00DEE4",
  "ONT-NANOCALLER" = "#00EEF4",
  "ONT-PARABRICK" = "#05F8FF",
  "ONT-PEPPER" = "#16F9FF",
  
  # MGI family (Green variations)
  "MGI-DEEPVARIANT" = "#486600",
  "MGI-CLAIR3" = "#597D00",
  "MGI-DRAGEN" = "#648D00",
  "MGI-GATK3" = "#4D6D00",
  "MGI-GATK4" = "#7CAE00",
  "MGI-LONGRANGER" = "#87BE00",
  "MGI-MEGABOLT" = "#93CE00",
  "MGI-NANOCALLER" = "#9EDE00",
  "MGI-PARABRICK" = "#AAEE00",
  "MGI-PEPPER" = "#B5FF00",
  
  # 10X family (Orange variations)
  "10X-DEEPVARIANT" = "#AD7000",
  "10X-CLAIR3" = "#CE8500",
  "10X-DRAGEN" = "#DE9000",
  "10X-GATK3" = "#BE7B00",
  "10X-GATK4" = "#FFA500",
  "10X-LONGRANGER" = "#FEAA10",
  "10X-MEGABOLT" = "#FEB020",
  "10X-NANOCALLER" = "#FFB630",
  "10X-PARABRICK" = "#FFBB40",
  "10X-PEPPER" = "#FFC151",

  # ULTIMA family (Blue variations)
  "ULTIMA-DEEPVARIANT" = "#0072B2",
  "ULTIMA-CLAIR3" = "#177EB9",
  "ULTIMA-DRAGEN" = "#2E8BC0",
  "ULTIMA-GATK3" = "#4598C7",
  "ULTIMA-GATK4" = "#5CA5CE",
  "ULTIMA-LONGRANGER" = "#73B2D5",
  "ULTIMA-MEGABOLT" = "#8BBEDB",
  "ULTIMA-NANOCALLER" = "#A2CBE3",
  "ULTIMA-PARABRICK" = "#B9D8EA",
  "ULTIMA-PEPPER" = "#D0E5F1"
)

# Fixed lightness-fraction table for the auto-extend ramp below: the tech's
# true base color sits at fraction 0.5 (rank 1, DEEPVARIANT's pch code); every
# later pch code -- in the order curated then reserve pch codes were defined
# above -- alternates lighter/darker and moves further from 0.5. So the
# earliest-known callers render closest to the tech's real color, and each
# additional one (reserve callers, or any future ones) pushes further toward
# the light/dark extremes -- and since it's a fixed function of pch code alone,
# a given caller's shade never moves once generated, no matter what's added later.
PCH_SHADE_ORDER <- c(16, 15, 18, 17, 4, 3, 10, 12, 1, 0, 2, 5, 6, 7, 8, 9, 11, 13, 14)
PCH_SHADE_FRACTION <- local({
  offsets <- numeric(length(PCH_SHADE_ORDER))
  step <- 0
  for (i in seq_along(PCH_SHADE_ORDER)) {
    if (i == 1) next
    if (i %% 2 == 0) step <- step + 0.05
    offsets[i] <- if (i %% 2 == 0) step else -step
  }
  setNames(pmin(pmax(0.5 + offsets, 0.05), 0.95), as.character(PCH_SHADE_ORDER))
})

# Auto-extend to any tech x caller combo not in the curated 50 above (e.g. a
# newly-added technology or caller). Shade lightness is a fixed function of
# the caller's own (now permanently stable) pch code via PCH_SHADE_FRACTION
# above, not of how many techs/callers currently exist -- so a given combo's
# shade never shifts once generated, and never needs a separate persistence
# file of its own.
extend_tech_caller_colors <- function(base_map, technology_colors, caller_shapes) {
  techs <- setdiff(names(technology_colors), "Unknown")
  callers <- setdiff(names(caller_shapes), "Unknown")
  generated <- list()
  for (tech in techs) {
    base_hex <- technology_colors[[tech]]
    ramp <- colorRampPalette(c("#000000", base_hex, "#FFFFFF"))(101)
    for (caller in callers) {
      key <- paste0(tech, "-", caller)
      if (!(key %in% names(base_map))) {
        pch_key <- as.character(caller_shapes[[caller]])
        fraction <- if (pch_key %in% names(PCH_SHADE_FRACTION)) PCH_SHADE_FRACTION[[pch_key]] else 0.5
        generated[[key]] <- ramp[round(fraction * 100) + 1]
      }
    }
  }
  c(base_map, unlist(generated))
}

tech_caller_colors <- extend_tech_caller_colors(tech_caller_colors, technology_colors, caller_shapes)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Null coalescing operator ( for safe value handling)
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x) || x == "") y else x
}

# Convert R data to JSON format for Python interface
json_param <- function(data) {
  if (is.null(data) || length(data) == 0) {
    return("[]")
  }
  jsonlite::toJSON(data, auto_unbox = TRUE)
}

py_df_to_r <- function(py_result) {
  if (is.null(py_result)) return(data.frame())
  
  # Check if it's a DataFrame
  if (!("to_dict" %in% names(py_result))) {
    return(py_to_r(py_result))  # Not a DataFrame, use default conversion
  }
  
  records <- py_result$to_dict("list")
  records <- lapply(records, function(col) {
    sapply(col, function(x) if (is.null(x)) NA else x)
  })
  as.data.frame(records, stringsAsFactors = FALSE)
}