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

# Technology color scheme
technology_colors <- c(
  "ILLUMINA" = "#F8766D",    # Red
  "PACBIO" = "#C77CFF",      # Purple
  "ONT" = "#00BFC4",         # Cyan
  "MGI" = "#7CAE00",         # Green
  "10X" = "#FFA500",         # Orange
  "Unknown" = "#E76BF3"      # Fallback color
)

# Caller shape mapping for scatter plots
caller_shapes <- c(
  "DEEPVARIANT" = 16,   # ●
  "CLAIR3"       = 15,   # ■
  "DRAGEN"       = 18,   # ◆
  "GATK3"        = 17,   # ▲ (inherited from GATK)
  "GATK4"        = 4,    # ✕
  "LONGRANGER"   = 3,    # ＋
  "MEGABOLT"     = 10,   # ⊕
  "NANOCALLER"   = 12,   # ⊞
  "PARABRICK"    = 1,    # ○
  "PEPPER"       = 0,    # □
  "Unknown"      = 4     # ✕ (fallback)
)
# extra shape to use for new caller: 8 ->  ✶

# Matching HTML legend symbols
shape_symbols <- c(
  "16" = "●",
  "17" = "▲",
  "15" = "■",
  "18" = "◆",
  "8"  = "✶",
  "4"  = "✕",
  "3"  = "+",
  "10" = "⊕",
  "12" = "⊞",
  "1"  = "○",
  "0"  = "□"
)


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
  "10X-PEPPER" = "#FFC151"
)

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

# Safe pandas to R conversion
py_df_to_r <- function(py_result) {
  if (is.null(py_result)) return(data.frame())
  records <- py_result$to_dict("list")
  records <- lapply(records, function(col) {
    sapply(col, function(x) if (is.null(x)) NA else x)
  })
  as.data.frame(records, stringsAsFactors = FALSE)
}