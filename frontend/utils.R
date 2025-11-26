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
  "GATK"         = 17,   # ▲
  "CLAIR3"       = 15,   # ■
  "DRAGEN"       = 18,   # ◆
  "GATK3"        = 8,    # ✶
  "GATK4"        = 4,    # ✕
  "LONGRANGER"   = 3,    # ＋
  "MEGABOLT"     = 10,   # ⊕
  "NANOCALLER"   = 12,   # ⊞
  "PARABRICK"    = 1,    # ○
  "PEPPER"       = 0,    # □
  "Unknown"      = 4     # ✕ (fallback)
)


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
  "ILLUMINA-GATK" = "#F53B2F",
  "ILLUMINA-CLAIR3" = "#F54A3E",
  "ILLUMINA-DRAGEN" = "#F6584E",
  "ILLUMINA-GATK3" = "#F7675D",
  "ILLUMINA-GATK4" = "#F8756C",
  "ILLUMINA-LONGRANGER" = "#F8847C",
  "ILLUMINA-MEGABOLT" = "#F9938B",
  "ILLUMINA-NANOCALLER" = "#FAA19B",
  "ILLUMINA-PARABRICK" = "#FAB0AA",
  "ILLUMINA-PEPPER" = "#FBBEBA",
  
  # PACBIO family (Purple variations)
  "PACBIO-DEEPVARIANT" = "#A42AFE",
  "PACBIO-GATK" = "#AB3BFF",
  "PACBIO-CLAIR3" = "#B24BFF",
  "PACBIO-DRAGEN" = "#B95BFF",
  "PACBIO-GATK3" = "#C06BFF",
  "PACBIO-GATK4" = "#C77CFF",
  "PACBIO-LONGRANGER" = "#CD8CFE",
  "PACBIO-MEGABOLT" = "#D49CFE",
  "PACBIO-NANOCALLER" = "#DBACFF",
  "PACBIO-PARABRICK" = "#E2BCFF",
  "PACBIO-PEPPER" = "#E9CCFE",
  
  # ONT family (Cyan variations)
  "ONT-DEEPVARIANT" = "#006F72",
  "ONT-GATK" = "#007F83",
  "ONT-CLAIR3" = "#008F93",
  "ONT-DRAGEN" = "#009FA3",
  "ONT-GATK3" = "#00AFB3",
  "ONT-GATK4" = "#00BEC4",
  "ONT-LONGRANGER" = "#00CED4",
  "ONT-MEGABOLT" = "#00DEE4",
  "ONT-NANOCALLER" = "#00EEF4",
  "ONT-PARABRICK" = "#05F8FF",
  "ONT-PEPPER" = "#16F9FF",
  
  # MGI family (Green variations)
  "MGI-DEEPVARIANT" = "#486600",
  "MGI-GATK" = "#4D6D00",
  "MGI-CLAIR3" = "#597D00",
  "MGI-DRAGEN" = "#648D00",
  "MGI-GATK3" = "#709D00",
  "MGI-GATK4" = "#7CAE00",
  "MGI-LONGRANGER" = "#87BE00",
  "MGI-MEGABOLT" = "#93CE00",
  "MGI-NANOCALLER" = "#9EDE00",
  "MGI-PARABRICK" = "#AAEE00",
  "MGI-PEPPER" = "#B5FF00",
  
  # 10X family (Orange variations)
  "10X-DEEPVARIANT" = "#AD7000",
  "10X-GATK" = "#BE7B00",
  "10X-CLAIR3" = "#CE8500",
  "10X-DRAGEN" = "#DE9000",
  "10X-GATK3" = "#EE9A00",
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