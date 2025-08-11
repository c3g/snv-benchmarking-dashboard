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
  "Unknown" = "#E76BF3"      # Fallback color
)

# Caller shape mapping for scatter plots
caller_shapes <- c(
  "DEEPVARIANT" = 16,        # Circle 
  "GATK" = 17,               # Triangle
  "CLAIR3" = 15,             # Square 
  "Unknown" = 4              # X shape
)

# Shape symbols for HTML legend display
shape_symbols <- c("16" = "●", "17" = "▲", "15" = "■", "4" = "✕")

# Technology-caller gradient combinations for stratified plots
tech_caller_colors <- c(
  # ILLUMINA family (Red variations)
  "ILLUMINA-DEEPVARIANT" = "#F8766D",
  "ILLUMINA-GATK" = "#E55A5A",
  "ILLUMINA-CLAIR3" = "#FF9999",
  
  # PACBIO family (Purple variations)
  "PACBIO-DEEPVARIANT" = "#C77CFF",
  "PACBIO-GATK" = "#B366FF",
  "PACBIO-CLAIR3" = "#D999FF",
  
  # ONT family (Cyan variations)
  "ONT-DEEPVARIANT" = "#00BFC4",
  "ONT-GATK" = "#00A5A8",
  "ONT-CLAIR3" = "#33CCCC",
  
  # MGI family (Green variations)
  "MGI-DEEPVARIANT" = "#7CAE00",
  "MGI-GATK" = "#6B9500",
  "MGI-CLAIR3" = "#99CC33"
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