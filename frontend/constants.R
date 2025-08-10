# ============================================================================
# constants.R
# ============================================================================
"
Configuration constants and static values for SNV Benchmarking Dashboard.

Main components:
- CSS styling definitions
- UI configuration constants, UI dropdowns (metric labels, technology/caller options, etc)
"

# ============================================================================
# CSS STYLING CONSTANTS
# ============================================================================

# Main application CSS styles
APP_CSS_STYLES <- "
    .sidebar {
      position: fixed !important;
      top: 0;
      left: 0;
      height: 100vh;
      width: 330px;
      background-color: #f8f9fa;
      border-right: 1px solid #dee2e6;
      overflow-y: auto;
      z-index: 1000;
      padding: 0;
      box-sizing: border-box;
    }
    
    .sidebar-header {
      position: sticky;
      top: 0;
      background: linear-gradient(135deg, #007bff 0%, #0056b3 100%);
      color: white;
      padding: 15px 20px;
      border-bottom: 1px solid #dee2e6;
      z-index: 1002;
    }
    
    .sidebar-content {
      padding: 20px;
      height: calc(100vh - 80px);
      overflow-y: auto;
    }
    
    .main-content {
      margin-left: 350px;
      padding: 20px;
      min-height: 100vh;
    }

    /* Table styling */
    .dataTables_wrapper tbody td {
      text-transform: capitalize !important;
    }

    .dataTables_wrapper tbody td:nth-child(1) {
      text-transform: none !important;
    }

    /* Plot column constraints */
    .plotly-container {
      width: 100% !important;
      max-width: 100% !important;
      overflow: hidden !important;
    }
    
    .plot-column .plotly {
      width: 100% !important;
      max-width: 100% !important;
      box-sizing: border-box !important;
    }
    
    .col-sm-5 {
      max-width: 41.66667% !important;
      flex: 0 0 41.66667% !important;
    }
    
    .col-sm-2 {
      max-width: 16.66667% !important;
      flex: 0 0 16.66667% !important;
    }
    
    .row {
      flex-wrap: nowrap !important;
    }
    
    /* Metric selection pills */
    .metric-pill {
      padding: 8px 16px;
      border-radius: 20px;
      cursor: pointer;
      font-weight: 500;
      font-size: 11px;
      transition: all 0.2s ease;
      text-align: center;
      display: inline-block;
    }
    
    /* Force legend removal for stratified plots */
    .ggplot .legend {
      display: none !important;
    }
"

# Row expansion and metadata styling
METADATA_CSS_STYLES <- "
    /* Table expand buttons */
    .details-toggle {
      background: none;
      border: none;
      color: #6c757d;
      cursor: pointer;
      padding: 2px 4px;
      border-radius: 3px;
      font-size: 12px;
      margin: 0;
    }
    .details-toggle:hover {
      background: #e9ecef;
    }
    
    /* Row expansion container */
    .detail-content {
      background: #f8f9fa;
      padding: 15px;
      border-left: 3px solid #007bff;
      font-size: 12px;
    }
    
    /* Row expansion 3-column grid */
    .detail-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 15px;
    }
    
    /* Metadata cards for visualization tab */
    .metadata-grid-4col {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
      gap: 20px;
      margin-top: 15px;
    }
    
    .metadata-card {
      background: #ffffff;
      border: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      transition: box-shadow 0.3s ease;
    }
    .metadata-card:hover {
      box-shadow: 0 4px 8px rgba(0,0,0,0.15);
    }
"

# ============================================================================
# UI CONFIGURATION CONSTANTS
# ============================================================================

# Metric display labels for UI
METRIC_LABELS <- list(
  "f1_score" = "F1 Score",
  "precision" = "Precision", 
  "recall" = "Recall"
)

# Filter type options for sidebar
FILTER_TYPES <- list(
  "Show All" = "none",
  "Technology" = "tech", 
  "Variant Caller" = "caller"
)

# Available options for dropdowns
TECHNOLOGY_OPTIONS <- c("ILLUMINA", "PACBIO", "ONT", "MGI")
CALLER_OPTIONS <- c("DEEPVARIANT", "GATK", "CLAIR3")
VARIANT_TYPES <- c("SNP", "INDEL")


