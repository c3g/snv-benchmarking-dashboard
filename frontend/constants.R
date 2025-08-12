# ============================================================================
# constants.R
# ============================================================================
"
Configuration constants and static values for SNV Benchmarking Dashboard.

Main components:
- UI configuration constants, UI dropdowns (metric labels, technology/caller options, etc)
- CSS styling definitions
- JavaScript interaction handlers
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


# ============================================================================
# CSS STYLING CONSTANTS
# ============================================================================

# Main app style

APP_CSS_STYLES <- "
    body {
      background-color: #fafafa;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    }
    
    .sidebar {
      position: fixed !important;
      top: 0;
      left: 0;
      height: 100vh;
      width: 330px;
      background-color: #f7f8f9;
      border-right: 1px solid #e4e7ea;
      overflow-y: auto;
      z-index: 1000;
      padding: 0;
      box-sizing: border-box;
      box-shadow: 2px 0 8px rgba(0, 0, 0, 0.04);
    }
    
    .sidebar-header {
      position: sticky;
      top: 0;
      background: linear-gradient(135deg, #4472ca 0%, #365a96 100%);
      color: white;
      padding: 16px 20px;
      border-bottom: none;
      z-index: 1002;
    }
    
    .sidebar-header h4 {
      margin: 0;
      font-weight: 600;
      font-size: 1.3em;
    }
    
    .sidebar-content {
      padding: 20px 20px;
      height: calc(100vh - 72px);
      overflow-y: auto;
    }
    
    .sidebar-content h4 {
      color: #4a5568;
      font-weight: 600;
      margin-bottom: 12px;
      margin-top: 16px;
      font-size: 1.05em;
      border-bottom: 1px solid #e4e7ea;
      padding-bottom: 6px;
    }
    
    .sidebar-content h4:first-of-type {
      margin-top: 0;
    }
    
    .sidebar-content .radio {
      margin-bottom: 6px !important;
    }
    
    .sidebar-content .radio label {
      padding-left: 20px !important;
      font-size: 14px !important;
      line-height: 1.3 !important;
    }
    
    .sidebar-content .btn-primary,
    .sidebar-content .btn-success, 
    .sidebar-content .btn-warning {
      width: 100% !important;
      margin-bottom: 8px !important;
      border: none !important;
      border-radius: 5px !important;
      font-weight: 500 !important;
      transition: all 0.2s ease !important;
      padding: 10px 16px !important;
      text-align: center !important;
      display: block !important;
      box-sizing: border-box !important;
      font-size: 13px !important;
    }
    
    .sidebar-content .btn-primary {
      background-color: #5084d1 !important;
      color: white !important;
    }
    
    .sidebar-content .btn-primary:hover {
      background-color: #4472ca !important;
      transform: translateY(-1px) !important;
    }
    
    .sidebar-content .btn-success {
      background-color: #42a65c !important;
      color: white !important;
    }
    
    .sidebar-content .btn-success:hover {
      background-color: #358a4c !important;
      transform: translateY(-1px) !important;
    }
    
    .sidebar-content .btn-warning {
      background-color: #9470d4 !important;
      color: white !important;
    }
    
    .sidebar-content .btn-warning:hover {
      background-color: #8460c4 !important;
      transform: translateY(-1px) !important;
    }
    
    .sidebar-content .form-group {
      margin-bottom: 12px !important;
    }
    
    .sidebar-content .form-control {
      padding: 6px 10px !important;
      font-size: 13px !important;
    }
    
    .sidebar-content .checkbox {
      margin-bottom: 4px !important;
    }
    
    .sidebar-content .checkbox label {
      padding-left: 20px !important;
      font-size: 13px !important;
      line-height: 1.3 !important;
    }
    
    .sidebar-content .panel {
      margin-bottom: 12px !important;
      padding: 12px !important;
    }
    
    .sidebar-content .panel-heading {
      padding: 8px 12px !important;
      margin: -12px -12px 8px -12px !important;
    }
    
    .sidebar-content .panel-body {
      padding: 8px !important;
    }
    
    .sidebar-content table {
      font-size: 12px !important;
      margin-bottom: 8px !important;
    }
    
    .sidebar-content table td,
    .sidebar-content table th {
      padding: 6px 8px !important;
    }
    
    .main-content {
      margin-left: 350px;
      padding: 12px;
      min-height: 100vh;
      background-color: #fafafa;
      width: calc(100vw - 350px);
      max-width: none;
    }
    
    .well, .panel {
      background-color: #ffffff;
      border: 1px solid #e4e7ea;
      border-radius: 6px;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
      margin-bottom: 20px;
      padding: 20px;
    }
    
    .alert {
      background-color: #ffffff;
      border: 1px solid #e4e7ea;
      border-radius: 6px;
      border-left: 3px solid #5084d1;
      margin-bottom: 20px;
    }
    
    .alert-info {
      background: linear-gradient(135deg, #f5f8ff 0%, #eaf2ff 100%);
      border-left-color: #5084d1;
      border-color: #d6e3ff;
    }
    
    .nav-tabs {
      border-bottom: 1px solid #e4e7ea;
      margin-bottom: 20px;
    }
    
    .nav-tabs > li {
      margin-bottom: 0 !important;
    }
    
    .nav-tabs > li > a {
      border-radius: 5px 5px 0 0 !important;
      margin-right: 8px !important;
      background-color: #f7f8f9 !important;
      border: 1px solid #e4e7ea !important;
      color: #556b78 !important;
      font-weight: 500 !important;
      padding: 12px 20px !important;
      transition: all 0.15s ease !important;
    }
    
    .nav-tabs > li > a:hover {
      background-color: #edf2f7 !important;
      color: #4472ca !important;
      border-color: #cbd5e0 !important;
    }
    
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:hover,
    .nav-tabs > li.active > a:focus {
      background-color: #5084d1 !important;
      color: white !important;
      border-color: #5084d1 !important;
    }
    
    .dataTables_wrapper {
      background-color: #ffffff;
      border-radius: 6px;
      padding: 20px;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
      border: 1px solid #e4e7ea;
    }
    
    .dataTables_wrapper tbody tr:hover {
      background-color: #f7fafc;
    }
    
    .dataTables_wrapper tbody td {
      text-transform: capitalize !important;
      border-bottom: 1px solid #f7f8f9;
    }
    
    .dataTables_wrapper tbody td:nth-child(1) {
      text-transform: none !important;
    }
    
    .btn-primary {
      background-color: #5084d1;
      border: none;
      border-radius: 5px;
      font-weight: 500;
      padding: 10px 18px;
      transition: all 0.2s ease;
    }
    
    .btn-primary:hover {
      background-color: #4472ca;
      transform: translateY(-1px);
    }
    
    .btn-success {
      background-color: #42a65c;
      border: none;
      border-radius: 5px;
    }
    
    .btn-success:hover {
      background-color: #358a4c;
    }
    
    .plotly-container {
      border-radius: 6px;
      overflow: hidden;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
      border: 1px solid #e4e7ea;
    }
    
    .metric-pill {
      padding: 8px 16px;
      border-radius: 14px;
      cursor: pointer;
      font-weight: 500;
      font-size: 12px;
      transition: all 0.2s ease;
      text-align: center;
      border: 1px solid #e4e7ea;
      background: linear-gradient(135deg, #f5f8ff 0%, #eaf2ff 100%);
      color: #5084d1;
    }
    
    .metric-pill:hover {
      border-color: #5084d1;
      color: #4472ca;
      background: linear-gradient(135deg, #eaf2ff 0%, #dfeaff 100%);
    }
    
    .metric-pill.active {
      background-color: #5084d1;
      border-color: #5084d1;
      color: white;
    }
    
    .dt-body-wrap {
    white-space: normal !important;
    word-wrap: break-word !important;
}

  "
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
# JAVASCRIPT INTERACTION HANDLERS
# ============================================================================

# Table row expansion functionality
TABLE_INTERACTION_JS <- "
    var expandedRows = {};
    
    function toggleDetails(experimentId) {
      var button = event.target;
      var row = button.closest('tr');
      var nextRow = row.nextElementSibling;
      
      if (nextRow && nextRow.classList.contains('detail-row-' + experimentId)) {
        if (nextRow.style.display === 'none') {
          nextRow.style.display = '';
          button.innerHTML = '▼';
          expandedRows[experimentId] = true;
        } else {
          nextRow.style.display = 'none';
          button.innerHTML = '▶';
          expandedRows[experimentId] = false;
        }
      } else {
        button.innerHTML = '▼';
        expandedRows[experimentId] = true;
        Shiny.setInputValue('expand_experiment_details', {
          id: experimentId,
          timestamp: new Date().getTime()
        });
      }
    }
"

# Custom message handlers for dynamic content insertion
CUSTOM_MESSAGE_HANDLERS_JS <- "
    Shiny.addCustomMessageHandler('insertDetailsRow', function(data) {
      var experimentId = data.experimentId;
      var html = data.html;
      
      var table = document.querySelector('#experiments_table table tbody');
      var rows = table.querySelectorAll('tr');
      
      for (var i = 0; i < rows.length; i++) {
        var button = rows[i].querySelector('.details-toggle');
        if (button && button.getAttribute('onclick').includes(experimentId)) {
          rows[i].insertAdjacentHTML('afterend', html);
          break;
        }
      }
    });
"

# Collapsible section handlers for region selection
COLLAPSIBLE_HANDLERS_JS <- "
    $(document).on('click', '[data-toggle=\"collapse\"]', function() {
      var triangle = $(this).find('h6');
      if (triangle.text().startsWith('▶')) {
        triangle.text(triangle.text().replace('▶', '▼'));
      } else {
        triangle.text(triangle.text().replace('▼', '▶'));
      }
    });
"

# Metric selection pill interactive functionality
METRIC_SELECTION_JS <- "
    $(document).ready(function() {
      $('.metric-pill').click(function() {
        var value = $(this).data('value');
        
        // Remove active styling from all pills
        $('.metric-pill').each(function() {
         $(this).css({
            'background': '#5084d1',
            'color': 'white', 
            'border': 'none',
            'box-shadow': '0 4px 12px rgba(80,132,209,0.4)',
            'transform': 'translateY(-1px)'
          });
        });
        
        // Add active styling to clicked pill
        $(this).css({
          'background': '#007bff',
          'color': 'white',
          'border': 'none',
          '0 6px 16px rgba(80,132,209,0.25)'
          'transform': 'translateY(-1px)'
        });
        
        // Update the hidden radio button
        $('input[name=\"selected_metric\"][value=\"' + value + '\"]').prop('checked', true).trigger('change');
      });
      
      // Enhanced hover effects
      $('.metric-pill').hover(
        function() {
          if ($(this).css('background-color') !== 'rgb(0, 123, 255)') {
            $(this).css({
              'transform': 'translateY(-2px)',
              'box-shadow': '0 6px 16px rgba(0,123,255,0.25)'
            });
          }
        },
        function() {
         if ($(this).css('background-color') !== 'rgb(80, 132, 209)') {
            $(this).css({
              'transform': 'translateY(0px)',
              'box-shadow': '0 3px 8px rgba(0,0,0,0.15)'
            });
          }
        }
      );
    });
"


