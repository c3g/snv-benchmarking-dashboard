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

# Available options for dropdowns - uses enum_mappings.py

TECHNOLOGY_OPTIONS <- enums$VALID_TECHNOLOGIES
CALLER_OPTIONS <-  enums$VALID_CALLERS
TRUTH_SET_OPTIONS <-  c("ALL", enums$VALID_TRUTH_SETS)
TARGET_OPTIONS <- enums$VALID_TARGETS
PLATFORM_TYPE_OPTIONS <- enums$VALID_PLATFORM_TYPES
CALLER_TYPE_OPTIONS <- enums$VALID_CALLER_TYPES
REFERENCE_OPTIONS <- enums$VALID_REFERENCES
SAMPLE_OPTIONS <- enums$VALID_SAMPLES
VARIANT_TYPE_OPTIONS <- enums$VALID_VARIANT_TYPES
VARIANT_SIZE_OPTIONS <- enums$VALID_VARIANT_SIZES
VARIANT_ORIGIN_OPTIONS <- enums$VALID_VARIANT_ORIGINS
BECHMARKING_TOOL_OPTIONS <- enums$VALID_BENCHMARK_TOOLS

'
<<<< commented out for now as we only use uppercase versions for now>>>>
# Display names for UI dropdowns
TECHNOLOGY_DISPLAY_NAMES <- c(
  "ILLUMINA" = "Illumina",
  "PACBIO" = "PacBio",
  "ONT" = "ONT",
  "MGI" = "MGI",
  "10X" = "10X",
  "ULTIMA" = "Ultima"
)

CALLER_DISPLAY_NAMES <- c(
  "DEEPVARIANT" = "DeepVariant",
  "GATK" = "GATK",
  "CLAIR3" = "Clair3",
  "DRAGEN" = "DRAGEN",
  "GATK3" = "GATK3",
  "GATK4" = "GATK4",
  "LONGRANGER" = "LongRanger",
  "MEGABOLT" = "Megabolt",
  "NANOCALLER" = "NanoCaller",
  "PARABRICK" = "Parabrick",
  "PEPPER" = "Pepper"
)'

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
    
    .sidebar-header a {
      text-decoration: none !important;
      display: inline-block;
      transition: transform 0.2s ease, opacity 0.2s ease;
      width: 100%;
    }
    
    .sidebar-header a:hover {
      transform: translateY(-1px);
      opacity: 0.95;
    }
    
    .sidebar-header a:hover img {
      transform: scale(1.08);
    }
    
    .sidebar-header a h4 {
      color: white !important;
    }
    
    .sidebar-header h4 {
      margin: 0;
      font-weight: 600;
      font-size: 1.3em;
    }
    
    .sidebar-content {
      padding: 20px 20px 20px 20px;
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
    
    /* Active state for comparison mode buttons */
    .sidebar-content .btn-comparison-active {
      box-shadow: inset 0 2px 4px rgba(0,0,0,0.3), 0 0 0 3px rgba(255,255,255,0.3) !important;
      transform: scale(0.98) !important;
    }
    
    .sidebar-content .btn-comparison-active::before {
      content: '●  ';
      font-size: 10px;
    }
    
    .sidebar-content .form-group {
      margin-bottom: 12px !important;
    }
    
    .sidebar-content .form-control {
      padding: 6px 10px !important;
      font-size: 13px !important;
    }
    /* Fix dropdown menu overflow in sidebar */
    .sidebar-content .selectize-dropdown {
      max-height: 400px !important;
      overflow-y: auto !important;
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
      overflow-x: auto; 
    }
    
    .dataTables_wrapper tbody tr:nth-child(even) {
      background-color: #f1f5ff;  /* More noticeable blue */
    }
    
    .dataTables_wrapper tbody tr.odd {
      background-color: #ffffff;
    }

    .dataTables_wrapper tbody tr.even {
      background-color: #f1f5ff;
    }

    .dataTables_wrapper tbody tr[class*='detail-row'] {
    background-color: #fafbfc !important;
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
    
    /* LOADING */
    .shiny-output-container {
      position: relative;
    }
    
    .shiny-spinner-output-container {
      position: relative;
    }
    
    .shiny-spinner-output-container > .load-container {
      display: flex;
      align-items: center;
      justify-content: center;
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      z-index: 1000;
    }
    
    .shiny-spinner-placeholder {
      background: rgba(255, 255, 255, 0.8);
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      display: flex;
      align-items: center;
      justify-content: center;
      z-index: 999;
    }
    
    .custom-spinner {
      border: 3px solid #f3f3f3;
      border-top: 3px solid #5084d1;
      border-radius: 50%;
      width: 40px;
      height: 40px;
      animation: spin 1s linear infinite;
    }
    
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
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

    /* Visibility filter row (frirst tab) */
    .visibility-filter-row {
      display: flex;
      align-items: center;
      gap: 12px;
      margin-bottom: 15px;
      padding: 10px 15px;
      background: #f4f5f7;
      border-radius: 6px;
      border: 1px solid #e0e0e0;
    }
    
    .visibility-filter-row .visibility-label {
      font-weight: 600;
      color: #333;
      line-height: 1;
    }
    
    .visibility-filter-row .form-group {
      margin: 0;
    }
    
    .visibility-filter-row .shiny-options-group {
      display: flex;
      gap: 0;
    }
    
    .visibility-filter-row .radio-inline {
      padding: 7px 16px;
      margin: 0;
      background: white;
      border: 1px solid #ccc;
      cursor: pointer;
      font-size: 13px;
      font-weight: 500;
      color: #555;
      line-height: 1;
    }
    
    .visibility-filter-row .radio-inline:first-child {
      border-radius: 5px 0 0 5px;
    }
    
    .visibility-filter-row .radio-inline:last-child {
      border-radius: 0 5px 5px 0;
      border-left: none;
    }
    
    .visibility-filter-row .radio-inline:not(:first-child):not(:last-child) {
      border-left: none;
    }
    
    .visibility-filter-row .radio-inline:hover {
      background: #e9ecef;
    }
    
    .visibility-filter-row .radio-inline input[type='radio'] {
      display: none;
    }
    
    .visibility-filter-row .radio-inline:has(input:checked) {
      background: #4472ca;
      color: white;
      border-color: #4472ca;
      
    }

    .experiment-count-badge {
    margin-left: 12px;
    padding: 4px 10px;
    background: #e9ecef;
    border-radius: 12px;
    font-size: 12px;
    color: #495057;
    font-weight: 500;
  }
  
  /* Admin Panel Styles */
  .admin-stat-card {
    background: #ffffff;
    border: 1px solid #e4e7ea;
    border-radius: 6px;
    padding: 15px;
    text-align: center;
  }
  
  .admin-panel-section {
    background: #ffffff;
    border: 1px solid #e4e7ea;
    border-radius: 6px;
    padding: 20px;
    margin-bottom: 20px;
  }
  
  .admin-panel-section h5 {
    margin-bottom: 12px;
    font-weight: 600;
    color: #333;
    font-size: 14px;
  }

  /* Fix modal scrolling */
  .modal-body {
    max-height: 80vh;
    overflow-y: auto;
  }
"
# Region selection CSS
REGION_INFO_CSS <- "
  .region-info-icon {
    color: #007bff;
    cursor: help;
    margin-left: 5px;
    font-size: 14px;
  }
  
  .tooltip-inner {
    max-width: 350px;
    text-align: left;
    font-size: 13px;
    line-height: 1.5;
    padding: 12px 15px;
    background-color: #e3f2fd;
    color: #1565c0;
    border: 1px solid #bbdefb;
    border-radius: 6px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.15);
    opacity: 1;
  }
  
  .tooltip {
    opacity: 1 !important;
  }
  
  .tooltip.bs-tooltip-right .arrow::before {
    border-right-color: #e3f2fd;
  }
  
  .tooltip.bs-tooltip-left .arrow::before {
    border-left-color: #e3f2fd;
  }
  
  .tooltip.bs-tooltip-top .arrow::before {
    border-top-color: #e3f2fd;
  }
  
  .tooltip.bs-tooltip-bottom .arrow::before {
    border-bottom-color: #e3f2fd;
  }
"
# Truth set filter panel styling
TRUTH_SET_FILTER_CSS <- "
  .truth-set-filter-panel {
    background-color: #f8f9fa;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    padding: 12px 16px;
    margin-bottom: 20px;
    display: flex;
    align-items: center;
    gap: 12px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
  }
  
  .truth-set-filter-panel label {
    margin: 0;
    font-weight: 500;
    font-size: 14px;
    color: #495057;
    white-space: nowrap;
  }
  
  .truth-set-filter-panel .form-group {
    margin: 0;
    flex: 1;
    max-width: 250px;
  }
  
  .truth-set-filter-panel select {
    font-size: 13px;
    padding: 6px 10px;
    border-radius: 4px;
    border: 1px solid #ced4da;
  }
  
  .truth-set-filter-panel .info-icon {
    color: #6c757d;
    cursor: help;
    font-size: 16px;
  }
"

# ============================================================================
# JAVASCRIPT INTERACTION HANDLERS
# ============================================================================

# Comparison button active state handler
COMPARISON_BUTTON_JS <- "
$(document).ready(function() {
  // Button IDs for comparison modes
  var comparisonButtons = ['#compare_advanced', '#compare_experiments'];
  
  // Handle comparison button clicks - toggle active state
  $(comparisonButtons.join(',')).on('click', function() {
    // Remove active class from all comparison buttons
    $(comparisonButtons.join(',')).removeClass('btn-comparison-active');
    // Add active class to clicked button
    $(this).addClass('btn-comparison-active');
  });
  
  // Handle logo/home button click - remove all active states
  $('#logo_home_btn').on('click', function() {
    $(comparisonButtons.join(',')).removeClass('btn-comparison-active');
  });
});
"

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

# Custom message handlers for dynamic content insertion - handle reset with JS
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
    
    Shiny.addCustomMessageHandler('resetHierarchyCheckboxes', function(data) {
      // Uncheck all technology checkboxes
      $('.tech-parent-checkbox').prop('checked', false);
      $('.tech-child-checkbox').prop('checked', false);
      
      // Uncheck all caller checkboxes
      $('.caller-parent-checkbox').prop('checked', false);
      $('.caller-child-checkbox').prop('checked', false);
      
      // Collapse all expanded sections
      $('.tech-children-container').slideUp(150);
      $('.caller-children-container').slideUp(150);
      $('.tech-expand-arrow').text('▶');
      $('.caller-expand-arrow').text('▶');
      
      // Reset Shiny input values
      Shiny.setInputValue('tech_hierarchy_selection', {}, {priority: 'event'});
      Shiny.setInputValue('caller_hierarchy_selection', {}, {priority: 'event'});
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
            'background': 'white',
            'color': '#6c757d',
            'border': '1px solid #dee2e6',
            'box-shadow': '0 3px 8px rgba(0,0,0,0.15)',
            'transform': 'translateY(0px)'
          });
        });
        
        // Add active styling to clicked pill
        $(this).css({
          'background': '#5084d1',
          'color': 'white',
          'border': 'none',
          'box-shadow': '0 4px 12px rgba(80,132,209,0.4)',
          'transform': 'translateY(-1px)'
        });
        
        // Update the hidden radio button
        $('input[name=\"selected_metric\"][value=\"' + value + '\"]').prop('checked', true).trigger('change');
      });
      
      // Enhanced hover effects
      $('.metric-pill').hover(
        function() {
          if ($(this).css('background-color') !== 'rgb(80, 132, 209)') {
            $(this).css({
              'transform': 'translateY(-2px)',
              'box-shadow': '0 6px 16px rgba(80,132,209,0.25)'
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


# tooltip JavaScript
REGION_TOOLTIPS_JS <- "
  $(document).ready(function(){
    // Initialize tooltips
    $('[data-toggle=\"tooltip\"]').tooltip({
      placement: 'right',
      html: true
    });
  });

"
# Plotly refresh on tab 3
PLOTLY_REFRESH_JS <- "
  // Force Plotly to recalculate layout when Visualizations tab is shown
  $(document).on('shown.bs.tab', 'a[data-value=\"Visualizations\"]', function() {
    setTimeout(function() {
      if (window.Plotly) {
        var plots = document.querySelectorAll('.plotly');
        plots.forEach(function(plot) {
          Plotly.Plots.resize(plot);
        });
      }
    }, 100);
  });
"

# Update APP_CSS_STYLES at the very end
APP_CSS_STYLES <- paste0(APP_CSS_STYLES, TRUTH_SET_FILTER_CSS, REGION_INFO_CSS)