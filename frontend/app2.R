# ============================================================================
# app.R - SNV Benchmarking Dashboard (Modular Version)
# ============================================================================
"
Main Shiny application for SNV Benchmarking Dashboard.

This application provides an interactive interface for analyzing variant calling
performance metrics across different sequencing technologies, platforms, and 
genomic regions. Features include experiment comparison, performance visualization,
and stratified analysis.

Architecture:
- Backend: Python (SQLAlchemy, pandas) for data processing
- Frontend: R Shiny for interactive visualization  
- Database: SQLite for experiment metadata and results storage
- Modular Structure: Separated into functional components for maintainability
"

# ============================================================================
# LIBRARIES AND DEPENDENCIES
# ============================================================================

# Core Shiny and data manipulation
library(reticulate)
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(jsonlite)

# Visualization libraries
library(ggplot2)
library(plotly)
library(ggsci)
library(patchwork)
library(geomtextpath)

# UI enhancement libraries
library(htmltools)
library(htmlwidgets)
library(shinyBS)

# ============================================================================
# PYTHON BACKEND INTERFACE
# ============================================================================

# Initialize Python environment (db will be imported inside reactive functions)
py_run_string("import sys")
py_run_string("sys.path.append('../backend')")

# ============================================================================
# R MODULE IMPORTS
# ============================================================================

# Import core modules (order matters!)
source("constants.R")
source("utils.R")
source("data_processing.R")
source("plot_functions.R")
source("table_functions.R")
source("observers.R")
source("ui_components.R")

# Import specialized modules
source("html_export.R")    # HTML report generation
source("upload_ui.R")      # File upload interface
source("upload_server.R")  # File upload processing

# Set global ggplot theme
theme_set(theme_bw())

# ============================================================================
# USER INTERFACE DEFINITION
# ============================================================================

ui <- fluidPage(
  
  # ====================================================================
  # HEAD SECTION - CSS STYLES AND SCRIPTS
  # ====================================================================
  
  tags$head(
    # Main application styles
    tags$style(HTML(APP_CSS_STYLES)),
    tags$style(HTML(METADATA_CSS_STYLES)),
    
    # JavaScript for table row expansion
    tags$script(HTML("
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
    ")),
    
    # Custom message handler for metadata insertion
    tags$script(HTML("
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
    ")),
    
    # Collapsible section handlers
    tags$script(HTML("
      $(document).on('click', '[data-toggle=\"collapse\"]', function() {
        var triangle = $(this).find('h6');
        if (triangle.text().startsWith('▶')) {
          triangle.text(triangle.text().replace('▶', '▼'));
        } else {
          triangle.text(triangle.text().replace('▼', '▶'));
        }
      });
    ")),
    
    # Metric selection pill handlers
    tags$script(HTML("
      $(document).ready(function() {
        $('.metric-pill').click(function() {
          var value = $(this).data('value');
          
          // Remove active styling from all pills
          $('.metric-pill').each(function() {
            $(this).css({
              'background': 'white',
              'color': '#495057',
              'border': '1px solid #dee2e6',
              'box-shadow': '0 3px 8px rgba(0,0,0,0.15)',
              'transform': 'translateY(0px)'
            });
          });
          
          // Add active styling to clicked pill
          $(this).css({
            'background': '#007bff',
            'color': 'white',
            'border': 'none',
            'box-shadow': '0 4px 12px rgba(0,123,255,0.4)',
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
            if ($(this).css('background-color') !== 'rgb(0, 123, 255)') {
              $(this).css({
                'transform': 'translateY(0px)',
                'box-shadow': '0 3px 8px rgba(0,0,0,0.15)'
              });
            }
          }
        );
      });
    "))
  ),
  
  # ====================================================================
  # MAIN LAYOUT - SIDEBAR AND CONTENT
  # ====================================================================
  
  sidebarLayout(
    
    # SIDEBAR PANEL
    div(class = "sidebar",
        div(class = "sidebar-header",
            h4("SNV Benchmarking Dashboard", 
               style = "margin: 0; font-size: 1.3em; font-weight: 600;")
        ),
        div(class = "sidebar-content",
            
            # Filter Options Section
            h4("Filter Options:"),
            radioButtons(
              "filter_type",
              "Filter by:",
              choices = FILTER_TYPES,
              selected = "none"
            ),
            
            # Conditional filter dropdowns
            conditionalPanel(
              condition = "input.filter_type == 'tech'",
              selectInput("filter_technology", "Choose Technology:",
                          choices = TECHNOLOGY_OPTIONS, selected = "ILLUMINA")
            ),
            
            conditionalPanel(
              condition = "input.filter_type == 'caller'",
              selectInput("filter_caller", "Choose Caller:",
                          choices = CALLER_OPTIONS, selected = "DEEPVARIANT")
            ),
            
            # Comparison Options Section
            hr(),
            h4("Comparison Options:"),
            
            # Comparison mode buttons
            actionButton("compare_techs", "Compare Sequencing Technologies",
                         class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("compare_callers", "Compare Variant Callers",
                         class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("compare_experiments", "Compare Specific Experiments", 
                         class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
            
            # Technology comparison panel
            conditionalPanel(
              condition = "output.comparison_mode == 'tech_comparison'",
              hr(),
              h5("Technology Comparison Setup:"),
              checkboxGroupInput("selected_technologies", "Select technologies (2 or more):",
                                 choices = list("Illumina" = "ILLUMINA", "PacBio" = "PACBIO", 
                                                "ONT" = "ONT", "MGI" = "MGI")),
              selectInput("tech_comparison_caller", "Choose a caller (for all):",
                          choices = c("DeepVariant" = "DEEPVARIANT", "GATK" = "GATK", "Clair3" = "CLAIR3"),
                          selected = "DEEPVARIANT"),
              conditionalPanel(
                condition = "input.selected_technologies && input.selected_technologies.length >= 2",
                actionButton("submit_tech_comparison", "Submit Technology Comparison",
                             class = "btn-primary", style = "width: 100%;")
              ),
              conditionalPanel(
                condition = "!input.selected_technologies || input.selected_technologies.length < 2",
                p("Please select at least 2 technologies", style = "color: red; font-size: 12px;")
              )
            ),
            
            # Caller comparison panel
            conditionalPanel(
              condition = "output.comparison_mode == 'caller_comparison'",
              hr(),
              h5("Caller Comparison Setup:"),
              checkboxGroupInput("selected_callers", "Select callers (2 or more):",
                                 choices = list("DeepVariant" = "DEEPVARIANT", "GATK" = "GATK", "Clair3" = "CLAIR3")),
              selectInput("caller_comparison_tech", "Choose a technology (for all):",
                          choices = c("Illumina" = "ILLUMINA", "PacBio" = "PACBIO", "ONT" = "ONT", "MGI" = "MGI"),
                          selected = "ILLUMINA"),
              conditionalPanel(
                condition = "input.selected_callers && input.selected_callers.length >= 2",
                actionButton("submit_caller_comparison", "Submit Caller Comparison",
                             class = "btn-success", style = "width: 100%;")
              ),
              conditionalPanel(
                condition = "!input.selected_callers || input.selected_callers.length < 2",
                p("Please select at least 2 callers", style = "color: red; font-size: 12px;")
              )
            ),
            
            # Manual experiment selection panel
            conditionalPanel(
              condition = "output.comparison_mode == 'manual_selection'",
              p("Click on experiments in the table to select them for comparison.", style = "font-size: 15px;"),
              textOutput("selected_experiments_count"),
              actionButton("clear_experiment_selection", "Clear Selection",
                           class = "btn-secondary btn-sm", style = "width: 100%")
            ),
            
            # Selected experiments display (bottom of sidebar)
            conditionalPanel(
              condition = "output.comparison_mode == 'manual_selection' && output.has_selected_experiments",
              hr(),
              div(
                class = "panel panel-info",
                div(class = "panel-heading d-flex justify-content-between align-items-center",
                    h5("Selected Experiments for Comparison", class = "mb-0")
                ),
                div(class = "panel-body", style = "padding: 10px;",
                    div(style = "max-height: 90px; overflow-y: auto; overflow-x: auto; border: 1px solid #dee2e6;",
                        tableOutput("compact_selected_experiments")
                    ),
                    br(),
                    div(style = "text-align: center;",
                        actionButton("submit_bottom_comparison", "Submit Selected Experiments",
                                     class = "btn-warning")
                    )
                )
              )
            )
        )),
    
    # MAIN CONTENT PANEL
    div(class = "main-content",
        width = 9,
        
        # Header with download and upload buttons
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0; padding-bottom: 0;",
          div(style = "flex-grow: 1;"),
          div(
            style = "margin-left: 20px; padding-top: 5px;",
            upload_button_ui(),
            downloadButton(
              "export_html_report", 
              label = tagList(icon("download"), "Download Report"),
              class = "btn-primary btn-sm",
              style = "font-size: 14px; padding: 6px 12px; white-space: nowrap;"
            )
          )
        ),
        
        br(),
        
        # Main tab panel
        tabsetPanel(
          
          # TAB 1: EXPERIMENTS OVERVIEW
          tabPanel(
            "Experiments",
            br(),
            div(class = "container-fluid", style = "width: 100%; padding: 0;",
                div(
                  class = "alert alert-info",
                  style = "margin-bottom: 20px;",
                  h5("Experiment Overview"),
                  p("This table displays all available benchmarking experiments with their key metadata. ", 
                    "Use the sidebar to ", strong("filter by technology or variant caller"), 
                    ", or choose from the comparison options to ", 
                    strong("analyze multiple technologies"), ", ", 
                    strong("compare variant callers"), ", or ", 
                    strong("select specific experiments"), " for detailed analysis."),
                  p(style = "font-size: 0.9em; margin-bottom: 0;",
                    strong("Navigation:"), " Click the ▶ button in any row to expand detailed metadata, or switch to other tabs to view performance results and visualizations.")
                ),
                div(style = "width: 100%; overflow-x: auto;",
                    DT::dataTableOutput("experiments_table")
                )
            )
          ),
          
          # TAB 2: PERFORMANCE RESULTS
          tabPanel(
            "Performance Results", 
            br(),
            div(
              class = "alert alert-info",
              style = "margin-bottom: 20px;",
              h5("Performance Results"),
              p("This table shows detailed performance metrics for each experiment. Each experiment displays ", 
                strong("two rows"), ": one for ", 
                span(style = "color: #d73027; font-weight: bold;", "SNP variants"), 
                " and one for ", 
                span(style = "color: #4575b4; font-weight: bold;", "INDEL variants"), 
                ". Metrics are shown as percentages for easy comparison.")
            ),
            DT::dataTableOutput("performance_table")
          ),
          
          # TAB 3: VISUALIZATIONS
          tabPanel(
            "Visualizations",
            br(),
            fluidRow(
              column(12,
                     div(
                       class = "alert alert-info",
                       h5("🎯 Performance Visualizations"),
                       p("These scatter plots display precision vs recall performance for each experiment, with ", 
                         strong("F1 contour lines"), " showing performance benchmarks. "),
                       p(strong("Click points"), " and scroll down to view experiment details below, or ", strong("hover"), " for quick metrics"),
                       p(style = "font-size: 0.9em; color: #6c757d;", "Tip: Drag to zoom, double-click to reset")
                     )
              )
            ),
            br(),
            fluidRow(
              column(4, class = "plot-column",
                     h4("SNP Performance", style = "color: #d73027; font-weight: bold; text-align: center;"),
                     br(),
                     plotlyOutput("snp_plot", height = "500px")
              ),
              column(4, class = "plot-column",
                     h4("INDEL Performance", style = "color: #4575b4; font-weight: bold; text-align: center;"),
                     br(),
                     plotlyOutput("indel_plot", height = "500px")
              ),
              column(3,
                     div(style = "margin-top: 160px;", 
                         htmlOutput("technology_legend"),
                         br(),
                         htmlOutput("caller_legend")
                     )
              ),
              column(1, "")
            ),
            br(),
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "output.has_selected_point",
                       wellPanel(
                         style = "background-color: #f8f9fa; border-left: 4px solid #007bff; margin-top: 15px;",
                         fluidRow( 
                           column(10,
                                  h5("Selected Experiment Details"),
                                  htmlOutput("basic_experiment_info")
                           ),
                           column(2,
                                  div(style = "text-align: right; padding-top: 10px;",
                                      actionButton("expand_metadata", "Show All Details", 
                                                   class = "btn-primary btn-sm")
                                  )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.expand_metadata % 2 == 1",
                           hr(),
                           htmlOutput("full_experiment_metadata")
                         )
                       )
                     )
              )
            )
          ),
          
          # TAB 4: STRATIFIED ANALYSIS
          tabPanel(
            "Stratified Analysis",
            br(),
            div(
              class = "alert alert-info",
              style = "margin-bottom: 20px;",
              h5("Stratified Performance Analysis"),
              p("Analyze F1 scores across different genomic regions for the selected experiments. "),
              p(style = "font-size: 0.9em; margin-bottom: 0;",
                strong("Note: "), "Only experiments from your current selection (previous tabs) are shown.")
            ),
            
            # Region selection panel
            wellPanel(
              style = "background-color: #f8f9fa; margin-bottom: 20px;",
              h5("Select Regions to Analyze", style = "margin-top: 0;"),
              
              fluidRow(
                column(12,
                       div(style = "background: white; border: 1px solid #dee2e6; border-radius: 5px; padding: 20px;",
                           
                           # Main Regions
                           div(
                             h6("📍 Main Regions", 
                                style = "margin-bottom: 10px; color: #495057; font-weight: bold; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
                             checkboxGroupInput(
                               "core_regions",
                               NULL,
                               choices = list(
                                 "Overall" = "All Regions",
                                 "Easy Regions" = "Easy Regions", 
                                 "Difficult Regions" = "Difficult Regions"
                               ),
                               selected = c("All Regions"),
                               inline = TRUE
                             )
                           ),
                           
                           # Functional Regions (Collapsible)
                           div(
                             style = "margin-top: 15px;",
                             tags$a(
                               href = "#functional_collapse",
                               `data-toggle` = "collapse",
                               style = "text-decoration: none; color: #495057;",
                               h6("▶ Functional Regions",
                                  style = "margin-bottom: 10px; font-weight: bold; border-bottom: 1px solid #dee2e6; padding-bottom: 5px; cursor: pointer;")
                             ),
                             div(
                               id = "functional_collapse",
                               class = "collapse",
                               style = "margin-top: 6px;",
                               checkboxGroupInput(
                                 "functional_regions",
                                 NULL,
                                 choices = list(
                                   "RefSeq CDS" = "RefSeq CDS",
                                   "Not in CDS" = "Non-CDS Regions"
                                 ),
                                 selected = character(0),
                                 inline = TRUE
                               )
                             )
                           ),
                           
                           # Homopolymer Regions
                           div(
                             style = "margin-top: 15px;",
                             tags$a(
                               href = "#homopolymer_collapse",
                               `data-toggle` = "collapse",
                               style = "text-decoration: none; color: #495057;",
                               h6("▶ Homopolymer Regions",
                                  style = "margin-bottom: 10px; font-weight: bold; border-bottom: 1px solid #dee2e6; padding-bottom: 5px; cursor: pointer;")
                             ),
                             div(
                               id = "homopolymer_collapse",
                               class = "collapse",
                               style = "margin-top: 6px;",
                               checkboxGroupInput(
                                 "homopolymer_regions",
                                 NULL,
                                 choices = list(
                                   "Homopolymer 4-6bp" = "Homopolymer 4-6bp",
                                   "Homopolymer 7-11bp" = "Homopolymer 7-11bp",
                                   "Homopolymer >11bp" = "Homopolymer >11bp"
                                 ),
                                 selected = character(0),
                                 inline = TRUE
                               )
                             )
                           ),
                           
                           # GC Content Regions (Collapsible)
                           div(
                             style = "margin-top: 15px;",
                             tags$a(
                               href = "#gc_collapse",
                               `data-toggle` = "collapse",
                               style = "text-decoration: none; color: #495057;",
                               h6("▶ GC Content Regions",
                                  style = "margin-bottom: 10px; font-weight: bold; border-bottom: 1px solid #dee2e6; padding-bottom: 5px; cursor: pointer;")
                             ),
                             div(
                               id = "gc_collapse",
                               class = "collapse",
                               style = "margin-top: 10px;",
                               fluidRow(
                                 column(4,
                                        h6("Low GC:", style = "font-size: 12px; color: #6c757d; margin-bottom: 8px;"),
                                        checkboxGroupInput(
                                          "gc_low",
                                          NULL,
                                          choices = list(
                                            "Very Low (<15%)" = "GC_<15",
                                            "GC 15-20%" = "GC_15_20",
                                            "GC 20-25%" = "GC_20_25",
                                            "GC 25-30%" = "GC_25_30"
                                          ),
                                          selected = character(0)
                                        )
                                 ),
                                 column(4,
                                        h6("Normal GC:", style = "font-size: 12px; color: #6c757d; margin-bottom: 8px;"),
                                        checkboxGroupInput(
                                          "gc_normal",
                                          NULL,
                                          choices = list(
                                            "GC 30-55%" = "GC_30_55",
                                            "GC 55-60%" = "GC_55_60", 
                                            "GC 60-65%" = "GC_60_65",
                                            "GC 65-70%" = "GC_65_70"
                                          ),
                                          selected = character(0)
                                        )
                                 ),
                                 column(4,
                                        h6("High GC:", style = "font-size: 12px; color: #6c757d; margin-bottom: 8px;"),
                                        checkboxGroupInput(
                                          "gc_high",
                                          NULL,
                                          choices = list(
                                            "GC 70-75%" = "GC_70_75",
                                            "GC 75-80%" = "GC_75_80",
                                            "GC 80-85%" = "GC_80_85",
                                            "Very High (>85%)" = "GC_>85"
                                          ),
                                          selected = character(0)
                                        )
                                 )
                               )
                             )
                           ),
                           
                           # Other Regions (Collapsible)
                           div(
                             style = "margin-top: 15px;",
                             tags$a(
                               href = "#other_collapse",
                               `data-toggle` = "collapse",
                               style = "text-decoration: none; color: #495057;",
                               h6("▶ Other Regions",
                                  style = "margin-bottom: 10px; font-weight: bold; border-bottom: 1px solid #dee2e6; padding-bottom: 5px; cursor: pointer;")
                             ),
                             div(
                               id = "other_collapse",
                               class = "collapse",
                               style = "margin-top: 6px;",
                               div(
                                 h6("Complex Regions:", style = "font-size: 12px; color: #6c757d; margin-bottom: 8px;"),
                                 checkboxGroupInput(
                                   "complex_regions",
                                   NULL,
                                   choices = list(
                                     "MHC" = "MHC Region",
                                     "Segmental Duplications" = "Segmental Duplications",
                                     "Low Mappability" = "Low Mappability"
                                   ),
                                   selected = character(0),
                                   inline = TRUE
                                 )
                               )
                             )
                           )
                       )
                )
              ),
              
              # Quick selection buttons
              fluidRow(
                column(12,
                       div(style = "text-align: center; margin: 15px 0;",
                           actionButton("clear_all_regions", "Clear All", 
                                        class = "btn-outline-danger btn-sm")
                       )
                )
              ),
              
              # Update button
              div(style = "text-align: center; margin-top: 15px;",
                  actionButton(
                    "update_stratified",
                    "Update Analysis",
                    class = "btn-primary",
                    style = "padding: 10px 25px; font-size: 16px;"
                  )
              )
            ),
            
            # Results display
            conditionalPanel(
              condition = "output.has_stratified_data",
              
              # Summary info
              fluidRow(
                column(12,
                       div(
                         style = "background: #e9ecef; padding: 15px; margin-bottom: 20px; border-radius: 5px;",
                         htmlOutput("stratified_summary")
                       )
                )
              ),
              
              # Metric selection
              wellPanel(
                style = "background-color: #f8f9fa; margin-bottom: 20px;",
                div(
                  style = "display: flex; justify-content: center; gap: 15px;",
                  
                  div(
                    id = "metric-selection-container",
                    style = "display: flex; gap: 15px;",
                    
                    # F1 Score (selected by default)
                    div(
                      class = "metric-pill",
                      `data-value` = "f1_score",
                      style = "padding: 8px 16px; border-radius: 20px; cursor: pointer; font-weight: 500; font-size: 11px; border: 1px solid #007bff; background: #007bff; color: white; transition: all 0.2s ease; text-align: center;",
                      "F1 Score"
                    ),
                    
                    # Precision 
                    div(
                      class = "metric-pill",
                      `data-value` = "precision",
                      style = "padding: 8px 16px; border-radius: 20px; cursor: pointer; font-weight: 500; font-size: 11px; border: 1px solid #dee2e6; background: white; color: #6c757d; transition: all 0.2s ease; text-align: center;",
                      "Precision"
                    ),
                    
                    # Recall 
                    div(
                      class = "metric-pill",
                      `data-value` = "recall",
                      style = "padding: 8px 16px; border-radius: 20px; cursor: pointer; font-weight: 500; font-size: 11px; border: 1px solid #dee2e6; background: white; color: #6c757d; transition: all 0.2s ease; text-align: center;",
                      "Recall"
                    )
                  ),
                  
                  # Hidden radio button for Shiny
                  div(
                    style = "display: none;",
                    radioButtons(
                      "selected_metric",
                      NULL,
                      choices = list(
                        "F1" = "f1_score",
                        "Precision" = "precision", 
                        "Recall" = "recall"
                      ),
                      selected = "f1_score"
                    )
                  )
                )
              ),
              
              # Results tabs
              tabsetPanel(
                tabPanel("📊 Performance Plots",
                         br(),
                         fluidRow(
                           column(6,
                                  h4("SNP Performance by Region", style = "color: #d73027; font-weight: bold; text-align: center;"),
                                  br(),
                                  plotOutput("stratified_snp_plot", height = "600px")
                           ),
                           column(6,
                                  h4("INDEL Performance by Region", style = "color: #4575b4; font-weight: bold; text-align: center;"),
                                  br(),
                                  plotOutput("stratified_indel_plot", height = "600px")
                           )
                         )
                ),
                
                tabPanel("📋 Data Tables",
                         br(),
                         div(
                           h4("SNP Performance by Region", style = "color: #d73027; font-weight: bold; margin-bottom: 15px;"),
                           DT::dataTableOutput("snp_metrics_table"),
                           style = "margin-bottom: 40px;"
                         ),
                         
                         hr(style = "border-top: 2px solid #dee2e6; margin: 30px 0;"),
                         
                         div(
                           h4("INDEL Performance by Region", style = "color: #4575b4; font-weight: bold; margin-bottom: 15px;"),
                           DT::dataTableOutput("indel_metrics_table")
                         )
                ) 
              ) 
            ), 
            
            # No data message
            conditionalPanel(
              condition = "!output.has_stratified_data",
              div(
                class = "alert alert-warning",
                style = "margin-top: 30px; text-align: center;",
                h5("No Data to Display"),
                p("Please select some regions and experiments from previous tabs, then click 'Update Analysis'.")
              )
            )
          )
        ),
        
        # Upload modal
        upload_modal_ui()
    )
  )
)

# ============================================================================
# SERVER DEFINITION
# ============================================================================

server <- function(input, output, session) {
  
  # ====================================================================
  # INITIALIZE MODULES
  # ====================================================================
  
  # Setup upload functionality
  upload_components <- upload_server(input, output, session)
  
  # Setup core data processing (returns reactive values)
  data_reactives <- setup_data_reactives(input, output, session)
  
  # Setup all output functions (pass data_reactives to each)
  setup_plot_outputs(input, output, session, data_reactives)
  setup_table_outputs(input, output, session, data_reactives)
  setup_ui_outputs(input, output, session, data_reactives)
  setup_observers(input, output, session, data_reactives)
  
  # ====================================================================
  # HTML EXPORT FUNCTIONALITY
  # ====================================================================
  
  output$export_html_report <- downloadHandler(
    filename = function() {
      paste0("benchmarking_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    
    content = function(file) {
      # Get main visualization data
      viz_data <- data_reactives$viz_performance_data()
      
      # Get stratified data
      stratified_data <- NULL
      current_metric <- input$selected_metric %||% "f1_score"
      
      # Use tab 4 data if available
      if (data_reactives$stratified_triggered() && nrow(data_reactives$stratified_filtered_data()) > 0) {
        stratified_data <- data_reactives$stratified_filtered_data()
      }
      
      # Generate report
      html_content <- create_html_report(viz_data, stratified_data, current_metric)
      writeLines(html_content, file)
    }
  )
}

# ============================================================================
# APPLICATION LAUNCH
# ============================================================================

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))