# ============================================================================
# app.R - SNV Benchmarking Dashboard
# ============================================================================
"
Interactive web application for comparing variant calling performance across
different sequencing technologies (Illumina, PacBio, ONT, MGI) and variant 
calling algorithms (DeepVariant, GATK, Clair3). Analyzes hap.py benchmarking
results with stratified performance metrics across genomic regions.

Architecture:
- Frontend: R Shiny for interactive visualization  
- Backend: Python for data processing and database queries
- Database: SQLite for experiment metadata and results storage

Features:
- Experiment browsing with technology/caller filtering
- Interactive precision-recall plots with F1 contours
- Stratified analysis across genomic regions
- Upload interface for new hap.py results
- Delete dataset functionality
- HTML report export functionality

Main Components:
- Tab 1: Experiment Overview (browse and select experiments)
- Tab 2: Performance Results (performance metrics table)
- Tab 3: Visualizations (interactive plots)
- Tab 4: Stratified Analysis (regional performance)
- Sidebar: Filter controls and comparison modes

Module Files:
- constants.R: UI styling, JavaScript, dropdown options
- data_processing.R: Reactive data management and state handling
- plot_functions.R: Interactive visualization generation
- table_functions.R: Data table rendering and formatting
- observers.R: Event handling and user interaction logic
- ui_components.R: Dynamic UI elements and upload modal
- utils.R: Color schemes, helper functions, backend interface
"

# ============================================================================
# LIBRARIES AND DEPENDENCIES
# ============================================================================

# core and data manipulation
library(reticulate)
library(shiny)
library(shinyjs) #for auth
library(DT)
library(dplyr)
library(tidyr)
library(jsonlite)

# Visualization and UI
library(ggplot2)
library(plotly)
library(ggsci)
library(patchwork)
library(geomtextpath)
library(htmltools)
library(htmlwidgets)
library(shinyBS)

# ============================================================================
# SHINY OPTIONS
# ============================================================================

options(
  shiny.maxRequestSize = 100*1024^2,     # 100MB upload limit
  shiny.sanitize.errors = FALSE,          
  shiny.autoreload = FALSE
)

# Session timeouts for OAuth flow
shinyOptions(
  session.timeout = 3600000,              # 1 hour
  connection.timeout = 3600000
)

# ============================================================================
# PYTHON BACKEND INTERFACE
# ============================================================================

tryCatch({
  py_run_string("import sys")
  py_run_string("sys.path.append('../backend')")
  
  # Clear any cached old upload_handler module
 # py_run_string("if 'upload_handler' in sys.modules: del sys.modules['upload_handler']")
  
  db <<- import("db_interface")
  upload_handler <<- import("upload_handler")
}, error = function(e) {
  stop("Cannot connect to Python backend.")
})

# ============================================================================
# R MODULE IMPORTS
# ============================================================================

# Import R modules
source("constants.R")
source("utils.R")
source("auth.R")
source("dynamic_options.R")
source("data_processing.R")
source("plot_functions.R")
source("table_functions.R")
source("observers.R")
source("ui_components.R")
source("html_export.R") 


# global theme
theme_set(theme_bw())

# ============================================================================
# USER INTERFACE DEFINITION
# ============================================================================

ui <- fluidPage(
  
  useShinyjs(), #for auth
  # ====================================================================
  # HEAD SECTION - CSS and JavaScript
  # ====================================================================
  
  tags$head(
    # CSS styles
    tags$style(HTML(APP_CSS_STYLES)),
    tags$style(HTML(METADATA_CSS_STYLES)),
    tags$style(HTML(HIERARCHICAL_CHECKBOX_CSS)),
    
    # JavaScript 
    tags$script(HTML(TABLE_INTERACTION_JS)),
    tags$script(HTML(CUSTOM_MESSAGE_HANDLERS_JS)),
    tags$script(HTML(COLLAPSIBLE_HANDLERS_JS)),
    tags$script(HTML(METRIC_SELECTION_JS)),
    tags$script(HTML(PLOTLY_REFRESH_JS)),
    tags$script(HTML(HIERARCHICAL_CHECKBOX_JS)),
    tags$script(HTML(COMPARISON_BUTTON_JS))
  ),
  
  # ====================================================================
  # MAIN LAYOUT - SIDEBAR AND CONTENT
  # ====================================================================
  
  sidebarLayout(
    
    # SIDEBAR PANEL
    div(class = "sidebar",
      div(class = "sidebar-header",
                  # Logo and title container
                  actionLink("logo_home_btn",
                    div(style = "display: flex; align-items: center; gap: 12px; cursor: pointer;",
                        # Logo
                        img(src = "C3G_Logo.png", 
                            alt = "Reset Dashboard", 
                            style = "height: 50px; width: 55px; object-fit: contain;"),
                        # Title
                        h4("SNV Benchmarking Dashboard", 
                          style = "margin: 0; font-size: 1.25em; font-weight: 600; line-height: 1.2; color: white;")
                    ),

                  )
        ),
        div(class = "sidebar-content",
            
            # Filter Options Section
            h4("Filter Options:"),
            radioButtons(
              "filter_type",
              NULL,
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
            h4("Comparison Options:"),
            
            # Comparison mode buttons
            actionButton("compare_advanced", "Advanced Comparison",
                         class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("compare_experiments", "Manual Selection", 
                         class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            
            # Advanced comparison panel
            conditionalPanel(
              condition = "output.comparison_mode == 'advanced_comparison'",
              hr(style = "margin: 8px 0;"),
              
              # Compact header
              h5("Advanced Comparison", 
                 style = "margin-bottom: 8px; font-weight: 600; font-size: 13px;"),
              
              # Tech section
              div(
                style = "margin-bottom: 10px;",
                h6("Technologies (Click ▶ for Platforms):", 
                   style = "font-size: 12px; font-weight: 600; color: #4472ca; margin-bottom: 4px;"),
                uiOutput("tech_hierarchy_ui")
              ),
              
              # Caller section
              div(
                style = "margin-bottom: 10px;",
                h6("Callers (Click ▶ for Versions):", 
                   style = "font-size: 12px; font-weight: 600; color: #4472ca; margin-bottom: 4px;"),
                uiOutput("caller_hierarchy_ui")
              ),
              
              # Submit button
              uiOutput("advanced_comparison_submit_ui")
            ),
            
            # Manual experiment selection panel
            conditionalPanel(
              condition = "output.comparison_mode == 'manual_selection'",
              p("Click on experiments in the table to select them for comparison.", style = "font-size: 12px;"),
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
                    h5("Selected Experiments", class = "mb-0", style = "text-align: center")
                ),
                div(class = "panel-body", style = "padding: 5px;",
                    div(style = "max-height: 90px; overflow-y: auto; overflow-x: auto; border: 0.5px solid #dee2e6;",
                        tableOutput("compact_selected_experiments")
                    ),
                    br(),
                    div(style = "text-align: center;",
                        actionButton("submit_bottom_comparison", "Submit Selected Experiments",
                                     class = "btn-primary")
                    )
                )
              )
            )

        )),
    
    # MAIN CONTENT PANEL
    div(class = "main-content",
        width = 9,
        
        # Header with download, upload, and delete buttons
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 0; padding-bottom: 0;",
          
          div(), #left side
          # Right side - Action buttons
          div(
            style = "display: flex; gap: 8px; align-items: center; padding-top: 5px;",
            
            # Beta feature notice
            conditionalPanel(
              condition = "output.user_authenticated && !output.user_is_admin",
              div(
                style = "display: inline-flex; align-items: center; gap: 6px; padding: 5px 10px; background-color: #fff8e1; border-left: 3px solid #ffc107; border-radius: 3px; font-size: 11px; margin-right: 8px;",
                icon("info-circle", style = "color: #f57c00; font-size: 12px;"),
                span(style = "font-size: 13px;",
                  strong("Beta Access:"), " Authentication is active. Additional features for registered users will be available in upcoming releases."
                )
              )
            ),
            
            # Upload button - ONLY VISIBLE TO ADMIN _ FOR NOW
            conditionalPanel(
              condition = "output.user_is_admin",
              actionButton(
                "show_upload_modal", 
                label = tagList(icon("upload"), "Upload Dataset"),
                class = "btn-success btn-sm",
                style = "font-size: 13px; padding: 6px 12px; white-space: nowrap; min-width: 160px; border-radius: 3px;"
              )
            ),
            
            # Delete button - ONLY VISIBLE TO ADMIN
            conditionalPanel(
              condition = "output.user_is_admin",
              actionButton(
                "show_delete_modal", 
                label = tagList(icon("trash"), "Delete Datasets"),
                class = "btn-danger btn-sm",
                style = "font-size: 13px; padding: 6px 12px; white-space: nowrap; min-width: 160px; border: none !important;"
              )
            ),
            
            # Download button - ALWAYS VISIBLE (anonymous users can download)
            downloadButton(
              "export_html_report", 
              label = "Download Report",
              class = "btn-primary btn-sm",
              style = "font-size: 13px; padding: 6px 22px; white-space: nowrap; min-width: 160px; border: none !important; height: auto;"
            ),
             auth_ui()  # Authentication status/button from auth.R
          )
        ),
        
        
        br(),
        
        # Main tab panel
        tabsetPanel(
          id = "main_tabs",
          # TAB 1: EXPERIMENTS OVERVIEW
          tabPanel(
            "Experiments",
            div(class = "container-fluid", style = "width: 100%; padding: 0;",
                div(
                  class = "alert alert-info",
                  style = "margin-bottom: 20px;",
                  h5(icon("database"), " SNV Benchmarking Database - Comprehensive Variant Calling Evaluation"),
                  
                  p(strong("Overview:"), " This dashboard provides systematic evaluation and comparison of variant calling performance across multiple sequencing technologies and computational pipelines. Each experiment represents a controlled benchmarking analysis using ",
                    a("hap.py", href = "https://github.com/Illumina/hap.py", target = "_blank"),
                    " (a standard benchmarking tool) to compare variant calls against established genomic reference standards including ",
                    a("GIAB (Genome in a Bottle)", href = "https://www.nist.gov/programs-projects/genome-bottle", target = "_blank"), ", ",
                    a("CMRG", href = "https://data.nist.gov/od/id/mds2-2475", target = "_blank"), 
                    ", and ", a("T2T", href = "https://github.com/marbl/CHM13", target = "_blank"), " truth sets."),
                  
                  p(strong("Technologies Covered:"), " Compare performance across major sequencing platforms including ",
                    strong("Short-read sequencing"), " (",
                    a("Illumina", href = "https://www.illumina.com/", target = "_blank"), ", ",
                    a("MGI", href = "https://en.mgi-tech.com/", target = "_blank"), 
                    ") and ", strong("Long-read sequencing"), " (",
                    a("PacBio", href = "https://www.pacb.com/", target = "_blank"), ", ",
                    a("Oxford Nanopore (ONT)", href = "https://nanoporetech.com/", target = "_blank"), 
                    ") using variant callers including ",
                    a("DeepVariant", href = "https://github.com/google/deepvariant", target = "_blank"), " (ML-based), ",
                    a("GATK", href = "https://gatk.broadinstitute.org/", target = "_blank"), " (Traditional), ",
                    a("DRAGEN", href = "https://www.illumina.com/products/by-type/informatics-products/dragen-bio-it-platform.html", 
                      target = "_blank"), " (Hardware-accelerated), and ", 
                    a("Clair3", href = "https://github.com/HKU-BAL/Clair3", target = "_blank"), " (Long-read optimized)."),
                
                    div(
                    style = "margin-top: 12px; padding: 10px; background-color: #dce6f2; border-left: 4px solid #4472C4; border-radius: 4px;",
                    p(style = "margin: 0; font-size: 0.95em;",
                      icon("compass"), strong(" How to Navigate:"), br(),
                      "• Use ", strong("sidebar filters"), " to narrow by technology or caller | Results update in all tabs", br(),
                      "• Choose a ", strong("comparison option"), " to evaluate multiple experiments side-by-side", br(),
                      "• Expand ", strong("▶ arrows"), " for detailed experiment metadata" , br(),
                      "• To manually select: Click ", strong("Compare Specific Experiments"), " button, then", strong("select"), "table rows"    
                    )
                  )
                ),
                div(style = "width: 100%; overflow-x: auto;",
                    DT::dataTableOutput("experiments_table")
                )
            )
          ),
          
          # TAB 2: PERFORMANCE RESULTS
          tabPanel(
            "Performance Results", 
            div(
              class = "alert alert-info",
              style = "margin-bottom: 20px;",
              h5(icon("chart-line"), " Variant Calling Performance Metrics"),
              
              p(strong("Performance Overview:"), " Quantitative ",
                a("hap.py", href = "https://github.com/Illumina/hap.py", target = "_blank"),
                " benchmarking results showing ",
                a("precision, recall, and F1-score", href = "https://en.wikipedia.org/wiki/Precision_and_recall", target = "_blank"),
                " for each technology-caller combination against validated truth sets. ",
              p(strong("Key Metrics:"), " ",
                strong("Precision"), " measures accuracy of called variants (% true positives), ",
                strong("Recall"), " measures completeness (% of true variants detected), and ",
                strong("F1-score"), " provides balanced performance assessment. Higher percentages indicate better performance across all metrics."),
              
              div(
                style = "margin-top: 12px; padding: 10px; background-color: #dce6f2; border-left: 4px solid #4472C4; border-radius: 4px;",
                p(style = "margin: 0; font-size: 0.95em;",
                  icon("search"), strong(" Exploring Results:"), br(),
                  "• Results reflect experiments selected in ", strong("Tab 1"), " (filters or comparison modes)", br(),
                  "• Use ", strong("truth set filter"), " above to focus on specific reference standards", br(),
                  "• Click ", strong("column headers"), " to sort by any metric", br(),
                  "• Each experiment shows two rows: ", 
                  span(style = "color: #d73027; font-weight: bold;", "SNP"), " and ", 
                  span(style = "color: #4575b4; font-weight: bold;", "INDEL"), " variants"
                )
              )
              )
            ),
            #truthset filter 
            div(class = "truth-set-filter-panel",
              tags$label("Choose Reference Standards for Comparison:"),
              selectInput(
                "truth_set_filter_tab2",
                label = NULL,
                choices = TRUTH_SET_OPTIONS,
                selected = "All Truth Sets",
                width = "100%"
              ),
              tags$span(
                class = "info-icon",
                icon("info-circle"),
                title = "Filter experiments by benchmarking truth set",
                `data-toggle` = "tooltip"
              )
            ),
            DT::dataTableOutput("performance_table")
          ),
          
          # TAB 3: VISUALIZATIONS
          tabPanel(
            "Visualizations",
            fluidRow(
              column(12,
div(
             class = "alert alert-info",
             h5(icon("chart-area"), " Performance Characterization Plots"),
             p(strong("Performance Plots:"), " Precision vs. recall scatter plots with ",
               a("F1-score", href = "https://en.wikipedia.org/wiki/F-score", target = "_blank"),
               " contour lines for visual comparison of variant calling performance. ",
               "Curved lines represent constant F1-scores, helping identify optimal precision-recall balance. ",
               "Each point represents one experiment, colored by sequencing technology (",
               a("Illumina", href = "https://www.illumina.com/", target = "_blank"), ", ",
               a("PacBio", href = "https://www.pacb.com/", target = "_blank"), ", ",
               a("ONT", href = "https://nanoporetech.com/", target = "_blank"), ", ",
               a("MGI", href = "https://en.mgi-tech.com/", target = "_blank"),
               ") and shaped by variant caller (",
               a("DeepVariant", href = "https://github.com/google/deepvariant", target = "_blank"), ", ",
               a("GATK", href = "https://gatk.broadinstitute.org/", target = "_blank"), ", ",
               a("Clair3", href = "https://github.com/HKU-BAL/Clair3", target = "_blank"), ", ",
               a("DRAGEN", href = "https://www.illumina.com/products/by-type/informatics-products/dragen-bio-it-platform.html", 
                target = "_blank"), 
                ")."),
             
                      # interaction instructions
                      div(
                        style = "margin-top: 12px; padding: 10px; background-color: #dce6f2; border-left: 4px solid #4472C4; border-radius: 4px;",
                        p(style = "margin: 0; font-size: 0.95em;",
                          icon("mouse-pointer"), strong(" How to Interact:"), br(),
                          "• ", strong("Click and drag")," to zoom in |", strong("Double-click"), " to reset view", br(),
                          "• ", strong("Hover")," for quick performance metrics", br(),
                          "• ", strong("Click")," points and", strong("scroll down"),"to view detailed experiment metadata",
                        )
                      )
                    )
              )
            ),
            #truthset filter 
            div(class = "truth-set-filter-panel",
              tags$label("Choose Reference Standards for Comparison:"),
              selectInput(
                "truth_set_filter_tab3",
                label = NULL,
                choices = TRUTH_SET_OPTIONS,
                selected = "All Truth Sets",
                width = "100%"
              ),
              tags$span(
                class = "info-icon",
                icon("info-circle"),
                title = "Filter experiments by benchmarking truth set",
                `data-toggle` = "tooltip"
              )
            ),
            # Performance plots
            wellPanel(
              style = "background-color: #ffffff; padding: 25px; margin-bottom: 20px;",
              fluidRow(
                column(4, class = "plot-column",
                  style = "max-width: 33.33%; overflow: hidden;",
                  # SNP Plot title 
                  div(
                    style = "border-left: 3px solid #d32f2f; padding-left: 12px; margin-bottom: 5px; margin-left: 53px;",
                    h4("SNP Performance", 
                      style = "color: #d32f2f; font-weight: 600; margin: 0; 
                        font-size: 17px; letter-spacing: 0.3px;")
                  ),
                  plotlyOutput("snp_plot", height = "500px")
                ),
                column(4, class = "plot-column",
                  style = "max-width: 33.33%; overflow: hidden;",
                  # INDEL plot title
                  div(
                    style = "border-left: 3px solid #1976d2; padding-left: 12px; margin-bottom: 5px; margin-left: 53px;",
                    h4("INDEL Performance", 
                      style = "color: #1976d2; font-weight: 600; margin: 0; 
                        font-size: 17px; letter-spacing: 0.3px;")
                  ),
                  # Loading spinner
                  conditionalPanel(
                    condition = "!output.indel_plot",
                    div(
                      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 40px;",
                      div(class = "custom-spinner", style = "margin: 0 auto;"),
                      p("Loading plots...", 
                        style = "color: #6c757d; margin-top: 15px; font-size: 14px; text-align: center; margin-bottom: 0;")
                    )
                  ),
                  plotlyOutput("indel_plot", height = "500px") 
                ),
                column(4,
                  div(
                    # Chart Reference header 
                    div(
                      style = "border-left: 3px solid #7b2d8e; padding-left: 12px; margin-bottom: 5px; margin-left: 5px;",
                      h4("Chart Reference", 
                        style = "color: #7b2d8e; font-weight: 600; margin: 0; 
                          font-size: 17px; letter-spacing: 0.3px;")
                    ),
                    br(), br(),
                    htmlOutput("technology_legend"),
                    br(),
                    htmlOutput("caller_legend")
                  )
                )
              )
            ),
            # Selected experiment details
            br(),
            fluidRow(
              column(12,
                     conditionalPanel(
                       condition = "output.has_selected_point",
                       wellPanel(
                         style = "background-color: #f8f9fa; border-left: 4px solid #007bff; margin-top: 15px;",
                         fluidRow( 
                           column(10,
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
            div(
              class = "alert alert-info",
              style = "margin-bottom: 20px;",
              h5(icon("layer-group"), " Stratified Performance Analysis"),
              
              p(strong("Regional Breakdown:"), " Performance metrics displayed across genomic regions with different sequence characteristics. 
                Available stratifications include complexity-based regions (easy/difficult), GC content ranges, functional annotations (coding/non-coding), 
                repetitive sequences (homopolymers, tandem repeats, segmental duplications, satellites), and specialized regions (MHC, low mappability areas). 
                These stratifications follow ",
                a("GIAB genome stratification standards", href = "https://github.com/genome-in-a-bottle/genome-stratifications", target = "_blank"), "."),
              
              div(
                style = "margin-top: 12px; padding: 10px; background-color: #dce6f2; border-left: 4px solid #4472C4; border-radius: 4px;",
                p(style = "margin: 0; font-size: 0.95em;",
                  icon("sliders-h"), strong(" How to Use:"), br(),
                  "• Results reflect experiments selected in ", strong("Tab 1"), br(),
                  "• Choose ", strong("genomic regions"), " below (expand ▶ sections for more options)", br(),
                  "• Click ", strong("Update Analysis"), " to generate stratified results"
                )
              )
            ),
            #truthset filter 
            div(class = "truth-set-filter-panel",
              tags$label("Choose Reference Standards for Comparison:"),
              selectInput(
                "truth_set_filter_tab4",
                label = NULL,
                choices = TRUTH_SET_OPTIONS,
                selected = "All Truth Sets",
                width = "100%"
              ),
              tags$span(
                class = "info-icon",
                icon("info-circle"),
                title = "Filter experiments by benchmarking truth set",
                `data-toggle` = "tooltip"
              )
            ),
            # Region selection panel
            wellPanel(
              style = "background-color: #f8f9fa; margin-bottom: 20px;",
              h5("Select Regions to Analyze", style = "margin-top: 0;"),
              
              fluidRow(
                column(12,
                       div(style = "background: white; border: 1px solid #dee2e6; border-radius: 5px; padding: 20px;",

                        # Primary Stratifications
                        div(
                          h6("Primary Stratifications", 
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
                          ),
                          tags$script(HTML("
                $(document).ready(function(){
                  $('input[value=\"Easy Regions\"]').parent().attr({
                    'data-toggle': 'tooltip',
                    'title': 'High-confidence regions with unique sequences and good mappability'
                  });
                  $('input[value=\"Difficult Regions\"]').parent().attr({
                    'data-toggle': 'tooltip', 
                    'title': 'Challenging regions with repetitive sequences, low complexity, or structural variations'
                  });
                });
              "))
                        ),
                        
                        # Functional regions
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
                                "Non-CDS Regions" = "Non-CDS Regions"
                              ),
                              selected = character(0),
                              inline = TRUE
                            ),
                            tags$script(HTML("
                            $(document).ready(function(){
                              $('input[value=\"RefSeq CDS\"]').parent().attr({
                                'data-toggle': 'tooltip',
                                'title': 'Protein-coding DNA sequences from NCBI Reference Sequence database'
                              });
                              $('input[value=\"Non-CDS Regions\"]').parent().attr({
                                'data-toggle': 'tooltip',
                                'title': 'Non-coding genomic regions including introns, intergenic, and regulatory sequences'
                              });
                            });
                          "))
                          )
                        ),
                        
                        # Repetitive DNA Regions
                        div(
                          style = "margin-top: 15px;",
                          tags$a(
                            href = "#repetitive_collapse",
                            `data-toggle` = "collapse",
                            style = "text-decoration: none; color: #495057;",
                            h6("▶ Repetitive DNA Regions",
                              style = "margin-bottom: 10px; font-weight: bold; border-bottom: 1px solid #dee2e6; padding-bottom: 5px; cursor: pointer;")
                          ),
                          div(
                            id = "repetitive_collapse",
                            class = "collapse",
                            style = "margin-top: 6px;",
                            
                            # Simple Repeats subsection
                            div(
                              style = "margin-bottom: 10px;",
                              h6("Simple Repeats:", style = "font-size: 13px; color: #6c757d; margin-bottom: 5px; font-weight: 600;"),
                              checkboxGroupInput(
                                "homopolymer_regions",
                                NULL,
                                choices = list(
                                  "Homopolymer 4-6bp" = "Homopolymer 4-6bp",
                                  "Homopolymer 7-11bp" = "Homopolymer 7-11bp",
                                  "Homopolymer >12bp" = "Homopolymer >12bp",
                                  "Homopolymer ≥21bp" = "Homopolymer ≥21bp"
                                ),
                                selected = character(0),
                                inline = TRUE
                              )
                            ),
                            
                            # Tandem Repeats subsection
                            div(
                              style = "margin-bottom: 10px;",
                              h6("Tandem Repeats:", style = "font-size: 13px; color: #6c757d; margin-bottom: 5px; font-weight: 600;"),
                              checkboxGroupInput(
                                "satellites_regions",
                                NULL,
                                choices = list(
                                  "Satellites" = "Satellites",
                                  "All Tandem Repeat & Homopolymers" = "All TR & Homopolymers"
                                ),
                                selected = character(0),
                                inline = TRUE
                              )
                            ),
                            
                            # Non-Repetitive subsection
                            div(
                              h6("Non-Repetitive:", style = "font-size: 13px; color: #6c757d; margin-bottom: 5px; font-weight: 600;"),
                              checkboxGroupInput(
                                "non_repetitive_regions",
                                NULL,
                                choices = list(
                                  "Non-Satellites" = "Non-Satellites",
                                  "Non-Tandem Repeat & Non-Homopolymers" = "Non-TR & Non-Homopolymers"
                                ),
                                selected = character(0),
                                inline = TRUE
                              )
                            ),
                            
                            # Tooltips for repetitive regions
                            tags$script(HTML("
                              $(document).ready(function(){
                                $('input[value=\"Homopolymer 4-6bp\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Short repetitive single-nucleotide runs (4-6 consecutive identical bases)'
                                });
                                $('input[value=\"Homopolymer 7-11bp\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Medium-length repetitive single-nucleotide runs (7-11 consecutive identical bases)'
                                });
                                $('input[value=\"Homopolymer >12bp\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Long repetitive single-nucleotide runs (>12 consecutive identical bases)'
                                });
                                $('input[value=\"Homopolymer ≥21bp\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Very long repetitive single-nucleotide runs (≥21 consecutive identical bases)'
                                });
                                $('input[value=\"Satellites\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Short tandem repeats often found in centromeric and pericentromeric regions'
                                });
                                $('input[value=\"All TR & Homopolymers\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'All tandem repeats and homopolymer regions combined'
                                });
                                $('input[value=\"Non-Satellites\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Regions excluding satellite DNA sequences'
                                });
                                $('input[value=\"Non-TR & Non-Homopolymers\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Regions excluding all tandem repeats and homopolymers'
                                });
                                $('[data-toggle=\"tooltip\"]').tooltip();
                              });
                            "))
                          )
                        ),
                        
                        # Structural Complexity
                        div(
                          style = "margin-top: 15px;",
                          tags$a(
                            href = "#complex_collapse",
                            `data-toggle` = "collapse",
                            style = "text-decoration: none; color: #495057;",
                            h6("▶ Structural Complexity",
                              style = "margin-bottom: 10px; font-weight: bold; border-bottom: 1px solid #dee2e6; padding-bottom: 5px; cursor: pointer;")
                          ),
                          div(
                            id = "complex_collapse",
                            class = "collapse",
                            style = "margin-top: 6px;",
                            checkboxGroupInput(
                              "complex_regions",
                              NULL,
                              choices = list(
                                "Segmental Duplications" = "Segmental Duplications",
                                "Non-Segmental Duplications" = "Non-Segmental Duplications",
                                "Low Mappability" = "Low Mappability",
                                "Non-Low Mappability" = "Non-Low Mappability",
                                "MHC Region" = "MHC Region"
                              ),
                              selected = character(0),
                              inline = TRUE
                            ),
                            tags$script(HTML("
                              $(document).ready(function(){
                                $('input[value=\"Segmental Duplications\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Large blocks of duplicated DNA (≥1kb, >90% identity)'
                                });
                                $('input[value=\"Non-Segmental Duplications\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Regions excluding segmental duplications'
                                });
                                $('input[value=\"Low Mappability\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Difficult-to-sequence regions where reads map ambiguously'
                                });
                                $('input[value=\"Non-Low Mappability\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Regions with good read mappability'
                                });
                                $('input[value=\"MHC Region\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Major Histocompatibility Complex - highly polymorphic immune system genes'
                                });
                                $('[data-toggle=\"tooltip\"]').tooltip();
                              });
                            "))
                          )
                        ),
                        
                        # GC Composition
                        div(
                          style = "margin-top: 15px;",
                          tags$a(
                            href = "#gc_collapse",
                            `data-toggle` = "collapse",
                            style = "text-decoration: none; color: #495057;",
                            h6("▶ GC Composition",
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
                            ),
                            h6("Extreme GC:", style = "font-size: 12px; color: #6c757d; margin-bottom: 8px;"),
                            checkboxGroupInput(
                              "gc_extreme",
                              NULL,
                              choices = list(
                                "GC <25% or >65%" = "GC <25 or >65",
                                "GC <30% or >55%" = "GC <30 or >55"
                              ),
                              selected = character(0),
                              inline = FALSE
                            ),
                            tags$script(HTML("
                              $(document).ready(function(){
                                $('input[value=\"GC <25 or >65\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Extreme GC content regions: either very low (<25%) or very high (>65%)'
                                });
                                $('input[value=\"GC <30 or >55\"]').parent().attr({
                                  'data-toggle': 'tooltip',
                                  'title': 'Moderate-extreme GC content regions: either low (<30%) or high (>55%)'
                                });
                                $('[data-toggle=\"tooltip\"]').tooltip();
                              });
                            "))
                          )
                        ),
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
              ),
              
              # Initialize tooltips
              tags$script(HTML(REGION_TOOLTIPS_JS))
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
              create_experiment_details_panel_ui(),
              # Results tabs
              tabsetPanel(
                tabPanel("📊 Performance Plots",
                         br(),
                         div(
                           style = "background-color: #ffffff; padding: 25px; margin-bottom: 20px;",
                           fluidRow(
                             column(6,
                                    #SNV title card
                                    div(
                                      style = "background: linear-gradient(135deg, #fefafa 0%, #fdf5f5 100%); 
                                       border: 1px solid #ef9a9a; border-radius: 8px; 
                                       padding: 10px 16px; margin-bottom: 18px; text-align: center;
                                       box-shadow: 0 1px 2px rgba(239, 154, 154, 0.1);",
                                      h4("SNP Performance by Region", 
                                         style = "color: #d32f2f; font-weight: 600; margin: 0; 
                                        font-size: 17px; letter-spacing: 0.3px;")
                                    ),
                                    br(),
                                    conditionalPanel(
                                      condition = "!output.stratified_snp_plot",
                                      
                                      div(
                                        style = "text-align: right; padding: 10px;",
                                        div(class = "custom-spinner", style = "margin-left: auto; margin-right: 0;"),
                                        p("Loading plots...", 
                                          style = "color: #6c757d; margin-top: 15px; font-size: 14px; margin-bottom: 0;")
                                      )
                                    ),
                                    plotOutput("stratified_snp_plot", height = "600px")
                             ),
                             column(6,
                                    # INDEL title card
                                    div(
                                      style = "background: linear-gradient(135deg, #fafbfd 0%, #f5f7fc 100%); 
                                       border: 1px solid #9fa8da; border-radius: 8px; 
                                       padding: 10px 16px; margin-bottom: 18px; text-align: center;
                                       box-shadow: 0 1px 2px rgba(159, 168, 218, 0.1);",
                                      h4("INDEL Performance by Region", 
                                         style = "color: #1976d2; font-weight: 600; margin: 0; 
                                        font-size: 17px; letter-spacing: 0.3px;")
                                    ),
                                    br(),
                                    plotOutput("stratified_indel_plot", height = "600px")
                             )
                           )
                         )
                ),
                
                tabPanel("📋 Data Tables",
                         br(),
                         div(
                           div(
                             style = "background: linear-gradient(135deg, #fefafa 0%, #fdf5f5 100%); 
                             border: 1px solid #ef9a9a; border-radius: 8px; 
                             padding: 10px 16px; margin-bottom: 18px; text-align: center;
                             box-shadow: 0 1px 2px rgba(239, 154, 154, 0.1);",
                             h4("SNP Performance by Region", 
                                style = "color: #d32f2f; font-weight: 600; margin: 0; 
                                 font-size: 17px; letter-spacing: 0.3px;")
                           ),
                           DT::dataTableOutput("snp_metrics_table"),
                           style = "margin-bottom: 40px;"
                         ),
                         
                         hr(style = "border-top: 2px solid #dee2e6; margin: 30px 0;"),
                         
                         div(
                           div(
                             style = "background: linear-gradient(135deg, #fafbfd 0%, #f5f7fc 100%); 
                                       border: 1px solid #9fa8da; border-radius: 8px; 
                                       padding: 10px 16px; margin-bottom: 18px; text-align: center;
                                       box-shadow: 0 1px 2px rgba(159, 168, 218, 0.1);",
                             h4("INDEL Performance by Region", 
                                style = "color: #1976d2; font-weight: 600; margin: 0; 
                              font-size: 17px; letter-spacing: 0.3px;")
                           ),
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
        upload_modal_ui(),
        delete_modal_ui()
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
  
  # Initialize auth 
  auth_server(input, output, session)

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
      disable("export_html_report")
      html("export_html_report", 
          '<i class="fa fa-spinner fa-spin"></i> Generating...')
      
      paste0("benchmarking_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    
    content = function(file) {
      on.exit({
        enable("export_html_report")
        html("export_html_report", 
            'Download Report')
      })
      
      viz_data <- data_reactives$viz_performance_data()
      
      stratified_data <- NULL
      current_metric <- input$selected_metric %||% "f1_score"
      
      if (data_reactives$stratified_triggered() && nrow(data_reactives$stratified_filtered_data()) > 0) {
        stratified_data <- data_reactives$stratified_filtered_data()
      }
      
      html_content <- create_html_report(viz_data, stratified_data, current_metric)
      writeLines(html_content, file)
    }
  )
}

# ============================================================================
# APPLICATION LAUNCH
# ============================================================================
shinyApp(ui = ui, server = server)