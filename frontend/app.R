# =============================================================================
# LIBRARIES & SETUP
# =============================================================================

library(reticulate)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggsci)
library(patchwork)
library(geomtextpath)
library(htmltools)
library(htmlwidgets)
library(jsonlite)

# Import database interface
py_run_string("import sys")
py_run_string("sys.path.append('../backend')")
db <- import("db_interface")

# Import html generation code
source("html_export.R")

# Set theme
theme_set(theme_bw())

# =============================================================================
# CONSTANTS & CONFIGURATION  
# =============================================================================

# Color and shape mappings for visualization
technology_colors <- c(
  "ILLUMINA" = "#F8766D",    
  "PACBIO" = "#C77CFF",     
  "ONT" = "#00BFC4",         
  "MGI" = "#7CAE00",        
  "Unknown" = "#E76BF3"   
)
caller_shapes <- c(
  "DEEPVARIANT" = 16,        # Circle 
  "GATK" = 17,              # Triangle
  "CLAIR3" = 15,            # Square 
  "Unknown" = 4             # X 
)
# shape conversion to HTML
shape_symbols = c("16" = "●", "17" = "▲", "15" = "■", "4" = "✕")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || is.na(x) || x == "") y else x

# Helper function for F1 contours
create_f1_contour <- function() {
  f1_contour_function <- function(p, r) {
    result <- 2 * (p * r) / (p + r)
    result[!is.finite(result)] <- NA
    return(result)
  }
  
  # Create grid for contour calculation
  p_seq <- seq(0.01, 0.99, length.out = 100)
  r_seq <- seq(0.01, 0.99, length.out = 100)
  
  contour_data <- expand.grid(p = p_seq, r = r_seq) %>%
    mutate(f1 = f1_contour_function(p, r)) %>%
    filter(!is.na(f1) & is.finite(f1))
  
  return(contour_data)
}
  # JSON helper function 
  json_param <- function(data) {
    if (is.null(data) || length(data) == 0) {
      return("[]")
    }
    jsonlite::toJSON(data,auto_unbox = TRUE)
  }
  
# ============================================================================
# MANUAL HTML LEGEND CREATION FUNCTIONS
# ============================================================================

create_technology_legend <- function() {
  legend_items <- ""
  
  for (tech in names(technology_colors)) {
    if (tech != "Unknown") {
      color <- technology_colors[tech]
      legend_items <- paste0(legend_items, 
                             '<div style="display: flex; align-items: center; margin-bottom: 5px;">',
                             '<div style="width: 12px; height: 12px; background-color: ', color, '; border-radius: 50%; margin-right: 8px; border: 1px solid #333;"></div>',
                             '<span style="font-size: 12px;">', tech, '</span>',
                             '</div>'
      )
    }
  }
  
  return(paste0(
    '<div style="background: white; padding: 10px; border: 1px solid #ddd; border-radius: 5px; margin-bottom: 10px;">',
    '<div style="font-weight: bold; margin-bottom: 8px; font-size: 13px;">Sequencing Technology</div>',
    legend_items,
    '</div>'
  ))
}

create_caller_legend <- function() {
  legend_items <- ""
  
  for (caller in names(caller_shapes)) {
    if (caller != "Unknown") {
      shape_code <- as.character(caller_shapes[caller])
      symbol <- shape_symbols[shape_code]
      
      legend_items <- paste0(legend_items,
                             '<div style="display: flex; align-items: center; margin-bottom: 5px;">',
                             '<span style="font-size: 14px; margin-right: 8px; width: 12px; text-align: center; color: #333;">', symbol, '</span>',
                             '<span style="font-size: 12px;">', caller, '</span>',
                             '</div>'
      )
    }
  }
  
  return(paste0(
    '<div style="background: white; padding: 10px; border: 1px solid #ddd; border-radius: 5px;">',
    '<div style="font-weight: bold; margin-bottom: 8px; font-size: 13px;">Variant Caller</div>',
    legend_items,
    '</div>'
  ))
}

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- fluidPage(
  
  div(
    h3("SNV Benchmarking Dashboard", 
       style = "color: #007bff; font-weight: 600; margin-bottom: 20px; font-size: 1.7em;")
  ),
  
  #CSS for row expansion and full metadata info
  tags$head(
    tags$style(HTML("
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
    
    /* Row expansion section titles */
    .detail-section h6 {
      margin: 0 0 8px 0;
      color: #007bff;
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0.5px;
      font-weight: 600;
      border-bottom: 1px solid #dee2e6;
      padding-bottom: 4px;
    }
    
    /* Row expansion metadata items */
    .detail-item {
      margin-bottom: 4px;
      color: #6c757d;
      font-size: 11px;
      line-height: 1.3;
    }
    .detail-item strong {
      color: #495057;
      font-weight: 600;
    }
    
    /* Visualization tab 4-column grid */
    .metadata-grid-4col {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
      gap: 20px;
      margin-top: 15px;
    }
    
    /* Visualization tab cards */
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
    
    /* Visualization tab card titles */
    .metadata-card h6 {
      color: #495057;
      margin: 0 0 15px 0;
      font-size: 14px;
      font-weight: 600;
      border-bottom: 2px solid #007bff;
      padding-bottom: 8px;
    }
    
    /* Visualization tab metadata items */
    .metadata-item {
      margin-bottom: 8px;
      color: #6c757d;
      font-size: 14px;
      line-height: 1.4;
    }
    .metadata-item strong {
      color: #007bff;
      font-weight: 600;
      display: inline-block;
      min-width: 120px;
    }
    
    /* Mobile responsive */
    @media (max-width: 768px) {
      .metadata-grid-4col {
        grid-template-columns: 1fr;
      }
      .detail-grid {
        grid-template-columns: 1fr;
      }
    }
  "))
  ),
  
    tags$script(HTML("
    /* Table row expansion toggle */
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
  
    tags$script(HTML("
    /* Insert metadata rows from R server */
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
  
  sidebarLayout(
    
    # -------------------------------------------------------------------------
    # SIDEBAR PANEL
    # -------------------------------------------------------------------------
    sidebarPanel(
      width = 3,
      
      h4("Filter Options:"),
      
      # Choose filter type
      radioButtons(
        "filter_type",
        "Filter by:",
        choices = list(
          "Show All" = "none",
          "Technology" = "tech", 
          "Variant Caller" = "caller"
        ),
        selected = "none"
      ),
      
      # Technology filter dropdown
      conditionalPanel(
        condition = "input.filter_type == 'tech'",
        selectInput(
          "technology",
          "Choose Technology:",
          choices = c("ILLUMINA", "PACBIO", "ONT", "MGI"),
          selected = "ILLUMINA"
        )
      ),
      
      # Caller filter dropdown 
      conditionalPanel(
        condition = "input.filter_type == 'caller'",
        selectInput(
          "caller",
          "Choose Caller:",
          choices = c("DEEPVARIANT", "GATK", "CLAIR3"),
          selected = "DEEPVARIANT"
        )
      ),
      
      # ====================================================================
      # COMPARISON BUTTONS SECTION
      # ====================================================================
      hr(),
      h4("Comparison Options:"),
      
      # Compare Technologies Button
      actionButton(
        "compare_techs",
        "Compare Sequencing Technologies",
        class = "btn-primary",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      # Compare Callers Button  
      actionButton(
        "compare_callers", 
        "Compare Variant Callers",
        class = "btn-success",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      # Compare Specific Experiments Button
      actionButton(
        "compare_experiments",
        "Compare Specific Experiments", 
        class = "btn-warning",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      # ====================================================================
      # TECHNOLOGY COMPARISON PANEL
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'tech_comparison'",
        hr(),
        h5("Technology Comparison Setup:"),
        
        # Select multiple technologies
        checkboxGroupInput(
          "selected_technologies",
          "Select technologies (2 or more):",
          choices = list(
            "Illumina" = "ILLUMINA",
            "PacBio" = "PACBIO",
            "ONT" = "ONT", 
            "MGI" = "MGI"
          )
        ),
        
        # Select one caller (control)
        selectInput(
          "tech_comparison_caller",
          "Choose a caller (for all):",
          choices = c("DeepVariant" = "DEEPVARIANT", 
                      "GATK" = "GATK", 
                      "Clair3" = "CLAIR3"),
          selected = "DEEPVARIANT"
        ),
        
        # Submit button for tech comparison
        conditionalPanel(
          condition = "input.selected_technologies && input.selected_technologies.length >= 2",
          actionButton(
            "submit_tech_comparison",
            "Submit Technology Comparison",
            class = "btn-primary",
            style = "width: 100%;"
          )
        ),
        
        conditionalPanel(
          condition = "!input.selected_technologies || input.selected_technologies.length < 2",
          p("Please select at least 2 technologies", style = "color: red; font-size: 12px;")
        )
      ),
      
      # ====================================================================
      # CALLER COMPARISON PANEL
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'caller_comparison'",
        hr(),
        h5("Caller Comparison Setup:"),
        
        # Select multiple callers
        checkboxGroupInput(
          "selected_callers",
          "Select callers (2 or more):",
          choices = list(
            "DeepVariant" = "DEEPVARIANT",
            "GATK" = "GATK",
            "Clair3" = "CLAIR3"
          )
        ),
        
        # Select one technology (control)
        selectInput(
          "caller_comparison_tech",
          "Choose a technology (for all):",
          choices = c("Illumina" = "ILLUMINA", 
                      "PacBio" = "PACBIO", 
                      "ONT" = "ONT",
                      "MGI" = "MGI"),
          selected = "ILLUMINA"
        ),
        
        # Submit button for caller comparison
        conditionalPanel(
          condition = "input.selected_callers && input.selected_callers.length >= 2",
          actionButton(
            "submit_caller_comparison",
            "Submit Caller Comparison",
            class = "btn-success",
            style = "width: 100%;"
          )
        ),
        
        conditionalPanel(
          condition = "!input.selected_callers || input.selected_callers.length < 2",
          p("Please select at least 2 callers", style = "color: red; font-size: 12px;")
        )
      ),
      
      # ====================================================================
      # EXPERIMENT SELECTION INFO PANEL
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'manual_selection'",
        p("Click on experiments in the table select them for comparison.", style ="font-size: 15px;"),
        
        # Show selected count
        textOutput("selected_experiments_count"),
        
        # Clear selection button
        actionButton(
          "clear_experiment_selection",
          "Clear Selection",
          class = "btn-secondary btn-sm",
          style = "width: 100%"
        )
      ),
      
      # ====================================================================
      # SELECTED EXPERIMENTS DISPLAY (Bottom of sidebar)
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'manual_selection' && output.has_selected_experiments",
        hr(),
        div(
          class = "panel panel-info",
          div(class = "panel-heading d-flex justify-content-between align-items-center", #heading
              h5("Selected Experiments for Comparison", class = "mb-0"),
              span(class = "badge badge-info rounded-pill", 
                   textOutput("selected_count_badge", inline = TRUE))
          ),
          div(class = "panel-body", style = "padding: 10px;", #content (table)
              div(style = "max-height: 90px; overflow-y: auto; overflow-x: auto; border: 1px solid #dee2e6;",
                  tableOutput("compact_selected_experiments")
              ),
              br(),
              div(style = "text-align: center;", #submit button
                  actionButton(
                    "submit_bottom_comparison",
                    "Submit Selected Experiments",
                    class = "btn-warning"
                  )
              )
          )
        )
      ),
    ),
    
    # -------------------------------------------------------------------------
    # MAIN PANEL
    # -------------------------------------------------------------------------
    mainPanel(
      width = 9,
      
      # Export button (top right)
      div(
        style = "display: flex; justify-content: space-between; align-items: center; 
             margin-bottom: 0; padding-bottom: 0;",
        div(
          style = "flex-grow: 1;",
        ),
        div(
          style = "margin-left: 20px; padding-top: 5px;",
          downloadButton(
            "export_html_report", 
            label = tagList( "Export Report"),
            class = "btn-primary btn-sm",
            style = "font-size: 14px; padding: 6px 12px; white-space: nowrap;"
          )
        )
      ),
      
      br(),
      
      tabsetPanel(
        
        # ====================================================================
        # TAB 1: EXPERIMENT OVERVIEW
        # ====================================================================
        tabPanel(
          "Experiments",
          br(),
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
            p(style = "font-size: 0.9em; margin-bottom: 0;"
              , strong("Navigation:"), " Click the ▶ button in any row to expand detailed metadata, or switch to other tabs to view performance results and visualizations.")
          ),
          DT::dataTableOutput("experiments_table")
        ),
        
        # ====================================================================
        # TAB 2: PERFORMANCE RESULTS 
        # ====================================================================
        tabPanel(
          "Performance Results", 
          br(),
          # Add informative message
          div(
            class = "alert alert-info",
            style = "margin-bottom: 20px;",
            h5("Performance Results"),
            p("This table shows detailed performance metrics for each experiment. Each experiment displays ", 
              strong("two rows"), ": one for ", 
              span(style = "color: #d73027; font-weight: bold;", "SNP variants"), 
              " and one for ", 
              span(style = "color: #4575b4; font-weight: bold;", "INDEL variants"), 
              ". Metrics are shown as percentages for easy comparison."),
          ),
          DT::dataTableOutput("performance_table")
        ),
        
        # ====================================================================
        # TAB 3: VISUALIZATIONS (PRECISION/RECALL)
        # ====================================================================
        tabPanel(
          "Visualizations",
          br(),
          fluidRow(
            column(12,
                   div(
                     class = "alert alert-info",
                     h5(" Performance Visualizations"),
                     p("These scatter plots display precision vs recall performance for each experiment, with ", 
                       strong("F1 contour lines"), " showing performance benchmarks. "),
                     p(strong("Click points"), " and scroll down to view experiment details below, or ", strong("hover"), " for quick metrics"),
                     p(style = "font-size: 0.9em; color: #6c757d;", "Tip: Drag to zoom, double-click to reset")
                   )
            )
          ),
          br(),
          fluidRow(
            # SNP Plot Column 
            column(4,
                   h4("SNP Performance"),
                   plotlyOutput("snp_plot", height = "500px")
            ),
            # INDEL Plot Column 
            column(4,
                   h4("INDEL Performance"), 
                   plotlyOutput("indel_plot", height = "500px")
            ),
            # LEGENDS Column 
            column(3,
                   br(), br(), br(), br(),
                   htmlOutput("technology_legend"),
                   br(),
                   htmlOutput("caller_legend")
            )
          ),
          # Selected point/experiment details 
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
                                htmlOutput("basic_experiment_info")  # General metadata details
                         ),
                         column(2,
                                div(style = "text-align: right; padding-top: 10px;",
                                    actionButton("expand_metadata", "Show All Details", 
                                                 class = "btn-primary btn-sm")
                                )
                         )
                       ),
                       
                       # Expandable full metadata section
                       conditionalPanel(
                         condition = "input.expand_metadata % 2 == 1",
                         hr(),
                         htmlOutput("full_experiment_metadata") # Full metadata
                       )
                     )
                   )
            )
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER DEFINITION
# =============================================================================

server <- function(input, output, session) {
  
  # ====================================================================
  # 1. REACTIVE VALUES FOR TRACKING STATE
  # ====================================================================
  
  current_mode <- reactiveVal("filter")  # App mode: "filter", "tech_comparison", "caller_comparison", "manual_selection"
  
  display_experiment_ids <- reactiveVal(numeric(0))  # Experiemnt IDs to show in tables/plots
  
  table_selected_ids <- reactiveVal(numeric(0))      # IDs selected by clicking table rows
  
  plot_clicked_id <- reactiveVal(NULL)               # Single ID from clicking plot points
  
  # Comparison state
  comparison_submitted <- reactiveVal(FALSE)         # Whether any comparison has been submitted
  comparison_type <- reactiveVal(NULL)               # "technology" or "caller" 
  comparison_results <- reactiveVal(numeric(0))     # IDs from submitted comparison
  
  # Track which rows are expanded
  expanded_rows <- reactiveVal(character(0))
  # ====================================================================
  # 2. DATA PROCESSING FUNCTIONS 
  # ====================================================================
  
  # 2.1
  # Get experiment IDs based on filter
  experiment_ids <- reactive({
    # use specific experiments displayed 
    if (length(display_experiment_ids()) > 0) {
      return(display_experiment_ids())
    }
    
    # filtering
    if (input$filter_type == "tech") {
      return(db$get_experiments_by_technology(input$technology))
    } else if (input$filter_type == "caller") {
      return(db$get_experiments_by_caller(input$caller))
    } else {
      overview <- db$get_experiments_overview()
      return(overview$id)
    }
  })
  
  # 2.2
  # Get overview metadata for selected experiments
  experiments_data <- reactive({
    # Submitted comparisons (tech/caller)
    if (comparison_submitted() && length(comparison_results()) > 0) {
      exp_ids_json <- json_param(comparison_results()) # IDs in JSON format
      return(db$get_experiments_overview(NULL, exp_ids_json))
    }
    # Manual selection
    if (current_mode() == "manual_selection") {
      filters <- NULL
      if (input$filter_type == "tech") {
        filters <- list(technology = input$technology)
      } else if (input$filter_type == "caller") {
        filters <- list(caller = input$caller)
      }
      return(db$get_experiments_overview(filters, NULL))
    }
    
    # Regular filtering
    filters <- NULL
    if (input$filter_type == "tech") {
      filters <- list(technology = input$technology)
    } else if (input$filter_type == "caller") {
      filters <- list(caller = input$caller)
    }
    
    return (db$get_experiments_overview(filters, NULL))
    

    
  })
  
  # 2.3
  # experiment IDs for performance
  performance_experiment_ids <- reactive({
    if (current_mode() == "manual_selection") {
      # Selected experiment IDs
      return(table_selected_ids())
    } else if (comparison_submitted()) {
      # Comparison results IDs
      return(comparison_results())
    } else {
      # Regular filtering IDs
      return(experiment_ids())
    }
  })
  
  # 2.4
  # Complete metadata and performance results for visualization
  viz_performance_data <- reactive({
    ids <- performance_experiment_ids()
    
    if (length(ids) == 0) {
      return(data.frame())
    }
    
    tryCatch({ # input IDs in JSON format
      ids_json <- json_param(ids)
      perf_data <- db$get_performance_results(ids_json, c('SNP', 'INDEL'))
      metadata <- db$get_experiment_metadata(ids_json)
      
      # Filter and join performance data with metadata
      enhanced_data <- perf_data %>%
        filter(subset == "ALL_REGIONS" | subset == "*") %>%
        filter(!is.na(recall) & !is.na(precision) & !is.na(f1_score)) %>% #remove incomplete data
        left_join(metadata, by = c("experiment_id" = "id"), suffix = c("", "_meta")) # join all data
      
      return(enhanced_data)
      
    }, error = function(e) {
      cat("Error in viz_performance_data:", e$message, "\n")
      return(data.frame())
    })
  })
  
  # 2.5
  #  performance data for table with metadata and formatting
  performance_data <- reactive({
    viz_data <- viz_performance_data()
    
    if (nrow(viz_data) == 0) {
      return(data.frame())
    }
    
    # Select and enhance columns with proper formatting
    enhanced_data <- viz_data %>%
      select(
        experiment_id, experiment_name, 
        technology, caller_name,
        platform_name, chemistry_name, mean_coverage,
        variant_type, recall, precision, f1_score
      ) %>%
      # Convert metrics to percentages and format nicely
      mutate(
        recall = round(recall * 100, 2),
        precision = round(precision * 100, 2), 
        f1_score = round(f1_score * 100, 2),
        # Format coverage with units
        mean_coverage = case_when(
          is.na(mean_coverage) ~ "N/A",
          TRUE ~ paste0(round(mean_coverage, 1), "x")
        ),
        # Clean up chemistry name
        chemistry_name = ifelse(is.na(chemistry_name) | chemistry_name == "", "N/A", chemistry_name),
        # Clean up platform name
        platform_name = ifelse(is.na(platform_name) | platform_name == "", "N/A", platform_name)
      ) %>%
      # Order by experiment ID first, then variant type (SNP first, then INDEL for visual grouping)
      arrange(experiment_id, variant_type) %>%
      # Rename columns for better display
      rename(
        "ID" = experiment_id,
        "Experiment" = experiment_name,
        "Technology" = technology,
        "Caller" = caller_name,
        "Platform" = platform_name,
        "Chemistry" = chemistry_name,
        "Coverage" = mean_coverage,
        "Variant" = variant_type,
        "Recall (%)" = recall,
        "Precision (%)" = precision,
        "F1 Score (%)" = f1_score
      )
    
    return(enhanced_data)
  })
  
  # ====================================================================
  # 3. UI OUTPUTS FOR STATE MANAGEMENT
  # ====================================================================
  
  # Make current_mode available to UI
  output$comparison_mode <- reactive({
    current_mode()
  })
  outputOptions(output, "comparison_mode", suspendWhenHidden = FALSE)
  
  # Check if experiments are selected
  output$has_selected_experiments <- reactive({
    current_mode() == "manual_selection" && length(table_selected_ids()) > 0
  })
  outputOptions(output, "has_selected_experiments", suspendWhenHidden = FALSE)
  
  # Check if we have a selected point
  output$has_selected_point <- reactive({
    !is.null(plot_clicked_id())
  })
  outputOptions(output, "has_selected_point", suspendWhenHidden = FALSE)
  
  
  # ====================================================================
  # 4. BUTTON OBSERVERS FOR EVENT HANDLING
  # ====================================================================
  
  # 4.1
  # Technology comparison button
  observeEvent(input$compare_techs, {
    current_mode("tech_comparison")
    display_experiment_ids(numeric(0))
    table_selected_ids(numeric(0))
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_callers", selected = character(0))
    updateSelectInput(session, "caller_comparison_tech", selected = "ILLUMINA")
    
    # Reset submitted comparison states
    comparison_submitted(FALSE)
    comparison_type(NULL)
    comparison_results(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    
    showNotification("Technology comparison mode activated!", type = "message")
  })
  # -----------------------------------------------------------
  
  # 4.2 
  # Caller comparison button
  observeEvent(input$compare_callers, {
    current_mode("caller_comparison")
    display_experiment_ids(numeric(0))
    table_selected_ids(numeric(0))
    
    # Reset submitted comparison states
    comparison_submitted(FALSE)
    comparison_type(NULL)
    comparison_results(numeric(0))
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_technologies", selected = character(0))
    updateSelectInput(session, "tech_comparison_caller", selected = "DEEPVARIANT")
    
    showNotification("Caller comparison mode activated!", type = "message")
  })
  # -----------------------------------------------------------
  
  # 4.3
  # Specific experiments comparison button
  observeEvent(input$compare_experiments, {
    current_mode("manual_selection")
    display_experiment_ids(numeric(0))
    table_selected_ids(numeric(0))
    
    # Reset submitted comparison states
    comparison_submitted(FALSE)
    comparison_type(NULL)
    comparison_results(numeric(0))
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_technologies", selected = character(0))
    updateCheckboxGroupInput(session, "selected_callers", selected = character(0))
    updateSelectInput(session, "tech_comparison_caller", selected = "DEEPVARIANT")
    updateSelectInput(session, "caller_comparison_tech", selected = "ILLUMINA")
    
    showNotification("Click table rows to select experiments.", type = "message")
  })
  # -----------------------------------------------------------
  
  # 4.4
  # Clear experiment selection button
  observeEvent(input$clear_experiment_selection, {
    table_selected_ids(numeric(0))
    display_experiment_ids(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    showNotification("Experiment selection cleared!", type = "message")
  })
  # -----------------------------------------------------------
  
  # 4.5
  # Experiment table selection
  
  # Row selection in experiments table (only in manual selection mode)
  observeEvent(input$experiments_table_rows_selected, {
    if (current_mode() != "manual_selection") 
      return()
    
    current_data <- experiments_data()
    selected_rows <- input$experiments_table_rows_selected
    
    # Get selected IDs or empty vector if no selection/no data
    new_ids <- if (nrow(current_data) > 0 && length(selected_rows) > 0) {
      current_data$id[selected_rows]
    } else {
      numeric(0)
    }
    
    # Update reactive value
    table_selected_ids(new_ids)
  }, ignoreNULL = FALSE)
  # -----------------------------------------------------------
  
  # 4.6
  # Plot interaction observers - Handle clicks from both plots
  observeEvent(event_data("plotly_click", source = "snp_plot"), {
    click_data <- event_data("plotly_click", source = "snp_plot")
    if (!is.null(click_data)) {
      plot_clicked_id(click_data$customdata)
      showNotification("Scroll down to view experiment details.",
                       type = "message", duration = 3)
    }
  })
  
  observeEvent(event_data("plotly_click", source = "indel_plot"), {
    click_data <- event_data("plotly_click", source = "indel_plot")
    if (!is.null(click_data)) {
      plot_clicked_id(click_data$customdata)
      showNotification("Scroll down to view experiment details.",
                       type = "message", duration = 3)
    }
  })
  
  # ----------------------------------------------------------
  
  # 4.7
  # Comparison submission observers 
  
  ### Tech comparison ###
  observeEvent(input$submit_tech_comparison, {
    # Get all experiment IDs for selected technologies
    all_ids <- c()
    for(tech in input$selected_technologies) {
      tech_ids <- db$get_experiments_by_technology(tech)
      
      # Filter by caller
      for(id in tech_ids) {
        caller <- db$get_caller(id)
        if(!is.null(caller) && caller == input$tech_comparison_caller) {
          all_ids <- c(all_ids, id)
        }
      }
    }
    
    display_experiment_ids(all_ids)
    comparison_submitted(TRUE)
    comparison_type("technology")
    comparison_results(all_ids)
    
    showNotification(paste("Comparing", length(input$selected_technologies), "technologies with", length(all_ids), "experiments"), type = "message")
  })
  
  ### Caller comparison ###
  observeEvent(input$submit_caller_comparison, {
    # Get all experiment IDs for selected callers
    all_ids <- c()
    for(caller in input$selected_callers) {
      caller_ids <- db$get_experiments_by_caller(caller)
      
      # Filter by technology
      for(id in caller_ids) {
        tech <- db$get_technology(id)
        if(!is.null(tech) && tech == input$caller_comparison_tech) {
          all_ids <- c(all_ids, id)
        }
      }
    }
    
    display_experiment_ids(all_ids)
    comparison_submitted(TRUE)
    comparison_type("caller")
    comparison_results(all_ids)
    
    showNotification(paste("Comparing", length(input$selected_callers), "callers with", length(all_ids), "experiments"), type = "message")
  })
  
  ### Specific experiment comparison ###
  observeEvent(input$submit_bottom_comparison, {
    comparison_submitted(FALSE)
    showNotification("Using selected experiments for comparison", type = "message")
  })
  
  # -----------------------------------------------------------
  
  # 4.8
  # Handle experiment details expansion
  observeEvent(input$expand_experiment_details, {
    exp_id <- input$expand_experiment_details$id
    
    # Get detailed metadata for this experiment
    py_ids <- r_to_py(list(as.numeric(exp_id)))
    metadata <- db$get_experiment_metadata(py_ids)
    
    if (nrow(metadata) > 0) {
      meta <- metadata[1, ]
      
      # Create simple, compact HTML content
      details_html <- paste0(
        '<tr class="detail-row-', exp_id, '">',
        '<td colspan="11">',
        '<div class="detail-content">',
        '<div class="detail-grid">',
        
        # Platform Details
        '<div class="detail-section">',
        '<h6>Platform Details</h6>',
        '<div class="detail-item"><strong>Platform:</strong> ', meta$platform_name %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Version:</strong> ', meta$platform_version %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Type:</strong> ', meta$platform_type %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Target:</strong> ', meta$target %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Chemistry:</strong> ', meta$chemistry_name %||% "N/A", '</div>',
        '</div>',
        
        # Analysis Details  
        '<div class="detail-section">',
        '<h6>Analysis Details</h6>',
        '<div class="detail-item"><strong>Caller Type:</strong> ', meta$caller_type %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Caller Model:</strong> ', meta$caller_model %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Aligner:</strong> ', paste(meta$aligner_name %||% "N/A", meta$aligner_version %||% ""), '</div>',
        '<div class="detail-item"><strong>Variants:</strong> ', meta$variant_origin %||% "N/A", ' ', meta$variant_type %||% "", '</div>',
        '<div class="detail-item"><strong>Phased:</strong> ', ifelse(is.na(meta$is_phased), "N/A", ifelse(meta$is_phased, "Yes", "No")), '</div>',
        '</div>',
        
        # Quality & Truth
        '<div class="detail-section">',
        '<h6>Quality & Benchmarking</h6>',
        '<div class="detail-item"><strong>Coverage:</strong> ', ifelse(is.na(meta$mean_coverage), "N/A", paste0(round(meta$mean_coverage, 1), "x")), '</div>',
        '<div class="detail-item"><strong>Read Length:</strong> ', ifelse(is.na(meta$read_length), "N/A", paste0(meta$read_length, " bp")), '</div>',
        '<div class="detail-item"><strong>Truth Set:</strong> ', meta$truth_set_name %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Reference:</strong> ', meta$truth_set_reference %||% "N/A", '</div>',
        '<div class="detail-item"><strong>Sample:</strong> ', meta$truth_set_sample %||% "N/A", '</div>',
        '</div>',
        
        '</div>', # Close detail-grid
        '</div>', # Close detail-content
        '</td>',
        '</tr>'
      )
      
      # Send HTML to JavaScript to insert into table
      session$sendCustomMessage("insertDetailsRow", list(
        experimentId = exp_id,
        html = details_html
      ))
    }
  })
  
  #-----------------------------------------------
  
  # 4.9
  # Export html report (download)
  output$export_html_report <- downloadHandler(
    filename = function() {
      paste0("benchmarking_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    
    content = function(file) {
      viz_data <- viz_performance_data()
      html_content <- generate_benchmarking_report(viz_data)
      writeLines(html_content, file)
    }
  )
  
  # ====================================================================
  # 5. UI OUTPUTS FOR DISPALY
  # ====================================================================
  
  # 5.1
  # Selected experiment count
  output$selected_experiments_count <- renderText({
    if (current_mode() != "manual_selection") {
      return("Not in selection mode")
    }
    
    count <- length(table_selected_ids())
    if (count == 0) {
      "No experiments selected"
    } else if (count == 1) {
      "Selected: 1 experiment"
    } else {
      paste("Selected:", count, "experiments")
    }
  })
  # Badge count for bottom panel
  output$selected_count_badge <- renderText({
    length(table_selected_ids())
  })
  # -----------------------------------------------------------
  
  # 5.2
  # Basic experiment info (always shown when point is clicked)
  output$basic_experiment_info <- renderUI({
    exp_id <- plot_clicked_id()
    if (is.null(exp_id)) return(NULL)
    
    # Get experiment metadata
    exp_id_json <- json_param(list(exp_id))
    metadata <- db$get_experiment_metadata(exp_id_json)
    
    if (nrow(metadata) == 0) return(p("No metadata found"))
    
    meta <- metadata[1, ]
    
    div(
      h6(strong(meta$name), style = "color: #007bff; margin-bottom: 10px;"),
      div(
        class = "row",
        div(class = "col-md-3",
            p(strong("Technology: "), meta$technology %||% "N/A", style = "margin-bottom: 5px;")
        ),
        div(class = "col-md-3",
            p(strong("Platform: "), meta$platform_name %||% "N/A", style = "margin-bottom: 5px;")
        ),
        div(class = "col-md-3",
            p(strong("Caller: "), paste(meta$caller_name %||% "N/A", meta$caller_version %||% ""), style = "margin-bottom: 5px;")
        ),
        div(class = "col-md-3",
            p(strong("Coverage: "), 
              ifelse(is.na(meta$mean_coverage), "N/A", paste0(round(meta$mean_coverage, 1), "x")), 
              style = "margin-bottom: 5px;")
        )
      )
    )
  })
  # -----------------------------------------------------------
  
  # 5.3
  # Full experiment metadata (shown when expanded)
  output$full_experiment_metadata <- renderUI({
    exp_id <- plot_clicked_id()
    if (is.null(exp_id)) return(NULL)
    
    # Get experiment metadata
    exp_id_json <- json_param(list(exp_id))
    metadata <- db$get_experiment_metadata(exp_id_json)
    
    if (nrow(metadata) == 0) return(p("No metadata found"))
    
    meta <- metadata[1, ]
    
    div(
      h5("Complete Experiment Details"),
      
      # Use the new 4-column card layout
      div(
        class = "metadata-grid-4col",
        
        # SEQUENCING PLATFORM CARD
        div(class = "metadata-card",
            h6("Sequencing Platform"),
            p(strong("Technology: "), meta$technology %||% "N/A"),
            p(strong("Platform: "), meta$platform_name %||% "N/A"),
            p(strong("Platform Type: "), meta$platform_type %||% "N/A"),
            p(strong("Platform Version: "), meta$platform_version %||% "N/A"),
            p(strong("Target: "), meta$target %||% "N/A"),
            p(strong("Chemistry: "), meta$chemistry_name %||% "N/A")
        ),
        
        # ANALYSIS PIPELINE CARD
        div(class = "metadata-card",
            h6("Analysis Pipeline"),
            p(strong("Variant Caller: "), meta$caller_name %||% "N/A"),
            p(strong("Caller Version: "), meta$caller_version %||% "N/A"),
            p(strong("Caller Type: "), meta$caller_type %||% "N/A"),
            p(strong("Caller Model: "), meta$caller_model %||% "N/A"),
            p(strong("Aligner: "), paste(meta$aligner_name %||% "N/A", meta$aligner_version %||% "")),
            p(strong("Benchmark Tool: "), paste(meta$benchmark_tool_name %||% "N/A", meta$benchmark_tool_version %||% ""))
        ),
        
        # QUALITY METRICS CARD
        div(class = "metadata-card",
            h6("Quality Metrics"),
            p(strong("Mean Coverage: "), ifelse(is.na(meta$mean_coverage), "N/A", paste0(round(meta$mean_coverage, 1), "x"))),
            p(strong("Read Length: "), 
              ifelse(is.na(meta$read_length), 
                     ifelse(is.na(meta$mean_read_length), "N/A", paste0(meta$mean_read_length, " bp (mean)")), 
                     paste0(meta$read_length, " bp"))),
            p(strong("Mean Insert Size: "), ifelse(is.na(meta$mean_insert_size), "N/A", paste0(meta$mean_insert_size, " bp"))),
            p(strong("Created: "), ifelse(is.na(meta$created_at), "N/A", format(as.POSIXct(meta$created_at), "%Y-%m-%d")))
        ),
        
        # VARIANTS & TRUTH SET CARD
        div(class = "metadata-card",
            h6("Variants & Truth Set"),
            p(strong("Variant Type: "), meta$variant_type %||% "N/A"),
            p(strong("Variant Origin: "), meta$variant_origin %||% "N/A"),
            p(strong("Variant Size: "), meta$variant_size %||% "N/A"),
            p(strong("Is Phased: "), ifelse(is.na(meta$is_phased), "N/A", ifelse(meta$is_phased, "Yes", "No"))),
            p(strong("Truth Set: "), paste(meta$truth_set_name %||% "N/A", meta$truth_set_version %||% "")),
            p(strong("Sample: "), meta$truth_set_sample %||% "N/A"),
            p(strong("Reference: "), meta$truth_set_reference %||% "N/A")
        )
      )
    )
  })
  # -----------------------------------------------------------
  
  # 5.4
  # Legend outputs
  output$technology_legend <- renderUI({
    HTML(create_technology_legend())
  })
  
  output$caller_legend <- renderUI({
    HTML(create_caller_legend())
  })
  
  # -----------------------------------------------------------
  
  # 5.5 
  # Table outputs 
  # Experiments table
  output$experiments_table <- DT::renderDataTable({
    df <- experiments_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No experiments found")))
    }
    
    df$expand_button <- paste0(
      '<button class="details-toggle" onclick="toggleDetails(', df$id, ')">',
      '▶',
      '</button>'
    )
    
    # Reorder columns
    df <- df[, c("expand_button", setdiff(names(df), "expand_button"))]
    
    # Configure selection
    if (current_mode() == "manual_selection") {
      selection_config <- list(mode = 'multiple')
    } else {
      selection_config <- 'none'
    }
    
    dt <- DT::datatable(
      df,
      selection = selection_config,
      escape = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "15px", className = "dt-center"),   # Expand button
          list(targets = 1, width = "15px", className = "dt-center"),                      # ID
          list(targets = 2, width = "80px"),                                              # Name
          list(targets = 3, width = "80px"),                                               # Technology  
          list(targets = 4, width = "60px"),                                              # Platform
          list(targets = 5, width = "80px"),                                               # Caller
          list(targets = 6, width = "60px"),                                               # Version
          list(targets = 7, width = "60px"),                                               # Chemistry
          list(targets = 8, width = "40px"),                                               # Truth Set
          list(targets = 9, width = "40px"),                                               # Sample
          list(targets = 10, width = "65px")                                               # Created
        )
      ),
      rownames = FALSE,
      colnames = c("", "ID", "Name", "Technology", "Platform", "Caller", "Version", "Chemistry", "Truth Set", "Sample", "Created")
    ) %>%
      # Technology-based row coloring --------------------------------------------------------------------------------------------------------------------
      formatStyle(
        "technology", 
        target = "row",
        backgroundColor = styleEqual(
          c("ILLUMINA", "PACBIO", "ONT", "MGI"),  
          c("#fef6f6", "#faf8ff", "#f0fcfd", "#f7fbf5")  # Light colors
        )
      )
    
    return(dt)
  })
  
  # -----------------------------------------------------------
    # 5.6
  # Performance table with DT built-in conditional formatting
  output$performance_table <- DT::renderDataTable({
    df <- performance_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No performance data found")))
    }
    
    # Remove the row_class column if it exists
    if ("row_class" %in% names(df)) {
      df <- df %>% select(-row_class)
    }
    
    dt <- DT::datatable(
      df,
      selection = 'none',
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = 0, className = "dt-center", width = "50px"),     # ID column
          list(targets = c(8, 9, 10), className = "dt-center"),          # Performance columns
          list(targets = 6, className = "dt-center"),                    # Coverage column
          list(targets = "_all", className = "dt-body-nowrap")           # Prevent text wrapping
        )
      ),
      rownames = FALSE,
      colnames = c(
        "ID", "Experiment", "Technology", "Caller", 
        "Platform", "Chemistry", "Coverage", "Variant Type", 
        "Recall (%)", "Precision (%)", "F1 Score (%)"
      )
    ) %>%
      formatStyle(
        "Variant",
        target = "row",
        backgroundColor = styleEqual(
          c("SNP", "INDEL"), 
          c("#fdf2f2", "#f2f7fd")  # Light red for SNP, light blue for INDEL
        )
      )
    
    return(dt)
  })
  # -----------------------------------------------------------
  
  # 5.7
  # Selected experiments table (bottom of side panel)
  output$compact_selected_experiments <- renderTable({
    ids <- table_selected_ids()
    if (length(ids) == 0) {
      return(data.frame(Info = "No experiments selected"))
    }
    
    current_data <- experiments_data()
    selected_data <- current_data[current_data$id %in% ids, ]
    compact_data <- data.frame(
      ID = as.integer(selected_data$id),
      Tech = selected_data$technology,
      Caller = selected_data$caller,
      stringsAsFactors = FALSE
    )
    
    return(compact_data)
  }, striped = TRUE, hover = TRUE, spacing = 'xs', width = "100%",
  class = "table-condensed",
  style = "font-size: 11px; margin-bottom: 5px;")
  
  # ============================================================================
  # 6. PLOT OUTPUTS 
  # ============================================================================
  
  # 6.1 - SNP Plot
  output$snp_plot <- renderPlotly({
    tryCatch({
      viz_data <- viz_performance_data()
      
      if (nrow(viz_data) == 0) { # No performance data
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "SNP", x = "Precision", y = "Recall") +
          theme_bw()
        return(ggplotly(p))
      }
      
      snp_data <- viz_data %>% filter(variant_type == "SNP") # Filter SNP data only
      
      if (nrow(snp_data) == 0) { # No SNP data
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No SNP data", size = 6) +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "SNP", x = "Precision", y = "Recall") +
          theme_bw()
        return(ggplotly(p))
      }
      
      # Create contour data
      contour <- create_f1_contour()
      
      # Tooltip 
      snp_data$tooltip_text <- paste(
        "<b>ID:", snp_data$experiment_id," - ",
        "<b>", ifelse(is.na(snp_data$experiment_name) | is.null(snp_data$experiment_name), "Unknown", snp_data$experiment_name), "</b>",
        "<br><b>Technology:</b>", ifelse(is.na(snp_data$technology) | is.null(snp_data$technology), "N/A", snp_data$technology),
        "<br><b>Platform:</b>", ifelse(is.na(snp_data$platform_name) | is.null(snp_data$platform_name), "N/A", snp_data$platform_name),
        "<br><b>Caller:</b>", ifelse(is.na(snp_data$caller_name) | is.null(snp_data$caller_name), "N/A", snp_data$caller_name),
        "<br><br><b>Performance:</b>",
        "<br>• Precision:", paste0(round(as.numeric(snp_data$precision)*100, 2), "%"),
        "<br>• Recall:", paste0(round(as.numeric(snp_data$recall)*100, 2), "%"),
        "<br>• F1 Score:", paste0(round(as.numeric(snp_data$f1_score) * 100, 2), "%")
      )
      
      p <- ggplot() +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 6,
          color = "black", 
          alpha = 0.8,
          size = 0.5
        ) +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 12,
          color = "gray40", 
          alpha = 0.6,
          linetype = "dotted",
          size = 0.3
        ) +
        geom_point(
          data = snp_data, 
          aes(x = precision, y = recall, 
              fill = technology,           # fill by technology
              shape = caller,              # shape by caller
              text = tooltip_text,         # tooltip text
              customdata = experiment_id), 
          color = "black",                 # black outline
          stroke = 0.15,
          size = 2.2
        ) +
        scale_fill_manual(values = technology_colors) + 
        scale_shape_manual(values = caller_shapes) +       
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP", x = "Precision", y = "Recall") +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12),
          panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey95", size = 0.3),
          legend.position = "none"        
        )
      
      ggplotly(p, tooltip = "text", source = "snp_plot") %>%
        layout(showlegend = FALSE,
               dragmode = "zoom",
               hoverlabel = list(align = "left")
        ) %>%    
        event_register("plotly_click")
      
    }, error = function(e) {
      cat("Error in SNP plot:", e$message, "\n")
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error loading SNP data:", e$message), size = 4) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP - Error", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    })
  })
  
  # 6.2 - INDEL Plot
  output$indel_plot <- renderPlotly({
    tryCatch({
      viz_data <- viz_performance_data()
      
      if (nrow(viz_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 6) +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "INDEL", x = "Precision", y = "Recall") +
          theme_bw()
        return(ggplotly(p))
      }
      
      indel_data <- viz_data %>% filter(variant_type == "INDEL")
      
      if (nrow(indel_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No INDEL data", size = 6) +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "INDEL", x = "Precision", y = "Recall") +
          theme_bw()
        return(ggplotly(p))
      }
      
      # Create contour data
      contour <- create_f1_contour()
      
      # Safe tooltip creation with proper null checking
      indel_data$tooltip_text <- paste(
        "<b>ID:", indel_data$experiment_id," - ",
        "<b>", ifelse(is.na(indel_data$experiment_name) | is.null(indel_data$experiment_name), "Unknown", indel_data$experiment_name), "</b>",
        "<br><b>Technology:</b>", ifelse(is.na(indel_data$technology) | is.null(indel_data$technology), "N/A", indel_data$technology),
        "<br><b>Platform:</b>", ifelse(is.na(indel_data$platform_name) | is.null(indel_data$platform_name), "N/A", indel_data$platform_name),
        "<br><b>Caller:</b>", ifelse(is.na(indel_data$caller_name) | is.null(indel_data$caller_name), "N/A", indel_data$caller_name),
        "<br><br><b>Performance:</b>",
        "<br>• Precision:", paste0(round(as.numeric(indel_data$precision)*100, 2), "%"),
        "<br>• Recall:", paste0(round(as.numeric(indel_data$recall)*100, 2), "%"),
        "<br>• F1 Score:", paste0(round(as.numeric(indel_data$f1_score) * 100, 2), "%")
      )
      
      p <- ggplot() +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 6,
          color = "black", 
          alpha = 0.8,
          size = 0.5
        ) +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 12,
          color = "gray40", 
          alpha = 0.6,
          linetype = "dotted",
          size = 0.3
        ) +
        geom_point(
          data = indel_data, 
          aes(x = precision, y = recall, 
              fill = technology,           # fill by technology
              shape = caller,              # shape by caller
              text = tooltip_text,         # tooltip text
              customdata = experiment_id), 
          color = "black",                 # black outline
          size = 2.2,
          stroke = 0.15
        ) +
        scale_fill_manual(values = technology_colors) +   
        scale_shape_manual(values = caller_shapes) +       
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL", x = "Precision", y = "Recall") +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12),
          panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey95", size = 0.3),
          legend.position = "none"        
        )
      
      ggplotly(p, tooltip = "text", source = "indel_plot") %>%
        layout(showlegend = FALSE,
               dragmode = "zoom",
               hoverlabel = list(align = "left")
        ) %>%  
        event_register("plotly_click")
      
    }, error = function(e) {
      cat("Error in INDEL plot:", e$message, "\n")
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error loading INDEL data:", e$message), size = 4) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL - Error", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    })
  })
}
# =============================================================================
# APP LAUNCH
# =============================================================================

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))