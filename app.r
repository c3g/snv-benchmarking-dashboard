library(reticulate)
library(shiny)
library(DT)
library(ggplot2)

# Import Python module
db <- import("db_interface")

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  titlePanel("SNV Benchmarking Dashboard"),
  
  sidebarLayout(
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
      
      # Technology dropdown (conditional)
      conditionalPanel(
        condition = "input.filter_type == 'tech'",
        selectInput(
          "technology",
          "Choose Technology:",
          choices = c("ILLUMINA", "PACBIO", "ONT", "MGI"),
          selected = "ILLUMINA"
        )
      ),
      
      # Caller dropdown (conditional)  
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
      # NEW: COMPARISON BUTTONS SECTION
      # ====================================================================
      hr(),
      h4("Comparison Options:"),
      
      # Compare Technologies Button
      actionButton(
        "compare_techs",
        "Compare Technologies",
        class = "btn-primary",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      # Compare Callers Button  
      actionButton(
        "compare_callers", 
        "Compare Callers",
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
      # NEW: EXPERIMENT SELECTION AREA (for specific comparison)
      # ====================================================================
      conditionalPanel(
        condition = "input.compare_experiments > 0",
        hr(),
        h5("Selected Experiments:"),
        verbatimTextOutput("selected_experiments_info"),
        actionButton(
          "clear_selection",
          "Clear Selection",
          class = "btn-secondary btn-sm",
          style = "width: 100%;"
        )
      )
    ),
    
    mainPanel(
      width = 9,
      
      # Show experiment count
      h4(textOutput("experiment_count")),
      br(),
      
      tabsetPanel(
        
        # Tab 1: Experiments Overview
        tabPanel(
          "Experiments",
          br(),
          # Add info about selection when in experiment comparison mode
          conditionalPanel(
            condition = "input.compare_experiments > 0",
            div(
              class = "alert alert-info",
              h5("📋 Experiment Selection Mode"),
              p("Click on table rows to select experiments for comparison. Selected experiments will appear in the sidebar.")
            ),
            br()
          ),
          DT::dataTableOutput("experiments_table")
        ),
        
        # Tab 2: Performance Results
        tabPanel(
          "Performance Results", 
          br(),
          DT::dataTableOutput("performance_table")
        )
        
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # ====================================================================
  # REACTIVE VALUES FOR TRACKING STATE
  # ====================================================================
  
  # Track which comparison mode is active
  comparison_mode <- reactiveVal("none")  # "none", "tech", "caller", "experiments"
  
  # Store selected experiment IDs for specific comparison
  selected_experiment_ids <- reactiveVal(numeric(0))
  
  
  # ====================================================================
  # EXPERIMENT SELECTION LOGIC (for specific comparison)
  # ====================================================================
  
  # Handle row selection in experiments table (only when in experiment comparison mode)
  observeEvent(input$experiments_table_rows_selected, {
    if (comparison_mode() == "experiments") {
      current_data <- experiments_data()
      if (length(input$experiments_table_rows_selected) > 0 && nrow(current_data) > 0) {
        # Get the experiment IDs from selected rows
        selected_rows <- input$experiments_table_rows_selected
        new_ids <- current_data$id[selected_rows]
        selected_experiment_ids(new_ids)
      }
    }
  })
  
  # ====================================================================
  # DATA PROCESSING FUNCTIONS
  # ====================================================================
  
  # Get experiment IDs based on filter
  experiment_ids <- reactive({
    if (input$filter_type == "tech") {
      # Filter by technology
      ids <- db$get_experiments_by_technology(input$technology)
    } else if (input$filter_type == "caller") {
      # Filter by caller
      ids <- db$get_experiments_by_caller(input$caller)
    } else {
      # Show all - get IDs from overview
      overview <- db$get_experiments_overview()
      ids <- overview$id
    }
    return(ids)
  })
  
  # Get metadata for selected experiments
  experiments_data <- reactive({
    ids <- experiment_ids()
    
    if (length(ids) == 0) {
      return(data.frame())
    }
    
    if (input$filter_type == "none") {
      # For "show all", use the lighter overview function
      return(db$get_experiments_overview())
    } else {
      #  Get detailed metadata
      py_ids <- r_to_py(as.list(ids))
      return(db$get_experiment_metadata(py_ids))
    }
  })
  
  # Get performance results for selected experiments
  performance_data <- reactive({
    ids <- experiment_ids()
    
    if (length(ids) == 0) {
      return(data.frame())
    }
    
    py_ids <- r_to_py(as.list(ids))
    return(db$get_performance_results(py_ids))
  })
  
  # ====================================================================
  # OUTPUTS 
  # ====================================================================
  
  # Show experiment count
  output$experiment_count <- renderText({
    count <- length(experiment_ids())
    paste("Showing", count, "experiments")
  })
  
  # Experiments table (enhanced with selection for experiment comparison)
  output$experiments_table <- DT::renderDataTable({
    df <- experiments_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No experiments found")))
    }
    
    # Configure selection based on comparison mode
    if (comparison_mode() == "experiments") {
      # Multiple selection allowed when in experiment comparison mode
      selection_config <- list(mode = 'multiple')
    } else {
      # No selection in other modes
      selection_config <- 'none'
    }
    
    DT::datatable(
      df,
      selection = selection_config,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Performance table  
  output$performance_table <- DT::renderDataTable({
    df <- performance_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No performance data found")))
    }
    # Show key performance columns
    key_cols <- c("experiment_name", "variant_type", "recall", "precision", "f1_score")
    display_df <- df[, key_cols[key_cols %in% names(df)]]
    
    DT::datatable(
      display_df,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatRound(c("recall", "precision", "f1_score"), 4)
  })
  
}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server)