
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
      # For filtered results, get detailed metadata
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
  
  # Show experiment count
  output$experiment_count <- renderText({
    count <- length(experiment_ids())
    paste("Showing", count, "experiments")
  })
  
  # Experiments table
  output$experiments_table <- DT::renderDataTable({
    df <- experiments_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No experiments found")))
    }
    
    DT::datatable(
      df,
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
      DT::formatRound(c("recall", "precision", "f1_score"), 3)
  })
}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server)