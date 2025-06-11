
# Install required packages
if (!require("shiny")) install.packages("shiny")
if (!require("reticulate")) install.packages("reticulate")

library(reticulate)
library(shiny)
library(DT) # Interactive tables
library(ggplot2)


# ============================================================================
# UI 
# ============================================================================

ui <- fluidPage(
  titlePanel("SNV Benchmarking Experiments Overview"),
  
  br(),
  
  # Load data button
  actionButton("load_data", "Load Experiments", class = "btn"),
  
  br(), br(),
  
  # Results
  DT::dataTableOutput("experiments_table") # creates empty space for table 
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Load experiments when button is clicked
  observeEvent(input$load_data, {
    
    tryCatch({
      # Import and call Python function
      db_interface <- import("db_interface")
      result <- db_interface$get_experiments_overview()
      
      if (result) {
        # Convert Python result to R data frame
        experiments_data <- result$data
        
        # Create data frame
        df <- data.frame(
          # Extracts metadata from each experiment
          ID = sapply(experiments_data, function(x) x$id), 
          Name = sapply(experiments_data, function(x) x$name),
          Technology = sapply(experiments_data, function(x) x$technology),
          Platform = sapply(experiments_data, function(x) x$platform),
          Caller = sapply(experiments_data, function(x) x$caller),
          Truth_Set = sapply(experiments_data, function(x) x$truth_set),
          Sample = sapply(experiments_data, function(x) x$sample),
          Created = sapply(experiments_data, function(x) x$created_at)
        )
        
        # takes data and creates interactive table
        output$experiments_table <- DT::renderDataTable({ 
          DT::datatable(df, options = list(scrollX = TRUE))
        })
        
        showNotification(paste("Loaded", nrow(df), "experiments"), type = "message")
        
      } else {
        cat("Python function returned error:", result$error, "\n")
      }
      
    }, error = function(e) {
      cat("R Error:", e$message, "\n")
    })
  })
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
