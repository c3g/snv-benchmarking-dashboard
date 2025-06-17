library(reticulate)
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggsci)
library(ggrepel)
library(patchwork)
library(geomtextpath)

# Import Python module
db <- import("db_interface")

# ============================================================================
# UI
# ============================================================================

#######################
#####  SIDE PANEL #####
#######################

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
      # COMPARISON BUTTONS SECTION
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
      # TECHNOLOGY COMPARISON PANEL
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'tech'",
        hr(),
        h5("Technology Comparison Setup:"),
        
        # Select multiple technologies
        checkboxGroupInput(
          "selected_technologies",
          "Select Technologies (2 or more):",
          choices = list(
            "Illumina" = "ILLUMINA",
            "PacBio" = "PACBIO",
            "ONT" = "ONT", 
            "MGI" = "MGI"
          )
        ),
        
        # Select one caller to keep constant
        selectInput(
          "tech_comparison_caller",
          "Keep Caller Constant:",
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
        condition = "output.comparison_mode == 'caller'",
        hr(),
        h5("Caller Comparison Setup:"),
        
        # Select multiple callers
        checkboxGroupInput(
          "selected_callers",
          "Select Callers (2 or more):",
          choices = list(
            "DeepVariant" = "DEEPVARIANT",
            "GATK" = "GATK",
            "Clair3" = "CLAIR3"
          )
        ),
        
        # Select one technology to keep constant
        selectInput(
          "caller_comparison_tech",
          "Keep Technology Constant:",
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
      # EXPERIMENT SELECTION INFO 
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'experiments'",
        hr(),
        h5("Experiment Selection:"),
        p("Click on experiments in the table below to select them for comparison."),
        
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
      # SELECTED EXPERIMENTS DISPLAY (Bottom of page)
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'experiments' && output.has_selected_experiments",
        hr(),
        div(
          class = "panel panel-info",
          div(class = "panel-heading", 
              h5("Selected Experiments for Comparison ", 
                 span(class = "badge", textOutput("selected_count_badge", inline = TRUE)))
          ),
          div(class = "panel-body", style = "padding: 10px;",
              # Compact table with key info only
              div(style = "max-height: 200px; overflow-y: auto;",
                  tableOutput("compact_selected_experiments")
              ),
              br(),
              div(style = "text-align: center;",
                  actionButton(
                    "submit_bottom_comparison",
                    "Compare Selected Experiments",
                    class = "btn-warning"
                  )
              )
          )
        )
      )
    ),
    
    #######################
    #####  MAIN PANEL #####
    #######################
    
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
            condition = "output.comparison_mode == 'experiments'",
            div(
              class = "alert alert-info",
              h5("Experiment Selection Mode"),
              p("Click on table rows to select experiments for comparison.")
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
        ),
        
        # Tab 3: Visualization
        tabPanel(
          "Visualizations",
          br(),
          fluidRow(
            column(12,
                   div(
                     class = "alert alert-info",
                     h5("Precision/Recall Performance"),
                     textOutput("viz_experiment_info")
                   )
            )
          ),
          br(),
          fluidRow(
            column(6,
                   h4("SNP Performance"),
                   plotlyOutput("snp_plot", height = "500px")  # Note: plotlyOutput for hover
            ),
            column(6,
                   h4("INDEL Performance"), 
                   plotlyOutput("indel_plot", height = "500px")  # Note: plotlyOutput for hover
            )
          )
        ),
        # ================================================================
        # COMPARISON SETUP TAB
        # ================================================================
        tabPanel(
          "Comparison Setup",
          br(),
          
          # Technology comparison setup display
          conditionalPanel(
            condition = "output.comparison_mode == 'tech'",
            div(
              class = "panel panel-primary",
              div(class = "panel-heading", h4("Technology Comparison Setup")),
              div(class = "panel-body",
                  h5("Selected Technologies:"),
                  verbatimTextOutput("tech_setup_display"),
                  h5("Constant Caller:"),
                  verbatimTextOutput("tech_caller_display"),
                  conditionalPanel(
                    condition = "input.submit_tech_comparison > 0",
                    div(class = "alert alert-success",
                        h5("✓ Technology Comparison Submitted!"),
                        p("Comparison results would be calculated here...")
                    )
                  )
              )
            )
          ),
          
          # Caller comparison setup display
          conditionalPanel(
            condition = "output.comparison_mode == 'caller'",
            div(
              class = "panel panel-success", 
              div(class = "panel-heading", h4("Caller Comparison Setup")),
              div(class = "panel-body",
                  h5("Selected Callers:"),
                  verbatimTextOutput("caller_setup_display"),
                  h5("Constant Technology:"),
                  verbatimTextOutput("caller_tech_display"),
                  conditionalPanel(
                    condition = "input.submit_caller_comparison > 0",
                    div(class = "alert alert-success",
                        h5("✓ Caller Comparison Submitted!"),
                        p("Comparison results would be calculated here...")
                    )
                  )
              )
            )
          ),
          
          # Experiment comparison setup display
          conditionalPanel(
            condition = "output.comparison_mode == 'experiments'",
            div(
              class = "panel panel-warning",
              div(class = "panel-heading", h4("Specific Experiments Comparison Setup")),
              div(class = "panel-body",
                  h5("Selected Experiments:"),
                  DT::dataTableOutput("selected_experiments_table"),
                  br(),
                  conditionalPanel(
                    condition = "output.has_selected_experiments",
                    actionButton(
                      "submit_experiment_comparison",
                      "Submit Experiment Comparison",
                      class = "btn-warning btn-lg"
                    )
                  ),
                  conditionalPanel(
                    condition = "input.submit_experiment_comparison > 0",
                    div(class = "alert alert-success",
                        h5("✓ Experiment Comparison Submitted!"),
                        p("Comparison results would be calculated here...")
                    )
                  )
              )
            )
          ),
          
          # Default message when no comparison is selected
          conditionalPanel(
            condition = "output.comparison_mode == 'none'",
            div(
              class = "alert alert-info",
              h4("No Comparison Selected"),
              p("Choose a comparison option from the sidebar to set up your comparison."),
              tags$ul(
                tags$li("Technology Comparison: Compare 2+ technologies with the same caller"),
                tags$li("Caller Comparison: Compare 2+ callers with the same technology"), 
                tags$li("Experiment Comparison: Select specific experiments to compare")
              )
            )
          )
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
  # COMPARISON BUTTON OBSERVERS
  # ====================================================================
  
  # Technology comparison button
  observeEvent(input$compare_techs, {
    comparison_mode("tech")
    selected_experiment_ids(numeric(0))  # Reset experiment selection
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_callers", selected = character(0))
    updateSelectInput(session, "caller_comparison_tech", selected = "ILLUMINA")
    
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    
    showNotification("Technology comparison mode activated!", type = "message")
  })
  
  # Caller comparison button
  observeEvent(input$compare_callers, {
    comparison_mode("caller") 
    selected_experiment_ids(numeric(0))  # Reset experiment selection
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_technologies", selected = character(0))
    updateSelectInput(session, "tech_comparison_caller", selected = "DEEPVARIANT")
    
    showNotification("Caller comparison mode activated!", type = "message")
  })
  
  # Specific experiments comparison button
  observeEvent(input$compare_experiments, {
    comparison_mode("experiments")
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_technologies", selected = character(0))
    updateCheckboxGroupInput(session, "selected_callers", selected = character(0))
    updateSelectInput(session, "tech_comparison_caller", selected = "DEEPVARIANT")
    updateSelectInput(session, "caller_comparison_tech", selected = "ILLUMINA")
    
    showNotification("Click table rows to select experiments.", type = "message")
  })
  
  # Clear experiment selection button
  observeEvent(input$clear_experiment_selection, {
    selected_experiment_ids(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    showNotification("Experiment selection cleared!", type = "message")
  })
  
  # ====================================================================
  # EXPERIMENT SELECTION LOGIC (for specific comparison)
  # ====================================================================
  
  # Handle row selection in experiments table (only when in experiment comparison mode)
  observeEvent(input$experiments_table_rows_selected, {
    if (comparison_mode() == "experiments") {
      current_data <- experiments_data()
      
      # Always update based on current DataTable selection
      if (nrow(current_data) > 0) {
        selected_rows <- input$experiments_table_rows_selected
        
        if (is.null(selected_rows) || length(selected_rows) == 0) {
          # No selection
          selected_experiment_ids(numeric(0))
        } else {
          # Valid selection
          new_ids <- current_data$id[selected_rows]
          selected_experiment_ids(new_ids)
        }
      } else {
        # No data available
        selected_experiment_ids(numeric(0))
      }
    }
  }, ignoreNULL = FALSE) 
  
  # ====================================================================
  # DATA PROCESSING FUNCTIONS 
  # ====================================================================
  
  # Get experiment IDs based on filter
  experiment_ids <- reactive({
    # Handle experiment selection mode first
    if (comparison_mode() == "experiments" && length(selected_experiment_ids()) > 0) {
      return(selected_experiment_ids())
    }
    
    # Regular filtering
    if (input$filter_type == "tech") {
      ids <- db$get_experiments_by_technology(input$technology)
    } else if (input$filter_type == "caller") {
      ids <- db$get_experiments_by_caller(input$caller)
    } else {
      overview <- db$get_experiments_overview()
      ids <- overview$id
    }
    return(ids)
  })
  
  # Get metadata for selected experiments
  experiments_data <- reactive({
    # Build filters based on current filter type
    filters <- NULL
    
    if (input$filter_type == "tech") {
      filters <- list(technology = input$technology)
    } else if (input$filter_type == "caller") {
      filters <- list(caller = input$caller)
    }
    
    # Always use overview format with appropriate filters
    if (is.null(filters)) {
      return(db$get_experiments_overview())
    } else {
      return(db$get_experiments_overview(filters))
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
  
  # Visualizations
  viz_performance_data <- reactive({
    ids <- experiment_ids()
    
    if (length(ids) == 0) {
      return(data.frame())
    }
    
    py_ids <- r_to_py(as.list(ids))
    perf_data <- db$get_performance_results(py_ids, c('SNP', 'INDEL'))
    
    # Filter for ALL_REGIONS subset for main plots
    filtered_data <- perf_data %>%
      filter(subset == "ALL_REGIONS" | subset == "*") %>%
      filter(!is.na(recall) & !is.na(precision) & !is.na(f1_score))
    
    return(filtered_data)
  })
  
  # Helper function for F1 contours
  create_f1_contour <- function() {
    f1_contour_function <- function(p, r) 2 * (p * r) / (p + r)
    contour <- expand.grid(p = seq(0, 1, by = 0.01), r = seq(0, 1, by = 0.01)) %>%
      mutate(f1 = f1_contour_function(p, r)) %>%
      filter(is.finite(f1))
    return(contour)
  }
  # ====================================================================
  # OUTPUTS
  # ====================================================================
  
  # Show experiment count
  output$selected_experiments_count <- renderText({
    count <- length(selected_experiment_ids())
    if (count == 0) {
      "No experiments selected"
    } else if (count == 1) {
      "Selected: 1 experiment"
    } else {
      paste("Selected:", count, "experiments")
    }
  })
  
  # Make comparison_mode available to UI
  output$comparison_mode <- reactive({
    comparison_mode()
  })
  outputOptions(output, "comparison_mode", suspendWhenHidden = FALSE)
  
  # Check if experiments are selected (for conditional panels)
  output$has_selected_experiments <- reactive({
    length(selected_experiment_ids()) > 0
  })
  outputOptions(output, "has_selected_experiments", suspendWhenHidden = FALSE)
  
  # Selected experiments count in sidebar
  output$selected_experiments_count <- renderText({
    count <- length(selected_experiment_ids())
    paste("Selected:", count, "experiments")
  })
  
  # Badge count for bottom panel
  output$selected_count_badge <- renderText({
    length(selected_experiment_ids())
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
      DT::formatRound(c("recall", "precision", "f1_score"), 6)
  })
  
  # ====================================================================
  # COMPARISON SETUP DISPLAYS
  # ====================================================================
  
  # Technology comparison setup displays
  output$tech_setup_display <- renderText({
    if (length(input$selected_technologies) > 0) {
      paste(input$selected_technologies, collapse = ", ")
    } else {
      "No technologies selected"
    }
  })
  
  output$tech_caller_display <- renderText({
    input$tech_comparison_caller
  })
  
  # Caller comparison setup displays
  output$caller_setup_display <- renderText({
    if (length(input$selected_callers) > 0) {
      paste(input$selected_callers, collapse = ", ")
    } else {
      "No callers selected"
    }
  })
  
  output$caller_tech_display <- renderText({
    input$caller_comparison_tech
  })
  
  # Selected experiments table (in setup tab)
  output$selected_experiments_table <- DT::renderDataTable({
    ids <- selected_experiment_ids()
    if (length(ids) == 0) {
      return(DT::datatable(data.frame(Message = "No experiments selected")))
    }
    
    current_data <- experiments_data()
    selected_data <- current_data[current_data$id %in% ids, ]
    
    # Show key columns only
    key_cols <- c("id", "name", "technology", "caller")
    display_data <- selected_data[, key_cols[key_cols %in% names(selected_data)]]
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        dom = 't'  # Remove search/pagination for small table
      ),
      rownames = FALSE
    )
  })
  # Selected experiments table (at bottom of page) - COMPACT VERSION
  output$compact_selected_experiments <- renderTable({
    ids <- selected_experiment_ids()
    if (length(ids) == 0) {
      return(data.frame(Info = "No experiments selected"))
    }
    
    current_data <- experiments_data()
    selected_data <- current_data[current_data$id %in% ids, ]
    
    # Create compact display with only essential info
    compact_data <- data.frame(
      ID = as.integer(selected_data$id),
      Name = selected_data$name,
      Tech = selected_data$technology,
      Caller = selected_data$caller,
      stringsAsFactors = FALSE
    )
    
    return(compact_data)
  }, striped = TRUE, hover = TRUE, spacing = 'xs', width = "100%")
  
  # Visualization info text
  output$viz_experiment_info <- renderText({
    viz_data <- viz_performance_data()
    if (nrow(viz_data) == 0) {
      return("No performance data available for visualization")
    }
    
    exp_count <- length(unique(viz_data$experiment_name))
    snp_count <- nrow(viz_data[viz_data$variant_type == "SNP", ])
    indel_count <- nrow(viz_data[viz_data$variant_type == "INDEL", ])
    
    paste("Visualizing", exp_count, "experiments:", snp_count, "SNP results,", indel_count, "INDEL results")
  })
  
  # SNP Performance Plot
  output$snp_plot <- renderPlotly({
    viz_data <- viz_performance_data()
    
    if (nrow(viz_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No SNP data available", size = 6) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP Performance", x = "Precision", y = "Recall")
      return(ggplotly(p))
    }
    
    snp_data <- viz_data %>% filter(variant_type == "SNP")
    
    if (nrow(snp_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No SNP data", size = 6) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP Performance", x = "Precision", y = "Recall")
      return(ggplotly(p))
    }
    
    contour <- create_f1_contour()
    
    p <- ggplot() +
      geom_textcontour(data = contour, aes(p, r, z = f1), 
                       bins = 6, size = 2, alpha = 0.5, straight = TRUE) +
      geom_textcontour(data = contour, aes(p, r, z = f1), 
                       bins = 12, linetype = 3, size = 2, alpha = 0.35, straight = TRUE) +
      geom_point(data = snp_data, 
                 aes(x = precision, y = recall, color = experiment_name,
                     text = paste("Experiment:", experiment_name, 
                                  "<br>F1 Score:", round(f1_score * 100, 2), "%",
                                  "<br>Precision:", round(precision, 4),
                                  "<br>Recall:", round(recall, 4))), 
                 size = 4) +
      scale_color_jama() +
      xlim(0, 1) + ylim(0, 1) +
      labs(title = "SNP Performance", x = "Precision", y = "Recall", color = "Experiment") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # INDEL Performance Plot  
  output$indel_plot <- renderPlotly({
    viz_data <- viz_performance_data()
    
    if (nrow(viz_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No INDEL data available", size = 6) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL Performance", x = "Precision", y = "Recall")
      return(ggplotly(p))
    }
    
    indel_data <- viz_data %>% filter(variant_type == "INDEL")
    
    if (nrow(indel_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No INDEL data", size = 6) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL Performance", x = "Precision", y = "Recall")
      return(ggplotly(p))
    }
    
    contour <- create_f1_contour()
    
    p <- ggplot() +
      geom_textcontour(data = contour, aes(p, r, z = f1), 
                       bins = 6, size = 2, alpha = 0.5, straight = TRUE) +
      geom_textcontour(data = contour, aes(p, r, z = f1), 
                       bins = 12, linetype = 3, size = 2, alpha = 0.35, straight = TRUE) +
      geom_point(data = indel_data, 
                 aes(x = precision, y = recall, color = experiment_name,
                     text = paste("Experiment:", experiment_name, 
                                  "<br>F1 Score:", round(f1_score * 100, 2), "%",
                                  "<br>Precision:", round(precision, 4),
                                  "<br>Recall:", round(recall, 4))), 
                 size = 4) +
      scale_color_jama() +
      xlim(0, 1) + ylim(0, 1) +
      labs(title = "INDEL Performance", x = "Precision", y = "Recall", color = "Experiment") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  # ====================================================================
  # SUBMISSION OBSERVERS (Placeholder for now)
  # ====================================================================
  
observeEvent(input$submit_tech_comparison, {
  cat("Tech comparison clicked\n")
})

observeEvent(input$submit_caller_comparison, {
  cat("Caller comparison clicked\n")
})

observeEvent(input$submit_experiment_comparison, {
  cat("Experiment comparison clicked\n")
})

observeEvent(input$submit_bottom_comparison, {
  cat("Bottom comparison clicked\n")
})
}
# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
