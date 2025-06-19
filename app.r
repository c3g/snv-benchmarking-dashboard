# ============================================================================
# LIBRARIES & SETUP
# ============================================================================
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

# Import Python database interface
db <- import("db_interface")

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  titlePanel("SNV Benchmarking Dashboard"),
  
  sidebarLayout(
    # ========================================================================
    # SIDEBAR PANEL
    # ========================================================================
    sidebarPanel(
      width = 3,
      
      # Basic Filtering Section
      h4("Filter Options:"),
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
      
      # Conditional filter inputs
      conditionalPanel(
        condition = "input.filter_type == 'tech'",
        selectInput("technology", "Choose Technology:",
                    choices = c("ILLUMINA", "PACBIO", "ONT", "MGI"),
                    selected = "ILLUMINA")
      ),
      
      conditionalPanel(
        condition = "input.filter_type == 'caller'",
        selectInput("caller", "Choose Caller:",
                    choices = c("DEEPVARIANT", "GATK", "CLAIR3"),
                    selected = "DEEPVARIANT")
      ),
      
      hr(),
      
      # Comparison Mode Selection
      h4("Comparison Options:"),
      actionButton("compare_techs", "Compare Technologies", 
                   class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
      actionButton("compare_callers", "Compare Callers", 
                   class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
      actionButton("compare_experiments", "Compare Specific Experiments", 
                   class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
      
      # Technology Comparison Panel
      conditionalPanel(
        condition = "output.comparison_mode == 'tech'",
        hr(),
        h5("Technology Comparison Setup:"),
        checkboxGroupInput("selected_technologies", "Select Technologies (2+):",
                           choices = list("Illumina" = "ILLUMINA", "PacBio" = "PACBIO", 
                                          "ONT" = "ONT", "MGI" = "MGI")),
        selectInput("tech_comparison_caller", "Keep Caller Constant:",
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
      
      # Caller Comparison Panel
      conditionalPanel(
        condition = "output.comparison_mode == 'caller'",
        hr(),
        h5("Caller Comparison Setup:"),
        checkboxGroupInput("selected_callers", "Select Callers (2+):",
                           choices = list("DeepVariant" = "DEEPVARIANT", "GATK" = "GATK", "Clair3" = "CLAIR3")),
        selectInput("caller_comparison_tech", "Keep Technology Constant:",
                    choices = c("Illumina" = "ILLUMINA", "PacBio" = "PACBIO", 
                                "ONT" = "ONT", "MGI" = "MGI"),
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
      
      # Experiment Selection Panel
      conditionalPanel(
        condition = "output.comparison_mode == 'experiments'",
        hr(),
        h5("Experiment Selection:"),
        p("Click on experiments in the table below to select them for comparison."),
        textOutput("selected_experiments_count"),
        actionButton("clear_experiment_selection", "Clear Selection",
                     class = "btn-secondary btn-sm", style = "width: 100%")
      ),
      
      # Selected Experiments Display (Bottom)
      conditionalPanel(
        condition = "output.comparison_mode == 'experiments' && output.has_selected_experiments",
        hr(),
        div(class = "panel panel-info",
            div(class = "panel-heading", 
                h5("Selected Experiments ", span(class = "badge", textOutput("selected_count_badge", inline = TRUE)))),
            div(class = "panel-body", style = "padding: 10px;",
                div(style = "max-height: 200px; overflow-y: auto;",
                    tableOutput("compact_selected_experiments")),
                br(),
                div(style = "text-align: center;",
                    actionButton("submit_experiment_comparison", "Compare Selected Experiments",
                                 class = "btn-warning"))))
      )
    ),
    
    # ========================================================================
    # MAIN PANEL
    # ========================================================================
    mainPanel(
      width = 9,
      h4(textOutput("experiment_count")),
      br(),
      
      tabsetPanel(
        # Experiments Tab
        tabPanel("Experiments",
                 br(),
                 conditionalPanel(
                   condition = "output.comparison_mode == 'experiments'",
                   div(class = "alert alert-info",
                       h5("Experiment Selection Mode"),
                       p("Click on table rows to select experiments for comparison."))
                 ),
                 conditionalPanel(
                   condition = "output.comparison_mode == 'tech' || output.comparison_mode == 'caller'",
                   div(class = "alert alert-warning",
                       h5("Comparison Mode Active"),
                       textOutput("comparison_status_text"),
                       p("Submit your comparison to see visualizations."))
                 ),
                 DT::dataTableOutput("experiments_table")),
        
        # Performance Results Tab
        tabPanel("Performance Results",
                 br(),
                 conditionalPanel(
                   condition = "output.comparison_mode == 'tech' || output.comparison_mode == 'caller'",
                   div(class = "alert alert-info",
                       h5("Performance Results"),
                       p("Performance results will be shown here after you submit your comparison selection."))
                 ),
                 conditionalPanel(
                   condition = "output.comparison_mode != 'tech' && output.comparison_mode != 'caller'",
                   DT::dataTableOutput("performance_table")
                 )),
        
        # Visualizations Tab
        tabPanel("Visualizations",
                 br(),
                 fluidRow(
                   column(12, div(class = "alert alert-info",
                                  h5("Precision/Recall Performance - HG002 Sample"),
                                  textOutput("viz_experiment_info")))
                 ),
                 fluidRow(
                   column(6, h4("SNP Performance"), plotlyOutput("snp_plot", height = "500px")),
                   column(6, h4("INDEL Performance"), plotlyOutput("indel_plot", height = "500px"))
                 ))
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
  # Selected experiments table
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
  # Selected experiments table (at bottom of page)
  output$compact_selected_experiments <- renderTable({
    ids <- selected_experiment_ids()
    if (length(ids) == 0) {
      return(data.frame(Info = "No experiments selected"))
    }
    
    current_data <- experiments_data()
    selected_data <- current_data[current_data$id %in% ids, ]
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
  
  
  # SNP Performance Plot (Plotly-compatible)
  output$snp_plot <- renderPlotly({
    viz_data <- viz_performance_data()
    
    if (nrow(viz_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No SNP data available", size = 6) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    }
    
    snp_data <- viz_data %>% filter(variant_type == "SNP")
    
    if (nrow(snp_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No SNP data", size = 6) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    }
    
    # Create contour data
    contour <- create_f1_contour()
    
    p <- ggplot() +
      # F1 score contour lines - MAJOR
      geom_contour(
        data = contour, 
        aes(x = p, y = r, z = f1), 
        bins = 6,
        color = "black", 
        alpha = 0.8,
        size = 0.5
      ) +
      # F1 score contour lines - MINOR
      geom_contour(
        data = contour, 
        aes(x = p, y = r, z = f1), 
        bins = 12,
        color = "gray40", 
        alpha = 0.6,
        linetype = "dotted",
        size = 0.3
      ) +
      # F1 score labels next to points
      geom_text_repel(
        data = snp_data,
        aes(x = precision, y = recall, 
            label = paste0(round(f1_score * 100, 1), "%")),
        size = 3, 
        box.padding = 0.3, 
        point.padding = 0.3, 
        segment.color = "grey50", 
        max.overlaps = 20,
        force = 2
      ) +
      # Data points
      geom_point(
        data = snp_data, 
        aes(x = precision, y = recall, color = experiment_name,
            text = paste("Experiment:", experiment_name, 
                         "<br>F1 Score:", round(f1_score * 100, 2), "%",
                         "<br>Precision:", round(precision, 4),
                         "<br>Recall:", round(recall, 4))), 
        size = 1.7
      ) +
      scale_color_jama() +
      xlim(0, 1) + ylim(0, 1) +
      labs(title = "SNP", x = "Precision", y = "Recall", color = "sample") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 12),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey95", size = 0.3)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = TRUE)
  })
  
  # INDEL Performance Plot (Plotly-compatible)
  output$indel_plot <- renderPlotly({
    viz_data <- viz_performance_data()
    
    if (nrow(viz_data) == 0) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "No INDEL data available", size = 6) +
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
    
    p <- ggplot() +
      # F1 score contour lines - MAJOR
      geom_contour(
        data = contour, 
        aes(x = p, y = r, z = f1), 
        bins = 6,
        color = "black", 
        alpha = 0.8,
        size = 0.5
      ) +
      # F1 score contour lines - MINOR
      geom_contour(
        data = contour, 
        aes(x = p, y = r, z = f1), 
        bins = 12,
        color = "gray40", 
        alpha = 0.6,
        linetype = "dotted",
        size = 0.3
      ) +
      # F1 score labels next to points
      geom_text_repel(
        data = indel_data,
        aes(x = precision, y = recall, 
            label = paste0(round(f1_score * 100, 1), "%")),
        size = 3, 
        box.padding = 0.3, 
        point.padding = 0.3, 
        segment.color = "grey50", 
        max.overlaps = 20,
        force = 2
      ) +
      # Data points
      geom_point(
        data = indel_data, 
        aes(x = precision, y = recall, color = experiment_name,
            text = paste("Experiment:", experiment_name, 
                         "<br>F1 Score:", round(f1_score * 100, 2), "%",
                         "<br>Precision:", round(precision, 4),
                         "<br>Recall:", round(recall, 4))), 
        size = 1.7
      ) +
      scale_color_jama() +
      xlim(0, 1) + ylim(0, 1) +
      labs(title = "INDEL", x = "Precision", y = "Recall", color = "sample") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 12),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.grid.minor = element_line(color = "grey95", size = 0.3)
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = TRUE)
  })
  
  # ====================================================================
  # SUBMISSION OBSERVERS
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