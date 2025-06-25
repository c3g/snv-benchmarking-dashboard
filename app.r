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
      # SELECTED EXPERIMENTS DISPLAY (Bottom of sidebar)
      # ====================================================================
      conditionalPanel(
        condition = "output.comparison_mode == 'manual_selection' && output.has_selected_experiments",
        hr(),
        div(
          class = "panel panel-info",
          div(class = "panel-heading", #heading
              h5("Selected Experiments for Comparison ", 
                 span(class = "badge", textOutput("selected_count_badge", inline = TRUE)))
          ),
          div(class = "panel-body", style = "padding: 10px;", #content (table)
              div(style = "max-height: 200px; overflow-y: auto;",
                  tableOutput("compact_selected_experiments")
              ),
              br(),
              div(style = "text-align: center;", #submit button
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
        
        # ====================================================================
        # TAB 1: EXPERIMENT OVERVIEW
        # ====================================================================
        tabPanel(
          "Experiments",
          br(),
          # Add info about selection when in experiment comparison mode
          conditionalPanel(
            condition = "output.comparison_mode == 'manual_selection'",
            div(
              class = "alert alert-info",
              h5("Experiment Selection Mode"),
              p("Click on table rows to select experiments for comparison.")
            ),
            br()
          ),
          DT::dataTableOutput("experiments_table")
        ),
        
        # ====================================================================
        # TAB 2: PERFORMANCE RESULTS 
        # ====================================================================
        tabPanel(
          "Performance Results", 
          br(),
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
                     h5("Performance Analysis"),
                     p(strong("Click points"), " and scroll down to view experiment details below, or ", strong("hover"), " for quick metrics"),
                     p(style = "font-size: 0.9em; color: #6c757d;", "Tip: Drag to zoom, double-click to reset")
                   )
            )
          ),
          br(),
          fluidRow(
            column(6,
                   h4("SNP Performance"),
                   plotlyOutput("snp_plot", height = "500px")
            ),
            column(6,
                   h4("INDEL Performance"), 
                   plotlyOutput("indel_plot", height = "500px")
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

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  
  # Color and shape mappings for visualization
  # Muted colors:
  technology_colors <- c(
    "ILLUMINA" = "#2E5F88",    # Blue
    "PACBIO" = "#CC7A00",      # Orange  
    "ONT" = "#4A7C35",         # Green
    "MGI" = "#A52A2A",         # Red
    "Unknown" = "#5C5C5C"      # Gray
  )
  
  'technology_colors <- c(
    "ILLUMINA" = "#1f77b4",    # Blue
    "PACBIO" = "#ff7f0e",      # Orange  
    "ONT" = "#2ca02c",         # Green
    "MGI" = "#d62728",         # Red
    "Unknown" = "#7f7f7f"      # Gray
  )'
  
  caller_shapes <- c(
    "DEEPVARIANT" = 16,        # Circle
    "GATK" = 17,              # Triangle
    "CLAIR3" = 15,            # Square
    "Unknown" = 4             # X
  )
  # ====================================================================
  # REACTIVE VALUES FOR TRACKING STATE (Option 2 - Cleaner Names)
  # ====================================================================
  
  # App mode and state
  current_mode <- reactiveVal("filter")  # "filter", "tech_comparison", "caller_comparison", "manual_selection"
  
  # Experiment IDs (single source of truth)
  display_experiment_ids <- reactiveVal(numeric(0))  # Always contains IDs to show in tables/plots
  
  # Manual selection specific
  table_selected_ids <- reactiveVal(numeric(0))      # IDs selected by clicking table rows
  
  # Plot interaction specific  
  plot_clicked_id <- reactiveVal(NULL)               # Single ID from clicking plot points
  
  # Comparison state
  comparison_submitted <- reactiveVal(FALSE)         # Whether any comparison has been submitted
  comparison_type <- reactiveVal(NULL)               # "technology" or "caller" 
  comparison_results <- reactiveVal(numeric(0))     # IDs from submitted comparison
  
  # ====================================================================
  # COMPARISON BUTTON OBSERVERS
  # ====================================================================
  
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
  
  # Clear experiment selection button
  observeEvent(input$clear_experiment_selection, {
    table_selected_ids(numeric(0))
    display_experiment_ids(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    showNotification("Experiment selection cleared!", type = "message")
  })
  
  # ====================================================================
  # EXPERIMENT SELECTION LOGIC (for manual selection)
  # ====================================================================
  
  # Handle row selection in experiments table (only when in manual selection mode)
  observeEvent(input$experiments_table_rows_selected, {
    if (current_mode() == "manual_selection") {
      current_data <- experiments_data()
      
      # Always update based on current DataTable selection
      if (nrow(current_data) > 0) {
        selected_rows <- input$experiments_table_rows_selected
        
        if (is.null(selected_rows) || length(selected_rows) == 0) {
          table_selected_ids(numeric(0))
          display_experiment_ids(numeric(0))
        } else {
          new_ids <- current_data$id[selected_rows]
          table_selected_ids(new_ids)
          display_experiment_ids(new_ids)
        }
      } else {
        table_selected_ids(numeric(0))
        display_experiment_ids(numeric(0))
      }
    }
  }, ignoreNULL = FALSE) 
  
  # ====================================================================
  # DATA PROCESSING FUNCTIONS 
  # ====================================================================
  
  
  # Get experiment IDs based on filter
  experiment_ids <- reactive({
    # If we have specific experiments to display, use those
    if (length(display_experiment_ids()) > 0) {
      return(display_experiment_ids())
    }
    
    # Otherwise use regular filtering
    if (input$filter_type == "tech") {
      return(db$get_experiments_by_technology(input$technology))
    } else if (input$filter_type == "caller") {
      return(db$get_experiments_by_caller(input$caller))
    } else {
      overview <- db$get_experiments_overview()
      return(overview$id)
    }
  })
  
  # Get metadata for selected experiments
  experiments_data <- reactive({
    # Handle submitted comparisons first - get detailed metadata for specific IDs
    if (comparison_submitted()) {
      ids <- experiment_ids()
      if (length(ids) == 0) {
        return(data.frame())
      }
      py_ids <- r_to_py(as.list(ids))
      detailed_data <- db$get_experiment_metadata(py_ids)
      
      # Convert to overview format to match expected structure
      if (nrow(detailed_data) > 0) {
        overview_format <- detailed_data %>%
          select(id, name, technology, platform_name, caller_name, caller_version, 
                 chemistry_name, truth_set_name, truth_set_sample, created_at) %>%
          rename(platform = platform_name, caller = caller_name, 
                 chemistry = chemistry_name, truth_set = truth_set_name, 
                 sample = truth_set_sample)
        return(overview_format)
      } else {
        return(data.frame())
      }
    }
    
    # Regular filtering for other cases
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
  
  # Replace the entire viz_performance_data_with_metadata reactive with this:
  viz_performance_data <- reactive({
    ids <- experiment_ids()
    
    if (length(ids) == 0) {
      return(data.frame())
    }
    
    tryCatch({
      py_ids <- r_to_py(as.list(ids))
      # Use the enhanced get_performance_results that now includes technology and caller
      perf_data <- db$get_performance_results(py_ids, c('SNP', 'INDEL'))
      
      # Filter for visualization
      if (nrow(perf_data) == 0) {
        return(data.frame())
      }
      
      filtered_data <- perf_data %>%
        filter(subset == "ALL_REGIONS" | subset == "*") %>%
        filter(!is.na(recall) & !is.na(precision) & !is.na(f1_score))
      
      return(filtered_data)
      
    }, error = function(e) {
      cat("Error in viz_performance_data:", e$message, "\n")
      return(data.frame())
    })
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
  
  # Handle clicks from both plots
  observe({
    # SNP plot clicks
    snp_click <- event_data("plotly_click", source = "snp_plot")
    if (!is.null(snp_click)) {
      plot_clicked_id(snp_click$customdata)
    }
    
    # INDEL plot clicks
    indel_click <- event_data("plotly_click", source = "indel_plot")
    if (!is.null(indel_click)) {
      plot_clicked_id(indel_click$customdata)
    }
  })
  
  # ====================================================================
  # OUTPUTS
  # ====================================================================
  
  # Show experiment count
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
  
  # Make current_mode available to UI
  output$comparison_mode <- reactive({
    current_mode()
  })
  outputOptions(output, "comparison_mode", suspendWhenHidden = FALSE)
  
  # Check if experiments are selected (for conditional panels)
  output$has_selected_experiments <- reactive({
    current_mode() == "manual_selection" && length(table_selected_ids()) > 0
  })
  outputOptions(output, "has_selected_experiments", suspendWhenHidden = FALSE)
  
  # Check if we have a selected point
  output$has_selected_point <- reactive({
    !is.null(plot_clicked_id())
  })
  outputOptions(output, "has_selected_point", suspendWhenHidden = FALSE)
  
  # Badge count for bottom panel
  output$selected_count_badge <- renderText({
    length(table_selected_ids())
  })
  
  # Experiments table (enhanced with selection for experiment comparison)
  output$experiments_table <- DT::renderDataTable({
    df <- experiments_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No experiments found")))
    }
    
    # Configure selection based on comparison mode
    if (current_mode() == "manual_selection") {
      # Multiple selection allowed when in manual selection mode
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
  
  # Selected experiments table (at bottom of page)
  output$compact_selected_experiments <- renderTable({
    ids <- table_selected_ids()
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
  
  # Enhanced visualization info text
  output$viz_experiment_info <- renderText({
    viz_data <- viz_performance_data_with_metadata()
    if (nrow(viz_data) == 0) {
      return("No performance data available for visualization")
    }
    
    exp_count <- length(unique(viz_data$experiment_name))
    snp_count <- nrow(viz_data[viz_data$variant_type == "SNP", ])
    indel_count <- nrow(viz_data[viz_data$variant_type == "INDEL", ])
    
    paste("Visualizing", exp_count, "experiments:")
  })
  
  
  # ============================================================================
  # SNP PLOT OUTPUT
  # ============================================================================
  
  output$snp_plot <- renderPlotly({
    tryCatch({
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
      
      # Safe tooltip creation with proper null checking
      snp_data$tooltip_text <- paste(
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
        geom_point(
          data = snp_data, 
          aes(x = precision, y = recall, 
              fill = technology,           # color by technology
              shape = caller,              # shape by caller
              text = tooltip_text,
              customdata = experiment_id), 
          color = "black", # outline
          stroke = 0.15,
          size = 2.2 # dot size
        ) +
        scale_color_manual(values = technology_colors) +    # manual colors
        scale_shape_manual(values = caller_shapes) +       # manual shapes
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP", x = "Precision", y = "Recall", color = "Experiment") +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12),
          panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey95", size = 0.3)
        )
      
      ggplotly(p, tooltip = "text", source = "snp_plot") %>%
        layout(showlegend = TRUE) %>%
        event_register("plotly_click")
      
    }, error = function(e) {
      cat("Error in SNP plot:", e$message, "\n")
      # Return basic error plot
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error loading SNP data:", e$message), size = 4) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP - Error", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    })
  })
  
  # ============================================================================
  # INDEL PLOT OUTPUT
  # ============================================================================
  
  output$indel_plot <- renderPlotly({
    tryCatch({
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
      
      # Safe tooltip creation with proper null checking
      indel_data$tooltip_text <- paste(
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
              fill = technology,           # color by technology
              shape = caller,              # shape by caller
              text = tooltip_text,
              customdata = experiment_id), 
          size = 2.2,  # dot size
          color = "black", # outline
          stroke = 0.15,
        ) +
        scale_color_manual(values = technology_colors) +    # manual colors
        scale_shape_manual(values = caller_shapes) +       # manual shapes
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL", x = "Precision", y = "Recall", color = "Experiment") +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12),
          panel.grid.major = element_line(color = "grey90", size = 0.5),
          panel.grid.minor = element_line(color = "grey95", size = 0.3)
        )
      
      ggplotly(p, tooltip = "text", source = "indel_plot") %>%
        layout(showlegend = TRUE) %>%
        event_register("plotly_click")
      
    }, error = function(e) {
      cat("Error in INDEL plot:", e$message, "\n")
      # Return basic error plot
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error loading INDEL data:", e$message), size = 4) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL - Error", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    })
  })
  
  # Basic experiment info (always shown when point is clicked)
  output$basic_experiment_info <- renderUI({
    exp_id <- plot_clicked_id()
    if (is.null(exp_id)) return(NULL)
    
    # Get experiment metadata
    py_ids <- r_to_py(list(exp_id))
    metadata <- db$get_experiment_metadata(py_ids)
    
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
  
  # Full experiment metadata (shown when expanded)
  output$full_experiment_metadata <- renderUI({
    exp_id <- plot_clicked_id()
    if (is.null(exp_id)) return(NULL)
    
    # Get experiment metadata
    py_ids <- r_to_py(list(exp_id))
    metadata <- db$get_experiment_metadata(py_ids)
    
    if (nrow(metadata) == 0) return(p("No metadata found"))
    
    meta <- metadata[1, ]
    
    div(
      h5("Complete Experiment Details"),
      div(
        class = "row",
        
        # Column 1: Sequencing
        div(class = "col-md-4",
            wellPanel(
              style = "background-color: white; padding: 15px;",
              h6("Sequencing Technology", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
              p(strong("Technology: "), meta$technology %||% "N/A"),
              p(strong("Platform: "), meta$platform_name %||% "N/A"),
              p(strong("Platform Type: "), meta$platform_type %||% "N/A"),
              p(strong("Platform Version: "), meta$platform_version %||% "N/A"),
              p(strong("Target: "), meta$target %||% "N/A"),
              p(strong("Chemistry: "), meta$chemistry_name %||% "N/A")
            )
        ),
        
        # Column 2: Analysis
        div(class = "col-md-4",
            wellPanel(
              style = "background-color: white; padding: 15px;",
              h6("Analysis Algorithms", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
              p(strong("Variant Caller: "), meta$caller_name %||% "N/A"),
              p(strong("Caller Version: "), meta$caller_version %||% "N/A"),
              p(strong("Caller Type: "), meta$caller_type %||% "N/A"),
              p(strong("Caller Model: "), meta$caller_model %||% "N/A"),
              p(strong("Aligner: "), paste(meta$aligner_name %||% "N/A", meta$aligner_version %||% "")),
              p(strong("Benchmark Tool: "), paste(meta$benchmark_tool_name %||% "N/A", meta$benchmark_tool_version %||% ""))
            )
        ),
        
        # Column 3: Quality & Truth
        div(class = "col-md-4",
            wellPanel(
              style = "background-color: white; padding: 15px;",
              h6("Quality & Benchmarking", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
              p(strong("Mean Coverage: "), ifelse(is.na(meta$mean_coverage), "N/A", paste0(round(meta$mean_coverage, 1), "x"))),
              p(strong("Read Length: "), ifelse(is.na(meta$read_length), "N/A", paste0(meta$read_length, " bp"))),
              p(strong("Mean Insert Size: "), ifelse(is.na(meta$mean_insert_size), "N/A", paste0(meta$mean_insert_size, " bp"))),
              p(strong("Truth Set: "), paste(meta$truth_set_name %||% "N/A", meta$truth_set_version %||% "")),
              p(strong("Sample: "), meta$truth_set_sample %||% "N/A"),
              p(strong("Reference: "), meta$truth_set_reference %||% "N/A")
            )
        )
      ),
      
      # Additional details row
      div(
        class = "row",
        div(class = "col-md-12",
            wellPanel(
              style = "background-color: white; padding: 15px;",
              h6("Additional Details", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 5px;"),
              div(
                class = "row",
                div(class = "col-md-3", p(strong("Variant Type: "), meta$variant_type %||% "N/A")),
                div(class = "col-md-3", p(strong("Variant Origin: "), meta$variant_origin %||% "N/A")),
                div(class = "col-md-3", p(strong("Is Phased: "), ifelse(is.na(meta$is_phased), "N/A", ifelse(meta$is_phased, "Yes", "No")))),
                div(class = "col-md-3", p(strong("Created: "), ifelse(is.na(meta$created_at), "N/A", format(as.POSIXct(meta$created_at), "%Y-%m-%d"))))
              )
            )
        )
      )
    )
  })
  
  # Helper function for null coalescing
  `%||%` <- function(x, y) if (is.null(x) || is.na(x) || x == "") y else x
  
  # ====================================================================
  # SUBMISSION OBSERVERS
  # ====================================================================
  
  observeEvent(input$submit_tech_comparison, {
    # Get all experiment IDs for selected technologies with constant caller
    all_ids <- c()
    for(tech in input$selected_technologies) {
      tech_ids <- db$get_experiments_by_technology(tech)
      
      # Filter by caller to keep it constant
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
  
  observeEvent(input$submit_caller_comparison, {
    # Get all experiment IDs for selected callers with constant technology
    all_ids <- c()
    for(caller in input$selected_callers) {
      caller_ids <- db$get_experiments_by_caller(caller)
      
      # Filter by technology to keep it constant
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
  
  observeEvent(input$submit_experiment_comparison, {
    comparison_submitted(FALSE)
    showNotification("Using selected experiments for comparison", type = "message")
  })
  
  observeEvent(input$submit_bottom_comparison, {
    comparison_submitted(FALSE)
    showNotification("Using selected experiments for comparison", type = "message")
  })
}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))