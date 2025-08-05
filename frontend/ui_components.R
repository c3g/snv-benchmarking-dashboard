# ============================================================================
# ui_components.R
# ============================================================================
"
Reusable UI components and output functions for SNV Benchmarking Dashboard.

Main components:
- Status and info output functions
- Metadata display components
- UI state management outputs
- Reusable alert and notification components
"

# ============================================================================
# UI OUTPUT SETUP FUNCTION
# ============================================================================

setup_ui_outputs <- function(input, output, session, data_reactives) {
  
  # ====================================================================
  # STATE MANAGEMENT OUTPUTS
  # ====================================================================
  
  # Comparison mode indicator
  output$comparison_mode <- reactive({
    data_reactives$current_mode()
  })
  outputOptions(output, "comparison_mode", suspendWhenHidden = FALSE)
  
  # Selected experiments check
  output$has_selected_experiments <- reactive({
    data_reactives$current_mode() == "manual_selection" && length(data_reactives$table_selected_ids()) > 0
  })
  outputOptions(output, "has_selected_experiments", suspendWhenHidden = FALSE)
  
  # Selected point check
  output$has_selected_point <- reactive({
    !is.null(data_reactives$plot_clicked_id())
  })
  outputOptions(output, "has_selected_point", suspendWhenHidden = FALSE)
  
  # Stratified data availability check
  output$has_stratified_data <- reactive({
    data_reactives$stratified_triggered() && nrow(data_reactives$stratified_filtered_data()) > 0
  })
  outputOptions(output, "has_stratified_data", suspendWhenHidden = FALSE)
  
  # ====================================================================
  # SIDEBAR STATUS OUTPUTS
  # ====================================================================
  
  # Selected experiment count
  output$selected_experiments_count <- renderText({
    if (data_reactives$current_mode() != "manual_selection") {
      return("Not in selection mode")
    }
    
    count <- length(data_reactives$table_selected_ids())
    if (count == 0) {
      "No experiments selected"
    } else if (count == 1) {
      "Selected: 1 experiment"
    } else {
      paste("Selected:", count, "experiments")
    }
  })
  
  # Selected count badge for bottom panel
  output$selected_count_badge <- renderText({
    length(data_reactives$table_selected_ids())
  })
  
  # ====================================================================
  # EXPERIMENT METADATA OUTPUTS (TAB 3)
  # ====================================================================
  
  # Basic experiment info (always shown when point is clicked)
  output$basic_experiment_info <- renderUI({
    exp_id <- data_reactives$plot_clicked_id()
    
    # Validation checks
    if (is.null(exp_id) || is.na(exp_id) || length(exp_id) == 0) {
      return(NULL)
    }
    
    # Ensure exp_id is numeric and valid
    exp_id <- as.numeric(exp_id)
    if (is.na(exp_id) || exp_id <= 0) {
      return(p("Invalid experiment ID"))
    }
    
    # Get experiment metadata with error handling
    tryCatch({
      exp_id_json <- json_param(list(exp_id))
      metadata <- db$get_experiment_metadata(exp_id_json)
      
      if (nrow(metadata) == 0) {
        return(p("No metadata found for experiment ID:", exp_id))
      }
      
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
              p(strong("Caller: "), paste(meta$caller %||% "N/A", meta$caller_version %||% ""), style = "margin-bottom: 5px;")
          ),
          div(class = "col-md-3",
              p(strong("Coverage: "), 
                ifelse(is.na(meta$mean_coverage), "N/A", paste0(round(meta$mean_coverage, 1), "x")), 
                style = "margin-bottom: 5px;")
          )
        )
      )
    }, error = function(e) {
      p("Error loading metadata: ", e$message, style = "color: red;")
    })
  })
  
  # Full experiment metadata (shown when expanded)
  output$full_experiment_metadata <- renderUI({
    exp_id <- data_reactives$plot_clicked_id()
    
    # Validation checks
    if (is.null(exp_id) || is.na(exp_id) || length(exp_id) == 0) {
      return(NULL)
    }
    
    exp_id <- as.numeric(exp_id)
    if (is.na(exp_id) || exp_id <= 0) {
      return(p("Invalid experiment ID"))
    }
    
    # Get experiment metadata 
    tryCatch({
      exp_id_json <- json_param(list(exp_id))
      metadata <- db$get_experiment_metadata(exp_id_json)
      
      if (nrow(metadata) == 0) {
        return(p("No metadata found for experiment ID:", exp_id))
      }
      
      meta <- metadata[1, ]
      
      div(
        h5("Complete Experiment Details"),
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
              p(strong("Variant Caller: "), meta$caller %||% "N/A"),
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
    }, error = function(e) {
      p("Error loading full metadata: ", e$message, style = "color: red;")
    })
  })
  
  # ====================================================================
  # STRATIFIED ANALYSIS OUTPUTS (TAB 4)
  # ====================================================================
  
  # Stratified analysis summary
  output$stratified_summary <- renderUI({
    data <- data_reactives$stratified_filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    n_experiments <- length(unique(data$experiment_id))
    n_regions <- length(unique(data$subset))
    n_results <- nrow(data)
    
    HTML(paste0(
      "<strong>Analysis Summary:</strong> ",
      n_experiments, " experiments × ",
      n_regions, " regions = ",
      n_results, " total results"
    ))
  })
}