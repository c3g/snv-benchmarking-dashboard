# ============================================================================
# ui_components.R
# ============================================================================
"
UI components and dynamic outputs for SNV Benchmarking Dashboard.
Creates interface elements that respond automatically to user actions.

Main sections:
1. setup_ui_outputs() - Creates state signals and dynamic content
   - State signals (show/hide UI panels automatically)
   - Sidebar status text (experiment counts, selection feedback)
   - Experiment metadata displays (plot click details)
   - Stratified analysis summaries

2. upload_modal_ui() - Complete form for adding new experiments
   - Multi-step upload process
   - Organized metadata sections (technology, caller, truth set, etc.)
   - Validation and filename preview

3. delete_modal_ui() - List of datasets to select in order to remove from the database
4. Stratified experiment details panel
"
# ============================================================================
# UI OUTPUT SETUP FUNCTION
# ============================================================================

setup_ui_outputs <- function(input, output, session, data_reactives) {
  
  # ====================================================================
  # STATE MANAGEMENT OUTPUTS
  # ====================================================================
  
  # Comparison mode
  output$comparison_mode <- reactive({
    data_reactives$current_mode()
  })
  outputOptions(output, "comparison_mode", suspendWhenHidden = FALSE)
  
  # Selected experiments (tab 1)
  output$has_selected_experiments <- reactive({
    data_reactives$current_mode() == "manual_selection" && length(data_reactives$table_selected_ids()) > 0
  })
  outputOptions(output, "has_selected_experiments", suspendWhenHidden = FALSE)
  
  # Selected point (tab 3)
  output$has_selected_point <- reactive({
    !is.null(data_reactives$plot_clicked_id())
  })
  outputOptions(output, "has_selected_point", suspendWhenHidden = FALSE)
  
  # Stratified data (tab 4)
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
  # VISIBILITY FILTER OUTPUTS 
  # ====================================================================
  
  # Count of user's own experiments for visibility filter display
  output$my_experiments_count <- renderText({
    input$user_authenticated  # Re-run when auth changes
    
    user_info <- get_user_info(session)
    if (is.null(user_info)) return("")
    
    user_id <- session$userData$user_id
    if (is.null(user_id)) return("")
    
    tryCatch({
      user_exps <- db$get_user_experiments(user_id)
      count <- nrow(user_exps)
      
      if (count == 0) {
        "No uploads yet"
      } else {
        paste0("You have ", count, " upload", ifelse(count != 1, "s", ""))
      }
    }, error = function(e) {
      ""
    })
  })
  
  # Showing X experiments indicator
  output$showing_experiments_count <- renderText({
    df <- data_reactives$experiments_data()
    count <- nrow(df)
    paste0("Showing: ", count, " experiment", ifelse(count != 1, "s", ""))
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
    exp_id <- as.numeric(exp_id)
    if (is.na(exp_id) || exp_id <= 0) {
      return(p("Invalid experiment ID"))
    }
    
    # Get experiment metadata
    tryCatch({
      
      metadata <- tryCatch({
        exp_id_json <- json_param(list(exp_id))
        user_info <- get_user_info(session)
        user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
        is_admin_user <- if (!is.null(user_info)) user_info$is_admin else FALSE
        db$get_experiment_metadata(exp_id_json, user_id, is_admin_user)
      }, error = function(e) {
        data.frame(name = "Error loading metadata")
      })
      
      if (nrow(metadata) == 0) {
        return(p("No metadata found for experiment ID:", exp_id))
      }
      
      meta <- metadata[1, ]
      
      div(
        h6(strong("ID: ",exp_id, "-", meta$name), 
           h5("Selected Experiment Details"),
           style = "color: #4472ca; margin-bottom: 10px; font-size: 16px; font-weight: 600;"),
        div(
          class = "row",
          div(class = "col-md-3",
              p(strong("Technology: "), meta$technology %||% "N/A", 
                style = "margin-bottom: 5px; font-size: 14px; line-height: 1.4;")
          ),
          div(class = "col-md-3",
              p(strong("Platform: "), meta$platform_name %||% "N/A", 
                style = "margin-bottom: 5px; font-size: 14px; line-height: 1.4;")
          ),
          div(class = "col-md-3",
              p(strong("Caller: "), paste(meta$caller %||% "N/A", meta$caller_version %||% ""), 
                style = "margin-bottom: 5px; font-size: 14px; line-height: 1.4;")
          ),
          div(class = "col-md-3",
              p(strong("Coverage: "), 
                ifelse(is.na(meta$mean_coverage), "N/A", paste0(round(meta$mean_coverage, 1), "x")), 
                style = "margin-bottom: 5px; font-size: 14px; line-height: 1.4;")
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
      user_info <- get_user_info(session)
      user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
      is_admin_user <- if (!is.null(user_info)) user_info$is_admin else FALSE
      metadata <- db$get_experiment_metadata(exp_id_json, user_id, is_admin_user)
      
      if (nrow(metadata) == 0) {
        return(p("No metadata found for experiment ID:", exp_id))
      }
      
      meta <- metadata[1, ]
      
      # Get performance data for this specific experiment
      tryCatch({
        performance_data <- db$get_experiments_with_performance(exp_id_json, VARIANT_TYPES)
        snp_perf <- performance_data %>% filter(variant_type == "SNP")
        indel_perf <- performance_data %>% filter(variant_type == "INDEL")
      }, error = function(e) {
        snp_perf <- data.frame()
        indel_perf <- data.frame()
      })
      
      div(
        h5(paste("Complete Experiment Details"), 
           style = "font-weight: 600; margin-bottom: 20px; font-size: 17px;"),
        div(
          class = "metadata-grid-4col",
          
          # SEQUENCING PLATFORM CARD
          div(class = "metadata-card",
              h6("Sequencing Platform", 
                 style = "color: #4472ca; font-weight: 600; font-size: 15px; border-bottom: 2px solid #4472ca; padding-bottom: 8px; margin-bottom: 15px;"),
              p(strong("Technology: "), meta$technology %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Platform: "), meta$platform_name %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Platform Type: "), meta$platform_type %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Platform Version: "), meta$platform_version %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Target: "), meta$target %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Chemistry: "), meta$chemistry_name %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
          ),
          
          # ANALYSIS PIPELINE CARD
          div(class = "metadata-card",
              h6("Analysis Pipeline", 
                 style = "color: #4472ca; font-weight: 600; font-size: 15px; border-bottom: 2px solid #4472ca; padding-bottom: 8px; margin-bottom: 15px;"),
              p(strong("Variant Caller: "), meta$caller %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Caller Version: "), meta$caller_version %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Caller Type: "), meta$caller_type %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Caller Model: "), meta$caller_model %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Aligner: "), paste(meta$aligner_name %||% "N/A", meta$aligner_version %||% ""), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Benchmark Tool: "), paste(meta$benchmark_tool_name %||% "N/A", meta$benchmark_tool_version %||% ""), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
          ),
          
          # QUALITY METRICS CARD
          div(class = "metadata-card",
              h6("Quality Metrics", 
                 style = "color: #4472ca; font-weight: 600; font-size: 15px; border-bottom: 2px solid #4472ca; padding-bottom: 8px; margin-bottom: 15px;"),
              p(strong("Mean Coverage: "), ifelse(is.na(meta$mean_coverage), "N/A", paste0(round(meta$mean_coverage, 1), "x")), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Read Length: "), 
                ifelse(is.na(meta$read_length), 
                       ifelse(is.na(meta$mean_read_length), "N/A", paste0(meta$mean_read_length, " bp (mean)")), 
                       paste0(meta$read_length, " bp")), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Mean Insert Size: "), ifelse(is.na(meta$mean_insert_size), "N/A", paste0(meta$mean_insert_size, " bp")), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Created: "), ifelse(is.na(meta$created_at), "N/A", format(as.POSIXct(meta$created_at), "%Y-%m-%d")), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
          ),
          
          # VARIANTS & TRUTH SET CARD
          div(class = "metadata-card",
              h6("Variants & Truth Set", 
                 style = "color: #4472ca; font-weight: 600; font-size: 15px; border-bottom: 2px solid #4472ca; padding-bottom: 8px; margin-bottom: 15px;"),
              p(strong("Variant Type: "), meta$variant_type %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Variant Origin: "), meta$variant_origin %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Variant Size: "), meta$variant_size %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Is Phased: "), ifelse(is.na(meta$is_phased), "N/A", ifelse(meta$is_phased, "Yes", "No")), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Truth Set: "), paste(meta$truth_set_name %||% "N/A", meta$truth_set_version %||% ""), 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Sample: "), meta$truth_set_sample %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
              p(strong("Reference: "), meta$truth_set_reference %||% "N/A", 
                style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
          ),
          
          # SNP PERFORMANCE CARD
          div(class = "metadata-card",
              h6("SNP Performance", 
                 style = "color: #4472ca; font-weight: 600; font-size: 15px; border-bottom: 2px solid #4472ca; padding-bottom: 8px; margin-bottom: 15px;"),
              if (nrow(snp_perf) > 0) {
                tagList(
                  p(strong("F1 Score: "), paste0(round(snp_perf$f1_score * 100, 2), "%"), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("Precision: "), paste0(round(snp_perf$precision * 100, 2), "%"), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("Recall: "), paste0(round(snp_perf$recall * 100, 2), "%"), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("True Positives: "), format(snp_perf$truth_tp, big.mark = ","), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("False Negatives: "), format(snp_perf$truth_fn, big.mark = ","), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("False Positives: "), format(snp_perf$query_fp, big.mark = ","), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
                )
              } else {
                p("No SNP performance data available", 
                  style = "color: #6c757d; font-style: italic; margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
              }
          ),
          
          # INDEL PERFORMANCE CARD
          div(class = "metadata-card",
              h6("INDEL Performance", 
                 style = "color: #4472ca; font-weight: 600; font-size: 15px; border-bottom: 2px solid #4472ca; padding-bottom: 8px; margin-bottom: 15px;"),
              if (nrow(indel_perf) > 0) {
                tagList(
                  p(strong("F1 Score: "), paste0(round(indel_perf$f1_score * 100, 2), "%"), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("Precision: "), paste0(round(indel_perf$precision * 100, 2), "%"), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("Recall: "), paste0(round(indel_perf$recall * 100, 2), "%"), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("True Positives: "), format(indel_perf$truth_tp, big.mark = ","), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("False Negatives: "), format(indel_perf$truth_fn, big.mark = ","), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;"),
                  p(strong("False Positives: "), format(indel_perf$query_fp, big.mark = ","), 
                    style = "margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
                )
              } else {
                p("No INDEL performance data available", 
                  style = "color: #6c757d; font-style: italic; margin-bottom: 8px; font-size: 14px; line-height: 1.4;")
              }
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

  # Stratified analysis summary - not used for now 
  'output$stratified_summary <- renderUI({
    data <- data_reactives$stratified_filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    n_experiments <- length(unique(data$experiment_id))
    n_regions <- length(unique(data$subset))
    n_results <- nrow(data)
      
      HTML(paste0(
        "<strong>Analysis Summary:</strong> ",
        n_experiments, " experiments x",
        n_regions, " regions = ",
        n_results, " total results"
      ))
    })'

  # Experiment count for stratified analysis
  output$stratified_experiment_count <- renderText({
    data <- data_reactives$stratified_filtered_data()
    
    if (nrow(data) == 0) {
      return("")
    }
    
    n_experiments <- length(unique(data$experiment_id))
    paste0("(", n_experiments, " experiment", ifelse(n_experiments != 1, "s", ""), ")")
  })
  
  # Experiment info list with technology-based colors
  output$experiment_info_list <- renderUI({
    data <- data_reactives$stratified_filtered_data()
    
    if (nrow(data) == 0) {
      return(p("No experiments selected", style = "color: #6c757d;"))
    }
    
    experiment_ids <- unique(data$experiment_id)
    
    # Use json_param for reliable JSON formatting
    tryCatch({
      ids_json <- json_param(experiment_ids)
      metadata <- db$get_experiment_metadata(ids_json)
      
      if (nrow(metadata) == 0) {
        return(p("Unable to load experiment metadata", style = "color: #dc3545;"))
      }
      
      # Header row
      header <- div(
        style = "padding: 6px 10px; margin-bottom: 8px; 
                background: #e9ecef; border-radius: 4px;
                display: flex; align-items: center; gap: 10px;
                font-size: 11px; font-weight: 600; color: #495057;",
        
        div(style = "width: 50px;", "ID"),
        span("│", style = "color: #adb5bd;"),
        div(style = "width: 75px;", "Technology"),
        span("│", style = "color: #adb5bd;"),
        div(style = "width: 85px;", "Platform"),
        span("│", style = "color: #adb5bd;"),
        div(style = "width: 90px;", "Caller"),
        span("│", style = "color: #adb5bd;"),
        div(style = "width: 120px;", "Version"),
        span("│", style = "color: #adb5bd;"),
        div(style = "width: 70px;", "Chemistry"),
        span("│", style = "color: #adb5bd;"),
        div(style = "width: 60px;", "Coverage"),
        span("│", style = "color: #adb5bd;"),
        div(style = "width: 90px;", "Truth Set")
      )
      
      # Data rows with technology-based colors
      rows <- lapply(1:nrow(metadata), function(i) {
        exp <- metadata[i, ]
        
        # Get color from technology_colors mapping
        tech <- exp$technology %||% "Unknown"
        exp_color <- technology_colors[tech]
        if (is.null(exp_color) || is.na(exp_color)) {
          exp_color <- technology_colors["Unknown"]
        }
        
        div(
          style = paste0("padding: 6px 10px; margin-bottom: 3px; 
                  background: linear-gradient(90deg, #fafbfc 0%, #ffffff 100%);
                  border-left: 3px solid ", exp_color, "; border-radius: 4px;
                  display: flex; align-items: center; gap: 10px;
                  font-size: 11px;"),
          
          # ID with color dot
          div(
            style = "width: 50px; display: flex; align-items: center; gap: 5px;",
            span(style = paste0("width: 7px; height: 7px; border-radius: 50%; 
                                background: ", exp_color, "; flex-shrink: 0;")),
            strong(paste0("ID:", exp$id), style = "color: #2c3e50;")
          ),
          
          span("│", style = "color: #dee2e6;"),
          
          # Technology
          div(style = "width: 75px; color: #495057;",
              exp$technology %||% "N/A"),
          
          span("│", style = "color: #dee2e6;"),
          
          # Platform
          div(style = "width: 85px; color: #495057;",
              exp$platform_name %||% "N/A"),
          
          span("│", style = "color: #dee2e6;"),
          
          # Caller
          div(style = "width: 90px; color: #495057;",
              exp$caller %||% "N/A"),
          
          span("│", style = "color: #dee2e6;"),
          
          # Version
          div(style = "width: 120px; color: #495057;",
              exp$caller_version %||% "N/A"),
          
          span("│", style = "color: #dee2e6;"),
          
          # Chemistry
          div(style = "width: 70px; color: #495057;",
              exp$chemistry_name %||% "N/A"),
          
          span("│", style = "color: #dee2e6;"),
          
          # Coverage
          div(style = "width: 60px; color: #495057; font-weight: 500;",
              ifelse(is.na(exp$mean_coverage), "N/A", 
                    paste0(round(exp$mean_coverage, 1), "x"))),
          
          span("│", style = "color: #dee2e6;"),
          
          # Truth set
          div(style = "width: 90px; color: #495057;",
              paste0(exp$truth_set_name %||% "N/A", " ", 
                    exp$truth_set_version %||% ""))
        )
      })
      
      return(div(header, rows))
      
    }, error = function(e) {
      p(paste("Error loading experiment metadata:", e$message), 
        style = "color: #dc3545; font-size: 12px;")
    })
  })
  
  # Prevent suspension when container is hidden
  outputOptions(output, "experiment_info_list", suspendWhenHidden = FALSE)
  outputOptions(output, "stratified_experiment_count", suspendWhenHidden = FALSE)
  
  # ====================================================================
  # UPLOAD FILENAME PREVIEW
  # ====================================================================
  
    # filename preview
  output$filename_preview <- renderText({

    # Helper function to clean values
    strip_value <- function(value) {
      if (is.null(value) || is.na(value) || value == "") {
        return("")
      }
      return(trimws(as.character(value)))
    }

    # Check all required fields are filled
    if (!is.null(input$exp_name) && input$exp_name != "" &&
        !is.null(input$technology) && input$technology != "" &&
        !is.null(input$platform_name) && input$platform_name != "" &&
        !is.null(input$caller_name) && input$caller_name != "" &&
        !is.null(input$truth_set_name) && input$truth_set_name != "") {
      
      tryCatch({
        # Get next experiment ID with fallback
        next_id <- tryCatch({
          overview <- db$get_experiments_overview()
          if (is.null(overview) || nrow(overview) == 0) {
            1
          } else {
            max(overview$id, na.rm = TRUE) + 1
          }
        }, error = function(e) {
          1  # Default to 1 if database call fails
        })
        
        preview_id <- sprintf("%03d", next_id)
        
        # Extract sample name
        sample <- strsplit(strip_value(input$exp_name), "_")[[1]][1]
        if (is.na(sample) || sample == "") sample <- "UNKNOWN"
        
        # Clean metadata components
        technology <- tolower(strip_value(input$technology))
        platform <- tolower(strip_value(input$platform_name))
        caller <- tolower(strip_value(input$caller_name))
        truthset <- tolower(strip_value(input$truth_set_name))
        
        # Build filename
        filename <- paste0(preview_id, "_", sample, "_", technology, "_", platform, "_", caller, "_", truthset, ".csv")
        
        return(paste("Preview:", filename))
        
      }, error = function(e) {
        return(paste("Preview error:", e$message))
      })
      
    } else {
      return("Fill required fields (*) to see filename preview")
    }
  })
  outputOptions(output, "filename_preview", suspendWhenHidden = FALSE, priority = 10)
}
# ============================================================================
# UPLOAD UI COMPONENTS
# ============================================================================

# Upload Modal UI
upload_modal_ui <- function() {
  bsModal(
    "upload_modal", 
    "Add New Experiment", 
    "show_upload_modal", 
    size = "large",
    
    div(
      style = "background-color: #e7f3ff; border: 1px solid #4472ca; border-radius: 6px; 
              padding: 16px; margin-bottom: 20px;",
      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
        icon("info-circle", style = "color: #4472ca; font-size: 22px;"),
        h5("File Requirements", style = "margin: 0; color: #4472ca; font-weight: 600;")
      ),
      p(
        style = "margin: 0 0 8px 32px; color: #2c5282; font-size: 14px;",
        "Upload must be a ", strong("hap.py CSV output file"), " containing the following columns:"
      ),
      tags$ul(
        style = "margin: 0 0 0 45px; color: #2c5282; font-size: 13px;",
        tags$li(code("Type"), " - Variant type (SNP/INDEL)"),
        tags$li(code("Subtype"), " - Variant subtype"),
        tags$li(code("Subset"), " - Genomic region"),
        tags$li(code("METRIC.Recall"), " - Recall metric"),
        tags$li(code("METRIC.Precision"), " - Precision metric"),
        tags$li(code("METRIC.F1_Score"), " - F1 score metric")
      )
    ),
    # 1: File Upload
    wellPanel(
      style = "background-color: #f8f9fa; margin-bottom: 20px;",
      fluidRow(
        column(8,
               h4("1. Upload hap.py CSV File"),
               fileInput("upload_file", 
                         "Select File",
                         accept = ".csv",
                         placeholder = "Choose hap.py CSV file...")
        ),
        column(4,
               br(),
               div(id = "file_status", style = "margin-top: 10px;")
        )
      )
    ),
    
    # 2: Metadata Form
    wellPanel(
      h4("2. Experiment Metadata"),
      
      # Experiment Info
      fluidRow(
        column(4,
               textInput("exp_name", "Experiment Name*", 
                         placeholder = "e.g., HG002_Illumina_NovaSeq")
        ),
        column(4,
               textInput("description", "Description", 
                         placeholder = "Optional description")
        ),
        column(4,
               selectInput("target", "Target",
                           choices = c("WGS" = "wgs", "WES" = "wes"),
                           selected = "wgs")
        )
      ),
      
      # Sequencing Technology
      h5("Sequencing Technology", style = "color: #4472ca; margin-top: 20px; font-weight: 600;"),
      fluidRow(
        column(3,
          selectInput("technology", "Technology*",
                      choices = c("", "ILLUMINA", "PACBIO", "ONT", "MGI", "10X"))
        ),
        column(3,
               textInput("platform_name", "Platform*")
        ),
        column(3,
               selectInput("platform_type", "Platform Type*",
                choices = c("", "SRS" = "srs", "LRS" = "lrs", "Synthetic" = "synthetic"))        ),
        column(3,
               textInput("platform_version", "Platform Version",
                         placeholder = "Optional")
        )
      ),
      
      # Chemistry
      fluidRow(
        column(6,
               textInput("chemistry_name", "Chemistry",
                         placeholder = "Optional")
        ),
        column(6,
               textInput("chemistry_version", "Chemistry Version",
                         placeholder = "Optional")
        )
      ),
      
      # Variant Caller
      h5("Variant Caller", style = "color: #4472ca; margin-top: 20px; font-weight: 600;"),
      fluidRow(
        column(3,
          selectInput("caller_name", "Caller*",
                      choices = c("", "DEEPVARIANT", "CLAIR3", "DRAGEN",
                                  "GATK3", "GATK4", "LONGRANGER", "MEGABOLT",
                                  "NANOCALLER", "PARABRICK", "PEPPER"))
        ),
        column(3,
               selectInput("caller_type", "Caller Type*",
                           choices = c("","ML" = "ml", "Traditional" = "traditional"))
        ),
        column(3,
               textInput("caller_version", "Caller Version*",
                         placeholder = "Required")
        ),
        column(3,
               textInput("caller_model", "Caller Model",
                         placeholder = "Optional")
        )
      ),
      
      # Aligner
      h5("Aligner", style = "color: #4472ca; margin-top: 20px; font-weight: 600;"),
      fluidRow(
        column(6,
               textInput("aligner_name", "Aligner",
                         placeholder = "Optional")
        ),
        column(6,
               textInput("aligner_version", "Aligner Version",
                         placeholder = "Optional")
        )
      ),
      
      # Truth Set
      h5("Truth Set", style = "color: #4472ca; margin-top: 20px; font-weight: 600;"),
      fluidRow(
        column(3,
               selectInput("truth_set_name", "Truth Set*",
                           choices = c("", "GIAB" = "giab", "CMRG" = "cmrg", "T2T" = "t2t"))
        ),
        column(3,
               selectInput("truth_set_sample", "Sample",
                           choices = c("HG001" = "hg001", "HG002" = "hg002", "HG003" = "hg003", "HG004" = "hg004"),
                           selected = "hg002")
        ),
        column(3,
               textInput("truth_set_version", "Truth Set Version")
        ),
        column(3,
               selectInput("truth_set_reference", "Reference",
                           choices = c("GRCh37" = "grch37", "GRCh38" = "grch38"),
                           selected = "grch38")
        )
      ),
      
      # Variant Info
      h5("Variant Information", style = "color: #4472ca; margin-top: 20px; font-weight: 600;"),
      fluidRow(
        column(3,
               selectInput("variant_type", "Variant Type",
                           choices = c("SNP+INDEL" = "snp+indel", "SNP" = "snp", "INDEL" = "indel"),
                           selected = "snp+indel")
        ),
        column(3,
               selectInput("variant_size", "Variant Size",
                           choices = c("Small" = "small", "Large" = "large"),
                           selected = "small")
        ),
        column(3,
               selectInput("variant_origin", "Variant Origin",
                           choices = c("Germline" = "germline", "Somatic" = "somatic"),
                           selected = "germline")
        ),
        column(3,
               selectInput("is_phased", "Phased",
                           choices = c("No" = "false", "Yes" = "true"),
                           selected = "false")
        )
      ),
      
      # Benchmark Tool
      h5("Benchmark Tool", style = "color: #4472ca; margin-top: 20px; font-weight: 600;"),
      fluidRow(
        column(6,
               selectInput("benchmark_tool_name", "Benchmark Tool",
                           choices = c("hap.py" = "hap.py", "vcfdist" = "vcfdist", "truvari" = "truvari"),
                           selected = "hap.py")
        ),
        column(6,
               textInput("benchmark_tool_version", "Tool Version")
        )
      ),
      
      # Quality Metrics
      h5("Quality Metrics", style = "color: #4472ca; margin-top: 20px; font-weight: 600;"),
      fluidRow(
        column(2,
               numericInput("mean_coverage", "Mean Coverage*", 
                            value = NA, min = 1, max = 200, step = 0.1)
        ),
        column(3,
               numericInput("read_length", "Read Length (bp) _ Short Read Sequencing", 
                            value = NA, min = 50, max = 500, step = 0.1)
        ),
        column(3,
               numericInput("mean_insert_size", "Mean Insert Size (bp) _ Short Read Sequencing", 
                            value = NA, min = 100, max = 1000, step = 1)
        ),
        column(4,
               numericInput("mean_read_length", "Mean Read Length (bp) _ Long Read Sequencing", 
                            value = NA, min = 100, max = 100000, step = 0.1)
        )
      )
    ),
    
# 3: Preview & Submit
    wellPanel(
      h4("3. Review & Submit"),
      fluidRow(
               div(
                 h5("Generated Filename:"),
                 verbatimTextOutput("filename_preview", placeholder = TRUE)
        )),
        fluidRow(
        column(6,
              br(),
               h5("Visibility"),
               br(),
               radioButtons(
                 "experiment_visibility",
                 NULL,
                 choices = list(
                   "Public" = "public",
                   "Private" = "private"
                 ),
                 selected = "public"
               ),
        ),
        column(6,
               div(style = "text-align: center; padding-top: 35px;",
                   actionButton("submit_upload", "Add to Database", 
                                class = "btn-success btn-lg", 
                                style = "min-width: 130px; background-color: #42a65c; border: none;")
               )
        )
      ),
      
      br(),
      div(id = "upload_status", style = "margin-top: 15px;")
    )

  )
}

# ============================================================================
# DELETE UI COMPONENTS
# ============================================================================

# Delete Modal UI
delete_modal_ui <- function() {
  bsModal(
    "delete_modal", 
    "Delete Datasets", 
    "show_delete_modal", 
    size = "large",
    
    # Warning message
    div(
      class = "alert alert-danger",
      style = "margin-bottom: 20px;",
      h5(icon("exclamation-triangle"), " Warning: Permanent Deletion"),
      p("This action will permanently delete selected experiments and their associated files.")
    ),
    
    # Experiment selection
      h4("Select Experiments to Delete"),
      div(
        style = "max-height: 400px; overflow-y: auto; border: 1px solid #dee2e6; padding: 10px;",
        DT::dataTableOutput("delete_experiments_table")
      ),
    
    # Action buttons
    div(
      style = "text-align: right; margin-top: 20px;",
      actionButton("cancel_delete", "Cancel", class = "btn-secondary"),
      actionButton("confirm_delete_selected", "Delete Selected", 
                   class = "btn-danger", style = "margin-left: 10px;")
    ),
    
    # Status display
    div(id = "delete_status", style = "margin-top: 15px;")
  )
}

# ============================================================================
# FILE BROWSER UI COMPONENTS  
# ============================================================================

file_browser_modal_ui <- function() {
  bsModal(
    "file_browser_modal",
    "File Browser",
    "show_file_browser",
    size = "large",
    
    # Path display
    div(
      style = "margin-bottom: 10px; padding: 10px; background: #f8f9fa; border-radius: 4px;",
      strong("Path: "), textOutput("fb_current_path", inline = TRUE)
    ),
    
    # File table container
    div(
      style = "max-height: 370px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 4px;",
      uiOutput("fb_files_table_ui")
    ),
    
    # Selected file display
    div(
      style = "margin-top: 15px; padding: 10px; background: #e9ecef; border-radius: 4px;",
      strong("Selected: "), textOutput("fb_selected_display", inline = TRUE)
    ),
    
    # Action buttons
    div(
      style = "margin-top: 15px; display: flex; gap: 10px; justify-content: flex-end; align-items: center;",
      fileInput("fb_upload_file", NULL, buttonLabel = "Upload File", multiple = FALSE),
      downloadButton("fb_download_all_btn", "Download All", class = "btn-primary btn-sm"),
      downloadButton("fb_download_btn", "Download", class = "btn-info btn-sm"),
      actionButton("fb_rename_btn", "Rename", class = "btn-warning btn-sm"),
      actionButton("fb_delete_btn", "Delete", class = "btn-danger btn-sm")
    )
  )
}
# ============================================================================
# STRATIFIED EXPERIMENT DETAILS PANEL
# ============================================================================
create_experiment_details_panel_ui <- function() {
  div(
    style = "margin-bottom: 20px;",
    
    # Header
    div(
      onclick = "toggleExpList()",
      style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; 
               padding: 10px 16px; cursor: pointer; display: flex; 
               justify-content: space-between; align-items: center;",
      
      div(
        span(id = "exp_list_arrow", "▶", style = "font-size: 11px; margin-right: 8px;"),
        strong("Show Experiment Information "),
        textOutput("stratified_experiment_count", inline = TRUE)
      ),
      
      span("Click to expand", style = "font-size: 12px; color: #6c757d; font-style: italic;")
    ),
    
    # List container
    div(
      id = "experiment_list_container",
      style = "display: none; margin-top: 8px; padding: 12px; 
               background: white; border: 1px solid #dee2e6; border-radius: 6px;",
      uiOutput("experiment_info_list")
    ),
    
    tags$script(HTML("
      function toggleExpList() {
        var container = document.getElementById('experiment_list_container');
        var arrow = document.getElementById('exp_list_arrow');
        
        if (container.style.display === 'none') {
          container.style.display = 'block';
          arrow.innerHTML = '▼';
        } else {
          container.style.display = 'none';
          arrow.innerHTML = '▶';
        }
      }
    "))
  )
}

# ============================================================================
# ADMIN PANEL UI COMPONENTS
# ============================================================================
# creates the admin panel UI content
admin_panel_ui <- function() {
  div(
    class = "container-fluid",
    style = "padding: 20px;",
    
    # Header
    div(
      style = "margin-bottom: 20px; border-bottom: 1px solid #e4e7ea; padding-bottom: 15px;",
      h4( " Admin Dashboard", 
         style = "color: #4472ca; font-weight: 600; margin: 0;"),
      p("Manage experiments, users, and system settings", 
        style = "color: #6c757d; margin: 5px 0 0 0; font-size: 13px;")
    ),
    
    # Statistics Cards Row
    div(
      class = "row",
      style = "margin-bottom: 20px;",
      
      # Total Experiments
      div(
        class = "col-md-2",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; 
                   padding: 15px; text-align: center;",
          p("Total Experiments", style = "margin: 0 0 5px 0; font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px;"),
          h3(textOutput("admin_total_experiments", inline = TRUE), 
             style = "margin: 0; font-weight: 600; font-size: 24px; color: #4472ca;")
        )
      ),
      
      # Public
      div(
        class = "col-md-2",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; 
                   padding: 15px; text-align: center;",
          p("Public", style = "margin: 0 0 5px 0; font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px;"),
          h3(textOutput("admin_public_experiments", inline = TRUE), 
             style = "margin: 0; font-weight: 600; font-size: 24px; color: #2e7d32;")
        )
      ),
      
      # Private
      div(
        class = "col-md-2",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; 
                   padding: 15px; text-align: center;",
          p("Private", style = "margin: 0 0 5px 0; font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px;"),
          h3(textOutput("admin_private_experiments", inline = TRUE), 
             style = "margin: 0; font-weight: 600; font-size: 24px; color: #c62828;")
        )
      ),
      
      # Users
      div(
        class = "col-md-2",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; 
                   padding: 15px; text-align: center;",
          p("Users", style = "margin: 0 0 5px 0; font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px;"),
          h3(textOutput("admin_total_users", inline = TRUE), 
             style = "margin: 0; font-weight: 600; font-size: 24px; color: #4472ca;")
        )
      ),
      
      # Admins
      div(
        class = "col-md-2",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; 
                   padding: 15px; text-align: center;",
          p("Admins", style = "margin: 0 0 5px 0; font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px;"),
          h3(textOutput("admin_admin_users", inline = TRUE), 
             style = "margin: 0; font-weight: 600; font-size: 24px; color: #5c6bc0;")
        )
      ),
      
      # This Week uploads
      div(
        class = "col-md-2",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; 
                   padding: 15px; text-align: center;",
          p("This Week", style = "margin: 0 0 5px 0; font-size: 11px; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px;"),
          h3(textOutput("admin_recent_uploads", inline = TRUE), 
             style = "margin: 0; font-weight: 600; font-size: 24px; color: #6c757d;")
        )
      )
    ),
    
    # Quick Actions Row
    div(
      class = "row",
      style = "margin-bottom: 20px;",
      div(
        class = "col-md-12",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; padding: 15px;",
          h5("Quick Actions", style = "margin: 0 0 12px 0; font-weight: 600; color: #333; font-size: 14px;"),
          div(
            style = "display: flex; gap: 10px; flex-wrap: wrap;",
            actionButton("admin_upload_btn", 
                        tagList(icon("upload"), " Upload Dataset"),
                        class = "btn-sm",
                          style = "padding: 6px 16px; background-color: white; border: 1px solid #4472ca; color: #4472ca;"),
            actionButton("admin_delete_btn", 
                        tagList(icon("trash"), " Delete Datasets"),
                        class = "btn-default btn-sm",
                        style = "padding: 8px 16px; border: 1px solid #d9534f; color: #d9534f; background: white;"),
            actionButton("admin_file_browser_btn", 
                        tagList(icon("folder-open"), " File Browser"),
                        class = "btn-default btn-sm",
                        style = "padding: 8px 16px;")
          )
        )
      )
    ),
    
    # Main Content - Two Column Layout
    div(
      class = "row",
      
      # Left Column - Private Uploads
      div(
        class = "col-md-8",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; padding: 20px;",
          
          # Header with count
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
            h5("Private Uploads", style = "margin: 0; font-weight: 600; color: #333; font-size: 14px;"),
            span(
              style = "background-color: #f8f9fa; color: #6c757d; padding: 4px 10px; border-radius: 4px; font-size: 12px; border: 1px solid #e4e7ea;",
              textOutput("admin_private_count_badge", inline = TRUE)
            )
          ),
          
          # Info text
          p("Review and manage private experiments uploaded by all users.",
            style = "color: #6c757d; font-size: 12px; margin-bottom: 12px;"),
          
          # Private experiments table (using uiOutput)
          div(
            style = "border: 1px solid #e9ecef; border-radius: 4px; max-height: 300px; overflow-y: auto;",
            uiOutput("admin_private_experiments_ui")
          ),
          
          # Action buttons for selected
          div(
            style = "margin-top: 12px; padding-top: 12px; border-top: 1px solid #e9ecef; display: flex; gap: 8px;",
            actionButton("admin_make_public_btn", 
                        tagList(icon("globe"), " Make Public"),
                        class = "btn-default btn-sm",
                        style = "padding: 6px 12px; border: 1px solid #5cb85c; color: #5cb85c; background: white;"),
            actionButton("admin_view_experiment_btn", 
                        tagList(icon("eye"), " View"),
                        class = "btn-default btn-sm",
                        style = "padding: 6px 12px;"),
            actionButton("admin_delete_private_btn", 
                        tagList(icon("trash"), " Delete"),
                        class = "btn-default btn-sm",
                        style = "padding: 6px 12px; border: 1px solid #d9534f; color: #d9534f; background: white;")
          )
        )
      ),
      
      # Right Column - Users List
      div(
        class = "col-md-4",
        div(
          style = "background: #ffffff; border: 1px solid #e4e7ea; border-radius: 6px; padding: 20px;",
          
          # Header
          h5("Registered Users", style = "margin: 0 0 12px 0; font-weight: 600; color: #333; font-size: 14px;"),
          
          # Users table (using uiOutput)
          div(
            style = "border: 1px solid #e9ecef; border-radius: 4px; max-height: 300px; overflow-y: auto;",
            uiOutput("admin_users_ui")
          )
        )
      )
    )
  )
}
# Admin Dashboard Modal (wraps panel UI)
admin_modal_ui <- function() {
  bsModal(
    id = "admin_modal",
    title = tagList(icon("cog"), " Admin Dashboard"),
    trigger = NULL,
    size = "large",
    admin_panel_ui()
  )
}