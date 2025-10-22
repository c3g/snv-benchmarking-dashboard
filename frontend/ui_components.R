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
   - Multi-step upload process (file → metadata → submit)
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
        db$get_experiment_metadata(exp_id_json)
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
      metadata <- db$get_experiment_metadata(exp_id_json)
      
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
    output$experiment_info_list <- renderUI({
      data <- data_reactives$stratified_filtered_data()
      
      if (nrow(data) == 0) {
        return(p("No experiments selected", style = "color: #6c757d;"))
      }
      
      experiment_ids <- unique(data$experiment_id)
      metadata <- db$get_experiment_metadata(toJSON(experiment_ids))
      
      if (nrow(metadata) == 0) return(NULL)
      
      exp_colors <- setNames(
        colorRampPalette(c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", 
                          "#9b59b6", "#1abc9c"))(length(experiment_ids)),
        experiment_ids
      )
      
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
      
      # Data rows
      rows <- lapply(1:nrow(metadata), function(i) {
        exp <- metadata[i, ]
        exp_color <- exp_colors[as.character(exp$id)]
        
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
    })
  # ====================================================================
  # UPLOAD FILENAME PREVIEW
  # ====================================================================
  
  # filename preview
  output$filename_preview <- renderText({
    if (!is.null(input$exp_name) && input$exp_name != "" &&
        !is.null(input$technology) && input$technology != "" &&
        !is.null(input$platform_name) && input$platform_name != "" &&
        !is.null(input$caller_name) && input$caller_name != "" &&
        !is.null(input$truth_set_name) && input$truth_set_name != "") {
      
      # Helper function to clean values
      strip_value <- function(value) {
        if (is.null(value) || is.na(value) || value == "") {
          return("")
        }
        return(trimws(as.character(value)))
      }
      
      tryCatch({
        # Get next experiment ID 
        overview <- db$get_experiments_overview()
        next_id <- max(overview$id, na.rm = TRUE) +1 
        preview_id <- sprintf("%03d", next_id)  # Format with 3 digits
        
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
        return("Error generating preview")
      })
      
    } else {
      return("Fill required fields (*) to see filename preview")
    }
  })
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
                           choices = c("", "ILLUMINA", "PACBIO", "ONT", "MGI"))
        ),
        column(3,
               textInput("platform_name", "Platform*")
        ),
        column(3,
               selectInput("platform_type", "Platform Type",
                           choices = c("","SRS" = "srs", "LRS" = "lrs"))
        ),
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
                           choices = c("", "DEEPVARIANT", "GATK", "CLAIR3"))
        ),
        column(3,
               selectInput("caller_type", "Caller Type",
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
               selectInput("truth_set_name", "Truth Set",
                           choices = c("GIAB" = "giab", "CMRG" = "cmrg", "T2T" = "t2t"),
                           selected = "giab")
        ),
        column(3,
               selectInput("truth_set_sample", "Sample",
                           choices = c("HG001" = "hg001", "HG002" = "hg002", "HG003" = "hg003", "HG004" = "hg004"),
                           selected = "hg002")
        ),
        column(3,
               textInput("truth_set_version", "Truth Set Version",
                         value = "4.2.1")
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
               textInput("benchmark_tool_version", "Tool Version",
                         value = "0.3.12")
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
        column(8,
               div(
                 h5("Generated Filename:"),
                 verbatimTextOutput("filename_preview", placeholder = TRUE)
               )
        ),
        column(4,
               div(style = "text-align: center; padding-top: 20px;",
                   actionButton("submit_upload", "Add to Database", 
                                class = "btn-success btn-lg", 
                                style = "min-width: 150px; background-color: #42a65c; border: none;")
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