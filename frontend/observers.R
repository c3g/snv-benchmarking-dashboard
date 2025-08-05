# ============================================================================
# observers.R
# ============================================================================
"
Event observers and reactive handlers for SNV Benchmarking Dashboard.

Main components:
- Button click handlers for comparison modes
- Table row selection observers
- Plot interaction handlers (clicks)
- Comparison submission logic
- Experiment details expansion
- Stratified analysis update handlers
"

# ============================================================================
# OBSERVER SETUP FUNCTION
# ============================================================================

setup_observers <- function(input, output, session, data_reactives) {
  
  # ====================================================================
  # COMPARISON MODE BUTTON OBSERVERS
  # ====================================================================
  
  # Technology comparison button
  observeEvent(input$compare_techs, {
    data_reactives$current_mode("tech_comparison")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_callers", selected = character(0))
    updateSelectInput(session, "caller_comparison_tech", selected = "ILLUMINA")
    
    # Reset submitted comparison states
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    
    showNotification("Technology comparison mode activated!", type = "message")
  })
  
  # Caller comparison button
  observeEvent(input$compare_callers, {
    data_reactives$current_mode("caller_comparison")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    
    # Reset submitted comparison states
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_technologies", selected = character(0))
    updateSelectInput(session, "tech_comparison_caller", selected = "DEEPVARIANT")
    
    showNotification("Caller comparison mode activated!", type = "message")
  })
  
  # Specific experiments comparison button
  observeEvent(input$compare_experiments, {
    data_reactives$current_mode("manual_selection")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    
    # Reset submitted comparison states
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    
    # Reset other comparison selections
    updateCheckboxGroupInput(session, "selected_technologies", selected = character(0))
    updateCheckboxGroupInput(session, "selected_callers", selected = character(0))
    updateSelectInput(session, "tech_comparison_caller", selected = "DEEPVARIANT")
    updateSelectInput(session, "caller_comparison_tech", selected = "ILLUMINA")
    
    showNotification("Click table rows to select experiments.", type = "message")
  })
  
  # Clear experiment selection button
  observeEvent(input$clear_experiment_selection, {
    data_reactives$table_selected_ids(numeric(0))
    data_reactives$display_experiment_ids(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    showNotification("Experiment selection cleared!", type = "message")
  })
  
  # ====================================================================
  # TABLE INTERACTION OBSERVERS
  # ====================================================================
  
  # Row selection in experiments table (only in manual selection mode)
  observeEvent(input$experiments_table_rows_selected, {
    if (data_reactives$current_mode() != "manual_selection") 
      return()
    
    current_data <- data_reactives$experiments_data()
    selected_rows <- input$experiments_table_rows_selected
    
    # Get selected IDs or empty vector if no selection/no data
    new_ids <- if (nrow(current_data) > 0 && length(selected_rows) > 0) {
      current_data$id[selected_rows]
    } else {
      numeric(0)
    }
    
    # Update reactive value
    data_reactives$table_selected_ids(new_ids)
  }, ignoreNULL = FALSE)
  
  # ====================================================================
  # PLOT INTERACTION OBSERVERS
  # ====================================================================
  
  # Handle clicks from SNP plot
  observeEvent(event_data("plotly_click", source = "snp_plot_isolated"), {
    click_data <- event_data("plotly_click", source = "snp_plot_isolated")
    if (!is.null(click_data)) {
      plot_id <- click_data$customdata
      exp_id <- gsub("snp_", "", plot_id)
      data_reactives$plot_clicked_id(as.numeric(exp_id))
      showNotification("Scroll down to view experiment details.",
                       type = "message", duration = 3)
    }
  })
  
  # Handle clicks from INDEL plot
  observeEvent(event_data("plotly_click", source = "indel_plot_isolated"), {
    click_data <- event_data("plotly_click", source = "indel_plot_isolated")
    if (!is.null(click_data)) {
      plot_id <- click_data$customdata
      exp_id <- gsub("indel_", "", plot_id)
      data_reactives$plot_clicked_id(as.numeric(exp_id))
      showNotification("Scroll down to view experiment details.",
                       type = "message", duration = 3)
    }
  })
  
  # ====================================================================
  # COMPARISON SUBMISSION OBSERVERS
  # ====================================================================
  
  # Technology comparison submission
  observeEvent(input$submit_tech_comparison, {
    # Re-import db inside observer to avoid session issues
    db <- import("db_interface")
    
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
    
    data_reactives$display_experiment_ids(all_ids)
    data_reactives$comparison_submitted(TRUE)
    data_reactives$comparison_type("technology")
    data_reactives$comparison_results(all_ids)
    
    showNotification(paste("Comparing", length(input$selected_technologies), "technologies with", length(all_ids), "experiments"), type = "message")
  })
  
  # Caller comparison submission
  observeEvent(input$submit_caller_comparison, {
    # Re-import db inside observer to avoid session issues
    db <- import("db_interface")
    
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
    
    data_reactives$display_experiment_ids(all_ids)
    data_reactives$comparison_submitted(TRUE)
    data_reactives$comparison_type("caller")
    data_reactives$comparison_results(all_ids)
    
    showNotification(paste("Comparing", length(input$selected_callers), "callers with", length(all_ids), "experiments"), type = "message")
  })
  
  # Specific experiment comparison submission
  observeEvent(input$submit_bottom_comparison, {
    data_reactives$comparison_submitted(FALSE)
    showNotification("Using selected experiments for comparison", type = "message")
  })
  
  # ====================================================================
  # EXPERIMENT DETAILS EXPANSION (TAB 1)
  # ====================================================================
  
  observeEvent(input$expand_experiment_details, {
    # Re-import db inside observer to avoid session issues
    db <- import("db_interface")
    
    exp_id <- input$expand_experiment_details$id
    
    # Get detailed metadata for this experiment
    py_ids <- r_to_py(list(as.numeric(exp_id)))
    metadata <- db$get_experiment_metadata(py_ids)
    
    if (nrow(metadata) > 0) {
      meta <- metadata[1, ]
      
      # Create compact HTML content for row expansion
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
  
  # ====================================================================
  # STRATIFIED ANALYSIS OBSERVERS (TAB 4)
  # ====================================================================
  
  # Clear all region selections button
  observeEvent(input$clear_all_regions, {
    updateCheckboxGroupInput(session, "core_regions", selected = character(0))
    updateCheckboxGroupInput(session, "functional_regions", selected = character(0))
    updateCheckboxGroupInput(session, "homopolymer_regions", selected = character(0))
    updateCheckboxGroupInput(session, "gc_low", selected = character(0))
    updateCheckboxGroupInput(session, "gc_normal", selected = character(0))
    updateCheckboxGroupInput(session, "gc_high", selected = character(0))
    updateCheckboxGroupInput(session, "complex_regions", selected = character(0))
    
    showNotification("All selections cleared!", type = "message", duration = 2)
  })
  
  # Update stratified analysis button
  observeEvent(input$update_stratified, {
    
    selected_regions <- data_reactives$get_selected_regions()
    
    # Validation
    if (length(selected_regions) == 0) {
      showNotification("Please select at least one region!", type = "message", duration = 4)
      return()
    }
    
    # Current experiment IDs (from previous tabs)
    current_exp_ids <- data_reactives$performance_experiment_ids()
    
    if (length(current_exp_ids) == 0) {
      showNotification("No experiments selected.", 
                       type = "warning", duration = 5)
      return()
    }
    
    # Query database with region filtering
    tryCatch({
      # Re-import db inside observer to avoid session issues
      db <- import("db_interface")
      
      showNotification("Loading stratified data...", type = "message", duration = 2)
      
      ids_json <- json_param(current_exp_ids)
      
      # Pass regions to database query for SQL filtering
      enhanced_data <- db$get_stratified_performance_by_regions(
        ids_json, 
        c('SNP', 'INDEL'),
        selected_regions
      )
      
      # Get metadata if we have results
      if (nrow(enhanced_data) > 0) {
        
        # Store results
        data_reactives$stratified_raw_data(enhanced_data)
        data_reactives$stratified_triggered(TRUE)
        
        # Count notification 
        n_experiments <- length(unique(enhanced_data$experiment_id))
        n_results <- nrow(enhanced_data)
        n_regions <- length(unique(enhanced_data$subset))
        
        showNotification(
          paste("Loaded", n_results, "results for", n_experiments, "experiments across", n_regions, "regions"), 
          type = "message", 
          duration = 4
        )
        
      } else {
        data_reactives$stratified_raw_data(data.frame())
        data_reactives$stratified_triggered(FALSE)
        showNotification("No data found for selected regions and experiments.", type = "warning", duration = 4)
      }
      
    }, error = function(e) {
      data_reactives$stratified_raw_data(data.frame())
      data_reactives$stratified_triggered(FALSE)
      showNotification(paste("Error loading stratified data:", e$message), type = "error", duration = 6)
    })
  })
}