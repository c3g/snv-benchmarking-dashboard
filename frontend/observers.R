# ============================================================================
# observers.R
# ============================================================================
"
Event observers and reactive handlers for SNV Benchmarking Dashboard.

Main components:
- Comparison mode buttons (Sidebar: tech/caller/manual comparison switching)
- Table selections (Tab 1: row clicking for experiment selection)
- Plot interactions (Tab 3: point clicking for experiment details)
- Stratified analysis (Tab 4: region selection and update triggers)
- File upload (Modal: validation, processing, database integration)
- Delete functionality (Modal: experiment selection, confirmation, database integration)

Observer functions:
- Button handlers (mode switching, submission, clearing)
- Table/plot event handlers (selection tracking, click detection)
- Upload processing (file validation, metadata handling)
"

# ============================================================================
# OBSERVER SETUP FUNCTION
# ============================================================================

setup_observers <- function(input, output, session, data_reactives) {
  
  # ====================================================================
  # DYNAMIC DROPDOWN INITIALIZATION
  # ====================================================================
  
  # Populate simple dropdowns with database values on startup
  observe({
    tech_choices <- get_technology_choices()
    caller_choices <- get_caller_choices()
    
    # Update caller/tech select dropdowns in comparison panels
    updateSelectInput(session, "caller_comparison_tech", choices = tech_choices, 
                      selected = if(length(tech_choices) > 0) tech_choices[1] else NULL)
    updateSelectInput(session, "tech_comparison_caller", choices = caller_choices,
                      selected = if(length(caller_choices) > 0) caller_choices[1] else NULL)
    
    # Update filter dropdowns in sidebar
    updateSelectInput(session, "filter_technology", choices = tech_choices,
                      selected = if(length(tech_choices) > 0) tech_choices[1] else NULL)
    updateSelectInput(session, "filter_caller", choices = caller_choices,
                      selected = if(length(caller_choices) > 0) caller_choices[1] else NULL)
  }) |> bindEvent(data_reactives$data_refresh_trigger(), ignoreNULL = FALSE)
  
  # ====================================================================
  # HIERARCHICAL CHECKBOX UI OUTPUTS
  # ====================================================================
  
  # Render hierarchical technology checkboxes
  output$tech_hierarchy_ui <- renderUI({
    data_reactives$data_refresh_trigger()  # refresh when data changes
    create_hierarchical_tech_ui("tech")
  })
  
  # Render hierarchical caller checkboxes
  output$caller_hierarchy_ui <- renderUI({
    data_reactives$data_refresh_trigger()
    create_hierarchical_caller_ui("caller")
  })
  
  # Render submit button for tech comparison (shows when selection exists)
  output$tech_comparison_submit_ui <- renderUI({
    selection <- input$tech_hierarchy_selection
    
    # Check if selection has any entries (it's a named list from JS)
    has_selection <- !is.null(selection) && length(names(selection)) > 0
    
    if (has_selection) {
      actionButton("submit_tech_comparison", 
                   paste0("Compare (", length(names(selection)), " selected)"),
                   class = "btn-primary", style = "width: 100%;")
    } else {
      p("Select at least 1 technology", style = "color: #dc3545; font-size: 11px; margin: 0;")
    }
  })
  
  # Render submit button for caller comparison
  output$caller_comparison_submit_ui <- renderUI({
    selection <- input$caller_hierarchy_selection
    
    # Check if selection has any entries
    has_selection <- !is.null(selection) && length(names(selection)) > 0
    
    if (has_selection) {
      actionButton("submit_caller_comparison", 
                   paste0("Compare (", length(names(selection)), " selected)"),
                   class = "btn-success", style = "width: 100%;")
    } else {
      p("Select at least 1 caller", style = "color: #dc3545; font-size: 11px; margin: 0;")
    }
  })

  # ====================================================================
  # COMPARISON MODE BUTTON OBSERVERS
  # ====================================================================
 
  data_refresh_trigger <- data_reactives$data_refresh_trigger
  
  # Technology comparison button
  observeEvent(input$compare_techs, {
    data_reactives$current_mode("tech_comparison")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    
    # Reset filters 
    updateRadioButtons(session, "filter_type", selected = "none")
    
    # Reset other comparison selections
    updateTabsetPanel(session, "main_tabs", selected = "Experiments")
    
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
    
    # Reset filters
    updateRadioButtons(session, "filter_type", selected = "none")
    
    # Reset submitted comparison states
    updateTabsetPanel(session, "main_tabs", selected = "Experiments")
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    
    showNotification("Caller comparison mode activated!", type = "message")
  })
  
  # Specific experiments comparison button
  observeEvent(input$compare_experiments, {
    data_reactives$current_mode("manual_selection")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    
    # Reset filters 
    #updateRadioButtons(session, "filter_type", selected = "none")
    
    # Reset submitted comparison states
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    
    # Reset other comparison selections
    updateTabsetPanel(session, "main_tabs", selected = "Experiments")
    updateSelectInput(session, "tech_comparison_caller", selected = names(get_caller_choices())[1])
    updateSelectInput(session, "caller_comparison_tech", selected = names(get_technology_choices())[1])
    
    showNotification("Click table rows to select experiments.", type = "message")
  })
  
  # Clear experiment selection button
  observeEvent(input$clear_experiment_selection, {
    data_reactives$table_selected_ids(numeric(0))
    data_reactives$display_experiment_ids(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    showNotification("Experiment selection cleared!", type = "message")
  })
  
  # Logo home/reset button
  observeEvent(input$logo_home_btn, {
    data_reactives$current_mode("filter")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    data_reactives$plot_clicked_id(NULL)
    data_reactives$stratified_triggered(FALSE)
    data_reactives$active_truth_set_filter("All Truth Sets")
    
    # Reset all filter controls
    updateRadioButtons(session, "filter_type", selected = "none")
    updateSelectInput(session, "filter_technology", selected = "ILLUMINA")
    updateSelectInput(session, "filter_caller", selected = "DEEPVARIANT")
    
    # Reset comparison selections
    updateCheckboxGroupInput(session, "selected_technologies", selected = character(0))
    updateCheckboxGroupInput(session, "selected_callers", selected = character(0))
    updateSelectInput(session, "tech_comparison_caller", selected = "DEEPVARIANT")
    updateSelectInput(session, "caller_comparison_tech", selected = "ILLUMINA")
    
    # Reset truth set filters
    updateSelectInput(session, "truth_set_filter_tab2", selected = "All Truth Sets")
    updateSelectInput(session, "truth_set_filter_tab3", selected = "All Truth Sets")
    updateSelectInput(session, "truth_set_filter_tab4", selected = "All Truth Sets")
    
    # Clear table selection
    dataTableProxy('experiments_table') %>% selectRows(NULL)
        
    # Reset stratified region selections
    updateCheckboxGroupInput(session, "basic_regions", selected = "All Regions")
    updateCheckboxGroupInput(session, "functional_regions", selected = character(0))
    updateCheckboxGroupInput(session, "homopolymer_regions", selected = character(0))
    updateCheckboxGroupInput(session, "satellites_regions", selected = character(0))
    updateCheckboxGroupInput(session, "non_repetitive_regions", selected = character(0))
    updateCheckboxGroupInput(session, "complex_regions", selected = character(0))
    updateCheckboxGroupInput(session, "gc_low", selected = character(0))
    updateCheckboxGroupInput(session, "gc_normal", selected = character(0))
    updateCheckboxGroupInput(session, "gc_high", selected = character(0))
    updateCheckboxGroupInput(session, "gc_extreme", selected = character(0))
    
    # Switch to Experiments tab
    updateTabsetPanel(session, "main_tabs", selected = "Experiments")
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
    selection <- input$tech_hierarchy_selection
    caller_filter <- input$tech_comparison_caller
    
    if (is.null(selection) || length(names(selection)) == 0) {
      showNotification("Please select at least one technology", type = "warning")
      return()
    }
    
    # Build experiment IDs based on hierarchical selection
    all_ids <- c()
    
    for (tech in names(selection)) {
      platforms <- selection[[tech]]
      tech_ids <- c()
      
      if (is.null(platforms)) {
        # Parent selected = all platforms for this technology
        tech_ids <- db$get_experiments_filtered(
          technology = tech,
          platform = NULL,
          caller = caller_filter,
          version = NULL
        )
      } else {
        # Specific platforms selected
        for (platform in platforms) {
          platform_ids <- db$get_experiments_filtered(
            technology = tech,
            platform = platform,
            caller = caller_filter,
            version = NULL
          )
          tech_ids <- c(tech_ids, platform_ids)
        }
        tech_ids <- unique(tech_ids)
      }
      
      all_ids <- c(all_ids, tech_ids)
    }
    
    all_ids <- unique(all_ids)
    data_reactives$display_experiment_ids(all_ids)
    data_reactives$comparison_submitted(TRUE)
    data_reactives$comparison_type("technology")
    data_reactives$comparison_results(all_ids)
    
    showNotification(
      paste("Found", length(all_ids), "experiments matching", length(names(selection)), "technology selection(s)"), 
      type = "message"
    )
  })
  
  # Caller comparison submission
  observeEvent(input$submit_caller_comparison, {
    selection <- input$caller_hierarchy_selection
    tech_filter <- input$caller_comparison_tech
    
    if (is.null(selection) || length(names(selection)) == 0) {
      showNotification("Please select at least one caller", type = "warning")
      return()
    }
    
    # Build experiment IDs based on hierarchical selection
    all_ids <- c()
    
    for (caller in names(selection)) {
      versions <- selection[[caller]]
      caller_ids <- c()
      
      if (is.null(versions)) {
        # Parent selected = all versions for this caller
        caller_ids <- db$get_experiments_filtered(
          technology = tech_filter,
          platform = NULL,
          caller = caller,
          version = NULL
        )
      } else {
        # Specific versions selected
        for (version in versions) {
          version_ids <- db$get_experiments_filtered(
            technology = tech_filter,
            platform = NULL,
            caller = caller,
            version = version
          )
          caller_ids <- c(caller_ids, version_ids)
        }
        caller_ids <- unique(caller_ids)
      }
      
      all_ids <- c(all_ids, caller_ids)
    }
    
    all_ids <- unique(all_ids)
    data_reactives$display_experiment_ids(all_ids)
    data_reactives$comparison_submitted(TRUE)
    data_reactives$comparison_type("caller")
    data_reactives$comparison_results(all_ids)
    
    showNotification(
      paste("Found", length(all_ids), "experiments matching", length(names(selection)), "caller selection(s)"), 
      type = "message"
    )
  })
  
  # Specific experiment comparison submission
  observeEvent(input$submit_bottom_comparison, {
    data_reactives$comparison_submitted(FALSE)
    showNotification("Using selected experiments for comparison", type = "message")
  })
  
  # ====================================================================
  # TRUTH SET FILTER SYNCHRONIZATION OBSERVERS
  # ====================================================================
  # Tab 2 filter
  observeEvent(input$truth_set_filter_tab2, {
    new_value <- input$truth_set_filter_tab2
    if (!is.null(new_value) && new_value != data_reactives$active_truth_set_filter()) {
      data_reactives$active_truth_set_filter(new_value)
      
      # Sync other tabs
      updateSelectInput(session, "truth_set_filter_tab3", selected = new_value)
      updateSelectInput(session, "truth_set_filter_tab4", selected = new_value)
    }
  })

  # Tab 3 filter
  observeEvent(input$truth_set_filter_tab3, {
    new_value <- input$truth_set_filter_tab3
    if (!is.null(new_value) && new_value != data_reactives$active_truth_set_filter()) {
      data_reactives$active_truth_set_filter(new_value)
      
      # Sync other tabs
      updateSelectInput(session, "truth_set_filter_tab2", selected = new_value)
      updateSelectInput(session, "truth_set_filter_tab4", selected = new_value)
    }
  })

  # Tab 4 filter
  observeEvent(input$truth_set_filter_tab4, {
    new_value <- input$truth_set_filter_tab4
    if (!is.null(new_value) && new_value != data_reactives$active_truth_set_filter()) {
      data_reactives$active_truth_set_filter(new_value)
      
      # Sync other tabs
      updateSelectInput(session, "truth_set_filter_tab2", selected = new_value)
      updateSelectInput(session, "truth_set_filter_tab3", selected = new_value)
    }
  })

  # ====================================================================
  # EXPERIMENT DETAILS EXPANSION (TAB 1)
  # ====================================================================
  
  observeEvent(input$expand_experiment_details, {
    exp_id <- input$expand_experiment_details$id
    
    # Get metadata
    py_ids <- r_to_py(list(as.numeric(exp_id)))
    metadata <- db$get_experiment_metadata(py_ids)
    
    # Generate HTML using table function
    details_html <- create_experiment_details_html(metadata)
    
    if (!is.null(details_html)) {
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
    updateCheckboxGroupInput(session, "gc_extreme", selected = character(0))
    updateCheckboxGroupInput(session, "complex_regions", selected = character(0))
    updateCheckboxGroupInput(session, "satellites_regions", selected = character(0))
    updateCheckboxGroupInput(session, "non_repetitive_regions", selected = character(0))
    
    
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
    current_exp_ids <- data_reactives$performance_experiment_ids_filtered()
    
    if (length(current_exp_ids) == 0) {
      showNotification("No experiments selected.", 
                       type = "warning", duration = 5)
      return()
    }
    
    # Query database with region filtering
    tryCatch({
      
      showNotification("Loading stratified data...", type = "message", duration = 2)
      
      ids_json <- json_param(current_exp_ids)

      # convert to list
      regions_list <- as.list(selected_regions)
      # Pass regions to database query for SQL filtering
      enhanced_data <- tryCatch({
        db$get_stratified_performance_by_regions(
          ids_json, 
          VARIANT_TYPES,
          regions_list
        )
      }, error = function(e) {
        showNotification(paste("Database error:", e$message), type = "error", duration = 8)
        data.frame()  # Return empty data frame
      })
      
      
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
  
  # ====================================================================
  # UPLOAD FUNCTIONALITY OBSERVERS
  # ====================================================================
  
  # Import the Python upload handler
  upload_handler <- import("upload_handler")
  
  # Helper functions for safe input handling
  safe_input <- function(input_value, default = "") {
    if (is.null(input_value) || is.na(input_value) || input_value == "") {
      return(default)
    }
    return(as.character(input_value))
  }
  
  safe_numeric <- function(input_value) {
    if (is.null(input_value) || is.na(input_value)) {
      return("")
    }
    return(as.character(input_value))
  }
  
  # File validation with real-time feedback
  observe({
    if (!is.null(input$upload_file)) {
      file_info <- input$upload_file
      
      # Basic file validation
      if (!grepl("\\.csv$", file_info$name, ignore.case = TRUE)) {
        output$file_status <- renderUI({
          div(class = "alert alert-danger", style = "padding: 8px; margin: 5px 0;",
              "Please upload a CSV file")
        })
      } else if (file_info$size == 0) {
        output$file_status <- renderUI({
          div(class = "alert alert-danger", style = "padding: 8px; margin: 5px 0;",
              "File appears to be empty")
        })
      } else if (file_info$size > 100 * 1024 * 1024) {  # 100MB limit
        output$file_status <- renderUI({
          div(class = "alert alert-warning", style = "padding: 8px; margin: 5px 0;",
              "Large file detected. Upload may take some time.")
        })
      } else {
        output$file_status <- renderUI({
          div(class = "alert alert-success", style = "padding: 8px; margin: 5px 0;",
              paste("File ready:", file_info$name, 
                    paste0("(", round(file_info$size / 1024, 1), " KB)")))
        })
      }
    } else {
      output$file_status <- renderUI({ NULL })
    }
  })
  
  # Main upload submission observer
  observeEvent(input$submit_upload, {
    
  user_info <- get_user_info(session)
  
  # Check if user is authenticated
  if (!is_authenticated(session)) {
    showNotification(
      "Please sign in to upload datasets", 
      type = "error", 
      duration = 5
    )
    return()
  }
    # Basic validation checks
    if (is.null(input$upload_file)) {
      showNotification("Please select a file to upload", type = "error", duration = 5)
      return()
    }
    
    # Check required fields
    required_fields <- list(
      exp_name = input$exp_name,
      technology = input$technology,
      platform_name = input$platform_name,
      caller_name = input$caller_name
    )
    
    missing_fields <- names(required_fields)[sapply(required_fields, function(x) is.null(x) || x == "")]
    
    if (length(missing_fields) > 0) {
      showNotification(
        paste("Please fill required fields:", paste(missing_fields, collapse = ", ")), 
        type = "error", 
        duration = 6
      )
      return()
    }
    
    # Show loading notification
    loading_id <- showNotification(
      "Processing upload... Please wait", 
      type = "message", 
      duration = NULL,
      closeButton = FALSE
    )
    
    # Prepare complete metadata JSON
    tryCatch({
      
      metadata_json <- jsonlite::toJSON(list(
        # REQUIRED FIELDS
        exp_name = safe_input(input$exp_name),
        technology = safe_input(input$technology),
        platform_name = safe_input(input$platform_name),
        caller_name = safe_input(input$caller_name),
        
        # BASIC INFO
        description = safe_input(input$description),
        target = safe_input(input$target, "wgs"),
        
        # SEQUENCING PLATFORM
        platform_type = safe_input(input$platform_type),
        platform_version = safe_input(input$platform_version),
        
        # CHEMISTRY
        chemistry_name = safe_input(input$chemistry_name),
        chemistry_version = safe_input(input$chemistry_version),
        
        # VARIANT CALLER
        caller_type = safe_input(input$caller_type),
        caller_version = safe_input(input$caller_version),
        caller_model = safe_input(input$caller_model),
        
        # ALIGNER
        aligner_name = safe_input(input$aligner_name),
        aligner_version = safe_input(input$aligner_version),
        
        # TRUTH SET
        truth_set_name = safe_input(input$truth_set_name, "giab"),
        truth_set_sample = safe_input(input$truth_set_sample, "hg002"),
        truth_set_version = safe_input(input$truth_set_version, "4.2.1"),
        truth_set_reference = safe_input(input$truth_set_reference, "grch38"),
        
        # VARIANT INFO
        variant_type = safe_input(input$variant_type, "snp+indel"),
        variant_size = safe_input(input$variant_size, "small"),
        variant_origin = safe_input(input$variant_origin, "germline"),
        is_phased = safe_input(input$is_phased, "false"),
        
        # BENCHMARK TOOL
        benchmark_tool_name = safe_input(input$benchmark_tool_name, "hap.py"),
        benchmark_tool_version = safe_input(input$benchmark_tool_version, "0.3.12"),
        
        # QUALITY METRICS
        mean_coverage = safe_numeric(input$mean_coverage),
        read_length = safe_numeric(input$read_length),
        mean_insert_size = safe_numeric(input$mean_insert_size),
        mean_read_length = safe_numeric(input$mean_read_length)
        
      ), auto_unbox = TRUE)
      
      # Print metadata for debugging
      cat("Metadata JSON:\n", metadata_json, "\n")
      
      # Call Python upload handler
      result <- tryCatch({
        upload_handler$upload_experiment(
          file_path = input$upload_file$datapath,
          metadata_json = metadata_json,
          username = user_info$username,
          is_admin = user_info$is_admin
        )
      }, error = function(e) {
        removeNotification(loading_id)
        showNotification(paste("Upload failed:", e$message), type = "error", duration = 10)
        return(list(success = FALSE, message = paste("System error:", e$message)))
      })
      
      # Remove loading notification
      removeNotification(loading_id)
      
      # Handle result
      if (result$success) {
        showNotification(
          HTML(paste("Upload Successful!<br>", result$message)), 
          type = "message", 
          duration = 8
        )

        # Close modal
        toggleModal(session, "upload_modal", toggle = "close")
        
        #refresh data wituout reloading the page
        data_refresh_trigger(data_refresh_trigger() + 1)
        return(TRUE)
        
      } else {
        #error notifications
        if (!is.null(result$unauthorized) && result$unauthorized) {
          showNotification(
            HTML(paste("Unauthorized<br>", result$error)), 
            type = "error", 
            duration = 10
          )
        } else {
          showNotification(
            HTML(paste("Upload Failed<br>", result$message)), 
            type = "error", 
            duration = 12
          )
        }
        
        return(FALSE)
      }
      
    }, error = function(e) {
      # Remove loading notification
      removeNotification(loading_id)

      # Show error
      showNotification(
        paste("Upload Error:", e$message), 
        type = "error", 
        duration = 10
      )
      
      # Print error details for debugging
      cat("Upload Error Details:\n")
      print(e)
      
      return(FALSE)
    })
  })
  
# ====================================================================
# DELETE FUNCTIONALITY OBSERVERS
# ====================================================================

# Import the Python delete handler
delete_handler <- import("delete_handler")

# Show selected experiments count in modal
output$selected_delete_count <- renderText({
  selected_rows <- input$delete_experiments_table_rows_selected
  if (length(selected_rows) == 0) {
    "No experiments selected"
  } else {
    paste("Selected:", length(selected_rows), "experiments")
  }
})

# Delete confirmation - select experiments and show final confirmation modal
observeEvent(input$confirm_delete_selected, {
  selected_rows <- input$delete_experiments_table_rows_selected

  if (length(selected_rows) == 0) {
    showNotification("Please select experiments to delete", type = "warning", duration = 4)
    return()
  }
  
  all_experiments <- tryCatch({
    db$get_experiments_overview()
  }, error = function(e) {
    showNotification("Unable to load experiments", type = "error", duration = 4)
    return(data.frame())
  })
  
  if (nrow(all_experiments) == 0) return()
  
  selected_ids <- all_experiments$id[selected_rows]
  session$userData$selected_delete_ids <- selected_ids
  
  showModal(modalDialog(
    title = "Final Confirmation",
    div(
      class = "alert alert-danger",
      h5("Deleting the following experiments:"),
      tags$ul(
        lapply(selected_ids, function(id) {
          exp_name <- all_experiments$name[all_experiments$id == id]
          tags$li(paste("ID", id, "-", exp_name))
        })
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("final_confirm_delete", "Delete Dataset(s)", 
                   class = "btn-danger")
    )
  ))
})

# Handle final delete confirmation
observeEvent(input$final_confirm_delete, {
  user_info <- get_user_info(session)
  
  if (!is_authenticated(session)) {
    removeModal()
    showNotification("Please sign in", type = "error", duration = 5)
    return()
  }
  
  if (!is_admin(user_info$group)) {
    removeModal()
    showNotification("Admin privileges required", type = "error", duration = 5)
    return()
  }
  
  selected_ids <- session$userData$selected_delete_ids
  
  if (length(selected_ids) == 0) {
    removeModal()
    return()
  }
  
  removeModal()
  
  loading_id <- showNotification(
    paste("Deleting", length(selected_ids), "experiments and rebuilding database..."),
    type = "message", 
    duration = NULL,
    closeButton = FALSE
  )
  
  success_count <- 0
  total_count <- length(selected_ids)
  sorted_ids <- sort(selected_ids, decreasing = TRUE)
  
  for (exp_id in sorted_ids) {
    result <- delete_handler$delete_experiment(
      exp_id, 
      username = user_info$username,
      is_admin = user_info$is_admin
    )
    
    if (result$success) {
      success_count <- success_count + 1
    } else if (!is.null(result$unauthorized) && result$unauthorized) {
      removeNotification(loading_id)
      showNotification(
        paste("Unauthorized:", result$error),
        type = "error",
        duration = 10
      )
      return()
    }
  }
  
  removeNotification(loading_id)
  
  if (success_count == total_count) {
    showNotification(
      paste("Successfully deleted", success_count, "experiments and rebuilt database."),
      type = "message",
      duration = 6
    )
  } else {
    showNotification(
      paste("Deleted", success_count, "out of", total_count, "experiments. Some failures occurred."),
      type = "warning",
      duration = 8
    )
  }
  
  toggleModal(session, "delete_modal", toggle = "close")
  data_reactives$data_refresh_trigger(data_reactives$data_refresh_trigger() + 1)
  session$userData$selected_delete_ids <- NULL
})

# Handle cancel delete
observeEvent(input$cancel_delete, {
  toggleModal(session, "delete_modal", toggle = "close")
})
}