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
  
  # ====================================================================
  # DYNAMIC DROPDOWN INITIALIZATION
  # ====================================================================
  
  # Populate simple dropdowns with database values on startup
  observe({
    tech_choices <- get_technology_choices()
    caller_choices <- get_caller_choices()
    
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
    data_reactives$data_refresh_trigger()
    create_hierarchical_tech_ui("tech")
  })
  
  # Render hierarchical caller checkboxes
  output$caller_hierarchy_ui <- renderUI({
    data_reactives$data_refresh_trigger()
    create_hierarchical_caller_ui("caller")
  })
  
  # Render submit button for advanced comparison
  output$advanced_comparison_submit_ui <- renderUI({
    tech_selection <- input$tech_hierarchy_selection
    caller_selection <- input$caller_hierarchy_selection
    
    # Check if either tech or caller has selections
    has_tech_selection <- !is.null(tech_selection) && length(names(tech_selection)) > 0
    has_caller_selection <- !is.null(caller_selection) && length(names(caller_selection)) > 0
    
    if (has_tech_selection || has_caller_selection) {
      tech_count <- if(has_tech_selection) length(names(tech_selection)) else 0
      caller_count <- if(has_caller_selection) length(names(caller_selection)) else 0
      
      label_text <- if (has_tech_selection && has_caller_selection) {
        paste0("Compare (", tech_count, " tech x ", caller_count, " caller)")
      } else if (has_tech_selection) {
        paste0("Compare (", tech_count, " tech selected)")
      } else {
        paste0("Compare (", caller_count, " caller selected)")
      }
      
      actionButton("submit_advanced_comparison", 
                   label_text,
                   class = "btn-primary", 
                   style = "width: 100%; font-weight: 500;")
    } else {
      p("Select at least 1 technology or caller", 
        style = "color: #dc3545; font-size: 11px; margin: 0; text-align: center;")
    }
  })

  # ====================================================================
  # COMPARISON MODE BUTTON OBSERVERS
  # ====================================================================
 
  data_refresh_trigger <- data_reactives$data_refresh_trigger
  
  # Advanced comparison button
  observeEvent(input$compare_advanced, {
    data_reactives$current_mode("advanced_comparison")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    
    # Reset filters 
    updateRadioButtons(session, "filter_type", selected = "none")
    
    # Reset submitted comparison states
    updateTabsetPanel(session, "main_tabs", selected = "Experiments")
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    dataTableProxy('experiments_table') %>% selectRows(NULL)
    
    showNotification("Advanced comparison mode activated! Select technologies and/or callers.", type = "message")
  })
  
  # Manual selection button
  observeEvent(input$compare_experiments, {
    data_reactives$current_mode("manual_selection")
    data_reactives$display_experiment_ids(numeric(0))
    data_reactives$table_selected_ids(numeric(0))
    
    # Reset submitted comparison states
    data_reactives$comparison_submitted(FALSE)
    data_reactives$comparison_type(NULL)
    data_reactives$comparison_results(numeric(0))
    
    updateTabsetPanel(session, "main_tabs", selected = "Experiments")
    
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
    data_reactives$active_truth_set_filter("ALL")
    
    # Reset all filter controls
    updateRadioButtons(session, "filter_type", selected = "none")
    updateSelectInput(session, "filter_technology", selected = "ILLUMINA")
    updateSelectInput(session, "filter_caller", selected = "DEEPVARIANT")
    
    # Reset hierarchical checkboxes via JavaScript
    session$sendCustomMessage("resetHierarchyCheckboxes", list())

    # Reset truth set filters
    updateSelectInput(session, "truth_set_filter_tab2", selected = "ALL")
    updateSelectInput(session, "truth_set_filter_tab3", selected = "ALL")
    updateSelectInput(session, "truth_set_filter_tab4", selected = "ALL")
    
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
  observeEvent(input$experiments_table_rows_selected, {
    if (data_reactives$current_mode() != "manual_selection") return()
    
    current_data <- data_reactives$experiments_data()
    if (nrow(current_data) == 0) return()
    
    visible_ids <- current_data$id
    selected_rows <- input$experiments_table_rows_selected
    prev_selected <- isolate(data_reactives$table_selected_ids())
    
    # IDs selected in current view
    selected_in_view <- if (length(selected_rows) > 0) current_data$id[selected_rows] else numeric(0)
    
    # Keep hidden selections + current view selections
    hidden <- prev_selected[!prev_selected %in% visible_ids]
    final <- unique(c(hidden, selected_in_view))
    
    data_reactives$table_selected_ids(final)
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
  
  # Unified advanced comparison submission
  observeEvent(input$submit_advanced_comparison, {
    # Get user context
    user_info <- get_user_info(session)
    user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
    is_admin_user <- if (!is.null(user_info)) user_info$is_admin else FALSE
  

    tech_selection <- input$tech_hierarchy_selection
    caller_selection <- input$caller_hierarchy_selection
    
    # Validate that at least one selection exists
    has_tech <- !is.null(tech_selection) && length(names(tech_selection)) > 0
    has_caller <- !is.null(caller_selection) && length(names(caller_selection)) > 0
    
    if (!has_tech && !has_caller) {
      showNotification("Please select at least one technology or caller", type = "warning")
      return()
    }
    
    # Build experiment IDs for all combinations
    all_ids <- c()
    
    # If no tech selected, use all techs; if no caller selected, use all callers
    techs_to_query <- if (has_tech) names(tech_selection) else db$get_distinct_technologies()
    callers_to_query <- if (has_caller) names(caller_selection) else db$get_distinct_callers()
    
    # Iterate through all tech x caller combinations
    for (tech in techs_to_query) {
      for (caller in callers_to_query) {
        combo_ids <- c()
        
        # Get platforms for this tech (if specified)
        platforms <- if (has_tech) tech_selection[[tech]] else NULL
        
        # Get versions for this caller (if specified)
        versions <- if (has_caller) caller_selection[[caller]] else NULL
        
        # Query based on whether we have specific platforms/versions
        if (is.null(platforms)) {
          # All platforms for this tech
          if (is.null(versions)) {
            # All versions for this caller
            combo_ids <- db$get_experiments_filtered(
              technology = tech,
              platform = NULL,
              caller = caller,
              version = NULL
            )
          } else {
            # Specific versions
            for (version in versions) {
              version_ids <- db$get_experiments_filtered(
                technology = tech,
                platform = NULL,
                caller = caller,
                version = version,
                user_id = user_id, 
                is_admin = is_admin_user 
              )
              combo_ids <- c(combo_ids, version_ids)
            }
          }
        } else {
          # Specific platforms
          for (platform in platforms) {
            if (is.null(versions)) {
              # All versions for this caller
              platform_ids <- db$get_experiments_filtered(
                technology = tech,
                platform = platform,
                caller = caller,
                version = NULL
              )
              combo_ids <- c(combo_ids, platform_ids)
            } else {
              # Specific versions
              for (version in versions) {
                version_combo_ids <- db$get_experiments_filtered(
                  technology = tech,
                  platform = platform,
                  caller = caller,
                  version = version
                )
                combo_ids <- c(combo_ids, version_combo_ids)
              }
            }
          }
        }
        
        all_ids <- c(all_ids, combo_ids)
      }
    }
    
    # Remove duplicates and update state
    all_ids <- unique(all_ids)
    data_reactives$display_experiment_ids(all_ids)
    data_reactives$comparison_submitted(TRUE)
    data_reactives$comparison_type("advanced")
    data_reactives$comparison_results(all_ids)
    
    # Build descriptive notification
    tech_desc <- if (has_tech) {
      paste(length(techs_to_query), "technology selection(s)")
    } else {
      "all technologies"
    }
    
    caller_desc <- if (has_caller) {
      paste(length(callers_to_query), "caller selection(s)")
    } else {
      "all callers"
    }
    
    showNotification(
      paste0("Found ", length(all_ids), " experiments matching ", tech_desc, " x ", caller_desc), 
      type = "message",
      duration = 5
    )
  })
  
  # Manual experiment comparison submission
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
    
    # Get user context for visibility filtering
    user_info <- get_user_info(session)
    user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
    is_admin_user <- if (!is.null(user_info)) user_info$is_admin else FALSE
    
    # Get metadata with user context
    py_ids <- r_to_py(list(as.numeric(exp_id)))
    metadata <- db$get_experiment_metadata(py_ids, user_id, is_admin_user)
    
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
          VARIANT_TYPE_OPTIONS,
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
      
      # Determine visibility - non-admins can only upload private
      upload_visibility <- if (isTRUE(user_info$is_admin)) {
        safe_input(input$experiment_visibility, "private")
      } else {
        "private"  # Force private for non-admins
      }
      
      # Log the upload attempt
      cat("Upload attempt by:", user_info$username, 
          "| Admin:", user_info$is_admin, 
          "| User ID:", user_info$user_id,
          "| Visibility:", upload_visibility, "\n")
      
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
        mean_read_length = safe_numeric(input$mean_read_length),

        # OWNERSHIP AND VISIBILITY
        experiment_visibility = upload_visibility,
        owner_username = user_info$username,
        owner_id = user_info$user_id  # Database user ID from session
        
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

# Force table refresh when delete modal opens (prevents browser caching)
observeEvent(input$show_delete_modal, {
  data_reactives$delete_modal_trigger(data_reactives$delete_modal_trigger() + 1)
})
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
  
  # Use stored table data to ensure row indices match what user sees
  all_experiments <- session$userData$delete_table_data
  
  if (is.null(all_experiments) || nrow(all_experiments) == 0) {
    showNotification("Table data not available. Please reopen the delete modal.", type = "error", duration = 4)
    return()
  }
  
  selected_ids <- as.integer(all_experiments$id[selected_rows])
  session$userData$selected_delete_ids <- selected_ids
  
  # Log for debugging
  message(paste("Delete modal: selected rows =", paste(selected_rows, collapse=","), 
                "-> IDs =", paste(selected_ids, collapse=",")))
  
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
  
  if (!user_info$is_admin) {
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
    # Ensure experiment_id is passed as integer
    result <- delete_handler$delete_experiment(
      experiment_id = as.integer(exp_id), 
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
      paste("Successfully deleted", success_count, "experiments."),
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
  # Clean up stored data
  session$userData$selected_delete_ids <- NULL
  session$userData$delete_table_data <- NULL
})

# Handle cancel delete
observeEvent(input$cancel_delete, {
  toggleModal(session, "delete_modal", toggle = "close")
  # Clean up stored data
  session$userData$delete_table_data <- NULL
  session$userData$selected_delete_ids <- NULL
})
# ============================================================================
# FILE BROWSER OBSERVERS
# ============================================================================

# Track selected file path
fb_selected_path <- reactiveVal(NULL)

# Track current directory listing
fb_directory_data <- reactiveVal(NULL)

# Load directory when modal opens
observeEvent(input$show_file_browser, {
  fb_selected_path(NULL)  # Reset selection
  
  user <- get_user_info(session)
  result <- file_manager$list_directory(
    path = NULL,
    username = user$username,
    is_admin = user$is_admin
  )
  
  if (result$success) {
    fb_directory_data(result)
  } else {
    showNotification(paste("Error:", result$error), type = "error")
    fb_directory_data(NULL)
  }
})

# Render current path
output$fb_current_path <- renderText({
  data <- fb_directory_data()
  if (is.null(data)) return("Loading...")
  data$current_path
})

# Render file table
output$fb_files_table_ui <- renderUI({
  data <- fb_directory_data()
  if (is.null(data) || !data$success) {
    return(div(
      style = "padding: 20px; text-align: center; color: #6c757d;",
      "No files to display"
    ))
  }
  
  files <- data$files
  if (length(files) == 0) {
    return(div(
      style = "padding: 20px; text-align: center; color: #6c757d;",
      "Directory is empty"
    ))
  }
  
  rows <- lapply(seq_along(files), function(i) {
    f <- files[[i]]
    icon_name <- if (f$is_dir) "folder" else "file"
    icon_color <- if (f$is_dir) "#f6b93b" else "#95a5a6"
    size_str <- if (is.null(f$size)) "--" else paste0(round(f$size/1024, 1), " KB")
    
    tags$tr(
      id = paste0("fb_row_", i),
      onclick = sprintf("
        document.querySelectorAll('#fb_table tr').forEach(r => r.style.backgroundColor = '');
        this.style.backgroundColor = '#e3f2fd';
        Shiny.setInputValue('fb_select_file', '%s', {priority: 'event'});
      ", f$path),
      style = "cursor: pointer;",
      tags$td(icon(icon_name, style = paste0("color:", icon_color)), " ", f$name),
      tags$td(size_str),
      tags$td(f$modified)
    )
  })
  
  tags$table(
    id = "fb_table",
    class = "table table-striped table-hover",
    style = "margin-bottom: 0;",
    tags$thead(
      tags$tr(
        tags$th("Name"),
        tags$th("Size"),
        tags$th("Modified")
      )
    ),
    tags$tbody(rows)
  )
})

# Track selection
observeEvent(input$fb_select_file, {
  fb_selected_path(input$fb_select_file)
})

# Display selected file
output$fb_selected_display <- renderText({
  path <- fb_selected_path()
  if (is.null(path)) "None" else basename(path)
})

# Helper to refresh file list
refresh_file_browser <- function() {
  user <- get_user_info(session)
  result <- file_manager$list_directory(
    path = NULL,
    username = user$username,
    is_admin = user$is_admin
  )
  if (result$success) {
    fb_directory_data(result)
  }
}

# Rename button
observeEvent(input$fb_rename_btn, {
  path <- fb_selected_path()
  if (is.null(path)) {
    showNotification("Select a file first", type = "warning")
    return()
  }
  
  showModal(modalDialog(
    title = "Rename File",
    textInput("fb_new_name", "New name:", value = basename(path)),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("fb_rename_confirm", "Rename", class = "btn-primary")
    )
  ))
})

# Confirm rename
observeEvent(input$fb_rename_confirm, {
  path <- fb_selected_path()
  new_name <- trimws(input$fb_new_name)
  
  if (is.null(path) || new_name == "") {
    showNotification("Enter a valid name", type = "warning")
    return()
  }
  
  user <- get_user_info(session)
  result <- file_manager$rename_file(
    path = path,
    new_name = new_name,
    username = user$username,
    is_admin = user$is_admin
  )
  
  removeModal()
  
  if (result$success) {
    showNotification(result$message, type = "message")
    fb_selected_path(NULL)
    refresh_file_browser()
  } else {
    showNotification(result$error, type = "error")
  }
})

# Delete button
observeEvent(input$fb_delete_btn, {
  path <- fb_selected_path()
  if (is.null(path)) {
    showNotification("Select a file first", type = "warning")
    return()
  }
  
  showModal(modalDialog(
    title = tagList(icon("trash-alt"), " Confirm Delete"),
    p(paste("Delete:", basename(path), "?")),
    p("This cannot be undone.", style = "color: #dc3545;"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("fb_delete_confirm", "Delete", class = "btn-danger")
    )
  ))
})

# Confirm delete
observeEvent(input$fb_delete_confirm, {
  path <- fb_selected_path()
  user <- get_user_info(session)
  
  result <- file_manager$delete_file(
    path = path,
    username = user$username,
    is_admin = user$is_admin
  )
  
  removeModal()
  
  if (result$success) {
    showNotification(result$message, type = "message")
    fb_selected_path(NULL)
    refresh_file_browser()
  } else {
    showNotification(result$error, type = "error")
  }
})

# Upload file
observeEvent(input$fb_upload_file, {
  file_info <- input$fb_upload_file
  if (is.null(file_info)) return()
  
  user <- get_user_info(session)
  result <- file_manager$upload_file(
    temp_path = file_info$datapath,
    filename = file_info$name,
    dest_folder = NULL,
    username = user$username,
    is_admin = user$is_admin
  )
  
  if (result$success) {
    showNotification(result$message, type = "message")
    refresh_file_browser()
  } else {
    showNotification(result$error, type = "error")
  }
})

# Download single file
output$fb_download_btn <- downloadHandler(
  filename = function() {
    path <- fb_selected_path()
    if (is.null(path)) return("no_file_selected.txt")
    basename(path)
  },
  content = function(file) {
    path <- fb_selected_path()
    if (!is.null(path) && file.exists(path)) {
      file.copy(path, file)
    } else {
      showNotification("File not found", type = "error")
    }
  }
)

# Download all files as zip
output$fb_download_all_btn <- downloadHandler(
  filename = function() {
    paste0("data_files_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
  },
  content = function(file) {
    user <- get_user_info(session)
    
    result <- file_manager$create_zip_download(
      username = user$username,
      is_admin = user$is_admin,
      max_size_mb = 500L
    )
    
    if (result$success) {
      file.copy(result$zip_path, file)
      unlink(result$zip_path)  # Cleanup temp zip
    } else {
      showNotification(result$error, type = "warning", duration = 8)
    }
  }
)

# ====================================================================
# ADMIN PANEL OBSERVERS
# ====================================================================

# Load admin stats when admin tab is shown
observeEvent(input$main_tabs, {
  if (input$main_tabs != "admin_tab") return()
  
  user_info <- get_user_info(session)
  if (is.null(user_info) || !user_info$is_admin) return()
  
  stats <- tryCatch({
    db$get_admin_stats()
  }, error = function(e) {
    list(
      total_experiments = 0, public_experiments = 0, private_experiments = 0,
      total_users = 0, admin_users = 0, recent_uploads = 0, success = FALSE
    )
  })
  
  data_reactives$admin_stats(stats)
}, ignoreInit = FALSE)

# Also refresh stats when data changes
observeEvent(data_reactives$data_refresh_trigger(), {
  user_info <- get_user_info(session)
  if (is.null(user_info) || !user_info$is_admin) return()
  
  stats <- tryCatch({
    db$get_admin_stats()
  }, error = function(e) {
    list(
      total_experiments = 0, public_experiments = 0, private_experiments = 0,
      total_users = 0, admin_users = 0, recent_uploads = 0, success = FALSE
    )
  })
  
  data_reactives$admin_stats(stats)
}, ignoreInit = TRUE)

# Statistics card outputs
output$admin_total_experiments <- renderText({
  stats <- data_reactives$admin_stats()
  if (is.null(stats)) return("--")
  as.character(stats$total_experiments)
})

output$admin_public_experiments <- renderText({
  stats <- data_reactives$admin_stats()
  if (is.null(stats)) return("--")
  as.character(stats$public_experiments)
})

output$admin_private_experiments <- renderText({
  stats <- data_reactives$admin_stats()
  if (is.null(stats)) return("--")
  as.character(stats$private_experiments)
})

output$admin_total_users <- renderText({
  stats <- data_reactives$admin_stats()
  if (is.null(stats)) return("--")
  as.character(stats$total_users)
})

output$admin_admin_users <- renderText({
  stats <- data_reactives$admin_stats()
  if (is.null(stats)) return("--")
  as.character(stats$admin_users)
})

output$admin_total_storage <- renderText({
  stats <- data_reactives$admin_stats()
  if (is.null(stats)) return("--")
  paste0(stats$total_storage_mb)
})


# Private experiments table - using renderUI with simple HTML table
output$admin_private_experiments_ui <- renderUI({
  # Trigger on tab switch
  input$main_tabs
  data_reactives$data_refresh_trigger()
  
  user_info <- get_user_info(session)
  if (is.null(user_info) || !user_info$is_admin) {
    return(p("Access denied", style = "padding: 20px; color: #999;"))
  }
  
  private_exps <- tryCatch({
    db$get_all_private_experiments()
  }, error = function(e) {
    return(p(paste("Error:", e$message), style = "padding: 20px; color: red;"))
  })
  
  if (is.null(private_exps) || nrow(private_exps) == 0) {
    return(div(
      style = "padding: 20px; text-align: center; color: #6c757d;",
      icon("check-circle", style = "font-size: 24px; margin-bottom: 10px; display: block;"),
      "No private experiments"
    ))
  }
  
  # Store data for action buttons
  session$userData$private_exps_data <- private_exps
  
  # Build table rows
  rows <- lapply(1:nrow(private_exps), function(i) {
    exp <- private_exps[i, ]
    tags$tr(
      id = paste0("exp_row_", exp$id),
      class = "admin-exp-row",
      `data-id` = exp$id,
      tags$td(exp$id, style = "padding: 8px 10px; border-bottom: 1px solid #eee;"),
      tags$td(exp$name, style = "padding: 8px 10px; border-bottom: 1px solid #eee;"),
      tags$td(exp$owner_username, style = "padding: 8px 10px; border-bottom: 1px solid #eee;"),
      tags$td(exp$technology, style = "padding: 8px 10px; border-bottom: 1px solid #eee;"),
      tags$td(exp$caller, style = "padding: 8px 10px; border-bottom: 1px solid #eee;"),
      tags$td(exp$created_at, style = "padding: 8px 10px; border-bottom: 1px solid #eee;")
    )
  })
  
  # Build complete table with CSS and JS
  tagList(
    tags$style(HTML("
      .admin-exp-row { cursor: pointer; transition: background-color 0.15s; }
      .admin-exp-row:hover { background-color: #f5f5f5; }
      .admin-exp-row.selected { background-color: #e3f2fd; }
    ")),
    tags$table(
      id = "admin_private_table",
      style = "width: 100%; border-collapse: collapse; font-size: 13px;",
      tags$thead(
        tags$tr(
          style = "background: #f8f9fa; text-align: left;",
          tags$th("ID", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
          tags$th("Name", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
          tags$th("Owner", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
          tags$th("Technology", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
          tags$th("Caller", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
          tags$th("Created", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;")
        )
      ),
      tags$tbody(rows)
    ),
    tags$script(HTML("
      $(document).on('click', '.admin-exp-row', function() {
        $('.admin-exp-row').removeClass('selected');
        $(this).addClass('selected');
        var expId = $(this).data('id');
        Shiny.setInputValue('admin_selected_exp', expId, {priority: 'event'});
      });
    "))
  )
})

# Users table - using renderUI with simple HTML table
output$admin_users_ui <- renderUI({
  input$main_tabs
  data_reactives$data_refresh_trigger()
  
  user_info <- get_user_info(session)
  if (is.null(user_info) || !user_info$is_admin) {
    return(p("Access denied", style = "padding: 20px; color: #999;"))
  }
  
  users <- tryCatch({
    db$get_all_users_with_stats()
  }, error = function(e) {
    return(p(paste("Error:", e$message), style = "padding: 20px; color: red;"))
  })
  
  if (is.null(users) || nrow(users) == 0) {
    return(div(
      style = "padding: 20px; text-align: center; color: #6c757d;",
      "No users found"
    ))
  }
  
  # Build table rows
  rows <- lapply(1:nrow(users), function(i) {
    user <- users[i, ]
    admin_badge <- if (user$is_admin) {
      span("Admin", style = "background: #e3f2fd; color: #1976d2; padding: 2px 6px; border-radius: 3px; font-size: 11px;")
    } else {
      span("User", style = "background: #f5f5f5; color: #666; padding: 2px 6px; border-radius: 3px; font-size: 11px;")
    }
    
    tags$tr(
      tags$td(user$id, style = "padding: 8px; border-bottom: 1px solid #eee; text-align: center;"),
      tags$td(user$username, style = "padding: 8px; border-bottom: 1px solid #eee;"),
      tags$td(user$email %||% "N/A", style = "padding: 8px; border-bottom: 1px solid #eee;"),
      tags$td(user$upload_count, style = "padding: 8px; border-bottom: 1px solid #eee; text-align: center;"),
      tags$td(user$last_login %||% "Never", style = "padding: 8px; border-bottom: 1px solid #eee;"),
      tags$td(admin_badge, style = "padding: 8px; border-bottom: 1px solid #eee;")
    )
  })
  
  # Build complete table
  tags$table(
    style = "width: 100%; border-collapse: collapse; font-size: 13px;",
    tags$thead(
      tags$tr(
        style = "background: #f8f9fa; text-align: left;",
        tags$th("ID", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600; text-align: center; width: 50px;"),
        tags$th("Username", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
        tags$th("Email", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
        tags$th("Uploads", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600; text-align: center; width: 80px;"),
        tags$th("Last Login", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600;"),
        tags$th("Role", style = "padding: 10px; border-bottom: 2px solid #dee2e6; font-weight: 600; width: 70px;")
      )
    ),
    tags$tbody(rows)
  )
})

# Quick action: Upload button
observeEvent(input$admin_upload_btn, {
  toggleModal(session, "admin_modal", toggle = "close")  # Close admin first
  toggleModal(session, "upload_modal", toggle = "open")
})

# Quick action: Delete button
observeEvent(input$admin_delete_btn, {
  toggleModal(session, "admin_modal", toggle = "close")  # Close admin first
  toggleModal(session, "delete_modal", toggle = "open")
})

# Quick action: File browser button
observeEvent(input$admin_file_browser_btn, {
  # Initialize file browser data
  user <- get_user_info(session)
  result <- file_manager$list_directory(
    path = NULL,
    username = user$username,
    is_admin = user$is_admin
  )
  if (result$success) {
    fb_directory_data(result)
  }
  toggleModal(session, "admin_modal", toggle = "close")  # Close admin first
  toggleModal(session, "file_browser_modal", toggle = "open")
})

# Make experiment public
observeEvent(input$admin_make_public_btn, {
  exp_id <- input$admin_selected_exp
  
  if (is.null(exp_id)) {
    showNotification("Please click on an experiment row first", type = "warning")
    return()
  }
  
  # Get experiment info from stored data
  private_exps <- session$userData$private_exps_data
  if (is.null(private_exps) || nrow(private_exps) == 0) {
    private_exps <- tryCatch({
      db$get_all_private_experiments()
    }, error = function(e) {
      data.frame()
    })
  }
  
  if (nrow(private_exps) == 0) return()
  
  exp_row <- private_exps[private_exps$id == exp_id, ]
  if (nrow(exp_row) == 0) {
    showNotification("Experiment not found", type = "error")
    return()
  }
  
  exp_name <- exp_row$name[1]
  
  showModal(modalDialog(
    title = "Confirm Make Public",
    p(paste0("Make experiment '", exp_name, "' (ID: ", exp_id, ") public?")),
    p("This will make it visible to all users.", style = "color: #6c757d;"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_make_public", "Make Public", class = "btn-success")
    )
  ))
  
  session$userData$pending_public_exp_id <- exp_id
})

# Confirm make public
observeEvent(input$confirm_make_public, {
  exp_id <- session$userData$pending_public_exp_id
  if (is.null(exp_id)) return()
  
  result <- tryCatch({
    db$toggle_experiment_visibility(as.integer(exp_id), make_public = TRUE)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  removeModal()
  
  if (result$success) {
    showNotification(result$message, type = "message", duration = 5)
    # Refresh data
    data_reactives$data_refresh_trigger(data_reactives$data_refresh_trigger() + 1)
  } else {
    showNotification(paste("Failed:", result$error), type = "error", duration = 8)
  }
  
  session$userData$pending_public_exp_id <- NULL
})

# view experiment details
observeEvent(input$admin_view_experiment_btn, {
  toggleModal(session, "admin_modal", toggle = "close")  # Close admin first
  exp_id <- input$admin_selected_exp
  
  if (is.null(exp_id)) {
    showNotification("Please select an experiment first", type = "warning")
    return()
  }
  
  user_info <- get_user_info(session)
  metadata <- tryCatch({
    db$get_experiment_metadata(
      experiment_ids_param = as.integer(exp_id),
      user_id = user_info$user_id,
      is_admin = user_info$is_admin
    )
  }, error = function(e) NULL)
  
  if (is.null(metadata) || nrow(metadata) == 0) {
    showNotification("Experiment not found", type = "error")
    return()
  }
  
  meta <- metadata[1, ]
  
  # Store exp_id for download handler

session$userData$admin_view_exp_id <- as.integer(exp_id)
  
  showModal(modalDialog(
    title = tagList(sprintf(" Experiment %s Details", exp_id)),
    size = "l",
    easyClose = TRUE,
    
    # Header with visibility badge
    div(style = "margin-bottom: 15px; padding-bottom: 10px; border-bottom: 1px solid #eee;",
      h4(meta$name, style = "margin: 0 0 5px 0;"),
      span(
        if (isTRUE(meta$is_public)) "Public" else "Private",
        style = paste0(
          "padding: 3px 8px; border-radius: 3px; font-size: 12px; ",
          if (isTRUE(meta$is_public)) "background: #d4edda; color: #155724;" else "background: #fff3cd; color: #856404;"
        )
      ),
      if (!is.null(meta$owner_username)) span(paste(" | Owner:", meta$owner_username), style = "color: #666; font-size: 13px;")
    ),
    fluidRow(
      # Left column - Basic Info
      column(6,
        h5("Basic Information", style = "border-bottom: 1px solid #eee; padding-bottom: 8px; margin-bottom: 12px;"),
        tags$table(style = "width: 100%; font-size: 13px;",
          tags$tr(tags$td(strong("ID:"), style = "padding: 4px 8px; width: 40%;"), tags$td(meta$id)),
          tags$tr(tags$td(strong("Name:"), style = "padding: 4px 8px;"), tags$td(meta$name)),
          tags$tr(tags$td(strong("Created:"), style = "padding: 4px 8px;"), tags$td(meta$created_at)),
          tags$tr(tags$td(strong("Visibility:"), style = "padding: 4px 8px;"), 
                  tags$td(if(isTRUE(meta$is_public)) span("Public", class = "label label-success") else span("Private", class = "label label-warning"))),
          tags$tr(tags$td(strong("Owner:"), style = "padding: 4px 8px;"), tags$td(meta$owner_username %||% "N/A"))
        ),
        
        h5("Sequencing Technology", style = "border-bottom: 1px solid #eee; padding-bottom: 8px; margin: 16px 0 12px 0;"),
        tags$table(style = "width: 100%; font-size: 13px;",
          tags$tr(tags$td(strong("Technology:"), style = "padding: 4px 8px; width: 40%;"), tags$td(meta$technology %||% "N/A")),
          tags$tr(tags$td(strong("Platform:"), style = "padding: 4px 8px;"), tags$td(meta$platform_name %||% "N/A")),
          tags$tr(tags$td(strong("Platform Type:"), style = "padding: 4px 8px;"), tags$td(meta$platform_type %||% "N/A")),
          tags$tr(tags$td(strong("Chemistry:"), style = "padding: 4px 8px;"), tags$td(meta$chemistry_name %||% "N/A"))
        )
      ),
      
      # Right column - Analysis Info
      column(6,
        h5("Variant Calling", style = "border-bottom: 1px solid #eee; padding-bottom: 8px; margin-bottom: 12px;"),
        tags$table(style = "width: 100%; font-size: 13px;",
          tags$tr(tags$td(strong("Caller:"), style = "padding: 4px 8px; width: 40%;"), tags$td(meta$caller %||% "N/A")),
          tags$tr(tags$td(strong("Version:"), style = "padding: 4px 8px;"), tags$td(meta$caller_version %||% "N/A")),
          tags$tr(tags$td(strong("Type:"), style = "padding: 4px 8px;"), tags$td(meta$caller_type %||% "N/A")),
          tags$tr(tags$td(strong("Aligner:"), style = "padding: 4px 8px;"), tags$td(meta$aligner_name %||% "N/A"))
        ),
        
        h5("Truth Set", style = "border-bottom: 1px solid #eee; padding-bottom: 8px; margin: 16px 0 12px 0;"),
        tags$table(style = "width: 100%; font-size: 13px;",
          tags$tr(tags$td(strong("Name:"), style = "padding: 4px 8px; width: 40%;"), tags$td(meta$truth_set_name %||% "N/A")),
          tags$tr(tags$td(strong("Sample:"), style = "padding: 4px 8px;"), tags$td(meta$truth_set_sample %||% "N/A")),
          tags$tr(tags$td(strong("Reference:"), style = "padding: 4px 8px;"), tags$td(meta$truth_set_reference %||% "N/A"))
        ),
        
        h5("Quality Metrics", style = "border-bottom: 1px solid #eee; padding-bottom: 8px; margin: 16px 0 12px 0;"),
        tags$table(style = "width: 100%; font-size: 13px;",
          tags$tr(tags$td(strong("Coverage:"), style = "padding: 4px 8px; width: 40%;"), tags$td(if(!is.na(meta$mean_coverage)) paste0(meta$mean_coverage, "x") else "N/A")),
          tags$tr(tags$td(strong("Read Length:"), style = "padding: 4px 8px;"), tags$td(meta$read_length %||% "N/A"))
        )
      )
    ),
        footer = tagList(
      downloadButton("admin_download_happy", "Download hap.py File", class = "btn-primary"),
      modalButton("Close")
    )
  ))
})

# Download happy file from admin panel
output$admin_download_happy <- downloadHandler(
  filename = function() {
    exp_id <- session$userData$admin_view_exp_id
    if (is.null(exp_id)) return("experiment.csv")
    sprintf("%03d_happy_results.csv", exp_id)
  },
  content = function(file) {
    exp_id <- session$userData$admin_view_exp_id
    if (is.null(exp_id)) {
      writeLines("Error: No experiment selected", file)
      return()
    }
    
    # Import config module to get DATA_FOLDER
    config <- import("config")
    data_folder <- config$DATA_FOLDER
    
    prefix <- sprintf("%03d_", exp_id)
    
    files <- list.files(data_folder, pattern = paste0("^", prefix, ".*\\.csv$"), full.names = TRUE)
    files <- files[!grepl("^000_", basename(files))]
    
    if (length(files) == 0) {
      writeLines(paste("Happy file not found for experiment", exp_id), file)
      return()
    }
    
    file.copy(files[1], file)
  }
)

# Delete private experiment from admin panel
observeEvent(input$admin_delete_private_btn, {
  exp_id <- input$admin_selected_exp
  
  if (is.null(exp_id)) {
    showNotification("Please click on an experiment row first", type = "warning")
    return()
  }
  
  # Get experiment info from stored data
  private_exps <- session$userData$private_exps_data
  if (is.null(private_exps) || nrow(private_exps) == 0) {
    private_exps <- tryCatch({
      db$get_all_private_experiments()
    }, error = function(e) {
      data.frame()
    })
  }
  
  if (nrow(private_exps) == 0) return()
  
  exp_row <- private_exps[private_exps$id == exp_id, ]
  if (nrow(exp_row) == 0) {
    showNotification("Experiment not found", type = "error")
    return()
  }
  
  exp_name <- exp_row$name[1]
  owner <- exp_row$owner_username[1]
  
  showModal(modalDialog(
    title = tagList(icon("exclamation-triangle"), " Confirm Delete"),
    div(
      class = "alert alert-danger",
      p(strong("You are about to delete:")),
      tags$ul(
        tags$li(paste("Experiment:", exp_name)),
        tags$li(paste("ID:", exp_id)),
        tags$li(paste("Owner:", owner))
      ),
      p("This action cannot be undone.", style = "font-weight: bold;")
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_admin_delete_private", "Delete", class = "btn-danger")
    )
  ))
  
  session$userData$pending_delete_exp_id <- exp_id
})

# Confirm delete from admin panel
observeEvent(input$confirm_admin_delete_private, {
  exp_id <- session$userData$pending_delete_exp_id
  if (is.null(exp_id)) return()
  
  user_info <- get_user_info(session)
  
  result <- tryCatch({
    delete_handler$delete_experiment(
      experiment_id = as.integer(exp_id),
      username = user_info$username,
      is_admin = user_info$is_admin
    )
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  removeModal()
  
  if (result$success) {
    showNotification(result$message, type = "message", duration = 5)
    data_reactives$data_refresh_trigger(data_reactives$data_refresh_trigger() + 1)
  } else {
    showNotification(paste("Delete failed:", result$error), type = "error", duration = 8)
  }
  
  session$userData$pending_delete_exp_id <- NULL
})

# Dynamic admin tab insertion/removal
# Open admin modal
observeEvent(input$show_admin_modal, {
 user_info <- get_user_info(session)
 if (!is.null(user_info) && isTRUE(user_info$is_admin)) {
   toggleModal(session, "admin_modal", toggle = "open")
 }
})
}