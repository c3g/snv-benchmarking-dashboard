# ============================================================================
# data_processing.R
# ============================================================================
"
Reactive data processing functions for SNV Benchmarking Dashboard.
Turns user interactions into database queries and prepares data for outputs.

Main components:
- State tracking (current mode/ selection, etc)
- Data fetching (get experiments from database based on filters/selections)
- Data formatting (convert raw data into table/plot-ready format)
- Mode handling (simple filtering vs technology comparisons vs manual selection)

"

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Filter experiments by visibility mode (applied after database query)
apply_visibility_filter_local <- function(df, filter_mode, user_id) {
  # Add at the start:
  if (is.null(filter_mode) || length(filter_mode) == 0 || !nzchar(filter_mode)) {
    return(df)
  }
  
  # Handle NA values in is_public column - treat NA as public (legacy data)
  switch(filter_mode,
    "public" = df %>% filter(is_public == TRUE | is.na(is_public) | is.na(owner_id)),
    "mine" = if (!is.null(user_id)) df %>% filter(owner_id == user_id & is_public == FALSE) else df[0,],
    df  # "all" returns unchanged
  )
}
# ============================================================================
# SETUP FUNCTION - CREATES ALL REACTIVE VALUES AND DATA PROCESSING
# ============================================================================

setup_data_reactives <- function(input, output, session) {
  
  # ====================================================================
  # REACTIVE VALUES FOR STATE MANAGEMENT
  # ====================================================================

  # Trigger to refresh all data
   data_refresh_trigger <- reactiveVal(0)
  # Trigger to refresh delete modal table (prevents browser caching issues)

  delete_modal_trigger <- reactiveVal(0)
  # Current app mode: "filter", "tech_comparison", "caller_comparison", "manual_selection"

  current_mode <- reactiveVal("filter")
  
  # experiments to show in main table (overrides filters)
  display_experiment_ids <- reactiveVal(numeric(0))
  
  # Currently active truth set filter
  active_truth_set_filter <- reactiveVal("All")

  # Visibility filter mode -- not used 
  # visibility_filter <- reactiveVal("all")  # "all", "public", "mine"

  # User-selected experiments from table clicks
  table_selected_ids <- reactiveVal(numeric(0))
  
  # Clicked experiment from plots
  plot_clicked_id <- reactiveVal(NULL)
  
  # Has user submitted a comparison 
  comparison_submitted <- reactiveVal(FALSE)
  
  # What type of comparison: "technology" or "caller"
  comparison_type <- reactiveVal(NULL)
  
  # Results from submitted comparisons
  comparison_results <- reactiveVal(numeric(0))
  
  # Which table rows are expanded 
  expanded_rows <- reactiveVal(character(0))
  
  # Raw stratified data from database
  stratified_raw_data <- reactiveVal(data.frame())
  
  # Processed stratified data for display
  stratified_filtered_data <- reactiveVal(data.frame())
  
  # Has stratified analysis been run (shows/hides Tab 4)
  stratified_triggered <- reactiveVal(FALSE)

  # admin ralated stats
  admin_stats <- reactiveVal(NULL)
  
  # ====================================================================
  # CORE DATA PROCESSING FUNCTIONS
  # ====================================================================
  
  # Get experiment IDs based on current filter settings
  experiment_ids <- reactive({
    
    # Re-run when auth state changes
    input$user_authenticated
    
    tryCatch({
      if (length(display_experiment_ids()) > 0) {
        return(display_experiment_ids())
      }
      
      # Get user context for visibility filtering
      user_info <- get_user_info(session)
      user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
      is_admin_user <- if (!is.null(user_info)) isTRUE(user_info$is_admin) else FALSE
      
      if (input$filter_type == "tech") {
        result <- db$get_experiments_by_technology(input$filter_technology, user_id, is_admin_user)
        if (is.null(result) || length(result) == 0) return(numeric(0))
        return(result)
      } else if (input$filter_type == "caller") {
        result <- db$get_experiments_by_caller(input$filter_caller, user_id, is_admin_user)
        if (is.null(result) || length(result) == 0) return(numeric(0))
        return(result)
      } else {
        overview <- db$get_experiments_overview(NULL, NULL, user_id, is_admin_user)
        if (is.null(overview) || nrow(overview) == 0) return(numeric(0))
        return(overview$id)
      }
    }, error = function(e) {
      showNotification("Database connection error. Please refresh the page.", 
                       type = "error", duration = 10)
      return(numeric(0))
    })
  })
  
  # Get overview metadata for selected experiments
  experiments_data <- reactive({
    data_refresh_trigger()
    input$user_authenticated

    tryCatch({
      # Get user context (use isTRUE to safely handle NA values)
      user_info <- get_user_info(session)
      user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
      is_admin_user <- if (!is.null(user_info)) isTRUE(user_info$is_admin) else FALSE
      
      # Get visibility filter (default to "all" if not set)
      cat("visibility_filter:", input$visibility_filter, "length:", length(input$visibility_filter), "\n")

      vis_filter <- input$visibility_filter
      if (is.null(vis_filter) || length(vis_filter) == 0 || !nzchar(vis_filter)) {
        vis_filter <- "all"
      }
      # Comparison mode - show comparison results
      if (comparison_submitted() && length(comparison_results()) > 0) {
        exp_ids_json <- json_param(comparison_results())
        df <- db$get_experiments_overview(NULL, exp_ids_json, user_id, is_admin_user)
        if (is.null(df) || !is.data.frame(df)) return(data.frame())
        return(apply_visibility_filter_local(df, vis_filter, user_id))
      }
      
      # Manual selection mode
      if (current_mode() == "manual_selection") {
        filters <- NULL
        if (input$filter_type == "tech") {
          filters <- list(technology = input$filter_technology)
        } else if (input$filter_type == "caller") {
          filters <- list(caller = input$filter_caller)
        }
        df <- db$get_experiments_overview(filters, NULL, user_id, is_admin_user)
        if (is.null(df) || !is.data.frame(df)) return(data.frame())
        return(apply_visibility_filter_local(df, vis_filter, user_id))
      }
      
      # Standard filter mode
      filters <- NULL
      if (input$filter_type == "tech") {
        filters <- list(technology = input$filter_technology)
      } else if (input$filter_type == "caller") {
        filters <- list(caller = input$filter_caller)
      }
      
      df <- db$get_experiments_overview(filters, NULL, user_id, is_admin_user)
      if (is.null(df) || !is.data.frame(df)) return(data.frame())
      return(apply_visibility_filter_local(df, vis_filter, user_id))
    }, error = function(e) {
      cat("Error in experiments_data:", e$message, "\n")
      return(data.frame())
    })
  })
  
  # Get experiment IDs for performance analysis
  performance_experiment_ids <- reactive({
    
    # Re-run when auth state changes
    input$user_authenticated
    
    if (current_mode() == "manual_selection") {
      selected_ids <- table_selected_ids()
      if (length(selected_ids) > 0) {
        return(selected_ids)
      } else {
        return(experiment_ids())
      }
    } else if (comparison_submitted()) {
      return(comparison_results())
    } else {
      return(experiment_ids())
    }
  })
  
  # Filter performance experiment IDs by active truth set
  performance_experiment_ids_filtered <- reactive({
    ids <- performance_experiment_ids()
    truth_set_filter <- active_truth_set_filter() 
    
    # If "ALL" selected, return all IDs
    if (is.null(truth_set_filter) || truth_set_filter == "ALL") {
      return(ids)
    }
    
    # filter experiments by truth set
    tryCatch({
      user_info <- get_user_info(session)
      user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
      is_admin_user <- if (!is.null(user_info)) isTRUE(user_info$is_admin) else FALSE
      overview <- db$get_experiments_overview(NULL, json_param(ids), user_id, is_admin_user)
      filtered <- overview %>%
        filter(toupper(truth_set) == toupper(truth_set_filter)) %>%
        pull(id)
      
      if (length(filtered) == 0) {
        showNotification(paste("No experiments found with truth set:", truth_set_filter), 
                        type = "warning", duration = 4)
      }
      return(filtered)
    }, error = function(e) {
      cat("Error filtering by truth set:", e$message, "\n")
      return(ids)
    })
  })

  # Complete metadata and performance results for visualization
  viz_performance_data <- reactive({

    
    ids <- performance_experiment_ids_filtered()
    if (length(ids) == 0) return(data.frame())
    
    tryCatch({
      ids_json <- json_param(ids)
      enhanced_data <- db$get_experiments_with_performance(ids_json, VARIANT_TYPE_OPTIONS)
      
      # Additional validation
      if (nrow(enhanced_data) == 0) {
        cat("No performance data found for experiments:", paste(ids, collapse = ", "), "\n")
        return(data.frame())
      }
      
      return(enhanced_data %>% filter(!is.na(recall) & !is.na(precision) & !is.na(f1_score)))
    }, error = function(e) {
      cat("Error in viz_performance_data:", e$message, "\n")
      return(data.frame())
    })
  })
  
  # SNP-specific plot data
  snp_plot_data <- reactive({
  req(nrow(viz_performance_data()) > 0) # request data to calculate
  
  viz_performance_data() %>%
    filter(variant_type == "SNP") %>%
      mutate(plot_id = paste0("snp_", experiment_id))
  })
  
  # INDEL-specific plot data
  indel_plot_data <- reactive({
  req(nrow(viz_performance_data()) > 0) # request data to calculate
  
  viz_performance_data() %>%
    filter(variant_type == "INDEL") %>%
      mutate(plot_id = paste0("indel_", experiment_id))
  })
  
  # Performance data for tables 
  performance_data <- reactive({
    viz_data <- viz_performance_data()
    
    if (nrow(viz_data) == 0) {
      return(data.frame())
    }
    
    enhanced_data <- viz_data %>%
      select(
        experiment_id, experiment_name, 
        technology, platform_name,
        caller, caller_version, chemistry_name, mean_coverage,
        variant_type, recall, precision, f1_score
      ) %>%
      mutate(
        recall = round(recall * 100, 2),
        precision = round(precision * 100, 2), 
        f1_score = round(f1_score * 100, 2),
        mean_coverage = case_when(
          is.na(mean_coverage) ~ "N/A",
          TRUE ~ paste0(round(mean_coverage, 1), "x")
        ),
        chemistry_name = ifelse(is.na(chemistry_name) | chemistry_name == "", "N/A", chemistry_name),
        platform_name = ifelse(is.na(platform_name) | platform_name == "", "N/A", platform_name)
      ) %>%
      arrange(experiment_id, variant_type) %>%
      rename(
        "ID" = experiment_id,
        "Experiment" = experiment_name,
        "Technology" = technology,
        "Platform" = platform_name,        
        "Caller" = caller,
        "Version" = caller_version,
        "Chemistry" = chemistry_name,
        "Coverage" = mean_coverage,
        "Variant" = variant_type,
        "F1 Score (%)" = f1_score,
        "Precision (%)" = precision,
        "Recall (%)" = recall
      )
    
    return(enhanced_data)
  })
  
  # ====================================================================
  # STRATIFIED ANALYSIS DATA PROCESSING
  # ====================================================================
  
  # Collect all selected regions from UI checkboxes
  get_selected_regions <- reactive({
    all_selected_regions <- c(
      input$core_regions,
      input$functional_regions, 
      input$homopolymer_regions,
      input$gc_low,
      input$gc_normal, 
      input$gc_high,
      input$gc_extreme,
      input$complex_regions,
      input$satellites_regions,
      input$non_repetitive_regions
    )
    
    all_selected_regions <- unique(all_selected_regions[all_selected_regions != ""])
    return(all_selected_regions)
  })
  
  # Process stratified data with metric selection
  stratified_processed_data <- reactive({
    if (!stratified_triggered() || nrow(stratified_raw_data()) == 0) {
      return(data.frame())
    }
    
    raw_data <- stratified_raw_data()
    processed_data <- raw_data %>%
      filter(!is.na(f1_score) & !is.na(precision) & !is.na(recall)) %>% 
      mutate(
        exp_label = paste0("ID:", experiment_id, " (", 
                           coalesce(technology, "Unknown"), "-", 
                           coalesce(caller, "Unknown"), 
                           ")")
      ) %>%
      arrange(subset, variant_type, desc(experiment_id))
    
    return(processed_data)
  })
  
  # Apply metric selection to display data
  stratified_display_data <- reactive({
    processed_data <- stratified_processed_data()
    selected_metric <- input$selected_metric
    
    if (nrow(processed_data) == 0) {
      return(data.frame())
    }
    
    processed_data %>%
      mutate(metric_value = !!sym(selected_metric))
  })
  
  # Update filtered data when display data changes
  observe({
    stratified_filtered_data(stratified_display_data())
  })
  
  # Create F1 table data
  f1_table_data <- reactive({
    data <- stratified_filtered_data()
    create_f1_table(data)
  })
  
  # ====================================================================
  # RETURN ALL REACTIVE VALUES FOR USE IN SERVER
  # ====================================================================
  
  return(list(
    # State management
    admin_stats = admin_stats,
    current_mode = current_mode,
    display_experiment_ids = display_experiment_ids,
    active_truth_set_filter = active_truth_set_filter,
    table_selected_ids = table_selected_ids,
    plot_clicked_id = plot_clicked_id,
    comparison_submitted = comparison_submitted,
    comparison_type = comparison_type,
    comparison_results = comparison_results,
    expanded_rows = expanded_rows,
    stratified_raw_data = stratified_raw_data,
    stratified_filtered_data = stratified_filtered_data,
    stratified_triggered = stratified_triggered,
    data_refresh_trigger = data_refresh_trigger,
    delete_modal_trigger = delete_modal_trigger,
    
    # Data processing functions
    experiment_ids = experiment_ids,
    experiments_data = experiments_data,
    performance_experiment_ids = performance_experiment_ids,
    performance_experiment_ids_filtered = performance_experiment_ids_filtered,
    viz_performance_data = viz_performance_data,
    snp_plot_data = snp_plot_data,
    indel_plot_data = indel_plot_data,
    performance_data = performance_data,
    get_selected_regions = get_selected_regions,
    stratified_processed_data = stratified_processed_data,
    stratified_display_data = stratified_display_data,
    f1_table_data = f1_table_data
  ))
}