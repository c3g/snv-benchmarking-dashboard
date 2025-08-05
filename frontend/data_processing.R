# ============================================================================
# data_processing.R
# ============================================================================
"
Reactive data processing functions for SNV Benchmarking Dashboard.

Main components:
- Core data retrieval and filtering
- Performance data processing for tables and plots
- Stratified analysis data preparation
- Experiment metadata handling
- Data transformation and enhancement functions
"

# ============================================================================
# SETUP FUNCTION - CREATES ALL REACTIVE VALUES AND DATA PROCESSING
# ============================================================================

setup_data_reactives <- function(input, output, session) {
  
  # ====================================================================
  # REACTIVE VALUES FOR STATE MANAGEMENT
  # ====================================================================
  
  current_mode <- reactiveVal("filter")
  display_experiment_ids <- reactiveVal(numeric(0))
  table_selected_ids <- reactiveVal(numeric(0))
  plot_clicked_id <- reactiveVal(NULL)
  comparison_submitted <- reactiveVal(FALSE)
  comparison_type <- reactiveVal(NULL)
  comparison_results <- reactiveVal(numeric(0))
  expanded_rows <- reactiveVal(character(0))
  stratified_raw_data <- reactiveVal(data.frame())
  stratified_filtered_data <- reactiveVal(data.frame())
  stratified_triggered <- reactiveVal(FALSE)
  
  # ====================================================================
  # CORE DATA PROCESSING FUNCTIONS
  # ====================================================================
  
  # Get experiment IDs based on current filter settings
  experiment_ids <- reactive({
    # Re-import db inside reactive to avoid session issues
    db <- import("db_interface")
    
    if (length(display_experiment_ids()) > 0) {
      return(display_experiment_ids())
    }
    
    if (input$filter_type == "tech") {
      return(db$get_experiments_by_technology(input$filter_technology))
    } else if (input$filter_type == "caller") {
      return(db$get_experiments_by_caller(input$filter_caller))
    } else {
      overview <- db$get_experiments_overview()
      return(overview$id)
    }
  })
  
  # Get overview metadata for selected experiments
  experiments_data <- reactive({
    # Re-import db inside reactive to avoid session issues
    db <- import("db_interface")
    
    if (comparison_submitted() && length(comparison_results()) > 0) {
      exp_ids_json <- json_param(comparison_results())
      return(db$get_experiments_overview(NULL, exp_ids_json))
    }
    
    if (current_mode() == "manual_selection") {
      filters <- NULL
      if (input$filter_type == "tech") {
        filters <- list(technology = input$filter_technology)
      } else if (input$filter_type == "caller") {
        filters <- list(caller = input$filter_caller)
      }
      return(db$get_experiments_overview(filters, NULL))
    }
    
    filters <- NULL
    if (input$filter_type == "tech") {
      filters <- list(technology = input$filter_technology)
    } else if (input$filter_type == "caller") {
      filters <- list(caller = input$filter_caller)
    }
    
    return(db$get_experiments_overview(filters, NULL))
  })
  
  # Get experiment IDs for performance analysis
  performance_experiment_ids <- reactive({
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
  
  # Complete metadata and performance results for visualization
  viz_performance_data <- reactive({
    # Re-import db inside reactive to avoid session issues
    db <- import("db_interface")
    
    ids <- performance_experiment_ids()
    if (length(ids) == 0) return(data.frame())
    
    tryCatch({
      ids_json <- json_param(ids)
      enhanced_data <- db$get_experiments_with_performance(ids_json, c('SNP', 'INDEL'))
      return(enhanced_data %>% filter(!is.na(recall) & !is.na(precision) & !is.na(f1_score)))
    }, error = function(e) {
      cat("Error in viz_performance_data:", e$message, "\n")
      return(data.frame())
    })
  })
  
  # SNP-specific plot data
  snp_plot_data <- reactive({
    viz_data <- viz_performance_data()
    if (nrow(viz_data) == 0) return(data.frame())
    
    viz_data %>% 
      filter(variant_type == "SNP") %>%
      mutate(plot_id = paste0("snp_", experiment_id))
  })
  
  # INDEL-specific plot data
  indel_plot_data <- reactive({
    viz_data <- viz_performance_data()
    if (nrow(viz_data) == 0) return(data.frame())
    
    viz_data %>% 
      filter(variant_type == "INDEL") %>%
      mutate(plot_id = paste0("indel_", experiment_id))
  })
  
  # Performance data for tables with formatting
  performance_data <- reactive({
    viz_data <- viz_performance_data()
    
    if (nrow(viz_data) == 0) {
      return(data.frame())
    }
    
    enhanced_data <- viz_data %>%
      select(
        experiment_id, experiment_name, 
        technology, caller,
        platform_name, chemistry_name, mean_coverage,
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
        "Caller" = caller,
        "Platform" = platform_name,
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
      input$complex_regions
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
                           coalesce(caller, "Unknown"), "-",
                           coalesce(chemistry_name, ""), ")")
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
    current_mode = current_mode,
    display_experiment_ids = display_experiment_ids,
    table_selected_ids = table_selected_ids,
    plot_clicked_id = plot_clicked_id,
    comparison_submitted = comparison_submitted,
    comparison_type = comparison_type,
    comparison_results = comparison_results,
    expanded_rows = expanded_rows,
    stratified_raw_data = stratified_raw_data,
    stratified_filtered_data = stratified_filtered_data,
    stratified_triggered = stratified_triggered,
    
    # Data processing functions
    experiment_ids = experiment_ids,
    experiments_data = experiments_data,
    performance_experiment_ids = performance_experiment_ids,
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