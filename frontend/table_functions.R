# ============================================================================
# table_functions.R
# ============================================================================
"
Table generation and rendering functions for SNV Benchmarking Dashboard.

Main components:
- Experiments overview table with row expansion
- Performance results table with conditional formatting
- Stratified analysis tables (SNP and INDEL)
- Selected experiments compact table
- Table styling and interaction handlers
"

# ============================================================================
# TABLE OUTPUT SETUP FUNCTION
# ============================================================================

setup_table_outputs <- function(input, output, session, data_reactives) {
  
  # ====================================================================
  # EXPERIMENTS OVERVIEW TABLE (TAB 1)
  # ====================================================================
  
  output$experiments_table <- DT::renderDataTable({
    df <- data_reactives$experiments_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No experiments found")))
    }
    
    # Add expand button column
    df$expand_button <- paste0(
      '<button class="details-toggle" onclick="toggleDetails(', df$id, ')">',
      '▶',
      '</button>'
    )
    
    # Format created_at date
    if ("created_at" %in% names(df)) {
      df$created_at <- format(as.Date(df$created_at), "%Y/%m/%d")
    }
    
    # Reorder columns to put expand button first
    df <- df[, c("expand_button", setdiff(names(df), "expand_button"))]
    
    # Configure table selection based on current mode
    if (data_reactives$current_mode() == "manual_selection") {
      selection_config <- list(mode = 'multiple')
    } else {
      selection_config <- 'none'
    }
    
    # Create DataTable
    dt <- DT::datatable(
      df,
      selection = selection_config,
      escape = FALSE,
      extensions = c('Responsive'),
      options = list(
        responsive = TRUE,
        scrollX = TRUE,
        autoWidth = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).css('width', '100%');",
          "  $(this.api().table().node()).css('width', '100%');",
          "  $('.dataTables_wrapper').css('width', '100%');",
          "}"
        ),
        columnDefs = list(
          list(targets = 0, orderable = FALSE, width = "20px"),
          list(targets = 1, width = "20px", className = "dt-center"),
          list(targets = 2, width = 150)
        )
      ),
      rownames = FALSE,
      colnames = c("", "ID", "Name", "Technology", "Platform", "Caller", "Version", "Chemistry", "Truth Set", "Sample", "Created")
    ) %>%
      # Technology-based row coloring
      formatStyle(
        "technology", 
        target = "row",
        backgroundColor = styleEqual(
          c("ILLUMINA", "PACBIO", "ONT", "MGI"),  
          c("#fef6f6", "#faf8ff", "#f0fcfd", "#f7fbf5") 
        )
      )
    
    return(dt)
  })
  
  # ====================================================================
  # PERFORMANCE RESULTS TABLE (TAB 2)
  # ====================================================================
  
  output$performance_table <- DT::renderDataTable({
    df <- data_reactives$performance_data()
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No performance data found")))
    }
    
    # Remove row_class column if it exists
    if ("row_class" %in% names(df)) {
      df <- df %>% select(-row_class)
    }
    
    # Create DataTable with conditional formatting
    dt <- DT::datatable(
      df,
      selection = 'none',
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = 0, className = "dt-center", width = "50px"),     # ID column
          list(targets = c(8, 9, 10), className = "dt-center"),          # Performance columns
          list(targets = 6, className = "dt-center"),                    # Coverage column
          list(targets = "_all", className = "dt-body-nowrap")           # Prevent text wrapping
        )
      ),
      rownames = FALSE,
      colnames = c(
        "ID", "Experiment", "Technology", "Caller", 
        "Platform", "Chemistry", "Coverage", "Variant Type", 
        "F1 Score (%)", "Precision (%)", "Recall (%)"
      )
    ) %>%
      # Variant type row coloring
      formatStyle(
        "Variant",
        target = "row",
        backgroundColor = styleEqual(
          c("SNP", "INDEL"), 
          c("#fdf2f2", "#f2f7fd")  # Light red for SNP, light blue for INDEL
        )
      )
    
    return(dt)
  })
  
  # ====================================================================
  # STRATIFIED ANALYSIS TABLES (TAB 4)
  # ====================================================================
  
  # SNP Metrics Table
  output$snp_metrics_table <- DT::renderDataTable({
    req(input$update_stratified)
    req(input$selected_metric)  
    
    # Re-import db inside reactive to avoid session issues
    db <- import("db_interface")
    
    # Get stratified data
    all_selected_regions <- c(input$core_regions, input$functional_regions, 
                              input$homopolymer_regions, input$gc_low, 
                              input$gc_normal, input$gc_high, input$complex_regions)
    
    if(length(all_selected_regions) == 0) {
      return(data.frame(Message = "Please select at least one region"))
    }
    
    stratified_data <- db$get_stratified_performance_by_regions(
      experiment_ids_param = toJSON(data_reactives$performance_experiment_ids()),
      variant_types = list("SNP", "INDEL"),
      regions = all_selected_regions
    )
    
    # Create SNP table with metric highlighting
    create_metric_table(stratified_data, "SNP", input$selected_metric)
    
  }, server = FALSE)
  
  # INDEL Metrics Table
  output$indel_metrics_table <- DT::renderDataTable({
    req(input$update_stratified)
    req(input$selected_metric)  
    
    # Re-import db inside reactive to avoid session issues
    db <- import("db_interface")
    
    # Get stratified data
    all_selected_regions <- c(input$core_regions, input$functional_regions, 
                              input$homopolymer_regions, input$gc_low, 
                              input$gc_normal, input$gc_high, input$complex_regions)
    
    if(length(all_selected_regions) == 0) {
      return(data.frame(Message = "Please select at least one region"))
    }
    
    stratified_data <- db$get_stratified_performance_by_regions(
      experiment_ids_param = toJSON(data_reactives$performance_experiment_ids()),
      variant_types = list("SNP", "INDEL"),
      regions = all_selected_regions
    )
    
    # Create INDEL table with metric highlighting
    create_metric_table(stratified_data, "INDEL", input$selected_metric)
    
  }, server = FALSE)
  
  # ====================================================================
  # SELECTED EXPERIMENTS COMPACT TABLE (SIDEBAR)
  # ====================================================================
  
  output$compact_selected_experiments <- renderTable({
    ids <- data_reactives$table_selected_ids()
    if (length(ids) == 0) {
      return(data.frame(Info = "No experiments selected"))
    }
    
    current_data <- data_reactives$experiments_data()
    selected_data <- current_data[current_data$id %in% ids, ]
    compact_data <- data.frame(
      ID = as.integer(selected_data$id),
      Tech = selected_data$technology,
      Caller = selected_data$caller,
      stringsAsFactors = FALSE
    )
    
    return(compact_data)
  }, striped = TRUE, hover = TRUE, spacing = 'xs', width = "100%",
  class = "table-condensed",
  style = "font-size: 11px; margin-bottom: 5px;")
}