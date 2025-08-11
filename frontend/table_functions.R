# ============================================================================
# table_functions.R
# ============================================================================
"
Table generation and rendering functions for SNV Benchmarking Dashboard.
Converts processed data into interactive DataTables for dashboard display.

Main tables:
- Experiments overview (Tab 1: main table with row expansion for metadata)
- Performance results (Tab 2: formatted metrics as percentages)  
- Stratified analysis (Tab 4: SNP/INDEL tables with metric highlighting)
- Selected experiments (Sidebar: compact table for comparisons)

Helper functions:
- create_metric_table() (formats stratified data with highlighting)
- create_f1_table() (creates pivot tables for regions)
- setup_table_outputs() (creates all table outputs)

"

# ============================================================================
# DATA PROCESSING FOR TABLES
# ============================================================================

# Create F1 score tables for stratified analysis
create_f1_table <- function(data) {
  if (nrow(data) == 0) {
    return(data.frame(Message = "No data available"))
  }
  
  # Create pivot table: experiments as rows, regions as columns
  table_data <- data %>%
    select(experiment_id, experiment_name, variant_type, subset, f1_score) %>%
    mutate(exp_label = paste0("ID:", experiment_id, " (", variant_type, ")")) %>%
    select(-experiment_id, -experiment_name, -variant_type) %>%
    pivot_wider(names_from = subset, values_from = f1_score, names_sort = TRUE) %>%
    mutate(across(where(is.numeric), ~ round(.x * 100, 2)))
  
  # Rename first column
  colnames(table_data)[1] <- "Experiment"
  
  return(table_data)
}

# Create metric tables for stratified analysis with highlighting
create_metric_table <- function(stratified_data, variant_filter, selected_metric) {
  # Filter for specific variant type
  filtered_data <- stratified_data %>%
    filter(variant_type == variant_filter)
  
  if(nrow(filtered_data) == 0) {
    return(data.frame(Message = paste("No", variant_filter, "data available for selected regions")))
  }
  
  # Format the data showing all metrics
  display_data <- filtered_data %>%
    select(experiment_id, experiment_name, technology, caller, chemistry_name, subset, 
           f1_score, precision, recall) %>%
    mutate(
      across(c(recall, precision, f1_score), ~ round(.x * 100, 2)),
      chemistry_name = ifelse(is.na(chemistry_name) | chemistry_name == "" | chemistry_name == "NULL", 
                              "N/A", chemistry_name)
    ) %>%
    rename(
      "ID" = experiment_id,
      "Experiment" = experiment_name,
      "Technology" = technology,
      "Caller" = caller,
      "Chemistry" = chemistry_name,
      "Region" = subset,
      "F1 Score %" = f1_score,
      "Precision %" = precision,
      "Recall %" = recall
    )
  
  # Get column index of selected metric for highlighting
  metric_col_idx <- switch(selected_metric,
                           "f1_score" = which(names(display_data) == "F1 Score %"),    
                           "precision" = which(names(display_data) == "Precision %"),   
                           "recall" = which(names(display_data) == "Recall %"),         
                           which(names(display_data) == "F1 Score %")  # default
  )
  
  # Create the data table with highlighting
  dt <- DT::datatable(display_data,
                      options = list(
                        pageLength = 10,
                        scrollX = TRUE,
                        dom = 'frtip',
                        columnDefs = list(
                          list(targets = 0, className = "dt-center"),
                          list(targets = "_all", className = "dt-left")   
                        )
                      ),
                      rownames = FALSE,
                      class = 'cell-border stripe hover compact'
  )
  
  # Apply highlighting to selected metric
  if(length(metric_col_idx) > 0) {
    dt <- dt %>%
      DT::formatStyle(
        columns = metric_col_idx,
        backgroundColor = "#e3f2fd",
        fontWeight = "bold"
      )
  }
  
  return(dt)
}

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
    current_mode <- data_reactives$current_mode()
    if (current_mode == "manual_selection") {
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
    
    # Use data that's already processed and available
    stratified_data <- data_reactives$stratified_filtered_data()
    
    if(nrow(stratified_data) == 0) {
      return(data.frame(Message = "Please select at least one region and update analysis"))
    }
    
    # Create SNP table with metric highlighting
    create_metric_table(stratified_data, "SNP", input$selected_metric)
  }, server = FALSE)
  
  # INDEL Metrics Table
  output$indel_metrics_table <- DT::renderDataTable({
    req(input$update_stratified)
    req(input$selected_metric)
    
    # Use data that's already processed and available
    stratified_data <- data_reactives$stratified_filtered_data()
    
    if(nrow(stratified_data) == 0) {
      return(data.frame(Message = "Please select at least one region and update analysis"))
    }
    
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
