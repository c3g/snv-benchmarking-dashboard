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
- Delete modal experiment table (Delete datasets Modal)

Helper functions:
- create_experiment_details_html() (expand Tab 1 table rows)
- create_metric_table() (formats stratified data with highlighting)
- create_f1_table() (creates pivot tables for regions)
- setup_table_outputs() (creates all table outputs)
- create_delete_experiments_table() (creates a list of experiments to select from)
"

# ============================================================================
# DATA PROCESSING FOR TABLES
# ============================================================================
create_experiment_details_html <- function(metadata) {
  if (nrow(metadata) == 0) return(NULL)
  
  meta <- metadata[1, ]
  exp_id <- meta$id
  
  # Create compact HTML content for row expansion
  details_html <- paste0(
    '<tr class="detail-row-', exp_id, '">',
    '<td colspan="12">',
    '<div class="detail-content">',
    '<div class="detail-grid">',
    
    # Platform Details
    '<div class="detail-section">',
    '<h6 style="color: #4472ca; font-weight: 700; font-size: 13px; border-bottom: 2px solid #4472ca; padding-bottom: 4px; margin-bottom: 8px;">Platform Details</h6>',
    '<div class="detail-item"><strong>Platform:</strong> ', meta$platform_name %||% "N/A", '</div>',
    '<div class="detail-item"><strong>Version:</strong> ', meta$platform_version %||% "N/A", '</div>',
    '<div class="detail-item"><strong>Type:</strong> ', meta$platform_type %||% "N/A", '</div>',
    '<div class="detail-item"><strong>Target:</strong> ', meta$target %||% "N/A", '</div>',
    '<div class="detail-item"><strong>Chemistry:</strong> ', meta$chemistry_name %||% "N/A", '</div>',
    '</div>',
    
    # Analysis Details  
    '<div class="detail-section">',
    '<h6 style="color: #4472ca; font-weight: 700; font-size: 13px; border-bottom: 2px solid #4472ca; padding-bottom: 4px; margin-bottom: 8px;">Analysis Details</h6>',
    '<div class="detail-item"><strong>Caller Type:</strong> ', meta$caller_type %||% "N/A", '</div>',
    '<div class="detail-item"><strong>Caller Model:</strong> ', meta$caller_model %||% "N/A", '</div>',
    '<div class="detail-item"><strong>Aligner:</strong> ', paste(meta$aligner_name %||% "N/A", meta$aligner_version %||% ""), '</div>',
    '<div class="detail-item"><strong>Variants:</strong> ', meta$variant_origin %||% "N/A", ' ', meta$variant_type %||% "", '</div>',
    '<div class="detail-item"><strong>Phased:</strong> ', ifelse(is.na(meta$is_phased), "N/A", ifelse(meta$is_phased, "Yes", "No")), '</div>',
    '</div>',
    
    # Quality & Truth
    '<div class="detail-section">',
    '<h6 style="color: #4472ca; font-weight: 700; font-size: 13px; border-bottom: 2px solid #4472ca; padding-bottom: 4px; margin-bottom: 8px;">Quality & Benchmarking</h6>',
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
}

# ------------------------------------------------
# Create metrics tables for stratified analysis
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

# ------------------------------------------------
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
    select(experiment_id, experiment_name, technology, caller,caller_version, platform_name, subset, 
           f1_score, precision, recall) %>%
    mutate(
      across(c(recall, precision, f1_score), ~ round(.x * 100, 2)),
    # chemistry name not included in stratified results for now
    #  chemistry_name = ifelse(is.na(chemistry_name) | chemistry_name == "" | chemistry_name == "NULL", 
    #                          "N/A", chemistry_name)
    ) %>%
    arrange(experiment_id) %>% 
    rename(
      "ID" = experiment_id,
      "Experiment" = experiment_name,
      "Technology" = technology,
      "Caller" = caller,
      "Version" = caller_version,
      "Platform" = platform_name,
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
                        pageLength = 20,
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
        backgroundColor = "#d0dde8",
        fontWeight = "bold"
      )
  }
  
  return(dt)
}

# ------------------------------------------------
# Create table for delete modal
create_delete_experiments_table <- function(data) {
  if (nrow(data) == 0) {
    return(DT::datatable(data.frame(Message = "No experiments found")))
  }
  
  # Create simplified view for deletion
  delete_data <- data %>%
    select(id, name, technology, platform_name, caller, caller_version, created_at) %>%
    mutate(
      created_at = if("created_at" %in% names(.)) format(as.Date(created_at), "%Y/%m/%d") else "N/A"
    )
  
  # Create DataTable with selection enabled
  dt <- DT::datatable(
    delete_data,
    selection = list(mode = 'multiple', target = 'row'),
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      dom = 'frtip',
      stateSave = FALSE, #prevents browser caching issues
      columnDefs = list(
        list(targets = "_all", className = "dt-left")
      )
    ),
    rownames = FALSE,
    colnames = c("ID", "Experiment", "Technology", "Platform", "Caller", "Version", "Created"),
    class = 'cell-border stripe hover compact'
  )
  
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
    
    # Handle NULL or empty dataframes
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No experiments found")))
    }
    
    # Add visibility indicator column with safe NULL/NA handling
    if ("is_public" %in% colnames(df) && nrow(df) > 0) {
      df$Visibility <- sapply(df$is_public, function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x) || isTRUE(x)) {
          "Public"
        } else {
          "Private"
        }
      })
    } else {
      df$Visibility <- "Public"
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
    
    # Select and reorder columns
    column_order <- c(
      "expand_button", 
      "id", 
      "name", 
      "technology", 
      "platform_name", 
      "caller", 
      "caller_version", 
      "chemistry", 
      "truth_set", 
      "sample",
      "Visibility", 
      "created_at"
    )
    
    # Keep only columns that exist in df
    column_order <- column_order[column_order %in% names(df)]
    df <- df[, column_order]
    
    # Configure table selection based on current mode
    current_mode <- data_reactives$current_mode()
    
    if (current_mode == "manual_selection") {
      selected_ids <- isolate(data_reactives$table_selected_ids())
      selected_rows <- which(df$id %in% selected_ids)
      selection_config <- list(mode = 'multiple', selected = selected_rows)
    } else {
      selection_config <- 'none'
    }
    
    # Create DataTable
    dt <- DT::datatable(
      df,
      selection = selection_config,
      escape = FALSE,
      options = list(
        pageLength = 20,
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
          list(targets = 0, orderable = FALSE, width = "20px"),              # Expand button
          list(targets = 1, width = "20px", className = "dt-center"),        # ID
          list(targets = 2, width = 150),                                     # Name
          list(targets = 10, className = "dt-center", width = "80px")        # Visibility (centered)
        )
      ),
      rownames = FALSE,
      colnames = c(
        "",            # Expand button
        "ID", 
        "Name", 
        "Technology", 
        "Platform", 
        "Caller", 
        "Version", 
        "Chemistry", 
        "Truth Set", 
        "Sample",
        "Visibility", 
        "Created"
      )
    )
      dt <- dt %>%
      formatStyle(
        "Visibility",
        backgroundColor = styleEqual("Private", "#fff3cd")  # Light yellow
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
        pageLength = 20,
        scrollX = TRUE,
        columnDefs = list(
          list(targets = 0, className = "dt-center", width = "50px"),     # ID column
          list(targets = c(9, 10, 11), className = "dt-center"),         # Performance columns 
          list(targets = 7, className = "dt-center"),                    # Coverage column 
          list(targets = 5, className = "dt-body-wrap"),                 # Version column 
          list(targets = "_all", className = "dt-body-nowrap")
        )
      ),
      rownames = FALSE,
      colnames = c(
        "ID", "Experiment", "Technology", "Platform",
        "Caller", "Version", "Chemistry", "Coverage", "Variant Type", 
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
    
    # Use data that's already processed
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
    
    # Use data that's already processed
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
  
  # ====================================================================
  # DELETE EXPERIMENTS TABLE (MODAL)
  # ====================================================================
  
output$delete_experiments_table <- DT::renderDataTable({
    # Force refresh when modal opens (prevents browser caching)
    data_reactives$delete_modal_trigger()
    data_reactives$data_refresh_trigger()
    
    # Get all experiments for deletion selection
    all_experiments <- tryCatch({
      user_info <- get_user_info(session)
      user_id <- if (!is.null(user_info)) session$userData$user_id else NULL
      is_admin_user <- if (!is.null(user_info)) isTRUE(user_info$is_admin) else FALSE
      db$get_experiments_overview(NULL, NULL, user_id, is_admin_user)
    }, error = function(e) {
      data.frame()
    })
    
    # Store data for use in delete confirmation
    session$userData$delete_table_data <- all_experiments
    
    create_delete_experiments_table(all_experiments)
  }, server = TRUE)  # Server-side for reliable selection
  outputOptions(output, "delete_experiments_table", suspendWhenHidden = FALSE)
  
}