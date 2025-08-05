# ============================================================================
# utils.R
# ============================================================================
"
Utility functions for SNV Benchmarking Dashboard.

Main components:
- 1. Color and shape mappings
- 2. Helper functions for data processing and JSON parameter handling for R-Python interface
- 3. F1 contour calculation for performance plots
- 4. HTML legend generation for plot displays
- 5. Data processing for tables
"

# ============================================================================
# 1. COLOR AND SHAPE MAPPINGS
# ============================================================================

# Technology color scheme for consistent visualization
technology_colors <- c(
  "ILLUMINA" = "#F8766D",    # Red
  "PACBIO" = "#C77CFF",      # Purple
  "ONT" = "#00BFC4",         # Cyan
  "MGI" = "#7CAE00",         # Green
  "Unknown" = "#E76BF3"      # Fallback color
)

# Caller shape mapping for scatter plots
caller_shapes <- c(
  "DEEPVARIANT" = 16,        # Circle 
  "GATK" = 17,               # Triangle
  "CLAIR3" = 15,             # Square 
  "Unknown" = 4              # X shape
)

# Shape symbols for HTML legend display
shape_symbols <- c("16" = "●", "17" = "▲", "15" = "■", "4" = "✕")

# Technology-caller gradient combinations for stratified plots
tech_caller_colors <- c(
  # ILLUMINA family (Red variations)
  "ILLUMINA-DEEPVARIANT" = "#F8766D",
  "ILLUMINA-GATK" = "#E55A5A",
  "ILLUMINA-CLAIR3" = "#FF9999",
  
  # PACBIO family (Purple variations)
  "PACBIO-DEEPVARIANT" = "#C77CFF",
  "PACBIO-GATK" = "#B366FF",
  "PACBIO-CLAIR3" = "#D999FF",
  
  # ONT family (Cyan variations)
  "ONT-DEEPVARIANT" = "#00BFC4",
  "ONT-GATK" = "#00A5A8",
  "ONT-CLAIR3" = "#33CCCC",
  
  # MGI family (Green variations)
  "MGI-DEEPVARIANT" = "#7CAE00",
  "MGI-GATK" = "#6B9500",
  "MGI-CLAIR3" = "#99CC33"
)

# ============================================================================
# 2. HELPER FUNCTIONS
# ============================================================================

# Null coalescing operator (safe value handling)
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x) || x == "") y else x
}

# Convert R data to JSON format for Python interface
json_param <- function(data) {
  if (is.null(data) || length(data) == 0) {
    return("[]")
  }
  jsonlite::toJSON(data, auto_unbox = TRUE)
}

# ============================================================================
# 3. PLOT UTILITY FUNCTIONS
# ============================================================================

# Calculate F1 score contour data for performance plots
create_f1_contour <- function() {
  # F1 calculation function
  f1_contour_function <- function(p, r) {
    result <- 2 * (p * r) / (p + r)
    result[!is.finite(result)] <- NA
    return(result)
  }
  
  # Create precision and recall sequences
  p_seq <- seq(0.01, 0.99, length.out = 100)
  r_seq <- seq(0.01, 0.99, length.out = 100)
  
  # Generate contour grid
  contour_data <- expand.grid(p = p_seq, r = r_seq) %>%
    mutate(f1 = f1_contour_function(p, r)) %>%
    filter(!is.na(f1) & is.finite(f1))
  
  return(contour_data)
}

# Create stratified performance plots with technology-caller gradients
create_stratified_grouped_plot <- function(data, variant_type, metric_name = "f1_score") {
  # Filter for variant type
  plot_data <- data %>%
    filter(variant_type == !!variant_type)
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                      label = paste("No", variant_type, "data for selected regions"), 
                      size = 6) +
             xlim(0, 1) + ylim(0, 1) +
             theme_bw())
  }
  
  # Dynamic metric labels
  metric_labels <- list(
    "f1_score" = "F1 Score",
    "precision" = "Precision", 
    "recall" = "Recall"
  )
  
  metric_label <- metric_labels[[metric_name]] %||% "Metric"
  
  # Create technology-caller combination key
  plot_data <- plot_data %>%
    mutate(tech_caller = paste(technology, caller, sep = "-"))
  
  # Create the gradient plot
  plot <- ggplot(plot_data, aes(x = metric_value, y = exp_label)) + 
    geom_col(aes(fill = tech_caller), alpha = 0.85, width = 0.7) + 
    geom_text(
      aes(label = paste0(round(metric_value * 100, 2), "%")), 
      hjust = -0.1, size = 3.5, color = "black"
    ) +
    scale_fill_manual(values = tech_caller_colors, na.value = "gray70") +
    scale_x_continuous(limits = c(0, 1.1), labels = scales::percent_format()) +
    facet_wrap(~ subset, scales = "free_y", ncol = 1) +
    labs(x = metric_label, y = "Experiment", fill = "Technology + Caller") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.y = element_text(face = "bold", size = 9),
      axis.text.x = element_text(face = "bold", size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend(ncol = 4, byrow = TRUE))
  
  return(plot)
}

# ============================================================================
# 4. HTML LEGEND GENERATION
# ============================================================================

# Create technology legend for visualization tab
create_technology_legend <- function() {
  legend_items <- ""
  
  for (tech in names(technology_colors)) {
    if (tech != "Unknown") {
      color <- technology_colors[tech]
      legend_items <- paste0(legend_items, 
                             '<div style="display: flex; align-items: center; margin-bottom: 5px;">',
                             '<div style="width: 12px; height: 12px; background-color: ', color, 
                             '; border-radius: 50%; margin-right: 8px; border: 1px solid #333;"></div>',
                             '<span style="font-size: 12px;">', tech, '</span>',
                             '</div>')
    }
  }
  
  return(paste0(
    '<div style="background: white; padding: 10px; border: 1px solid #ddd; border-radius: 5px; margin-bottom: 10px;">',
    '<div style="font-weight: bold; margin-bottom: 8px; font-size: 13px;">Sequencing Technology</div>',
    legend_items,
    '</div>'
  ))
}

# Create caller legend for visualization tab
create_caller_legend <- function() {
  legend_items <- ""
  
  for (caller in names(caller_shapes)) {
    if (caller != "Unknown") {
      shape_code <- as.character(caller_shapes[caller])
      symbol <- shape_symbols[shape_code]
      
      legend_items <- paste0(legend_items,
                             '<div style="display: flex; align-items: center; margin-bottom: 5px;">',
                             '<span style="font-size: 14px; margin-right: 8px; width: 12px; text-align: center; color: #333;">', 
                             symbol, '</span>',
                             '<span style="font-size: 12px;">', caller, '</span>',
                             '</div>')
    }
  }
  
  return(paste0(
    '<div style="background: white; padding: 10px; border: 1px solid #ddd; border-radius: 5px;">',
    '<div style="font-weight: bold; margin-bottom: 8px; font-size: 13px;">Variant Caller</div>',
    legend_items,
    '</div>'
  ))
}

# ============================================================================
# 5. DATA PROCESSING FOR TABLES
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