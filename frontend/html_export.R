# html_export.R
# HTML Export Module for SNV Benchmarking Dashboard

library(ggplot2)
library(dplyr)
library(ggrepel)
library(base64enc)
library(ggforce)
library(geomtextpath) 

source("constants.R")
source("utils.R")

# =============================================================================
# 1. HELPER FUNCTIONS
# =============================================================================

safe_percent <- function(x) {
  if (is.null(x) || is.na(x) || !is.numeric(x)) {
    return("N/A")
  }
  tryCatch({
    result <- as.numeric(x) * 100
    return(paste0(round(result, 2), "%"))
  }, error = function(e) {
    return("N/A")
  })
}

safe_value <- function(x, default = "N/A") {
  if (is.null(x) || is.na(x) || x == "") {
    return(default)
  }
  return(as.character(x))
}

safe_coverage <- function(x) {
  if (is.na(x) || is.null(x)) {
    return("N/A")
  }
  tryCatch({
    paste0(round(as.numeric(x), 1), "x")
  }, error = function(e) "N/A")
}

# =============================================================================
# 2. PLOT FUNCTIONS
# =============================================================================

create_f1_contour <- function() {
  f1_contour_function <- function(p, r) {
    result <- 2 * (p * r) / (p + r)
    result[!is.finite(result)] <- NA
    return(result)
  }
  
  contour_data <- expand.grid(p = seq(0, 1, by = 0.01), r = seq(0, 1, by = 0.01)) %>%
    mutate(f1 = f1_contour_function(p, r)) %>%
    filter(is.finite(f1))
  
  return(contour_data)
}

calculate_zoom_limits <- function(data) {
  if (nrow(data) == 0) {
    return(list(x_min = 0.98, x_max = 1.0, y_min = 0.98, y_max = 1.0))
  }
  
  padding <- 0.002
  x_range <- range(data$precision, na.rm = TRUE)
  y_range <- range(data$recall, na.rm = TRUE)
  
  return(list(
    x_min = max(0, x_range[1] - padding),
    x_max = min(1, x_range[2] + padding),
    y_min = max(0, y_range[1] - padding),
    y_max = min(1, y_range[2] + padding)
  ))
}

create_zoomed_performance_plot <- function(data, variant_type) {
  if (nrow(data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = paste("No", variant_type, "data"), size = 6) +
             xlim(0, 1) + ylim(0, 1) +
             labs(title = variant_type, x = "Precision", y = "Recall") +
             theme_bw())
  }
  
  
  contour <- create_f1_contour()
  zoom_limits <- calculate_zoom_limits(data)
  
  p <- ggplot() +
    geom_textcontour(
      data = contour, 
      aes(p, r, z = f1), 
      bins = 6, 
      size = 2, 
      alpha = 0.5, 
      straight = TRUE
    ) +
    geom_textcontour(
      data = contour, 
      aes(p, r, z = f1), 
      bins = 12, 
      linetype = 3, 
      size = 2, 
      alpha = 0.35, 
      straight = TRUE
    ) +
    geom_text_repel(
      data = data,
      aes(x = precision, y = recall, 
          label =  paste0("ID: ", experiment_id)),                 
      size = 3.5,                                 
      box.padding = 0.3,                         
      point.padding = 0.2,                       
      segment.color = "grey80",               
      color = "grey30",                         
      fontface = "bold",
      fill = alpha("white", 0.8),                 
      label.padding = unit(0.3, "lines"),       
      label.r = unit(0.25, "lines"),             
      min.segment.length = 0.1                   
    ) +
    geom_point(
      data = data, 
      aes(x = precision, y = recall, 
          color = technology,
          shape = caller),
      size = 3.5, 
      alpha = 0.8,
      stroke = 1            
    ) +
    facet_zoom(
      xlim = c(zoom_limits$x_min, zoom_limits$x_max),
      ylim = c(zoom_limits$y_min, zoom_limits$y_max)
    ) +
    scale_color_manual(values = technology_colors) +
    scale_shape_manual(values = caller_shapes) +
    labs(title = variant_type, x = "Precision", y = "Recall") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_line(color = "grey95", size = 0.3)
    )
  
  return(p)
}

generate_zoomed_plot_images <- function(viz_data) {
  snp_data <- viz_data %>% filter(variant_type == "SNP")
  indel_data <- viz_data %>% filter(variant_type == "INDEL")
  
  snp_plot <- create_zoomed_performance_plot(snp_data, "SNP")
  indel_plot <- create_zoomed_performance_plot(indel_data, "INDEL")
  
  temp_snp <- tempfile(fileext = ".png")
  temp_indel <- tempfile(fileext = ".png")
  ggsave(temp_snp, snp_plot, width = 12, height = 6, dpi = 300)
  ggsave(temp_indel, indel_plot, width = 12, height = 6, dpi = 300)
  
  snp_base64 <- base64enc::base64encode(temp_snp) 
  indel_base64 <- base64enc::base64encode(temp_indel)
  
  file.remove(c(temp_snp, temp_indel))
  
  return(list(snp = snp_base64, indel = indel_base64))
}

generate_plots_section <- function(viz_data) {
  if (nrow(viz_data) == 0) {
    return('<h2>Performance Visualizations</h2><p>No data available for visualization</p>')
  }
  
  plots <- generate_zoomed_plot_images(viz_data)
  
  html_plots <- paste0('
        <h2>Performance Visualizations</h2>
        <p style="color: #6c757d; margin-bottom: 30px;">
            Precision vs Recall scatter plots with F1-score contour lines. 
            Each plot shows both full view (left) and zoomed region (right).
        </p>
        
        <div style="margin-bottom: 40px;">
            <h3>SNP Performance</h3>
            <img src="data:image/png;base64,', plots$snp, '" 
                style="width: 100%; max-width: 1000px; border: 1px solid #dee2e6; border-radius: 5px;" />
        </div>
        
        <div style="margin-bottom: 40px;">
            <h3>INDEL Performance</h3>
            <img src="data:image/png;base64,', plots$indel, '" 
                 style="width: 100%; max-width: 1000px; border: 1px solid #dee2e6; border-radius: 5px;" />
        </div>')
  
  return(html_plots)
}

# =============================================================================
# 3. STRATIFIED PLOT FUNCTIONS
# =============================================================================

generate_stratified_plots <- function(stratified_data, selected_metric = "f1_score") {
  if (nrow(stratified_data) == 0) {
    return(list(snp = NULL, indel = NULL))
  }
  
  metric_label <- METRIC_LABELS[[selected_metric]] %||% "Metric"
  
  create_variant_plot <- function(variant_type) {
    plot_data <- stratified_data %>%
      filter(variant_type == !!variant_type) %>%
      mutate(
        tech_caller = paste(technology, caller, sep = "-"),
        exp_label = paste0("ID:", experiment_id, " (", technology, "-", caller, ")"),
        metric_value = !!sym(selected_metric)
      ) %>%
      arrange(subset, desc(experiment_id))
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = paste("No", variant_type, "data"), size = 6) +
               theme_minimal())
    }
    
    p <- ggplot(plot_data, aes(x = metric_value, y = exp_label)) + 
      geom_col(aes(fill = tech_caller), alpha = 0.85, width = 0.7) + 
      geom_text(aes(label = paste0(round(metric_value * 100, 2), "%")), 
                hjust = -0.1, size = 3.5, color = "black") +
      scale_fill_manual(values = tech_caller_colors, na.value = "gray70") +
      scale_x_continuous(limits = c(0, 1.1), labels = scales::percent_format()) +
      facet_wrap(~ subset, scales = "free_y", ncol = 1) +
      labs(x = metric_label, y = "Experiment", fill = "Technology + Caller") +
      theme_bw() +
      theme(
        strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
        strip.text = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 10),
        legend.position = "bottom",
        legend.text = element_text(size =10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      guides(fill = guide_legend(ncol = 4, byrow = TRUE))
    
    return(p)
  }
  
  snp_plot <- create_variant_plot("SNP")
  indel_plot <- create_variant_plot("INDEL")
  
  temp_snp <- tempfile(fileext = ".png")
  temp_indel <- tempfile(fileext = ".png")
  
  n_regions <- length(unique(stratified_data$subset))
  n_experiments <- length(unique(stratified_data$experiment_id))

  height_per_region <- 3
  height_per_experiment <- 0.4
  base_height <- 4
  
  plot_height <- base_height + (n_regions * height_per_region) + (n_experiments * height_per_experiment) 
  
  ggsave(temp_snp, snp_plot, width = 12, height = plot_height, dpi = 300)
  ggsave(temp_indel, indel_plot, width = 12, height = plot_height, dpi = 300)
  
  snp_base64 <- base64enc::base64encode(temp_snp)
  indel_base64 <- base64enc::base64encode(temp_indel)
  
  file.remove(c(temp_snp, temp_indel))
  
  return(list(snp = snp_base64, indel = indel_base64))
}

create_stratified_table <- function(stratified_data, selected_metric = "f1_score") {
  if (nrow(stratified_data) == 0) {
    return('<p>No data available for table</p>')
  }
  
  table_data <- stratified_data %>%
    select(experiment_id, experiment_name, technology, caller, variant_type, subset, recall, precision, f1_score) %>%
    mutate(
      across(c(recall, precision, f1_score), ~ round(.x * 100, 2))
    ) %>%
    arrange(experiment_id, variant_type, subset)
  
  html_table <- '
    <div style="margin-top: 30px;">
        <h3>Detailed Results Table</h3>
        <div style="overflow-x: auto;">
            <table style="width: 100%; border-collapse: collapse; margin: 20px 0; font-size: 13px;">
                <thead>
                    <tr style="background-color: #f8f9fa;">
                        <th style="padding: 8px; text-align: left; border-bottom: 2px solid #dee2e6; font-weight: bold;">ID</th>
                        <th style="padding: 8px; text-align: left; border-bottom: 2px solid #dee2e6; font-weight: bold;">Experiment</th>
                        <th style="padding: 8px; text-align: center; border-bottom: 2px solid #dee2e6; font-weight: bold;">Technology</th>
                        <th style="padding: 8px; text-align: center; border-bottom: 2px solid #dee2e6; font-weight: bold;">Caller</th>
                        <th style="padding: 8px; text-align: center; border-bottom: 2px solid #dee2e6; font-weight: bold;">Variant</th>
                        <th style="padding: 8px; text-align: left; border-bottom: 2px solid #dee2e6; font-weight: bold;">Region</th>
                        <th style="padding: 8px; text-align: right; border-bottom: 2px solid #dee2e6; font-weight: bold;">Recall (%)</th>
                        <th style="padding: 8px; text-align: right; border-bottom: 2px solid #dee2e6; font-weight: bold;">Precision (%)</th>
                        <th style="padding: 8px; text-align: right; border-bottom: 2px solid #dee2e6; font-weight: bold;">F1 Score (%)</th>
                    </tr>
                </thead>
                <tbody>'
  
  for (i in 1:nrow(table_data)) {
    row <- table_data[i, ]
    row_color <- if (row$variant_type == "SNP") "#fdf2f2" else "#f2f7fd"
    
    recall_style <- if (selected_metric == "recall") "background-color: #e3f2fd; font-weight: bold;" else ""
    precision_style <- if (selected_metric == "precision") "background-color: #e3f2fd; font-weight: bold;" else ""
    f1_style <- if (selected_metric == "f1_score") "background-color: #e3f2fd; font-weight: bold;" else ""
    
    html_table <- paste0(html_table, '
                    <tr style="background-color: ', row_color, ';">
                        <td style="padding: 6px; border-bottom: 1px solid #ddd;">', row$experiment_id, '</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd; font-size: 12px;">', safe_value(row$experiment_name), '</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd; text-align: center;">', safe_value(row$technology), '</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd; text-align: center;">', safe_value(row$caller), '</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd; text-align: center; font-weight: bold;">', row$variant_type, '</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd;">', safe_value(row$subset), '</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd; text-align: right; ', recall_style, '">', row$recall, '%</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd; text-align: right; ', precision_style, '">', row$precision, '%</td>
                        <td style="padding: 6px; border-bottom: 1px solid #ddd; text-align: right; ', f1_style, '">', row$f1_score, '%</td>
                    </tr>')
  }
  
  html_table <- paste0(html_table, '
                </tbody>
            </table>
        </div>
    </div>')
  
  return(html_table)
}

add_stratified_section <- function(stratified_data, selected_metric = "f1_score") {
  if (is.null(stratified_data) || nrow(stratified_data) == 0) {
    return('<h2>Stratified Analysis</h2><p>No stratified data available</p>')
  }
  
  n_experiments <- length(unique(stratified_data$experiment_id))
  n_regions <- length(unique(stratified_data$subset))
  n_results <- nrow(stratified_data)
  
  metric_labels <- list("f1_score" = "F1 Score", "precision" = "Precision", "recall" = "Recall")
  metric_label <- metric_labels[[selected_metric]] %||% "Performance Metric"
  
  html_content <- paste0('
    <h2>Stratified Performance Analysis</h2>
    <p style="color: #6c757d; margin-bottom: 20px;">
        Performance analysis across different genomic regions showing ', metric_label, ' scores.
        Analysis includes ', n_experiments, ' experiments across ', n_regions, ' genomic regions 
        (', n_results, ' total results).
    </p>
    
    <div style="background: #e9ecef; padding: 15px; margin-bottom: 20px; border-radius: 5px;">
        <strong>Analysis Summary:</strong> ', n_experiments, ' experiments x ', n_regions, ' regions = ', n_results, ' total results
    </div>')
  
  plots <- generate_stratified_plots(stratified_data, selected_metric)
  
  html_content <- paste0(html_content, '
    <div style="display: flex; gap: 20px; margin-bottom: 40px; align-items: flex-start;">
        <div style="flex: 1;">
            <h3 style="color: #d73027; font-weight: bold; text-align: center; margin-bottom: 15px;">SNP Performance by Region</h3>')
  
  if (!is.null(plots$snp)) {
    html_content <- paste0(html_content, '
            <img src="data:image/png;base64,', plots$snp, '" 
                style="width: 100%; border: 1px solid #dee2e6; border-radius: 5px;" />')
  } else {
    html_content <- paste0(html_content, '
            <div style="text-align: center; padding: 40px; background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;">
                <p style="color: #6c757d;">No SNP data available</p>
            </div>')
  }
  
  html_content <- paste0(html_content, '
        </div>
        <div style="flex: 1;">
            <h3 style="color: #4575b4; font-weight: bold; text-align: center; margin-bottom: 15px;">INDEL Performance by Region</h3>')
  
  if (!is.null(plots$indel)) {
    html_content <- paste0(html_content, '
            <img src="data:image/png;base64,', plots$indel, '" 
                 style="width: 100%; border: 1px solid #dee2e6; border-radius: 5px;" />')
  } else {
    html_content <- paste0(html_content, '
            <div style="text-align: center; padding: 40px; background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;">
                <p style="color: #6c757d;">No INDEL data available</p>
            </div>')
  }
  
  html_content <- paste0(html_content, '
        </div>
    </div>')
  
  
  # Add the data table at the bottom
  html_content <- paste0(html_content, create_stratified_table(stratified_data, selected_metric))
  
  return(html_content)
}
# =============================================================================
# 4. DATA PROCESSING FUNCTIONS
# =============================================================================

process_variant_data <- function(viz_data, variant_type) {
  viz_data %>% 
    filter(variant_type == !!variant_type) %>%
    mutate(f1_numeric = as.numeric(f1_score)) %>%
    filter(!is.na(f1_numeric)) %>%
    arrange(desc(f1_numeric))
}
# =============================================================================
# HTML CODE
# =============================================================================

generate_html_header <- function() {
  '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>SNV Benchmarking Report</title>
    <style>
        body { 
            font-family: Arial, sans-serif; 
            margin: 40px; 
            line-height: 1.6; 
            background-color: #ffffff;
        }
        .container { max-width: 1200px; margin: 0 auto; }
        h1 { 
            color: #007bff; 
            text-align: center; 
            border-bottom: 3px solid #007bff;
            padding-bottom: 10px;
            margin-bottom: 30px;
        }
        h2 { 
            color: #495057; 
            margin-top: 40px;
            border-bottom: 2px solid #dee2e6;
            padding-bottom: 8px;
        }
        .summary { 
            background: #f8f9fa; 
            padding: 20px; 
            border-radius: 5px; 
            margin: 20px 0; 
            border-left: 4px solid #007bff;
        }
        table { 
            width: 100%; 
            border-collapse: collapse; 
            margin: 20px 0; 
            font-size: 14px;
        }
        th, td { 
            padding: 10px; 
            text-align: left; 
            border-bottom: 1px solid #ddd; 
        }
        th { 
            background-color: #e3f2fd; 
            font-weight: bold;
            color: #495057;
        }
        tr:nth-child(even) { background-color: #f8f9fa; }
        .best-performer { 
            background-color: #d4edda !important; 
            font-weight: bold;
        }
        .metadata-section {
            background-color: #f8f9fa;
            padding: 20px;
            margin: 20px 0;
            border-radius: 5px;
            border-left: 4px solid #007bff;
        }
        .metadata-grid-6col {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
            gap: 20px;
            margin-top: 15px;
        }
        .metadata-card {
            background: #ffffff;
            border: 1px solid #dee2e6;
            border-radius: 8px;
            padding: 20px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            transition: box-shadow 0.3s ease;
        }
        .metadata-card:hover {
            box-shadow: 0 4px 8px rgba(0,0,0,0.15);
        }
        .metadata-card h4 {
            color: #495057;
            margin: 0 0 15px 0;
            font-size: 16px;
            font-weight: 600;
            border-bottom: 2px solid #007bff;
            padding-bottom: 8px;
            display: flex;
            align-items: center;
        }
        .error-message {
            background: #f8d7da;
            color: #721c24;
            padding: 20px;
            border-radius: 5px;
            border-left: 4px solid #dc3545;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>SNV Benchmarking Report</h1>'
}
generate_summary_section <- function(viz_data, experiment_ids) {
  technologies <- unique(viz_data$technology[!is.na(viz_data$technology)])
  callers <- unique(viz_data$caller[!is.na(viz_data$caller)])
  paste0('
      <p style="text-align: center; color: #6c757d; margin-bottom: 30px;">
          Generated on ', format(Sys.time(), "%B %d, %Y at %H:%M"), '<br>
          Total Experiments: ', length(experiment_ids), '
      </p>
      
      <div class="summary">
          <h3>Export Summary</h3>
          <ul>
              <li><strong>Total Experiments:</strong> ', length(experiment_ids), '</li>
              <li><strong>Technologies:</strong> ', paste(technologies, collapse = ", "), '</li>
              <li><strong>Variant Callers:</strong> ', paste(callers, collapse = ", "), '</li>
          </ul>
      </div>')
}

generate_performance_table <- function(data, variant_type) {
  if (nrow(data) == 0) {
    return(paste0('
        <h2>', variant_type, ' Performance Results</h2>
        <p>No ', variant_type, ' data available</p>'))
  }
  
  data <- data %>% arrange(experiment_id)
  
  html_table <- paste0('
        <h2>', variant_type, ' Performance Results</h2>
        <table>
            <thead>
                <tr>
                    <th>ID</th>
                    <th>Experiment</th>
                    <th>Technology</th>
                    <th>Platform</th>
                    <th>Chemistry</th>
                    <th>Caller</th>
                    <th>Version</th>
                    <th>Coverage</th>
                    <th>Precision (%)</th>
                    <th>Recall (%)</th>
                    <th>F1 Score (%)</th>
                </tr>
            </thead>
            <tbody>')
  
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    
    html_table <- paste0(html_table, '
                <tr>
                    <td>', safe_value(row$experiment_id), '</td>
                    <td>', safe_value(row$experiment_name), '</td>
                    <td>', safe_value(row$technology), '</td>
                    <td>', safe_value(row$platform_name), '</td>
                    <td>', safe_value(row$chemistry_name), '</td>
                    <td>', safe_value(row$caller), '</td>
                    <td>', safe_value(row$caller_version), '</td>
                    <td>', safe_coverage(row$mean_coverage), '</td>
                    <td>', safe_percent(row$precision), '</td>
                    <td>', safe_percent(row$recall), '</td>
                    <td>', safe_percent(row$f1_score), '</td>
                </tr>')
  }
  
  paste0(html_table, '
            </tbody>
        </table>')
}

# =============================================================================
# ENHANCED METADATA GENERATION WITH 6 CARDS
# =============================================================================

generate_metadata_section <- function(viz_data, experiment_ids) {
  html_metadata <- '<h2>Experiment Metadata</h2>'
  
  for (exp_id in experiment_ids) {
    exp_data <- viz_data[viz_data$experiment_id == exp_id, ]
    if (nrow(exp_data) > 0) {
      exp_data <- exp_data[1, ]
      
      # Get performance data for this specific experiment (SNP and INDEL)
      snp_perf <- viz_data %>% filter(experiment_id == exp_id, variant_type == "SNP")
      indel_perf <- viz_data %>% filter(experiment_id == exp_id, variant_type == "INDEL")
      
      html_metadata <- paste0(html_metadata, '
        <div class="metadata-section">
            <h3>Experiment ', exp_id, ': ', safe_value(exp_data$experiment_name), '</h3>
            
            <div class="metadata-grid-6col">
                
                <!-- CARD 1: Sequencing Technology & Platform -->
                <div class="metadata-card">
                    <h4>Sequencing Technology</h4>
                    <div class="metadata-item"><strong>Technology:</strong> ', safe_value(exp_data$technology), '</div>
                    <div class="metadata-item"><strong>Platform:</strong> ', safe_value(exp_data$platform_name), '</div>
                    <div class="metadata-item"><strong>Platform Type:</strong> ', safe_value(exp_data$platform_type), '</div>
                    <div class="metadata-item"><strong>Platform Version:</strong> ', safe_value(exp_data$platform_version), '</div>
                    <div class="metadata-item"><strong>Target:</strong> ', safe_value(exp_data$target), '</div>
                    <div class="metadata-item"><strong>Chemistry:</strong> ', safe_value(exp_data$chemistry_name), '</div>
                    <div class="metadata-item"><strong>Chemistry Version:</strong> ', safe_value(exp_data$chemistry_version), '</div>
                </div>
                
                <!-- CARD 2: Analysis Pipeline -->
                <div class="metadata-card">
                    <h4>Analysis Pipeline</h4>
                    <div class="metadata-item"><strong>Variant Caller:</strong> ', safe_value(exp_data$caller), '</div>
                    <div class="metadata-item"><strong>Caller Version:</strong> ', safe_value(exp_data$caller_version), '</div>
                    <div class="metadata-item"><strong>Caller Type:</strong> ', safe_value(exp_data$caller_type), '</div>
                    <div class="metadata-item"><strong>Caller Model:</strong> ', safe_value(exp_data$caller_model), '</div>
                    <div class="metadata-item"><strong>Aligner:</strong> ', safe_value(exp_data$aligner_name), ' ', safe_value(exp_data$aligner_version), '</div>
                    <div class="metadata-item"><strong>Benchmark Tool:</strong> ', safe_value(exp_data$benchmark_tool_name), ' ', safe_value(exp_data$benchmark_tool_version), '</div>
                </div>
                
                <!-- CARD 3: Quality Metrics -->
                <div class="metadata-card">
                    <h4>Quality Metrics</h4>
                    <div class="metadata-item"><strong>Mean Coverage:</strong> ', safe_coverage(exp_data$mean_coverage), '</div>
                    <div class="metadata-item"><strong>Read Length:</strong> ', 
                              ifelse(is.na(exp_data$read_length), 
                                     ifelse(is.na(exp_data$mean_read_length), "N/A", paste0(safe_value(exp_data$mean_read_length), " bp (mean)")), 
                                     paste0(safe_value(exp_data$read_length), " bp")), '</div>
                    <div class="metadata-item"><strong>Insert Size:</strong> ', 
                              ifelse(is.na(exp_data$mean_insert_size), "N/A", paste0(safe_value(exp_data$mean_insert_size), " bp")), '</div>
                    <div class="metadata-item"><strong>Quality Score:</strong> ', safe_value(exp_data$read_quality, "N/A"), '</div>
                    <div class="metadata-item"><strong>Max Aligned Read:</strong> ', safe_value(exp_data$max_aligned_read, "N/A"), '</div>
                </div>
                
                <!-- CARD 4: Variants & Truth Set -->
                <div class="metadata-card">
                    <h4>Variants & Truth Set</h4>
                    <div class="metadata-item"><strong>Variant Type:</strong> ', safe_value(exp_data$variant_type_detail), '</div>
                    <div class="metadata-item"><strong>Variant Origin:</strong> ', safe_value(exp_data$variant_origin), '</div>
                    <div class="metadata-item"><strong>Variant Size:</strong> ', safe_value(exp_data$variant_size), '</div>
                    <div class="metadata-item"><strong>Is Phased:</strong> ', 
                              ifelse(is.na(exp_data$is_phased), "N/A", ifelse(exp_data$is_phased, "Yes", "No")), '</div>
                    <div class="metadata-item"><strong>Truth Set:</strong> ', safe_value(exp_data$truth_set_name), ' ', safe_value(exp_data$truth_set_version), '</div>
                    <div class="metadata-item"><strong>Sample:</strong> ', safe_value(exp_data$truth_set_sample), '</div>
                    <div class="metadata-item"><strong>Reference:</strong> ', safe_value(exp_data$truth_set_reference), '</div>
                </div>
                
                <!-- CARD 5: SNP Performance Summary -->
                <div class="metadata-card">
                    <h4>SNP Performance</h4>')
      
      if (nrow(snp_perf) > 0) {
        html_metadata <- paste0(html_metadata, '
                    <div class="metadata-item"><strong>F1 Score:</strong> <span class="metric-highlight">', safe_percent(snp_perf$f1_score[1]), '</span></div>
                    <div class="metadata-item"><strong>Precision:</strong> ', safe_percent(snp_perf$precision[1]), '</div>
                    <div class="metadata-item"><strong>Recall:</strong> ', safe_percent(snp_perf$recall[1]), '</div>
                    <div class="metadata-item"><strong>True Positives:</strong> ', format(snp_perf$truth_tp[1], big.mark = ","), '</div>
                    <div class="metadata-item"><strong>False Negatives:</strong> ', format(snp_perf$truth_fn[1], big.mark = ","), '</div>
                    <div class="metadata-item"><strong>False Positives:</strong> ', format(snp_perf$query_fp[1], big.mark = ","), '</div>
                    <div class="metadata-item"><strong>Total Truth:</strong> ', format(snp_perf$truth_total[1], big.mark = ","), '</div>')
      } else {
        html_metadata <- paste0(html_metadata, '
                    <div class="metadata-item" style="color: #6c757d; font-style: italic;">No SNP performance data available</div>')
      }
      
      html_metadata <- paste0(html_metadata, '
                </div>
                
                <!-- CARD 6: INDEL Performance Summary -->
                <div class="metadata-card">
                    <h4>INDEL Performance</h4>')
      
      if (nrow(indel_perf) > 0) {
        html_metadata <- paste0(html_metadata, '
                    <div class="metadata-item"><strong>F1 Score:</strong> <span class="metric-highlight">', safe_percent(indel_perf$f1_score[1]), '</span></div>
                    <div class="metadata-item"><strong>Precision:</strong> ', safe_percent(indel_perf$precision[1]), '</div>
                    <div class="metadata-item"><strong>Recall:</strong> ', safe_percent(indel_perf$recall[1]), '</div>
                    <div class="metadata-item"><strong>True Positives:</strong> ', format(indel_perf$truth_tp[1], big.mark = ","), '</div>
                    <div class="metadata-item"><strong>False Negatives:</strong> ', format(indel_perf$truth_fn[1], big.mark = ","), '</div>
                    <div class="metadata-item"><strong>False Positives:</strong> ', format(indel_perf$query_fp[1], big.mark = ","), '</div>
                    <div class="metadata-item"><strong>Total Truth:</strong> ', format(indel_perf$truth_total[1], big.mark = ","), '</div>')
      } else {
        html_metadata <- paste0(html_metadata, '
                    <div class="metadata-item" style="color: #6c757d; font-style: italic;">No INDEL performance data available</div>')
      }
      
      html_metadata <- paste0(html_metadata, '
                </div>
                
            </div> <!-- End metadata-grid-6col -->
        </div> <!-- End metadata-section -->')
    }
  }
  
  return(html_metadata)
}

generate_html_footer <- function() {
  paste0('
        <div class="footer">
            <p>Report generated by SNV Benchmarking Dashboard on ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
        </div>
    </div>
</body>
</html>')
}

generate_error_html <- function(error_message) {
  paste0('<!DOCTYPE html>
<html>
<head><title>Export Error</title></head>
<body>
    <div class="container">
        <div class="error-message">
            <h1>Export Error</h1>
            <p>', error_message, '</p>
        </div>
    </div>
</body>
</html>')
}

# =============================================================================
# 5. MAIN REPORT GENERATION FUNCTIONS
# =============================================================================

generate_benchmarking_report <- function(viz_data) {
  if (is.null(viz_data) || nrow(viz_data) == 0) {
    return(generate_error_html("No data available for export"))
  }
  
  tryCatch({
    snp_data <- process_variant_data(viz_data, "SNP")
    indel_data <- process_variant_data(viz_data, "INDEL")
    experiment_ids <- unique(viz_data$experiment_id[!is.na(viz_data$experiment_id)])
    
    html_header <- generate_html_header()
    html_summary <- generate_summary_section(viz_data, experiment_ids)
    html_plots <- generate_plots_section(viz_data)
    html_snp_table <- generate_performance_table(snp_data, "SNP")
    html_indel_table <- generate_performance_table(indel_data, "INDEL")
    html_metadata <- generate_metadata_section(viz_data, experiment_ids)
    html_footer <- generate_html_footer()
    
    paste0(html_header, html_summary, html_plots, html_snp_table, 
           html_indel_table, html_metadata, html_footer)
    
  }, error = function(e) {
    generate_error_html(paste("Export error:", e$message))
  })
}

create_html_report <- function(viz_data, stratified_data = NULL, selected_metric = "f1_score") {
  if (is.null(viz_data) || nrow(viz_data) == 0) {
    return(generate_error_html("No data available for export"))
  }
  
  tryCatch({
    snp_data <- process_variant_data(viz_data, "SNP")
    indel_data <- process_variant_data(viz_data, "INDEL")
    experiment_ids <- unique(viz_data$experiment_id[!is.na(viz_data$experiment_id)])
    
    html_header <- generate_html_header()
    html_summary <- generate_summary_section(viz_data, experiment_ids)
    html_plots <- generate_plots_section(viz_data)
    html_snp_table <- generate_performance_table(snp_data, "SNP")
    html_indel_table <- generate_performance_table(indel_data, "INDEL")
    
    html_stratified <- ""
    if (!is.null(stratified_data) && nrow(stratified_data) > 0) {
      html_stratified <- add_stratified_section(stratified_data, selected_metric)
    }
    
    html_metadata <- generate_metadata_section(viz_data, experiment_ids)
    html_footer <- generate_html_footer()
    
    paste0(html_header, html_summary, html_plots, html_snp_table, 
           html_indel_table, html_stratified, html_metadata, html_footer)
    
  }, error = function(e) {
    generate_error_html(paste("Export error:", e$message))
  })
}