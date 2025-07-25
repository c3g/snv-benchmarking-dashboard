# html_export.R
# HTML Export Module for SNV Benchmarking Dashboard

library(ggplot2)
library(dplyr)
library(ggrepel)
library(base64enc)
library(ggforce)
library(geomtextpath) 

# =============================================================================
# 1. HELPER FUNCTIONS
# =============================================================================
# 1.1
# Safe data handling
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
# 2.1 Create F1 contour data
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

# -------------------------------------

# 2.2
# Calculate zoom limits based on data
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
# -------------------------------------

# 2.3
# Create single plot with zoom
create_zoomed_performance_plot <- function(data, variant_type) {
  if (nrow(data) == 0) { # No data
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = paste("No", variant_type, "data"), size = 6) +
             xlim(0, 1) + ylim(0, 1) +
             labs(title = variant_type, x = "Precision", y = "Recall") +
             theme_bw())
  }
  
  # Color and shape mappings
  technology_colors <- c(
    "ILLUMINA" = "#F8766D", "PACBIO" = "#C77CFF", 
    "ONT" = "#00BFC4", "MGI" = "#7CAE00", "Unknown" = "#E76BF3"
  )
  caller_shapes <- c(
    "DEEPVARIANT" = 16, "GATK" = 17, "CLAIR3" = 15, "Unknown" = 4
  )
  
  # Create contour data
  contour <- create_f1_contour()
  
  # Calculate zoom limits
  zoom_limits <- calculate_zoom_limits(data)
  
  # Create plot
  p <- ggplot() +
    geom_textcontour( # Contour 1
      data = contour, 
      aes(p, r, z = f1), 
      bins = 6, 
      size = 2, 
      alpha = 0.5, 
      straight = TRUE
    ) +
    geom_textcontour( # Contour 2
      data = contour, 
      aes(p, r, z = f1), 
      bins = 12, 
      linetype = 3, 
      size = 2, 
      alpha = 0.35, 
      straight = TRUE
    ) +
    geom_text_repel( # ID label
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
    geom_point( # Data points
      data = data, 
      aes(x = precision, y = recall, 
          color = technology,        # Color by technology
          shape = caller),      # Shape by caller
      size = 3.5, 
      alpha = 0.8,
      stroke = 1            
    ) +
    facet_zoom( # Zoomed-in view
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
# -------------------------------------

# 2.4
# Generate both SNP and INDEL plots
generate_zoomed_plot_images <- function(viz_data) {
  # Split data by variant type
  snp_data <- viz_data %>% filter(variant_type == "SNP")
  indel_data <- viz_data %>% filter(variant_type == "INDEL")
  
  snp_plot <- create_zoomed_performance_plot(snp_data, "SNP")
  indel_plot <- create_zoomed_performance_plot(indel_data, "INDEL")
  
  # Save plots as png
  temp_snp <- tempfile(fileext = ".png")
  temp_indel <- tempfile(fileext = ".png")
  ggsave(temp_snp, snp_plot, width = 12, height = 6, dpi = 300)
  ggsave(temp_indel, indel_plot, width = 12, height = 6, dpi = 300)
  # Convert to Base64 for HTML
  snp_base64 <- base64enc::base64encode(temp_snp) 
  indel_base64 <- base64enc::base64encode(temp_indel)
  
  file.remove(c(temp_snp, temp_indel)) # Remove extra files
  
  return(list(snp = snp_base64, indel = indel_base64))
}

# -------------------------------------

# 2.5
# Generate plot section in HTML
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
# 3. DATA PROCESSING FUNCTIONS
# =============================================================================

# Prepare all data
process_variant_data <- function(viz_data, variant_type) {
  viz_data %>% 
    filter(variant_type == !!variant_type) %>%
    mutate(f1_numeric = as.numeric(f1_score)) %>%
    filter(!is.na(f1_numeric)) %>%
    arrange(desc(f1_numeric))
}

# HTML section generators - section styles
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
            background-color: #f8f9fa; 
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
        .metadata-grid-4col {
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
        
        .metadata-card h4 i {
            margin-right: 8px;
            font-size: 18px;
            color: #007bff;
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
# Summary section
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

# Performance table section
generate_performance_table <- function(data, variant_type) {
  if (nrow(data) == 0) {
    return(paste0('
        <h2>', variant_type, ' Performance Results</h2>
        <p>No ', variant_type, ' data available</p>'))
  }
  
  html_table <- paste0('
        <h2>', variant_type, ' Performance Results (Best to Worst F1 Score)</h2>
        <table>
            <thead>
                <tr>
                    <th>Rank</th>
                    <th>ID</th>
                    <th>Experiment</th>
                    <th>Technology</th>
                    <th>Platform</th>
                    <th>Chemistry</th>
                    <th>Caller</th>
                    <th>Precision (%)</th>
                    <th>Recall (%)</th>
                    <th>F1 Score (%)</th>
                </tr>
            </thead>
            <tbody>')
  
  for (i in 1:nrow(data)) {
    row <- data[i, ]
    row_class <- if(i == 1) "best-performer" else ""
    
    html_table <- paste0(html_table, '
                <tr class="', row_class, '">
                    <td>', i, '</td>
                    <td><strong>', safe_value(row$experiment_id), '</strong></td>
                    <td>', safe_value(row$experiment_name), '</td>
                    <td>', safe_value(row$technology), '</td>
                    <td>', safe_value(row$platform_name), '</td>
                    <td>', safe_value(row$chemistry_name), '</td>
                    <td>', safe_value(row$caller), '</td>
                    <td>', safe_percent(row$precision), '</td>
                    <td>', safe_percent(row$recall), '</td>
                    <td><strong>', safe_percent(row$f1_score), '</strong></td>
                </tr>')
  }
  
  paste0(html_table, '
            </tbody>
        </table>')
}

# Metadata section
generate_metadata_section <- function(viz_data, experiment_ids) {
  html_metadata <- '<h2>Experiment Metadata</h2>'
  
  for (exp_id in experiment_ids) {
    exp_data <- viz_data[viz_data$experiment_id == exp_id, ]
    if (nrow(exp_data) > 0) {
      exp_data <- exp_data[1, ]
      
      html_metadata <- paste0(html_metadata, '
        <div class="metadata-section">
            <h3>Experiment ', exp_id, ': ', safe_value(exp_data$experiment_name), '</h3>
            
            <!-- 4-column semantic grid -->
            <div class="metadata-grid-4col">
                <!-- SEQUENCING PLATFORM -->
                <div class="metadata-card">
                    <h4><i class="icon-dna"></i> Sequencing Platform</h4>
                    <div class="metadata-item"><strong>Technology:</strong> ', safe_value(exp_data$technology), '</div>
                    <div class="metadata-item"><strong>Platform:</strong> ', safe_value(exp_data$platform_name), '</div>
                    <div class="metadata-item"><strong>Version:</strong> ', safe_value(exp_data$platform_version), '</div>
                    <div class="metadata-item"><strong>Type:</strong> ', safe_value(exp_data$platform_type), '</div>
                    <div class="metadata-item"><strong>Target:</strong> ', safe_value(exp_data$target), '</div>
                    <div class="metadata-item"><strong>Chemistry:</strong> ', safe_value(exp_data$chemistry_name), '</div>
                </div>
                
                <!-- ANALYSIS PIPELINE -->
                <div class="metadata-card">
                    <h4><i class="icon-pipeline"></i> Analysis Pipeline</h4>
                    <div class="metadata-item"><strong>Variant Caller:</strong> ', safe_value(exp_data$caller), '</div>
                    <div class="metadata-item"><strong>Caller Version:</strong> ', safe_value(exp_data$caller_version), '</div>
                    <div class="metadata-item"><strong>Caller Type:</strong> ', safe_value(exp_data$caller_type), '</div>
                    <div class="metadata-item"><strong>Caller Model:</strong> ', safe_value(exp_data$caller_model), '</div>
                    <div class="metadata-item"><strong>Aligner:</strong> ', safe_value(exp_data$aligner_name), ' ', safe_value(exp_data$aligner_version), '</div>
                    <div class="metadata-item"><strong>Benchmark Tool:</strong> ', safe_value(exp_data$benchmark_tool_name), ' ', safe_value(exp_data$benchmark_tool_version), '</div>
                </div>
                
                <!-- QUALITY METRICS -->
                <div class="metadata-card">
                    <h4><i class="icon-quality"></i> Quality Metrics</h4>
                    <div class="metadata-item"><strong>Mean Coverage:</strong> ', safe_coverage(exp_data$mean_coverage), '</div>
                    <div class="metadata-item"><strong>Read Length:</strong> ', 
                              ifelse(is.na(exp_data$read_length), 
                                     ifelse(is.na(exp_data$mean_read_length), "N/A", paste0(exp_data$mean_read_length, " bp (mean)")), 
                                     paste0(exp_data$read_length, " bp")), '</div>
                    <div class="metadata-item"><strong>Insert Size:</strong> ', 
                              ifelse(is.na(exp_data$mean_insert_size), "N/A", paste0(exp_data$mean_insert_size, " bp")), '</div>
                    <div class="metadata-item"><strong>Created:</strong> ', 
                              ifelse(is.na(exp_data$created_at), "N/A", format(as.POSIXct(exp_data$created_at), "%Y-%m-%d")), '</div>
                </div>
                
                <!-- VARIANT & TRUTH SET -->
                <div class="metadata-card">
                    <h4><i class="icon-truth"></i> Variants & Truth Set</h4>
                    <div class="metadata-item"><strong>Variant Type:</strong> ', safe_value(exp_data$variant_type), '</div>
                    <div class="metadata-item"><strong>Variant Origin:</strong> ', safe_value(exp_data$variant_origin), '</div>
                    <div class="metadata-item"><strong>Variant Size:</strong> ', safe_value(exp_data$variant_size), '</div>
                    <div class="metadata-item"><strong>Is Phased:</strong> ', 
                              ifelse(is.na(exp_data$is_phased), "N/A", ifelse(exp_data$is_phased, "Yes", "No")), '</div>
                    <div class="metadata-item"><strong>Truth Set:</strong> ', safe_value(exp_data$truth_set_name), ' ', safe_value(exp_data$truth_set_version), '</div>
                    <div class="metadata-item"><strong>Sample:</strong> ', safe_value(exp_data$truth_set_sample), '</div>
                    <div class="metadata-item"><strong>Reference:</strong> ', safe_value(exp_data$truth_set_reference), '</div>
                </div>
            </div>
        </div>')
    }
  }
  
  return(html_metadata)
}


 # Footer section
generate_html_footer <- function() {
  paste0('
        <div class="footer">
            <p>Report generated by SNV Benchmarking Dashboard on ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
        </div>
    </div>
</body>
</html>')
}

# Error message
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
# 4. MAIN REPORT GENERATION FUNCTION
# =============================================================================
generate_benchmarking_report <- function(viz_data) {
  
  if (is.null(viz_data) || nrow(viz_data) == 0) {
    return(generate_error_html("No data available for export"))
  }
  
  tryCatch({
    snp_data <- process_variant_data(viz_data, "SNP")
    indel_data <- process_variant_data(viz_data, "INDEL")
    experiment_ids <- unique(viz_data$experiment_id[!is.na(viz_data$experiment_id)])
    
    # Generate HTML sections
    html_header <- generate_html_header()
    html_summary <- generate_summary_section(viz_data, experiment_ids)
    html_plots <- generate_plots_section(viz_data)
    html_snp_table <- generate_performance_table(snp_data, "SNP")
    html_indel_table <- generate_performance_table(indel_data, "INDEL")
    html_metadata <- generate_metadata_section(viz_data, experiment_ids)
    html_footer <- generate_html_footer()
    
    # Combine all sections (plots come after summary, before tables)
    paste0(html_header, html_summary, html_plots, html_snp_table, 
           html_indel_table, html_metadata, html_footer)
    
  }, error = function(e) {
    generate_error_html(paste("Export error:", e$message))
  })
}

