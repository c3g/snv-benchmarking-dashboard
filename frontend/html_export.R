# html_export.R
# HTML Export Module for SNV Benchmarking Dashboard

# Helper functions for safe data handling
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

# Main HTML generation function
generate_benchmarking_report <- function(viz_data) {
  
  # Validate input
  if (is.null(viz_data) || nrow(viz_data) == 0) {
    return(generate_error_html("No data available for export"))
  }
  
  tryCatch({
    # Process data safely
    snp_data <- process_variant_data(viz_data, "SNP")
    indel_data <- process_variant_data(viz_data, "INDEL")
    experiment_ids <- unique(viz_data$experiment_id[!is.na(viz_data$experiment_id)])
    
    # Generate HTML sections
    html_header <- generate_html_header()
    html_summary <- generate_summary_section(viz_data, experiment_ids)
    html_snp_table <- generate_performance_table(snp_data, "SNP")
    html_indel_table <- generate_performance_table(indel_data, "INDEL")
    html_metadata <- generate_metadata_section(viz_data, experiment_ids)
    html_footer <- generate_html_footer()
    
    # Combine all sections
    paste0(html_header, html_summary, html_snp_table, html_indel_table, 
           html_metadata, html_footer)
    
  }, error = function(e) {
    generate_error_html(paste("Export error:", e$message))
  })
}

# Data processing functions
process_variant_data <- function(viz_data, variant_type) {
  viz_data %>% 
    filter(variant_type == !!variant_type) %>%
    mutate(f1_numeric = as.numeric(f1_score)) %>%
    filter(!is.na(f1_numeric)) %>%
    arrange(desc(f1_numeric))
}

# HTML section generators
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
        .metadata-grid {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 15px;
            margin-top: 15px;
        }
        .metadata-item { margin-bottom: 8px; font-size: 14px; }
        .metadata-item strong { color: #495057; }
        .footer {
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #dee2e6;
            color: #6c757d;
            font-size: 12px;
            text-align: center;
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
  callers <- unique(viz_data$caller_name[!is.na(viz_data$caller_name)])
  
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
  
  html_table <- paste0('
        <h2>', variant_type, ' Performance Results (Best to Worst F1 Score)</h2>
        <table>
            <thead>
                <tr>
                    <th>Rank</th>
                    <th>Experiment</th>
                    <th>Technology</th>
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
                    <td>', safe_value(row$experiment_name), '</td>
                    <td>', safe_value(row$technology), '</td>
                    <td>', safe_value(row$caller_name), '</td>
                    <td>', safe_percent(row$precision), '</td>
                    <td>', safe_percent(row$recall), '</td>
                    <td><strong>', safe_percent(row$f1_score), '</strong></td>
                </tr>')
  }
  
  paste0(html_table, '
            </tbody>
        </table>')
}

generate_metadata_section <- function(viz_data, experiment_ids) {
  html_metadata <- '<h2>Experiment Metadata</h2>'
  
  for (exp_id in experiment_ids) {
    exp_data <- viz_data[viz_data$experiment_id == exp_id, ]
    if (nrow(exp_data) > 0) {
      exp_data <- exp_data[1, ]
      
      html_metadata <- paste0(html_metadata, '
        <div class="metadata-section">
            <h3>Experiment ', exp_id, ': ', safe_value(exp_data$experiment_name), '</h3>
            <div class="metadata-grid">
                <div>
                    <h4>Sequencing Technology</h4>
                    <div class="metadata-item"><strong>Technology:</strong> ', safe_value(exp_data$technology), '</div>
                    <div class="metadata-item"><strong>Platform:</strong> ', safe_value(exp_data$platform_name), '</div>
                    <div class="metadata-item"><strong>Chemistry:</strong> ', safe_value(exp_data$chemistry_name), '</div>
                    <div class="metadata-item"><strong>Coverage:</strong> ', safe_coverage(exp_data$mean_coverage), '</div>
                </div>
                <div>
                    <h4>Analysis Pipeline</h4>
                    <div class="metadata-item"><strong>Variant Caller:</strong> ', safe_value(exp_data$caller_name), '</div>
                    <div class="metadata-item"><strong>Caller Version:</strong> ', safe_value(exp_data$caller_version), '</div>
                    <div class="metadata-item"><strong>Aligner:</strong> ', safe_value(exp_data$aligner_name), '</div>
                </div>
                <div>
                    <h4>Truth Set & Quality</h4>
                    <div class="metadata-item"><strong>Truth Set:</strong> ', safe_value(exp_data$truth_set_name), '</div>
                    <div class="metadata-item"><strong>Sample:</strong> ', safe_value(exp_data$truth_set_sample), '</div>
                    <div class="metadata-item"><strong>Reference:</strong> ', safe_value(exp_data$truth_set_reference), '</div>
                </div>
            </div>
        </div>')
    }
  }
  
  return(html_metadata)
}

generate_html_footer <- function() {
  paste0('
        <div class="footer">
            <p>Report generated by SNV Benchmarking Dashboard on ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
            <p>This report contains only the currently selected/filtered experiments</p>
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


