# ============================================================================
# plot_functions.R
# ============================================================================
"
Plot generation functions for SNV Benchmarking Dashboard.
Converts processed data into interactive ggplot/plotly visualizations.

Main plots:
- Performance scatter plots (Tab 3: SNP/INDEL precision vs recall with F1 contours)
- Stratified bar charts (Tab 4: performance across genomic regions)
- Plot legends (Tab 3: technology colors and caller shapes)

Helper functions:
- create_f1_contour() (calculates F1 benchmark lines for scatter plots)
- create_stratified_grouped_plot() (creates regional performance bar charts)
- create_technology_legend() / create_caller_legend() (HTML legend builders)
- setup_plot_outputs() (creates all plot outputs with interactivity)

"

# ============================================================================
# CORE PLOT FUNCTIONS
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
    labs(x = metric_label, y = "") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "#e9ecef", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.y = element_text(face = "bold", size = 9),
      axis.text.x = element_text(face = "bold", size = 9),
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(plot)
}

# ============================================================================
# VISUAL ELEMENTS & LEGENDS
# ============================================================================

# technology legend for Tab 3
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

# caller legend for tab 3
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
# PLOT OUTPUT HANDLERS
# ============================================================================

setup_plot_outputs <- function(input, output, session, data_reactives) {
  
  # ====================================================================
  # PERFORMANCE SCATTER PLOTS (TAB 3)
  # ====================================================================
  
  # SNP Performance Plot
  output$snp_plot <- renderPlotly({
    tryCatch({
      snp_data <- data_reactives$snp_plot_data()
      
      if (nrow(snp_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No SNP data available", size = 6) +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "SNP", x = "Precision", y = "Recall") +
          theme_bw()
        return(ggplotly(p))
      }
      
      contour <- create_f1_contour()
      
      # Create enhanced tooltip text with tech, platform, caller, and chemistry
      snp_data$tooltip_text <- paste(
        "<b>ID:", snp_data$experiment_id," - ",
        "<b>", ifelse(is.na(snp_data$experiment_name) | is.null(snp_data$experiment_name), "Unknown", snp_data$experiment_name), "</b>",
        "<br>",
        "<br>• Technology:", snp_data$technology,
        "<br>• Platform:", ifelse(is.na(snp_data$platform_name) | is.null(snp_data$platform_name), "N/A", snp_data$platform_name),
        "<br>• Caller:", snp_data$caller,
        "<br>• Chemistry:", ifelse(is.na(snp_data$chemistry_name) | is.null(snp_data$chemistry_name), "N/A", snp_data$chemistry_name),
        "<br><br><b>Performance:</b>",
        "<br>• Precision:", paste0(round(as.numeric(snp_data$precision)*100, 2), "%"),
        "<br>• Recall:", paste0(round(as.numeric(snp_data$recall)*100, 2), "%"),
        "<br>• F1 Score:", paste0(round(as.numeric(snp_data$f1_score) * 100, 2), "%")
      )
      
      # Create ggplot
      p <- ggplot() +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 6,
          color = "black", 
          alpha = 0.8,
          linewidth = 0.5
        ) +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 12,
          color = "gray40", 
          alpha = 0.6,
          linetype = "dotted",
          linewidth = 0.3
        ) +
        geom_point(
          data = snp_data, 
          aes(x = precision, y = recall, 
              fill = technology,
              shape = caller,
              text = tooltip_text,
              customdata = plot_id),
          color = "black",
          stroke = 0.15,
          size = 3 ###-----------------SNP PLOT POINT SIZE
        ) +
        scale_fill_manual(values = technology_colors) + 
        scale_shape_manual(values = caller_shapes) +       
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP", x = "Precision", y = "Recall") +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12),
          panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
          panel.grid.minor = element_line(color = "grey95", linewidth = 0.3),
          legend.position = "none"        
        )
      
      # Convert to plotly
      suppressWarnings({
        ggplotly(p, tooltip = "text", source = "snp_plot_isolated") %>%
          layout(showlegend = FALSE,
                 dragmode = "zoom",
                 hoverlabel = list(align = "left")
          ) %>%    
          event_register("plotly_click")
      })
      
    }, error = function(e) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error loading SNP data:", e$message), size = 4) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "SNP - Error", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    })
  })
  
  # INDEL Performance Plot
  output$indel_plot <- renderPlotly({
    tryCatch({
      indel_data <- data_reactives$indel_plot_data()
      
      if (nrow(indel_data) == 0) {
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No INDEL data available", size = 6) +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "INDEL", x = "Precision", y = "Recall") +
          theme_bw()
        return(ggplotly(p))
      }
      
      contour <- create_f1_contour()
      
      # Create enhanced tooltip text with tech, platform, caller, and chemistry
      indel_data$tooltip_text <- paste(
        "<b>ID:", indel_data$experiment_id," - ",
        "<b>", ifelse(is.na(indel_data$experiment_name) | is.null(indel_data$experiment_name), "Unknown", indel_data$experiment_name), "</b>",
        "</br>",
        "<br>• Technology:", indel_data$technology,
        "<br>• Platform:", ifelse(is.na(indel_data$platform_name) | is.null(indel_data$platform_name), "N/A", indel_data$platform_name),
        "<br>• Caller:", indel_data$caller,
        "<br>• Chemistry:", ifelse(is.na(indel_data$chemistry_name) | is.null(indel_data$chemistry_name), "N/A", indel_data$chemistry_name),
        "<br><br><b>Performance:</b>",
        "<br>• Precision:", paste0(round(as.numeric(indel_data$precision)*100, 2), "%"),
        "<br>• Recall:", paste0(round(as.numeric(indel_data$recall)*100, 2), "%"),
        "<br>• F1 Score:", paste0(round(as.numeric(indel_data$f1_score) * 100, 2), "%")
      )
      
      # Create ggplot
      p <- ggplot() +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 6,
          color = "black", 
          alpha = 0.8,
          linewidth = 0.5
        ) +
        geom_contour(
          data = contour, 
          aes(x = p, y = r, z = f1), 
          bins = 12,
          color = "gray40", 
          alpha = 0.6,
          linetype = "dotted",
          linewidth = 0.3
        ) +
        geom_point(
          data = indel_data, 
          aes(x = precision, y = recall, 
              fill = technology,
              shape = caller,
              text = tooltip_text,
              customdata = plot_id),
          color = "black",
          size = 3, ###-----------------INDEL PLOT POINT SIZE
          stroke = 0.15
        ) +
        scale_fill_manual(values = technology_colors) +   
        scale_shape_manual(values = caller_shapes) +       
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL", x = "Precision", y = "Recall") +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12),
          panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
          panel.grid.minor = element_line(color = "grey95", linewidth = 0.3),
          legend.position = "none"        
        )
      
      # Convert to plotly
      suppressWarnings({
        ggplotly(p, tooltip = "text", source = "indel_plot_isolated") %>%
          layout(showlegend = FALSE,
                 dragmode = "zoom",
                 hoverlabel = list(align = "left")
          ) %>%    
          event_register("plotly_click")
      })
      
    }, error = function(e) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Error loading INDEL data:", e$message), size = 4) +
        xlim(0, 1) + ylim(0, 1) +
        labs(title = "INDEL - Error", x = "Precision", y = "Recall") +
        theme_bw()
      return(ggplotly(p))
    })
  })
  
  # ====================================================================
  # STRATIFIED ANALYSIS PLOTS (TAB 4)
  # ====================================================================
  
  # SNP Stratified Plot
  output$stratified_snp_plot <- renderPlot({
    data <- data_reactives$stratified_filtered_data()
    create_stratified_grouped_plot(data, "SNP", input$selected_metric)
  }, height = function() {
    data <- data_reactives$stratified_filtered_data()
    if (nrow(data) == 0) return(400)
    
    n_regions <- length(unique(data$subset))
    n_experiments <- length(unique(data$experiment_id))
    
    # Use original calculation method
    base_height <- max(8, n_regions * 2 + n_experiments * 0.5)
    height_per_region <- 40 + (n_experiments * 16)
    
    return(base_height + (n_regions * height_per_region))
  })
  
  # INDEL Stratified Plot
  output$stratified_indel_plot <- renderPlot({
    data <- data_reactives$stratified_filtered_data()
    create_stratified_grouped_plot(data, "INDEL", input$selected_metric)
  }, height = function() {
    data <- data_reactives$stratified_filtered_data()
    if (nrow(data) == 0) return(400)
    
    n_regions <- length(unique(data$subset))
    n_experiments <- length(unique(data$experiment_id))
    
    # Use original calculation method
    base_height <- max(8, n_regions * 2 + n_experiments * 0.5)
    height_per_region <- 40 + (n_experiments * 16)
    
    return(base_height + (n_regions * height_per_region))
  })
  
  # ====================================================================
  # LEGEND OUTPUTS (TAB 3)
  # ====================================================================
  
  # Technology legend
  output$technology_legend <- renderUI({
    HTML(create_technology_legend())
  })
  
  # Caller legend
  output$caller_legend <- renderUI({
    HTML(create_caller_legend())
  })
}