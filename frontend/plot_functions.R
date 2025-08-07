# ============================================================================
# plot_functions.R
# ============================================================================
"
Plot generation functions for SNV Benchmarking Dashboard.

Main components:
- SNP and INDEL performance scatter plots with F1 contours
- Stratified analysis bar plots with technology-caller gradients
- Plot height calculations for dynamic sizing
- Interactive plot configurations for Plotly integration
"

# ============================================================================
# PLOT OUTPUT SETUP FUNCTION
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
        "<b>",
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
          size = 2.5
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
        "<br><br><b>Setup:</b>",
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
          size = 2.5,
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