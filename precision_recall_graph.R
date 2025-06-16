# Load libraries
library(reticulate)
library(ggplot2)
library(dplyr)
library(ggsci)
library(ggrepel)
library(patchwork)
library(geomtextpath)

# Set theme
theme_set(theme_bw())

# ============================================================================
# DATA LOADING
# ============================================================================

# Import database interface
db <- import("db_interface")

# Get all experiments and their performance data
experiments_overview <- db$get_experiments_overview()
all_ids <- as.list(experiments_overview$id)
performance_data <- db$get_performance_results(all_ids, c('SNP', 'INDEL'))

# ============================================================================
# DATA PREPARATION
# ============================================================================

# Prepare data similar to original important_tbl
viz_data <- performance_data %>%
  filter(!is.na(recall) & !is.na(precision) & !is.na(f1_score)) %>%
  select(experiment_name, variant_type, recall, precision, f1_score) %>%
  rename(
    sample = experiment_name,
    type = variant_type,
    Precision = precision,
    Recall = recall,
    F1 = f1_score
  ) %>%
  mutate(types = factor(type, levels = c("SNP", "INDEL")))

print(paste("Loaded", nrow(viz_data), "records"))

# F1 calculation for drawing the contour reference lines
f1_contour_function <- function(p, r) 2 * (p * r) / (p + r)
contour <- expand.grid(p = seq(0, 1, by = 0.01), r = seq(0, 1, by = 0.01)) %>%
  mutate(f1 = f1_contour_function(p, r)) %>%
  filter(is.finite(f1))



# ============================================================================
# SNP PLOT
# ============================================================================

# Filter for SNP data
viz_data_snp <- viz_data %>% filter(types == "SNP")

# Create SNP plot (simplified - no zoom)
gg_snp <- ggplot() +
  geom_textcontour(data = contour, aes(p, r, z = f1), bins = 6, size = 2, alpha = 0.5, straight = TRUE) +
  geom_textcontour(data = contour, aes(p, r, z = f1), bins = 12, linetype = 3, size = 2, alpha = 0.35, straight = TRUE) +
  geom_text_repel(data = viz_data_snp,
                  aes(x = Precision, y = Recall, label = format(F1 * 100, digits = 4)),
                  size = 1.6, box.padding = 0.1, point.padding = 0.3, segment.color = "grey50",max.overlaps = 20) +
  geom_point(data = viz_data_snp, aes(x = Precision, y = Recall, color = sample)) +
  scale_color_jama() +
  #xlim(0.85, 1.0) +  # Zoom in on x-axis
  #ylim(0.85, 1.0) +  # Zoom in on y-axis
  xlab("Precision") + ylab("Recall") + ggtitle("SNP") +
  theme(plot.title = element_text(size = 10))

# ============================================================================
# INDEL PLOT
# ============================================================================

# Filter for INDEL data
viz_data_indel <- viz_data %>% filter(types == "INDEL")

# Create INDEL plot (simplified - no zoom)
gg_indel <- ggplot() +
  geom_textcontour(data = contour, aes(p, r, z = f1), bins = 6, size = 2, alpha = 0.5, straight = TRUE) +
  geom_textcontour(data = contour, aes(p, r, z = f1), bins = 12, linetype = 3, size = 2, alpha = 0.35, straight = TRUE) +
  geom_text_repel(data = viz_data_indel,
                  aes(x = Precision, y = Recall, label = format(F1 * 100, digits = 4)),
                  size = 1.6, box.padding = 0.1, point.padding = 0.3, segment.color = "grey50",max.overlaps = 20) +
  geom_point(data = viz_data_indel, aes(x = Precision, y = Recall, color = sample)) +
  scale_color_jama() +
  #xlim(0.85, 1.0) +  # Zoom in on x-axis
  #ylim(0.85, 1.0) +  # Zoom in on y-axis
  xlab("Precision") + ylab("Recall") + ggtitle("INDEL") +
  theme(plot.title = element_text(size = 10))

'# Calculate zoom ranges based on actual data
snp_data <- viz_data %>% filter(types == "SNP")
indel_data <- viz_data %>% filter(types == "INDEL")

# Define zoom ranges with some padding
if(nrow(snp_data) > 0) {
  snp_p_range <- range(snp_data$Precision, na.rm = TRUE)
  snp_r_range <- range(snp_data$Recall, na.rm = TRUE)
  
  # Add padding (2% on each side)
  snp_p_min <- max(0.8, snp_p_range[1] - 0.02)
  snp_p_max <- min(1.0, snp_p_range[2] + 0.02)
  snp_r_min <- max(0.8, snp_r_range[1] - 0.02)
  snp_r_max <- min(1.0, snp_r_range[2] + 0.02)
  
  print(paste("SNP zoom - Precision:", round(snp_p_min, 3), "to", round(snp_p_max, 3)))
  print(paste("SNP zoom - Recall:", round(snp_r_min, 3), "to", round(snp_r_max, 3)))
}

if(nrow(indel_data) > 0) {
  indel_p_range <- range(indel_data$Precision, na.rm = TRUE)
  indel_r_range <- range(indel_data$Recall, na.rm = TRUE)
  
  # Add padding (2% on each side)
  indel_p_min <- max(0.8, indel_p_range[1] - 0.02)
  indel_p_max <- min(1.0, indel_p_range[2] + 0.02)
  indel_r_min <- max(0.8, indel_r_range[1] - 0.02)
  indel_r_max <- min(1.0, indel_r_range[2] + 0.02)
  
  print(paste("INDEL zoom - Precision:", round(indel_p_min, 3), "to", round(indel_p_max, 3)))
  print(paste("INDEL zoom - Recall:", round(indel_r_min, 3), "to", round(indel_r_max, 3)))
}

# Create contour grid with higher resolution for zoomed areas
f1_contour_function <- function(p, r) 2 * (p * r) / (p + r)
contour <- expand.grid(p = seq(0, 1, by = 0.005), r = seq(0, 1, by = 0.005)) %>%
  mutate(f1 = f1_contour_function(p, r)) %>%
  filter(is.finite(f1))

# ============================================================================
# SNP PLOT (ZOOMED)
# ============================================================================

if(nrow(snp_data) > 0) {
  gg_snp <- ggplot() +
    geom_textcontour(data = contour, aes(p, r, z = f1), bins = 75, size = 3, alpha = 0.8, straight = TRUE, color = "black") +
    geom_textcontour(data = contour, aes(p, r, z = f1), bins = 140, linetype = 3, size = 2, alpha = 0.6, straight = TRUE, color = "gray40") +
    geom_text_repel(data = snp_data,
                    aes(x = Precision, y = Recall, label = format(F1 * 100, digits = 4)),
                    size = 2, box.padding = 0.2, point.padding = 0.3, segment.color = "grey50", max.overlaps = 20) +
    geom_point(data = snp_data, aes(x = Precision, y = Recall, color = sample), size = 3) +
    scale_color_jama() +
    xlim(snp_p_min, snp_p_max) +  # Zoom to data range
    ylim(snp_r_min, snp_r_max) +  # Zoom to data range
    xlab("Precision") + ylab("Recall") + ggtitle("SNP (Zoomed)") +
    theme(plot.title = element_text(size = 10))
} else {
  gg_snp <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "No SNP data", size = 6) +
    ggtitle("SNP (No Data)")
}

# ============================================================================
# INDEL PLOT (ZOOMED)
# ============================================================================

if(nrow(indel_data) > 0) {
  gg_indel <- ggplot() +
    geom_textcontour(data = contour, aes(p, r, z = f1), bins = 20, size = 3, alpha = 0.8, straight = TRUE, color = "black") +
    geom_textcontour(data = contour, aes(p, r, z = f1), bins = 40, linetype = 3, size = 2, alpha = 0.6, straight = TRUE, color = "gray40") +
    geom_text_repel(data = indel_data,
                    aes(x = Precision, y = Recall, label = format(F1 * 100, digits = 4)),
                    size = 2, box.padding = 0.2, point.padding = 0.3, segment.color = "grey50", max.overlaps = 20) +
    geom_point(data = indel_data, aes(x = Precision, y = Recall, color = sample), size = 3) +
    scale_color_jama() +
    xlim(indel_p_min, indel_p_max) +  # Zoom to data range
    ylim(indel_r_min, indel_r_max) +  # Zoom to data range
    xlab("Precision") + ylab("Recall") + ggtitle("INDEL (Zoomed)") +
    theme(plot.title = element_text(size = 10))
} else {
  gg_indel <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "No INDEL data", size = 6) +
    ggtitle("INDEL (No Data)")
}
'
# ============================================================================
# COMBINE AND PRINT
# ============================================================================

final_plot <- (gg_snp + gg_indel + plot_layout(guides = "collect")) & theme(legend.position = "bottom")

# Display
print(final_plot)