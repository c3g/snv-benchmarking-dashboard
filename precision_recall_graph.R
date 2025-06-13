
# Load libraries (same as original)
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
                  size = 1.6, box.padding = 0.1, point.padding = 0.125, segment.color = "grey50") +
  geom_point(data = viz_data_snp, aes(x = Precision, y = Recall, color = sample)) +
  scale_color_jama() +
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
                  size = 1.6, box.padding = 0.1, point.padding = 0.125, segment.color = "grey50") +
  geom_point(data = viz_data_indel, aes(x = Precision, y = Recall, color = sample)) +
  scale_color_jama() +
  xlab("Precision") + ylab("Recall") + ggtitle("INDEL") +
  theme(plot.title = element_text(size = 10))

# ============================================================================
# COMBINE AND PRINT
# ============================================================================

final_plot <- (gg_snp + gg_indel + plot_layout(guides = "collect")) & theme(legend.position = "bottom")

# Display
print(final_plot)