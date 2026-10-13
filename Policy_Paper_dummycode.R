# =========================================================================
# Spatial Visualization and Statistical Analysis of US Artificial Reef Policies
# By: Nicholas Tucker
# Last Modified: 13 OCT 2026
# =========================================================================
# Note: The current iteration uses a dummy dataset. Column names and contents 
# are subject to change upon final data integration.
# =========================================================================

# -------------------------------------------------------------------------
# Required Packages & Libraries
# -------------------------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sf")) install.packages("sf")
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("cowplot")) install.packages("cowplot")
if (!require("rstatix")) install.packages("rstatix")
if (!require("vcd")) install.packages("vcd")
if (!require("MASS")) install.packages("MASS")

library(tidyverse)  # Data manipulation and visualization
library(sf)         # Spatial data processing
library(rnaturalearth) # Base geographic boundaries
library(cowplot)    # Multi-panel plot arrangement
library(rstatix)    # Cramer's V and contingency statistics
library(vcd)        # Association plots and categorical utilities
library(MASS)       # Log-linear modeling via loglm()

# -------------------------------------------------------------------------
# Load Dataset
# -------------------------------------------------------------------------
df <- read.csv("~/Desktop/artificial structures/Dummy_Data_Policy_Paper.csv")
colnames(df)

# =========================================================================
# FIGURE 1: Spatial Distribution of Policy Documents by Material Type
# =========================================================================

# 1. Clean Coordinates and Aggregate Document Counts by Location
pie_summary <- df %>%
  mutate(
    Lat = as.numeric(as.character(Lat)),
    Lon = as.numeric(as.character(Lon))
  ) %>%
  filter(!is.na(Lat) & !is.na(Lon)) %>%
  group_by(State, Lat, Lon, Material_Type) %>%
  summarise(count = n(), .groups = "drop")

# 2. Extract Base Map Boundaries (United States Medium Resolution)
us_states <- ne_states(country = "united states of america", returnclass = "sf")

# 3. Establish Palette Mapping for Material Categories
materials <- unique(pie_summary$Material_Type)
palette_colors <- RColorBrewer::brewer.pal(max(3, length(materials)), "Set2")
names(palette_colors) <- materials

# 4. Helper Function: Construct Individual Mini Pie Chart Grobs
make_pie_grob <- function(sub_df) {
  p <- ggplot(sub_df, aes(x = "", y = count, fill = Material_Type)) +
    geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.2) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = palette_colors) +
    theme_void() +
    theme(legend.position = "none")
  
  ggplotGrob(p)
}

# 5. Group Data, Calculate Total Sample Size (N), and Generate Grobs
nested_pies <- pie_summary %>%
  group_by(State, Lat, Lon) %>%
  summarise(
    total_n = sum(count),
    data = list(pick(everything())),
    .groups = "drop"
  ) %>%
  mutate(grob = map(data, make_pie_grob))

# 6. Helper Function: Construct Regional Map Panel with Overlaid Grobs
build_map_panel <- function(state_data, xlims, ylims, pie_size, title = NULL) {
  # Base geographic map layer
  p <- ggplot() +
    geom_sf(data = us_states, fill = "#e5e7eb", color = "#9ca3af", linewidth = 0.3) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    theme_minimal() +
    labs(title = title, x = NULL, y = NULL) +
    theme(
      panel.background = element_rect(fill = "#f0f9ff", color = NA),
      panel.grid = element_line(color = "white", linewidth = 0.2),
      plot.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 7)
    )
  
  # Filter points residing within panel geographic boundaries
  panel_pies <- state_data %>%
    filter(Lon >= xlims[1] & Lon <= xlims[2] & Lat >= ylims[1] & Lat <= ylims[2]) %>%
    mutate(
      custom_label = case_when(
        State == "National" ~ "National Level",
        State == "UNK"      ~ "Non-Specific Region",
        TRUE                ~ NA_character_
      )
    )
  
  if (nrow(panel_pies) > 0) {
    # Overlay pie chart grobs at respective coordinates
    for (i in seq_len(nrow(panel_pies))) {
      lon <- panel_pies$Lon[i]
      lat <- panel_pies$Lat[i]
      grob <- panel_pies$grob[[i]]
      
      p <- p + annotation_custom(
        grob = grob,
        xmin = lon - pie_size / 2, xmax = lon + pie_size / 2,
        ymin = lat - pie_size / 2, ymax = lat + pie_size / 2
      )
    }
    
    # Render sample size text labels (n=...) above pie charts
    p <- p + geom_text(
      data = panel_pies,
      aes(x = Lon, y = Lat + (pie_size * 0.75), label = paste0("n=", total_n)),
      size = 2.6,
      fontface = "bold",
      color = "#1f2937"
    )
    
    # Render custom regional descriptive titles ("National Level", "Non-Specific Region")
    p <- p + geom_text(
      data = panel_pies %>% filter(!is.na(custom_label)),
      aes(x = Lon, y = Lat + (pie_size * 1.35), label = custom_label),
      size = 3.0,
      fontface = "bold",
      color = "#000000"
    )
  }
  
  return(p)
}

# 7. Generate Individual Geographic Map Panels
map_conus <- build_map_panel(nested_pies, c(-125, -66), c(22, 50), pie_size = 2.5, title = "Contiguous U.S. & GOM")
map_alaska <- build_map_panel(nested_pies, c(-180, -130), c(51, 72), pie_size = 3.5, title = "Alaska")
map_hawaii <- build_map_panel(nested_pies, c(-161, -154), c(18, 23), pie_size = 0.8, title = "Hawaii")

# 8. Extract Standalone Legend Object
legend_data <- data.frame(Material_Type = factor(materials, levels = materials), x = 0, y = 0)
dummy_legend <- ggplot(legend_data, aes(x = x, y = y, fill = Material_Type)) +
  geom_tile() +
  scale_fill_manual(values = palette_colors, name = "Material Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 9)
  )

legend_grob <- cowplot::get_legend(dummy_legend)

# 9. Assemble Multi-Panel Compound Figure Layout
insets_column <- cowplot::plot_grid(map_alaska, map_hawaii, ncol = 1, rel_heights = c(1, 1))
main_plot <- cowplot::plot_grid(map_conus, insets_column, ncol = 2, rel_widths = c(2.3, 1))

final_figure_1 <- cowplot::plot_grid(
  cowplot::ggdraw() + cowplot::draw_label("Figure 1: Distribution of Artificial Reef Policies by Material Type", 
                                          fontface = 'bold', x = 0.02, hjust = 0, size = 14),
  main_plot,
  legend_grob,
  ncol = 1,
  rel_heights = c(0.08, 1, 0.1)
)

final_figure_1


# =========================================================================
# STATISTICAL ANALYSIS: Question 1 Policy Landscape Distributions
# =========================================================================

# 1. Format Factor Variables
df_stats <- df %>%
  mutate(
    Material_Type = factor(Material_Type),
    Doc_Type      = factor(Doc_Type),
    Spatial_Scale = factor(Spatial_Scale)
  )

# -------------------------------------------------------------------------
# STEP 1: Contingency Cross-Tabulations
# -------------------------------------------------------------------------
cat("\n================ CROSS-TABULATION: Material_Type x Doc_Type ================\n")
tab_mat_doc <- table(df_stats$Material_Type, df_stats$Doc_Type)
print(tab_mat_doc)

cat("\n================ CROSS-TABULATION: Material_Type x Spatial_Scale ================\n")
tab_mat_scale <- table(df_stats$Material_Type, df_stats$Spatial_Scale)
print(tab_mat_scale)

# -------------------------------------------------------------------------
# STEP 2: Fisher's Exact Tests (Monte Carlo Simulation for Low Cell Counts)
# -------------------------------------------------------------------------
cat("\n================ FISHER'S EXACT TEST: Material vs. Doc_Type ================\n")
fisher_doc <- fisher.test(tab_mat_doc, simulate.p.value = TRUE, B = 10000)
print(fisher_doc)

cat("\n================ FISHER'S EXACT TEST: Material vs. Spatial_Scale ================\n")
fisher_scale <- fisher.test(tab_mat_scale, simulate.p.value = TRUE, B = 10000)
print(fisher_scale)

# -------------------------------------------------------------------------
# STEP 3: Effect Size Measurement (Cramer's V)
# -------------------------------------------------------------------------
# Effect size conventions: ~0.1 (small), ~0.3 (medium), ~0.5 (large)
cat("\n================ CRAMER'S V: Material vs. Doc_Type ================\n")
v_doc <- cramer_v(tab_mat_doc)
print(v_doc)

cat("\n================ CRAMER'S V: Material vs. Spatial_Scale ================\n")
v_scale <- cramer_v(tab_mat_scale)
print(v_scale)

# -------------------------------------------------------------------------
# STEP 4: Log-Linear Models of Independence
# -------------------------------------------------------------------------
# ~ 1 + 2 evaluates the null hypothesis of mutual independence across dimensions
cat("\n================ LOG-LINEAR MODEL: Material_Type x Spatial_Scale ================\n")
loglm_scale_indep <- loglm(~ 1 + 2, data = tab_mat_scale)
print(loglm_scale_indep)

cat("\n================ LOG-LINEAR MODEL: Material_Type x Doc_Type ================\n")
loglm_doc_indep <- loglm(~ 1 + 2, data = tab_mat_doc)
print(loglm_doc_indep)

# -------------------------------------------------------------------------
# STEP 5: Association Mosaic Visualizations
# -------------------------------------------------------------------------
# Identifies specific cell-level deviations from expected independent values
assocplot(tab_mat_doc, main = "Association Plot: Material Type vs. Document Type",
          col = c("black", "red"))

assocplot(tab_mat_scale, main = "Association Plot: Material Type vs. Spatial Scale",
          col = c("black", "red"))