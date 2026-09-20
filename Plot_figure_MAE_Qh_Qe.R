library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggtext)   # for colored axis labels

# Set working directory
DIR = dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(DIR)

df_mae_Qle <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/MAE_tables/df_mae_Qle3_corrected.csv")
df_mae_Qh  <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/MAE_tables/df_mae_Qh3_corrected.csv")

# ----- Station list -----
stations <- c("AU-Preston", "AU-SurreyHills", "CA-Sunset", "FI-Kumpula", "FI-Torni",
              "FR-Capitole", "GR-Heckor", "JP-Yoyogi", "KR-Jungnang", "KR-Ochang",
              "MX-Escandon", "NL-Amsterdam", "PL-Lipowa", "PL-Narutowicza",
              "SG-TelokKurau", "UK-KingsCollege", "UK-Swindon", "US-Baltimore",
              "US-Minneapolis1", "US-Minneapolis2", "US-WestPhoenix")

# ----- Build MAE df -----
df_mae <- data.frame(
  Site = stations,
  Qle_current  = df_mae_Qle$current,
  Qle_hcc_hyd = df_mae_Qle$hcc_hyd,
  Qh_current  = df_mae_Qh$current,
  Qh_hcc_hyd  = df_mae_Qh$hcc_hyd
)

# Get rid of underscore to fix bug
colnames(df_mae) <- c("Site", "Qle_current", "Qle_hcchyd", "Qh_current", "Qh_hcchyd")

# ----- Impervious % and label order -----
impervious_percentages <- c(
  "KR-Jungnang" = 96.5, "MX-Escandon" = 94, "JP-Yoyogi" = 92, "GR-Heckor" = 91.6, "FR-Capitole" = 90,
  "SG-TelokKurau" = 85, "UK-KingsCollege" = 79, "FI-Torni" = 77, "PL-Lipowa" = 76, "CA-Sunset" = 68,
  "NL-Amsterdam" = 68, "PL-Narutowicza" = 65, "AU-Preston" = 62, "AU-SurreyHills" = 54,
  "UK-Swindon" = 49, "US-WestPhoenix" = 48, "KR-Ochang" = 47, "FI-Kumpula" = 46,
  "US-Baltimore" = 31.3, "US-Minneapolis1" = 21, "US-Minneapolis2" = 5
)

impervious_order <- names(impervious_percentages)
impervious_order_labelled <- paste0(impervious_order, " (", impervious_percentages, "%)")

station_colors <- c(
  "AU-Preston (62%)"       = "#44AA99",
  "AU-SurreyHills (54%)"   = "#44AA99",
  "CA-Sunset (68%)"        = "#44AA99",
  "FI-Kumpula (46%)"       = "#882255",
  "FI-Torni (77%)"         = "#882265",
  "FR-Capitole (90%)"      = "#44AA99",
  "GR-Heckor (91.6%)"      = "#44AA99",
  "JP-Yoyogi (92%)"        = "#44AA99",
  "KR-Jungnang (96.5%)"    = "#882255",
  "KR-Ochang (47%)"        = "#882255",
  "MX-Escandon (94%)"      = "#44AA99",
  "NL-Amsterdam (68%)"     = "#44AA99",
  "PL-Lipowa (76%)"        = "#882255",
  "PL-Narutowicza (65%)"   = "#882255",
  "SG-TelokKurau (85%)"    = "#332288",
  "UK-KingsCollege (79%)"  = "#44AA99",
  "UK-Swindon (49%)"       = "#44AA99",
  "US-Baltimore (31.3%)"   = "#44AA99",
  "US-WestPhoenix (48%)"   = "#DDCC77",
  "US-Minneapolis1 (21%)"  = "#882255",
  "US-Minneapolis2 (5%)"   = "#882255"
)

# Helper to wrap axis labels in a colored <span>
station_labeler <- function(labels) {
  sapply(labels, function(x) {
    col <- station_colors[[x]]
    if (is.null(col)) col <- "black"
    paste0("<span style='color:", col, "'>", x, "</span>")
  })
}

# ----- Prepare data for plot -----
plot_data <- df_mae %>%
  pivot_longer(cols = -Site, names_to = c("Flux", "Surface"), names_sep = "_") %>%
  filter(!is.na(value)) %>%
  mutate(
    Site = factor(Site, levels = impervious_order),
    Site_label = factor(
      paste0(Site, " (", impervious_percentages[as.character(Site)], "%)"),
      levels = impervious_order_labelled
    ),
    color_group = paste(Flux, Surface, sep = ".")
  )

# Add Urban-PLUMBER reference points
plumber_points <- data.frame(
  Site = "AU-Preston",
  Site_label = factor("AU-Preston (62%)", levels = impervious_order_labelled),
  Flux = c("Qle", "Qh"),
  Surface = c("urban_plumber", "urban_plumber"),
  value = c(24, 30),
  color_group = c("Qle.urban_plumber", "Qh.urban_plumber")
)

plot_data <- bind_rows(plot_data, plumber_points)

# Background bands
n_sites <- length(impervious_order)

band_data <- data.frame(
  xmin = seq(0.5, n_sites - 0.5, by = 1),
  xmax = seq(1.5, n_sites + 0.5, by = 1),
  fill = rep(c("gray81", "white"), length.out = n_sites)
)

# Group means
mean_values <- plot_data %>%
  filter(Surface != "urban_plumber") %>%
  group_by(Flux, Surface) %>%
  summarise(mean_mae = mean(value, na.rm = TRUE), .groups = "drop")

# Aesthetics
flux_colors <- c(
  "Qle.current" = "#E69F00", "Qle.hcchyd" = "#CC79A7",
  "Qh.current"  = "#E69F00", "Qh.hcchyd"  = "#CC79A7",
  "Qle.urban_plumber" = "black", "Qh.urban_plumber" = "black"
)

linetypes <- c(
  "Qle.current" = "dashed", "Qle.hcchyd" = "dashed",
  "Qh.current"  = "dotted", "Qh.hcchyd"  = "dotted"
)

legend_labels_color <- c(
  "Qle.current" = "LE (Current)",
  "Qle.hcchyd" = "LE (HCC_HYD)",
  "Qle.urban_plumber" = "LE (Urban-PLUMBER)",
  "Qh.current" = "H (Current)",
  "Qh.hcchyd" = "H (HCC_HYD)",
  "Qh.urban_plumber" = "H (Urban-PLUMBER)"
)


fixed_theme <- theme(
  axis.text.x = element_markdown(angle = 60, hjust = 1, size = 12),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "gray80"),
  legend.position = "bottom",
  legend.box = "vertical",
  legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0), # Pulls legend up toward the plot
  legend.margin     = margin(t = 0, r = 0, b = 0, l = 0),   # Removes inner margin around legend
  legend.spacing.y  = unit(0.2, "cm")
)

# ----- Qle only -----
plot_data_qle <- plot_data %>% filter(Flux == "Qle")
mean_values_qle <- mean_values %>% filter(Flux == "Qle")

mae_plot_qle <- ggplot() +
  geom_rect(
    data = band_data,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0,
      ymax = 60,
      fill = fill
    ),
    alpha = 0.2
  ) +
  scale_fill_identity() +
  geom_hline(
    data = mean_values_qle,
    aes(
      yintercept = mean_mae,
      color = paste(Flux, Surface, sep = "."),
      linetype = paste(Flux, Surface, sep = ".")
    ),
    linewidth = 1,
    alpha = 0.8,
    show.legend = TRUE
  ) +
  geom_point(
    data = plot_data_qle,
    aes(
      x = Site_label,
      y = value,
      color = color_group,
      shape = Flux
    ),
    size = 3.5,
    position = position_dodge(width = 0.6)
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    labels = station_labeler
  ) +
  scale_color_manual(
    values = flux_colors[c("Qle.current", "Qle.hcchyd", "Qle.urban_plumber")],
    breaks = c("Qle.current", "Qle.hcchyd", "Qle.urban_plumber"),
    labels = c("Current", "HCC_HYD", "Urban-PLUMBER"),
    
  ) +
  scale_shape_manual(values = c("Qle" = 17)) +
  scale_linetype_manual(
    values = linetypes[c("Qle.current", "Qle.hcchyd")],
    breaks = c("Qle.current", "Qle.hcchyd"),
    labels = c("Current", "HCC_HYD")
  ) +
  labs(
    y = expression("Mean Absolute Error (" * W ~ m^-2 * ")"),
    x = "",
    color = expression(bold(Q[E])),
    linetype = expression(bold("Group Mean ") * bold(Q[E]))
  ) +
  theme_bw(base_size = 12) +
  fixed_theme +
  theme(
    # --- Base text elements ---
    text = element_text(size = 12),
    
    # --- Tick mark labels (x and y axes) ---
    axis.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    
    # --- Axis titles ---
    axis.title = element_text(size = 12),
    
    # --- Legend titles and entries ---
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(shape = c(17, 17, 17), linetype = rep(0, 3))
    ),
    shape = "none",
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        color = flux_colors[c("Qle.current", "Qle.hcchyd")],
        shape = NA
      )
    )
  )

mae_plot_qle

ggsave(
  "Figure_mae_qle.pdf",
  plot = mae_plot_qle,
  width = 10,
  height = 6,
  dpi = 500
)

# ----- Qh only -----
plot_data_qh <- plot_data %>% filter(Flux == "Qh")
mean_values_qh <- mean_values %>% filter(Flux == "Qh")

mae_plot_qh <- ggplot() +
  geom_rect(
    data = band_data,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = 0,
      ymax = 60,
      fill = fill
    ),
    alpha = 0.2
  ) +
  scale_fill_identity() +
  geom_hline(
    data = mean_values_qh,
    aes(
      yintercept = mean_mae,
      color = paste(Flux, Surface, sep = "."),
      linetype = paste(Flux, Surface, sep = ".")
    ),
    linewidth = 1,
    alpha = 0.8,
    show.legend = TRUE
  ) +
  geom_point(
    data = plot_data_qh,
    aes(
      x = Site_label,
      y = value,
      color = color_group,
      shape = Flux
    ),
    size = 3.5,
    position = position_dodge(width = 0.6)
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    expand = c(0, 0)
  ) +
  scale_x_discrete(
    labels = station_labeler
  ) +
  scale_color_manual(
    values = flux_colors[c("Qh.current", "Qh.hcchyd", "Qh.urban_plumber")],
    breaks = c("Qh.current", "Qh.hcchyd", "Qh.urban_plumber"),
    labels = c("Current", "HCC_HYD", "Urban-PLUMBER")
  ) +
  scale_shape_manual(values = c("Qh" = 16)) +
  scale_linetype_manual(
    values = linetypes[c("Qh.current", "Qh.hcchyd")],
    breaks = c("Qh.current", "Qh.hcchyd"),
    labels = c("Current", "HCC_HYD")
  ) +
  labs(
    y = expression("Mean Absolute Error (" * W ~ m^-2 * ")"),
    x = "",
    color = expression(bold(Q[H])),
    linetype = expression(bold("Group Mean ") * bold(Q[H]))
  ) +
  theme_bw() +
  fixed_theme +
  theme(
    # --- Base text elements ---
    text = element_text(size = 12),
    
    # --- Tick mark labels (x and y axes) ---
    axis.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    
    # --- Axis titles ---
    axis.title = element_text(size = 12),
    
    # --- Legend titles and entries ---
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(shape = c(16, 16, 16), linetype = rep(0, 3))
    ),
    shape = "none",
    linetype = guide_legend(
      order = 2,
      override.aes = list(
        color = flux_colors[c("Qh.current", "Qh.hcchyd")],
        shape = NA
      )
    )
  )

mae_plot_qh

  ggsave(
  "Figure_mae_qh.pdf",
  plot = mae_plot_qh,
  width = 10,
  height = 6,
  dpi = 500
)
  