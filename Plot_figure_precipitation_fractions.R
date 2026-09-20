library(tidyverse)
library(ggtext)
library(glue)

#Set working directory
DIR = dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(DIR)


# Load data
flux_data <- read_csv("mean_annual_fluxes_by_station.csv")

# Rename columns and capitalize stations
colnames(flux_data) <- c("Station", "Scenario", "Evapotranspiration", "Runoff", "Drainage")
flux_data$Station <- c(
  rep("AU-Preston", 4), rep("AU-SurreyHills", 4), rep("CA-Sunset", 4), rep("FI-Kumpula", 4), rep("FI-Torni", 4),
  rep("FR-Capitole", 4), rep("GR-Heckor", 4), rep("JP-Yoyogi", 4), rep("KR-Jungnang", 4), rep("KR-Ochang", 4),
  rep("MX-Escandon", 4), rep("NL-Amsterdam", 4), rep("PL-Lipowa", 4), rep("PL-Narutowicza", 4),
  rep("SG-TelokKurau", 4), rep("UK-KingsCollege", 4), rep("UK-Swindon", 4), rep("US-Baltimore", 4),
  rep("US-Minneapolis1", 4), rep("US-Minneapolis2", 4), rep("US-WestPhoenix", 4)
)

# Define impervious percentages and ordered stations
impervious_df <- tibble(
  Station = c(
    "KR-Jungnang", "MX-Escandon", "JP-Yoyogi", "GR-Heckor", "FR-Capitole", 
    "SG-TelokKurau", "UK-KingsCollege", "FI-Torni", "PL-Lipowa", "CA-Sunset", 
    "NL-Amsterdam", "PL-Narutowicza", "AU-Preston", "AU-SurreyHills", 
    "UK-Swindon", "US-WestPhoenix", "KR-Ochang", "FI-Kumpula", 
    "US-Baltimore", "US-Minneapolis1", "US-Minneapolis2"
  ),
  Impervious = c(
    96.5, 94, 92, 91.6, 90,
    85, 79, 77, 76, 68,
    68, 65, 62, 54,
    49, 48, 47, 46,
    31.3, 21, 5
  )
)

# Join impervious % to flux_data
flux_data <- flux_data %>%
  left_join(impervious_df, by = "Station") %>%
  mutate(
    Station_label = paste0(Station, " (", Impervious, "%)")
  )

# Define correct order for plotting
ordered_stations <- c(
  "KR-Jungnang", "MX-Escandon", "JP-Yoyogi", "GR-Heckor", "FR-Capitole", 
  "SG-TelokKurau", "UK-KingsCollege", "FI-Torni", "PL-Lipowa", "CA-Sunset", 
  "NL-Amsterdam", "PL-Narutowicza", "AU-Preston", "AU-SurreyHills", 
  "UK-Swindon", "US-WestPhoenix", "KR-Ochang", "FI-Kumpula", 
  "US-Baltimore", "US-Minneapolis1", "US-Minneapolis2"
)

flux_data$Station_label <- factor(
  flux_data$Station_label,
  levels = paste0(ordered_stations, " (", impervious_df$Impervious[match(ordered_stations, impervious_df$Station)], "%)")
)

# Define correct order of scenarios
scenario_order <- c("Current", "HCC", "HYD", "HCC_HYD")
flux_data <- flux_data %>%
  mutate(Scenario = factor(Scenario, levels = scenario_order))


# Define station_colors using new labels
station_colors <- c(
  "AU-Preston (62%)"       = "#44AA99",
  "AU-SurreyHills (54%)"   = "#44AA99",
  "CA-Sunset (68%)"        = "#44AA99",
  "FI-Kumpula (46%)"       = "#882255",
  "FI-Torni (77%)"         = "#882255",
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

# Calculate total annual precipitation and convert each component to a percentage
# Here, precipitation is inferred from the annual water balance as
# evapotranspiration + runoff + drainage.
flux_data <- flux_data %>%
  mutate(
    Total_precipitation = Evapotranspiration + Runoff + Drainage
  )

# Total precipitation shown in each facet. Because precipitation forcing should
# be identical among scenarios at a given site, the mean is used only to avoid
# displaying negligible numerical differences caused by rounding.
precipitation_by_station <- flux_data %>%
  group_by(Station_label) %>%
  summarise(
    Precipitation = mean(Total_precipitation, na.rm = TRUE),
    Precipitation_range = max(Total_precipitation, na.rm = TRUE) -
      min(Total_precipitation, na.rm = TRUE),
    .groups = "drop"
  )


colnames(flux_data) <- c("Station", "Scenario", "Evapotranspiration", "Surface Runoff", "Subsurface Runoff",          
                         "Impervious", "Station_label", "Total_precipitation")

# Prepare stacked percentage data
plot_data <- flux_data %>%
  pivot_longer(
    cols = c("Evapotranspiration", "Surface Runoff", "Subsurface Runoff"),
    names_to = "Component",
    values_to = "Value"
  ) %>%
  mutate(
    Percentage = 100 * Value / Total_precipitation,
    Component = factor(
      Component,
      levels = c("Subsurface Runoff", "Evapotranspiration", "Surface Runoff")
    )
  )

# Color facet labels and add total precipitation below the station name
station_order <- levels(flux_data$Station_label)
precipitation_lookup <- setNames(
  precipitation_by_station$Precipitation,
  as.character(precipitation_by_station$Station_label)
)

colored_labels <- glue(
  "<span style='color:{station_colors[station_order]}'><b>{station_order}</b></span><br>",
  "<span style='color:black;font-size:10pt'>P = {round(precipitation_lookup[station_order])} mm yr<sup>-1</sup></span>"
)

plot_data$Station_colored <- factor(
  plot_data$Station_label,
  levels = station_order
)
levels(plot_data$Station_colored) <- colored_labels

# Plot
component_colors <- c(
  "Subsurface Runoff"  = "#F0E442",
  "Evapotranspiration" = "#009E73",
  "Surface Runoff"     = "#56B4E9"
)

p <- ggplot(plot_data, aes(x = Scenario, y = Percentage, fill = Component)) +
  geom_col(
    width = 0.65,
    position = position_stack(reverse = TRUE)
  ) +
  facet_wrap(~Station_colored) + #, nrow=5, ncol=5) + # added nrow and ncol
  scale_fill_manual(
    values = component_colors,
    breaks = c("Subsurface Runoff", "Evapotranspiration", "Surface Runoff")
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.01))
  ) +
  # Apply the 0–100% display range after stacking. Using limits inside
  # scale_y_continuous() can remove the upper stack segment when floating-point
  # rounding makes the cumulative total slightly larger than 100.
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    x = NULL,
    y = "Fraction of annual precipitation"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_bw(base_size = 12) +
  theme(
    axis.text            = element_text(size=12),
    axis.text.x          = element_text(angle = 45, hjust = 1, size=10),
    axis.text.y          = element_text(size = 12),
    axis.title           = element_text(size=12),
    strip.background     = element_blank(),
    strip.text           = element_markdown(size = 10, lineheight = 1.05),

    legend.position        =  "inside",
    legend.position.inside = c(1, 0),
    legend.justification   = c(1, 0),
    legend.direction     = "horizontal",
    legend.title         = element_blank(),
    legend.text          = element_text(size = 12),
    legend.key.size      = unit(1.4, "lines"),
    legend.spacing.x     = unit(0.8, "cm"),
    legend.background    = element_blank(),
    legend.box.background = element_blank(),
    legend.key           = element_rect(colour = NA),

    panel.grid.major.x   = element_blank(),
    panel.grid.minor     = element_blank(),
    plot.margin          = margin(10, 10, 10, 10)
  )

p

# Save
ggsave("Figure_precipitation_fractions.pdf", p, width = 10, height = 10, dpi = 500)