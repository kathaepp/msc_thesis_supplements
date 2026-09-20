library(readr)
library(tidyverse)
library(gridExtra)
library(tidyr)
library(dplyr)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stations <- c("AU-Preston", "AU-SurreyHills", "CA-Sunset", "FI-Kumpula", "FI-Torni",
              "FR-Capitole", "GR-HECKOR", "JP-Yoyogi", "KR-Jungnang", "KR-Ochang",
              "MX-Escandon", "NL-Amsterdam", "PL-Lipowa", "PL-Narutowicza",
              "SG-TelokKurau06", "UK-KingsCollege", "UK-Swindon", "US-Baltimore",
              "US-Minneapolis1", "US-Minneapolis2", "US-WestPhoenix")
ordered_types <- c("Current", "HCC", "HYD", "HCC_HYD")
df_bias_djf_max_Qh <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_djf_max_Qh.csv")
df_bias_djf_max_Qh$Variable <- "Qh"
df_bias_djf_max_Qh$Stat <- "max"
df_bias_djf_max_Qh$Station <- stations

# Convert the dataframe
df_bias_djf_max_Qh <- df_bias_djf_max_Qh %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "DJF",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)


df_bias_djf_max_Rnet <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_djf_max_Rnet.csv")
df_bias_djf_max_Rnet$Variable <- "Rnet"
df_bias_djf_max_Rnet$Stat <- "max"
df_bias_djf_max_Rnet$Station <- stations

# Convert the dataframe
df_bias_djf_max_Rnet <- df_bias_djf_max_Rnet %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "DJF",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)

df_bias_djf_max_Qle <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_djf_max_Qle.csv")
df_bias_djf_max_Qle$Variable <- "Qle"
df_bias_djf_max_Qle$Stat <- "max"
df_bias_djf_max_Qle$Station <- stations

# Convert the dataframe
df_bias_djf_max_Qle <- df_bias_djf_max_Qle %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "DJF",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)



df_bias_djf_min_Qh <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_djf_min_Qh.csv")
df_bias_djf_min_Qh$Variable <- "Qh"
df_bias_djf_min_Qh$Stat <- "min"
df_bias_djf_min_Qh$Station <- stations

# Convert the dataframe
df_bias_djf_min_Qh <- df_bias_djf_min_Qh %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "DJF",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)



df_bias_djf_min_Qle <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_djf_min_Qle.csv")
df_bias_djf_min_Qle$Variable <- "Qle"
df_bias_djf_min_Qle$Stat <- "min"
df_bias_djf_min_Qle$Station <- stations

# Convert the dataframe
df_bias_djf_min_Qle <- df_bias_djf_min_Qle %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "DJF",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)



df_bias_jja_max_Qh <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_jja_max_Qh.csv")
df_bias_jja_max_Qh$Variable <- "Qh"
df_bias_jja_max_Qh$Stat <- "max"
df_bias_jja_max_Qh$Station <- stations

# Convert the dataframe
df_bias_jja_max_Qh <- df_bias_jja_max_Qh %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "JJA",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)



df_bias_jja_max_Qle <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_jja_max_Qle.csv")
df_bias_jja_max_Qle$Variable <- "Qle"
df_bias_jja_max_Qle$Stat <- "max"
df_bias_jja_max_Qle$Station <- stations

# Convert the dataframe
df_bias_jja_max_Qle <- df_bias_jja_max_Qle %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "JJA",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)



df_bias_jja_max_Rnet <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_jja_max_Rnet.csv")
df_bias_jja_max_Rnet$Variable <- "Rnet"
df_bias_jja_max_Rnet$Stat <- "max"
df_bias_jja_max_Rnet$Station <- stations

# Convert the dataframe
df_bias_jja_max_Rnet <- df_bias_jja_max_Rnet %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "JJA",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)



df_bias_jja_min_Qh <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_jja_min_Qh.csv")
df_bias_jja_min_Qh$Variable <- "Qh"
df_bias_jja_min_Qh$Stat <- "min"
df_bias_jja_min_Qh$Station <- stations

# Convert the dataframe
df_bias_jja_min_Qh <- df_bias_jja_min_Qh %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "JJA",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)



df_bias_jja_min_Qle <- read_csv("C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/bias_tables/df_bias_jja_min_Qle.csv")
df_bias_jja_min_Qle$Variable <- "Qle"
df_bias_jja_min_Qle$Stat <- "min"
df_bias_jja_min_Qle$Station <- stations

# Convert the dataframe
df_bias_jja_min_Qle <- df_bias_jja_min_Qle %>%
  pivot_longer(
    cols = c(current, hcc, hyd, hcc_hyd),
    names_to = "Type",
    values_to = "mean_bias"
  ) %>%
  mutate(
    Season = "JJA",  
    Type = case_when(
      Type == "current" ~ "Current",
      Type == "hcc" ~ "HCC",
      Type == "hyd" ~ "HYD",
      Type == "hcc_hyd" ~ "HCC_HYD",
      TRUE ~ Type
    )
  ) %>%
  select(Variable, Stat, Station, Season, Type, mean_bias) %>%
  arrange(Station, Type)


bias_all2 <- bind_rows(df_bias_jja_min_Qle, df_bias_jja_min_Qh,
                       df_bias_jja_max_Qle, df_bias_jja_max_Qh,
                       df_bias_jja_max_Rnet,
                       df_bias_djf_min_Qle, df_bias_djf_min_Qh,
                       df_bias_djf_max_Qle, df_bias_djf_max_Qh,
                       df_bias_djf_max_Rnet
                       )


bias_all2$Season[bias_all2$Station == "AU-Preston" & bias_all2$Season == "DJF"] <- "Summer"
bias_all2$Season[bias_all2$Station == "AU-SurreyHills" & bias_all2$Season == "DJF"] <- "Summer"
bias_all2$Season[bias_all2$Station == "AU-Preston" & bias_all2$Season == "JJA"] <- "Winter"
bias_all2$Season[bias_all2$Station == "AU-SurreyHills" & bias_all2$Season == "JJA"] <- "Winter"
bias_all2$Season[bias_all2$Season == "JJA"] <- "Summer"
bias_all2$Season[bias_all2$Season == "DJF"] <- "Winter"

# Colors
plot_colors <- c("Current" = "#E69F00", "HCC" = "#D55E00", 
                 "HYD" = "#0072B2", "HCC_HYD" = "#CC79A7")


plot_bias_boxplot_only <- function(df_season, season_label) {
  
  # Desired facet order
  facet_levels <- c("Rnet_max", "Qh_max", "Qh_min", "Qle_max", "Qle_min")
  
  labels_facet <- c(
    "Rnet_max" = "R[net]~'- max'",
    "Qh_max"   = "Q[H]~'- max'",
    "Qh_min"   = "Q[H]~'- min'",
    "Qle_max"  = "Q[E]~'- max'",
    "Qle_min"  = "Q[E]~'- min'"
  )
  
  df_season_filt <- df_season %>% 
    filter(!(Variable == "Rnet" & Stat == "min")) %>%
    mutate(
      Type = factor(Type, levels = ordered_types),
      Facet = factor(paste0(Variable, "_", Stat), levels = facet_levels)
    )
  
  # Plot
  p_box <- ggplot(df_season_filt, aes(x = Type, y = mean_bias, fill = Type)) +
    geom_boxplot(outlier.shape = NA, width = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~ Facet, scales = "free_y", nrow = 1, labeller = as_labeller(labels_facet, default = label_parsed))+
    scale_fill_manual(values = plot_colors) +
    labs(title = paste(season_label),
         y = expression("Bias (" * W ~ m^-2 * ")"),
         x = "") +
    theme_bw(base_size = 12) +
    coord_cartesian(ylim = c(-200, 200)) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size=12),
      axis.text.y = element_text(size=12),
      strip.text = element_text(face = "bold", size=12),
      strip.background = element_rect(fill = "white")
    )
  
  return(p_box)
}

bias_jja_long <- bias_all2[bias_all2$Season == "Summer",]
bias_djf_long <-  bias_all2[bias_all2$Season == "Winter",]

# Generate plots
plot_jja_box <- plot_bias_boxplot_only(bias_jja_long, "Summer")
plot_djf_box <- plot_bias_boxplot_only(bias_djf_long, "Winter")

# Combine both seasons with patchwork
combined_boxplots <- plot_jja_box / plot_djf_box + 
  theme(plot.tag = element_text(face = 'bold'))

#Plot Figure
combined_boxplots

#Save Figure
ggsave("Figure_bias_distributions.pdf",
       combined_boxplots, width = 10, height = 10, dpi = 500)