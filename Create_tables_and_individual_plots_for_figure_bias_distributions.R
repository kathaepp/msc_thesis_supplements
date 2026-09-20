library(readr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ============================================================
# SETTINGS
# ============================================================

simulation_names <- c(
  Current = "current",
  HCC = "hcc",
  HYD   = "hyd",
  HCC_HYD   = "hcc_hyd"
)

stations <- c("AU-Preston", "AU-Surreyhills", "CA-Sunset", "FI-Kumpula", "FI-Torni",
              "FR-Capitole", "GR-Heckor", "JP-Yoyogi", "KR-Jungnang", "KR-Ochang",
              "MX-Escandon", "NL-Amsterdam", "PL-Lipowa", "PL-Narutowicza",
              "SG-Telokkurau06", "UK-Kingscollege", "UK-Swindon", "US-Baltimore",
              "US-Minneapolis1", "US-Minneapolis2", "US-Westphoenix")

# Create the bias tables

df_bias_djf_max_Qh <- matrix(
  ncol = length(simulation_names),
  nrow = length(stations)
)

colnames(df_bias_djf_max_Qh) <- simulation_names
row.names(df_bias_djf_max_Qh) <- stations

outdir <- "C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/plot_bias_by_stations/"

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

indir <- "C:/Users/Katharina/Downloads/M.Sc._Thesis/Morgane_R_plot_scripts_for_JSBACH_rev/tables_figure3/"

# ============================================================
# PLOTS FOR EACH STATION AND SIMULATION
# ============================================================

for (station in stations) {
  for (season in c("DJF", "JJA")) {
    for (stat in c("max", "min")) {
      for (variable in c(
        "Qh",
        "Qle",
        "Rnet",
        "SWup",
        "LWup",
        "G"
      )) {
        
        # No minimum tables for Rnet or SWup
        if (
          stat == "min" &
          variable %in% c("Rnet", "SWup")
        ) {
          next
        }
        
        file <- paste0(
          indir,
          station, "_",
          variable, "_",
          stat, "_",
          season,
          ".csv"
        )
        
        if (!file.exists(file)) next
        
        df_station <- read_csv(
          file,
          show_col_types = FALSE
        )
        
        values <- unlist(
          df_station[
            c(
              "Observations",
              names(simulation_names)
            )
          ]
        )
        
        if (!any(is.finite(values))) {
          print(
            paste(
              "No finite values:",
              file
            )
          )
          
          next
        }
        
        limits <- range(
          values[is.finite(values)]
        )
        
        for (type in names(simulation_names)) {
          
          x <- df_station$Observations
          y <- df_station[[type]]
          
          valid <- (
            is.finite(x) &
              is.finite(y)
          )
          
          if (!any(valid)) {
            print(
              paste(
                "No valid pairs:",
                station,
                type,
                variable,
                stat,
                season
              )
            )
            
            next
          }
          
          png(
            paste0(
              outdir,
              station, "_",
              type, "_",
              variable, "_",
              stat, "_",
              season,
              ".png"
            ),
            width = 1500,
            height = 1500
          )
          
          plot(
            x,
            y,
            type = "n",
            xlim = limits,
            ylim = limits,
            asp = 1,
            main = paste(
              type,
              station,
              variable,
              stat,
              season
            ),
            xlab = "Observations",
            ylab = "Simulations"
          )
          
          abline(
            0,
            1,
            lty = 2
          )
          
          points(
            x,
            y
          )
          
          if (sum(valid) >= 2) {
            abline(
              lm(
                y[valid] ~ x[valid]
              ),
              lwd = 2
            )
          }
          
          dev.off()
        }
      }
    }
  }
}

# ============================================================
# CREATE BIAS TABLES FOR FIGURE
# ============================================================

bias_outdir <- paste0(
  indir,
  "bias_tables/"
)

dir.create(
  bias_outdir,
  showWarnings = FALSE
)

for (season in c("DJF", "JJA")) {
  for (stat in c("max", "min")) {
    for (variable in c(
      "Qh",
      "Qle",
      "Rnet",
      "SWup",
      "LWup",
      "G"
    )) {
      
      # No minimum tables for Rnet or SWup
      if (
        stat == "min" &
        variable %in% c("Rnet", "SWup")
      ) {
        next
      }
      
      bias <- matrix(
        NA,
        nrow = length(stations),
        ncol = length(simulation_names)
      )
      
      colnames(bias) <- names(simulation_names)
      rownames(bias) <- stations
      
      for (i in seq_along(stations)) {
        
        file <- paste0(
          indir,
          stations[i], "_",
          variable, "_",
          stat, "_",
          season,
          ".csv"
        )
        
        if (!file.exists(file)) next
        
        df_station <- read_csv(
          file,
          show_col_types = FALSE
        )
        
        for (type in names(simulation_names)) {
          
          valid <- (
            is.finite(df_station$Observations) &
              is.finite(df_station[[type]])
          )
          
          if (any(valid)) {
            bias[i, type] <- mean(
              df_station[[type]][valid] -
                df_station$Observations[valid]
            )
          }
        }
      }
      
      colnames(bias) <- unname(simulation_names)
      
      write.csv(
        bias,
        paste0(
          bias_outdir,
          "df_bias_",
          tolower(season), "_",
          stat, "_",
          variable,
          ".csv"
        ),
        row.names = FALSE
      )
    }
  }
}
