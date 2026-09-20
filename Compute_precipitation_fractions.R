library(ncdf4)
library(dplyr)
library(lubridate)
library(tidyr)

# === SIMULATION AND FOLDER SETUP ===
simulation_current_name <- "current"
simulation_hcc_name <- "hcc"
simulation_hyd_name <- "hyd"
simulation_hcc_hyd_name <- "hcc_hyd"
folder_simulations <- "/cluster/work/climate/kepp/urban_implementation/icon-mpim/experiments/"

# === LIST OF STATIONS ===
list_names_fluxnet_stations <- c("au-preston", "au-surreyhills", "ca-sunset", "fi-kumpula", "fi-torni",
                                 "fr-capitole", "gr-heckor", "jp-yoyogi", "kr-jungnang", "kr-ochang",
                                 "mx-escandon", "nl-amsterdam", "pl-lipowa", "pl-narutowicza", "sg-telokkurau06",
                                 "uk-kingscollege", "uk-swindon", "us-baltimore", "us-minneapolis1", "us-minneapolis2", "us-westphoenix")

list_years_fluxnet_stations <- c("19930101T000000Z", "19940101T000000Z", "20020101T000000Z", "20000101T000000Z",
                                 "20000101T000000Z", "19940101T000000Z", "20090101T000000Z", "20060101T000000Z",
                                 "20070101T000000Z", "20050101T000000Z", "20010101T000000Z", "20090101T000000Z",
                                 "19980101T000000Z", "19980101T000000Z", "19960101T000000Z", "20020101T000000Z",
                                 "20010101T000000Z", "19920101T000000Z", "19960101T000000Z","19960101T000000Z", "20010101T000000Z")

# === MASTER DATA STORAGE ===
all_stations_data <- list()

# === MAIN LOOP ===
for (i in seq_along(list_names_fluxnet_stations)) {
  station <- list_names_fluxnet_stations[i]
  print(paste0("Computing annual means for ", station))
  years <- list_years_fluxnet_stations[i]

  sim_names <- c(Current = simulation_current_name,
                 HCC = simulation_hcc_name,
                 HYD = simulation_hyd_name,
                 HCC_HYD = simulation_hcc_hyd_name)

  station_data <- list()

  for (type in names(sim_names)) {
    sim_name <- sim_names[[type]]
    file_path <- paste0(folder_simulations, "land_jsbach_sitelevel_", sim_name, "_",
                        station, "/land_jsbach_sitelevel_", sim_name, "_", station, "_lnd_basic_", station, "_ml_",
                        years, ".nc")

    nc <- nc_open(file_path)
    time_vals <- ncvar_get(nc, "time")
    time_units <- ncatt_get(nc, "time", "units")$value
    origin_date <- sub(".*since ", "", time_units)
    datetime_vals <- as.POSIXct(origin_date, tz = "UTC") + as.difftime(time_vals, units = "mins")

    ET <- ncvar_get(nc, "hydro_evapotrans_box") * 1800  
    R <- ncvar_get(nc, "hydro_runoff_box") * 1800     
    D <- ncvar_get(nc, "hydro_drainage_box") * 1800   
    nc_close(nc)
    
    ET <- -ET # ICON-Land outputs ET in opposite direction

    df <- data.frame(Date = datetime_vals, ET = ET, R = R, D = D)
    df$Year <- year(df$Date)
    df$Type <- type
    station_data[[type]] <- df
  }

  df_all <- bind_rows(station_data)

  # === SUM ANNUAL VALUES THEN TAKE MEAN ===
  df_annual <- df_all %>%
    group_by(Type, Year) %>%
    summarize(ET = sum(ET, na.rm = TRUE),
              R = sum(R, na.rm = TRUE),
              D = sum(D, na.rm = TRUE),
              .groups = "drop")

  df_mean_annual <- df_annual %>%
    group_by(Type) %>%
    summarize(ET_mean = mean(ET, na.rm = TRUE),
              R_mean = mean(R, na.rm = TRUE),
              D_mean = mean(D, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(Station = station)

  all_stations_data[[i]] <- df_mean_annual
}

# === FINAL DATA FRAME WITH MEAN ANNUAL VALUES BY STATION AND TYPE ===
mean_flux_per_station <- bind_rows(all_stations_data) %>%
  select(Station, Type, ET_mean, R_mean, D_mean)

# === OUTPUT EXAMPLE ===
print(mean_flux_per_station)

# Optional: write to CSV
write.csv(mean_flux_per_station, "/cluster/home/katepp/R_analysis_rev/mean_annual_fluxes_by_station.csv", row.names = FALSE)

