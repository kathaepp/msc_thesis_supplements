# Load libraries

library(ncdf4)
library(lubridate)

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

year_range <- c("19930101T000000Z", "19940101T000000Z", "20020101T000000Z", "20000101T000000Z",
                "20000101T000000Z", "19940101T000000Z", "20090101T000000Z", "20060101T000000Z",
                "20070101T000000Z", "20050101T000000Z", "20010101T000000Z", "20090101T000000Z",
                "19980101T000000Z", "19980101T000000Z", "19960101T000000Z", "20020101T000000Z",
                "20010101T000000Z", "19920101T000000Z", "19960101T000000Z","19960101T000000Z", "20010101T000000Z")

folder_obs <- "/cluster/home/katepp/Urban-PLUMBER/"
folder_sim <- "/cluster/work/climate/kepp/urban_implementation/icon-mpim/experiments/land_jsbach_sitelevel_"
output_folder <- "/cluster/home/katepp/R_analysis_rev/MAE_tables/"

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)


# ============================================================
# CREATE EMPTY OUTPUT TABLES
# ============================================================

# MAE tables

df_mae_Qh <- data.frame(Station = stations)
df_mae_Qle <- data.frame(Station = stations)
df_mae_Rnet <- data.frame(Station = stations)

# Number of model-observation pairs used for each MAE

df_n_Qh <- data.frame(Station = stations)
df_n_Qle <- data.frame(Station = stations)
df_n_Rnet <- data.frame(Station = stations)

# Add one empty column for each simulation

for (simulation_code in simulation_names) {
  df_mae_Qh[[simulation_code]] <- NA_real_
  df_mae_Qle[[simulation_code]] <- NA_real_
  df_mae_Rnet[[simulation_code]] <- NA_real_

  df_n_Qh[[simulation_code]] <- NA_integer_
  df_n_Qle[[simulation_code]] <- NA_integer_
  df_n_Rnet[[simulation_code]] <- NA_integer_
}

# This table contains all metrics and can be used to check the calculation

metrics_long <- data.frame(
  Station = character(),
  Simulation = character(),
  SimulationCode = character(),
  Variable = character(),
  N = integer(),
  MAE = numeric(),
  Bias = numeric(),
  RMSE = numeric()
)


# ============================================================
# LOOP OVER STATIONS
# ============================================================

for (i in 1:length(stations)) {

  station <- stations[i]
  yrange <- year_range[i]

  message("Processing ", station)


  # ----------------------------------------------------------
  # READ OBSERVATIONS
  # ----------------------------------------------------------

  observation_file <- paste0(
    folder_obs,
    station,
    "/timeseries/",
    station,
    "_clean_observations_v1.nc"
  )

  nc_obs <- nc_open(observation_file)

  time_vals <- ncvar_get(nc_obs, "time")
  time_units <- ncatt_get(nc_obs, "time", "units")$value
  origin_date <- sub(".*since ", "", time_units)

  datetime_obs <- as.POSIXct(
    origin_date,
    tryFormats = c(
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d",
      "%Y-%b-%d %H:%M:%S"
    ),
    tz = "UTC"
  ) + as.difftime(time_vals, units = "secs")
  datetime_obs <- datetime_obs + 30*60 # add 30min to account for period-ending timestamps in Urban-PLUMBER

  Qh_obs <- as.numeric(ncvar_get(nc_obs, "Qh"))
  Qle_obs <- as.numeric(ncvar_get(nc_obs, "Qle"))

  SWdown_obs <- as.numeric(ncvar_get(nc_obs, "SWdown"))
  SWup_obs <- as.numeric(ncvar_get(nc_obs, "SWup"))
  LWdown_obs <- as.numeric(ncvar_get(nc_obs, "LWdown"))
  LWup_obs <- as.numeric(ncvar_get(nc_obs, "LWup"))

  nc_close(nc_obs)

  Rnet_obs <- SWdown_obs - SWup_obs + LWdown_obs - LWup_obs


  # ----------------------------------------------------------
  # LOOP OVER SIMULATIONS
  # ----------------------------------------------------------
  lower_station <- tolower(station)
  for (type in names(simulation_names)) {
    sim <- simulation_names[[type]]

    simulation_file <- paste0(folder_sim, sim, "_", lower_station, "/land_jsbach_sitelevel_", sim, "_",
                              lower_station, "_lnd_basic_", lower_station, "_ml_", yrange, ".nc")

    nc_sim <- nc_open(simulation_file)

    time_vals <- ncvar_get(nc_sim, "time")
    time_units <- ncatt_get(nc_sim, "time", "units")$value
    origin_date <- sub(".*since ", "", time_units)

    datetime_sim <- as.POSIXct(
      origin_date,
      tryFormats = c(
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S",
        "%Y-%m-%d",
        "%Y-%b-%d %H:%M:%S"
      ),
      tz = "UTC"
    ) +
      as.difftime(time_vals, units = "mins")

    Qh_sim <- as.numeric(-ncvar_get(nc_sim, "seb_sensible_hflx_box")) # opposite sign in ICON-Land
    Qle_sim <- as.numeric(-ncvar_get(nc_sim, "seb_latent_hflx_box")) # opposite sign in ICON-Land
    SWnet_sim <- as.numeric(ncvar_get(nc_sim, "rad_sw_srf_net_box"))
    LWnet_sim <- as.numeric(ncvar_get(nc_sim, "rad_lw_srf_net_box"))

    nc_close(nc_sim)

    Rnet_sim <- SWnet_sim + LWnet_sim


    # --------------------------------------------------------
    # MATCH THE SIMULATION TO THE OBSERVATION TIMESTAMPS
    # --------------------------------------------------------

    matching_rows <- match(
      round(as.numeric(datetime_obs)),
      round(as.numeric(datetime_sim))
    )

    Qh_sim_matched <- Qh_sim[matching_rows]
    Qle_sim_matched <- Qle_sim[matching_rows]
    Rnet_sim_matched <- Rnet_sim[matching_rows]


    # --------------------------------------------------------
    # Qh
    # --------------------------------------------------------

    valid_Qh <- is.finite(Qh_obs) & is.finite(Qh_sim_matched)
    N_Qh <- sum(valid_Qh)

    if (N_Qh > 0) {
      error_Qh <- Qh_sim_matched[valid_Qh] - Qh_obs[valid_Qh]

      MAE_Qh <- mean(abs(error_Qh))
      Bias_Qh <- mean(error_Qh)
      RMSE_Qh <- sqrt(mean(error_Qh^2))

    } else {
      MAE_Qh <- NA_real_
      Bias_Qh <- NA_real_
      RMSE_Qh <- NA_real_
    }


    # --------------------------------------------------------
    # Qle
    # --------------------------------------------------------

    valid_Qle <- is.finite(Qle_obs) & is.finite(Qle_sim_matched)
    N_Qle <- sum(valid_Qle)

    if (N_Qle > 0) {
      error_Qle <- Qle_sim_matched[valid_Qle] - Qle_obs[valid_Qle]

      MAE_Qle <- mean(abs(error_Qle))
      Bias_Qle <- mean(error_Qle)
      RMSE_Qle <- sqrt(mean(error_Qle^2))

    } else {
      MAE_Qle <- NA_real_
      Bias_Qle <- NA_real_
      RMSE_Qle <- NA_real_
    }


    # --------------------------------------------------------
    # Rnet
    # --------------------------------------------------------

    valid_Rnet <- is.finite(Rnet_obs) & is.finite(Rnet_sim_matched)
    N_Rnet <- sum(valid_Rnet)

    if (N_Rnet > 0) {
      error_Rnet <- Rnet_sim_matched[valid_Rnet] - Rnet_obs[valid_Rnet]

      MAE_Rnet <- mean(abs(error_Rnet))
      Bias_Rnet <- mean(error_Rnet)
      RMSE_Rnet <- sqrt(mean(error_Rnet^2))

    } else {
      MAE_Rnet <- NA_real_
      Bias_Rnet <- NA_real_
      RMSE_Rnet <- NA_real_
    }


    # --------------------------------------------------------
    # SAVE VALUES IN THE COMPACT TABLES
    # --------------------------------------------------------

    df_mae_Qh[[sim]][i] <- MAE_Qh
    df_mae_Qle[[sim]][i] <- MAE_Qle
    df_mae_Rnet[[sim]][i] <- MAE_Rnet

    df_n_Qh[[sim]][i] <- N_Qh
    df_n_Qle[[sim]][i] <- N_Qle
    df_n_Rnet[[sim]][i] <- N_Rnet


    # --------------------------------------------------------
    # ALSO SAVE EVERYTHING IN ONE LONG TABLE
    # --------------------------------------------------------

    metrics_long <- rbind(
      metrics_long,
      data.frame(
        Station = station,
        Simulation = type,
        SimulationCode = sim,
        Variable = "Qh",
        N = N_Qh,
        MAE = MAE_Qh,
        Bias = Bias_Qh,
        RMSE = RMSE_Qh
      ),
      data.frame(
        Station = station,
        Simulation = type,
        SimulationCode = sim,
        Variable = "Qle",
        N = N_Qle,
        MAE = MAE_Qle,
        Bias = Bias_Qle,
        RMSE = RMSE_Qle
      ),
      data.frame(
        Station = station,
        Simulation = type,
        SimulationCode = sim,
        Variable = "Rnet",
        N = N_Rnet,
        MAE = MAE_Rnet,
        Bias = Bias_Rnet,
        RMSE = RMSE_Rnet
      )
    )
  }
}


# ============================================================
# ORDER AND SAVE THE LONG TABLE
# ============================================================

metrics_long <- metrics_long[
  order(
    match(metrics_long$Variable, c("Qh", "Qle", "Rnet")),
    match(metrics_long$Station, stations),
    match(metrics_long$Simulation, names(simulation_names))
  ),
]

rownames(metrics_long) <- NULL

write.csv(
  metrics_long,
  file.path(output_folder, "MAE_metrics_strict_matching_long.csv"),
  row.names = FALSE
)


# ============================================================
# SAVE THE MAE TABLES
# ============================================================

write.csv(
  df_mae_Qh,
  file.path(output_folder, "df_mae_Qh3_corrected.csv"),
  row.names = FALSE
)

write.csv(
  df_mae_Qle,
  file.path(output_folder, "df_mae_Qle3_corrected.csv"),
  row.names = FALSE
)

write.csv(
  df_mae_Rnet,
  file.path(output_folder, "df_mae_Rnet3_corrected.csv"),
  row.names = FALSE
)


# ============================================================
# SAVE THE NUMBER OF POINTS USED FOR EACH MAE
# ============================================================

write.csv(
  df_n_Qh,
  file.path(output_folder, "df_n_Qh3_corrected.csv"),
  row.names = FALSE
)

write.csv(
  df_n_Qle,
  file.path(output_folder, "df_n_Qle3_corrected.csv"),
  row.names = FALSE
)

write.csv(
  df_n_Rnet,
  file.path(output_folder, "df_n_Rnet3_corrected.csv"),
  row.names = FALSE
)

message("All MAE tables saved.")
