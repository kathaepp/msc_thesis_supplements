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

time_zones <- c(
  "AU-Preston" = "Australia/Melbourne", "AU-Surreyhills" = "Australia/Melbourne",
  "CA-Sunset" = "America/Vancouver", "FI-Kumpula" = "Europe/Helsinki",
  "FI-Torni" = "Europe/Helsinki", "FR-Capitole" = "Europe/Paris",
  "GR-Heckor" = "Europe/Athens", "JP-Yoyogi" = "Asia/Tokyo",
  "KR-Jungnang" = "Asia/Seoul", "KR-Ochang" = "Asia/Seoul",
  "MX-Escandon" = "America/Mexico_City", "NL-Amsterdam" = "Europe/Amsterdam",
  "PL-Lipowa" = "Europe/Warsaw", "PL-Narutowicza" = "Europe/Warsaw",
  "SG-Telokkurau06" = "Asia/Singapore", "UK-Kingscollege" = "Europe/London",
  "UK-Swindon" = "Europe/London", "US-Baltimore" = "America/New_York",
  "US-Minneapolis1" = "America/Chicago", "US-Minneapolis2" = "America/Chicago",
  "US-Westphoenix" = "America/Phoenix"
)

folder_obs <- "/cluster/home/katepp/Urban-PLUMBER/"
folder_sim <- "/cluster/work/climate/kepp/urban_implementation/icon-mpim/experiments/land_jsbach_sitelevel_"
output_folder <- "/cluster/home/katepp/R_analysis_rev/tables_figure3"

dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

maximum_start_hour <- 11
maximum_end_hour   <- 15
minimum_start_hour <- 20
minimum_end_hour   <- 6
variables <- c("Qh", "Qle", "Rnet", "SWup", "LWup", "G")

df_infos <- matrix(nrow = length(stations), ncol = 27)
row.names(df_infos) <- stations
colnames(df_infos) <- c("total_length", 
                        "Qh_valid", "Qle_valid", "Rnet_valid", 
                        "Qh_max_length_jja", "Qle_max_length_jja", "Rnet_max_length_jja",
                        "Qh_min_length_jja", "Qle_min_length_jja",
                        "Qh_max_length_djf", "Qle_max_length_djf", "Rnet_max_length_djf",
                        "Qh_min_length_djf", "Qle_min_length_djf",
                        "SWup_valid", "LWup_valid", "G_valid",
                        "SWup_max_length_jja", "LWup_max_length_jja", "G_max_length_jja",
                        "LWup_min_length_jja", "G_min_length_jja",
                        "SWup_max_length_djf", "LWup_max_length_djf", "G_max_length_djf",
                        "LWup_min_length_djf", "G_min_length_djf")

# Create a function for later
# For each retained observation day/night, keep the simulation's own
# maximum/minimum over the same midday/night period.
fill_simulation_column <- function(
    table,
    sim_dates,
    sim_values,
    simulation_name,
    station,
    table_name,
    maximum_or_minimum,
    tz
) {
  
  sim_table <- data.frame(
    Date = sim_dates,
    Value = sim_values
  )
  
  if (maximum_or_minimum == "maximum") {
    
    # Keep the midday hours and assign the local calendar day
    sim_table <- sim_table[
      hour(sim_table$Date) >= maximum_start_hour &
        hour(sim_table$Date) < maximum_end_hour &
        is.finite(sim_table$Value),
      ]
    
    sim_table$Day <- as.Date(sim_table$Date, tz = tz)
    
    # Keep only days retained in the observation table
    sim_table <- sim_table[
      sim_table$Day %in% table$Day,
      ]
    
    # Sort from highest to lowest value for each day
    sim_table <- sim_table[
      order(sim_table$Day, -sim_table$Value, sim_table$Date),
      ]
    
  } else if (maximum_or_minimum == "minimum") {
    
    # Define the date of the night to which each time step belongs
    sim_night_day <- as.Date(
      sim_table$Date - hours(minimum_end_hour),
      tz = tz
    )
    
    sim_night_hours <- (
      hour(sim_table$Date) >= minimum_start_hour |
        hour(sim_table$Date) < minimum_end_hour
    )
    
    sim_table <- sim_table[
      sim_night_hours & is.finite(sim_table$Value),
      ]
    
    # Associate the early morning with the preceding evening
    sim_table$Day <- as.Date(
      sim_table$Date - hours(minimum_end_hour),
      tz = tz
    )
    
    # Keep only nights retained in the observation table
    sim_table <- sim_table[
      sim_table$Day %in% table$Day,
      ]
    
    
    # Sort from lowest to highest value for each night
    sim_table <- sim_table[
      order(sim_table$Day, sim_table$Value, sim_table$Date),
      ]
    
    
  } else {
    stop("maximum_or_minimum must be either 'maximum' or 'minimum'.")
  }
  
  # Keep the first row, therefore the simulated maximum/minimum, for each day/night
  sim_table <- sim_table[
    !duplicated(sim_table$Day),
    c("Day", "Value")
  ]
  
  # Match by local day/night, not by the exact observation timestamp
  matching_rows <- match(
    table$Day,
    sim_table$Day
  )
  
  table[[simulation_name]] <- sim_table$Value[matching_rows]
  
  # Report days/nights that were not found
  number_unmatched <- sum(is.na(matching_rows))
  
  if (number_unmatched > 0) {
    warning(
      station, " - ", simulation_name, " - ", table_name, ": ",
      number_unmatched, " of ", nrow(table),
      " days/nights were not matched.",
      call. = FALSE
    )
  }
  
  return(table)
}

i=1

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# We loop over the stations

for (i in 1:length(stations)) {
  
  station <- stations[i]
  yrange <- year_range[i]
  tz <- time_zones[[station]]
  
  observation_file <- paste0(folder_obs, station, "/timeseries/", station, "_clean_observations_v1.nc")
  nc_obs <- nc_open(observation_file)

  time_vals <- ncvar_get(nc_obs, "time")
  time_units <- ncatt_get(nc_obs, "time", "units")$value
  origin_date <- sub(".*since ", "", time_units)
  
  datetime_vals <- as.POSIXct(
    origin_date,
    tryFormats = c("%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S"),
    tz = "UTC"
  ) + as.difftime(time_vals, units = "secs")
  datetime_vals <- with_tz(datetime_vals, tz = tz) + 30*60 # add 30 mins to observations to account for period-ending timestamps in Urban-PLUMBER 
  
  Qh_obs     <- as.numeric(ncvar_get(nc_obs, "Qh"))
  Qle_obs    <- as.numeric(ncvar_get(nc_obs, "Qle"))
  SWdown_obs <- as.numeric(ncvar_get(nc_obs, "SWdown"))
  SWup_obs   <- as.numeric(ncvar_get(nc_obs, "SWup"))
  LWdown_obs <- as.numeric(ncvar_get(nc_obs, "LWdown"))
  LWup_obs   <- as.numeric(ncvar_get(nc_obs, "LWup"))
  
  nc_close(nc_obs)
  
  Rnet_obs <- SWdown_obs - SWup_obs + LWdown_obs - LWup_obs
  
  # The observation G term is the surface energy-balance residual.
  # This sign convention is consistent with the model Qg output:
  # G = Qh + Qle - Rnet
  G_obs <- Qh_obs + Qle_obs - Rnet_obs
  
  # SWup and LWup are retained only when both the downwelling and
  # upwelling components needed for the model diagnosis are available.
  SWup_obs_for_table <- ifelse(
    is.finite(SWdown_obs) & is.finite(SWup_obs),
    SWup_obs,
    NA_real_
  )
  
  LWup_obs_for_table <- ifelse(
    is.finite(LWdown_obs) & is.finite(LWup_obs),
    LWup_obs,
    NA_real_
  )
  
  df_obs <- data.frame(
    Date = with_tz(datetime_vals, tz = tz),
    Qh = Qh_obs,
    Qle = Qle_obs,
    Rnet = Rnet_obs,
    SWdown = SWdown_obs,
    SWup = SWup_obs_for_table,
    LWdown = LWdown_obs,
    LWup = LWup_obs_for_table,
    G = G_obs
    )
  
  # Number of expected time steps in a complete midday/night period
  observation_time_step <- median(diff(as.numeric(datetime_vals)))
  expected_maximum_steps <- as.integer(round(
    (maximum_end_hour - maximum_start_hour) * 3600 /
      observation_time_step
  ))
  expected_minimum_steps <- as.integer(round(
    ((24 - minimum_start_hour) + minimum_end_hour) * 3600 /
      observation_time_step
  ))
  
  # We save the info on how many valid time step we have in the observations
  df_infos[i,1] <- length(df_obs$Date)
  df_infos[i,2] <- length(df_obs$Qh[is.na(df_obs$Qh) == F])
  df_infos[i,3] <- length(df_obs$Qle[is.na(df_obs$Qle) == F])
  df_infos[i,4] <- length(df_obs$Rnet[is.na(df_obs$Rnet) == F])
  df_infos[i,15] <- length(df_obs$SWup[is.na(df_obs$SWup) == F])
  df_infos[i,16] <- length(df_obs$LWup[is.na(df_obs$LWup) == F])
  df_infos[i,17] <- length(df_obs$G[is.na(df_obs$G) == F]) 
  
  #-----------------------------------------------------------------------------
  # JJA
  
  # Qh max
  obs_max_jja_Qh <- df_obs[month(df_obs$Date) %in% c(6, 7, 8) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
    is.finite(df_obs$Qh),
    c("Date", "Qh")
     ]
  
  # Calendar day in the station's local time zone
  obs_max_jja_Qh$Day <- as.Date(obs_max_jja_Qh$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_jja_Qh$Date,
    obs_max_jja_Qh$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_jja_Qh <- obs_max_jja_Qh[
    as.character(obs_max_jja_Qh$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest Qh
  obs_max_jja_Qh <- obs_max_jja_Qh[order(obs_max_jja_Qh$Day, -obs_max_jja_Qh$Qh, obs_max_jja_Qh$Date),]
  
  # Keep the first, and therefore highest-Qh, row for each day
  obs_max_jja_Qh <- obs_max_jja_Qh[!duplicated(obs_max_jja_Qh$Day), c("Day", "Date", "Qh")]
  
  # Rename the observation column
  names(obs_max_jja_Qh)[names(obs_max_jja_Qh) == "Qh"] <- "Observations"
  df_infos[i,5] <- length(obs_max_jja_Qh$Observations)
  
  
  # Qle max
  obs_max_jja_Qle <- df_obs[month(df_obs$Date) %in% c(6, 7, 8) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                             is.finite(df_obs$Qle),
                           c("Date", "Qle")
  ]
  obs_max_jja_Qle$Day <- as.Date(obs_max_jja_Qle$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_jja_Qle$Date,
    obs_max_jja_Qle$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_jja_Qle <- obs_max_jja_Qle[
    as.character(obs_max_jja_Qle$Day) %in% complete_days,
    ]
  obs_max_jja_Qle <- obs_max_jja_Qle[order(obs_max_jja_Qle$Day, -obs_max_jja_Qle$Qle, obs_max_jja_Qle$Date),]
  obs_max_jja_Qle <- obs_max_jja_Qle[!duplicated(obs_max_jja_Qle$Day), c("Day", "Date", "Qle")]
  names(obs_max_jja_Qle)[names(obs_max_jja_Qle) == "Qle"] <- "Observations"
  df_infos[i,6] <- length(obs_max_jja_Qle$Observations)
  
  # Rnet max
  obs_max_jja_Rnet <- df_obs[month(df_obs$Date) %in% c(6, 7, 8) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$Rnet),
                            c("Date", "Rnet")
  ]
  obs_max_jja_Rnet$Day <- as.Date(obs_max_jja_Rnet$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_jja_Rnet$Date,
    obs_max_jja_Rnet$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_jja_Rnet <- obs_max_jja_Rnet[
    as.character(obs_max_jja_Rnet$Day) %in% complete_days,
    ]
  obs_max_jja_Rnet <- obs_max_jja_Rnet[order(obs_max_jja_Rnet$Day, -obs_max_jja_Rnet$Rnet, obs_max_jja_Rnet$Date),]
  obs_max_jja_Rnet <- obs_max_jja_Rnet[!duplicated(obs_max_jja_Rnet$Day), c("Day", "Date", "Rnet")]
  names(obs_max_jja_Rnet)[names(obs_max_jja_Rnet) == "Rnet"] <- "Observations"
  df_infos[i,7] <- length(obs_max_jja_Rnet$Observations)

  # SWup max
  obs_max_jja_SWup <- df_obs[month(df_obs$Date) %in% c(6, 7, 8) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$SWup),
                            c("Date", "SWup")
  ]
  obs_max_jja_SWup$Day <- as.Date(obs_max_jja_SWup$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_jja_SWup$Date,
    obs_max_jja_SWup$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_jja_SWup <- obs_max_jja_SWup[
    as.character(obs_max_jja_SWup$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest SWup
  obs_max_jja_SWup <- obs_max_jja_SWup[
    order(obs_max_jja_SWup$Day, -obs_max_jja_SWup$SWup, obs_max_jja_SWup$Date),
    ]
  
  # Keep the highest value for each day
  obs_max_jja_SWup <- obs_max_jja_SWup[
    !duplicated(obs_max_jja_SWup$Day),
    c("Day", "Date", "SWup")
  ]
  
  names(obs_max_jja_SWup)[
    names(obs_max_jja_SWup) == "SWup"
  ] <- "Observations"
  
  rownames(obs_max_jja_SWup) <- NULL
  
  df_infos[i, 18] <- nrow(obs_max_jja_SWup)

  # LWup max
  obs_max_jja_LWup <- df_obs[month(df_obs$Date) %in% c(6, 7, 8) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$LWup),
                            c("Date", "LWup")
  ]
  obs_max_jja_LWup$Day <- as.Date(obs_max_jja_LWup$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_jja_LWup$Date,
    obs_max_jja_LWup$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_jja_LWup <- obs_max_jja_LWup[
    as.character(obs_max_jja_LWup$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest LWup
  obs_max_jja_LWup <- obs_max_jja_LWup[
    order(obs_max_jja_LWup$Day, -obs_max_jja_LWup$LWup, obs_max_jja_LWup$Date),
    ]
  
  # Keep the highest value for each day
  obs_max_jja_LWup <- obs_max_jja_LWup[
    !duplicated(obs_max_jja_LWup$Day),
    c("Day", "Date", "LWup")
  ]
  
  names(obs_max_jja_LWup)[
    names(obs_max_jja_LWup) == "LWup"
  ] <- "Observations"
  
  rownames(obs_max_jja_LWup) <- NULL
  
  df_infos[i, 19] <- nrow(obs_max_jja_LWup)

  # G max
  obs_max_jja_G <- df_obs[month(df_obs$Date) %in% c(6, 7, 8) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$G),
                            c("Date", "G")
  ]
  obs_max_jja_G$Day <- as.Date(obs_max_jja_G$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_jja_G$Date,
    obs_max_jja_G$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_jja_G <- obs_max_jja_G[
    as.character(obs_max_jja_G$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest G
  obs_max_jja_G <- obs_max_jja_G[
    order(obs_max_jja_G$Day, -obs_max_jja_G$G, obs_max_jja_G$Date),
    ]
  
  # Keep the highest value for each day
  obs_max_jja_G <- obs_max_jja_G[
    !duplicated(obs_max_jja_G$Day),
    c("Day", "Date", "G")
  ]
  
  names(obs_max_jja_G)[
    names(obs_max_jja_G) == "G"
  ] <- "Observations"
  
  rownames(obs_max_jja_G) <- NULL
  
  df_infos[i, 20] <- nrow(obs_max_jja_G)
  
  # Qh min
  # Define the date of the night to which each time step belongs
  night_day <- as.Date(
    df_obs$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  night_hours <- (
    hour(df_obs$Date) >= minimum_start_hour |
      hour(df_obs$Date) < minimum_end_hour
  )
  
  obs_min_jja_Qh <- df_obs[
    month(night_day) %in% c(6, 7, 8) &
      night_hours &
      is.finite(df_obs$Qh),
    c("Date", "Qh")
  ]
  
  # Associate the early morning with the preceding evening
  obs_min_jja_Qh$Day <- as.Date(
    obs_min_jja_Qh$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_jja_Qh$Date,
    obs_min_jja_Qh$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_jja_Qh <- obs_min_jja_Qh[
    as.character(obs_min_jja_Qh$Day) %in% complete_nights,
    ]
  
  # Sort from lowest to highest Qh
  obs_min_jja_Qh <- obs_min_jja_Qh[
    order(
      obs_min_jja_Qh$Day,
      obs_min_jja_Qh$Qh,
      obs_min_jja_Qh$Date
    ),
  ]
  
  # Keep the lowest value for each night
  obs_min_jja_Qh <- obs_min_jja_Qh[
    !duplicated(obs_min_jja_Qh$Day),
    c("Day", "Date", "Qh")
  ]
  
  names(obs_min_jja_Qh)[
    names(obs_min_jja_Qh) == "Qh"
  ] <- "Observations"
  
  rownames(obs_min_jja_Qh) <- NULL
  
  df_infos[i, 8] <- nrow(obs_min_jja_Qh)
  
  # Qle min
  obs_min_jja_Qle <- df_obs[
    month(night_day) %in% c(6, 7, 8) &
      night_hours &
      is.finite(df_obs$Qle),
    c("Date", "Qle")
  ]
  
  obs_min_jja_Qle$Day <- as.Date(
    obs_min_jja_Qle$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_jja_Qle$Date,
    obs_min_jja_Qle$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_jja_Qle <- obs_min_jja_Qle[
    as.character(obs_min_jja_Qle$Day) %in% complete_nights,
    ]
  
  obs_min_jja_Qle <- obs_min_jja_Qle[
    order(
      obs_min_jja_Qle$Day,
      obs_min_jja_Qle$Qle,
      obs_min_jja_Qle$Date
    ),
  ]
  
  obs_min_jja_Qle <- obs_min_jja_Qle[
    !duplicated(obs_min_jja_Qle$Day),
    c("Day", "Date", "Qle")
  ]
  
  names(obs_min_jja_Qle)[
    names(obs_min_jja_Qle) == "Qle"
  ] <- "Observations"
  
  rownames(obs_min_jja_Qle) <- NULL
  
  df_infos[i, 9] <- nrow(obs_min_jja_Qle)

  # LWup min
  obs_min_jja_LWup <- df_obs[
    month(night_day) %in% c(6, 7, 8) &
      night_hours &
      is.finite(df_obs$LWup),
    c("Date", "LWup")
  ]
  
  obs_min_jja_LWup$Day <- as.Date(
    obs_min_jja_LWup$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_jja_LWup$Date,
    obs_min_jja_LWup$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_jja_LWup <- obs_min_jja_LWup[
    as.character(obs_min_jja_LWup$Day) %in% complete_nights,
    ]
  
  obs_min_jja_LWup <- obs_min_jja_LWup[
    order(
      obs_min_jja_LWup$Day,
      obs_min_jja_LWup$LWup,
      obs_min_jja_LWup$Date
    ),
  ]
  
  obs_min_jja_LWup <- obs_min_jja_LWup[
    !duplicated(obs_min_jja_LWup$Day),
    c("Day", "Date", "LWup")
  ]
  
  names(obs_min_jja_LWup)[
    names(obs_min_jja_LWup) == "LWup"
  ] <- "Observations"
  
  rownames(obs_min_jja_LWup) <- NULL
  
  df_infos[i, 21] <- nrow(obs_min_jja_LWup)

  # G min
  obs_min_jja_G <- df_obs[
    month(night_day) %in% c(6, 7, 8) &
      night_hours &
      is.finite(df_obs$G),
    c("Date", "G")
  ]
  
  obs_min_jja_G$Day <- as.Date(
    obs_min_jja_G$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_jja_G$Date,
    obs_min_jja_G$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_jja_G <- obs_min_jja_G[
    as.character(obs_min_jja_G$Day) %in% complete_nights,
    ]
  
  obs_min_jja_G <- obs_min_jja_G[
    order(
      obs_min_jja_G$Day,
      obs_min_jja_G$G,
      obs_min_jja_G$Date
    ),
  ]
  
  obs_min_jja_G <- obs_min_jja_G[
    !duplicated(obs_min_jja_G$Day),
    c("Day", "Date", "G")
  ]
  
  names(obs_min_jja_G)[
    names(obs_min_jja_G) == "G"
  ] <- "Observations"
  
  rownames(obs_min_jja_G) <- NULL
  
  df_infos[i, 22] <- nrow(obs_min_jja_G)
  
  #-----------------------------------------------------------------------------
  # DJF
  
  # Qh max
  obs_max_djf_Qh <- df_obs[month(df_obs$Date) %in% c(12, 1, 2) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                             is.finite(df_obs$Qh),
                           c("Date", "Qh")
  ]
  
  # Calendar day in the station's local time zone
  obs_max_djf_Qh$Day <- as.Date(obs_max_djf_Qh$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_djf_Qh$Date,
    obs_max_djf_Qh$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_djf_Qh <- obs_max_djf_Qh[
    as.character(obs_max_djf_Qh$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest Qh
  obs_max_djf_Qh <- obs_max_djf_Qh[order(obs_max_djf_Qh$Day, -obs_max_djf_Qh$Qh, obs_max_djf_Qh$Date),]
  
  # Keep the first, and therefore highest-Qh, row for each day
  obs_max_djf_Qh <- obs_max_djf_Qh[!duplicated(obs_max_djf_Qh$Day), c("Day", "Date", "Qh")]
  
  # Rename the observation column
  names(obs_max_djf_Qh)[names(obs_max_djf_Qh) == "Qh"] <- "Observations"
  df_infos[i,10] <- length(obs_max_djf_Qh$Observations)
  
  
  # Qle max
  obs_max_djf_Qle <- df_obs[month(df_obs$Date) %in% c(12, 1, 2) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$Qle),
                            c("Date", "Qle")
  ]
  obs_max_djf_Qle$Day <- as.Date(obs_max_djf_Qle$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_djf_Qle$Date,
    obs_max_djf_Qle$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_djf_Qle <- obs_max_djf_Qle[
    as.character(obs_max_djf_Qle$Day) %in% complete_days,
    ]
  obs_max_djf_Qle <- obs_max_djf_Qle[order(obs_max_djf_Qle$Day, -obs_max_djf_Qle$Qle, obs_max_djf_Qle$Date),]
  obs_max_djf_Qle <- obs_max_djf_Qle[!duplicated(obs_max_djf_Qle$Day), c("Day", "Date", "Qle")]
  names(obs_max_djf_Qle)[names(obs_max_djf_Qle) == "Qle"] <- "Observations"
  df_infos[i,11] <- length(obs_max_djf_Qle$Observations)
  
  # Rnet max
  obs_max_djf_Rnet <- df_obs[month(df_obs$Date) %in% c(12, 1, 2) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                               is.finite(df_obs$Rnet),
                             c("Date", "Rnet")
  ]
  obs_max_djf_Rnet$Day <- as.Date(obs_max_djf_Rnet$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_djf_Rnet$Date,
    obs_max_djf_Rnet$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_djf_Rnet <- obs_max_djf_Rnet[
    as.character(obs_max_djf_Rnet$Day) %in% complete_days,
    ]
  obs_max_djf_Rnet <- obs_max_djf_Rnet[order(obs_max_djf_Rnet$Day, -obs_max_djf_Rnet$Rnet, obs_max_djf_Rnet$Date),]
  obs_max_djf_Rnet <- obs_max_djf_Rnet[!duplicated(obs_max_djf_Rnet$Day), c("Day", "Date", "Rnet")]
  names(obs_max_djf_Rnet)[names(obs_max_djf_Rnet) == "Rnet"] <- "Observations"
  df_infos[i,12] <- length(obs_max_djf_Rnet$Observations)

  # SWup max
  obs_max_djf_SWup <- df_obs[month(df_obs$Date) %in% c(12, 1, 2) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$SWup),
                            c("Date", "SWup")
  ]
  obs_max_djf_SWup$Day <- as.Date(obs_max_djf_SWup$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_djf_SWup$Date,
    obs_max_djf_SWup$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_djf_SWup <- obs_max_djf_SWup[
    as.character(obs_max_djf_SWup$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest SWup
  obs_max_djf_SWup <- obs_max_djf_SWup[
    order(obs_max_djf_SWup$Day, -obs_max_djf_SWup$SWup, obs_max_djf_SWup$Date),
    ]
  
  # Keep the highest value for each day
  obs_max_djf_SWup <- obs_max_djf_SWup[
    !duplicated(obs_max_djf_SWup$Day),
    c("Day", "Date", "SWup")
  ]
  
  names(obs_max_djf_SWup)[
    names(obs_max_djf_SWup) == "SWup"
  ] <- "Observations"
  
  rownames(obs_max_djf_SWup) <- NULL
  
  df_infos[i, 23] <- nrow(obs_max_djf_SWup)

  # LWup max
  obs_max_djf_LWup <- df_obs[month(df_obs$Date) %in% c(12, 1, 2) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$LWup),
                            c("Date", "LWup")
  ]
  obs_max_djf_LWup$Day <- as.Date(obs_max_djf_LWup$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_djf_LWup$Date,
    obs_max_djf_LWup$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_djf_LWup <- obs_max_djf_LWup[
    as.character(obs_max_djf_LWup$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest LWup
  obs_max_djf_LWup <- obs_max_djf_LWup[
    order(obs_max_djf_LWup$Day, -obs_max_djf_LWup$LWup, obs_max_djf_LWup$Date),
    ]
  
  # Keep the highest value for each day
  obs_max_djf_LWup <- obs_max_djf_LWup[
    !duplicated(obs_max_djf_LWup$Day),
    c("Day", "Date", "LWup")
  ]
  
  names(obs_max_djf_LWup)[
    names(obs_max_djf_LWup) == "LWup"
  ] <- "Observations"
  
  rownames(obs_max_djf_LWup) <- NULL
  
  df_infos[i, 24] <- nrow(obs_max_djf_LWup)

  # G max
  obs_max_djf_G <- df_obs[month(df_obs$Date) %in% c(12, 1, 2) & hour(df_obs$Date) >= maximum_start_hour & hour(df_obs$Date) < maximum_end_hour &
                              is.finite(df_obs$G),
                            c("Date", "G")
  ]
  obs_max_djf_G$Day <- as.Date(obs_max_djf_G$Date, tz = tz)
  
  # Keep the day only if every expected midday time step is present and finite
  number_values_by_day <- tapply(
    obs_max_djf_G$Date,
    obs_max_djf_G$Day,
    function(x) length(unique(x))
  )
  complete_days <- names(
    number_values_by_day[number_values_by_day == expected_maximum_steps]
  )
  obs_max_djf_G <- obs_max_djf_G[
    as.character(obs_max_djf_G$Day) %in% complete_days,
    ]
  
  # Sort by day, then from highest to lowest G
  obs_max_djf_G <- obs_max_djf_G[
    order(obs_max_djf_G$Day, -obs_max_djf_G$G, obs_max_djf_G$Date),
    ]
  
  # Keep the highest value for each day
  obs_max_djf_G <- obs_max_djf_G[
    !duplicated(obs_max_djf_G$Day),
    c("Day", "Date", "G")
  ]
  
  names(obs_max_djf_G)[
    names(obs_max_djf_G) == "G"
  ] <- "Observations"
  
  rownames(obs_max_djf_G) <- NULL
  
  df_infos[i, 25] <- nrow(obs_max_djf_G)
  
  # Qh min
  # Define the date of the night to which each time step belongs
  night_day <- as.Date(
    df_obs$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  night_hours <- (
    hour(df_obs$Date) >= minimum_start_hour |
      hour(df_obs$Date) < minimum_end_hour
  )
  
  obs_min_djf_Qh <- df_obs[
    month(night_day) %in% c(12, 1, 2) &
      night_hours &
      is.finite(df_obs$Qh),
    c("Date", "Qh")
  ]
  
  # Associate the early morning with the preceding evening
  obs_min_djf_Qh$Day <- as.Date(
    obs_min_djf_Qh$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_djf_Qh$Date,
    obs_min_djf_Qh$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_djf_Qh <- obs_min_djf_Qh[
    as.character(obs_min_djf_Qh$Day) %in% complete_nights,
    ]
  
  # Sort from lowest to highest Qh
  obs_min_djf_Qh <- obs_min_djf_Qh[
    order(
      obs_min_djf_Qh$Day,
      obs_min_djf_Qh$Qh,
      obs_min_djf_Qh$Date
    ),
  ]
  
  # Keep the lowest value for each night
  obs_min_djf_Qh <- obs_min_djf_Qh[
    !duplicated(obs_min_djf_Qh$Day),
    c("Day", "Date", "Qh")
  ]
  
  names(obs_min_djf_Qh)[
    names(obs_min_djf_Qh) == "Qh"
  ] <- "Observations"
  
  rownames(obs_min_djf_Qh) <- NULL
  
  df_infos[i, 13] <- nrow(obs_min_djf_Qh)
  
  # Qle min
  obs_min_djf_Qle <- df_obs[
    month(night_day) %in% c(12, 1, 2) &
      night_hours &
      is.finite(df_obs$Qle),
    c("Date", "Qle")
  ]
  
  obs_min_djf_Qle$Day <- as.Date(
    obs_min_djf_Qle$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_djf_Qle$Date,
    obs_min_djf_Qle$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_djf_Qle <- obs_min_djf_Qle[
    as.character(obs_min_djf_Qle$Day) %in% complete_nights,
    ]
  
  obs_min_djf_Qle <- obs_min_djf_Qle[
    order(
      obs_min_djf_Qle$Day,
      obs_min_djf_Qle$Qle,
      obs_min_djf_Qle$Date
    ),
  ]
  
  obs_min_djf_Qle <- obs_min_djf_Qle[
    !duplicated(obs_min_djf_Qle$Day),
    c("Day", "Date", "Qle")
  ]
  
  names(obs_min_djf_Qle)[
    names(obs_min_djf_Qle) == "Qle"
  ] <- "Observations"
  
  rownames(obs_min_djf_Qle) <- NULL
  
  df_infos[i, 14] <- nrow(obs_min_djf_Qle)

  # LWup min
  obs_min_djf_LWup <- df_obs[
    month(night_day) %in% c(12, 1, 2) &
      night_hours &
      is.finite(df_obs$LWup),
    c("Date", "LWup")
  ]
  
  obs_min_djf_LWup$Day <- as.Date(
    obs_min_djf_LWup$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_djf_LWup$Date,
    obs_min_djf_LWup$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_djf_LWup <- obs_min_djf_LWup[
    as.character(obs_min_djf_LWup$Day) %in% complete_nights,
    ]
  
  obs_min_djf_LWup <- obs_min_djf_LWup[
    order(
      obs_min_djf_LWup$Day,
      obs_min_djf_LWup$LWup,
      obs_min_djf_LWup$Date
    ),
  ]
  
  obs_min_djf_LWup <- obs_min_djf_LWup[
    !duplicated(obs_min_djf_LWup$Day),
    c("Day", "Date", "LWup")
  ]
  
  names(obs_min_djf_LWup)[
    names(obs_min_djf_LWup) == "LWup"
  ] <- "Observations"
  
  rownames(obs_min_djf_LWup) <- NULL
  
  df_infos[i, 26] <- nrow(obs_min_djf_LWup)

  # G min
  obs_min_djf_G <- df_obs[
    month(night_day) %in% c(12, 1, 2) &
      night_hours &
      is.finite(df_obs$G),
    c("Date", "G")
  ]
  
  obs_min_djf_G$Day <- as.Date(
    obs_min_djf_G$Date - hours(minimum_end_hour),
    tz = tz
  )
  
  # Keep the night only if every expected nighttime step is present and finite
  number_values_by_night <- tapply(
    obs_min_djf_G$Date,
    obs_min_djf_G$Day,
    function(x) length(unique(x))
  )
  complete_nights <- names(
    number_values_by_night[number_values_by_night == expected_minimum_steps]
  )
  obs_min_djf_G <- obs_min_djf_G[
    as.character(obs_min_djf_G$Day) %in% complete_nights,
    ]
  
  obs_min_djf_G <- obs_min_djf_G[
    order(
      obs_min_djf_G$Day,
      obs_min_djf_G$G,
      obs_min_djf_G$Date
    ),
  ]
  
  obs_min_djf_G <- obs_min_djf_G[
    !duplicated(obs_min_djf_G$Day),
    c("Day", "Date", "G")
  ]
  
  names(obs_min_djf_G)[
    names(obs_min_djf_G) == "G"
  ] <- "Observations"
  
  rownames(obs_min_djf_G) <- NULL
  
  df_infos[i, 27] <- nrow(obs_min_djf_G)
  
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  
  # Now we look at the simulations
  
  for (type in names(simulation_names)) {
    #type <- "Current"
    sim <- simulation_names[[type]]
    
    lower_station <- tolower(station)
    
    simulation_file <- paste0(folder_sim, sim, "_", lower_station, "/land_jsbach_sitelevel_", sim, "_",
                              lower_station, "_lnd_basic_", lower_station, "_ml_", yrange, ".nc")
    
    nc_sim <- nc_open(simulation_file)
    
    time_vals <- ncvar_get(nc_sim, "time")
    time_units <- ncatt_get(nc_sim, "time", "units")$value
    origin_date <- sub(".*since ", "", time_units)
    
    datetime_vals <- as.POSIXct(origin_date, tryFormats = c("%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S"), tz = "UTC") +
      as.difftime(time_vals, units = "mins")
    datetime_vals <- with_tz(datetime_vals, tz = tz)
    
    Qh_sim    <- as.numeric(-ncvar_get(nc_sim, "seb_sensible_hflx_box")) # opposite sign in ICON-Land
    Qle_sim   <- as.numeric(-ncvar_get(nc_sim, "seb_latent_hflx_box")) # opposite sign in ICON-Land
    G_sim     <- as.numeric(ncvar_get(nc_sim, "sse_grnd_hflx_box"))
    SWnet_sim <- as.numeric(ncvar_get(nc_sim, "rad_sw_srf_net_box"))
    LWnet_sim <- as.numeric(ncvar_get(nc_sim, "rad_lw_srf_net_box"))
    
    nc_close(nc_sim)
    
    simulation_dates_local <- with_tz(datetime_vals, tzone = tz)
    
    # Match the observed incoming radiation to the simulation timestamps.
    # The simulations use the observed atmospheric forcing, so simulated
    # upwelling radiation is diagnosed from observed downwelling radiation
    # minus simulated net radiation.
    matching_observation_rows <- match(
      round(as.numeric(simulation_dates_local)),
      round(as.numeric(df_obs$Date))
    )
    
    SWup_sim <- df_obs$SWdown[matching_observation_rows] - SWnet_sim
    LWup_sim <- df_obs$LWdown[matching_observation_rows] - LWnet_sim
    
    df_sim <- data.frame(
      Date = simulation_dates_local,
      Qh   = Qh_sim,
      Qle  = Qle_sim,
      Rnet = SWnet_sim + LWnet_sim,
      SWup = SWup_sim,
      LWup = LWup_sim,
      G    = G_sim
    )
    
    # Check that all simulation variables have the expected length
    stopifnot(
      nrow(df_sim) == length(Qh_sim),
      nrow(df_sim) == length(Qle_sim),
      nrow(df_sim) == length(G_sim),
      nrow(df_sim) == length(SWnet_sim),
      nrow(df_sim) == length(LWnet_sim),
      nrow(df_sim) == length(SWup_sim),
      nrow(df_sim) == length(LWup_sim)
    )
    
    # JJA maximum tables
    obs_max_jja_Qh <- fill_simulation_column(
      obs_max_jja_Qh, df_sim$Date, df_sim$Qh,
      type, station, "JJA maximum Qh", "maximum", tz
    )
    
    obs_max_jja_Qle <- fill_simulation_column(
      obs_max_jja_Qle, df_sim$Date, df_sim$Qle,
      type, station, "JJA maximum Qle", "maximum", tz
    )
    
    obs_max_jja_Rnet <- fill_simulation_column(
      obs_max_jja_Rnet, df_sim$Date, df_sim$Rnet,
      type, station, "JJA maximum Rnet", "maximum", tz
    )

    obs_max_jja_SWup <- fill_simulation_column(
      obs_max_jja_SWup, df_sim$Date, df_sim$SWup,
      type, station, "JJA maximum SWup", "maximum", tz
    )
    
    obs_max_jja_LWup <- fill_simulation_column(
      obs_max_jja_LWup, df_sim$Date, df_sim$LWup,
      type, station, "JJA maximum LWup", "maximum", tz
    )
    
    obs_max_jja_G <- fill_simulation_column(
      obs_max_jja_G, df_sim$Date, df_sim$G,
      type, station, "JJA maximum G", "maximum", tz
    )
    
    # JJA minimum tables
    obs_min_jja_Qh <- fill_simulation_column(
      obs_min_jja_Qh, df_sim$Date, df_sim$Qh,
      type, station, "JJA minimum Qh", "minimum", tz
    )
    
    obs_min_jja_Qle <- fill_simulation_column(
      obs_min_jja_Qle, df_sim$Date, df_sim$Qle,
      type, station, "JJA minimum Qle", "minimum", tz
    )

    obs_min_jja_LWup <- fill_simulation_column(
      obs_min_jja_LWup, df_sim$Date, df_sim$LWup,
      type, station, "JJA minimum LWup", "minimum", tz
    )
    
    obs_min_jja_G <- fill_simulation_column(
      obs_min_jja_G, df_sim$Date, df_sim$G,
      type, station, "JJA minimum G", "minimum", tz
    )
    
    # DJF maximum tables
    obs_max_djf_Qh <- fill_simulation_column(
      obs_max_djf_Qh, df_sim$Date, df_sim$Qh,
      type, station, "DJF maximum Qh", "maximum", tz
    )
    
    obs_max_djf_Qle <- fill_simulation_column(
      obs_max_djf_Qle, df_sim$Date, df_sim$Qle,
      type, station, "DJF maximum Qle", "maximum", tz
    )
    
    obs_max_djf_Rnet <- fill_simulation_column(
      obs_max_djf_Rnet, df_sim$Date, df_sim$Rnet,
      type, station, "DJF maximum Rnet", "maximum", tz
    )

    obs_max_djf_SWup <- fill_simulation_column(
      obs_max_djf_SWup, df_sim$Date, df_sim$SWup,
      type, station, "DJF maximum SWup", "maximum", tz
    )
    
    obs_max_djf_LWup <- fill_simulation_column(
      obs_max_djf_LWup, df_sim$Date, df_sim$LWup,
      type, station, "DJF maximum LWup", "maximum", tz
    )
    
    obs_max_djf_G <- fill_simulation_column(
      obs_max_djf_G, df_sim$Date, df_sim$G,
      type, station, "DJF maximum G", "maximum", tz
    )
    
    # DJF minimum tables
    obs_min_djf_Qh <- fill_simulation_column(
      obs_min_djf_Qh, df_sim$Date, df_sim$Qh,
      type, station, "DJF minimum Qh", "minimum", tz
    )
    
    obs_min_djf_Qle <- fill_simulation_column(
      obs_min_djf_Qle, df_sim$Date, df_sim$Qle,
      type, station, "DJF minimum Qle", "minimum", tz
    )

    obs_min_djf_LWup <- fill_simulation_column(
      obs_min_djf_LWup, df_sim$Date, df_sim$LWup,
      type, station, "DJF minimum LWup", "minimum", tz
    )
    
    obs_min_djf_G <- fill_simulation_column(
      obs_min_djf_G, df_sim$Date, df_sim$G,
      type, station, "DJF minimum G", "minimum", tz
    )
  }
  
  #---------------------------------------------------------------------------
  # Save all tables for this station
  
  tables_to_save <- list(
    Qh_max_JJA    = obs_max_jja_Qh,
    Qle_max_JJA   = obs_max_jja_Qle,
    Rnet_max_JJA  = obs_max_jja_Rnet,
    SWup_max_JJA  = obs_max_jja_SWup,
    LWup_max_JJA  = obs_max_jja_LWup,
    G_max_JJA     = obs_max_jja_G,
    Qh_min_JJA    = obs_min_jja_Qh,
    Qle_min_JJA   = obs_min_jja_Qle,
    LWup_min_JJA  = obs_min_jja_LWup,
    G_min_JJA     = obs_min_jja_G,
    Qh_max_DJF    = obs_max_djf_Qh,
    Qle_max_DJF   = obs_max_djf_Qle,
    Rnet_max_DJF  = obs_max_djf_Rnet,
    SWup_max_DJF  = obs_max_djf_SWup,
    LWup_max_DJF  = obs_max_djf_LWup,
    G_max_DJF     = obs_max_djf_G,
    Qh_min_DJF    = obs_min_djf_Qh,
    Qle_min_DJF   = obs_min_djf_Qle,
    LWup_min_DJF  = obs_min_djf_LWup,
    G_min_DJF     = obs_min_djf_G
  )
  
  for (table_name in names(tables_to_save)) {
    
    table_to_write <- tables_to_save[[table_name]]
    
    # Save the timestamp explicitly in the station's local time
    table_to_write$Date <- format(
      table_to_write$Date,
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )
    
    output_file <- file.path(
      output_folder,
      paste0(station, "_", table_name, ".csv")
    )
    
    write.csv(
      table_to_write,
      output_file,
      row.names = FALSE
    )
  }
  
  message("Tables saved for ", station)
  
}

df_infos_output <- data.frame(
  Station = rownames(df_infos),
  df_infos,
  row.names = NULL,
  check.names = FALSE
)

write.csv(
  df_infos_output,
  file.path(output_folder, "Table_data_availability.csv"),
  row.names = FALSE
)
