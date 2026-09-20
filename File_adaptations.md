This file details the technical steps taken to prepare the initial and boundary condition (ic/bc) files and the ICON-Land runscripts for our simulations.

# Preparation of ic/bc files:
- First, we used Zonda (Center for Climate Systems Modeling, 2026b) to select small grids in the area around the given flux tower site from the R5B10 grid (resolution apx. 0.99 km).
- Next, we used these grids to obtain the corresponding ic/bc files using Extpar (Center for Climate Systems Modeling, 2026a).
- Then, we used the Python function "iconarray.indfromlatlon" to find the index of the single cell closest to the flux tower station, and the cdo command "selgridcell" to select these single cells from the ic/bc files.
- Finally, we adapted the following variables to the values provided in the Urban-PLUMBER dataset (Lipson et al., 2022):
    - FR_CLAY: topsoil_clay_fraction
    - FR_SAND: topsoil_sand_fraction
    - FR_SILT: 1 - topsoil_clay_fraction - topsoil_sand_fraction
    - elevation: ground_height
    - oromea: ground_height
    - veg_ratio_max: see Appendix A.2 of thesis
    - pft_fractxx: see Appendix A.2 of thesis
- For the following variables, we assumed these values at all sites:
    - notsea: 1
    - sea: 0
    - fract_land: 1
    - land: 1
    - fract_lake: 0
    - lake: 0
    - fract_glac: 0
    - glac: 0
    - fract_veg: 1

# Preparation of runscripts:
- We created one .exp file per station and simulation based on the exp.land_jsbach_sitelevel_test file provided by MPI-M (2026).
- The following adjustments were made for all simulations and sites:
    - l_soil_texture: .TRUE. (in both the sse and hydro namelists)
    - hydro_scale: "uniform"
    - restart_interval: ""
    - output_interval: "PT30M"
    - file_interval: "P20Y"
    - num_land_sites: 1
    - rad_lyr_perp: .TRUE.
    - forcing_temp_frequ: "SUBDAILY"
    - forcing_precip_frequ: "SUBDAILY"
    - forcing_qair_frequ: "SUBDAILY"
    - forcing_lw_frequ: "SUBDAILY"
    - forcing_sw_frequ: "SUBDAILY"
    - forcing_wind_frequ: "SUBDAILY"
- The following variables were adapted accordingly for each site:
    - refyear (year of start_date)
    - start_date
    - end_date
    - forcing_steps_per_day (24 or 48 depending on data availability at the site)
    - forcing_synchron_factor (depending on forcing_steps_per_day)
    - land_sites
    - land_sites_dirs
    - land_sites_name
    - land_sites_lon
    - land_sites_lat
    - rad_yr_perp (year of start_date)
    


#References:

Center for Climate Systems Modeling. (2026a, July 8). EXTPAR Documentation. Retrieved
September 2, 2026, from https://c2sm.github.io/extpar/

Center for Climate Systems Modeling. (2026b). Zonda - ICON grid & EXTPAR interface.
Retrieved September 20, 2026, from https://zonda.ethz.ch/

Lipson, M., Grimmond, S., Best, M., Chow, W. T. L., Christen, A., Chrysoulakis, N., Coutts,
A., Crawford, B., Earl, S., Evans, J., Fortuniak, K., Heusinkveld, B. G., Hong, J.-W.,
Hong, J., Järvi, L., Jo, S., Kim, Y.-H., Kotthaus, S., Lee, K., . . . Ward, H. C. (2022).
Harmonized gap-filled datasets from 20 urban flux tower sites. Earth System Science
Data, 14 (11), 5157–5178. https://doi.org/10.5194/essd-14-5157-2022

Max Planck Institute for Meteorology. (2026, September 3). Icon / icon-mpim · GitLab. Re-
trieved September 3, 2026, from https://gitlab.dkrz.de/icon/icon-mpim
