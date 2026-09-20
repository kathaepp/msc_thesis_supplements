# Supplements to M.Sc. Thesis

## Overview
This repository contains the scripts used for analysis and visualization as well as a detailed explanation of file adaptations in my master thesis "Towards an Urban Scheme in ICON-Land".

## Files in this Repository
- `File_adaptations.md` contains detailed explanations of the preparation of the initial and boundary condition (ic/bc) files and the model runscripts
- `analysis_diurnal.ipynb` contains the code used to calculate and plot the mean diurnal cycles of surface energy fluxes at the AU-Preston site
- `Create_tables_and_individual_plots_for_figure_bias_distributions.R` and `Create_final_tables_for_figure_bias_distributions.R` contain the code used to calculate the mean bias distributions of simulated surface energy fluxes
- `Plot_figure_bias_distributions.R` contains the code used to plot the mean bias distributions of simulated surface energy fluxes
- `Compute_MAE_Qh_Qe.R` contains the code used to calculate the mean absolute error (MAE) of the simulated sensible and latent heat fluxes
- `Plot_figure_MAE_Qh_Qe.R` contains the code used to create the plots visualizing the MAE of the simulated sensible and latent heat fluxes
- `Compute_precipitation_fractions.R` contains the code used to calculate the precipitation fractioning
- `Plot_figure_precipitation_fractions.R` contains the code used to plots visualizing the precipitation fractioning
- `inspect_spinup.ipynb` contains the code used to plot the temporal evolution of total land water and ice content
