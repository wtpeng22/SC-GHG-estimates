# Load BRICK Mimi components.
setwd("/Users/tianpeng/Desktop/IAMs_Rversion/GIVE_Rversion/src/")

brick_target_scenario="RCP85";
brick_start_year=1850;
brick_end_year=2300
brick_time = brick_start_year:brick_end_year

source("components/brickcomponents/antarctic_icesheet_component.R")
source("components/brickcomponents/antarctic_ocean_component.R")
source("components/brickcomponents/glaciers_small_icecaps_component.R")
source("components/brickcomponents/global_sea_level_component.R")
source("components/brickcomponents/greenland_icesheet_component.R")
source("components/brickcomponents/landwater_storage_component.R")
source("components/brickcomponents/thermal_expansion_component.R")

# include other helper functions
source("components/BRICKdownscale.R")

# # include calibration functions
# source("components/brickcalibration/main_calibration.R")


# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------
# Function to create 'Building blocks for Relevant Ice and Climate Knowledge' (BRICK) model.
# -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------

brick_get_model <- function(rcp_scenario="RCP85", start_year=1850, end_year=2020) {
  
  # Load exogenous time-series for global surface temperature and ocean heat content (output from SNEASY under RCP8.5).
  # NOTE: for now, only `rcp_scenario = "RCP85"` is supported
  temperature_scenario <<- read.csv(file.path("..", "data", "brick_model_data", paste0("sneasy_temperature_", rcp_scenario, "_1850_2300.csv")))
  oceanheat_scenario <<- read.csv(file.path("..", "data", "brick_model_data", paste0("sneasy_oceanheat_", rcp_scenario, "_1850_2300.csv")))
  
  # # Create a Mimi-model instance.
  # brick <<- Mimi.Model()
  # 
  # # Set model time horizon, defaults to 1850-2300.
  # Mimi.set_dimension!(brick, :time, start_year:end_year)
  
  # # Add in BRICK components.
  # Mimi.add_comp(brick, antarctic_ocean)
  # Mimi.add_comp(brick, antarctic_icesheet)
  # Mimi.add_comp(brick, glaciers_small_icecaps)
  # Mimi.add_comp(brick, greenland_icesheet)
  # Mimi.add_comp(brick, thermal_expansion)
  # Mimi.add_comp(brick, landwater_storage)
  # Mimi.add_comp(brick, global_sea_level)
  
  # ----- Assign Model Parameters ----- #
  
  # ----- Antarctic Ocean ----- #
  anto_alpha <<- 0.28
  anto_beta <<- 0.95
  
  # ----- Antarctic Ice Sheet -----
  ais_rho_ice <<- 917.0
  ais_rho_seawater <<- 1030.0
  ais_rho_rock <<- 4000.0
  ais_sea_level0 <<- 0.0
  ais_ocean_temperature0 <<- 0.72
  ais_radius0 <<- 1.864e6
  ais_bedheight0 <<- 781.0
  ais_slope <<- 0.0006
  ais_mu <<- 11.0
  ais_runoffline_snowheight0 <<- 1400.0
  ais_c <<- 100.0
  ais_precipitation0 <<- 0.37
  ais_kappa <<- 0.062
  ais_nu <<- 0.0086
  ais_iceflow0 <<- 1.2
  ais_gamma <<- 2.9
  ais_alpha <<- 0.23
  ais_temperature_coefficient <<- 0.8365
  ais_temperature_intercept <<- 15.42
  ais_local_fingerprint <<- -1.18
  ocean_surface_area <<- 3.619e14
  temperature_threshold <<- -15.0
  lambda <<- 0.0093
  include_ais_DSL <<- TRUE
  
  # ----- Glaciers & Small Ice Caps ----- #
  gsic_beta_0 <<- 0.0013
  gsic_v_0 <<- 0.376
  gsic_s_0 <<- -0.0138
  gsic_n <<- 0.847
  gsic_teq <<- -0.15
  
  # ----- Greenland Ice Sheet -----
  greenland_a <<- -1.37
  greenland_b <<- 8.06
  greenland_alpha <<- 0.0008
  greenland_beta <<- 0.00009
  greenland_v0 <<- 7.52
  
  # ----- Thermal Expansion -----
  te_A <<- 3.619e14
  te_C <<- 3991.86795711963
  te_rho <<- 1027.0
  te_alpha <<- 0.16
  te_s0 <<- 0.0
  oceanheat_idx <<- match(oceanheat_scenario$Year, start_year:end_year)
  # ocean_heat_mixed <<- rep(0, length(start_year:end_year))
  # # ocean_heat_interior <<- oceanheat_scenario[oceanheat_idx,]$MAP.Ocean.Heat
  # ocean_heat_interior <<- del_ohc_accum_brick
  
  ocean_heat_mixed <<- Hector_ssp245_slrheatmixed_brick
  ocean_heat_interior <<- Hector_ssp245_slrheatinterior_brick
  
  # update_param!(m, :thermal_expansion, :ocean_heat_mixed, zeros(length(model_first:model_last)))
  # connect_param!(m, :thermal_expansion, :ocean_heat_interior, :OceanHeatAccumulator, :del_ohc_accum)
  
  # ----- Landwater Storage ----- #
  lws0 <<- 0.0
  first_projection_year <<- 2018
  
  # lws_random_sample <<- rnorm(length(start_year:end_year), mean = 0.0003, sd = 0.00018)
  
  # ----- Set Parameters With Common Values Across Components -----
  # ----- Landwater Storage ----- #
  lws0 <<- 0.0
  first_projection_year <<- 2018
  first_projection_index <<- match(first_projection_year,brick_start_year:brick_end_year)
  lws_random_sample <<- rnorm(length(start_year:end_year), mean = 0.0003, sd = 0.00018)
  lws_random_sample <<- matrix(0.0003,nrow = length(start_year:end_year),ncol = 1)
  
  # ----- Set Parameters With Common Values Across Components ----- #
  model_seawater_freeze <<- -1.8
  seawater_freeze <<- model_seawater_freeze
  seawater_freeze <<- model_seawater_freeze
  temperature_idx <<- match(temperature_scenario$Year, (start_year:end_year))
  model_global_surface_temperature <<- temperature_scenario[temperature_idx,]$MAP.Temperature
  global_surface_temperature <<- model_global_surface_temperature
  
  #####load temperature simulated by the FAIR model 
  global_surface_temperature <<- fair_brick_temperature;
  
  #-----------------------------------------#
  #----- Create Component Connections ----- #
  #-----------------------------------------#
  
}
  
BRICK_model_run <-function(brick_time){
  for (t in 1:length(brick_time)) {
    antarctic_icesheet_run_timestep(t);
    antarctic_ocean_run_timestep(t);
    glaciers_small_icecaps_run_timestep(t);
    greenland_icesheet_run_timestep(t);
    landwater_storage_run_timestep(t);
    thermal_expansion_run_timestep(t);
    global_sea_level_run_timestep(t);
  }
}

# brick_get_model(target_scenario,start_year,end_year)
# BRICK_model_run(time)
# plot(time,sea_level_rise)
# sea_level_rise
  
  
  
                     

