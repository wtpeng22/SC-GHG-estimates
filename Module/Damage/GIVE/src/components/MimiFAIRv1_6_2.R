# include the original FAIR model that can be used for testing against original 
# Python - this exports a function get_model_original(;rcp_scenario::String="RCP85", 
# start_year::Int=1750, end_year::Int=2100)

library(dplyr)

setwd("/Users/tianpeng/Desktop/IAMs_Rversion/GIVE_Rversion/src/")

start_year = 1750
end_year = 2300
target_scenario = "ssp245"
time <- start_year:end_year

# ---------------------------------------------
#   Initialize Mimi model.
# ---------------------------------------------
time <- start_year:end_year

# includes a fair monte carlo function that replicates the Python AR6 behavior
source(paste("components","fairmcs", "AR6_Monte_Carlo.R", sep = "/"))

# Load helper functions and MimiFAIRv1.6.2 model commponent files.
source(paste("components","fair_helper_functions.R", sep = "/"))

source(paste("components", "faircomponents","ch4_cycle.R", sep = "/"))

source(paste("components", "faircomponents", "n2o_cycle.R", sep = "/"))

source(paste("components", "faircomponents", "co2_cycle.R", sep = "/"))

other_ghg_names <- c("CF4", "C2F6", "C6F14", "HFC23", "HFC32", "HFC43_10", "HFC125", "HFC134a", "HFC143a", "HFC227ea", "HFC245fa", "SF6")
other_ghg = other_ghg_names
source(paste("components", "faircomponents", "other_ghg_cycles.R", sep = "/"))

ods_names <- c("CFC_11", "CFC_12", "CFC_113", "CFC_114", "CFC_115", "CARB_TET", "MCF", "HCFC_22", "HCFC_141B", "HCFC_142B", "HALON1211", "HALON1202", "HALON1301", "HALON2402", "CH3BR", "CH3CL")
ozone_depleting_substances = ods_names
source(paste("components", "faircomponents", "o3_depleting_substance_cycles.R", sep = "/"))

adjust_F2x = TRUE
source(paste("components", "faircomponents", "co2_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "ch4_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "n2o_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "o3_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "aerosol_direct_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "aerosol_indirect_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "other_ghg_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "o3_depleting_substance_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "contrails_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "black_carbon_snow_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "landuse_forcing.R", sep = "/"))

source(paste("components", "faircomponents", "total_forcing.R", sep = "/"))
source(paste("components", "faircomponents", "temperature.R", sep = "/"))


FAIR_parameters_load <- function(sample_idx = 0){
  if(fair_parameter_set == "deterministic"){
    beta_CO <<- 0.0
    scale_CH4 <<- 1.0
    F_solar <<- ar6_solar_forcing
    psi_CH4 <<- 2.33379720e-04
    scale_N2O <<- 1.0
    CO2_pi <<- gas_data[gas_data$gas == "CO2", "pi_conc_ar6"][1]
    deep_ocean_efficacy <<- 1.28
    scale_bcsnow <<- 1.0
    scale_aerosol_direct_OC <<- 1.0
    b_SOx <<- 3.452849302362568
    feedback <<- -0.037
    scale_O3 <<- 1.0
    b_POM <<- 33.126485122209154
    r0_co2 <<- 35.0
    beta_NH3 <<- -1.55605e-3
    lambda_global <<- 1.18
    scale_landuse <<- 1.0
    scale_volcanic <<- 1.0
    scale_aerosol_direct_SOx <<- 1.0
    beta_NOx <<- -1.16551e-3
    psi_N2O <<- 1.27179106e-03
    ocean_heat_capacity <<- c(8.2, 109.0)
    beta_OC <<- -1.45339e-3
    scale_solar <<- 1.0
    rC_co2 <<- 0.019
    scale_aerosol_direct_BC <<- 1.0
    scale_CH4_H2O <<- 1.0
    scale_aerosol_indirect <<- 1.0
    scale_ods <<- rep(1, length(ods_names))
    psi_CO <<- 1.14647701e-04
    scale_aerosol_direct_NOx_NH3 <<- 1.0
    scale_other_ghg <<- rep(1, length(other_ghg_names))
    psi_NMVOC <<- 5.14366051e-12
    F2x <<- 3.71
    beta_SOx <<- -6.2227e-3
    beta_NMVOC <<- -3.8392e-4
    rT_co2 <<- 4.165
    beta_BC <<- 1.601537e-2
    scale_CO2 <<- 1.0
    psi_ODS <<- -6.69347820e-05
    scale_aerosol_direct_CO_NMVOC <<- 1.0
    psi_NOx <<- 3.78354423e-03
    ocean_heat_exchange <<- 0.67
    phi <<- 0.07334277994353743
  } else if(fair_parameter_set == "random"){
    beta_CO <<- as.numeric(fair_samples[["b_aero_CO"]][sample_idx,])
    scale_CH4 <<- as.numeric(fair_samples[["scale_CH4"]][sample_idx,])
    F_solar <<- ar6_solar_forcing
    F_solar[1:(2100-1750+1)] <<- as.numeric(fair_samples[["F_solar"]][sample_idx,])
    psi_CH4 <<- as.numeric(fair_samples[["b_tro3_CH4"]][sample_idx,])
    scale_N2O <<- as.numeric(fair_samples[["scale_N2O"]][sample_idx,])
    CO2_pi <<- as.numeric(fair_samples[["C_pi"]][sample_idx,])
    deep_ocean_efficacy <<- as.numeric(fair_samples[["deep_ocean_efficacy"]][sample_idx,])
    scale_bcsnow <<- as.numeric(fair_samples[["scale_bcsnow"]][sample_idx,])
    scale_aerosol_direct_OC <<- as.numeric(fair_samples[["scale_aerosol_direct_OC"]][sample_idx,])
    b_SOx <<- as.numeric(fair_samples[["ghan_params_SOx"]][sample_idx,])
    feedback <<- as.numeric(fair_samples[["ozone_feedback"]][sample_idx,])
    scale_O3 <<- as.numeric(fair_samples[["scale_O3"]][sample_idx,])
    b_POM <<- as.numeric(fair_samples[["ghan_params_b_POM"]][sample_idx,])
    r0_co2 <<- as.numeric(fair_samples[["r0"]][sample_idx,])
    beta_NH3 <<- as.numeric(fair_samples[["b_aero_NH3"]][sample_idx,])
    lambda_global <<- as.numeric(fair_samples[["lambda_global"]][sample_idx,])
    scale_landuse <<- as.numeric(fair_samples[["scale_landuse"]][sample_idx,])
    scale_volcanic <<- as.numeric(fair_samples[["scale_volcanic"]][sample_idx,])
    scale_aerosol_direct_SOx <<- as.numeric(fair_samples[["scale_aerosol_direct_SOx"]][sample_idx,])
    beta_NOx <<- as.numeric(fair_samples[["b_aero_NOx"]][sample_idx,])
    psi_N2O <<- as.numeric(fair_samples[["b_tro3_N2O"]][sample_idx,])
    ocean_heat_capacity <<- as.numeric(fair_samples[["ocean_heat_capacity"]][sample_idx,])
    beta_OC <<- as.numeric(fair_samples[["b_aero_OC"]][sample_idx,])
    scale_solar <<- as.numeric(fair_samples[["scale_solar"]][sample_idx,])
    rC_co2 <<- as.numeric(fair_samples[["rc"]][sample_idx,])
    scale_aerosol_direct_BC <<- as.numeric(fair_samples[["scale_aerosol_direct_BC"]][sample_idx,])
    scale_CH4_H2O <<- as.numeric(fair_samples[["scale_CH4_H2O"]][sample_idx,])
    scale_aerosol_indirect <<- as.numeric(fair_samples[["scale_aerosol_indirect"]][sample_idx,])
    scale_ods <<- as.numeric(fair_samples[["scale_ods"]][sample_idx,])
    psi_CO <<- as.numeric(fair_samples[["b_tro3_CO"]][sample_idx,])
    scale_aerosol_direct_NOx_NH3 <<- as.numeric(fair_samples[["scale_aerosol_direct_NOx_NH3"]][sample_idx,])
    scale_other_ghg <<- as.numeric(fair_samples[["scale_other_ghg"]][sample_idx,])
    psi_NMVOC <<- as.numeric(fair_samples[["b_tro3_NMVOC"]][sample_idx,])
    F2x <<- as.numeric(fair_samples[["F2x"]][sample_idx,])
    beta_SOx <<- as.numeric(fair_samples[["b_aero_SOx"]][sample_idx,])
    beta_NMVOC <<- as.numeric(fair_samples[["b_aero_NMVOC"]][sample_idx,])
    rT_co2 <<- as.numeric(fair_samples[["rt"]][sample_idx,])
    beta_BC <<- as.numeric(fair_samples[["b_aero_BC"]][sample_idx,])
    scale_CO2 <<- as.numeric(fair_samples[["scale_CO2"]][sample_idx,])
    psi_ODS <<- as.numeric(fair_samples[["b_tro3_ODS"]][sample_idx,])
    scale_aerosol_direct_CO_NMVOC <<- as.numeric(fair_samples[["scale_aerosol_direct_CO_NMVOC"]][sample_idx,])
    psi_NOx <<- as.numeric(fair_samples[["b_tro3_NOx"]][sample_idx,])
    ocean_heat_exchange <<- as.numeric(fair_samples[["ocean_heat_exchange"]][sample_idx,])
    phi <<- as.numeric(fair_samples[["ghan_params_Pi"]][sample_idx,])
  }else{
    stop("Wrong fair sampling mode!")
  }
}

# Create a function to load MimiFAIRv1.6.2.
fair_get_model <- function(ar6_scenario = "ssp245", start_year = 1750, end_year = 2300) {
  
  # Load RCP and other data needed to construct FAIR (just hard-coded 1765 as start year because not using the RCP emissions from this function).
  fair_data <<- load_fair_data(1765, end_year, "RCP85")
  rcp_emissions <<- fair_data[[1]]
  volcano_forcing <<- fair_data[[2]]
  solar_forcing <<- fair_data[[3]]
  gas_data <<- fair_data[[4]]
  gas_fractions <<- fair_data[[5]]
  conversions <<- fair_data[[6]]
  
  # Load IPCC AR6 emissions scenario used for FAIRv1.6.2 ensemble runs (options = "ssp119", "ssp126", "ssp245", "ssp370", "ssp460", "ssp585").
  ar6_emissions_raw <<- read.csv(paste0("../data/fair_model_data/AR6_emissions_", ar6_scenario, "_1750_2300.csv"))
  
  # Subset AR6 emissions to proper years.
  emission_indices <<- which(ar6_emissions_raw$Year %in% start_year:end_year)
  ar6_emissions <<- ar6_emissions_raw[emission_indices, ]
  
  # Load IPCC AR6 natural CH4 and N2O emissions for FAIR (spans 1750-2500).
  ar6_natural_emissions_raw <<- read.csv("../data/fair_model_data/fair_wg3_natural_ch4_n2o.csv")
  
  # Subset natural CH4 and N2O emissions to proper years.
  natural_indices <<- which(ar6_natural_emissions_raw$year %in% start_year:end_year)
  ar6_natural_emissions <<- ar6_natural_emissions_raw[natural_indices, c("CH4", "N2O")]
  
  # Load IPCC solar forcing scenario (note, dataset runs from -6755 to 2299).
  ar6_solar_forcing_raw <<- read.csv("../data/fair_model_data/ar6_solar_erf.csv")

  # For cases when you want to run the model out to 2300, assume solar forcing value in 2300 follows the trend from 2298-2299.
  ar6_solar_forcing_raw <<- rbind(ar6_solar_forcing_raw, data.frame(year = 2300, solar_erf = ar6_solar_forcing_raw[nrow(ar6_solar_forcing_raw), "solar_erf"] + (ar6_solar_forcing_raw[nrow(ar6_solar_forcing_raw), "solar_erf"] - ar6_solar_forcing_raw[nrow(ar6_solar_forcing_raw) - 1, "solar_erf"])))

  # Subset solar forcing data to proper years.
  solar_indices <<- which(ar6_solar_forcing_raw$year %in% start_year:end_year)
  ar6_solar_forcing <<- ar6_solar_forcing_raw[solar_indices, "solar_erf"]
  
  # Load IPCC AR6 volcanic forcing scenario (note, dataset runs from -500 to 2019).
  ar6_volcanic_forcing_raw <<- read.csv("../data/fair_model_data/ar6_volcanic_erf.csv")
  
  # Extract indices for relevant years.
  if (end_year > 2019) {
    volcanic_indices <<- match((start_year:2019), ar6_volcanic_forcing_raw$year)
  } else {
    volcanic_indices <<- match((start_year:end_year), ar6_volcanic_forcing_raw$year)
  }
  
  # Create an empty array and store subset of volcanic forcings.
  ar6_volcanic_forcing <<- rep(0, length(start_year:end_year))
  ar6_volcanic_forcing[1:length(volcanic_indices)] <<- ar6_volcanic_forcing_raw[volcanic_indices, "volcanic_erf"]
  
  # From FAIR AR6 code on volcanic forcing: "ramp down last 10 years to zero according to https://www.geosci-model-dev.net/9/3461/2016/gmd-9-3461-2016.html"
  # This copies that code exactly.
  index_2019 <<- match(2019, (start_year:end_year))
  ar6_volcanic_forcing[(index_2019):(index_2019+10)] <<- ar6_volcanic_forcing[index_2019] * seq(1, 0, length.out=11)
  
  # Names of minor greenhouse gases and ozone-depleting substances (used or indexing).
  other_ghg_names <<- c("CF4", "C2F6", "C6F14", "HFC23", "HFC32", "HFC43_10", "HFC125", "HFC134a", "HFC143a", "HFC227ea", "HFC245fa", "SF6")
  ods_names <<- c("CFC_11", "CFC_12", "CFC_113", "CFC_114", "CFC_115", "CARB_TET", "MCF", "HCFC_22", "HCFC_141B", "HCFC_142B", "HALON1211", "HALON1202", "HALON1301", "HALON2402", "CH3BR", "CH3CL")
  
  # ---------------------------------------------
  # Set component-specific parameters
  # ---------------------------------------------
  
  # ---- Carbon Cycle ---- #
  
  # ---- CO2 Cycle ---- #
  CO2_0 <<- 278.052989189439
  iirf_h <<- 100.0
  # r0_co2 <<- 35.0
  # rT_co2 <<- 4.165
  # rC_co2 <<- 0.019
  tau_co2 <<- c(1000000, 394.4, 36.54, 4.304)
  a_co2 <<- c(0.2173, 0.2240, 0.2824, 0.2763)
  R0_co2 <<- c(0.0003062168651584551, 0.0003156584344017209, 0.0003979550976564552, 0.0003893590420767655)
  
  E_co2 <<- ar6_emissions$FossilCO2 + ar6_emissions$OtherCO2
  if(socioeconomics_source == "RFF"){
     E_co2[-(1:(RFF_start_year-model_first))] <<- co2_emissions
  }
  
  cumulative_emissions_CO2_0 <<- 0.003
  airborne_emissions_CO2_0 <<- 0.0
  iIRF_max <<- 97.0
  
  # ---- Methane Cycle ---- #
  fossil_emiss_CH4 <<- ar6_emissions$CH4
  if(socioeconomics_source == "RFF"){
    fossil_emiss_CH4[-(1:(RFF_start_year-model_first))] <<- ch4_emissions
  }
  
  natural_emiss_CH4 <<- ar6_natural_emissions$CH4
  tau_CH4 <<- 9.3
  fossil_frac <<- rep(1, length(start_year:end_year))
  oxidation_frac <<- 0.61
  mol_weight_CH4 <<- gas_data[gas_data$gas == "CH4", "mol_weight"]
  mol_weight_C <<- gas_data[gas_data$gas == "C", "mol_weight"]
  emiss2conc_ch4 <<- conversions[conversions$gases == "CH4", "emiss2conc"]
  CH4_0 <<- gas_data[gas_data$gas == "CH4", "pi_conc_ar6"]

  # ---- Nitrous Oxide Cycle ---- #
  fossil_emiss_N2O <<- ar6_emissions$N2O
  if(socioeconomics_source == "RFF"){
    fossil_emiss_N2O[-(1:(RFF_start_year-model_first))] <<- n2o_emissions
  }
  
  natural_emiss_N2O <<- ar6_natural_emissions$N2O
  tau_N2O <<- 121.0
  emiss2conc_n2o <<- conversions$emiss2conc[conversions$gases == "N2O"]
  N2O_0 <<- gas_data$pi_conc_ar6[gas_data$gas == "N2O"]
  
  # ---- Other Well-Mixed Greenhouse Gas Cycles ---- #
  tau_other_ghg <<- gas_data$lifetimes[gas_data$gas %in% other_ghg_names]
  emiss_other_ghg <<- as.matrix(ar6_emissions[,other_ghg_names])
  emiss2conc_other_ghg <<- conversions$emiss2conc[conversions$gases %in% other_ghg_names]
  other_ghg_0 <<- gas_data$pi_conc_ar6[gas_data$gas %in% other_ghg_names]
  
  # ---- Ozone-Depleting Substance Gas Cycles ---- #
  tau_ods <<- gas_data$lifetimes[gas_data$gas %in% ods_names]
  emiss_ods <<- as.matrix(ar6_emissions[,ods_names])
  emiss2conc_ods <<- conversions$emiss2conc[conversions$gases %in% ods_names]
  ods_0 <<- gas_data$pi_conc_ar6[gas_data$gas %in% ods_names]
  
  # ---- Carbon Dioxide Radiative Forcing ---- #
  # F2x <<- 3.71
  a1 <<- -2.4785e-07
  b1 <<- 0.00075906
  c1 <<- -0.0021492
  d1 <<- 5.2488
  adjust_F2x <<- TRUE
  
  # ---- Other Well-Mixed Greenhouse Gas Cycles ---- #
  tau_other_ghg <<- gas_data$lifetimes[gas_data$gas %in% other_ghg_names]
  emiss_other_ghg <<- as.matrix(ar6_emissions[,other_ghg_names])
  emiss2conc_other_ghg <<- conversions$emiss2conc[conversions$gases %in% other_ghg_names]
  other_ghg_0 <<- gas_data$pi_conc_ar6[gas_data$gas %in% other_ghg_names]
  
  # ---- Ozone-Depleting Substance Gas Cycles ---- #
  tau_ods <<- gas_data$lifetimes[gas_data$gas %in% ods_names]
  emiss_ods <<- as.matrix(ar6_emissions[,ods_names])
  emiss2conc_ods <<- conversions$emiss2conc[conversions$gases %in% ods_names]
  ods_0 <<- gas_data$pi_conc_ar6[gas_data$gas %in% ods_names]

  # ---- Carbon Dioxide Radiative Forcing ---- #
  F2x <<- 3.71
  a1 <<- -2.4785e-07
  b1 <<- 0.00075906
  c1 <<- -0.0021492
  d1 <<- 5.2488
  adjust_F2x <<- TRUE
  
  # ---- Methane Radiative Forcing ---- #
  a3 <<- -8.9603e-05
  b3 <<- -0.00012462
  d3 <<- 0.045194
  h2o_from_ch4 <<- 0.079047
  
  # ---- Nitrous Oxide Radiative Forcing ---- #
  a2 <<- -0.00034197
  b2 <<- 0.00025455
  c2 <<- -0.00024357
  d2 <<- 0.12173
  
  # ---- Ozone Radiative Forcing ---- #
  total_forcing_O3_0 <<- 0.0
  Br <<- gas_data[which(gas_data$gas %in% ods_names), "br_atoms"]
  Cl <<- gas_data[which(gas_data$gas %in% ods_names), "cl_atoms"]
  FC <<- gas_data[which(gas_data$gas %in% ods_names), "strat_frac"]
  # feedback <<- -0.037
  # psi_CH4 <<- 2.33379720e-04
  # psi_N2O <<- 1.27179106e-03
  # psi_ODS <<- -6.69347820e-05
  # psi_CO <<- 1.14647701e-04
  # psi_NMVOC <<- 5.14366051e-12
  # psi_NOx <<- 3.78354423e-03
  
  # ---- Aerosol Direct Radiative Forcing ---- #
  # beta_SOx <<- -6.2227e-3
  # beta_CO <<- 0.0
  # beta_NMVOC <<- -3.8392e-4
  # beta_NOx <<- -1.16551e-3
  # beta_BC <<- 1.601537e-2
  # beta_OC <<- -1.45339e-3
  # beta_NH3 <<- -1.55605e-3
  
  # ---- Aerosol Indirect Radiative Forcing ---- #
  # phi <<- 0.07334277994353743
  # b_SOx <<- 3.452849302362568
  # b_POM <<- 33.126485122209154
  rf_scale_aero_indirect <<- 1.0
  
  # ---- Other Well-Mixed Greenhouse Gas Radiative Forcings ---- #
  other_ghg_radiative_efficiency <<- gas_data[gas_data$gas %in% other_ghg_names, "rad_eff"]
  
  # ---- Ozone-Depleting Substance Radiative Forcings ---- #
  ods_radiative_efficiency <<- gas_data[gas_data$gas %in% ods_names, "rad_eff"]
  
  # ---- Contrails Radiative Forcing ---- #
  frac <<- rep(0, length(start_year:end_year))
  E_ref_contrails <<- 2.946
  F_ref_contrails <<- 0.0448
  ref_is_NO2 <<- TRUE
  mol_weight_NO2 <<- gas_data[gas_data$gas == "NO2", "mol_weight"]
  mol_weight_N <<- gas_data[gas_data$gas == "N", "mol_weight"]
  
  # ---- Black Carbon on Snow Radiative Forcing ---- #
  E_ref_bc <<- 6.095
  F_ref_bc <<- 0.08
  
  # ---- Land Use Change Radiative Forcing ---- #
  alpha_CO2_land <<- (-0.2/190)
  landuse_emiss <<- ar6_emissions$OtherCO2
  
  # ---- Total Radiative Forcing ---- #
  # scale_CO2 <<- 1.0
  # scale_CH4 <<- 1.0
  # scale_CH4_H2O <<- 1.0
  # scale_N2O <<- 1.0
  # scale_O3 <<- 1.0
  # scale_aerosol_indirect <<- 1.0
  # scale_bcsnow <<- 1.0
  # scale_landuse <<- 1.0
  scale_contrails <<- 0.0 # Default FAIR has contrail forcing switched off. Set scaling term to 0
  # scale_volcanic <<- 1.0
  # scale_solar <<- 1.0
  # scale_aerosol_direct_SOx <<- 1.0
  # scale_aerosol_direct_CO_NMVOC <<- 1.0
  # scale_aerosol_direct_NOx_NH3 <<- 1.0
  # scale_aerosol_direct_BC <<- 1.0
  # scale_aerosol_direct_OC <<- 1.0
  # scale_other_ghg <<- rep(1, length(other_ghg_names))
  # scale_ods <<- rep(1, length(ods_names))
  F_volcanic <<- ar6_volcanic_forcing
  # F_solar <<- ar6_solar_forcing
  F_exogenous <<- rep(0, length(start_year:end_year))
  
  # ---- Temperature ---- #
  earth_radius <<- 6371000
  seconds_per_year <<- (60*60*24*365.24219)
  # ocean_heat_exchange <<- 0.67
  # deep_ocean_efficacy <<- 1.28
  # lambda_global <<- 1.18
  T_mix0 <<- c(5.30299681e-03, 6.41290105e-05)
  T_deep0 <<- c(-1.33065374e-04, 1.50206328e-04)
  # ocean_heat_capacity <<- c(8.2, 109.0)
  
  # ---- Parameters Shared Across Multiple Components ---- #
  dt <<- 1.0
  emiss2conc_co2 <<- conversions[conversions$gases == "CO2", "emiss2conc"][1]
  CH4_pi <<- gas_data[gas_data$gas == "CH4", "pi_conc_ar6"][1]
  # CO2_pi <<- gas_data[gas_data$gas == "CO2", "pi_conc_ar6"][1]
  N2O_pi <<- gas_data[gas_data$gas == "N2O", "pi_conc_ar6"][1]
  ods_pi <<- gas_data[which(gas_data$gas %in% ods_names), "pi_conc_ar6"]
  other_ghg_pi <<- gas_data[which(gas_data$gas %in% other_ghg_names), "pi_conc_ar6"]
  SOx_emiss <<- ar6_emissions$SOx
  BC_emiss <<- ar6_emissions$B
  OC_emiss <<- ar6_emissions$OC
  CO_emiss <<- ar6_emissions$CO
  NMVOC_emiss <<- ar6_emissions$NMVOC
  NH3_emiss <<- ar6_emissions$NH3
  NOx_emiss <<- ar6_emissions$NOx
  SOx_emiss_pi <<- 1.22002422
  CO_emiss_pi <<- 348.527359
  NMVOC_emiss_pi <<- 60.0218262
  NOx_emiss_pi <<- 3.87593407
  BC_emiss_pi <<- 2.09777075
  OC_emiss_pi <<- 15.4476682
  
  #####load parameters with distributions
}

FAIR_model_run <- function(Marginal_choice,sample_idx = 0){
  fair_get_model(ar6_scenario = target_scenario, start_year = start_year, end_year = end_year)
  MarginalEmissionComponent(Marginal_choice);
  FAIR_parameters_load(sample_idx)
  for (t in 1:length(time)) {
    ch4_cycle_run_timestep(t);
    n2o_cycle_run_timestep(t);
    co2_cycle_run_timestep(t);
    other_ghg_cycles_run_timestep(t);
    o3_depleting_substance_cycles_run_timestep(t);
    co2_forcing_run_timestep(t);
    ch4_forcing_run_timestep(t);
    n2o_forcing_run_timestep(t);
    o3_forcing_run_timestep(t);
    aerosol_direct_forcing_run_timestep(t);
    aerosol_indirect_forcing_run_timestep(t);
    other_ghg_forcing_run_timestep(t);
    o3_depleting_substance_forcing_run_timestep(t);
    contrails_forcing_run_timestep(t);
    black_carbon_snow_forcing_run_timestep(t);
    landuse_forcing_run_timestep(t);
    total_forcing_run_timestep(t);
    temperature_run_timestep(t);
  }
}

# 
# fair_parameter_set = "deterministic"
# FAIR_model_run("baserun");
# print(temperature)
# 
# fair_parameter_set = "random"
# trials_num = 100;
# output_dir = "../data/fair_model_output/MC_outputs_temp.csv"
# get_mcs()
# MC_outputs_temp <- matrix(nrow = length(time),ncol = trials_num)
# for (i in 1:trials_num) {
#   print(i)
#   FAIR_model_run("baserun",sample_idx = i)
#   MC_outputs_temp[,i] <- temperature
# }
# write.csv(MC_outputs_temp,output_dir,quote = F,row.names = F)

