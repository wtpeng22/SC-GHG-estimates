# Create a function to load MimiFAIRv1.6.2 in its original form 

# This is the version of MimiFAIRv1_6_2 that will match the Python version and 
# can be used for testing.

get_model_original <- function(rcp_scenario="RCP85", start_year=1765, end_year=2500){
  
  # ---------------------------------------------
  # Set Up Data and Parameter Values
  # ---------------------------------------------
  
  # Load RCP and other data needed to construct FAIR.
  data <- load_fair_data(start_year, end_year, rcp_scenario)
  rcp_emissions <- data[[1]]
  volcano_forcing <- data[[2]]
  solar_forcing <- data[[3]]
  gas_data <- data[[4]]
  gas_fractions <- data[[5]]
  conversions <- data[[6]]
  
  # Names of minor greenhouse gases and ozone-depleting substances (used or indexing).
  other_ghg_names <- c("CF4", "C2F6", "C6F14", "HFC23", "HFC32", "HFC43_10", "HFC125", "HFC134a", "HFC143a", "HFC227ea", "HFC245fa", "SF6")
  ods_names <- c("CFC_11", "CFC_12", "CFC_113", "CFC_114", "CFC_115", "CARB_TET", "MCF", "HCFC_22", "HCFC_141B", "HCFC_142B", "HALON1211", "HALON1202", "HALON1301", "HALON2402", "CH3BR", "CH3CL")
  
  # ---------------------------------------------
  # Initialize Mimi model.
  # ---------------------------------------------
  
  m <- Mimi$new()
  
  # Set time and gas-grouping indices.
  m$set_dimension("time", start_year:end_year)
  m$set_dimension("other_ghg", other_ghg_names)
  m$set_dimension("ozone_depleting_substances", ods_names)
  
  # ---------------------------------------------
  # Add components to model
  # ---------------------------------------------
  
  add_comp(m, ch4_cycle)
  add_comp(m, n2o_cycle)
  add_comp(m, co2_cycle)
  add_comp(m, other_ghg_cycles)
  add_comp(m, o3_depleting_substance_cycles)
  add_comp(m, co2_forcing)
  add_comp(m, ch4_forcing)
  add_comp(m, n2o_forcing)
  add_comp(m, o3_forcing)
  add_comp(m, aerosol_direct_forcing)
  add_comp(m, aerosol_indirect_forcing)
  add_comp(m, other_ghg_forcing)
  add_comp(m, o3_depleting_substance_forcing)
  add_comp(m, contrails_forcing)
  add_comp(m, bc_snow_forcing)
  add_comp(m, landuse_forcing)
  add_comp(m, total_forcing)
  add_comp(m, temperature)
  
  # ---------------------------------------------
  # Set component-specific parameters
  # ---------------------------------------------
  
  # ---- Carbon Cycle ---- #
  set_param(m, "co2_cycle", "CO2_0", 278.052989189439) # From FAIR model run.
  set_param(m, "co2_cycle", "iirf_h", 100.0)
  set_param(m, "co2_cycle", "r0_co2", 35.0)
  set_param(m, "co2_cycle", "rT_co2", 4.165)
  set_param(m, "co2_cycle", "rC_co2", 0.019)
  set_param(m, "co2_cycle", "tau_co2", c(1000000, 394.4, 36.54, 4.304))
  set_param(m, "co2_cycle", "a_co2", c(0.2173,0.2240,0.2824,0.2763))
  set_param(m, "co2_cycle", "R0_co2", c(0.0003062168651584551, 0.0003156584344017209, 0.0003979550976564552, 0.0003893590420767655)) # From FAIR model run.
  set_param(m, "co2_cycle", "E_co2", rcp_emissions$FossilCO2 + rcp_emissions$OtherCO2)
  set_param(m, "co2_cycle", "cumulative_emissions_CO2₀", 0.003)
  set_param(m, "co2_cycle", "airborne_emissions_CO2₀", 0.0)
  set_param(m, "co2_cycle", "iIRF_max", 97.0)
  connect_param(m, "co2_cycle", "temperature", "temperature", "T")
  
  # ---- Methane Cycle ---- #
  set_param(m, "ch4_cycle", "fossil_emiss_CH₄", rcp_emissions$CH4)
  set_param(m, "ch4_cycle", "natural_emiss_CH₄", rcp_emissions$NaturalCH4)
  set_param(m, "ch4_cycle", "τ_CH₄", 9.3)
  set_param(m, "ch4_cycle", "fossil_frac", gas_fractions$ch4_fossil)
  set_param(m, "ch4_cycle", "oxidation_frac", 0.61)
  set_param(m, "ch4_cycle", "mol_weight_CH₄", gas_data[gas_data$gas == "CH4", "mol_weight"])
  set_param(m, "ch4_cycle", "mol_weight_C", gas_data[gas_data$gas == "C", "mol_weight"])
  set_param(m, "ch4_cycle", "emiss2conc_ch4", conversions[conversions$gases == "CH4", "emiss2conc"])
  set_param(m, "ch4_cycle", "CH₄_0", gas_data[gas_data$gas == "CH4", "pi_conc_ar6"])
  
  # ---- Nitrous Oxide Cycle ---- #
  set_param(m, "n2o_cycle", "fossil_emiss_N₂O", rcp_emissions$N2O)
  set_param(m, "n2o_cycle", "natural_emiss_N₂O", rcp_emissions$NaturalN2O)
  set_param(m, "n2o_cycle", "τ_N₂O", 121.0)
  set_param(m, "n2o_cycle", "emiss2conc_n2o", conversions[conversions$gases == "N2O", "emiss2conc"])
  set_param(m, "n2o_cycle", "N₂O_0", gas_data[gas_data$gas == "N2O", "pi_conc_ar6"])
  
  # ---- Other Well-Mixed Greenhouse Gas Cycles ---- #
  set_param(m, "other_ghg_cycles", "τ_other_ghg", gas_data[gas_data$gas %in% other_ghg_names, "lifetimes"])
  set_param(m, "other_ghg_cycles", "emiss_other_ghg", as.matrix(rcp_emissions[, other_ghg_names]))
  set_param(m, "other_ghg_cycles", "emiss2conc_other_ghg", conversions[conversions$gases %in% other_ghg_names, "emiss2conc"])
  set_param(m, "other_ghg_cycles", "other_ghg_0", gas_data[gas_data$gas %in% other_ghg_names, "pi_conc_ar6"])
  
  # ---- Ozone-Depleting Substance Gas Cycles ---- #
  set_param(m, "o3_depleting_substance_cycles", "τ_ods", gas_data[gas_data$gas %in% ods_names, "lifetimes"])
  set_param(m, "o3_depleting_substance_cycles", "emiss_ods", as.matrix(rcp_emissions[, ods_names]))
  set_param(m, "o3_depleting_substance_cycles", "emiss2conc_ods", conversions[conversions$gases %in% ods_names, "emiss2conc"])
  set_param(m, "o3_depleting_substance_cycles", "ods_0", gas_data[gas_data$gas %in% ods_names, "pi_conc_ar6"])
  
  # ---- Carbon Dioxide Radiative Forcing ----
    set_param(m, "F2x", 3.71)
  set_param(m, "co2_forcing", "a₁", -2.4785e-07)
  set_param(m, "co2_forcing", "b₁", 0.00075906)
  set_param(m, "co2_forcing", "c₁", -0.0021492)
  set_param(m, "co2_forcing", "d₁", 5.2488)
  set_param(m, "co2_forcing", "adjust_F2x", TRUE)
  connect_param(m, "co2_forcing" = "CO₂", "co2_cycle" = "co2")
  connect_param(m, "co2_forcing" = "N₂O", "n2o_cycle" = "N₂O")
  
  # ---- Methane Radiative Forcing ----
    set_param(m, "ch4_forcing", "a₃", -8.9603e-05)
  set_param(m, "ch4_forcing", "b₃", -0.00012462)
  set_param(m, "ch4_forcing", "d₃", 0.045194)
  set_param(m, "ch4_forcing", "h2o_from_ch4", 0.079047)
  connect_param(m, "ch4_forcing" = "N₂O", "n2o_cycle" = "N₂O")
  connect_param(m, "ch4_forcing" = "CH₄", "ch4_cycle" = "CH₄")
  
  # ---- Nitrous Oxide Radiative Forcing ----
    set_param(m, "n2o_forcing", "a₂", -0.00034197)
  set_param(m, "n2o_forcing", "b₂", 0.00025455)
  set_param(m, "n2o_forcing", "c₂", -0.00024357)
  set_param(m, "n2o_forcing", "d₂", 0.12173)
  connect_param(m, "n2o_forcing" = "CO₂", "co2_cycle" = "co2")
  connect_param(m, "n2o_forcing" = "N₂O", "n2o_cycle" = "N₂O")
  connect_param(m, "n2o_forcing" = "CH₄", "ch4_cycle" = "CH₄")
  
  # ---- Ozone Radiative Forcing ----
    set_param(m, "o3_forcing", "total_forcing_O₃_0", 0.0)
  set_param(m, "o3_forcing", "Br", gas_data[which(gas_data$gas %in% ods_names), "br_atoms"])
  set_param(m, "o3_forcing", "Cl", gas_data[which(gas_data$gas %in% ods_names), "cl_atoms"])
  set_param(m, "o3_forcing", "FC", gas_data[which(gas_data$gas %in% ods_names), "strat_frac"])
  set_param(m, "o3_forcing", "feedback", -0.037)
  set_param(m, "o3_forcing", "Ψ_CH₄", 2.33379720e-04)
  set_param(m, "o3_forcing", "Ψ_N₂O", 1.27179106e-03)
  set_param(m, "o3_forcing", "Ψ_ODS", -6.69347820e-05)
  set_param(m, "o3_forcing", "Ψ_CO", 1.14647701e-04)
  set_param(m, "o3_forcing", "Ψ_NMVOC", 5.14366051e-12)
  set_param(m, "o3_forcing", "Ψ_NOx", 3.78354423e-03)
  connect_param(m, "o3_forcing/N₂O" = "n2o_cycle/N₂O")
  connect_param(m, "o3_forcing/CH₄" = "ch4_cycle/CH₄")
  connect_param(m, "o3_forcing/temperature" = "temperature/T")
  connect_param(m, "o3_forcing/conc_ODS" = "o3_depleting_substance_cycles/conc_ods")
  
  # ---- Aerosol Direct Radiative Forcing ---- #
  set_param(m, "aerosol_direct_forcing", "β_SOx", -6.2227e-3)
  set_param(m, "aerosol_direct_forcing", "β_CO", 0.0)
  set_param(m, "aerosol_direct_forcing", "β_NMVOC", -3.8392e-4)
  set_param(m, "aerosol_direct_forcing", "β_NOx", -1.16551e-3)
  set_param(m, "aerosol_direct_forcing", "β_BC", 1.601537e-2)
  set_param(m, "aerosol_direct_forcing", "β_OC", -1.45339e-3)
  set_param(m, "aerosol_direct_forcing", "β_NH3", -1.55605e-3)
  
  # ---- Aerosol Indirect Radiative Forcing ---- #
  set_param(m, "aerosol_indirect_forcing", "ϕ", 0.07334277994353743)#-1.95011431)
  set_param(m, "aerosol_indirect_forcing", "b_SOx", 3.452849302362568)#0.01107147)
  set_param(m, "aerosol_indirect_forcing", "b_POM", 33.126485122209154)#0.01387492)
  set_param(m, "aerosol_indirect_forcing", "rf_scale_aero_indirect", 1.0)
  
  # ---- Other Well-Mixed Greenhouse Gas Cycles ----
    set_param(m, "other_ghg_cycles", "τ_other_ghg", gas_data[which(gas_data$gas %in% other_ghg_names), "lifetimes"])
  set_param(m, "other_ghg_cycles", "emiss_other_ghg", as.matrix(rcp_emissions[, other_ghg_names]))
  set_param(m, "other_ghg_cycles", "emiss2conc_other_ghg", conversions[which(conversions$gases %in% other_ghg_names), "emiss2conc"])
  set_param(m, "other_ghg_cycles", "other_ghg_0", gas_data[which(gas_data$gas %in% other_ghg_names), "pi_conc_ar6"])
  
  # ---- Ozone-Depleting Substance Gas Cycles ----
    set_param(m, "o3_depleting_substance_cycles", "τ_ods", gas_data[which(gas_data$gas %in% ods_names), "lifetimes"])
  set_param(m, "o3_depleting_substance_cycles", "emiss_ods", as.matrix(rcp_emissions[, ods_names]))
  set_param(m, "o3_depleting_substance_cycles", "emiss2conc_ods", conversions[which(conversions$gases %in% ods_names), "emiss2conc"])
  set_param(m, "o3_depleting_substance_cycles", "ods_0", gas_data[which(gas_data$gas %in% ods_names), "pi_conc_ar6"])
  
  # ---- Carbon Dioxide Radiative Forcing ----
    set_param(m, "F2x", 3.71)
  set_param(m, "co2_forcing", "a₁", -2.4785e-07)
  set_param(m, "co2_forcing", "b₁", 0.00075906)
  set_param(m, "co2_forcing", "c₁", -0.0021492)
  set_param(m, "co2_forcing", "d₁", 5.2488)
  set_param(m, "co2_forcing", "adjust_F2x", TRUE)
  connect_param(m, "co2_forcing", "CO₂", "co2_cycle", "co2")
  connect_param(m, "co2_forcing", "N₂O", "n2o_cycle", "N₂O")
  
  # ---- Methane Radiative Forcing ----
    set_param(m, "ch4_forcing", "a₃", -8.9603e-05)
  set_param(m, "ch4_forcing", "b₃", -0.00012462)
  set_param(m, "ch4_forcing", "d₃", 0.045194)
  set_param(m, "ch4_forcing", "h2o_from_ch4", 0.079047)
  connect_param(m, "ch4_forcing", "N₂O", "n2o_cycle", "N₂O")
  connect_param(m, "ch4_forcing", "CH₄", "ch4_cycle", "CH₄")
  
  # ---- Nitrous Oxide Radiative Forcing ----
  set_param(m, "n2o_forcing", "a₂", -0.00034197)
  set_param(m, "n2o_forcing", "b₂", 0.00025455)
  set_param(m, "n2o_forcing", "c₂", -0.00024357)
  set_param(m, "n2o_forcing", "d₂", 0.12173)
  connect_param(m, "n2o_forcing" = "CO₂", "co2_cycle" = "co2")
  connect_param(m, "n2o_forcing" = "N₂O", "n2o_cycle" = "N₂O")
  connect_param(m, "n2o_forcing" = "CH₄", "ch4_cycle" = "CH₄")
  
  # ---- Ozone Radiative Forcing ---- #
  set_param(m, "o3_forcing", "total_forcing_O₃_0", 0.0)
  set_param(m, "o3_forcing", "Br", gas_data[gas_data$gas %in% ods_names, "br_atoms"])
  set_param(m, "o3_forcing", "Cl", gas_data[gas_data$gas %in% ods_names, "cl_atoms"])
  set_param(m, "o3_forcing", "FC", gas_data[gas_data$gas %in% ods_names, "strat_frac"])
  set_param(m, "o3_forcing", "feedback", -0.037)
  set_param(m, "o3_forcing", "Ψ_CH₄", 2.33379720e-04)
  set_param(m, "o3_forcing", "Ψ_N₂O", 1.27179106e-03)
  set_param(m, "o3_forcing", "Ψ_ODS", -6.69347820e-05)
  set_param(m, "o3_forcing", "Ψ_CO", 1.14647701e-04)
  set_param(m, "o3_forcing", "Ψ_NMVOC", 5.14366051e-12)
  set_param(m, "o3_forcing", "Ψ_NOx", 3.78354423e-03)
  connect_param(m, "o3_forcing" = "N₂O", "n2o_cycle" = "N₂O")
  connect_param(m, "o3_forcing" = "CH₄", "ch4_cycle" = "CH₄")
  connect_param(m, "o3_forcing" = "temperature", "temperature" = "T")
  connect_param(m, "o3_forcing" = "conc_ODS", "o3_depleting_substance_cycles" = "conc_ods")
  # ---- Aerosol Direct Radiative Forcing ---- #
  set_param(m, "aerosol_direct_forcing", "β_SOx", -6.2227e-3)
  set_param(m, "aerosol_direct_forcing", "β_CO", 0.0)
  set_param(m, "aerosol_direct_forcing", "β_NMVOC", -3.8392e-4)
  set_param(m, "aerosol_direct_forcing", "β_NOx", -1.16551e-3)
  set_param(m, "aerosol_direct_forcing", "β_BC", 1.601537e-2)
  set_param(m, "aerosol_direct_forcing", "β_OC", -1.45339e-3)
  set_param(m, "aerosol_direct_forcing", "β_NH3", -1.55605e-3)
  
  # ---- Aerosol Indirect Radiative Forcing ---- #
  set_param(m, "aerosol_indirect_forcing", "ϕ", 0.07334277994353743)#-1.95011431)
  set_param(m, "aerosol_indirect_forcing", "b_SOx", 3.452849302362568)#0.01107147)
  set_param(m, "aerosol_indirect_forcing", "b_POM", 33.126485122209154)#0.01387492)
  set_param(m, "aerosol_indirect_forcing", "rf_scale_aero_indirect", 1.0)
  
  # ---- Other Well-Mixed Greenhouse Gas Radiative Forcings ---- #
  set_param(m, "other_ghg_forcing", "other_ghg_radiative_efficiency", gas_data[grep(paste(other_ghg_names, collapse="|"), gas_data$gas), "rad_eff"])
  connect_param(m, "other_ghg_forcing", "conc_other_ghg", "other_ghg_cycles", "conc_other_ghg")
  
  # ---- Ozone-Depleting Substance Radiative Forcings ---- #
  set_param(m, "o3_depleting_substance_forcing", "ods_radiative_efficiency", gas_data[grep(paste(ods_names, collapse="|"), gas_data$gas), "rad_eff"])
  connect_param(m, "o3_depleting_substance_forcing", "conc_ods", "o3_depleting_substance_cycles", "conc_ods")
  
  # ---- Contrails Radiative Forcing ----
    set_param(m, "contrails_forcing", "frac", gas_fractions$nox_aviation)
  set_param(m, "contrails_forcing", "E_ref_contrails", 2.946)
  set_param(m, "contrails_forcing", "F_ref_contrails", 0.0448)
  set_param(m, "contrails_forcing", "ref_is_NO2", TRUE)
  set_param(m, "contrails_forcing", "mol_weight_NO2", gas_data[gas_data$gas == "NO2", "mol_weight"])
  set_param(m, "contrails_forcing", "mol_weight_N", gas_data[gas_data$gas == "N", "mol_weight"])
  
  # ---- Black Carbon on Snow Radiative Forcing ----
    set_param(m, "bc_snow_forcing", "E_ref_bc", 6.095)
  set_param(m, "bc_snow_forcing", "F_ref_bc", 0.08)
  
  # ---- Land Use Change Radiative Forcing ----
    set_param(m, "landuse_forcing", "alpha_CO2_land", (-0.2/190))
  set_param(m, "landuse_forcing", "landuse_emiss", rcp_emissions$OtherCO2)
  
  # ---- Total Radiative Forcing ----
    set_param(m, "total_forcing", "scale_CO2", 1.0)
  set_param(m, "total_forcing", "scale_CH4", 1.0)
  set_param(m, "total_forcing", "scale_CH4_H2O", 1.0)
  set_param(m, "total_forcing", "scale_N2O", 1.0)
  set_param(m, "total_forcing", "scale_O3", 1.0)
  set_param(m, "total_forcing", "scale_aerosol_indirect", 1.0)
  set_param(m, "total_forcing", "scale_bcsnow", 1.0)
  set_param(m, "total_forcing", "scale_landuse", 1.0)
  set_param(m, "total_forcing", "scale_contrails", 0.0) # Default FAIR has contrail forcing switched off. Set scaling term to 0
  set_param(m, "total_forcing", "scale_volcanic", 1.0)
  set_param(m, "total_forcing", "scale_solar", 1.0)
  set_param(m, "total_forcing", "scale_aerosol_direct_SOx", 1.0)
  set_param(m, "total_forcing", "scale_aerosol_direct_CO_NMVOC", 1.0)
  set_param(m, "total_forcing", "scale_aerosol_direct_NOx_NH3", 1.0)
  set_param(m, "total_forcing", "scale_aerosol_direct_BC", 1.0)
  set_param(m, "total_forcing", "scale_aerosol_direct_OC", 1.0)
  set_param(m, "total_forcing", "scale_other_ghg", rep(1, length(other_ghg_names)))
  set_param(m, "total_forcing", "scale_ods", rep(1, length(ods_names)))
  set_param(m, "total_forcing", "F_volcanic", volcano_forcing)
  set_param(m, "total_forcing", "F_solar", solar_forcing)
  set_param(m, "total_forcing", "F_exogenous", rep(0, length(start_year:end_year)))
  # Load required libraries
  library(tidyverse)
  
  # Connect parameters
  connect_param(m, total_forcing = "F_CO2", co2_forcing = "rf_co2")
  connect_param(m, total_forcing = "F_CH4", ch4_forcing = "rf_ch4")
  connect_param(m, total_forcing = "F_CH4_H2O", ch4_forcing = "rf_ch4_h2o")
  connect_param(m, total_forcing = "F_N2O", n2o_forcing = "rf_n2o")
  connect_param(m, total_forcing = "F_O3", o3_forcing = "total_forcing_O3")
  connect_param(m, total_forcing = "F_aerosol_direct_SOx", aerosol_direct_forcing = "F_SOx_aero")
  connect_param(m, total_forcing = "F_aerosol_direct_CO", aerosol_direct_forcing = "F_CO_aero")
  connect_param(m, total_forcing = "F_aerosol_direct_NMVOC", aerosol_direct_forcing = "F_NMVOC_aero")
  connect_param(m, total_forcing = "F_aerosol_direct_NOx", aerosol_direct_forcing = "F_NOx_aero")
  connect_param(m, total_forcing = "F_aerosol_direct_BC", aerosol_direct_forcing = "F_BC_aero")
  connect_param(m, total_forcing = "F_aerosol_direct_OC", aerosol_direct_forcing = "F_OC_aero")
  connect_param(m, total_forcing = "F_aerosol_direct_NH3", aerosol_direct_forcing = "F_NH3_aero")
  connect_param(m, total_forcing = "F_aerosol_indirect", aerosol_indirect_forcing = "rf_aero_indirect")
  connect_param(m, total_forcing = "F_bcsnow", bc_snow_forcing = "forcing_BC_snow")
  connect_param(m, total_forcing = "F_landuse", landuse_forcing = "forcing_landuse")
  connect_param(m, total_forcing = "F_contrails", contrails_forcing = "forcing_contrails")
  connect_param(m, total_forcing = "F_other_ghg", other_ghg_forcing = "other_ghg_rf")
  connect_param(m, total_forcing = "F_ods", o3_depleting_substance_forcing = "ods_rf")
  
  # Temperature
  set_param(m, temperature, earth_radius = 6371000)
  set_param(m, temperature, seconds_per_year = (60 * 60 * 24 * 365.24219))
  set_param(m, temperature, ocean_heat_exchange = 0.67)
  set_param(m, temperature, deep_ocean_efficacy = 1.28)
  set_param(m, temperature, lambda_global = 1.18)
  set_param(m, temperature, T_mix0 = c(0.00530299681, 6.41290105e-05)) # From Python-FAIR model run.
  set_param(m, temperature, T_deep0 = c(-1.33065374e-04,  1.50206328e-04)) # From Python-FAIR model run.
  set_param(m, temperature, ocean_heat_capacity = c(8.2, 109.0))
  connect_param(m, temperature = "forcing", total_forcing = "total_forcing")
  
  # ---- Parameters Shared Across Multiple Components ---- #
  m <- set_param(m, "dt", 1.0)
  m <- set_param(m, "emiss2conc_co2", conversions[conversions$gases == "CO2", "emiss2conc"][1])
  m <- set_param(m, "CH₄_pi", gas_data[gas_data$gas == "CH4", "pi_conc_ar6"][1])
  m <- set_param(m, "CO₂_pi", gas_data[gas_data$gas == "CO2", "pi_conc_ar6"][1])
  m <- set_param(m, "N₂O_pi", gas_data[gas_data$gas == "N2O", "pi_conc_ar6"][1])
  m <- set_param(m, "ods_pi", gas_data[which(gas_data$gas %in% ods_names), "pi_conc_ar6"])
  m <- set_param(m, "other_ghg_pi", gas_data[which(gas_data$gas %in% other_ghg_names), "pi_conc_ar6"])
  m <- set_param(m, "SOx_emiss", rcp_emissions$SOx)
  m <- set_param(m, "BC_emiss", rcp_emissions$BC)
  m <- set_param(m, "OC_emiss", rcp_emissions$OC)
  m <- set_param(m, "CO_emiss", rcp_emissions$CO)
  m <- set_param(m, "NMVOC_emiss", rcp_emissions$NMVOC)
  m <- set_param(m, "NH3_emiss", rcp_emissions$NH3)
  m <- set_param(m, "NOx_emiss", rcp_emissions$NOx)
  m <- set_param(m, "SOx_emiss_pi", 0.0)
  m <- set_param(m, "CO_emiss_pi", 0.0)
  m <- set_param(m, "NMVOC_emiss_pi", 0.0)
  m <- set_param(m, "NOx_emiss_pi", 0.0)
  m <- set_param(m, "BC_emiss_pi", 0.0)
  m <- set_param(m, "OC_emiss_pi", 0.0)
  
  # Return model
  return(m)
}
             

