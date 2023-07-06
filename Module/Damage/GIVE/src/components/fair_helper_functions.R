# #----------------------------------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------------------------------
# This file contains functions and other snippets of code that are used in various calculations for Mimi-FAIR.
# #----------------------------------------------------------------------------------------------------------------------
# #----------------------------------------------------------------------------------------------------------------------



#######################################################################################################################
# LOAD ALL DATA AND PARAMETER VALUES NEEDED FOR Mimi-FAIR
########################################################################################################################
# Description: This function loads and cleans up the Mimi-FAIR data so it can be more easily incorporated into the model.
#
# Function Arguments:
#
#       start_year:      First year to run the model.
#       end_year:        Final year to run the model.
#       rcp_scenario:    A string indicating which RCP scenario to use ("RCP26", "RCP45", "RCP60", & "RCP85").
#----------------------------------------------------------------------------------------------------------------------

load_fair_data <- function(start_year, end_year, rcp_scenario) {
  
  # Calculate indicies to extract RCP data (RCP data spans 1765-2500)
  index <- match(c(start_year, end_year), 1765:2500)
  start_index <- index[1]
  end_index <- index[2]
  
  # Create vector of names for minor greenhouse gases to loop over.
  other_ghg_names <- c("CF4", "C2F6", "C6F14", "HFC23", "HFC32", "HFC43_10", "HFC125", "HFC134a", "HFC143a", "HFC227ea", "HFC245fa", "SF6", "CFC_11", "CFC_12", "CFC_113", "CFC_114", "CFC_115", "CARB_TET", "MCF", "HCFC_22", "HCFC_141B", "HCFC_142B", "HALON1211", "HALON1202", "HALON1301", "HALON2402", "CH3BR", "CH3CL")
  
  #---------------------------------------
  # Read in relevant data sets.
  #---------------------------------------
  
  # RCP scenario emissions.
  rcp_emissions_raw <- read.csv(paste0("../data/fair_model_data/", rcp_scenario, "_EMISSIONS.csv"), skip=36)
  
  # Natural emissions for methane and nitrous oxide as estimated by FAIR team.
  natural_emissions_raw <- read.csv(paste0("../data/fair_model_data/natural_emissions.csv"), skip=3)
  
  # CMIP6 Solar forcing.
  cmip6_solar_forcing <- read.csv(paste0("../data/fair_model_data/cmip6_solar.csv"), skip=6)[start_index:end_index, "Radiative forcing"]
  
  # CMIP6 volcanic forcing.
  cmip6_volcano_forcing <- read.csv(paste0("../data/fair_model_data/cmip6_volcanic.csv"), skip=8)[start_index:end_index, "Volcanic ERF"]
  
  # Fraction of NOx emissions attibutable to aviation (for contrail RF).
  aviation_fraction_raw <- read.csv(paste0("../data/fair_model_data/aviNOx_fraction.csv"), skip=4)[,rcp_scenario]
  
  # Time-varying shares of anthropogenic methane attribuatable to fossil sources.
  ch4_fossil_frac_raw <- read.csv(paste0("../data/fair_model_data/fossilCH4_fraction.csv"), skip=4)[,rcp_scenario]
  
  # Information on various gas specieis.
  gas_data <- read.csv(paste0("../data/fair_model_data/fair_ghg_species_data.csv"), skip=11)
  
  #---------------------------------------
  # Emissions
  #---------------------------------------
  emissions <- data.frame(matrix(nrow = length(start_index:end_index),ncol = 1))
  
  emissions$FossilCO2 <- rcp_emissions_raw[start_index:end_index, "FossilCO2"]
  emissions$OtherCO2 <- rcp_emissions_raw[start_index:end_index, "OtherCO2"]
  emissions$CH4 <- rcp_emissions_raw[start_index:end_index, "CH4"]
  emissions$NaturalCH4 <- natural_emissions_raw[start_index:end_index, "ch4"]
  emissions$N2O <- rcp_emissions_raw[start_index:end_index, "N2O"]
  emissions$NaturalN2O <- natural_emissions_raw[start_index:end_index, "n2o"]
  emissions$NMVOC <- rcp_emissions_raw[start_index:end_index, "NMVOC"]
  emissions$CO <- rcp_emissions_raw[start_index:end_index, "CO"]
  emissions$NOx <- rcp_emissions_raw[start_index:end_index, "NOx"]
  emissions$SOx <- rcp_emissions_raw[start_index:end_index, "SOx"]
  emissions$BC <- rcp_emissions_raw[start_index:end_index, "BC"]
  emissions$OC <- rcp_emissions_raw[start_index:end_index, "OC"]
  emissions$NH3 <- rcp_emissions_raw[start_index:end_index, "NH3"]
  
  emissions <- emissions[,-1]
  
  # Other greenhouse gases
  for (i in other_ghg_names) {
    emissions[,i] <- rcp_emissions_raw[start_index:end_index, i]
  }
  
  #---------------------------------------
  # Gas Fractions
  #---------------------------------------
  gas_fractions <- data.frame(matrix(nrow = length(start_index:end_index),ncol = 1))
  
  gas_fractions$nox_aviation <- aviation_fraction_raw[start_index:end_index]
  gas_fractions$ch4_fossil <- ch4_fossil_frac_raw[start_index:end_index]
  
  gas_fractions <- gas_fractions[,-1]
  #---------------------------------------
  # Emission to Concentration Conversions
  #---------------------------------------
  
  # Set names for concentration conversions.
  emiss_conversions <- data.frame(gases  = c("CO2", "CH4", "N2O", other_ghg_names))
  
  # Mass of atmosphere (kg).
  mass_atmos <- 5.1352e18
  
  # Molecular weights of air, CO₂, N₂, CH₄, and other greenhouse gases.
  mol_wt_air <- gas_data[gas_data$gas == "AIR", "mol_weight"]
  mol_wt_carbon <- gas_data[gas_data$gas == "C", "mol_weight"]
  mol_wt_n2 <- gas_data[gas_data$gas == "N2", "mol_weight"]
  mol_wt_ch4 <- gas_data[gas_data$gas == "CH4", "mol_weight"]
  mol_wt_others <- gas_data[gas_data$gas %in% other_ghg_names, "mol_weight"]
  
  # Calculate CO₂ conversion from GtC to ppm.
  emiss2conc_carbon <- (mass_atmos / 1.0e18) * (mol_wt_carbon / mol_wt_air)
  
  # Emission to concentration for CH₄.
  emiss2conc_ch4 <- (mass_atmos / 1.0e18) * (mol_wt_ch4 / mol_wt_air)
  
  # Emission to concentration for N₂O.
  # Note: Use N₂ for N₂O conversion, from FAIR: "Funny units for nitrogen emissions - N₂O is expressed in N₂ equivalent."
  emiss2conc_n2o <- (mass_atmos / 1.0e18) * (mol_wt_n2 / mol_wt_air)
  
  # Conversion factors for other Kyoto Protocol or ozone-deple
  emiss2conc_others <- (mass_atmos / 1.0e18) * (mol_wt_others / mol_wt_air)
  
  # Combine values conversion values into a single data frame.
  emiss_conversions <- data.frame(emiss2conc = c(emiss2conc_carbon, emiss2conc_ch4, emiss2conc_n2o, emiss2conc_others))
  emiss_conversions$gases <- c("CO2","CH4","N2O",other_ghg_names)
  return(list(emissions, cmip6_volcano_forcing, cmip6_solar_forcing, gas_data, gas_fractions, emiss_conversions))
}

