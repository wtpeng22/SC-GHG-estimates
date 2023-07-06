library(data.table)
library(dplyr)

country <- as.vector(countries)

SSP_source <- "Benveniste" # can be one of IIASA GDP, OECD Env-Growth, PIK GDP_32, and Benveniste
SSP <- "SSP2" # can be one of SSP1, SSP2, SSP3, SSP4, SSP5
emissions_source <- "Leach" # can be one of Leach
emissions_scenario <- "SSP245" # can be one of SSP119, SSP126, SSP245, SSP370, SSP585

country_names <- as.matrix(countries) # need the names of the countries from the dimension
ssp_timestep = damages_first:model_last

population <- matrix(nrow  = length(ssp_timestep), ncol = length(countries))

population_global <- matrix(nrow  = length(ssp_timestep), ncol = 1)
gdp <- matrix(nrow  = length(ssp_timestep), ncol = length(countries))
gdp_global <- matrix(nrow  = length(ssp_timestep), ncol = 1)

co2_emissions <- matrix(nrow  = length(ssp_timestep), ncol = 1)
ch4_emissions <- matrix(nrow  = length(ssp_timestep), ncol = 1)
n2o_emissions <- matrix(nrow  = length(ssp_timestep), ncol = 1)
sf6_emissions <- matrix(nrow  = length(ssp_timestep), ncol = 1)

# Load Socioeconomic Data as Needed
# population in millions of individuals
# GDP in billions of $2005 USD
  
socioeconomic_path <- file.path("..", "data", "ssp_socioeconomic", paste0(SSP_source, "_", SSP, ".csv"))
ssp_dict_key <- paste(SSP_source, "-", SSP)
  
# Define a list to store pre-loaded datasets
g_ssp_datasets <- list()
  
if (!(exists("g_ssp_datasets") && ssp_dict_key %in% names(g_ssp_datasets))) {
  g_ssp_datasets[[ssp_dict_key]] <- read.csv(socioeconomic_path)
}
  
# Check Countries - each country found in the model countries parameter
# must exist in the SSP socioeconomics dataframe
  
missing_countries <- character(0)
unique_country_list <- unique(g_ssp_datasets[[ssp_dict_key]]$country)
for (country in country_names) {
  if (!(country %in% unique_country_list)) {
    missing_countries <- c(missing_countries, country)
  }
}
if (length(missing_countries) > 0) {
  stop(sprintf("All countries in countries parameter must be found in SSPs component Socioeconomic Dataframe, the following were not found: %s", paste(missing_countries, collapse = ", ")))
}
  
# Load Emissions Data as Needed
# carbon dioxide emissions in GtC
# nitrous oxide emissions in MtN
# methane emissions in MtCH4
# SF6 emissions in MtSF6

emissions_path <- file.path("..", "data", "ssp_emissions", paste0(emissions_source, "_", emissions_scenario, ".csv"))
emissions_scenario_dict_key <- paste0(emissions_source, "-", emissions_scenario)
# Define a list to store pre-loaded datasets
g_emissions_scenario_datasets <- list()
  
if (!(emissions_scenario_dict_key %in% names(g_emissions_scenario_datasets))) {
  if (emissions_source == "Leach") {
    emissions_data <- read.csv(emissions_path, skip = 6) %>%
      # rename(year = X1, carbon_dioxide = X2, nitrous_oxide = X3, methane = X4, sf6 = X5) %>%
      rename(year = X) %>%
      select(year, carbon_dioxide, nitrous_oxide, methane, sf6)
  } else {
    emissions_data <- read.csv(emissions_path)
  }
  g_emissions_scenario_datasets[[emissions_scenario_dict_key]] <- emissions_data
}

SSPs_run_timestep <- function() {
  
  ssp_dict_key <- paste(SSP_source, "-", SSP)
  emissions_scenario_dict_key <- paste(emissions_source, "-", emissions_scenario,sep = "")
  year_label <- as.integer(ssp_timestep)
  
  # check that we only run the component where we have data
  if (!(all(year_label %in% g_ssp_datasets[[ssp_dict_key]]$year))) {
    stop(paste("Cannot run SSP component in year ", year_label, ", SSP socioeconomic variables not available for this model and year."))
  }
  if (!(all(year_label %in% g_emissions_scenario_datasets[[emissions_scenario_dict_key]]$year))) {
    stop(paste("Cannot run SSP component in year ", year_label, ", SSP emissions variables not available for this model and year."))
  }
  
  # ----------------------------------------------------------------------
  # Socioeconomic
  
  # filter the dataframe for values with the year matching ssp_timestep
  # t and only the SSP countries found in the model countries list,
  # already checked that all model countries are in SSP countries list
  socio_subset <- g_ssp_datasets[[ssp_dict_key]] %>%
    filter(year %in% year_label & country %in% country_names) %>%
    as.data.frame()
  
  # get the ordered indices of the SSP countries within the parameter 
  # of the model countries, already checked that all model countries
  # are in SSP countries list
  
  subset_country = socio_subset[socio_subset$year==2020,]$country
  subset_pop = matrix(socio_subset$pop, nrow = length(subset_country),byrow = TRUE) 
  subset_gdp = matrix(socio_subset$gdp, nrow = length(subset_country),byrow = TRUE) 
  
  order <- match(country_names, subset_country)
  
  population <<- t(subset_pop[order,])
  gdp <<- t(subset_gdp[order,])
  
  # add global data for future accessibility and quality control
  population_global <<- rowSums(population)
  gdp_global <<- rowSums(gdp)
  
  # ----------------------------------------------------------------------
  # Emissions
  
  emissions_subset <- g_emissions_scenario_datasets[[emissions_scenario_dict_key]] %>%
    filter(year %in% year_label) %>%
    as.data.frame()
  
  co2_emissions <<- emissions_subset$carbon_dioxide
  ch4_emissions <<- emissions_subset$methane
  n2o_emissions <<- emissions_subset$nitrous_oxide
  sf6_emissions <<- emissions_subset$sf6
}

