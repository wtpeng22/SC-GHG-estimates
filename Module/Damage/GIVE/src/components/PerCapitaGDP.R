# ------------------------------------------------------------------------------
# Calculate the Per Capita GDP
# ------------------------------------------------------------------------------
library(data.table)

# Create index for countries
country <- as.vector(countries)

# # Define parameters and variables
# gdp <- matrix(nrow  = length(timestep_socioeconomic), ncol = length(countries))
# population <- matrix(nrow  = length(timestep_socioeconomic), ncol = length(countries))
pc_gdp <- matrix(nrow  = length(timestep_socioeconomic), ncol = length(countries))
global_pc_gdp <- matrix(nrow  = length(timestep_socioeconomic), ncol = 1)

# Define function to run timestep
pc_gd_run_timestep <- function(t) {
  
  # Calculate global per capita GDP
  global_pc_gdp[t] <<- sum(gdp[t,]) / sum(population[t, ]) * 1e3
  
  # Calculate country-level per capita GDP
  for (c in 1:length(country)) {
    pc_gdp[t,c] <<- gdp[t,c ] / population[t,c] * 1e3
  }
}



