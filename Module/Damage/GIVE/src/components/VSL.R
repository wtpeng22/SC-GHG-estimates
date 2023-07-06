# ------------------------------------------------------------------------------
# Calculate the Value of a Statistical Life (following the FUND equations & 
# parameterization).
# ------------------------------------------------------------------------------

library(dplyr)

country <- as.vector(countries)

vsl_mortality <- matrix(nrow  = length(timestep_damage), ncol = length(countries))

# Define function to calculate VSL for a given timestep
vsl_run_timestep <- function(t) {
  for (c in 1:length(country)) {
    vsl_mortality[t,c] <<- alpha * (pc_gdp[t, c] / y0) ^ epsilon
  }
}




