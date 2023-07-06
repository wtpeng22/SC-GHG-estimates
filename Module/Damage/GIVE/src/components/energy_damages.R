# --------------------------------------------------
# Energy-Use Damages (based on Clarke et al. 2018)
# --------------------------------------------------
# Index for countries in the GCAM regions used for energy damage functions.
energy_countries <- as.vector(country)

# # Coefficient relating global temperature to change in energy expenditures as a share of GDP.
# beta_energy <- matrix(nrow = length(energy_countries),ncol = 1)
# 
# # Country-level GDP (billions US $2005 / yr").
# gdp <- matrix(nrow = length(timestep_damage),ncol = length(energy_countries))
# 
# # Global average surface temperature anomaly relative to pre-industrial (°C).
# temperature <- matrix(nrow = length(timestep_damage),ncol = 1)

# Change in energy expenditures in dollars (billions US $2005 / yr).
energy_costs_dollar <- matrix(nrow = length(timestep_damage),ncol = length(energy_countries))

# Change in energy expenditures as a share of GDP (Δ gdp share / °C).
energy_costs_share <- matrix(nrow = length(timestep_damage),ncol = length(energy_countries))

energy_costs_run_timestep <- function(t) {
  
  for (c in 1:length(energy_countries)) {
    
    # Calculate additional energy expenditures as a share of GDP (coefficient gives percentages, so divide by 100 to get share).
    energy_costs_share[t, c] <<- beta_energy[c] * temperature[t] / 100.0
    
    # Calculate additional energy expenditures in monetary terms.
    energy_costs_dollar[t, c] <<- energy_costs_share[t, c] * gdp[t, c]
  }
}

