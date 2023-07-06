# ------------------------------------------------------------
# Temperature Mortality Damages (based on Cromar et al. 2021)
# ------------------------------------------------------------
# Cromar Mortality Damages (based on Cromar et al. 2019)
# Define component

country = as.vector(countries);
# beta_mortality =  matrix(nrow  = length(countries), ncol = 1);
# baseline_mortality_rate = matrix(nrow  = length(timestep_damage), ncol = length(countries));
# temperature = NULL;
# population = NULL;
# vsl = NULL;
mortality_change = matrix(nrow  = length(timestep_damage), ncol = length(countries));
mortality_costs = matrix(nrow  = length(timestep_damage), ncol = length(countries));
excess_death_rate = matrix(nrow  = length(timestep_damage), ncol = length(countries));
excess_deaths = matrix(nrow  = length(timestep_damage), ncol = length(countries));

cromar_mortality_damages_run_timestep = function(t) {
  
  for (c in 1:length(country)) {
    # Calculate change in a country's baseline mortality rate due to combined effects of heat and cold.
    mortality_change[t, c] <<- beta_mortality[c] * temperature[t]
    
    # Calculate additional deaths per 1,000 population due to cold and heat.
    excess_death_rate[t, c] <<- baseline_mortality_rate[t, c] * mortality_change[t, c]
    
    # Calculate additional deaths that occur due to cold and heat (assumes population units in millions of persons so converts to thousands to match deathrates units).
    excess_deaths[t, c] <<- (population[t, c] * 1000) * excess_death_rate[t, c]
    
    # Multiply excess deaths by the VSL.
    mortality_costs[t, c] <<- vsl_mortality[t, c] * excess_deaths[t, c]
  }
}

