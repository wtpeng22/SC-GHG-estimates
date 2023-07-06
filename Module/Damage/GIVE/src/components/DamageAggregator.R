library(deSolve)

# Define indices
fund_regions <- country <- energy_countries <- numeric()
time <- seq(1, 100)

# Define parameters
include_cromar_mortality <- TRUE
include_ag <- TRUE
include_slr <- TRUE
include_energy <- TRUE
include_dice2016R2 <- FALSE
include_hs_damage <- FALSE

damage_cromar_mortality <- matrix(0, nrow=length(time), ncol=length(country))
colnames(damage_cromar_mortality) <- country
row.names(damage_cromar_mortality) <- time
damage_ag <- matrix(0, nrow=length(time), ncol=length(fund_regions))
colnames(damage_ag) <- fund_regions
row.names(damage_ag) <- time
damage_energy <- matrix(0, nrow=length(time), ncol=length(energy_countries))
colnames(damage_energy) <- energy_countries
row.names(damage_energy) <- time
damage_dice2016R2 <- rep(0, length(time))
names(damage_dice2016R2) <- time
damage_hs <- rep(0, length(time))
names(damage_hs) <- time
gdp <- matrix(0, nrow=length(time), ncol=length(country))
colnames(gdp) <- country
row.names(gdp) <- time

# Define variables
total_damage <- total_damage_share <- total_damage_domestic <- cromar_mortality_damage <- agriculture_damage <- energy_damage <- cromar_mortality_damage_domestic <- agriculture_damage_domestic <- energy_damage_domestic <- rep(0, length(time))
names(total_damage) <- names(total_damage_share) <- names(total_damage_domestic) <- names(cromar_mortality_damage) <- names(agriculture_damage) <- names(energy_damage) <- names(cromar_mortality_damage_domestic) <- names(agriculture_damage_domestic) <- names(energy_damage_domestic) <- time

# Define model function
damage_aggregator <- function(t, y, p) {
  
  # Compute global annual aggregates
  cromar_mortality_damage[t] <- sum(p["damage_cromar_mortality"][t,])
  agriculture_damage[t] <- sum(p["damage_ag"][t,]) * 1e9 
  energy_damage[t] <- sum(p["damage_energy"][t,]) * 1e9
  
  total_damage[t] <- 
    (include_cromar_mortality * cromar_mortality_damage[t]) +
    (include_ag * agriculture_damage[t]) +
    (include_energy * energy_damage[t]) +
    (include_dice2016R2 * p["damage_dice2016R2"][t] * 1e9) +
    (include_hs_damage * p["damage_hs"][t] * 1e9)
  
  gdp[t,] <- sum(p["gdp"][t,]) * 1e9
  
  total_damage_share[t] <- total_damage[t] / gdp[t,]
  
  ## domestic annual aggregates - for interim model outputs and partial SCCs
  cromar_mortality_damage_domestic[t] <- p["damage_cromar_mortality"][t,174]
  agriculture_damage_domestic[t] <- p["damage_ag"][t,1] * 1e9
  energy_damage_domestic[t] <- p["amage_energy"][t,12] * 1e9
  
 # Calculate domestic damages
  v$total_damage_domestic[t] <-
    (ifelse(p$include_cromar_mortality, v$cromar_mortality_damage_domestic[t], 0)) +
    (ifelse(p$include_ag, v$agriculture_damage_domestic[t], 0)) +
    (ifelse(p$include_energy, v$energy_damage_domestic[t], 0))
}
