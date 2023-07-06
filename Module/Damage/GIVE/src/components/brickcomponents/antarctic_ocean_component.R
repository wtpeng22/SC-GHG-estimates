# --------------------
# Model Parameters

anto_alpha <- NULL  # Sensitivity of the Antarctic Ocean temperature to global surface temperature (°C °C⁻¹).
anto_beta <- NULL  # Antarctic ocean temperature for 0 °C global surface temperature anomaly (°C).
seawater_freeze <- NULL  # Freezing temperature of seawater (°C).
global_surface_temperature <- NULL  # Global mean surface temperature relative to pre-industrial (°C).

# --------------------
# Model Variables
# --------------------

anto_temperature <- vector(length = length(time))  # Ocean surface temperature at Antarctica (°C).

# --------------------
# Model Equations
# --------------------

antarctic_ocean_run_timestep <- function(t) {
  # Define temporary numerator and denominator terms (just for convenience).
  term_1 <<- anto_alpha * global_surface_temperature[t] + anto_beta - seawater_freeze
  term_2 <<- 1 + exp(- (anto_alpha * global_surface_temperature[t] + anto_beta - seawater_freeze) / anto_alpha)
  
  # Calculate antarctic ocean temperature (bounded below by seawater freezing point).
  anto_temperature[t] <<- seawater_freeze + (term_1 / term_2)
}
