library(dplyr)

# --------------------
# Parameters
# --------------------

te_A                <- NULL             # Global ocean surface area (m²).
te_C                <- NULL             # Heat capacity of conservative temperature (J kg⁻¹ °C⁻¹).
te_rho                <- NULL            # Approximate density of global ocean (kg m⁻³).
te_alpha                <- NULL             # Global ocean-averaged thermal expansion coefficient (kg m⁻³ °C⁻¹).
te_s0               <- NULL             # Initial sea level rise due to thermal expansion designated in 1850 (m SLE).
ocean_heat_mixed    <- NULL            # Ocean heat content anomaly of mixed layer (10²² J).
ocean_heat_interior <- NULL            # Ocean heat content anomaly of interior ocean (10²² J).

# --------------------
# Model Variables
# --------------------

ocean_heat_content <- NULL   # Sum of ocean heat content in mixed layer and interior ocean.
delta_oceanheat        <- NULL   # Change in total ocean heat content (J).
te_sea_level       <- NULL   # Cumulative sea level rise due to thermal expansion (m).

# --------------------
# Model Equations
# --------------------

thermal_expansion_run_timestep <- function(t) {
  
  # Set initial conditions.
  if (t==1) {
    te_sea_level[t] <<- te_s0
    delta_oceanheat[t] <<- 0
    ocean_heat_content[t] <<- ocean_heat_mixed[t] + ocean_heat_interior[t]
  } else {
    # Calculate total ocean heat content anomaly.
    ocean_heat_content[t] <<- ocean_heat_mixed[t] + ocean_heat_interior[t]
    
    # Calculate change in total ocean heat content and convert to Joules.
    delta_oceanheat[t] <<- (ocean_heat_content[t] - ocean_heat_content[t-1]) * 1e22
    
    # Calculate thermal expansion contribution to sea level rise given the change in global ocean heat content.
    te_sea_level[t] <<- te_sea_level[t-1] + delta_oceanheat[t] * te_alpha / (te_A * te_C * te_rho^2)
  }
}
