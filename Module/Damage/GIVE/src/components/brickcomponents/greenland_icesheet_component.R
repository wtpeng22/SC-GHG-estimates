# --------------------
# Model Parameters
# --------------------
# --------------------
# Model Parameters
# --------------------
greenland_a <- NULL             # Sensitivity of the equilibrium volume to changes in temperature (mSLE °C⁻¹).
greenland_b <- NULL             # Equilibrium volume for 0 °C global surface temperature anomaly (mSLE).
greenland_alpha <- NULL            # Temperature sensitivity of Greenland ice sheet exponential decay rate (yr⁻¹ °C⁻¹).
greenland_beta <- NULL             # Exponential decay rate for 0 °C global surface temperature anomaly (yr⁻¹).
greenland_v0 <- NULL           # Initial volume of Greenland ice sheet (m SLE).
global_surface_temperature <- NULL # Global mean surface temperature relative to pre-industrial (°C).

# --------------------
# Model Variables
# --------------------
tau_inv <- NULL  # E-folding timescale of Greenland ice sheet volume changes due to changes in global temperature (yr⁻¹).
eq_volume <- NULL # Equilibrium ice sheet volume where sea level contribution is 0 (mSLE).
greenland_volume <- NULL # Volume of Greenland ice sheet (mSLE)
greenland_sea_level <- NULL # Cumulative sea level rise contribution from Greenland ice sheet (m).

# --------------------
# Model Equations
# --------------------

greenland_icesheet_run_timestep <- function(t) {
  
  # Calculate equilibrium ice sheet volume for current temperature anomaly.
  eq_volume[t] <<- greenland_a * global_surface_temperature[t] + greenland_b
  
  if (t==1) {
    
    # Set initial conditions for volume.
    greenland_volume[t] <<- greenland_v0
    
    # Calculate e-folding time scale based on initial conditions.
    tau_inv[t] <<- greenland_alpha * global_surface_temperature[t] + greenland_beta
    
  } else {
    
    # Calculate current Greenland ice sheet volume.
    greenland_volume[t] <<- greenland_volume[t-1] + tau_inv[t-1] * (eq_volume[t-1] - greenland_volume[t-1])
    
    # Calculate new e-folding time scale dependent on temperature and remaining ice sheet volume.
    tau_inv[t] <<- (greenland_alpha * global_surface_temperature[t] + greenland_beta) * (greenland_volume[t] / greenland_v0)
    
  }
  
  # Calculate total sea level contribution from Greenland Ice Sheet (with a check in case the ice sheet fully melts).
  if (greenland_volume[t] > 0.0) {
    greenland_sea_level[t] <<- greenland_v0 - greenland_volume[t]
  } else {
    greenland_sea_level[t] <<- greenland_v0
  }
}
