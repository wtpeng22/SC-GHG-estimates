# --------------------
# Model Parameters
# --------------------

seawater_freeze             <- NULL             # Freezing temperature of seawater (°C).
ais_rho_ice                   <- NULL             # Ice density (kg m⁻³).
ais_rho_seawater              <- NULL             # Average seawater density (kg m⁻³).
ais_rho_rock                  <- NULL             # Rock density (kg m⁻³).
ais_sea_level0              <- NULL             # Initial sea level rise from Antarctic ice sheet (m).
ais_ocean_temperature0      <- NULL             # Initial high-latitude ocean subsurface temperaturea (°C).
ais_radius0                 <- NULL             # Initial reference ice sheet radius (m).
ais_bedheight0              <- NULL             # Undisturbed bed height at the continent center (m).
ais_slope                   <- NULL             # Slope of the undisturbed ice sheet bed before loading.
ais_mu                       <- NULL             # Profile parameter for parabolic ice sheet surface, related to ice stress (m^0.5).
ais_runoffline_snowheight0  <- NULL             # Height of runoff line above which precipitation accumulates as snow for 0 °C local temperatures (m).
ais_c                       <- NULL             # Proportionality constant for the dependency of runoff line height on local Antarctic surface temperaure (m/°C)
ais_precipitation0          <- NULL             # Annual precipitation for for 0 °C local temperature (m ice equivalent yr⁻¹).
ais_kappa                       <- NULL             # Coefficient for exponential dependency of precipitation on Antarctic temperature.
ais_nu                       <- NULL             # Proportionality constant relating the runoff decrease with height to precipitation.
ais_iceflow0                <- NULL             # Proportionality constant for ice flow at the grounding line (m yr⁻¹).
ais_gamma                       <- NULL             # Power for the relation of ice flow speed to water depth.
ais_alpha                       <- NULL             # Partition parameter for effect of ocean subsurface temperature on ice flux.
ais_temperature_coefficient <- NULL             # Linear regression coefficient relating global mean surface temperature to Antarctic mean surface temperature.
ais_temperature_intercept   <- NULL             # Linear regression intercept term relating global mean surface temperature to Antarctic mean surface temperature.
ais_local_fingerprint       <- NULL             # Mean AIS fingerprint at AIS shore.
ocean_surface_area          <- NULL             # Surface area of the ocean (m²).
temperature_threshold       <- NULL             # Trigger temperature at which fast dynamics disintegration begins to occur (°C).
lambda                      <- NULL             # Antarctic fast dynamics disintegration rate when temperature is above trigger temperature (m yr⁻¹).
include_ais_DSL             <- NA               # Check for whether 'delta_sea_level' represents contribution from all components including AIS (true) or all other components excluding AIS (false).
global_surface_temperature  <- NULL             # Global mean surface temperature anomaly relative to pre-industrial (°C).
antarctic_ocean_temperature <- NULL             # High-latitude ocean subsurface temperaturea (°C).
global_sea_level            <- NULL             # Global sea level rise (m).

# --------------------
# Model Variables
# --------------------
delta_sea_level <- NULL  # rate of sea-level change from gic, gis, and te components (m/yr)
antartic_surface_temperature <- NULL  # Antarctic mean surface temperature ANOMALY for the previous year
runoffline_snowheight <- NULL  # Height of runoff line above which precipitation accumulates as snow (m)
precipitation <- NULL  # Annual precipitation (m ice)
ais_beta <- NULL  # mass balance gradient  (m^0.5) (degree of change in net mass balance as a function of elevation)
center2runoffline_distance <- NULL  # Distance from the continent center to where the runoff line intersects the ice sheet surface (m)
center2sea_distance <- NULL  # Distance from the continent center to where the ice sheet enters the sea (m).
ais_radius <- NULL  # ice sheet radius (m)
waterdepth <- NULL  # water depth at grounding line
ice_flux <- NULL  # Total ice flux across the grounding line (m3/yr)
ais_volume <- NULL  # volume of antarctic ice sheet (m^3)
beta_total <- NULL  # total rate of accumulation of mass on the Antarctic Ice Sheet (m^3/yr)
disintegration_rate <- NULL  # rate of disintegration
disintegration_volume <- NULL  # Volume of disintegrated ice during this time step [m SLE]
ais_sea_level <- NULL  # the volume of the antarctic ice sheet in SLE equivilent (m)

anto_temperature = antarctic_ocean_temperature
sea_level_rise = global_sea_level

antarctic_icesheet_run_timestep <- function(t) {
  # Define delta_sea_level terms (note, delta_sea_level initializes differently before being able to take differences from previous two periods).
  if (t==1) {
    delta_sea_level[t] <<- 0
  } else if (t == 2) {
    delta_sea_level[t] <<- sea_level_rise[t-1]
  } else {
    delta_sea_level[t] <<- sea_level_rise[t-1] - sea_level_rise[t-2]
  }
  
  # ---------------------------#
  # --- Initial Conditions --- #
  # ---------------------------#
  
  # Note: some initial values not required and will just be 'missing' model values.
  if (t==1) {
    # Initial distance from continent center to where the ice sheet enters the sea.
    center2sea_distance[t] <<- (ais_bedheight0 - ais_sea_level0) / ais_slope
    
    # Ice sheet radius.
    ais_radius[t] <<- ais_radius0
    
    # Calculate initial ice sheet volume.
    ais_volume[t] <<- pi * (1 + (ais_rho_ice/(ais_rho_rock - ais_rho_ice))) * ((8/15) * ais_mu^0.5 * ais_radius0^2.5 - (1/3) * ais_slope * ais_radius0^3)
    
    # Check if ice sheet volume requires marine ice sheet correction term.
    if (ais_radius0 > center2sea_distance[t]) {
      ais_volume[t] <<- ais_volume[t] - (pi * (ais_rho_seawater/(ais_rho_rock - ais_rho_ice)) * ((2/3) * ais_slope * (ais_radius0^3 - center2sea_distance[t]^3) - ais_bedheight0 * (ais_radius0^2 - center2sea_distance[t]^2)))
    }
    
    # Set initial sea level rise condition.
    ais_sea_level[t] <<- ais_sea_level0
  }
  
  # ----------------------------------------------#
  # --- Model Equations for All Other Periods --- #
  # ----------------------------------------------#
  
  else {
    # Calculate distance from the continent center to sea level.
    center2sea_distance[t] <<- (ais_bedheight0 - sea_level_rise[t-1]) / ais_slope
    
    # Calculate Antarctic surface temperature (based on previous year's global surface temperature anomaly).
    # Note: terms come from regression to predict global surface temperature anomalies from an Antarctic surface temperature reconstruction.
    antartic_surface_temperature[t] <<- (global_surface_temperature[t-1] - ais_temperature_intercept) / ais_temperature_coefficient
    
    # Calculate precipitation.
    precipitation[t] <<- ais_precipitation0 * exp(ais_kappa * antartic_surface_temperature[t])
    
    # Calculate mass balance gradient.
    ais_beta[t] <<- ais_nu * (precipitation[t]^(0.5))
    
    # Height of runoff line above which precipitation accumulates as snow.
    runoffline_snowheight[t] <<- ais_runoffline_snowheight0 + ais_c * antartic_surface_temperature[t]
    
    # Distance from the continent center to where the runoff line intersects the ice sheet surface.
    # Note, this term utilized only when Antarctic surface temperature warm enough for runoff to occur.
    center2runoffline_distance[t] <<- (ais_radius[t-1] - ((runoffline_snowheight[t] - ais_bedheight0 + ais_slope * ais_radius[t-1])^2) / ais_mu)
    
    # Total rate of accumulation of mass on the Antarctic ice sheet.
    # Note: Terms with 'center2runoffline_distance' only used if runoff line height where precipitation accumulates as snow > 0.
    if (runoffline_snowheight[t] > 0) {
      # Calculate additional terms separately (just for convenience).
      term1 <<- pi * ais_beta[t] * (runoffline_snowheight[t] - ais_bedheight0 + ais_slope * ais_radius[t-1]) * (ais_radius[t-1]^2 - center2runoffline_distance[t]^2)
      term2 <<- (4 * pi * ais_beta[t] * ais_mu^0.5 * (ais_radius[t-1] - center2runoffline_distance[t]) ^(5/2)) / 5
      term3 <<- (4 * pi * ais_beta[t] * ais_mu^0.5 * ais_radius[t-1] * (ais_radius[t-1] - center2runoffline_distance[t]) ^(3/2)) / 3
      
      beta_total[t] <<- precipitation[t] * pi * (ais_radius[t-1]^2) - term1 - term2 + term3
    } else {
      beta_total[t] <<- precipitation[t] * pi * (ais_radius[t-1]^2)
    }
  # ------------------------------#
  # --- Marine Ice Sheet Case --- #
  # ------------------------------#
  
  # Create temporary variable reprsenting dV/dR ratio (for convenience).
  fac <<- pi * (1 + ais_rho_ice/(ais_rho_rock - ais_rho_ice)) * (4/3 * ais_mu^0.5 * ais_radius[t-1]^1.5 - ais_slope * ais_radius[t-1]^2)
  
  # Calculate model equations and adjustments when there is a marine ice sheet & grounding line.
  if (ais_radius[t-1] > center2sea_distance[t]) {
    
    # Apply adjustment term for dV/dR variable.
    fac <<- fac - (2 * pi * (ais_rho_seawater/(ais_rho_rock - ais_rho_ice)) * (ais_slope * ais_radius[t-1]^2 - ais_bedheight0 * ais_radius[t-1]))
    
    # Water depth at grounding line (average around the ice sheet periphery over all ice streams).
    waterdepth[t] <<- ais_slope * ais_radius[t-1] - ais_bedheight0 + sea_level_rise[t-1]
    
    # Create temporary variable for ice speed at grounding line.
    speed <<- ais_iceflow0 * ((1 - ais_alpha) + ais_alpha * ((anto_temperature[t-1] - seawater_freeze)/(ais_ocean_temperature0 - seawater_freeze))^2) * (waterdepth[t]^ais_gamma) / ((ais_slope * ais_radius0 - ais_bedheight0) ^(ais_gamma - 1))  
    
    # Total ice flux across the grounding line.
    ice_flux[t] <<- -(2 * pi * ais_radius[t-1] * (ais_rho_seawater/ais_rho_ice) * waterdepth[t]) * speed
    
    # Create temporary variable for convenience (c_iso is ratio ISO / delta_sea_level).
    c_iso <<- (2 * pi * (ais_rho_seawater/(ais_rho_rock - ais_rho_ice)) * (ais_slope * center2sea_distance[t]^2 - (ais_bedheight0 / ais_slope) * center2sea_distance[t]))
    
    # Additional temporary variable requiring a check for whether sea level rise also includes Antarctic ice sheet contribution (or just contributions from other BRICK components).
    if (include_ais_DSL) {
      ISO <<- c_iso * delta_sea_level[t]
    } else {
      ISO <<- ((1-c_iso)/c_iso) * (delta_sea_level[t] - ais_local_fingerprint * (beta_total[t] - ice_flux[t]) / ocean_surface_area)
    }
    
  } else {
    # Set terms for case without a marine ice sheet & grounding line.
    waterdepth[t] <<- NaN
    ice_flux[t]   <<- 0.0
    ISO             <<- 0.0
  }
  # ------------------------------------------#
  # --- Antarctic Ice Sheet Fast Dynamics --- #
  # ------------------------------------------#
  
  # If local temperature above critical temperature, ice sheets disintegrate more rapidly.
  if (antartic_surface_temperature[t] > temperature_threshold) {
    disintegration_rate[t] <<- -lambda * 24.78e15 / 57.0
  } else {
    disintegration_rate[t] <<- 0.0
  }
  
  # Calculate total disintegration
  disintegration_volume[t] <<- -disintegration_rate[t] * 57.0 / 24.78e15
  
  # ---------------------------------------------------#
  #   --- Antarctic Ice Sheet Sea Level Contribution ---
  #   ---------------------------------------------------#
  #   Calculate ice sheet radius
  ais_radius[t] <<- ais_radius[t-1] + (beta_total[t] + ice_flux[t] + ISO + disintegration_rate[t]) / fac
  
  # Calculate ice sheet volume (accounting for potential rapid disintegration)
  ais_volume[t] <<- ais_volume[t-1] + beta_total[t] + ice_flux[t] + ISO + disintegration_rate[t]
  
  # Calculate sea level contribution (assuming steady state present day volume corresponding to 57 mSLE)
  ais_sea_level[t] <<- 57.0 * (1 - ais_volume[t] / ais_volume[1])
 }
}
              
  
    

