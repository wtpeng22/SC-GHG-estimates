# --------------------------------------------------
# Land use forcing from surface albedo change.
# --------------------------------------------------

alpha_CO2_land <- NULL   # Landuse radiative forcing coefficient.
landuse_emiss <- NULL   # RCP 'OtherCO2' emissions (GtC yr⁻¹).

# Variable
cumulative_emiss <- NULL   # Cumulative 'OtherCO2' emissions (GtC).
forcing_landuse <- NULL   # Forcing from land use change (Wm⁻²).

landuse_forcing_run_timestep <- function(t) {
  # Calculate cumulative land use emissions (based on RCP 'OtherCO2' emissions).
  if (t==1) {
    cumulative_emiss[t] <<- landuse_emiss[t]
  } else {
    cumulative_emiss[t] <<- cumulative_emiss[t-1] + landuse_emiss[t]
  }
  
  # Caluclate land use forcing (based on regression of non-fossil CO₂ emissions against AR5 land use forcing).
  forcing_landuse[t] <<- alpha_CO2_land * cumulative_emiss[t]
}


