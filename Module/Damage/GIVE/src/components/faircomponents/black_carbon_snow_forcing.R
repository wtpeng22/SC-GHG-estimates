# --------------------------------------------------
# Radiative forcing from black carbon on snow.
# --------------------------------------------------

E_ref_bc <- NULL # Reference year emissions.
F_ref_bc <- NULL # Reference forcing value (W/m⁻²).
BC_emiss <- matrix(nrow = length(time),ncol = 1) # Black carbon emissions (Mt yr⁻¹).

#Variable
forcing_BC_snow <- matrix(nrow = length(time),ncol = 1)  # Radiative forcing from black carbon (Wm⁻²).

black_carbon_snow_forcing_run_timestep <- function(t) {
  # Calculate forcing for BC.
  # Assumes a scaling factor so the 2011 relationship between BC forcing (best AR5 estimate) and emissions (Meinshausen) holds for all years.
  forcing_BC_snow[t] <<- BC_emiss[t] * (F_ref_bc / E_ref_bc)
}

