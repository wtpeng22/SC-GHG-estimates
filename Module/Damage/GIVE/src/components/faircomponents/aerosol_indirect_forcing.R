# --------------------------------------------------
# Indirect radiative forcing effect from aerosols.
# --------------------------------------------------

# This follows the `ghan2` indirect forcing calculations from FAIRv1.6.2
phi <- NULL # Scale factor.
b_SOx <- NULL # Sensitivity to sulfur oxides emissions.
b_POM <- NULL # Sensitivity to black carbon + organic carbon emissions.
SOx_emiss_pi <- NULL # Pre-industiral sulfur oxides emissions (Mt yr⁻¹).
BC_emiss_pi <- NULL # Pre-industiral black carbon emissions (Mt yr⁻¹).
OC_emiss_pi <- NULL # Pre-industrial organic carbon emissions (Mt yr⁻¹).
rf_scale_aero_indirect <- NULL # Scaling factor to capture effective radiative forcing uncertainty.
SOx_emiss <- matrix(nrow = length(time),ncol = 1) # Sulfur oxides emissions (MtS yr⁻¹).
BC_emiss <- matrix(nrow = length(time),ncol = 1) # Black carbon emissions (Mt yr⁻¹).
OC_emiss <- matrix(nrow = length(time),ncol = 1) # Organic carbon emissions (Mt yr⁻¹).

#Variable
pd_re <- matrix(nrow = length(time),ncol = 1) # Present-day indirect radiative forcing contribution from aerosols (Wm⁻²).
pi_re <- matrix(nrow = length(time),ncol = 1) # Pre-industrial indirect radiative forcing contribution from aerosols (Wm⁻²).
rf_aero_indirect <- matrix(nrow = length(time),ncol = 1) # Indirect radiative forcing from aerosols (Wm⁻²).

aerosol_indirect_forcing_run_timestep <- function(t) {
  
  # Calculate present-day forcing contribution.
  pd_re[t] <<- -phi * log(1.0 + SOx_emiss[t] / b_SOx + (BC_emiss[t] + OC_emiss[t]) / b_POM)
  
  # Calculate pre-industrial forcing contribution.
  pi_re[t] <<- -phi * log(1.0 + SOx_emiss_pi / b_SOx + (BC_emiss_pi + OC_emiss_pi) / b_POM)
  
  # Calculate indirect radiative forcing from aerosols (with potential scaling to account for radiative forcing uncertainty).
  rf_aero_indirect[t] <<- (pd_re[t] - pi_re[t]) * rf_scale_aero_indirect
}
