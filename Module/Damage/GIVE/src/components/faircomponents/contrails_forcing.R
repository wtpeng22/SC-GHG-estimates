# -------------------------------------------------------
# Radiative forcing from contrails
# -------------------------------------------------------

E_ref_contrails <- NULL   # Reference-year emissions of aviation nitrogen oxides (Mt yr⁻¹).
F_ref_contrails <- NULL   # Forcing from linear persistent contrails + contrail induced cirrus. The default for 2005 is from Lee et al, 2009 (Atmos. Environ., doi:10.1016/j.atmosenv.2009.04.024).
ref_is_NO2 <- NULL   # True if 'E_ref' is in units of NO₂ rather than N.
mol_weight_NO2 <- NULL   # Molecular mass of nitrogen dioxide.
mol_weight_N <- NULL   # Molecular mass of nitrogen.
NOx_emiss <- NULL   # Nitrogen oxides emissions.
frac <- NULL   # Fraction of total nitrogen oxides emissions due to aviation.
#rf_scale_contrails <- NULL   # Scaling factor to capture effective radiative forcing uncertainty.

##Variable
forcing_contrails <- NULL   # Forcing approximation from aviation contrails (Wm⁻²).

contrails_forcing_run_timestep <- function(t) {
  # Caluclate forcing from contrail.
  # Note: If 'E_ref' in units of NO₂ rather than N, use ratio of molecular weights for conversion.
  if (ref_is_NO2) {
    forcing_contrails[t] <<- NOx_emiss[t] * frac[t] * (F_ref_contrails/E_ref_contrails) * (mol_weight_NO2 / mol_weight_N)
  } else {
    forcing_contrails[t] <<- NOx_emiss[t] * frac[t] * (F_ref_contrails/E_ref_contrails)
  }
}

