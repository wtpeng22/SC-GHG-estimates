# ---------------------------------------------------------------------
# Radiative forcing from other well-mixed greenhouse gases.
# ---------------------------------------------------------------------

# Define Index
# other_ghg <- new_index()

# Define Parameters
other_ghg_pi <- matrix(nrow = length(other_ghg),ncol = 1)   # Initial (pre-industrial) concentration for other well-mixed greenhouse gases (ppt).
other_ghg_radiative_efficiency <- matrix(nrow = length(other_ghg),ncol = 1)   # Radiative efficiencies for other well-mixed greenhouse gases - from IPCC AR5 WG1 Ch8 SM (Wm⁻²ppb⁻¹).
conc_other_ghg <- matrix(nrow = length(time),ncol = length(other_ghg))   # Atmospheric concentrations for other well-mixed greenhouse gases (ppt).

# Define Variables
other_ghg_rf_total <- matrix(nrow = length(time),ncol = 1)   # Total radiative forcing for all well-mixed greenhouse gases (Wm⁻²).
other_ghg_rf <- matrix(nrow = length(time),ncol = length(other_ghg))   # Individual radiative forcings for each well-mixed greenhouse gas (Wm⁻²).

# Define function
other_ghg_forcing_run_timestep <- function(t) {
  for (g in 1:length(other_ghg)) {
    # Caluclate radiative forcing for individual gases.
    # Note: the factor of 0.001 here is because radiative efficiencies are given in Wm⁻²ppb⁻¹ and concentrations of minor gases are in ppt.
    other_ghg_rf[t, g] <<- (conc_other_ghg[t, g] - other_ghg_pi[g]) * other_ghg_radiative_efficiency[g] * 0.001
  }
  
  # Calculate total radiative forcing as sum of individual gas forcings.
  other_ghg_rf_total[t] <<- sum(other_ghg_rf[t, ])
}

