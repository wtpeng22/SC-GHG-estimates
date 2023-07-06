# # ----------------------------------------------------------
# # Concentrations of Other Well-Mixed Greenhouse Gases.
# # ----------------------------------------------------------
# other_ghg <- NULL                                            # Index for other well-mixed greenhouse gases
# #ozone_depleting_substances <- NULL                         # Index for ozone-depleting substances.

tau_other_ghg <- matrix(nrow = length(other_ghg),ncol = 1)       # Atmospheric (e-folding) lifetime for each gas (years).
other_ghg_0 <-  matrix(nrow = length(other_ghg),ncol = 1)              # Initial (pre-industrial) concentration for each gas (ppt).
emiss2conc_other_ghg <-  matrix(nrow = length(other_ghg),ncol = 1)    # Conversion between ppt concentrations and kt emissions.
emiss_other_ghg <-  matrix(nrow = length(other_ghg),ncol = 1)    # Emissions for other well-mixed greenhouse gases (kt yr⁻¹).

# Variable
conc_other_ghg <- matrix(nrow = length(time),ncol = length(other_ghg))       # Atmospheric concentrations for other well-mixed greenhouse gases (ppt).
#conc_ods <- matrix(nrow = length(time),ncol = length(ozone_depleting_substances))  # Concentrations for ozone-depleting substances (a subset of 'conc_other_ghg' - used in stratospheric O₃ component).

other_ghg_cycles_run_timestep <- function(t) {
  for (g in 1:length(other_ghg)) {
    
    # Set initial concentration values.
    if (t==1) {
      conc_other_ghg[t, g] <<- other_ghg_0[g]
    } else {
      # Calculate concentrations based on simple one-box exponential decay model.
      conc_other_ghg[t, g] <<- conc_other_ghg[t-1, g] - conc_other_ghg[t-1, g] * (1.0 - exp(-1/tau_other_ghg[g])) + 0.5 * (emiss_other_ghg[t-1, g] + emiss_other_ghg[t, g]) * (1.0/emiss2conc_other_ghg[g])
    }
  }
  
  # For convenience reasons, create a subset variable for ozone-depleting substances (indices 13-28) used in other model components.
  # TODO: Remove this type of indexing (leftover from original FAIR code).
  #conc_ods[t, ] <<- conc_other_ghg[t, 13:28]
}
