# # -----------------------------------------------------
# # Concentrations of Ozone-Depleting Substances
# # -----------------------------------------------------
# 
# ozone_depleting_substances <- NULL # Index for ozone-depleting substances.

tau_ods <-matrix(nrow = length(ozone_depleting_substances),ncol = 1)     # Atmospheric (e-folding) lifetime for each gas (years).
ods_0 <- matrix(nrow = length(ozone_depleting_substances),ncol = 1)          # Initial (pre-industrial) concentration for each gas (ppt).
emiss2conc_ods <- matrix(nrow = length(ozone_depleting_substances),ncol = 1)  # Conversion between ppt concentrations and kt emissions.
emiss_ods <- matrix(nrow = length(ozone_depleting_substances),ncol = 1) # Emissions for other well-mixed greenhouse gases (kt yr⁻¹).

conc_ods <- matrix(nrow = length(time),ncol = length(ozone_depleting_substances))   # Atmospheric concentrations for ozone-depleting substances (ppt).
#conc_ods <- matrix(nrow = length(time),ncol = length(ozone_depleting_substances))  # Concentrations for ozone-depleting substances (a subset of 'conc_other_ghg' - used in stratospheric O₃ component).


o3_depleting_substance_cycles_run_timestep <- function(t) {
  for (g in 1:length(ozone_depleting_substances)) {
    # Set initial concentration values.
    if (t==1) {
      conc_ods[t, g] <<- ods_0[g]
    } else {
      # Calculate concentrations based on simple one-box exponential decay model.
      conc_ods[t, g] <<- conc_ods[t-1, g] - conc_ods[t-1, g] * (1.0 - exp(-1/tau_ods[g])) + 0.5 * (emiss_ods[t-1, g] + emiss_ods[t, g]) * (1.0/emiss2conc_ods[g])
    }
  }
  
  # For convenience reasons, create a subset variable for ozone-depleting substances (indices 13-28) used in other model components.
  # TODO: Remove this type of indexing (leftover from original FAIR code).
  #conc_ods[t,] <<- conc_other_ghg[t, 13:28]
}



