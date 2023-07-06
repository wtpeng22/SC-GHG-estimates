# ---------------------------------------------------------------------
# Radiative forcing from ozone-depleting substances.
# ---------------------------------------------------------------------

# ozone_depleting_substances <- Index()  # Index for ozone-depleting substances.

ods_pi <- matrix(nrow = length(ozone_depleting_substances),ncol = 1)   # Initial (pre-industrial) concentration for ozone-depleting substances (ppt).
ods_radiative_efficiency <- matrix(nrow = length(ozone_depleting_substances),ncol = 1)   # Radiative efficiencies for ozone-depleting substances - from IPCC AR5 WG1 Ch8 SM (Wm⁻²ppb⁻¹).
conc_ods <- matrix(nrow = length(time),ncol = length(ozone_depleting_substances))   # Atmospheric concentrations for ozone-depleting substances (ppt).

#Variable
ods_rf_total <- matrix(nrow = length(time),ncol = 1)  # Total radiative forcing for all ozone-depleting substances (Wm⁻²).
ods_rf <- matrix(nrow = length(time),ncol = length(ozone_depleting_substances))   # Individual radiative forcings for each ozone-depleting substances (Wm⁻²).

o3_depleting_substance_forcing_run_timestep <- function(t) {
  for (g in 1:length(ozone_depleting_substances)) {
    # Calculate radiative forcing for individual gases.
    # Note: the factor of 0.001 here is because radiative efficiencies are given in Wm⁻²ppb⁻¹ and concentrations of minor gases are in ppt.
    ods_rf[t, g] <<- (conc_ods[t, g] - ods_pi[g]) * ods_radiative_efficiency[g] * 0.001
  }
  
  # Calculate total radiative forcing as sum of individual gas forcings.
  ods_rf_total[t] <<- sum(ods_rf[t, ])
}

