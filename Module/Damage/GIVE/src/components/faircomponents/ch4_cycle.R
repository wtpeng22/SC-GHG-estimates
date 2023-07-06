# --------------------------------------------------
# Methane Cycle.
# --------------------------------------------------
emiss2conc_ch4 <- NULL            # Conversion between ppb/ppt concentrations and Mt/kt emissions.
CH4_pi <- NULL                    # Pre-industrial atmospheric methane concentration (ppb).
CH4_0 <- NULL                     # Initial atmospheric methane concentration for first model period (ppb).
tau_CH4 <- NULL                   # Atmospheric (e-folding) lifetime of methane.
emiss2conc_co2 <- NULL            # Conversion factor between GtC and ppm.
oxidation_frac <- NULL            # Fraction of methane lost through reaction with hydroxyl radical that is converted to carbon dioxide.
mol_weight_CH4 <- NULL            # Molecular mass of methane.
mol_weight_C <- NULL              # Molecular mass of carbon.

###Parameters
fossil_emiss_CH4 <- matrix(nrow = length(time),ncol = 1) # Fossil-fuel methane emissions (Mt CH4 yr^-1).
natural_emiss_CH4 <- matrix(nrow = length(time),ncol = 1) # Natural methane emissions (Mt CH4 yr^-1).
fossil_frac <- matrix(nrow = length(time),ncol = 1)   # Fraction of anthropogenic methane attributable to fossil sources.

###Variables
CH4 <- matrix(nrow = length(time),ncol = 1)            # Atmospheric methane concentration (ppb).
oxidised_CH4 <- matrix(nrow = length(time),ncol = 1)   # Methane that has been oxidized to carbon dioxide (ppb).
oxidised_CH4_GtC <- matrix(nrow = length(time),ncol = 1) # Methane that has been oxidized to carbon dioxide (GtC)

ch4_cycle_run_timestep <- function(t) {
  
  # Set initial methane concentration values.
  if (t==1) {
    CH4[t] <<- CH4_0
    oxidised_CH4[t] <<- 0.0
    oxidised_CH4_GtC[t] <<- 0.0
  } else {
    # Calculate atmospheric methane concentration.
    emiss_prev <<- fossil_emiss_CH4[t-1] + natural_emiss_CH4[t]
    emiss_curr <<- fossil_emiss_CH4[t] + natural_emiss_CH4[t]
    CH4[t] <<- CH4[t-1] - CH4[t-1] * (1.0 - exp(-1/tau_CH4)) + 0.5 * (emiss_prev + emiss_curr) * (1.0/emiss2conc_ch4)
    
    # Calculate carbon dioxide from oxidized methane (bounded below at 0.0).
    oxidised_CH4[t] <<- max(((CH4[t-1]-CH4_pi) * (1.0 - exp(-1.0/tau_CH4)) * (mol_weight_C/mol_weight_CH4 * 0.001 * oxidation_frac * fossil_frac[t])), 0.0)
    
    # Also provide oxidised CH4 in units of GtC.
    oxidised_CH4_GtC[t] <<- oxidised_CH4[t] * emiss2conc_co2
  }
}
