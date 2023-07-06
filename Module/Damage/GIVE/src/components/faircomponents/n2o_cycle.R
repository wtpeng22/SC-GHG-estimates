# --------------------------------------------------
# Nitrous Oxide Cycle.
# --------------------------------------------------
# Define model parameters

emiss2conc_n2o <- NULL # Conversion between ppb/ppt concentrations and Mt/kt emissions.
N2O_0 <- NULL # Initial nitrous oxide concentration in first model period (ppb).
tau_N2O <- NULL # Atmospheric (e-folding) lifetime of N2O (dfault = 121 years).

fossil_emiss_N2O <- matrix(nrow = length(time),ncol = 1) # Fossil-fuel nitrous oxide emissions (Mt N2 yr⁻¹).
natural_emiss_N2O <- matrix(nrow = length(time),ncol = 1) # Natural nitrous oxide emissions (Mt N2 yr⁻¹).

###Variables
N2O <- matrix(nrow = length(time),ncol = 1) # Atmospheric nitrous oxide concentration (ppb).

# Define model function
n2o_cycle_run_timestep <- function(t) {
  
  # Set initial nitrous oxide concentration value.
  if (t==1) {
    N2O[t] <<- N2O_0
  } else {
    # Calculate atmospheric nitrous oxide concentration.
    # Note: natural emissions always for period 't' in FAIR code.
    emiss_prev <<- fossil_emiss_N2O[t-1] + natural_emiss_N2O[t]
    emiss_curr <<- fossil_emiss_N2O[t] + natural_emiss_N2O[t]
    N2O[t] <<- N2O[t-1] - N2O[t-1] * (1.0 - exp(-1/tau_N2O)) + 0.5 * (emiss_prev + emiss_curr) * (1.0/emiss2conc_n2o)
  }
}
