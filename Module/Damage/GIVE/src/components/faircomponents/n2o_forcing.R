# ---------------------------------------------------------------
# Radiative forcing from nitrous oxide
# ---------------------------------------------------------------

# Note: Modified Etminan relationship from Meinshausen et al 2019 https://gmd.copernicus.org/preprints/gmd-2019-222/gmd-2019-222.pdf (Table 3)

a2 <- NULL             # Nitrous oxide forcing coefficient.
b2 <- NULL             # Nitrous oxide forcing coefficient.
c2 <- NULL             # Nitrous oxide forcing coefficient.
d2 <- NULL             # Nitrous oxide forcing coefficient.
N2O_pi <- NULL          # Initial (pre-industrial) nitrous oxide concentration (ppb).
#rf_scale_N2O <- NULL   # Scaling factor to capture effective radiative forcing uncertainty.
CO2 <- matrix(nrow = length(time),ncol = 1)  # Atmospheric carbon dioxide concentration (ppm).
N2O <- matrix(nrow = length(time),ncol = 1)  # Atmospheric nitrous oxide concentration (ppb).
CH4 <- matrix(nrow = length(time),ncol = 1)  # Atmospheric methane concentration (ppb).

#Variable
rf_n2o <- matrix(nrow = length(time),ncol = 1) # Forcing from atmospheric nitrous oxide concentrations (Wm⁻²).

n2o_forcing_run_timestep <- function(t) {
  # Calculate nitrous oxide radiative forcing.
  rf_n2o[t] <<- (a2 * sqrt(CO2[t]) + b2 * sqrt(N2O[t]) + c2 * sqrt(CH4[t]) + d2) * (sqrt(N2O[t]) - sqrt(N2O_pi))
}

