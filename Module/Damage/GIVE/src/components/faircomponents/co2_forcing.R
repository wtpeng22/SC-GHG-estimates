# ---------------------------------------------------------------
# Radiative forcing from carbon dioxide
# ---------------------------------------------------------------

# Note: Modified Etminan relationship from Meinshausen et al 2019 https://gmd.copernicus.org/preprints/gmd-2019-222/gmd-2019-222.pdf (Table 3)

a1 <- NULL # Carbon dioxide forcing coefficient.
b1 <- NULL # Carbon dioxide forcing coefficient.
c1 <- NULL # Carbon dioxide forcing coefficient.
d1 <- NULL # Carbon dioxide forcing coefficient.
F2x <- NULL # Radiative forcing from a doubling of CO₂ (W m⁻²).
N2O_pi <- NULL # Initial (pre-industrial) nitrous oxide concentration (ppb).
CO2_pi <- NULL # Initial (pre-industrial) carbon dioxide concentration (ppm).
# adjust_F2x <- NULL # Boolean for whether or not to calculate a scaling scaling term to keep forcing from 2x CO₂ consistent.
# #rf_scale_CO₂ <- NULL # Scaling factor to capture effective radiative forcing uncertainty.
N2O <- matrix(nrow = length(time),ncol = 1) # Atmospheric nitrous oxide concentration (ppb).
CO2 <- matrix(nrow = length(time),ncol = 1) # Atmospheric carbon dioxide concentration (ppm).

#Variable
F2x_scale <- NULL # Radiative forcing scaling term (to keep forcing from 2x CO₂ consistent).
CO2_alpha <- matrix(nrow = length(time),ncol = 1) # Radiative forcing scaling coefficient.
N2O_alpha <- matrix(nrow = length(time),ncol = 1) # Radiative forcing scaling coefficient.
CO2_alpha_max <- NULL # Concentration value where forcing coefficient scaling term reaches its maximum value (ppm).
rf_co2 <- matrix(nrow = length(time),ncol = 1) # Forcing from atmospheric carbon dioxide concentrations (Wm⁻²).

co2_forcing_run_timestep <- function(t) {
  # Calculate some values required for CO₂ radiative forcing calculations.
  # Calculate maximum value for forcing scaling coefficient.
  CO2_alpha_max <<- CO2_pi - b1 / (2.0 * a1)
  
  # "Tune the coefficient of CO₂ forcing to acheive desired F2x, using pre-industrial CO₂ and N₂O. F2x_etminan ~= 3.801."
  if (adjust_F2x == TRUE) {
    # Calculate F2x from Etminan radiative forcing equations.
    F2x_etminan <<- (-2.4e-7 * CO2_pi^2 + 7.2e-4 * CO2_pi - 2.1e-4 * N2O_pi + 5.36) * log(2.0)
    # Calculate scaling term based on user-defined value for F2x.
    F2x_scale <<- F2x / F2x_etminan
  } else {
    # Set F2x_scale to 1.0 if no adjustment.
    F2x_scale <<- 1.0
  }
  
  # Calculate scaling term depending on whether or not CO₂ concentrations are between pre-industrial and maximum values for α.
  if (CO2_pi < CO2[t] && CO2[t] <= CO2_alpha_max) {
    # α when concentrations are between pre-industrial and maximum.
    CO2_alpha[t] <<- d1 + a1 * (CO2[t] - CO2_pi) ^ 2 + b1 * (CO2[t] - CO2_pi)
  } else if (CO2[t] <= CO2_pi) {
    # α when concentrations are less than pre-industrial.
    CO2_alpha[t] <<- d1
  } else {
    # α when concentrations are greater than maximum.
    CO2_alpha[t] <<- d1 - b1^2 / (4.0 * a1)
  }
  
  # Calculate α term for N₂O.
  N2O_alpha[t] <<- c1 * sqrt(N2O[t])
  
  # Calculate carbon dioxide radiative forcing (with potential F2x adjustment).
  rf_co2[t] <<- (CO2_alpha[t] + N2O_alpha[t]) * log(CO2[t] / CO2_pi) * F2x_scale
}

  
