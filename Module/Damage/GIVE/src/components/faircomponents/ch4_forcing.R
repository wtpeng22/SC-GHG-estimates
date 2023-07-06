# ---------------------------------------------------------------
# Radiative forcing from methane
# ---------------------------------------------------------------

# Note: Modified Etminan relationship from Meinshausen et al 2019 https://gmd.copernicus.org/preprints/gmd-2019-222/gmd-2019-222.pdf (Table 3)
a3 <- NULL             # Methane forcing coefficient.
b3 <- NULL             # Methane forcing coefficient.
d3 <- NULL             # Methane forcing coefficient.
CH4_pi <- NULL             # Initial (pre-industrial) methane concentration (ppb).
#rf_scale_CH₄ = NULL             # Scaling factor to capture effective radiative forcing uncertainty.
h2o_from_ch4 <- NULL             # Radiative forcing scaling factor for water capor from methane.
N2O <- matrix(nrow = length(time),ncol = 1) # Atmospheric nitrous oxide concentration (ppb).
CH4 <- matrix(nrow = length(time),ncol = 1) # Atmospheric methane concentration (ppb).

##Variable
rf_ch4 <- matrix(nrow = length(time),ncol = 1)  # Forcing from atmospheric methane concentrations (W m⁻²).
rf_ch4_h2o <- matrix(nrow = length(time),ncol = 1)  # Forcing from stratospheric water vapor due to oxidation of CH₄ to HₔO (W m⁻²).

ch4_forcing_run_timestep <- function(t) {
  # Calculate methane radiative forcing.
  rf_ch4[t] <<- (a3 * sqrt(CH4[t]) + b3 * sqrt(N2O[t]) + d3) * (sqrt(CH4[t]) - sqrt(CH4_pi))
  
  # Calculate stratospheric water vapor forcing from oxidation of CH₄ to HₔO.
  rf_ch4_h2o[t] <<- rf_ch4[t] * h2o_from_ch4
}

