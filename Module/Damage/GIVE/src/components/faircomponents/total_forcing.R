# #--------------------------
# # Total raditiave forcing
# #--------------------------
# 
# other_ghg <- Index() # Index for other well-mixed greenhouse gases.
# ozone_depleting_substances <- Index() # Index for ozone-depleting substances.

scale_CO2 <- NULL # Carbon dioxide scaling factor to capture effective radiative forcing uncertainty.
scale_CH4 <- NULL # Methane scaling factor to capture effective radiative forcing uncertainty.
scale_CH4_H2O <- NULL # Stratospheric water vapor from methane oxidation scaling factor to capture effective radiative forcing uncertainty.
scale_N2O <- NULL # Nitrous oxide scaling factor to capture effective radiative forcing uncertainty.
scale_O3 <- NULL # Ozone scaling factor to capture effective radiative forcing uncertainty.
#scale_aerosol_direct <- NULL # Direct aerosol effect scaling factor to capture effective radiative forcing uncertainty.
scale_aerosol_indirect <- NULL # Indirect aerosol effect scaling factor to capture effective radiative forcing uncertainty.
scale_bcsnow <- NULL # Black carbon on snow scaling factor to capture effective radiative forcing uncertainty.
scale_landuse <- NULL # Land-use surface albedo change scaling factor to capture effective radiative forcing uncertainty.
scale_contrails <- NULL # Contrails scaling factor to capture effective radiative forcing uncertainty.
scale_volcanic <- NULL # Volcanic scaling factor to capture effective radiative forcing uncertainty.
scale_solar <- NULL # Solar scaling factor to capture effective radiative forcing uncertainty.

scale_aerosol_direct_SOx <- NULL # Direct aerosol effect (SOx contribution) scaling factor to capture effective radiative forcing uncertainty.
scale_aerosol_direct_CO_NMVOC <- NULL # Direct aerosol effect (CO & NMVOC contribution) scaling factor to capture effective radiative forcing uncertainty.
#scale_aerosol_direct_NMVOC <- NULL # Direct aerosol effect (NMVOC contribution) scaling factor to capture effective radiative forcing uncertainty.
scale_aerosol_direct_NOx_NH3 <- NULL # Direct aerosol effect (NOx & NH3 contribution) scaling factor to capture effective radiative forcing uncertainty.
scale_aerosol_direct_BC <- NULL # Direct aerosol effect (BC contribution) scaling factor to capture effective radiative forcing uncertainty.
scale_aerosol_direct_OC <- NULL # Direct aerosol effect (OC contribution) scaling factor to capture effective radiative forcing uncertainty.
#scale_aerosol_direct_NH3 <- NULL # Direct aerosol effect (NH3 contribution) scaling factor to capture effective radiative forcing uncertainty.
scale_other_ghg <- matrix(nrow = length(other_ghg),ncol = 1) # Scaling factor to capture effective radiative forcing uncertainty for other well-mixed greenhouse gases.
scale_ods <-matrix(nrow = length(ozone_depleting_substances),ncol = 1) # Scaling factor to capture effective radiative forcing uncertainty for ozone-depeleting substances.

rf_co2 <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from carbon dioxide (Wm⁻²).
rf_ch4 <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from methane (Wm⁻²).
rf_ch4_h2o <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from stratospheric water varpor from methane oxidiation (Wm⁻²).
rf_n2o <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from nitrous oxide (Wm⁻²).
total_forcing_O3 <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from ozone (Wm⁻²).

F_SOx_aero <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from direct aerosol effect (SOx contribution) (W m⁻²).
F_CO_aero <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from direct aerosol effect (CO contribution) (W m⁻²).
F_NMVOC_aero <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from direct aerosol effect (NMVOC contribution) (W m⁻²).
F_NOx_aero <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from direct aerosol effect (NOx contribution) (W m⁻²).
F_BC_aero <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from direct aerosol effect (BC contribution) (W m⁻²).
F_OC_aero <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from direct aerosol effect (OC contribution) (W m⁻²).
F_NH3_aero <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from direct aerosol effect (NH3 contribution) (W m⁻²).

rf_aero_indirect <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from indirect aerosol effect (Wm⁻²).
forcing_BC_snow <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from black carbon on snow (Wm⁻²).
forcing_landuse <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from land-use surface albedo change (Wm⁻²).
forcing_contrails <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from contrails (Wm⁻²).
other_ghg_rf <- matrix(nrow = length(time),ncol = length(other_ghg)) # Radiative forcing from other well-mixed greenhouse gases (Wm⁻²).
ods_rf <- matrix(nrow = length(time),ncol = length(ozone_depleting_substances))  # Radiative forcing from ozone_depleting substances (Wm⁻²).

F_volcanic <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from volcanic eruptions (Wm⁻²).
F_solar <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from solar irradience (Wm⁻²).
F_exogenous <- matrix(nrow = length(time),ncol = 1) # Radiative forcing from exogenous sources not included in this module (Wm⁻²).

forcing <- matrix(nrow = length(time),ncol = 1) # Total radiative forcing, with individual components scaled by their respective efficacy (Wm⁻²).

total_forcing_run_timestep <- function(t) {
  
  # Calculate total radiative forcing as the sum of individual radiative forcings (with potential scaling factor).
  forcing[t] <<- rf_co2[t] * scale_CO2 +
    rf_ch4[t] * scale_CH4 +
    rf_ch4_h2o[t] * scale_CH4_H2O +
    rf_n2o[t] * scale_N2O +
    sum(other_ghg_rf[t,] * scale_other_ghg) +
    sum(ods_rf[t,] * scale_ods) +
    total_forcing_O3[t] * scale_O3 +
    F_SOx_aero[t] * scale_aerosol_direct_SOx +
    (F_CO_aero[t] + F_NMVOC_aero[t]) * scale_aerosol_direct_CO_NMVOC +
    (F_NOx_aero[t] + F_NH3_aero[t]) * scale_aerosol_direct_NOx_NH3 +
    F_BC_aero[t] * scale_aerosol_direct_BC +
    F_OC_aero[t] * scale_aerosol_direct_OC +
    rf_aero_indirect[t] * scale_aerosol_indirect +
    forcing_BC_snow[t] * scale_bcsnow +
    forcing_landuse[t] * scale_landuse +
    forcing_contrails[t] * scale_contrails +
    F_solar[t] * scale_solar +
    F_volcanic[t] * scale_volcanic +
    F_exogenous[t]
}







