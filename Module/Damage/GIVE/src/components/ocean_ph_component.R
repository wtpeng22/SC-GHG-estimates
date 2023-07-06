# ---------------------------------------------------------------------------------------------------------------------------
# Globally Averaged Ocean pH from "Valuing Climate Damages: Updating Estimation of the Social Cost of Carbon Dioxide" (2017)
# ---------------------------------------------------------------------------------------------------------------------------
beta1 = -0.3671
beta2 = 10.2328
pH_0 = 8.123
atm_co2_conc = matrix(nrow = length(time),ncol = 1)
pH = matrix(nrow = length(time),ncol = 1)

ocean_pH_run_timestep <-function(t){
  # Note: Report states, "Globally averaged pCO2 of the surface ocean can be estimated from globally averaged CO2 concentration in the atmosphere with
  #       approximately 1 year lag." As a result, the equation here uses atmospheric CO₂ concentrations from period [t-1] to calculate pH for period [t].
  if (t==1){
  # Set initial condition for timestep 1.
  pH[t] <<- pH_0
  } else
    # Calculate ocean pH following Equation 7 in Appendix F of report.
    pH[t] <<- beta1 * log(atm_co2_conc[t-1]) + beta2
  end
}

