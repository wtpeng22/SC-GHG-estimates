# -----------------------------------------------------------
# Carbon Cycle
# -----------------------------------------------------------

CO2_0 <- NULL
CO2_pi <- NULL
emiss2conc_co2 <- NULL
r0_co2 <- NULL
rT_co2 <- NULL
rC_co2 <- NULL
tau_co2 <- matrix(nrow = 4,ncol = 1)  
a_co2 <- matrix(nrow = 4,ncol = 1)
R0_co2 <- matrix(nrow = 4,ncol = 1)
E_co2 <- matrix(nrow = length(time),ncol = 1)
temperature <- matrix(nrow = length(time),ncol = 1)
cumulative_emissions_CO2_0 <- NULL
airborne_emissions_CO2_0 <- NULL
iirf_h <- NULL
iIRF_max <- NULL
dt <- NULL

# Variable
g0_co2 <- NULL
g1_co2 <- NULL
alpha_co2 <- matrix(nrow = length(time),ncol = 1)
CO2 <- matrix(nrow = length(time),ncol = 1)
GA_co2 <- matrix(nrow = length(time),ncol = 1)
GU_co2 <- matrix(nrow = length(time),ncol = 1)
iIRFT100_co2 <- matrix(nrow = length(time),ncol = 1)
decay_rate_co2 <- matrix(nrow = length(time),ncol = 4)
R_co2 <- matrix(nrow = length(time),ncol = 4)
cumulative_emissions_CO2 <- matrix(nrow = length(time),ncol = 1)
airborne_emissions_CO2 <- matrix(nrow = length(time),ncol = 1)

co2_cycle_run_timestep <- function(t) {
  if (t==1) {
    # Set initial values.
   R_co2[t,] <<-R0_co2
   CO2[t] <<-CO2_0
   cumulative_emissions_CO2[t] <<-cumulative_emissions_CO2_0
   airborne_emissions_CO2[t] <<-airborne_emissions_CO2_0
    
    # Calculate initial burden above pre-industrial values.
   GA_co2[t] <<- sum(R_co2[t,])
    
    #Initialise simplified carbon cycle parameters
   g1_co2 <<- sum(a_co2 *tau_co2 * (1 - (1 +iirf_h /tau_co2) * exp(-iirf_h /tau_co2)))
   g0_co2 <<- 1 / sinh(sum(a_co2 *tau_co2 * (1 - exp(-iirf_h /tau_co2))) /g1_co2)
  } else {
    # Calculate iIRF100.
   iIRFT100_co2[t] <<- min(r0_co2 +rC_co2 * (cumulative_emissions_CO2[t-1] -airborne_emissions_CO2[t-1]) +rT_co2 *temperature[t-1],iIRF_max)
    
    # Add a check for negative iIRF100 values (assume they remain fixed at previous value if going negative).
    if (iIRFT100_co2[t] <= 0) {
     iIRFT100_co2[t] <<-iIRFT100_co2[t-1]
    }
    
    # Calculate iIRF100
    iIRFT100_co2[t] <<- min(r0_co2 +rC_co2 * (cumulative_emissions_CO2[t-1] - airborne_emissions_CO2[t-1]) +rT_co2 *temperature[t-1],iIRF_max)
    
    # Add a check for negative iIRF100 values (assume they remain fixed at previous value if going negative).
    if (iIRFT100_co2[t] <= 0.0) {
      iIRFT100_co2[t] <<-  iIRFT100_co2[t-1]
    }
    
    # Calculate state-dependent lifetime adjustment term based on iIRF100 value.
    alpha_co2[t] <<- g0_co2 * sinh(iIRFT100_co2[t] / g1_co2)
    
    # Calculate concentrations in each carbon reservoir.
    for (i in 1:4) {
      
      # Calculate decay rate for iᵗʰ reservoir.
      decay_rate_co2[t,i] <<-dt / (alpha_co2[t] *tau_co2[i])
      
      # Calculate amount of carbon in iᵗʰ reservoir.
      R_co2[t,i] <<-E_co2[t-1] /emiss2conc_co2 *a_co2[i] * alpha_co2[t] * (tau_co2[i]/dt) * (1.0 - exp(-decay_rate_co2[t,i])) + R_co2[t-1,i] * exp(-decay_rate_co2[t,i])
      
    }
    
    # Calcualte atmospheric burden above pre-industiral levels.
    GA_co2[t] <<- sum(R_co2[t,])
    
    # Calculate atmospheric CO2 concentration.
    CO2[t] <<-CO2_pi + (GA_co2[t-1] + GA_co2[t]) / 2.0
    
    # Calculate total emissions remaining in the atmosphere
    airborne_emissions_CO2[t] <<- GA_co2[t] *emiss2conc_co2
    
    # Calculate cumulative emissions.
    cumulative_emissions_CO2[t] <<- cumulative_emissions_CO2[t-1] +E_co2[t]
  }
}
    
