# -----------------------------------------------------------
# Global Surface Temperature Change
# -----------------------------------------------------------

# This follows the forcing to temperature function described in Geoffroy et al. (2013a, 2013b) and implemented in FAIR v1.6.2.

earth_radius <- NULL # Mean radius of the Earth (m).
seconds_per_year <- NULL # Number of seconds in a year (s yr⁻¹).
ocean_heat_exchange <- NULL # Heat exchange coefficient between the two ocean layers (W m⁻²K⁻¹).
deep_ocean_efficacy <- NULL # Deep ocean efficacy parameter (unitless).
dt <- NULL # Length of timestep (years).
lambda_global <- NULL # Climate feedback parameter (convention is positive = stable) (W m⁻²K⁻¹).
T_mix0 <- matrix(nrow = 2,ncol = 1) # Initial condition for slow and fast contributions to temperature in mixed layer (K).
T_deep0 <- matrix(nrow = 2,ncol = 1) # Initial condition for slow and fast contributions to temperature in deep ocean (K).
ocean_heat_capacity <- matrix(nrow = 2,ncol = 1) # Heat capacities for mixed layer and deep ocean (W m⁻²yr⁻¹).
forcing <- matrix(nrow = length(time),ncol = 1) # Total effective radiative forcing (W m⁻²).

T_mix <- matrix(nrow = length(time),ncol = 2) # Slow and fast contributions to temperature in mixed layer (K).
T_deep <- matrix(nrow = length(time),ncol = 2) # Slow and fast contributions to temperature in deep ocean (K).
c_dtemp <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
heatflux <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
del_ohc <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
factor_lambda_eff <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
lambda <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
lambda_eff <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
ratio <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
int_f <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
int_s <- matrix(nrow = length(time),ncol = 1) # TODO: Fill in description with units.
temperature <- matrix(nrow = length(time),ncol = 1) # Global surface temperature anaomaly (K).

# Intermediate terms that depend on the sampled ocean heat parameters, but otherwise remain constant in all periods.
ntoa_joule <- NULL # TODO: Fill in description with units.
cdeep_p <- NULL # TODO: Fill in description with units.
gamma_p <- NULL # TODO: Fill in description with units.
g1 <- NULL # TODO: Fill in description with units.
g2 <- NULL # TODO: Fill in description with units.
g <- NULL # TODO: Fill in description with units.
gstar <- NULL # TODO: Fill in description with units.
delsqrt <- NULL # TODO: Fill in description with units.
afast <- NULL # TODO: Fill in description with units.
aslow <- NULL # TODO: Fill in description with units.
cc <- NULL # TODO: Fill in description with units.
amix_f <- NULL # TODO: Fill in description with units.
amix_s <- NULL # TODO: Fill in description with units.
adeep_f <- NULL # TODO: Fill in description with units.
adeep_s <- NULL # TODO: Fill in description with units.
adf <- NULL # TODO: Fill in description with units.
ads <- NULL # TODO: Fill in description with units.
exp_f = NULL # TODO: Fill in description with units.
exp_s = NULL # TODO: Fill in description with units.

temperature_run_timestep <- function(t) {
  
  ntoa_joule <<- 4 * pi * earth_radius^2 * seconds_per_year
  cdeep_p <<- ocean_heat_capacity[2] * deep_ocean_efficacy
  gamma_p <<- ocean_heat_exchange * deep_ocean_efficacy
  g1 <<- (lambda_global + gamma_p) / ocean_heat_capacity[1]
  g2 <<- gamma_p / cdeep_p
  g <<- g1 + g2
  gstar <<- g1 - g2
  delsqrt <<- sqrt(g * g - 4 * g2 * lambda_global / ocean_heat_capacity[1])
  afast <<- (g + delsqrt) / 2
  aslow <<- (g - delsqrt) / 2
  cc <<- 0.5 / (ocean_heat_capacity[1] * delsqrt)
  amix_f <<- cc * (gstar + delsqrt)
  amix_s <<- -cc * (gstar - delsqrt)
  adeep_f <<- -gamma_p / (ocean_heat_capacity[1] * cdeep_p * delsqrt)
  adeep_s <<- -adeep_f
  adf <<- 1 / (afast * dt)
  ads <<- 1 / (aslow * dt)
  exp_f <<- exp(-1.0 / adf)
  exp_s <<- exp(-1.0 / ads)
  
  if (t==1) {
    # Set initial conditions for slow and fast contributions to temperature in mixed layer and deep ocean.
    T_mix[t,]  <<- T_mix0
    T_deep[t,] <<- T_deep0
    temperature[t] <<- sum(T_mix[t,])
    
  } else {
    
    # Calculate intermediate values for fast and slow contributions to temperature.
    int_f[t] <<- (forcing[t-1]*adf + forcing[t]*(1-adf) - exp_f*(forcing[t-1]*(1+adf)-forcing[t]*adf))/afast
    int_s[t] <<- (forcing[t-1]*ads + forcing[t]*(1-ads) - exp_s*(forcing[t-1]*(1+ads)-forcing[t]*ads))/aslow
    
    # Calculate slow and fast contributions to temperature in mixed layer.
    T_mix[t,1] <<- exp_f * T_mix[t-1,1] + amix_f*int_f[t]
    T_mix[t,2] <<- exp_s*T_mix[t-1,2] + amix_s*int_s[t]
    
    # Calculate slow and fast contributions to temperature in deep ocean.
    T_deep[t,1] <<- exp_f*T_deep[t-1,1] + adeep_f*int_f[t]
    T_deep[t,2] <<- exp_s*T_deep[t-1,2] + adeep_s*int_s[t]
    
    # Calculate global surface temperature anomaly.
    temperature[t] <<- sum(T_mix[t,])
    
    # Calculate change in ocean heat content.
    c_dtemp[t] <<- ocean_heat_capacity[1]*(sum(T_mix[t,])-sum(T_mix[t-1,])) + ocean_heat_capacity[2]*(sum(T_deep[t,])-sum(T_deep[t-1,]))
    
    # Calculate heat flux.
    heatflux[t] <<- c_dtemp[t]/dt
    
    # Calculate change in ocean heat content in joules.
    del_ohc[t] <<- ntoa_joule * c_dtemp[t]
    
    # Calculate effective ocean heat uptake efficiency.
    factor_lambda_eff[t] <<- (deep_ocean_efficacy-1.0)*ocean_heat_exchange
    
    # Calculate effective climate sensitivity.
    if (abs(sum(T_mix[t,])) > 1e-6) {
      ratio[t] <<- (sum(T_mix[t,]) - sum(T_deep[t,])) / sum(T_mix[t,])
      lambda_eff[t] <<- lambda_global + factor_lambda_eff[t]*ratio[t]
    } else {
      lambda_eff[t] <<- lambda_global + factor_lambda_eff[t]
    }
    
  }
  
}

