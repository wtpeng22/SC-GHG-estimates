library(mvtnorm)
library(triangle)
# 
# """
#     get_mcs(trials; 
#         socioeconomics_source::Symbol = :RFF, 
#         mcs_years = 1750:2300, 
#         fair_parameter_set::Symbol = :random,
#         fair_parameter_set_ids::Union{Vector{Int}, Nothing} = nothing,
#         rffsp_sampling::Symbol = :random,
#         rffsp_sampling_ids::Union{Vector{Int}, Nothing} = nothing,
#         save_list::Vector = []
#     )
# 
# Return a Monte Carlo Simulation definition of type Mimi.SimulationDefinition that
# holds all random variables and distributions, as assigned to model component/parameter
# pairs, that will be used in a Monte Carlo Simulation. 
# 
# - `trials` (required) - number of trials to be run, used for presampling
# - `socioeconomics_source` (default :RFF) - which source the Socioeconomics component uses
# - `fair_parameter_set_ids` - (default nothing) - if `fair_parameter_set` is set 
# to :deterministic, this `n` element vector provides the fair parameter set ids 
# that will be run, otherwise it is set to `nothing` and ignored.
# - `rffsp_sampling` (default :random) - which sampling strategy to use for the RFF 
# SPs, :random means RFF SPs will be chosen randomly, while :deterministic means they 
# will be based on the provided vector of to `rffsp_sampling_ids` keyword argument. 
# - `rffsp_sampling_ids` - (default nothing) - if `rffsp_sampling` is set to :deterministic, 
# this `n` element vector provides the RFF SP ids that will be run, otherwise it is 
# set to `nothing` and ignored.
# - `save_list` (default []) - which parameters and varaibles to save for each trial,
# entered as a vector of Tuples (:component_name, :variable_name)
# """

fair_parameter_set = "deterministic"
rffsp_sampling = "deterministic"
brick_parameter_set = "deterministic"

# fair_parameter_set = "random"
# rffsp_sampling = "random"

BRICK_parameters <- read.csv(paste0(dirname(getwd()), "/data/BRICK_posterior_parameters_10k.csv"))
# Add Cromar uncertainty based on coefficients from Cromar et al.
cromar_coeffs <- read.csv("../data/CromarMortality_damages_coefficients.csv")
cromar_mapping_raw <- read.csv("../data/Mapping_countries_to_cromar_mortality_regions.csv")
# FAIR model parameters
output_dir = "../data/fair_model_output/MC_outputs_temp.csv"
get_mcs()

Monte_Carlo_sample <- function(){
  # add the socioeconomics RV if the socioeconomics source is Mimi RFF SPs
  # use SampleStore for a deterministic RFF SP sampling approach, otherwise
  # use an EmpiricalDistribution across all ids (equal probability is assumed 
  # if probabilities not provided)
  if (socioeconomics_source == "RFF") {
    if(rffsp_sampling == "random"){
      RFF_id <<- sample(seq(1,10000,1),1)
    }
  }

  ###random sample for the Agriculture damage sector
  highDF <<- gtap_df_all[, , 3]
  lowDF <<- gtap_df_all[, , 4]
  midDF <<- gtap_df_all[, , 5]
  
  ag_samples <<- rtriangle(1,0,1,0.5);
  
  gtap_df_sample <<- array(0, dim = c(16, 3))
  for (r in 1:16) {
    for (temp in 1:3) {
      x <- c(0, 0.5, 1)
      y <- c(lowDF[r, temp], midDF[r, temp], highDF[r, temp])
      gtap_df_sample[r, temp] <<- approx(x, y,ag_samples)$y
    }
  }
  gtap_df <<- gtap_df_sample

  # Get one random variable per region for mortality damage
  beta_mortality_sample <<- cromar_mapping_raw
  beta_mortality_sample$cromar_coeffs <<- NA;
  for (i in 1:nrow(cromar_coeffs)) {
    cromar_coeffs_sample <<- rnorm(1, mean = cromar_coeffs$Pooled.Beta[i], sd = cromar_coeffs$Pooled.SE[i])
    beta_mortality_sample[beta_mortality_sample$cromar_region == cromar_coeffs$Cromar.Region.Name[i],]$cromar_coeffs <<- cromar_coeffs_sample
  }
  beta_mortality <<- beta_mortality_sample$cromar_coeffs
  
  # Get one random variable per region for fair model
  sample_fair_idx <<- sample(1:2237, 1, replace = TRUE) 
}


Monte_Carlo_BRICK_model <- function(brick_sample){
  ###random sample for the brick model
  # brick_mcs_years <<- 1850:2300
  # lws_random_sample <<- rnorm(length(brick_mcs_years), mean = 0.0003, sd = 0.00018)
  # brick_sample <<- sample(seq(1,10000,1),1)
  BRICK_uncertain_parameters_sample <<- BRICK_parameters[brick_sample,]
  
  te_s0 <<- BRICK_uncertain_parameters_sample$thermal_s0
  te_alpha <<- BRICK_uncertain_parameters_sample$thermal_alpha
  
  gsic_v0 <<- BRICK_uncertain_parameters_sample$glaciers_v0
  gsic_s0 <<- BRICK_uncertain_parameters_sample$glaciers_s0
  gsic_beta0 <<- BRICK_uncertain_parameters_sample$glaciers_beta0
  gsic_n <<- BRICK_uncertain_parameters_sample$glaciers_n
  
  greenland_v0 <<- BRICK_uncertain_parameters_sample$greenland_v0
  greenland_a <<- BRICK_uncertain_parameters_sample$greenland_a
  greenland_b <<- BRICK_uncertain_parameters_sample$greenland_b
  greenland_alpha <<- BRICK_uncertain_parameters_sample$greenland_alpha
  greenland_beta <<- BRICK_uncertain_parameters_sample$greenland_beta
  
  anto_alpha <<- BRICK_uncertain_parameters_sample$anto_alpha
  anto_beta <<- BRICK_uncertain_parameters_sample$anto_beta
  
  ais_gamma <<- BRICK_uncertain_parameters_sample$ais_gamma
  ais_alpha <<- BRICK_uncertain_parameters_sample$ais_alpha
  ais_mu <<- BRICK_uncertain_parameters_sample$ais_mu
  ais_nu <<- BRICK_uncertain_parameters_sample$ais_v
  ais_precipitation0 <<- BRICK_uncertain_parameters_sample$ais_precip0
  ais_kappa <<- BRICK_uncertain_parameters_sample$ais_kappa
  ais_iceflow0 <<- BRICK_uncertain_parameters_sample$ais_flow0
  ais_runoffline_snowheight0 <<- BRICK_uncertain_parameters_sample$ais_runoff_height0
  ais_c <<- BRICK_uncertain_parameters_sample$ais_c
  ais_bedheight0 <<- BRICK_uncertain_parameters_sample$ais_bedheight0
  ais_slope <<- BRICK_uncertain_parameters_sample$ais_slope
  lambda <<- BRICK_uncertain_parameters_sample$ais_lambda
  temperature_threshold <<- BRICK_uncertain_parameters_sample$ais_temp_threshold
  ais_sea_level0 <<- BRICK_uncertain_parameters_sample$antarctic_s0
  
}



  
