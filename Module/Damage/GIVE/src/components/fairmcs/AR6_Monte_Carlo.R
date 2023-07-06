# 
# """
#     run_mcs(;trials::Int64 = 2237, 
#         output_dir::Union{String, Nothing} = nothing, 
#         save_trials::Bool = false,
#         m::Mimi.Model = get_model())
# 
# Return the results of a Monte Carlo Simulation with the defined number of trials
# and save data into the `output_dir` folder, optionally also saving trials if 
# `save_trials` is set to `true.` If no model is provided, use the default model 
# returned by get_model().
# """

run_mcs <- function(trials = 2237, output_dir = NULL, save_trials = FALSE, m = get_model()) {
  
  if(trials < 2) {
    stop("Must run 'run_mcs' function with a 'trials' argument greater than 1 due to a Mimi specification about SampleStores. TO BE FIXED SOON!")
  }
  
  # Set up output directories
  if(is.null(output_dir)) {
    output_dir <- file.path("../../output/mcs/", paste0("MCS ", format(Sys.time(), "%Y-%m-%d %H-%M-%S"), " MC", trials))
  }
  if(!file.exists(file.path(output_dir, "results"))) {
    dir.create(file.path(output_dir, "results"), recursive = TRUE)
  }
  
  trials_output_filename <- if(save_trials) file.path(output_dir, "trials.csv") else NULL
  
  # Get an instance of the mcs
  mcs <- get_mcs()
  
  # Run Monte Carlo trials
  results <- run(mcs, m, trials, trials_output_filename = trials_output_filename, results_output_dir = file.path(output_dir, "results"))
  
  return(results)
}

# """
#     get_mcs()
# 
# Return a Monte Carlo Simulation definition of type Mimi.SimulationDefinition that
# holds all random variables and distributions, as assigned to model component/parameter
# pairs, that will be used in a Monte Carlo Simulation.
# """

get_mcs <- function() {
  # define the Monte Carlo Simulation
  # mcs <- @defsim begin
  # save(temperature.T, co2_cycle.co2)
  # end
  
  # add the FAIR random variables and transforms - note this could be done within
  # the @defsim macro but we can use the dictionary to make this less verbose
  
  # Note that if a parameter component is not included in add_transform, the 
  # parameters are shared model parameters, and each line will create ONE random 
  # variable and assign all component parameters connected to that shared model 
  # parameter to the value taken on by that random variable
  
  fair_samples_map <<- get_fair_mcs_params()
  fair_samples <<- fair_samples_map
  # fair_samples_left <- fair_samples # we know we've added everything when this is empty!
  # 
  # # add and assign all random variables for single dimensional parameter
  # for (i in names(fair_samples_left)) {
  #   v <- fair_samples_left[[i]]
  #   if (ncol(v) == 1) {
  #     rv_name <- paste0("rv_", i)
  #     mcs <- add_RV(mcs, rv_name, SampleStore(fair_samples[[i]][,1]))
  #     add_transform(mcs, i, "=", rv_name)
  #     fair_samples_left <- fair_samples_left[!(names(fair_samples_left) %in% i)]
  #   }
  # }
  # 
  # # assign one random variable per year with a unique distribution from fair_samples
  # # TODO handle dimensions - what years does F_solar include? Assuming 1750 onwards for 361 years
  # for (year in 1750:2100) {
  #   rv_name <- paste0("rv_F_solar_", year)
  #   add_RV(mcs, rv_name, SampleStore(fair_samples$F_solar[,as.character(year)]))
  #   add_transform(mcs, "F_solar", "=", rv_name, list(year))
  # }
  # 
  # fair_samples_left[["F_solar"]] <- NULL
  # 
  # # Radiative forcing scaling - one distribution per "other" greenhouse gas, and
  # # one per ods
  # 
  # for (gas in names(fair_samples[['scale_other_ghg']])) {
  #   rv_name <- paste0("rv_scale_other_ghg_", gas)
  #   add_RV(mcs, rv_name, SampleStore(fair_samples[['scale_other_ghg']][, gas]))
  #   add_transform(mcs, "scale_other_ghg", "=", rv_name, list(gas))
  # }
  # fair_samples_left <- fair_samples
  # fair_samples_left[['scale_other_ghg']] <- NULL
  # 
  # for (ods in names(fair_samples[['scale_ods']])) {
  #   rv_name <- paste0("rv_scale_ods_", ods)
  #   add_RV(mcs, rv_name, SampleStore(fair_samples[['scale_ods']][, ods]))
  #   add_transform(mcs, "scale_ods", "=", rv_name, list(ods))
  # }
  # fair_samples_left[['scale_ods']] <- NULL
  # 
  # # ocean_heat_capacity takes an anonymous dim of 2 (deep and mixed, should label 
  # # explicilty) - anonymouse dims are named with Ints 1 and 2
  # 
  # rv_name <- "rv_ocean_heat_capacity_1"
  # add_RV(mcs, rv_name, SampleStore(fair_samples[['ocean_heat_capacity']][, "1"]))
  # add_transform(mcs, "ocean_heat_capacity", "=", rv_name, list(1))
  # 
  # rv_name <- "rv_ocean_heat_capacity_2"
  # add_RV(mcs, rv_name, SampleStore(fair_samples[['ocean_heat_capacity']][, "2"]))
  # add_transform(mcs, "ocean_heat_capacity", "=", rv_name, list(2))
  # 
  # fair_samples_left[['ocean_heat_capacity']] <- NULL
  # 
  # # check if we've added all FAIR parameters
  # if (!is.null(names(fair_samples_left))) {
  #   stop(paste("The following FAIR mcs uncertain parameters has not been added to the simulation:", names(fair_samples_left)))
  # }
  # 
  # return(mcs)
}
# 
# """
#     get_fair_mcs_params()
# 
# Return the FAIR mcs parameters mapped from parameter name to string name, and a dictionary
# using the parameter names as keys and a DataFrame holding the values as a value.
# """
get_fair_mcs_params <- function() {
  names_map <- get_fair_mcs_params_map()
  params_dict <- list()
  
  for (i in seq_along(names_map)) {
    v <- names_map[[i]]
    params_dict[[v]] <- read.csv(paste("../data", "/fair_mcs_params","/fair_mcs_params_", v, ".csv",sep=""))
  }
  
  result_list <- params_dict
  return(result_list)
}

get_fair_mcs_params_map <- function() {
  return(list(
    beta_CO = "b_aero_CO",
    scale_CH4 = "scale_CH4",
    F_solar = "F_solar",
    psi_CH4 = "b_tro3_CH4",
    scale_N2O = "scale_N2O",
    CO2_pi = "C_pi",
    deep_ocean_efficacy = "deep_ocean_efficacy",
    scale_bcsnow = "scale_bcsnow",
    scale_aerosol_direct_OC = "scale_aerosol_direct_OC",
    b_SOx = "ghan_params_SOx",
    feedback = "ozone_feedback",
    scale_O3 = "scale_O3",
    b_POM = "ghan_params_b_POM",
    r0_co2 = "r0",
    beta_NH3 = "b_aero_NH3",
    lambda_global = "lambda_global",
    scale_landuse = "scale_landuse",
    scale_volcanic = "scale_volcanic",
    scale_aerosol_direct_SOx = "scale_aerosol_direct_SOx",
    beta_NOx = "b_aero_NOx",
    psi_N2O = "b_tro3_N2O",
    ocean_heat_capacity = "ocean_heat_capacity",
    beta_OC = "b_aero_OC",
    scale_solar = "scale_solar",
    rC_co2 = "rc",
    scale_aerosol_direct_BC = "scale_aerosol_direct_BC",
    scale_CH4_H2O = "scale_CH4_H2O",
    scale_aerosol_indirect = "scale_aerosol_indirect",
    scale_ods = "scale_ods",
    psi_CO = "b_tro3_CO",
    scale_aerosol_direct_NOx_NH3 = "scale_aerosol_direct_NOx_NH3",
    scale_other_ghg = "scale_other_ghg",
    psi_NMVOC = "b_tro3_NMVOC",
    F2x = "F2x",
    beta_SOx = "b_aero_SOx",
    beta_NMVOC = "b_aero_NMVOC",
    rT_co2 = "rt",
    beta_BC = "b_aero_BC",
    scale_CO2 = "scale_CO2",
    psi_ODS = "b_tro3_ODS",
    scale_aerosol_direct_CO_NMVOC = "scale_aerosol_direct_CO_NMVOC",
    psi_NOx = "b_tro3_NOx",
    ocean_heat_exchange = "ocean_heat_exchange",
    phi = "ghan_params_Pi"
  ))
}

