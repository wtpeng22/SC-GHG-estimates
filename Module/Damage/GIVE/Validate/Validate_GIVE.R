###GIVE model R version, provided by Tianpeng Wang, Tsinghua University

#######load base data
# """
# Get a GIVE Model with the given argument Settings
# 
# -- Socioeconomic -- 
# 
# - socioeconomics_source (default :RFF) - The options are :RFF, which uses data from 
#     the RFF socioeconomic projections, or :SSP, which uses data from one of the 
#     Shared Socioeconomic Pathways
# 
# - SSP_scenario (default to nothing) - This setting is used only if one is using 
#     the SSPs as the socioeconomics_source, and the current options are "SSP119", 
#     "SSP126", "SSP245", "SSP370", "SSP585", and this will be used as follows.
#     See the SSPs component here: https://github.com/anthofflab/MimiSSPs.jl for more information.
# 
#     (1) Select the population and GDP trajectories for 2020 through 2300, mapping
#         each RCMIP scenario to the SSP (SSP1, 2, 3, 5 respectively)
#     
#     (2) Choose the ar6 scenario for data from 1750 - 2019 and the RCMIP emissions 
#         scenario from the MimiSSPs component to pull Leach et al. RCMIP scenario
#         data for 2020 to 2300 for CO2, CH4, and N2O.
# 
#     (NOTE) that if the socioeconomics_source is :RFF this will not be consequential 
#         and ssp245 will be used for the ar6 data from 1750 - 2019 and trace gases 
#         from 2020 onwards, while emissions for CO2, CH4, and N2O will come from
#         the MimiRFFSPs component.
# 
# - RFFSPsample (default to nothing, which will pull the in MimiRFFSPs) - choose
#     the sample for which to run the RFF SP. See the RFFSPs component here: 
#     https://github.com/rffscghg/MimiRFFSPs.jl.
# 
# -- Agriculture -- 
# 
# - Agriculture_gtap (default midDF) - specify the `Agriculture_gtap_gtap` input parameter as one of 
#     `["AgMIP_AllDF", "AgMIP_NoNDF", "highDF", "lowDF", "midDF"]`, indicating which 
#     gtap damage function the component should use. 
#  - Agriculture_floor_on_damages (default true) - If `Agriculture_gtap_floor_on_damages` = true, then 
#     the agricultural damages (negative values of the `agcost` variable) in each 
#     timestep will not be allowed to exceed 100% of the size of the  agricultural 
#     sector in each region.
# - Agriculture_ceiling_on_benefits (default false) - If `Agriculture_gtap_ceiling_on_benefits` = true, 
#     then the agricultural benefits (positive values of the `agcost` variable) in 
#     each timestep will not be allowed to exceed 100% of the size of the agricultural 
#     sector in each region.
# 
# -- Other --
# 
# - vsl (default :epa) - specify the soruce of the value of statistical life (VSL) being used in the model
# 
# """

library(data.table)
library(dplyr)

setwd("/Users/tianpeng/Desktop/IAMs_Rversion/GIVE_Rversion/src/")


pricelevel_2010_to_2005 = 87.504 / 96.166
pricelevel_2005_to_2020 = 113.648 / 87.504
pricelevel_1995_to_2005 = 87.504 / 71.823
pricelevel_2006_to_2005 = 87.504 / 90.204
pricelevel_2011_to_2005 = 87.504 / 98.164

socioeconomics_source = "RFF";   ###RFF和SSP二选一
SSP_scenario = "SSP245";
RFFSPsample = NULL;
Agriculture_gtap = "midDF";
Agriculture_floor_on_damages = TRUE;
Agriculture_ceiling_on_benefits = FALSE;
vsl = "epa"

# --------------------------------------------------------------------------
# MODEL - Model Data and Settings
# --------------------------------------------------------------------------

# dimensions and countries/regions lists
countries <- read.csv(file.path("..", "data", "Dimension_countries.csv"), header = TRUE)$CountryISO
ciam_countries <- read.csv(file.path("..", "data", "Dimension_ciam_countries.csv"), header = TRUE)$CountryISO
fund_regions <- read.csv(file.path("..", "data", "Dimension_fund_regions.csv"), header = TRUE)$fund_region
gcam_regions <- read.csv(file.path("..", "data", "Dimension_gcam_energy_regions.csv"), header = TRUE)$gcam_energy_region # not currently a dimension in model
cromar_regions <- read.csv(file.path("..", "data", "Dimension_cromar_mortality_regions.csv"), header = TRUE)$cromar_mortality_region # not currently a dimension in model

# Create country-region (FUND derived) mapping for Agriculture damage function
ag_mapping <- read.csv(file.path("..", "data", "Mapping_countries_to_fund_regions.csv"), header = TRUE)
if (!all(ag_mapping$ISO3 == countries)) stop("FUND mapping file ISO3 column must match model countries vector exactly.")
if (!identical(sort(unique(ag_mapping$fundregion)), sort(fund_regions))) stop("FUND mapping file fund_regions column must match model fund_regions vector exactly (when both are sorted).")
ag_mapping <- ag_mapping$fundregion

# Create country-region mapping for GCAM energy damage function.
energy_mapping <- read.csv(file.path("..", "data", "Mapping_countries_to_gcam_energy_regions.csv"), header = TRUE)
if (!all(energy_mapping$ISO3 == countries)) stop("GCAM mapping file ISO3 column must match model countries vector exactly.")
if (!identical(sort(unique(energy_mapping$gcamregion)), sort(gcam_regions))) stop("GCAM mapping file gcam_regions column must match model gcam_regions vector exactly (when both are sorted).")
energy_mapping <- energy_mapping$gcamregion

# Create country-region mapping for Cromar et al. temperature-mortality damage function.
cromar_mapping <- read.csv(file.path("..", "data", "Mapping_countries_to_cromar_mortality_regions.csv"), header = TRUE)
if (!all(cromar_mapping$ISO3 == countries)) stop("Cromar mortality mapping file ISO3 column must match model countries vector exactly.")
if (!identical(sort(unique(cromar_mapping$cromar_region)), sort(cromar_regions))) stop("Cromar mortality mapping file cromar_regions column must match model cromar_regions vector exactly (when both are sorted).")
cromar_mapping <- cromar_mapping$cromar_region

# BRICK Fingerprinting
segment_fingerprints <- read.csv(file.path("../data/CIAM/segment_fingerprints.csv")) %>%
  filter(rgn %in% ciam_countries)

# get the ar6 forcing scenario to be used for the FAIR model and Mortality component
if (socioeconomics_source == "RFF") {
  ar6_scenario <- "ssp245" # use SSP245 emissions scenario as the basis for trace gases for RFF SP
} else if (socioeconomics_source == "SSP") {
  ar6_scenario <- tolower(SSP_scenario)
}else{
  print("No ar6 forcing scenario found")
}

# Baseline mortality use SSP2 as a proxy for SSP4 and
# SSP1 as a proxy for SSP5 per instructions from the literature (relayed
# by David Smith and Bryan Parthum)
mortality_SSP_map <- c("SSP1" = "SSP1", "SSP2" = "SSP2", "SSP3" = "SSP3", "SSP4" = "SSP2", "SSP5" = "SSP1")

# Grab the SSP name from the full scenario ie. SSP2 from SSP245
if (socioeconomics_source == "SSP") {
  SSP <- substr(SSP_scenario, 1, 4)
} else {
  SSP <- NULL
}

# --------------------------------------------------------------------------    
# MODEL CONSTRUCTION
# --------------------------------------------------------------------------    

# component first and lasts
model_first = 1750
brick_first = 1850
damages_first = 2020
model_last = 2300

# VSL
if (vsl == "fund") {
  alpha = 4.99252262888626e6 * pricelevel_1995_to_2005;
  y0 = 24962.6131444313 * pricelevel_1995_to_2005;
} else if (vsl == "epa") {
  alpha = 7.73514707e6;
  y0 = 48726.60
} else {
  stop(paste("Invalid vsl argument of", vsl))
}
epsilon = 1.0


# # define gdp as the gdp get from the PerCapitaGDP.R code
# m <- connect_param!(m, "VSL" => "pc_gdp", "PerCapitaGDP" => "pc_gdp")

# Mortality estimate

library(dplyr)
library(tidyr)

if (socioeconomics_source == "SSP") {
  mortality_data <- read.csv(file.path(dirname(getwd()), "data", "Mortality_cdr_spp_country_extensions_annual.csv")) %>%
    filter(year %in% damages_first:model_last & scenario == mortality_SSP_map[SSP]) %>%
    select(year, ISO, cdf) %>%
    arrange(ISO) %>%
    pivot_wider(names_from = ISO, values_from = cdf) %>%
    select(-year)
  
  # make sure the columns match the mortality countries
  if (all(names(mortality_data) == countries)) {
    # model_ssp_baseline_mortality_rate = rbind(matrix(rep(NaN, (length(model_first:damages_first-1) * ncol(mortality_data))), nrow=length(model_first:damages_first-1), ncol=ncol(mortality_data)), as.matrix(mortality_data))
    model_ssp_baseline_mortality_rate = rbind(as.matrix(mortality_data));
  } else {
    stop("Countries in mortality data must match model countries.")
  }
}


# --------------------------------------------------------------------------
# Cromar et al. Temperature-Mortality Damages
# --------------------------------------------------------------------------

# Assign Cromar et al. regional temperature mortality coefficients to appropriate countries.

# Load raw data.
cromar_coeffs <- read.csv(file.path("..", "data", "CromarMortality_damages_coefficients.csv"))
cromar_mapping_raw <- read.csv(file.path("..", "data", "Mapping_countries_to_cromar_mortality_regions.csv"))

# Initialize an array to store country-level coefficients
country_beta_mortality <- rep(0, length(cromar_mapping_raw$ISO3))

# Loop through the regions and assign regional coefficients to proper sets of countries.
for (r in 1:length(cromar_regions)) {
  # Find country indices for region "r"
  r_index <- which(cromar_mapping_raw$cromar_region == cromar_regions[r])
  # Find index for region "r" coefficient.
  beta_index <- which(cromar_coeffs$Cromar.Region.Name == cromar_regions[r])
  # Assign all countries in that region proper coefficient.
  country_beta_mortality[r_index] <- cromar_coeffs$Pooled.Beta[beta_index]
}

# Get indices to reorder Cromar countries mapped to countries dimension (could be correct oder already, this is a safety check)
cromar_indices <- match(countries, cromar_mapping_raw$ISO3)
country_beta_mortality <- country_beta_mortality[cromar_indices]
beta_mortality = country_beta_mortality;

# Energy
# --------------------------------------------------------------------------

# Assign GCAM regional energy damage coefficients to appropriate countries.

# Load raw data.
energy_coeffs <- read.csv(file.path("..", "data", "energy_damages_gcam_region_coefficients.csv"))
gcam_mapping_raw <- read.csv(file.path("..", "data", "Mapping_countries_to_gcam_energy_regions.csv"))

# Initialize an array to store country-level coefficients
country_beta_energy <- rep(0, length(gcam_mapping_raw$ISO3))

# Loop through the regions and assign regional coefficients to proper subset of countries.
for (r in 1:length(gcam_regions)) {
  # Find country indices for region "r"
  r_index <- which(gcam_mapping_raw$gcamregion == gcam_regions[r])
  # Find index for region "r" coefficient.
  beta_index <- which(energy_coeffs$gcam_region == gcam_regions[r])
  # Assign all countries in that region proper coefficient.
  country_beta_energy[r_index] <- energy_coeffs[beta_index, "coefficient"]
}

beta_energy = country_beta_energy


# --------------------------------------------------------------------------
# Agriculture
# --------------------------------------------------------------------------
# 
# if (fund_regions != MooreAg$fund_regions) {
#   stop("FUND regions for RFF Model do not match FUND regions for Agriculture.")
# }
# Handle in pop and gdp 1990 baseline values
model_fund_regions = fund_regions
model_ag_mapping_input_regions = countries
model_ag_mapping_output_regions = fund_regions
model_ag_mapping = ag_mapping

# Access which of the 5 possible DFs to use for the damage function
gtaps <- c("AgMIP_AllDF", "AgMIP_NoNDF", "highDF", "lowDF", "midDF")
gtap_idx <- match(Agriculture_gtap, gtaps)
floor_on_damages = Agriculture_floor_on_damages
ceiling_on_benefits = Agriculture_ceiling_on_benefits
fund_datadir <- file.path("../data/FUND params")
dice_datadir <- file.path("../data/DICE climate output")
agrish0 = as.numeric(read.csv(file.path(fund_datadir, "agrish0.csv"))[,1])

# --------------------------------------------------------------------------
# Component-Specific Parameters and Connections
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# BRICK
# --------------------------------------------------------------------------


# ----- Antarctic Ocean ----- #
anto_alpha = 0.28
anto_beta = 0.95

# ----- Antarctic Ice Sheet ----- #
ais_rho_ice = 917.0
ais_rho_seawater = 1030.0
ais_rho_rock = 4000.0
ais_sea_level0 = 0.0
ais_ocean_temperature0 = 0.72
ais_radius0 = 1.864e6
ais_bedheight0 = 781.0
ais_slope = 0.0006
ais_mu = 11.0
ais_runoffline_snowheight0 = 1400.0
ais_c = 100.0
ais_precipitation0 = 0.37
ais_kappa = 0.062
ais_nu = 0.0086
ais_iceflow0 = 1.2
ais_gamma = 2.9
ais_alpha = 0.23
ais_temperature_coefficient = 0.8365
ais_temperature_intercept = 15.42
ais_local_fingerprint = -1.18
ocean_surface_area = 3.619e14
temperature_threshold = -15.0
lambda = 0.0093
include_ais_DSL = TRUE

# ----- Glaciers & Small Ice Caps ----- #
gsic_beta0 = 0.0013
gsic_v0 = 0.376
gsic_s0 = -0.0138
gsic_n = 0.847
gsic_teq = -0.15

# ----- Greenland Ice Sheet ----- #
greenland_a = -1.37
greenland_b = 8.06
greenland_alpha = 0.0008
greenland_beta = 0.00009
greenland_v0 = 7.52

# ----- Thermal Expansion ----- #
te_A = 3.619e14
te_C = 3991.86795711963
te_rho = 1027.0
te_alpha = 0.16
te_s0 = 0.0
ocean_heat_mixed = rep(0, length(model_first:model_last))

# ----- Landwater Storage ----- #
lws0 = 0.0
first_projection_year = 2018
lws_random_sample = rep(0.0003, model_last-model_first+1)

# ----- Set Parameters With Common Values Across Components ----- #
slr_norm_range_start = 1900
slr_norm_range_end = 1900

#######Emissions pulse function
MarginalEmissionComponent<-function(Marginal_choice){
  E_co2 <<- E_co2
  fossil_emiss_CH4 <<- fossil_emiss_CH4
  fossil_emiss_N2O <<- fossil_emiss_N2O
  if(Marginal_choice == "baserun"){}
  if(Marginal_choice == "marginal_CO2"){
    E_co2[emissionperiod]<<- E_co2[emissionperiod]+1;
  }
  if(Marginal_choice == "marginal_CH4"){
    fossil_emiss_CH4[emissionperiod]<<- fossil_emiss_CH4[emissionperiod]+1;
  }
  if(Marginal_choice == "marginal_N2O"){
    fossil_emiss_N2O[emissionperiod]<<- fossil_emiss_N2O[emissionperiod]+1;
  }
}

######calculate baseline damage
timestep_climate <- model_first:model_last
timestep_socioeconomic <- damages_first:model_last
timestep_damage <- damages_first:model_last

source("./components/MimiFAIRv1_6_2.R")
source("./components/GlobalTempNorm.R")
source("./components/OceanHeatAccumulator.R")
source("./components/MimiBRICK.R")
source("./components/GlobalSLRNorm.R")
source("./components/ciamparam/ciam_params.R")
source("./components/ciamparam/ciam_function.R")
if (socioeconomics_source == "SSP") {
  source("./components/SSPs.R")
}else if(socioeconomics_source == "RFF"){
  source("./components/RFF.R")
}
source("./components/netconsumption.R")

source("./components/PerCapitaGDP.R")
source("./components/VSL.R")
source("./components/cromar_mortality_damages.R")
source("./components/energy_damages.R")
source("./components/Agriculturedataprepare.R")
gtap_df <- gtap_df_all[, , gtap_idx]
source("./components/Agriculture_RegionAggregatorSum.R")
source("./components/AgricultureComponent.R")

source("components/main_ciam.R")
source("components/slrcost.R")

####Run socio-economic scenario
Run_socioeconomic_model <- function(){
  if (socioeconomics_source == "SSP") {
    data1990 <<- read.csv(file.path("..", "data", "Benveniste_SSPs", "Agriculture_1990vals.csv")) %>%
      dplyr::filter(SSP %in% substr(SSP_scenario, 1, 4)) %>%
      dplyr::select(fund_region, pop, gdp)
    
    idxs <<- match(data1990$fund_region, fund_regions) # get the ordering of 1990 regions matched to fund regions in model
    # if (!is.na(which(is.na(idxs)))){
    #   stop("FUND regions for RFF Model do not match FUND regions for Agriculture 1990 values.")
    # }
    data1990 <<- data1990[idxs,] # reorder based on idxs
    pop90 <<- data1990$pop
    gdp90 <<- data1990$gdp
    
    SSPs_run_timestep();
    
    baseline_mortality_rate <<- model_ssp_baseline_mortality_rate;
  }else if (socioeconomics_source == "RFF"){
    RFF_init(RFF_id)
    source("./components/Agriculture_RegionAggregatorSum_NoTime.R")
    pop90 <<- Agsumnotime_pop_run_timestep(population1990)
    gdp90 <<- Agsumnotime_pop_run_timestep(gdp1990)
    
    baseline_mortality_rate <<- deathrate; ####from RFFSP database
  }else{
    stop("Please select right scenario: SSP or RFF")
  }
}


#####Run climate model
sample_fair_idx = 0;
Run_climate_model <- function(Marginal_choice){
  fair_get_model(start_year=model_first, end_year=model_last, ar6_scenario = ar6_scenario)
  FAIR_model_run(Marginal_choice,sample_fair_idx)
  temperature_base <<- temperature;
  temperature_allyears <<- temperature
  temperature <<- temperature_allyears[-(1:(damages_first-model_first))]
  global_temperature <<- temperature_allyears # Global temperature deviation (°C).
  TempNorm_1995to2005_start <<- 1995
  TempNorm_1995to2005_end <<- 2005
  
  for (t in 1:length(timestep_climate)) {
    GlobalTempNorm_run_timestep(TempNorm_1995to2005_start,TempNorm_1995to2005_end,t)
    OceanHeatAccumulator_run_timestep(t)
  }
  TempNorm_1995to2005 <<- global_temperature_norm[-(1:(damages_first-model_first))]
  Ag_temp <<- TempNorm_1995to2005
  
  fair_brick_temperature <<- temperature_allyears[-(1:(brick_first-model_first))]
  del_ohc_accum_brick <<- del_ohc_accum[-(1:(brick_first-model_first))]
  brick_get_model("RCP85",brick_first,model_last)
  if(brick_parameter_set == "random" & Marginal_choice == "baserun"){
    brick_mcs_years <<- 1850:2300
    lws_random_sample_MC <<- rnorm(length(brick_mcs_years), mean = 0.0003, sd = 0.00018)
    brick_sample <<- sample(seq(1,10000,1),1)
  }
  if(brick_parameter_set == "deterministic"){}else if(brick_parameter_set == "random"){
    Monte_Carlo_BRICK_model(brick_sample);
    lws_random_sample <<- lws_random_sample_MC}else{stop("Wrong brick mode!")}
  BRICK_model_run(brick_first:model_last)
  update_ciam(segment_fingerprints)
  timestep_brick <<- brick_first:model_last
  for (t in 1:length(timestep_brick)) {
    GlobalSLRNorm_run_timestep(t);
  }
  gslr_brick <<- global_slr_norm
}

#####Run damage model
Run_noslr_damage_model <- function(){
  for (t in 1:length(timestep_damage)) {
    pc_gd_run_timestep(t)
    Agsum_pop_run_timestep(t)
    Agsum_gdp_run_timestep(t)
    vsl_run_timestep(t)
    cromar_mortality_damages_run_timestep(t)
    energy_costs_run_timestep(t)
    Ag_run_timestep(t)
  }
}

Run_slr_damage_model <- function(){
  for (i in 1:length(ciam_times)) {
    print(i)
    slrcost_run_timestep(i)
  }
}

#########Run the model to estimate baseline climate damage
GIVE_model_run <- function(Marginal_choice){
  Run_socioeconomic_model()
  Run_climate_model(Marginal_choice)
  Run_noslr_damage_model()
  Run_slr_damage_model()
}


######Estimate climate damage with CO2 perturbations
# emissionperiod = 2020-1750+1
# system.time({
#   GIVE_model_run("marginal_CO2")
# });

# # discount modules
# discount_rates = list(
#   c(label = "1.5%", prtp = exp(9.149606e-05) - 1, eta = 1.016010e+00), (label = "2.0%", prtp = exp(0.001972641) - 1, eta = 1.244458999), (label = "2.5%", prtp = exp(0.004618784) - 1, eta = 1.421158088), (label = "3.0%", prtp = exp(0.007702711) - 1, eta = 1.567899391),
# )


#####Calculate social cost of carbon
discount_years = damages_first:model_last
diagnostic_GIVE_model_deterministic <- function(){
  emissionperiod <<- 2020-1750+1
  fair_parameter_set <<- "deterministic"
  brick_parameter_set <<- "deterministic"
  rffsp_sampling <<- "deterministic"
  GIVE_model_run("baserun")
  global_sector_damage_base <<- rowSums(energy_costs_dollar) + rowSums(agcost) + rowSums(mortality_costs)/10^9
  ciam_optimal_costs <<- compute_PerfectForesight_OptimalCosts()
  global_slr_damage_base <<- compute_ciam_aggregate_damages()
  global_total_damage_base <<- global_sector_damage_base + global_slr_damage_base
  GIVE_model_run("marginal_CO2")
  global_sector_damage_perturb <<- rowSums(energy_costs_dollar) + rowSums(agcost) + rowSums(mortality_costs)/10^9
  ciam_optimal_costs <<- compute_PerfectForesight_OptimalCosts()
  global_slr_damage_perturb <<- compute_ciam_aggregate_damages()
  global_total_damage_perturb <<- global_sector_damage_perturb + global_slr_damage_perturb
  netconsumption_run_timestep(global_total_damage_base)
  df_eta <<- 1.244458999
  df_prtp <<- exp(0.001972641) - 1
  df_SCC <- ((net_cpc / net_cpc[1]) ^ (0-df_eta)) * (1 / ((1 + df_prtp) ^ (discount_years - damages_first)))
  SCC_alldam <<- sum((global_total_damage_perturb - global_total_damage_base)*df_SCC*12/44)
  # netconsumption_run_timestep(global_sector_damage_base)
  # df_eta <<- 1.244458999
  # df_prtp <<- exp(0.001972641) - 1
  # df_SCC <- ((net_cpc / net_cpc[1]) ^ (0-df_eta)) * (1 / ((1 + df_prtp) ^ (discount_years - damages_first)))
  # SCC_noslrdam <<- sum((global_sector_damage_perturb - global_sector_damage_base)*df_SCC*12/44)
}


#####Monte Carlo model run
source("main_mcs.R")

diagnostic_GIVE_model_Parallel <- function(run_time){
  for (nid in run_time:run_time) {
    emissionperiod <<- 2020-1750+1
    fair_parameter_set <<- "random"
    rffsp_sampling <<- "random"
    brick_parameter_set <<- "random"
    Monte_Carlo_sample()
    GIVE_model_run("baserun")
    temperature_MonteCarlo_baserun <<- temperature_allyears
    global_sector_damage_base <<- rowSums(energy_costs_dollar) + rowSums(agcost) + rowSums(mortality_costs)/10^9
    ciam_optimal_costs <<- compute_PerfectForesight_OptimalCosts()
    global_slr_damage_base <<- compute_ciam_aggregate_damages()
    global_total_damage_base <<- global_sector_damage_base + global_slr_damage_base
    lslr_base <<- lslr;
    GIVE_model_run("marginal_CO2")
    lslr_perturb <<- lslr;
    global_sector_damage_perturb <<- rowSums(energy_costs_dollar) + rowSums(agcost) + rowSums(mortality_costs)/10^9
    ciam_optimal_costs <<- compute_PerfectForesight_OptimalCosts()
    global_slr_damage_perturb <<- compute_ciam_aggregate_damages()
    global_total_damage_perturb <<- global_sector_damage_perturb + global_slr_damage_perturb
    netconsumption_run_timestep(global_total_damage_base)
    df_eta <<- 1.244458999
    df_prtp <<- exp(0.001972641) - 1
    df_SCC <<- ((net_cpc / net_cpc[1]) ^ (0-df_eta)) * (1 / ((1 + df_prtp) ^ (discount_years - damages_first)))
    SCC_alldam <<- sum((global_total_damage_perturb - global_total_damage_base)*df_SCC*12/44)
    # netconsumption_run_timestep(global_sector_damage_base)
    # df_eta <<- 1.244458999
    # df_prtp <<- exp(0.001972641) - 1
    # df_SCC <- ((net_cpc / net_cpc[1]) ^ (0-df_eta)) * (1 / ((1 + df_prtp) ^ (discount_years - damages_first)))
    # SCC_noslrdam <<- sum((global_sector_damage_perturb - global_sector_damage_base)*df_SCC*12/44)
  }
  return(list(SCC_alldam))
}

####Run the GIVE model under the deterministic mode
system.time({
  diagnostic_GIVE_model_deterministic()
});
print(SCC_alldam)

write.csv(global_total_damage_base,"../Validate/results/Global_total_damage_base_R.csv",quote = F,row.names = F)


# ####Parallel Monte Carlo run
# library(parallel)
# simulation_no = 20
# 
# SCC_output = matrix(nrow = simulation_no,ncol = 1)
# 
# system.time({
#   res1.p = mclapply(1:simulation_no,diagnostic_GIVE_model_Parallel,mc.cores = 2);
# });
# 
# for (run_time in 1:simulation_no) {
#   SCC_output[run_time] = res1.p[[run_time]][[1]];
# }
# mean(SCC_output)
# 
# write.csv(SCC_output,"./results/SCC_output.csv",quote = F,row.names = F)
# 
# # plot(2020:2300,(global_total_damage_perturb - global_total_damage_base)*(1/1.02^(1:281-1))*12/44)
# # plot(2020:2300,(global_sector_damage_perturb - global_sector_damage_base)*(1/1.02^(1:281-1))*12/44)




