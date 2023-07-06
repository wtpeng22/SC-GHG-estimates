# --------------------------------------------------------------------------
#   Model Parameters
# load the list of countries to use for CIAM, these are CIAM countries intersected
# with available CIAM components

ciam_countries <- read.csv("../data/Dimension_ciam_countries.csv")$CountryISO
# countries <- as.vector(country)
segments <- as.vector(segment_fingerprints$segments)
  
# time parameters
tstep <- 10 # this is assumed within the slrcost component -- DO NOT CHANGE
period_length <- 50
ciam_start_year <- 2020
ciam_end_year <- 2300
ciam_times <- seq(from = ciam_start_year, to = ciam_end_year, by = tstep)
adaptPers <- as.integer(match(unique(c(ciam_start_year, seq(ciam_start_year, ciam_end_year, by = period_length), ciam_end_year)), ciam_times))

# --------------------------------------------------------------------------
# #   Model Construction
# ciam_time = length(ciam_times)
# adaptPers = length(adaptPers)

# # Add CIAM components
# --------------------------------------------------------------------------
#   The Rest of the Parameters
ciam_params <- get_ciam_params(first = ciam_start_year, tstep = tstep, last = ciam_end_year,
                                adaptation_firsts = adaptPers, ciam_countries = ciam_countries,
                                xsc_params_path = "../data/CIAM/xsc_ciam_countries.csv")

rgns <- ciam_params[[1]]
segs <- ciam_params[[2]]

# # Check Dimensions
# if (!all(dim_keys(m, "ciam_country") == rgns)) {
#   stop("The countries in xsc key need to match the segments in m_give.")
# }
# if (!all(dim_keys(m, "segments") == segs)) {
#   stop("The segments in xsc key need to match the segments in m_give.")
# }
# 
# for (k in names(ciam_params)) {
#   # these are parameters we don't need to set, the correct one for the run
#   # is held in "surgeexposure"
#   if (!(k %in% c("surgeexposure_dc-gtsr", "surgeexposure_gtsr"))) {
#     update_param(m, "slrcost", k, ciam_params[[k]])
#   }
# }

slr = matrix(0, nrow = length(ciam_times), ncol = length(segments))
pop = matrix(0, nrow = length(ciam_times), ncol = length(ciam_countries))
ypcc = matrix(0, nrow = length(ciam_times), ncol = length(ciam_countries))
vsl_ciam_country = matrix(0, nrow = length(ciam_times), ncol = length(ciam_countries))

update_ciam <- function(segment_fingerprints) {
  # time parameters
  tstep <- 10 # this is assumed within the slrcost component -- DO NOT CHANGE
  ciam_start_year <<- 2020
  ciam_end_year <<- 2300
  ciam_times <<- seq(ciam_start_year, ciam_end_year, by = tstep)
  
  normalization_year <<- 2000 # normalize sea level rise to 2000
  
  # --------------------------------------------------------------------------
  # Parameters from GIVE Model m_give
  
  # Indices from GIVE Model m_give to slrcost
  m_give_years <<- brick_first:model_last
  time_idxs <<- match(ciam_times, m_give_years)
  idx_2000 <<- match(normalization_year, m_give_years)
  country_idxs <<- match(ciam_countries, countries)

  # Downscale GMSL to LSL for CIAM segemnts
  lslr <<- matrix(0, nrow = length(ciam_times), ncol = length(segments))
  for (i in 1:length(ciam_times)) {
    lslr[i,] <<- segment_fingerprints$fpGSIC_loc * (gsic_sea_level[time_idxs[i]] - gsic_sea_level[idx_2000]) +
      segment_fingerprints$fpGIS_loc * (greenland_sea_level[time_idxs[i]] - greenland_sea_level[idx_2000]) +
      segment_fingerprints$fpAIS_loc * (ais_sea_level[time_idxs[i]] - ais_sea_level[idx_2000]) +
      segment_fingerprints$fpTE_loc * (te_sea_level[time_idxs[i]] - te_sea_level[idx_2000]) +
      segment_fingerprints$fpLWS_loc * (lws_sea_level[time_idxs[i]] - lws_sea_level[idx_2000])
  }
  
  # slrcost_lslr <- lslr # local sea level rise in meters
  
  # Socioeconomics
  #   (1) select the ciam countries from the full set of countries
  #   (2) convert GDP and Population into Per Capita GDP (2010\$ USD per year)
  #   per capita) for the slrcost component
  #   (3) get the VSL for each CIAM country
  
  time_idxs_socio <<- match(ciam_times, damages_first:model_last)
  slr_pop <<- population[time_idxs_socio,country_idxs] # millions
  slr_gdp <<- gdp[time_idxs_socio,country_idxs] * 1/pricelevel_2010_to_2005 # billion US $2005/yr -> billion US $2010/yr
  slr_ypcc <<- slr_gdp / slr_pop * 1000 # USD $2010/yr/person

  # Calculate the VSL
  # alpha <- VSL$alpha   # VSL scaling parameter
  # epsilon <- VSL$epsilon   # Income elasticity of the value of a statistical life
  # y_0 <- VSL$y0  # Normalization constant
    
  # Mirror the VSL component which follows the FUND mortality equation: v.vsl[t,c] = p.α * (p.pc_gdp[t,c] / p.y₀) ^ p.ϵ
  # Component expects vsl in millions of US $2010 dollars
  vsl_ciam_country <<- (alpha * (slr_ypcc / y0) ^ epsilon) / 1e6
  # m <- update_param(m, "slrcost", "vsl_ciam_country", vsl_ciam_country)
}

compute_PerfectForesight_OptimalCosts_typestable <- function(protect_cost, retreat_cost, no_adapt_cost, ntsteps, nsegments) {
  
  # These are the decision options, each is a permutation of choice and level,
  # that we will allow. Note we ignore ProtectCost0 and RetreatCost0, with idxs 
  # 4 and 5 respectively, because we use allowMaintain = false.
  decision_options <- list(
    list(label = "ProtectCost10", choice = "ProtectCost", level = 10, idx = 1),
    list(label = "ProtectCost100", choice = "ProtectCost", level = 100, idx = 2),
    list(label = "ProtectCost1000", choice = "ProtectCost", level = 1000, idx = 3),
    list(label = "ProtectCost10000", choice = "ProtectCost", level = 10000, idx = 4),
    list(label = "RetreatCost1", choice = "RetreatCost", level = 1, idx = 1),
    list(label = "RetreatCost10", choice = "RetreatCost", level = 10, idx = 2),
    list(label = "RetreatCost100", choice = "RetreatCost", level = 100, idx = 3),
    list(label = "RetreatCost1000", choice = "RetreatCost", level = 1000, idx = 4),
    list(label = "RetreatCost10000", choice = "RetreatCost", level = 10000, idx = 5),
    list(label = "NoAdaptCost0", choice = "NoAdaptCost", level = 1, idx = 1)
  )
  
  noptions <- length(decision_options)
  
  # this will hold the optimal costs for each segment after considering Perfect Foresight
  optimal_costs <- array(dim = c(ntsteps, nsegments))
  
  # Preallocate this array and reuse for each segment
  npv <- numeric(noptions)
  
  # Precompute discount factor
  df <- (1.04)^((1-1:ntsteps)*10)
  
  # loop over segments finding the optimal decision for each in light of perfect foresight
  # NPV and filling in the optimal costs with the undiscounted costs for that decision
  for (segment in 1:nsegments) {
    npv[1:4] <- sapply(1:4, function(level) sum(protect_cost[1:ntsteps, segment, level] * df * 10))
    npv[5:9] <- sapply(1:5, function(level) sum(retreat_cost[1:ntsteps, segment, level] * df * 10))
    npv[10] <- sum(no_adapt_cost[1:ntsteps, segment] * df * 10)
    
    optimal_decision <- decision_options[which.min(npv)][[1]];
    if (optimal_decision$choice == "ProtectCost") {
      optimal_costs[, segment] <- protect_cost[, segment, optimal_decision$idx]
    } else if (optimal_decision$choice == "RetreatCost") {
      optimal_costs[, segment] <- retreat_cost[, segment, optimal_decision$idx]
    } else if (optimal_decision$choice == "NoAdaptCost") {
      optimal_costs[, segment] <- no_adapt_cost[, segment]
    } else {
      stop("Unknown option.")
    }
  }
  return(optimal_costs)
}
  

# NPV foresight correction
# This correction accounts for the fact that the new version of CIAM considers NPV 
# over the current adaptation period (50 years), whereas the previous 
# GAMS version assumes NPV is known across the entire model time horizon (2000-2100, 
# for example).

compute_PerfectForesight_OptimalCosts <- function() {
    ntsteps <<- length(ciam_times)
    nsegments <<- length(segments)
    
    ciam_optimal_costs <<- compute_PerfectForesight_OptimalCosts_typestable(ProtectCost,RetreatCost,NoAdaptCost,
      ntsteps,
      nsegments)
    
    return(ciam_optimal_costs)
}
  

CIAM_foresight = "perfect"  ###default setting
# - CIAM_GDPcap (default is false) - Limit SLR damages to country-level annual GDP
CIAM_GDPcap = FALSE

compute_ciam_aggregate_damages <- function() {
  if(CIAM_foresight == "perfect"){
    OptimalCost_base = ciam_optimal_costs
  }else if(CIAM_foresight == "limited"){
    OptimalCost_base = OptimalCost
  }else{
    stop("CIAM_foresight must be either :limited or :perfect.")
  }
  # Aggregate to Country-Level Damages
  
  # Obtain a key mapping segment ids to ciam country ids, both of which
  # line up with the orders of dim_keys of ciam_base
  ciam_country_mapping <- data.frame(segment_id = segments, ciam_country_id = unlist(lapply(xsc, function(x) x[1])))
  
  num_ciam_countries <- length(ciam_countries)
  damages_years = ciam_start_year:ciam_end_year
  damages_idxs = match(ciam_times,damages_years)
  
  OptimalCost_country <<- matrix(NA, nrow = length(damages_years), ncol = num_ciam_countries)
  CIAM_damage_country <<- matrix(0, nrow = length(damages_years), ncol = length(countries))
  
  for (country in 1:num_ciam_countries) { # 145 consecutive Region IDs mapping to the 145 countries in ciam_base dimension ciam_country
    
    matching_segment_ids <- which(ciam_country_mapping$ciam_country_id == country) # rows of the mapping DataFrame that have this ciam country
    # matching_segment_ids <- ciam_country_mapping$segment_id[rows] # the actual segment IDs that map to this ciam country
    
    base_damages <- rowSums(as.matrix(OptimalCost_base[, matching_segment_ids]))
    OptimalCost_country[, country] <<- c(rep(base_damages[1:(length(base_damages)-1)], each=10), base_damages[length(base_damages)]) # repeat to annual from decadal
   }
  
  # Limit Country-Level Sea Level Rise Damages to Country-Level GDP
  if (CIAM_GDPcap) {
    # Obtain annual country-level GDP, select 2020:2300 and CIAM countries, convert from $2005 to $2010 to match CIAM
    ciam_gdp <- gdp[, match(ciam_countries, countries)] * 1 / pricelevel_2010_to_2005
    base_lim_cnt <- as.integer(OptimalCost_country > ciam_gdp)
    OptimalCost_country <<- pmin(OptimalCost_country, ciam_gdp)
  } else {
    base_lim_cnt <- matrix(0, nrow=length(damages_years), ncol=num_ciam_countries)
  }
  
  # global
  global_damages_base <<- apply(OptimalCost_country, 1, sum) * pricelevel_2010_to_2005 # Unit of CIAM is billion USD $2010, convert to billion USD $2005
  Country_ciam_idx <<- match(ciam_countries,countries)
  CIAM_damage_country[,Country_ciam_idx] <<- OptimalCost_country * pricelevel_2010_to_2005 # Unit of CIAM is billion USD $2010, convert to billion USD $2005
  
  # CIAM starts in 2020 so pad with zeros at the beginning
  global_damages_base <<- c(rep(0, 2020 - damages_first), global_damages_base) # billion USD $2005
  CIAM_damage_country <<- rbind(matrix(0, nrow = (2020 - damages_first),ncol = length(countries)), CIAM_damage_country) # billion USD $2005
  
  return(global_damages_base) # billion USD $2005, 2100 is index 9 in 2020:10:2300, this is uncapped segment-level baseline damages in 2100
}
  