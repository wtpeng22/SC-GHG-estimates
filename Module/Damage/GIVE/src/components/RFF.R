
# (10/25/2021) BEA Table 1.1.9, line 1 GDP annual values as linked here: https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&select_all_years=0&nipa_table_list=13&series=a&first_year=2005&last_year=2020&scale=-99&categories=survey&thetable=
pricelevel_2011_to_2005 = 87.504/98.164


fill_socioeconomics <- function(source_Year, source_Country, source_Pop, source_GDP, population, gdp, country_lookup, RFF_start_year, RFF_end_year) {
  for (i in 1:length(source_Year)) {
    if (source_Year[i] >= RFF_start_year && source_Year[i] <= RFF_end_year) {
      year_index <<- source_Year[i] - RFF_start_year + 1
      country_index <<- country_lookup[[source_Country[i]]]
      
      population[year_index, country_index] <<- source_Pop[i] / 1e3 # convert thousands to millions
      gdp[year_index, country_index] <<- source_GDP[i] / 1e3 * pricelevel_2011_to_2005 # convert millions to billions; convert $2011 to $2005
    }
  }
}

fill_deathrates <- function(source_Year, source_ISO3, source_DeathRate, deathrate, country_lookup, RFF_start_year, RFF_end_year) {
  for (i in 1:length(source_Year)) {
    if (source_Year[i] >= RFF_start_year && source_Year[i] <= RFF_end_year) {
      year_index <<- source_Year[i] - RFF_start_year + 1
      country_index <<- country_lookup[[source_ISO3[i]]]
      deathrate[year_index, country_index] <<- source_DeathRate[i]
    }
  }
}

fill_emissions <- function(source, emissions_var, sample_id, RFF_start_year, RFF_end_year) {
  source_sample <- source[source$sample == sample_id,]
  for (row in 1:nrow(source_sample)) {
    source_target <- source_sample[row,]
    if (source_target$year >= RFF_start_year && source_target$year <= RFF_end_year) {
      year_index <- source_target$year - RFF_start_year + 1
      emissions_var[year_index] <- source_target$value
    }
  }
  return(emissions_var)
}

fill_population1990 <- function(source, population1990, country_lookup) {
  for (row in 1:nrow(source)) {
    source_target <- source[row,]
    country_index <- country_lookup[source_target$ISO3]
    population1990[country_index] <<- source_target$Population # millions
  }
}

fill_gdp1990 <- function(source, gdp1990, population1990, country_lookup, sample_id) {
  source_sample <- source[source$sample == sample_id,]
  for (row in 1:nrow(source_sample)) {
    source_target <- source_sample[row,]
    country_index <- country_lookup[source_target$country]
    gdp1990[country_index] <<- (source_target$value * population1990[country_index]) * pricelevel_2011_to_2005 / 1e3 # convert $2011 to $2005 and divide by 1e3 to get millions -> billions
  }
}

RFF_start_year = 2020
RFF_end_year = 2300
RFF_time = RFF_start_year:RFF_end_year
# country <- NULL
# country_names <- make_parm(index = country, type = "character") # need the names of the countries from the dimension

RFF_id <- 6546  # the sample (out of 10,000) to be used

population = matrix(nrow = length(RFF_time),ncol = length(countries))
population_global = matrix(nrow = length(RFF_time),ncol = 1)
deathrate = matrix(nrow = length(RFF_time),ncol = length(countries))
gdp = matrix(nrow = length(RFF_time),ncol = length(countries))
gdp_global = matrix(nrow = length(RFF_time),ncol = 1)
population1990 = matrix(nrow = length(countries),ncol = 1)
gdp1990 = matrix(nrow = length(countries),ncol = 1)

co2_emissions <-matrix(nrow = length(RFF_time),ncol = 1)
ch4_emissions <-matrix(nrow = length(RFF_time),ncol = 1)
n2o_emissions <-matrix(nrow = length(RFF_time),ncol = 1)

g_datasets <- list()

if (!("ch4" %in% names(g_datasets))) {
  g_datasets[["ch4"]] <- fread(file.path("../", "rffsps_v5", "emissions", "rffsp_ch4_emissions.csv"))
}
if (!("n2o" %in% names(g_datasets))) {
  g_datasets[["n2o"]] <- fread(file.path("../", "rffsps_v5", "emissions", "rffsp_n2o_emissions.csv"))
}
if (!("co2" %in% names(g_datasets))) {
  g_datasets[["co2"]] <- fread(file.path("../", "rffsps_v5", "emissions", "rffsp_co2_emissions.csv"))
}

RFF_init <- function(RFF_id) {
  
  # Add countries to a named vector where each country name is a key that holds 
  # its index in countries.
  country_lookup <- setNames(seq_along(countries), countries)
  country_indices <- countries
  
  # ----------------------------------------------------------------------
  # Socioeconomic Data
  #   population in millions of individuals
  #   GDP in billions of $2005 USD
  
  # Load Feather File
  rffsps_t <- arrow::read_feather(file.path("../","rffsps_v5", "pop_income", 
                                     paste0("rffsp_pop_income_run_", RFF_id, ".feather")))

  fill_socioeconomics(rffsps_t$Year, rffsps_t$Country, rffsps_t$Pop, rffsps_t$GDP, population, gdp, country_lookup, RFF_start_year, RFF_end_year)
  
  for (year in seq(RFF_start_year, RFF_end_year-5, by=5)) {
    for (country_idx in 1:length(country_indices)) {
      year_as_timestep <- year - RFF_start_year + 1
      pop_interpolator <- approx(c(year, year+5), c(log(population[year_as_timestep,country_idx]), log(population[year_as_timestep+5,country_idx])), xout=(year+1):(year+4))$y
      gdp_interpolator <- approx(c(year, year+5), c(log(gdp[year_as_timestep,country_idx]), log(gdp[year_as_timestep+5,country_idx])), xout=(year+1):(year+4))$y
      population[((year+1):(year+4)-RFF_start_year+1),country_idx] <<- exp(pop_interpolator)
      gdp[((year+1):(year+4)-RFF_start_year+1),country_idx] <<- exp(gdp_interpolator)
    }
  }
  
  # add global data for future accessibility and quality control
  gdp_global <<- rowSums(gdp)
  population_global <<- rowSums(population)

  # ----------------------------------------------------------------------
  # Death Rate Data
  #   crude death rate in deaths per 1000 persons

  # # key between population trajectory and death rates - each population
  # # trajectory is assigned to one of the 1000 death rates
  # if (!("pop_trajectory_key" %in% names(g_datasets))) {
  #   g_datasets[["pop_trajectory_key"]] <- read.csv(file.path(datadep, "rffsps_v5", "sample_numbers", "sampled_pop_trajectory_numbers.csv"))[["x"]]
  # }

  g_datasets[["pop_trajectory_key"]] <- read.csv(file.path("../", "rffsps_v5", "sample_numbers", "sampled_pop_trajectory_numbers.csv"))[["x"]]

  deathrate_trajectory_id <- as.integer(g_datasets[["pop_trajectory_key"]][RFF_id])

  # Load Feather File
  rffsps_death_t <- arrow::read_feather(file.path("../", "rffsps_v5", "death_rates", paste0("rffsp_death_rates_run_", deathrate_trajectory_id, ".feather")))
  fill_deathrates(rffsps_death_t$Year, rffsps_death_t$ISO3, rffsps_death_t$DeathRate, deathrate, country_lookup, RFF_start_year, RFF_end_year)

  # TODO could handle the repeating of years here instead of loading bigger files
  # ----------------------------------------------------------------------
  # Emissions Data
  #   carbon dioxide emissions in GtC
  #   nitrous oxide emissions in MtN2
  #   methane emissions in MtCH4
  # add data to the global dataset if it's not there


  # fill in the variables
  ch4_emissions <<- fill_emissions(g_datasets[["ch4"]], ch4_emissions, RFF_id, RFF_start_year, RFF_end_year)
  co2_emissions <<- fill_emissions(g_datasets[["co2"]], co2_emissions, RFF_id, RFF_start_year, RFF_end_year)
  n2o_emissions <<- fill_emissions(g_datasets[["n2o"]], n2o_emissions, RFF_id, RFF_start_year, RFF_end_year)

  # ----------------------------------------------------------------------
  # Population and GDP 1990 Values

  if (!("ypc1990" %in% names(g_datasets))) {
    g_datasets$ypc1990 <- fread(file.path("../", "rffsps_v5", "ypc1990", "rffsp_ypc1990.csv")) %>%
      mutate(sample = 1:10000) %>%
      gather(key = "country", value = "value", -sample) %>%
      # rename(sample = 1, country = 2, value = 3) %>%
      as.data.frame()
  }

  if (!("pop1990" %in% names(g_datasets))) {
    g_datasets$pop1990 <- read.csv(file.path("../data", "population1990.csv"))
  }

  fill_population1990(g_datasets$pop1990, population1990, country_lookup)
  fill_gdp1990(g_datasets$ypc1990, gdp1990, population1990, country_lookup, RFF_id)
}
   
RFF_run_timestep <- function(t) {
  if (!(t %in% RFF_start_year:RFF_end_year)) {
    stop(paste("Cannot run SP component in year", t, ", SP data is not available for this model and year."))
  }
}




     
