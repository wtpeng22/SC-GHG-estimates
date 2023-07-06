# library(data.table)
# library(stringr)
# library(countrycode)
# library(pracma)

out_of_sample = TRUE;
fyears = seq(2020,2100,by=1);
impulse_year = 2020;
saving_rate = 0.2;

g_rich <- function(temp, rid) { return(-0.191 / 100 * temp) }
g_poor <- function(temp, rid) { return(-1.041 / 100 * temp) }

warming_effect <- function(temp, temp_tm1, gdpcap_tm1, runid, out_of_sample=T,Poor_Rich){
  if(gdpcap_tm1 >= 3291){  ###3291 is the median GDP per capital in 1980 according to the WDI dataset
    return(g_rich(temp-temp_tm1,runid))
  } else {
    return(g_poor(temp-temp_tm1,runid))
  }
}

# Compute annual gowth rate of GDP per capital and Pop
annual_gdp_pop_g <- function(SD){
  g_years <- seq(SD$year[1], SD$year[length(SD$year)], by = 1)
  annual_gdp <- exp(approx(SD$year, log(SD$gdp), g_years)$y)
  annual_pop <- exp(approx(SD$year, log(SD$pop), g_years)$y)
  annual_gdpcap <- annual_gdp/annual_pop
  gdpg <- annual_gdp[2:(length(g_years))]/annual_gdp[1:(length(g_years) - 1)] - 1
  popg <- annual_pop[2:(length(g_years))]/annual_pop[1:(length(g_years) - 1)] - 1
  gdpcapg <- annual_gdpcap[2:(length(g_years))]/annual_gdpcap[1:(length(g_years) - 1)] - 1
  return(list(year = g_years[2:(length(g_years))],gdpr = gdpg,popr  = popg, gdpcapr = gdpcapg))
}

# Project all scenarios
reftemplastyear = F
nid=0

project_gdpcap <- function(SD){
  .gdpcap <- SD$gdppercap
  .gdpcap_cc <- SD$gdppercap
  .gdpcap_imp <- SD$gdppercap
  .gdprate_cc <- SD$gdpr
  .pop <- SD$pop
  
  .gdpcap_tm1 <- .gdpcap[1] # gdpcap nocc in 2019
  .gdpcap_tm1_cc <- .gdpcap[1] # gdpcap nocc in 2019
  .gdpcap_tm1_imp <- .gdpcap[1] # gdpcap nocc in 2019
  .pop_tm1 <- .pop[1] # pop in 2019
  
  .ref_temp <- SD$temp[1] # reftemp is baseline temp for BHM and DJO
  if (!reftemplastyear) {.ref_temp <- SD$basetemp[1]}
  
  for (i in seq_along(c(fyears))) {
    # No climate change
    .gdpcap[i] <- .gdpcap_tm1 * (1 + SD$gdpcapr[i])
    .gdpcap_tm1 <- .gdpcap[i]
    .pop[i] <- .pop_tm1 * (1 + SD$popr[i])
    .pop_tm1 <- .pop[i]
    # With climate change
    .gdprate_cc[i] <- SD$gdpcapr[i] + warming_effect(SD$temp[i], .ref_temp, .gdpcap_tm1_cc, nid, out_of_sample)
    .gdpcap_cc[i] <- .gdpcap_tm1_cc * (1 + .gdprate_cc[i])
    .gdpcap_imp[i] <- .gdpcap_tm1_imp * (1 + (SD$gdpcapr[i] + warming_effect(SD$temp_pulse[i], .ref_temp, .gdpcap_tm1_imp, nid, out_of_sample)))
    # if(.gdpcap_cc[i]>(15*(.gdpcap[i]))){
    #   .gdpcap_cc[i] <- .gdpcap_tm1_cc * (1 + SD$gdpcapr[i])
    #   .gdpcap_imp[i] <- .gdpcap_tm1_imp * (1 + SD$gdpcapr[i])
    # }
    .gdpcap_tm1_cc <- .gdpcap_cc[i]
    .gdpcap_tm1_imp <- .gdpcap_imp[i]

    if (reftemplastyear) {.ref_temp <- SD$temp[i]}
  }
  return(list(year = fyears, 
              gdpcap = .gdpcap,
              gdpcap_cc = .gdpcap_cc,
              gdpcap_imp = .gdpcap_imp,
              gdprate_cc = .gdprate_cc,
              pop=.pop
  ))
}


Playground_DJO<-function(){
# downscale temperature to countrial level
ssp_countrial_temp <<- growthrate_sample[year %in% fyears,-5];
colnames(ssp_countrial_temp)[3:4] <<- c("temp","temp_pulse");

ssp_countrial_temp$temp <<- rep(ssp_glob_temp,nrow(T_downscale_country)) * rep(T_downscale_country$tempcoeff,each = length(ssp_glob_temp));
ssp_countrial_temp$temp_pulse <<- rep(ssp_glob_temp_pulse,nrow(T_downscale_country)) * rep(T_downscale_country$tempcoeff,each = length(ssp_glob_temp));

# merge with base temp
ssp_gr <<- growthrate_sample[year %in% fyears,]
base_temp <<- gdpcap_baseline[,c(2,6)]
ssp_temp <<- merge(ssp_countrial_temp,base_temp,by = c("ISO3")); # add basetemp
ssp_temp$temp <<- ssp_temp$temp + ssp_temp$basetemp;
ssp_temp$temp_pulse <<- ssp_temp$temp_pulse + ssp_temp$basetemp;
ssp_gdpr <<- merge(ssp_gr,ssp_temp,by = c("ISO3","year")) # merge growth rate and temp
gdpcap_baseline$year <<- 2020;
ssp_gdpr <<- merge(ssp_gdpr, gdpcap_baseline[-c(1,6)],
                 by = c("ISO3","year"),all.x = T) # add gdpcap0
}

# Extrapolation SCC (before discounting)
very_last_year = 2200;
extrapolate_scc <- function(SD){
  .scc = SD[year == 2100,scc]
  .gdpr = (SD[year == 2100,gdpcap_cc]/SD[year == 2100,gdpcap_cc_impulse_year])^(1/(2100 - impulse_year)) - 1
  if (.gdpr < 0) {
    .gdprate_cc_avg = (SD[year == 2100,gdpcap_cc]/SD[year == 2100,gdpcap_cc_impulse_year])^(1/((2101:very_last_year) - impulse_year)) - 1
  }else {
    .gdprate_cc_avg = .gdpr
  }
  return(list(year = 2101:very_last_year, scc = .scc, gdprate_cc_avg = .gdprate_cc_avg))
}


