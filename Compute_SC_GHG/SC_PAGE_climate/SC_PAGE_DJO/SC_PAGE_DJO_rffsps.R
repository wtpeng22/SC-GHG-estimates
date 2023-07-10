library(data.table)
library(openxlsx)
library(arrow)
library(dplyr);

## DJO et al. damage function
setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise");
# setwd("/WORK/tsinghua_fteng_1/NCC_revise");

simulation_no = 10000

## Climate responses 
year_target=2019:2100;
PAGE_row_target = year_target-1950;

PAGE_data_T_rffsps_base = fread("./Module/Climate/PAGE/outputs/rffsps/T_base.csv",encoding = "UTF-8",header = T);
PAGE_data_T_rffsps_base = as.matrix(PAGE_data_T_rffsps_base[V1 %in% PAGE_row_target,-1]);

PAGE_data_T_rffsps_CO2 = fread("./Module/Climate/PAGE/outputs/rffsps/T_extra_CO2.csv",encoding = "UTF-8",header = T);
PAGE_data_T_rffsps_CO2 = as.matrix(PAGE_data_T_rffsps_CO2[V1 %in% PAGE_row_target,-1]);

PAGE_data_T_rffsps_CH4 = fread("./Module/Climate/PAGE/outputs/rffsps/T_extra_CH4.csv",encoding = "UTF-8",header = T);
PAGE_data_T_rffsps_CH4 = as.matrix(PAGE_data_T_rffsps_CH4[V1 %in% PAGE_row_target,-1]);

PAGE_data_T_rffsps_N2O = fread("./Module/Climate/PAGE/outputs/rffsps/T_extra_N2O.csv",encoding = "UTF-8",header = T);
PAGE_data_T_rffsps_N2O = as.matrix(PAGE_data_T_rffsps_N2O[V1 %in% PAGE_row_target,-1]);

Monte_Carlo_T <-function(){
  # 
  if(MarginalEmission_CO2 == TRUE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    ssp_glob_temp_target <<- PAGE_data_T_rffsps_base[,target_climate];
    ssp_glob_temp_pulse_target <<- PAGE_data_T_rffsps_CO2[,target_climate];
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == TRUE & MarginalEmission_N2O == FALSE){
    ssp_glob_temp_target <<- PAGE_data_T_rffsps_base[,target_climate];
    ssp_glob_temp_pulse_target <<- PAGE_data_T_rffsps_CH4[,target_climate];
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == TRUE){
    ssp_glob_temp_target <<- PAGE_data_T_rffsps_base[,target_climate];
    ssp_glob_temp_pulse_target <<- PAGE_data_T_rffsps_N2O[,target_climate];
  }

  ssp_glob_temp <<- ssp_glob_temp_target[-1] - ssp_glob_temp_target[1];
  ssp_glob_temp_pulse <<- ssp_glob_temp_pulse_target[-1] - ssp_glob_temp_pulse_target[1];
}

# downscale the global climate responses to countrial/regional level
T_downscale_country = read.xlsx("Module/Climate/downscale/Downscale_temp_coeff.xlsx",sheet = "country_temp_coeff",startRow = 1);
colnames(T_downscale_country)  = c("ISO3","tempcoeff")

# load baseline GDP and pop data
gdpcap_baseline = read.xlsx("Module/Socioeconomic/outputs/Country_level/Baseline_data.xlsx",sheet = "Country-level-data",startRow = 1)

base_year = 2019;
pro_years = 2020:2200;
pro_years_2100 = 2020:2100;

Country_num = length(T_downscale_country$ISO3)
Year_num = length(pro_years)

target_country = c("CAN","CHN","IND","RUS","USA");

nid_sample = read.csv("Module/Sample/nid_sample.csv");
scenario_sample = read.csv("Module/Sample/climate_scenario_sample.csv");

prtps = c(0.2) # %
etas = c(1.24) 

Diagnostic_SC_GHG <- function(gases_target,run_time){
  
  target_climate <<- run_time;
  target_scenario <<- scenario_sample$x[run_time];
  
  ##sample scenario
  rffsp_pop_income_run_sample <<- as.data.table(read_feather(paste("Module/rffsps_v5/pop_income/rffsp_pop_income_run_",target_scenario,".feather",sep = "")))
  colnames(rffsp_pop_income_run_sample) <<- c("ISO3","year","pop","gdp")
  rffsp_pop_income_run_sample$gdpcap <<- rffsp_pop_income_run_sample$gdp/rffsp_pop_income_run_sample$pop
  rffsp_growthrate_sample_pro <<- rffsp_pop_income_run_sample[, annual_gdp_pop_g(.SD), by = c("ISO3")]
  rffsp_growthrate_sample_2020 <<- rffsp_growthrate_sample_pro[rffsp_growthrate_sample_pro$year==(base_year+2),]
  rffsp_growthrate_sample_2020$year <<- base_year+1
  rffsp_growthrate_sample <<- rbind(rffsp_growthrate_sample_2020,rffsp_growthrate_sample_pro)
  growthrate_sample <<- rffsp_growthrate_sample[rffsp_growthrate_sample$ISO3 %in% gdpcap_baseline$ISO3,]
  growthrate_sample <<- growthrate_sample[order(ISO3),]
  
  ##sample damage function
  nid <<-nid_sample$x[run_time];
  
  ###run climate and damage models
  MarginalEmission_CO2<<-FALSE;
  MarginalEmission_CH4<<-FALSE;
  MarginalEmission_N2O<<-FALSE;
  
  if(gases_target=="CO2"){
    MarginalEmission_CO2<<-TRUE
    Monte_Carlo_T();
    Playground_DJO();
    MarginalEmission_CO2<<-FALSE
  }
  
  if(gases_target=="CH4"){
    MarginalEmission_CH4<<-TRUE
    Monte_Carlo_T();
    Playground_DJO();
    MarginalEmission_CH4<<-FALSE
  }
  
  if(gases_target=="N2O"){
    MarginalEmission_N2O<<-TRUE
    Monte_Carlo_T();
    Playground_DJO();
    MarginalEmission_N2O<<-FALSE
  }
  
  ##Country level
  res_scc <<- ssp_gdpr[,project_gdpcap(.SD),by = c("ISO3")];
  gdprate_cc_impulse_year = res_scc[year == impulse_year,
                                    .(gdpcap_cc_impulse_year = gdpcap_cc),
                                    by = c("ISO3")];
  res_scc <- merge(res_scc,gdprate_cc_impulse_year,by = c("ISO3"));
  res_scc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                     gdprate_cc,
                                     (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)];
  res_wscc <<- res_scc[,.(gdpcap_cc = weighted.mean(gdpcap_cc,pop)),by = c("year")];
  
  #World
  res_wscc <<- transform(res_wscc, gdpcap_cc_impulse_year = res_wscc[year==impulse_year]$gdpcap_cc);
  res_wscc[, gdprate_cc_avg := ifelse(year == impulse_year,
                                      NA,
                                      (gdpcap_cc/gdpcap_cc_impulse_year)^(1/(year - impulse_year)) - 1)]
  res_wscc <<- transform(res_wscc,gdprate_cc_avg_impulse_year = res_wscc[year == (impulse_year + 1)]$gdprate_cc_avg);
  res_wscc[year == impulse_year,gdprate_cc_avg := gdprate_cc_avg_impulse_year]
  res_wscc[,gdprate_cc_avg_impulse_year := NULL]
  
  # Compute SCC according to paper by Rennert et al. published on Nature
  # \dfrac {\partial C_{t}} {\partial E_{0}}\times P_{t}
  # approximate by change in GDP rather than consumption
  res_scc[, scc := -(gdpcap_imp - gdpcap_cc) * pop * (1 / pulse_scale)] # $2010/tCO2
  
  global_gdp = res_scc[,sum(gdpcap*pop),by=c("year")]
  global_damage = res_scc[,sum(gdpcap_cc*pop),by=c("year")]
  global_damage_impulse = res_scc[,sum(gdpcap_imp*pop),by=c("year")]
  colnames(global_gdp)[2]="global_gdp"
  colnames(global_damage)[2]="global_damage"
  colnames(global_damage_impulse)[2]="global_damage_impulse"
  
  res_wscc <<- merge(res_wscc,res_scc[, .(scc = sum(scc)),by = c("year")],
                     by = c("year"))
  
  ## project the SCC and gdp growth rate from 2100 to 2200 as constant with year 2100
  res_scc_future <- res_scc[,extrapolate_scc(.SD),by = c("ISO3")]
  res_wscc_future <- res_wscc[,extrapolate_scc(.SD)]
  res_scc <- rbindlist(list(res_scc,res_scc_future),fill = T)
  res_wscc <- rbindlist(list(res_wscc,res_wscc_future),fill = T)
  colnames(res_wscc) = c("year","global_gdpcap_cc","global_gdpcap_cc_impulse_year","global_gdprate_cc_avg","global_scc")
  res_scc = merge(res_scc,res_wscc,by = "year")
  
  # Discount SCC according to timevarying discount rate by Rennert et al. published on Nature
  # elasticity of marginal utility of consumption = 1.24
  # added 0.2% prtp to be compatible with EPA
  
  cscc1 = NULL
  cscc2 = NULL
  
  for (.prtp in prtps) {
    for (.eta in etas) {
      dscc <<- res_scc[,list(ISO3,year,gdprate_cc,gdprate_cc_avg,scc,global_gdprate_cc_avg,global_gdpcap_cc)]
      dscc[,global_gdpcap_cc := dscc[year == impulse_year, global_gdpcap_cc][1]*(1+dscc[year >= impulse_year, global_gdprate_cc_avg])^(dscc[year >= impulse_year, year-impulse_year])]
      dscc[,cpc_value := global_gdpcap_cc*(1-saving_rate)];
      # dscc[,dfac := (1/((1 + .prtp/100)^(year - impulse_year))*(((1+gdprate_cc_avg)^(year - impulse_year))^(0-.eta)))]
      dscc[,dfac := (1/((1 + .prtp/100)^(year - impulse_year))*(((1+global_gdprate_cc_avg)^(year - impulse_year))^(0-.eta)))]
      dscc[,dfac_ces := (1/((1 + .prtp/100)^(year - impulse_year))*((cpc_value)^(0-.eta)))]
      
      dscc[,dscc := dfac * scc]
      dscc[,dscc_ces := dfac_ces * scc]
      
      cscc1 = rbind(cscc1,dscc[,.(prtp = .prtp,eta = .eta,scc = sum(dscc)),
                               by = c("ISO3")],fill = T)
      cscc2 = rbind(cscc2,dscc[,.(prtp = .prtp,eta = .eta,scc = sum(dscc_ces)),
                               by = c("ISO3")],fill = T)
      global_cpc_value = dscc[ISO3 == "CHN",cpc_value]
    }
  }
  
  # Comparison EPA (SC-CO2) [[http://www3.epa.gov/climatechange/EPAactivities/economics/scc.html]]
  drs <<- c(1.5,2,3,5) #%
  
  cscc0 <<- NULL
  
  for (.dr in drs) {
    ## project the SCC from 2100 to 2200 as constant with year 2100
    dscc <<- res_scc[,list(ISO3,year,gdprate_cc,gdprate_cc_avg,scc)]
    dscc[,dfac := (1/(1 + .dr/100)^(year - impulse_year+1))]
    dscc[,dscc := dfac * scc]
    cscc0 <<- rbind(cscc0,dscc[,.(dr = .dr,scc = sum(dscc)),by = c("ISO3")])
  }
  cscc1$dr=-1;
  cscc2$dr=-2;
  cscc = rbindlist(list(cscc0,cscc1,cscc2),fill = T);
  wscc = cscc[,(scc = sum(scc)),by = c("dr")];
  wscc = cbind(rep("World",6),wscc)
  colnames(wscc)[1]="ISO3"
  colnames(wscc)[3]="scc"
  country_damage = res_scc[ISO3 %in% target_country]
  return(list(cscc,wscc,country_damage,global_damage,global_damage_impulse,global_gdp,global_cpc_value));
}



MarginalEmission_CO2<<-FALSE
MarginalEmission_CH4<<-TRUE
MarginalEmission_N2O<<-FALSE

pulse_scale = 1e9;
gases_types = c("CO2","CH4","N2O")
discount_types = c("fixed_1.5","fixed_2","fixed_3","fixed_5","timevarying","timevarying_ces")

CSC_CO2 = matrix(nrow = simulation_no,ncol = Country_num*length(discount_types))
WSC_CO2 = matrix(nrow = simulation_no,ncol = length(discount_types))
CSC_CH4 = matrix(nrow = simulation_no,ncol = Country_num*length(discount_types))
WSC_CH4 = matrix(nrow = simulation_no,ncol = length(discount_types))
CSC_N2O = matrix(nrow = simulation_no,ncol = Country_num*length(discount_types))
WSC_N2O = matrix(nrow = simulation_no,ncol = length(discount_types))
Country_target_gdpcap_base = matrix(nrow = simulation_no,ncol = length(pro_years_2100)*length(target_country))
Country_target_gdpcap_cc = matrix(nrow = simulation_no,ncol = length(pro_years_2100)*length(target_country))
Country_target_gdpcap_imp_CO2 = matrix(nrow = simulation_no,ncol = length(pro_years_2100)*length(target_country))
Country_target_gdpcap_imp_CH4 = matrix(nrow = simulation_no,ncol = length(pro_years_2100)*length(target_country))
Country_target_gdpcap_imp_N2O = matrix(nrow = simulation_no,ncol = length(pro_years_2100)*length(target_country))
Global_gdp_base = matrix(nrow = simulation_no,ncol = length(pro_years_2100))
Global_damage_base = matrix(nrow = simulation_no,ncol = length(pro_years_2100))
Global_damage_imp_CO2 = matrix(nrow = simulation_no,ncol = length(pro_years_2100))
Global_damage_imp_CH4 = matrix(nrow = simulation_no,ncol = length(pro_years_2100))
Global_damage_imp_N2O = matrix(nrow = simulation_no,ncol = length(pro_years_2100))
Global_CPC_CO2 = matrix(nrow = simulation_no,ncol = length(pro_years))
Global_CPC_CH4 = matrix(nrow = simulation_no,ncol = length(pro_years))
Global_CPC_N2O = matrix(nrow = simulation_no,ncol = length(pro_years))

source("Module/Damage/DJO/DJO_damage.R");

library(parallel)

res1.p = mclapply(1:simulation_no,FUN = function(run_time) {Diagnostic_SC_GHG("CO2",run_time)},mc.cores = 2);

for (run_time in 1:simulation_no) {
  CSC_CO2[run_time,] = res1.p[[run_time]][[1]]$scc/100;
  WSC_CO2[run_time,] = res1.p[[run_time]][[2]]$scc/100;
  
  Country_target_gdpcap_base[run_time,] = res1.p[[run_time]][[3]]$gdpcap[1:(5*length(pro_years_2100))];
  Country_target_gdpcap_cc[run_time,] = res1.p[[run_time]][[3]]$gdpcap_cc[1:(5*length(pro_years_2100))];
  Country_target_gdpcap_imp_CO2[run_time,] = res1.p[[run_time]][[3]]$gdpcap_imp[1:(5*length(pro_years_2100))];
  
  Global_damage_base[run_time,] = res1.p[[run_time]][[4]]$global_damage;
  Global_damage_imp_CO2[run_time,] = res1.p[[run_time]][[5]]$global_damage_impulse;
  Global_CPC_CO2[run_time,] = res1.p[[run_time]][[7]];
}

WSC_CO2[,6] = WSC_CO2[,6]*1/mean(Global_CPC_CO2[,impulse_year-base_year]^(0-etas))
CSC_CO2[,(Country_num*5+1):(Country_num*6)] = CSC_CO2[,(Country_num*5+1):(Country_num*6)]*1/mean(Global_CPC_CO2[,impulse_year-base_year]^(0-etas))

res2.p = mclapply(1:simulation_no,FUN = function(run_time) {Diagnostic_SC_GHG("CH4",run_time)},mc.cores = 2);

for (run_time in 1:simulation_no) {
  CSC_CH4[run_time,] = res2.p[[run_time]][[1]]$scc;
  WSC_CH4[run_time,] = res2.p[[run_time]][[2]]$scc;
  
  Country_target_gdpcap_imp_CH4[run_time,] = res2.p[[run_time]][[3]]$gdpcap_imp[1:(5*length(pro_years_2100))];
  Global_damage_imp_CH4[run_time,] = res2.p[[run_time]][[5]]$global_damage_impulse;
  Global_CPC_CH4[run_time,] = res1.p[[run_time]][[7]];
}

WSC_CH4[,6] = WSC_CH4[,6]*1/mean(Global_CPC_CH4[,impulse_year-base_year]^(0-etas))
CSC_CH4[,(Country_num*5+1):(Country_num*6)] = CSC_CH4[,(Country_num*5+1):(Country_num*6)]*1/mean(Global_CPC_CH4[,impulse_year-base_year]^(0-etas))

res3.p = mclapply(1:simulation_no,FUN = function(run_time) {Diagnostic_SC_GHG("N2O",run_time)},mc.cores = 2);

for (run_time in 1:simulation_no) {
  CSC_N2O[run_time,] = res3.p[[run_time]][[1]]$scc*10;
  WSC_N2O[run_time,] = res3.p[[run_time]][[2]]$scc*10;
  
  Country_target_gdpcap_imp_N2O[run_time,] = res3.p[[run_time]][[3]]$gdpcap_imp[1:(5*length(pro_years_2100))];
  Global_damage_imp_N2O[run_time,] = res3.p[[run_time]][[5]]$global_damage_impulse;
  Global_CPC_N2O[run_time,] = res1.p[[run_time]][[7]];
}

WSC_N2O[,6] = WSC_N2O[,6]*1/mean(Global_CPC_N2O[,impulse_year-base_year]^(0-etas))
CSC_N2O[,(Country_num*5+1):(Country_num*6)] = CSC_N2O[,(Country_num*5+1):(Country_num*6)]*1/mean(Global_CPC_N2O[,impulse_year-base_year]^(0-etas))


write.csv(CSC_CO2,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/CSC_CO2_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(WSC_CO2,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/WSC_CO2_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(CSC_CH4,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/CSC_CH4_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(WSC_CH4,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/WSC_CH4_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(CSC_N2O,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/CSC_N2O_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(WSC_N2O,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/WSC_N2O_PAGE_DJO.csv",quote = F,row.names = F)

write.csv(Country_target_gdpcap_base,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/gdpcap_base_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Country_target_gdpcap_cc,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/gdpcap_cc_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Country_target_gdpcap_imp_CO2,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/gdpcap_imp_CO2_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Country_target_gdpcap_imp_CH4,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/gdpcap_imp_CH4_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Country_target_gdpcap_imp_N2O,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/gdpcap_imp_N2O_PAGE_DJO.csv",quote = F,row.names = F)

write.csv(Global_gdp_base,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/Global_gdp_base_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Global_damage_base,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/Global_damage_base_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Global_damage_imp_CO2,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/Global_damage_imp_CO2_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Global_damage_imp_CH4,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/Global_damage_imp_CH4_PAGE_DJO.csv",quote = F,row.names = F)
write.csv(Global_damage_imp_N2O,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_DJO/rffsps/Global_damage_imp_N2O_PAGE_DJO.csv",quote = F,row.names = F)

