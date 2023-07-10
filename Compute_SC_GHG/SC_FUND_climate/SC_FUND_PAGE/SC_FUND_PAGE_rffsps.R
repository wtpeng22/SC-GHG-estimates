## FUND model
library(arrow)
library(openxlsx)
library(data.table)

setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise");
# setwd("/WORK/tsinghua_fteng_1/NCC_revise");

simulation_no = 10000

TimeStep=10;
Region_No=8;

simulation_year = 3;

globaltoregionaltemp = read.xlsx("Module/Climate/downscale/Downscale_temp_coeff.xlsx",sheet = "page_temp_coeff",startRow = 1);
globaltoregionaltemp = globaltoregionaltemp$Coeff

year_target = c(2009,2010,2020,2030,2040,2050,2075,2100,2150,2200);

### need Temperature, CO2 concentration and s_sealevel level rise
FUND_row_target = year_target-1950;

## Temperature
FUND_data_T_rffsps_base = fread("./Module/Climate/FUND/outputs/rffsps/T_base.csv",encoding = "UTF-8",header = T);
FUND_data_T_rffsps_base = as.matrix(FUND_data_T_rffsps_base[V1 %in% FUND_row_target,-1]);

FUND_data_T_rffsps_CO2 = fread("./Module/Climate/FUND/outputs/rffsps/T_extra_CO2.csv",encoding = "UTF-8",header = T);
FUND_data_T_rffsps_CO2 = as.matrix(FUND_data_T_rffsps_CO2[V1 %in% FUND_row_target,-1]);

FUND_data_T_rffsps_CH4 = fread("./Module/Climate/FUND/outputs/rffsps/T_extra_CH4.csv",encoding = "UTF-8",header = T);
FUND_data_T_rffsps_CH4 = as.matrix(FUND_data_T_rffsps_CH4[V1 %in% FUND_row_target,-1]);

FUND_data_T_rffsps_N2O = fread("./Module/Climate/FUND/outputs/rffsps/T_extra_N2O.csv",encoding = "UTF-8",header = T);
FUND_data_T_rffsps_N2O = as.matrix(FUND_data_T_rffsps_N2O[V1 %in% FUND_row_target,-1]);

## s_sealevel level rise
FUND_data_S_rffsps_base = fread("./Module/Climate/FUND/outputs/rffsps/S_base.csv",encoding = "UTF-8",header = T);
FUND_data_S_rffsps_base = as.matrix(FUND_data_S_rffsps_base[V1 %in% FUND_row_target,-1]);

FUND_data_S_rffsps_CO2 = fread("./Module/Climate/FUND/outputs/rffsps/S_extra_CO2.csv",encoding = "UTF-8",header = T);
FUND_data_S_rffsps_CO2 = as.matrix(FUND_data_S_rffsps_CO2[V1 %in% FUND_row_target,-1]);

FUND_data_S_rffsps_CH4 = fread("./Module/Climate/FUND/outputs/rffsps/S_extra_CH4.csv",encoding = "UTF-8",header = T);
FUND_data_S_rffsps_CH4 = as.matrix(FUND_data_S_rffsps_CH4[V1 %in% FUND_row_target,-1]);

FUND_data_S_rffsps_N2O = fread("./Module/Climate/FUND/outputs/rffsps/S_extra_N2O.csv",encoding = "UTF-8",header = T);
FUND_data_S_rffsps_N2O = as.matrix(FUND_data_S_rffsps_N2O[V1 %in% FUND_row_target,-1]);

Monte_Carlo_T <-function(){
  # 
  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    rt_g_globaltemperature <<- FUND_data_T_rffsps_base[,target_climate];
    s_sealevel <<- FUND_data_S_rffsps_base[,target_climate]/100;
  }
  
  if(MarginalEmission_CO2 == TRUE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    rt_g_globaltemperature <<- FUND_data_T_rffsps_CO2[,target_climate];
    s_sealevel <<- FUND_data_S_rffsps_CO2[,target_climate]/100;
  }
  
  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == TRUE & MarginalEmission_N2O == FALSE){
    rt_g_globaltemperature <<- FUND_data_T_rffsps_CH4[,target_climate];
    s_sealevel <<- FUND_data_S_rffsps_CH4[,target_climate]/100;
  }
  
  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == TRUE){
    rt_g_globaltemperature <<- FUND_data_T_rffsps_N2O[,target_climate];
    s_sealevel <<- FUND_data_S_rffsps_N2O[,target_climate]/100;
  }
}

scenario_sample = read.csv("Module/Sample/climate_scenario_sample.csv");

pop_rffsps = read.csv("Module/Socioeconomic/outputs/PAGE_data/rffsps_global_pop.csv",header = T)
gdp_rffsps = read.csv("Module/Socioeconomic/outputs/PAGE_data/rffsps_global_gdp.csv",header = T)
pop_rffsps = pop_rffsps[,-1]
gdp_rffsps = gdp_rffsps[,-1]

prtps = c(0.2) # %
etas = c(1.24) 
certainty_eq = "false"

diagnostic_PAGE<-function(run_time){
  model_choose<<-"MonteCarlo"
  target_climate <<- run_time;
  target_scenario <<- scenario_sample$x[run_time];
  gdp_target <<- matrix(gdp_rffsps[,target_scenario],nrow = TimeStep)/10^6
  pop_target <<- matrix(pop_rffsps[,target_scenario],nrow = TimeStep)/10^6
  
  Monte_Carlo_PAGE_damage();
  
  MarginalEmission_CO2<<-FALSE
  MarginalEmission_CH4<<-FALSE
  MarginalEmission_N2O<<-FALSE
  Monte_Carlo_T();
  playground_page_damage(ptp_timepreference = 1.5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_global <<- rowSums(wit_equityweightedimpact);
  damage_base_1.5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 2,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_base_2<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 3,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_base_3<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_base_5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_base_timevarying<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "true",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_base_timevarying_ces<<-te_totaleffect;
  
  MarginalEmission_CO2<<-TRUE;
  Monte_Carlo_T();
  playground_page_damage(ptp_timepreference = 1.5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_global_impulse_CO2 <<- rowSums(wit_equityweightedimpact);
  damage_CO2imp_1.5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 2,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_CO2imp_2<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 3,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_CO2imp_3<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_CO2imp_5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_CO2imp_timevarying<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "true",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_CO2imp_timevarying_ces<<-te_totaleffect;
  SCC_PAGE_CO2<<-c((damage_CO2imp_1.5-damage_base_1.5),(damage_CO2imp_2-damage_base_2),(damage_CO2imp_3-damage_base_3),(damage_CO2imp_5-damage_base_5),(damage_CO2imp_timevarying-damage_base_timevarying),(damage_CO2imp_timevarying_ces-damage_base_timevarying_ces))/100000;
  MarginalEmission_CO2<<-FALSE;
  
  MarginalEmission_CH4<<-TRUE
  Monte_Carlo_T();
  playground_page_damage(ptp_timepreference = 1.5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_global_impulse_CH4 <<- rowSums(wit_equityweightedimpact);
  damage_CH4imp_1.5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 2,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_CH4imp_2<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 3,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_CH4imp_3<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_CH4imp_5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_CH4imp_timevarying<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "true",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_CH4imp_timevarying_ces<<-te_totaleffect;
  SCC_PAGE_CH4<<-c((damage_CH4imp_1.5-damage_base_1.5),(damage_CH4imp_2-damage_base_2),(damage_CH4imp_3-damage_base_3),(damage_CH4imp_5-damage_base_5),(damage_CH4imp_timevarying-damage_base_timevarying),(damage_CH4imp_timevarying_ces-damage_base_timevarying_ces))/100000;
  MarginalEmission_CH4<<-FALSE;
  
  MarginalEmission_N2O<<-TRUE
  Monte_Carlo_T();
  playground_page_damage(ptp_timepreference = 1.5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_global_impulse_N2O <<- rowSums(wit_equityweightedimpact);
  damage_N2Oimp_1.5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 2,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_N2Oimp_2<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 3,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_N2Oimp_3<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = 5,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = 0)
  damage_N2Oimp_5<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "false",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_N2Oimp_timevarying<<-te_totaleffect;
  playground_page_damage(ptp_timepreference = prtps,equity_proportion = 0,certainty_eq = "true",emuc_utilityconvexity = 0,risk_aversion = etas)
  damage_N2Oimp_timevarying_ces<<-te_totaleffect;
  SCC_PAGE_N2O<<-c((damage_N2Oimp_1.5-damage_base_1.5),(damage_N2Oimp_2-damage_base_2),(damage_N2Oimp_3-damage_base_3),(damage_N2Oimp_5-damage_base_5),(damage_N2Oimp_timevarying-damage_base_timevarying),(damage_N2Oimp_timevarying_ces-damage_base_timevarying_ces))/100000;
  MarginalEmission_N2O<<-FALSE
  
  return(list(SCC_PAGE_CO2,SCC_PAGE_CH4,SCC_PAGE_N2O,damage_global,damage_global_impulse_CO2,damage_global_impulse_CH4,damage_global_impulse_N2O,globalconsumption_percap_afterdamage));
}

MarginalEmission_CO2<<-FALSE
MarginalEmission_CH4<<-FALSE
MarginalEmission_N2O<<-FALSE

SC_CO2 = matrix(nrow = simulation_no,ncol = 6)
SC_CH4 = matrix(nrow = simulation_no,ncol = 6)
SC_N2O = matrix(nrow = simulation_no,ncol = 6)

Global_Damage_base = matrix(nrow = simulation_no,ncol = 10)
Global_Damage_impulse_CO2 = matrix(nrow = simulation_no,ncol = 10)
Global_Damage_impulse_CH4 = matrix(nrow = simulation_no,ncol = 10)
Global_Damage_impulse_N2O = matrix(nrow = simulation_no,ncol = 10)
Global_CPC_value = matrix(nrow = simulation_no,ncol = 10)

model_choose="MonteCarlo";
  
source("Module/Damage/PAGE/PAGE_damage.R");
source("Module/Damage/PAGE/Monte_Carlo_damage.R");
  
library(parallel)
system.time({
res1.p = mclapply(1:simulation_no,FUN = function(run_time) {diagnostic_PAGE(run_time)},mc.cores = 2);
}
);

for (run_time in 1:simulation_no) {
  SC_CO2[run_time,] = res1.p[[run_time]][[1]];
  SC_CH4[run_time,] = res1.p[[run_time]][[2]]*100;
  SC_N2O[run_time,] = res1.p[[run_time]][[3]]*1000;
  Global_Damage_base[run_time,] = res1.p[[run_time]][[4]];
  Global_Damage_impulse_CO2[run_time,] = res1.p[[run_time]][[5]];
  Global_Damage_impulse_CH4[run_time,] = res1.p[[run_time]][[6]];
  Global_Damage_impulse_N2O[run_time,] = res1.p[[run_time]][[7]];
  Global_CPC_value[run_time,] = res1.p[[run_time]][[8]];
}
#

SC_CO2[,6] = SC_CO2[,6]*1/mean(Global_CPC_value[,simulation_year]^(0-etas))
SC_CH4[,6] = SC_CH4[,6]*1/mean(Global_CPC_value[,simulation_year]^(0-etas))
SC_N2O[,6] = SC_N2O[,6]*1/mean(Global_CPC_value[,simulation_year]^(0-etas))

write.csv(SC_CO2,"Compute_SC_GHG/SC_FUND_climate/SC_FUND_PAGE/rffsps/SC_CO2_FUND_PAGE.csv",quote = F,row.names = F)
write.csv(SC_CH4,"Compute_SC_GHG/SC_FUND_climate/SC_FUND_PAGE/rffsps/SC_CH4_FUND_PAGE.csv",quote = F,row.names = F)
write.csv(SC_N2O,"Compute_SC_GHG/SC_FUND_climate/SC_FUND_PAGE/rffsps/SC_N2O_FUND_PAGE.csv",quote = F,row.names = F)
write.csv(Global_Damage_base,"Compute_SC_GHG/SC_FUND_climate/SC_FUND_PAGE/rffsps/Global_damage_base_FUND_PAGE.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_CO2,"Compute_SC_GHG/SC_FUND_climate/SC_FUND_PAGE/rffsps/Global_damage_impulse_CO2_FUND_PAGE.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_CH4,"Compute_SC_GHG/SC_FUND_climate/SC_FUND_PAGE/rffsps/Global_damage_impulse_CH4_FUND_PAGE.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_N2O,"Compute_SC_GHG/SC_FUND_climate/SC_FUND_PAGE/rffsps/Global_damage_impulse_N2O_FUND_PAGE.csv",quote = F,row.names = F)

