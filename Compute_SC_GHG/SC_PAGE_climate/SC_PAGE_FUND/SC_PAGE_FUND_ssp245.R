## PAGE model
library(arrow)
library(openxlsx)
library(data.table)

setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise");
# setwd("/WORK/tsinghua_fteng_1/NCC_revise");

simulation_no = 10000

TimeStep=251;
Region_No=16;

globaltoregionaltemp = read.xlsx("Module/Climate/downscale/Downscale_temp_coeff.xlsx",sheet = "fund_temp_coeff",startRow = 1);
globaltoregionaltemp = globaltoregionaltemp$Coeff

end_year = 2200;
year_target = 1950:2200
simulation_year = 2020;
simulation_year_index = simulation_year-1950+1
dataupdate_year = 1995;
dataupdate_year_index = dataupdate_year-1950+1

### need Temperature, CO2 concentration and s_sealevel level rise
PAGE_row_target = year_target[-(1:50)]-1950;

climatedata_his = read.xlsx("Module/Climate/Hector/outputs/historic_climate.xlsx")
s_temp_his = climatedata_his$temp
acco2_his = climatedata_his$acco2
sea_his = climatedata_his$slr

#### Temperature data
PAGE_data_T_ssp245_base = fread("./Module/Climate/PAGE/outputs/ssp245/T_base.csv",encoding = "UTF-8",header = T);
PAGE_data_T_ssp245_base = as.matrix(PAGE_data_T_ssp245_base[V1 %in% PAGE_row_target,-1]);

PAGE_data_T_ssp245_CO2 = fread("./Module/Climate/PAGE/outputs/ssp245/T_extra_CO2.csv",encoding = "UTF-8",header = T);
PAGE_data_T_ssp245_CO2 = as.matrix(PAGE_data_T_ssp245_CO2[V1 %in% PAGE_row_target,-1]);

PAGE_data_T_ssp245_CH4 = fread("./Module/Climate/PAGE/outputs/ssp245/T_extra_CH4.csv",encoding = "UTF-8",header = T);
PAGE_data_T_ssp245_CH4 = as.matrix(PAGE_data_T_ssp245_CH4[V1 %in% PAGE_row_target,-1]);

PAGE_data_T_ssp245_N2O = fread("./Module/Climate/PAGE/outputs/ssp245/T_extra_N2O.csv",encoding = "UTF-8",header = T);
PAGE_data_T_ssp245_N2O = as.matrix(PAGE_data_T_ssp245_N2O[V1 %in% PAGE_row_target,-1]);

## CO2 concentration
PAGE_data_C_ssp245_base = fread("./Module/Climate/PAGE/outputs/ssp245/C_base.csv",encoding = "UTF-8",header = T);
PAGE_data_C_ssp245_base = as.matrix(PAGE_data_C_ssp245_base[V1 %in% PAGE_row_target,-1]);

PAGE_data_C_ssp245_CO2 = fread("./Module/Climate/PAGE/outputs/ssp245/C_extra_CO2.csv",encoding = "UTF-8",header = T);
PAGE_data_C_ssp245_CO2 = as.matrix(PAGE_data_C_ssp245_CO2[V1 %in% PAGE_row_target,-1]);

PAGE_data_C_ssp245_CH4 = fread("./Module/Climate/PAGE/outputs/ssp245/C_extra_CH4.csv",encoding = "UTF-8",header = T);
PAGE_data_C_ssp245_CH4 = as.matrix(PAGE_data_C_ssp245_CH4[V1 %in% PAGE_row_target,-1]);

PAGE_data_C_ssp245_N2O = fread("./Module/Climate/PAGE/outputs/ssp245/C_extra_N2O.csv",encoding = "UTF-8",header = T);
PAGE_data_C_ssp245_N2O = as.matrix(PAGE_data_C_ssp245_N2O[V1 %in% PAGE_row_target,-1]);

## Sea level rise
PAGE_data_S_ssp245_base = fread("./Module/Climate/PAGE/outputs/ssp245/S_base.csv",encoding = "UTF-8",header = T);
PAGE_data_S_ssp245_base = as.matrix(PAGE_data_S_ssp245_base[V1 %in% PAGE_row_target,-1]);

PAGE_data_S_ssp245_CO2 = fread("./Module/Climate/PAGE/outputs/ssp245/S_extra_CO2.csv",encoding = "UTF-8",header = T);
PAGE_data_S_ssp245_CO2 = as.matrix(PAGE_data_S_ssp245_CO2[V1 %in% PAGE_row_target,-1]);

PAGE_data_S_ssp245_CH4 = fread("./Module/Climate/PAGE/outputs/ssp245/S_extra_CH4.csv",encoding = "UTF-8",header = T);
PAGE_data_S_ssp245_CH4 = as.matrix(PAGE_data_S_ssp245_CH4[V1 %in% PAGE_row_target,-1]);

PAGE_data_S_ssp245_N2O = fread("./Module/Climate/PAGE/outputs/ssp245/S_extra_N2O.csv",encoding = "UTF-8",header = T);
PAGE_data_S_ssp245_N2O = as.matrix(PAGE_data_S_ssp245_N2O[V1 %in% PAGE_row_target,-1]);


Monte_Carlo_T <-function(){
  ##aeei update
  for(t in 2:TimeStep){
    year_current<<-clock_Current(t);
    EmissionComponent(forestemm,pgrowth,ypcgrowth,income,population)
  }
  #climate data
  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    s_temp <<- c(s_temp_his,PAGE_data_T_ssp245_base[,target_climate]);
    acco2 <<- c(acco2_his,PAGE_data_C_ssp245_base[,target_climate]);
    sea <<- c(sea_his,PAGE_data_S_ssp245_base[,target_climate]/100);
  }

  if(MarginalEmission_CO2 == TRUE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    s_temp <<- c(s_temp_his,PAGE_data_T_ssp245_CO2[,target_climate]);
    acco2 <<- c(acco2_his,PAGE_data_C_ssp245_CO2[,target_climate]);
    sea <<- c(sea_his,PAGE_data_S_ssp245_CO2[,target_climate]/100);
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == TRUE & MarginalEmission_N2O == FALSE){
    s_temp <<- c(s_temp_his,PAGE_data_T_ssp245_CH4[,target_climate]);
    acco2 <<- c(acco2_his,PAGE_data_C_ssp245_CH4[,target_climate]);
    sea <<- c(sea_his,PAGE_data_S_ssp245_CH4[,target_climate]/100);
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == TRUE){
    s_temp <<- c(s_temp_his,PAGE_data_T_ssp245_N2O[,target_climate]);
    acco2 <<- c(acco2_his,PAGE_data_C_ssp245_N2O[,target_climate]);
    sea <<- c(sea_his,PAGE_data_S_ssp245_N2O[,target_climate]/100);
  }
}


damage=matrix(nrow=TimeStep,ncol=Region_No);

DamageComponent<-function(){
  Playground_FUND_damage(TimeStep)
  for(t in 1:TimeStep){
    for(r in 1:Region_No){
      damage[t,r]<<-10^9*(eloss[t,r]+sloss[t,r]);
      income[t,r]<<-income[t,r];
    }
  }
}

riskaversion=1;
marginaldamage=matrix(nrow = TimeStep,ncol = Region_No);


MarginalDamageComponent<-function(damage1,damage2,consumption1,consumption2,income1,income2,population_pre,prstp,riskaversion){
  
  marginaldamage <<- damage2 - damage1;
  marginaldamage[1:(simulation_year_index-1),] = 0
  global_consumption_percap <<- rowSums(consumption1)/rowSums(population_pre)/10^6
  
  if(riskaversion==0){
    ds <<- 1/((1+prstp/100)**(year_target-simulation_year))
    ds_ces <<-0**(year_target-simulation_year)
    ds[1:(simulation_year_index-1)] <<- 1
  }else{
    ds <<- 1/((1+prstp/100)^(year_target-year_target[simulation_year_index]))*((global_consumption_percap/global_consumption_percap[simulation_year_index])^(0-riskaversion));
    ds_ces <<- 1/((1+prstp/100)^(year_target-year_target[simulation_year_index]))*((global_consumption_percap)^(0-riskaversion));
    ds[1:(simulation_year_index-1)] <<- 1;
  }
  
  i_aggregatedamage_regions<<-matrix(0,nrow = 1,ncol = Region_No);
  i_aggregatedamage_regions_ces<<-matrix(0,nrow = 1,ncol = Region_No);
  
  for (r in 1:Region_No) {
    i_aggregatedamage_regions[r] <<- sum(marginaldamage[,r]*ds)
    i_aggregatedamage_regions_ces[r] <<- sum(marginaldamage[,r]*ds_ces)
  }
  aggregatedamage_global <<- sum(i_aggregatedamage_regions)/10^7
  aggregatedamage_global_ces <<- sum(i_aggregatedamage_regions_ces)/10^7
}


pop_ssp245 = as.matrix(fread("Module/Socioeconomic/outputs/FUND_data/SSP2/Fund_pop_ssp_process2.csv",header = T,skip = 1))
gdp_ssp245 = as.matrix(fread("Module/Socioeconomic/outputs/FUND_data/SSP2/Fund_gdp_ssp_process2.csv",header = T,skip = 1))
pop_ssp245 = pop_ssp245[,-1]
gdp_ssp245 = gdp_ssp245[,-1]

prtps = c(0.2) # %
etas = c(1.24) 

diagnostic_FUND<-function(run_time){

  model_choose<<-"MonteCarlo"
  target_climate <<- run_time;
  gdp_target <<- gdp_ssp245
  pop_target <<- pop_ssp245
  
  Monte_Carlo_FUND_damage();
  
  MarginalEmission_CO2<<-FALSE
  MarginalEmission_CH4<<-FALSE
  MarginalEmission_N2O<<-FALSE
  Monte_Carlo_T();
  DamageComponent();
  damage_base<<-damage;
  income_base<<-income;
  consumption_base<<-consumption;
  acco2_base <<-acco2;
  s_temp_base <<- s_temp;
  sea_base<<-sea;
  population_pre<<-population;
  
  MarginalEmission_CO2<<-TRUE
  Monte_Carlo_T();
  DamageComponent();
  damage_CO2imp<<-damage;
  income_CO2imp<<-income;
  consumption_CO2imp<<-consumption;
  acco2_CO2imp <<-acco2;
  s_temp_CO2imp <<- s_temp;
  sea_CO2imp<<-sea;
  SC_FUND_CO2 <<- matrix(nrow = 6,ncol = 1)
  MarginalDamageComponent(damage_base,damage_CO2imp,consumption_base,consumption_CO2imp,income_base,income_CO2imp,population_pre,1.5,0);
  SC_FUND_CO2[1]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CO2imp,consumption_base,consumption_CO2imp,income_base,income_CO2imp,population_pre,2,0);
  SC_FUND_CO2[2]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CO2imp,consumption_base,consumption_CO2imp,income_base,income_CO2imp,population_pre,3,0);
  SC_FUND_CO2[3]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CO2imp,consumption_base,consumption_CO2imp,income_base,income_CO2imp,population_pre,5,0);
  SC_FUND_CO2[4]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CO2imp,consumption_base,consumption_CO2imp,income_base,income_CO2imp,population_pre,prtps,etas);
  SC_FUND_CO2[5]<<-c(aggregatedamage_global)/10^4;
  SC_FUND_CO2[6]<<-c(aggregatedamage_global_ces)/10^4;
  
  MarginalEmission_CO2<<-FALSE
  
  MarginalEmission_CH4<<-TRUE
  Monte_Carlo_T();
  DamageComponent();
  damage_CH4imp<<-damage;
  income_CH4imp<<-income;
  consumption_CH4imp<<-consumption;
  acco2_CH4imp <<-acco2;
  s_temp_CH4imp <<- s_temp;
  sea_CH4imp<<-sea;
  SC_FUND_CH4 <<- matrix(nrow = 6,ncol = 1)
  MarginalDamageComponent(damage_base,damage_CH4imp,consumption_base,consumption_CH4imp,income_base,income_CH4imp,population_pre,1.5,0);
  SC_FUND_CH4[1]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CH4imp,consumption_base,consumption_CH4imp,income_base,income_CH4imp,population_pre,2,0);
  SC_FUND_CH4[2]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CH4imp,consumption_base,consumption_CH4imp,income_base,income_CH4imp,population_pre,3,0);
  SC_FUND_CH4[3]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CH4imp,consumption_base,consumption_CH4imp,income_base,income_CH4imp,population_pre,5,0);
  SC_FUND_CH4[4]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_CH4imp,consumption_base,consumption_CH4imp,income_base,income_CH4imp,population_pre,prtps,etas);
  SC_FUND_CH4[5]<<-c(aggregatedamage_global)/10^4;
  SC_FUND_CH4[6]<<-c(aggregatedamage_global_ces)/10^4;
  
  MarginalEmission_CH4<<-FALSE
  
  MarginalEmission_N2O<<-TRUE
  Monte_Carlo_T();
  DamageComponent();
  damage_N2Oimp<<-damage;
  income_N2Oimp<<-income;
  consumption_N2Oimp<<-consumption;
  acco2_N2Oimp <<-acco2;
  s_temp_N2Oimp <<- s_temp;
  sea_N2Oimp<<-sea;
  SC_FUND_N2O <<- matrix(nrow = 6,ncol = 1)
  MarginalDamageComponent(damage_base,damage_N2Oimp,consumption_base,consumption_N2Oimp,income_base,income_N2Oimp,population_pre,1.5,0);
  SC_FUND_N2O[1]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_N2Oimp,consumption_base,consumption_N2Oimp,income_base,income_N2Oimp,population_pre,2,0);
  SC_FUND_N2O[2]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_N2Oimp,consumption_base,consumption_N2Oimp,income_base,income_N2Oimp,population_pre,3,0);
  SC_FUND_N2O[3]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_N2Oimp,consumption_base,consumption_N2Oimp,income_base,income_N2Oimp,population_pre,5,0);
  SC_FUND_N2O[4]<<-c(aggregatedamage_global)/10^4;
  MarginalDamageComponent(damage_base,damage_N2Oimp,consumption_base,consumption_N2Oimp,income_base,income_N2Oimp,population_pre,prtps,etas);
  SC_FUND_N2O[5]<<-c(aggregatedamage_global)/10^4;
  SC_FUND_N2O[6]<<-c(aggregatedamage_global_ces)/10^4;
  MarginalEmission_N2O<<-FALSE
  
  return(list(SC_FUND_CO2,SC_FUND_CH4,SC_FUND_N2O,rowSums(damage_base),rowSums(damage_CO2imp),rowSums(damage_CH4imp),rowSums(damage_N2Oimp),global_consumption_percap));
}

MarginalEmission_CO2<<-FALSE
MarginalEmission_CH4<<-FALSE
MarginalEmission_N2O<<-FALSE

SC_CO2 = matrix(nrow = simulation_no,ncol = 6)
SC_CH4 = matrix(nrow = simulation_no,ncol = 6)
SC_N2O = matrix(nrow = simulation_no,ncol = 6)
Global_damage_base = matrix(nrow = simulation_no,ncol = length(1950:2200))
Global_damage_impulse_CO2 = matrix(nrow = simulation_no,ncol = length(1950:2200))
Global_damage_impulse_CH4 = matrix(nrow = simulation_no,ncol = length(1950:2200))
Global_damage_impulse_N2O = matrix(nrow = simulation_no,ncol = length(1950:2200))
Global_CPC_value = matrix(nrow = simulation_no,ncol = length(1950:2200))

urb_pop_ssp<-read.xlsx("Module/Socioeconomic/outputs/FUND_data/Baseline_data.xlsx",sheet="urban-data",startRow =  2)
urb_pop_ssp=as.matrix(urb_pop_ssp)

AEEI_data_ssp<-read.xlsx("Module/Socioeconomic/outputs/FUND_data/Baseline_data.xlsx",sheet="aeei-data",startRow =  2)
AEEI_data_ssp=as.matrix(AEEI_data_ssp)
  
source("Module/Damage/FUND/FUND_damage.R");
source("Module/Damage/FUND/Monte_Carlo_damage.R");

library(parallel)
system.time({
res1.p = mclapply(1:simulation_no,FUN = function(run_time) {diagnostic_FUND(run_time)},mc.cores = 2);
});

for (run_time in 1:simulation_no) {
  SC_CO2[run_time,] = res1.p[[run_time]][[1]];
  SC_CH4[run_time,] = res1.p[[run_time]][[2]]*100;
  SC_N2O[run_time,] = res1.p[[run_time]][[3]]*1000;
  Global_damage_base[run_time,] = res1.p[[run_time]][[4]];
  Global_damage_impulse_CO2[run_time,] = res1.p[[run_time]][[5]];
  Global_damage_impulse_CH4[run_time,] = res1.p[[run_time]][[6]];
  Global_damage_impulse_N2O[run_time,] = res1.p[[run_time]][[7]];
  Global_CPC_value[run_time,] = res1.p[[run_time]][[8]];
};

SC_CO2[,6] = SC_CO2[,6]*1/mean(Global_CPC_value[,simulation_year-1950+1]^(0-etas))
SC_CH4[,6] = SC_CH4[,6]*1/mean(Global_CPC_value[,simulation_year-1950+1]^(0-etas))
SC_N2O[,6] = SC_N2O[,6]*1/mean(Global_CPC_value[,simulation_year-1950+1]^(0-etas))


write.csv(SC_CO2,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_FUND/ssp245/SC_CO2_PAGE_FUND.csv",quote = F,row.names = F)
write.csv(SC_CH4,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_FUND/ssp245/SC_CH4_PAGE_FUND.csv",quote = F,row.names = F)
write.csv(SC_N2O,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_FUND/ssp245/SC_N2O_PAGE_FUND.csv",quote = F,row.names = F)

write.csv(Global_damage_base,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_FUND/ssp245/Global_damage_base_PAGE_FUND.csv",quote = F,row.names = F)
write.csv(Global_damage_impulse_CO2,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_FUND/ssp245/Global_damage_impulse_CO2_PAGE_FUND.csv",quote = F,row.names = F)
write.csv(Global_damage_impulse_CH4,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_FUND/ssp245/Global_damage_impulse_CH4_PAGE_FUND.csv",quote = F,row.names = F)
write.csv(Global_damage_impulse_N2O,"Compute_SC_GHG/SC_PAGE_climate/SC_PAGE_FUND/ssp245/Global_damage_impulse_N2O_PAGE_FUND.csv",quote = F,row.names = F)




