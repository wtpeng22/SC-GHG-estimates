## MAGICC_Howard model
setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise");
simulation_no = 10000

scenario_sample<-read.csv("Module/Sample/climate_scenario_sample.csv",header=T);

model_choose ="MonteCarlo"
t2xco2=1
Time_periods=38;
Time_seq = 1:Time_periods
year_target = seq(2015,2200,5);

library(data.table)

### data from the MAGICC model
MAGICC_row_target = year_target-1765;

MAGICC_data_T_ssp245_base = fread("./Module/Climate/MAGICC/outputs/ssp245/T_base.csv",encoding = "UTF-8",header = T);
MAGICC_data_T_ssp245_base = as.matrix(MAGICC_data_T_ssp245_base[V1 %in% MAGICC_row_target,-1]);

MAGICC_data_T_ssp245_CO2 = fread("./Module/Climate/MAGICC/outputs/ssp245/T_extra_CO2.csv",encoding = "UTF-8",header = T);
MAGICC_data_T_ssp245_CO2 = as.matrix(MAGICC_data_T_ssp245_CO2[V1 %in% MAGICC_row_target,-1]);

MAGICC_data_T_ssp245_CH4 = fread("./Module/Climate/MAGICC/outputs/ssp245/T_extra_CH4.csv",encoding = "UTF-8",header = T);
MAGICC_data_T_ssp245_CH4 = as.matrix(MAGICC_data_T_ssp245_CH4[V1 %in% MAGICC_row_target,-1]);

MAGICC_data_T_ssp245_N2O = fread("./Module/Climate/MAGICC/outputs/ssp245/T_extra_N2O.csv",encoding = "UTF-8",header = T);
MAGICC_data_T_ssp245_N2O = as.matrix(MAGICC_data_T_ssp245_N2O[V1 %in% MAGICC_row_target,-1]);

Monte_Carlo_T <-function(){
  # 
  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    Howard_temp <<- MAGICC_data_T_ssp245_base[,sample_inHoward];
  }
  
  if(MarginalEmission_CO2 == TRUE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    Howard_temp <<- MAGICC_data_T_ssp245_CO2[,sample_inHoward];
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == TRUE & MarginalEmission_N2O == FALSE){
    Howard_temp <<- MAGICC_data_T_ssp245_CH4[,sample_inHoward];
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == TRUE){
    Howard_temp <<- MAGICC_data_T_ssp245_N2O[,sample_inHoward];
  }
}

diagnostic_Howard <- function(gases_target,discount_type,prstp,elasmu){

  MarginalEmission_CO2<<-FALSE;
  MarginalEmission_CH4<<-FALSE;
  MarginalEmission_N2O<<-FALSE;
    
  Monte_Carlo_T();
  Playground_model();
  Consumption_base <<- C;
  TATM_base <<- TATM;
  Global_damage <<- DAMAGES;

  if(gases_target == "CO2"){   
    MarginalEmission_CO2<<-TRUE
    Monte_Carlo_T();
    Playground_model();
    Consumption_pulse <<- C;
    TATM_pulse <<- TATM;
    Global_damage_pulse <<- DAMAGES;
    MarginalEmission_CO2<<-FALSE
  }
    
  if(gases_target == "CH4"){   
      MarginalEmission_CH4<<-TRUE
      Monte_Carlo_T();
      Playground_model();
      Consumption_pulse <<- C;
      TATM_pulse <<- TATM;
      Global_damage_pulse <<- DAMAGES;
      MarginalEmission_CH4<<-FALSE
  }
  
  if(gases_target == "N2O"){   
      MarginalEmission_N2O<<-TRUE
      Monte_Carlo_T();
      Playground_model();
      Consumption_pulse <<- C;
      TATM_pulse <<- TATM;
      Global_damage_pulse <<- DAMAGES;
      MarginalEmission_N2O<<-FALSE
  }
    
  if(discount_type == "fixed"){
    rr <<- 1/((1+prstp)**(tstep*(Time_seq-simulation_year)))
    rr[1:(simulation_year-1)] <<- 1
  }
  if(discount_type == "timevarying"){
    rr <<- 1/((1+prstp)^(year_target-year_target[simulation_year]))*(((Consumption_base/l)/(Consumption_base[simulation_year]/l[simulation_year]))^(0-elasmu))
    rr[1:(simulation_year-1)] <<- 1
  }
  if(discount_type == "timevarying_ces"){
    rr <<- 1/((1+prstp)^(year_target-year_target[simulation_year]))*(((Consumption_base/l))^(0-elasmu))
    rr[1:(simulation_year-1)] <<- 1
  }
  
  SC_Howard_total <<- sum((Global_damage_pulse - Global_damage)*rr)*5*10
  CPC_value <<- Consumption_base/l
}

MarginalEmission_CO2<-FALSE
MarginalEmission_CH4<-FALSE
MarginalEmission_N2O<-FALSE

SC_CO2_total = matrix(nrow = simulation_no,ncol = 6)
SC_CH4_total = matrix(nrow = simulation_no,ncol = 6)
SC_N2O_total = matrix(nrow = simulation_no,ncol = 6)

Global_Damage_base = matrix(nrow = simulation_no,ncol = Time_periods)
Global_Damage_impulse_CO2 = matrix(nrow = simulation_no,ncol = Time_periods)
Global_Damage_impulse_CH4 = matrix(nrow = simulation_no,ncol = Time_periods)
Global_Damage_impulse_N2O = matrix(nrow = simulation_no,ncol = Time_periods)

Global_CPC_base = matrix(nrow = simulation_no,ncol = Time_periods)

# Howard_pop_data<-as.matrix(fread("Module/Socioeconomic/outputs/DICE_data/ssp245_global_pop.csv",header = TRUE))
# Howard_gdp_data<-as.matrix(fread("Module/Socioeconomic/outputs/DICE_data/ssp245_global_gdp.csv",header = TRUE))

Howard_data = read.csv("Module/Damage/Howard/Howard_ssp245.csv")
source("Module/Damage/Howard/Howard_damage.R");

eta = 1.24
prtp = 0.002

for (j in 1:simulation_no) {
    print(j)
    sample_inHoward = j;
    target_sample <- scenario_sample$x[sample_inHoward];
    
    # Howard_data[2,-(1:2)] =  Howard_pop_data[,target_sample+1]
    # Howard_data[3,-(1:2)] =  Howard_gdp_data[,target_sample+1]
  
    diagnostic_Howard("CO2","fixed",0.015,0);
    SC_CO2_total[j,1] = SC_Howard_total
    Global_Damage_base[j,] = Global_damage;
    Global_Damage_impulse_CO2[j,] = Global_damage_pulse;
    diagnostic_Howard("CO2","fixed",0.02,0);
    SC_CO2_total[j,2] = SC_Howard_total
    diagnostic_Howard("CO2","fixed",0.03,0);
    SC_CO2_total[j,3] = SC_Howard_total
    diagnostic_Howard("CO2","fixed",0.05,0);
    SC_CO2_total[j,4] = SC_Howard_total
    diagnostic_Howard("CO2","timevarying",prtp,eta);
    SC_CO2_total[j,5] = SC_Howard_total
    diagnostic_Howard("CO2","timevarying_ces",prtp,eta);
    SC_CO2_total[j,6] = SC_Howard_total
    
    diagnostic_Howard("CH4","fixed",0.015,0);
    SC_CH4_total[j,1] = SC_Howard_total*100
    Global_Damage_impulse_CH4[j,] = Global_damage_pulse;
    diagnostic_Howard("CH4","fixed",0.02,0);
    SC_CH4_total[j,2] = SC_Howard_total*100
    diagnostic_Howard("CH4","fixed",0.03,0);
    SC_CH4_total[j,3] = SC_Howard_total*100
    diagnostic_Howard("CH4","fixed",0.05,0);
    SC_CH4_total[j,4] = SC_Howard_total*100
    diagnostic_Howard("CH4","timevarying",prtp,eta);
    SC_CH4_total[j,5] = SC_Howard_total*100
    diagnostic_Howard("CH4","timevarying_ces",prtp,eta);
    SC_CH4_total[j,6] = SC_Howard_total*100

    diagnostic_Howard("N2O","fixed",0.015,0);
    SC_N2O_total[j,1] = SC_Howard_total*1000
    Global_Damage_impulse_N2O[j,] = Global_damage_pulse;
    diagnostic_Howard("N2O","fixed",0.02,0);
    SC_N2O_total[j,2] = SC_Howard_total*1000
    diagnostic_Howard("N2O","fixed",0.03,0);
    SC_N2O_total[j,3] = SC_Howard_total*1000
    diagnostic_Howard("N2O","fixed",0.05,0);
    SC_N2O_total[j,4] = SC_Howard_total*1000
    diagnostic_Howard("N2O","timevarying",prtp,eta);
    SC_N2O_total[j,5] = SC_Howard_total*1000
    diagnostic_Howard("N2O","timevarying_ces",prtp,eta);
    SC_N2O_total[j,6] = SC_Howard_total*1000
    
    Global_CPC_base[j,] = CPC_value
}

SC_CO2_total[,6] = SC_CO2_total[,6]*1/mean(Global_CPC_base[,simulation_year]^(0-eta))
SC_CH4_total[,6] = SC_CH4_total[,6]*1/mean(Global_CPC_base[,simulation_year]^(0-eta))
SC_N2O_total[,6] = SC_N2O_total[,6]*1/mean(Global_CPC_base[,simulation_year]^(0-eta))

write.csv(SC_CO2_total,"Compute_SC_GHG/SC_MAGICC_climate/SC_MAGICC_Howard/ssp245/SC_CO2_total_MAGICC_Howard.csv",quote = F,row.names = F)
write.csv(SC_CH4_total,"Compute_SC_GHG/SC_MAGICC_climate/SC_MAGICC_Howard/ssp245/SC_CH4_total_MAGICC_Howard.csv",quote = F,row.names = F)
write.csv(SC_N2O_total,"Compute_SC_GHG/SC_MAGICC_climate/SC_MAGICC_Howard/ssp245/SC_N2O_total_MAGICC_Howard.csv",quote = F,row.names = F)

write.csv(Global_Damage_base,"Compute_SC_GHG/SC_MAGICC_climate/SC_MAGICC_Howard/ssp245/Global_Damage_base_MAGICC_Howard.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_CO2,"Compute_SC_GHG/SC_MAGICC_climate/SC_MAGICC_Howard/ssp245/Global_Damage_impulse_CO2_MAGICC_Howard.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_CH4,"Compute_SC_GHG/SC_MAGICC_climate/SC_MAGICC_Howard/ssp245/Global_Damage_impulse_CH4_MAGICC_Howard.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_N2O,"Compute_SC_GHG/SC_MAGICC_climate/SC_MAGICC_Howard/ssp245/Global_Damage_impulse_N2O_MAGICC_Howard.csv",quote = F,row.names = F)


