## Hector_DICE model
setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise");
simulation_no = 10000

scenario_sample<-read.csv("Module/Sample/climate_scenario_sample.csv",header=T);

model_choose ="MonteCarlo"
t2xco2=1
Time_periods=38;
Time_seq = 1:Time_periods
year_target = seq(2015,2200,5);

library(data.table)

### data from the Hector model
Hector_row_target = year_target-2000;

Hector_data_T_ssp245_base = fread("./Module/Climate/Hector/outputs/ssp245/T_base.csv",encoding = "UTF-8",header = T);
Hector_data_T_ssp245_base = as.matrix(Hector_data_T_ssp245_base[V1 %in% Hector_row_target,-1]);

Hector_data_T_ssp245_CO2 = fread("./Module/Climate/Hector/outputs/ssp245/T_extra_CO2.csv",encoding = "UTF-8",header = T);
Hector_data_T_ssp245_CO2 = as.matrix(Hector_data_T_ssp245_CO2[V1 %in% Hector_row_target,-1]);

Hector_data_T_ssp245_CH4 = fread("./Module/Climate/Hector/outputs/ssp245/T_extra_CH4.csv",encoding = "UTF-8",header = T);
Hector_data_T_ssp245_CH4 = as.matrix(Hector_data_T_ssp245_CH4[V1 %in% Hector_row_target,-1]);

Hector_data_T_ssp245_N2O = fread("./Module/Climate/Hector/outputs/ssp245/T_extra_N2O.csv",encoding = "UTF-8",header = T);
Hector_data_T_ssp245_N2O = as.matrix(Hector_data_T_ssp245_N2O[V1 %in% Hector_row_target,-1]);

Monte_Carlo_T <-function(){
  # 
  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    DICE_temp <<- Hector_data_T_ssp245_base[,sample_indice];
  }
  
  if(MarginalEmission_CO2 == TRUE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == FALSE){
    DICE_temp <<- Hector_data_T_ssp245_CO2[,sample_indice];
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == TRUE & MarginalEmission_N2O == FALSE){
    DICE_temp <<- Hector_data_T_ssp245_CH4[,sample_indice];
  }

  if(MarginalEmission_CO2 == FALSE & MarginalEmission_CH4 == FALSE & MarginalEmission_N2O == TRUE){
    DICE_temp <<- Hector_data_T_ssp245_N2O[,sample_indice];
  }
}

diagnostic_DICE <- function(gases_target,discount_type,prstp,elasmu){

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
  
  SC_DICE_total <<- sum((Global_damage_pulse - Global_damage)*rr)*5*10
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

# Dice_pop_data<-as.matrix(fread("Module/Socioeconomic/outputs/DICE_data/ssp245_global_pop.csv",header = TRUE))
# Dice_gdp_data<-as.matrix(fread("Module/Socioeconomic/outputs/DICE_data/ssp245_global_gdp.csv",header = TRUE))

DICE_data = read.csv("Module/Damage/DICE/DICE_ssp245.csv")
source("Module/Damage/DICE/DICE_damage.R");

eta = 1.24
prtp = 0.002

for (j in 1:simulation_no) {
    print(j)
    sample_indice = j;
    target_sample <- scenario_sample$x[sample_indice];
    
    # DICE_data[2,-(1:2)] =  Dice_pop_data[,target_sample+1]
    # DICE_data[3,-(1:2)] =  Dice_gdp_data[,target_sample+1]
  
    diagnostic_DICE("CO2","fixed",0.015,0);
    SC_CO2_total[j,1] = SC_DICE_total
    Global_Damage_base[j,] = Global_damage;
    Global_Damage_impulse_CO2[j,] = Global_damage_pulse;
    diagnostic_DICE("CO2","fixed",0.02,0);
    SC_CO2_total[j,2] = SC_DICE_total
    diagnostic_DICE("CO2","fixed",0.03,0);
    SC_CO2_total[j,3] = SC_DICE_total
    diagnostic_DICE("CO2","fixed",0.05,0);
    SC_CO2_total[j,4] = SC_DICE_total
    diagnostic_DICE("CO2","timevarying",prtp,eta);
    SC_CO2_total[j,5] = SC_DICE_total
    diagnostic_DICE("CO2","timevarying_ces",prtp,eta);
    SC_CO2_total[j,6] = SC_DICE_total
    
    diagnostic_DICE("CH4","fixed",0.015,0);
    SC_CH4_total[j,1] = SC_DICE_total*100
    Global_Damage_impulse_CH4[j,] = Global_damage_pulse;
    diagnostic_DICE("CH4","fixed",0.02,0);
    SC_CH4_total[j,2] = SC_DICE_total*100
    diagnostic_DICE("CH4","fixed",0.03,0);
    SC_CH4_total[j,3] = SC_DICE_total*100
    diagnostic_DICE("CH4","fixed",0.05,0);
    SC_CH4_total[j,4] = SC_DICE_total*100
    diagnostic_DICE("CH4","timevarying",prtp,eta);
    SC_CH4_total[j,5] = SC_DICE_total*100
    diagnostic_DICE("CH4","timevarying_ces",prtp,eta);
    SC_CH4_total[j,6] = SC_DICE_total*100

    diagnostic_DICE("N2O","fixed",0.015,0);
    SC_N2O_total[j,1] = SC_DICE_total*1000
    Global_Damage_impulse_N2O[j,] = Global_damage_pulse;
    diagnostic_DICE("N2O","fixed",0.02,0);
    SC_N2O_total[j,2] = SC_DICE_total*1000
    diagnostic_DICE("N2O","fixed",0.03,0);
    SC_N2O_total[j,3] = SC_DICE_total*1000
    diagnostic_DICE("N2O","fixed",0.05,0);
    SC_N2O_total[j,4] = SC_DICE_total*1000
    diagnostic_DICE("N2O","timevarying",prtp,eta);
    SC_N2O_total[j,5] = SC_DICE_total*1000
    diagnostic_DICE("N2O","timevarying_ces",prtp,eta);
    SC_N2O_total[j,6] = SC_DICE_total*1000
    
    Global_CPC_base[j,] = CPC_value
}

SC_CO2_total[,6] = SC_CO2_total[,6]*1/mean(Global_CPC_base[,simulation_year]^(0-eta))
SC_CH4_total[,6] = SC_CH4_total[,6]*1/mean(Global_CPC_base[,simulation_year]^(0-eta))
SC_N2O_total[,6] = SC_N2O_total[,6]*1/mean(Global_CPC_base[,simulation_year]^(0-eta))

write.csv(SC_CO2_total,"Compute_SC_GHG/SC_Hector_climate/SC_Hector_DICE/ssp245/SC_CO2_total_Hector_DICE.csv",quote = F,row.names = F)
write.csv(SC_CH4_total,"Compute_SC_GHG/SC_Hector_climate/SC_Hector_DICE/ssp245/SC_CH4_total_Hector_DICE.csv",quote = F,row.names = F)
write.csv(SC_N2O_total,"Compute_SC_GHG/SC_Hector_climate/SC_Hector_DICE/ssp245/SC_N2O_total_Hector_DICE.csv",quote = F,row.names = F)

write.csv(Global_Damage_base,"Compute_SC_GHG/SC_Hector_climate/SC_Hector_DICE/ssp245/Global_Damage_base_Hector_DICE.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_CO2,"Compute_SC_GHG/SC_Hector_climate/SC_Hector_DICE/ssp245/Global_Damage_impulse_CO2_Hector_DICE.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_CH4,"Compute_SC_GHG/SC_Hector_climate/SC_Hector_DICE/ssp245/Global_Damage_impulse_CH4_Hector_DICE.csv",quote = F,row.names = F)
write.csv(Global_Damage_impulse_N2O,"Compute_SC_GHG/SC_Hector_climate/SC_Hector_DICE/ssp245/Global_Damage_impulse_N2O_Hector_DICE.csv",quote = F,row.names = F)



