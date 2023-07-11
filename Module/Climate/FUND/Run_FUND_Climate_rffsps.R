# FUND model climate module
library(data.table)
setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise/Module");

MarginalEmission_CO2=FALSE
MarginalEmission_CH4=FALSE
MarginalEmission_N2O=FALSE
MarginalEmission_SF6=FALSE

TimeStep=351
stimulation_no=10000;

FUND_temp_base=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_temp_anomaly_CO2=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_temp_anomaly_CH4=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_temp_anomaly_N2O=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_temp_base[,1] = 0:(TimeStep-1)
FUND_temp_anomaly_CO2[,1] = 0:(TimeStep-1)
FUND_temp_anomaly_CH4[,1] = 0:(TimeStep-1)
FUND_temp_anomaly_N2O[,1] = 0:(TimeStep-1)

FUND_slr_base=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_slr_anomaly_CO2=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_slr_anomaly_CH4=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_slr_anomaly_N2O=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_slr_base[,1] = 0:(TimeStep-1)
FUND_slr_anomaly_CO2[,1] = 0:(TimeStep-1)
FUND_slr_anomaly_CH4[,1] = 0:(TimeStep-1)
FUND_slr_anomaly_N2O[,1] = 0:(TimeStep-1)

FUND_acco2_base=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_acco2_anomaly_CO2=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_acco2_anomaly_CH4=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_acco2_anomaly_N2O=matrix(nrow = TimeStep, ncol = stimulation_no+1 );
FUND_acco2_base[,1] = 0:(TimeStep-1)
FUND_acco2_anomaly_CO2[,1] = 0:(TimeStep-1)
FUND_acco2_anomaly_CH4[,1] = 0:(TimeStep-1)
FUND_acco2_anomaly_N2O[,1] = 0:(TimeStep-1)

emissionperiod=71;
Region_No=16
library(openxlsx)

emission=matrix(nrow = TimeStep,ncol = Region_No)
forestemm=matrix(nrow = TimeStep,ncol = Region_No)
ch4=matrix(nrow = TimeStep,ncol = Region_No)
n2o=matrix(nrow = TimeStep,ncol = Region_No)
sf6=matrix(nrow = TimeStep,ncol = Region_No)


model_choose="MonteCarlo"
  
GHGem<-read.csv("Climate/FUND/rffsps/emissions_rffsps_template.csv",header = T,skip = 3);
rfso2<-read.csv("Climate/FUND/rffsps/rfso2_ssp245.csv",header = T);
  
GHGem_base<-GHGem
rfso2_base<-rfso2
  
source("Climate/FUND/FUND_Climate.R");
ECS_distribution<-read.csv("Sample/ecs_sample.csv",header=T);
scenario_sample<-read.csv("Sample/climate_scenario_sample.csv",header=T);

rffsp_co2_emissions = fread("./rffsps_v5/emissions/rffsp_co2_emissions.csv",header = T)
rffsp_ch4_emissions = fread("./rffsps_v5/emissions/rffsp_ch4_emissions.csv",header = T)
rffsp_n2o_emissions = fread("./rffsps_v5/emissions/rffsp_n2o_emissions.csv",header = T)

for (i in 1:stimulation_no) {
    print(i)
    ecs_num = i;
    sample_num = scenario_sample$x[i];
    GHGem$ffi_emissions[(2020-1765+1):(2300-1765+1)] = rffsp_co2_emissions[rffsp_co2_emissions$sample==sample_num,]$value - GHGem$luc_emissions[(2020-1765+1):(2300-1765+1)]
    GHGem$CH4_emissions[(2020-1765+1):(2300-1765+1)] = rffsp_ch4_emissions[rffsp_ch4_emissions$sample==sample_num,]$value
    GHGem$N2O_emissions[(2020-1765+1):(2300-1765+1)] = rffsp_n2o_emissions[rffsp_n2o_emissions$sample==sample_num,]$value
    
    model_choose="MonteCarlo"
    Monte_carlo_fund_climate();
    MarginalEmission_CO2=FALSE
    MarginalEmission_CH4=FALSE
    MarginalEmission_N2O=FALSE
    MarginalEmission_SF6=FALSE
    playground_fund_climate();
    s_temp[is.nan(s_temp)] = 0
    sea[is.nan(sea)] = 0
    acco2[is.nan(acco2)] = 0
    FUND_temp_base[,i+1]=s_temp;
    FUND_slr_base[,i+1]=sea;
    FUND_acco2_base[,i+1]=acco2;
    
    MarginalEmission_CO2=TRUE;
    playground_fund_climate();
    s_temp[is.nan(s_temp)] = 0
    sea[is.nan(sea)] = 0
    acco2[is.nan(acco2)] = 0
    FUND_temp_anomaly_CO2[,i+1]=s_temp;
    FUND_slr_anomaly_CO2[,i+1]=sea;
    FUND_acco2_anomaly_CO2[,i+1]=acco2;
    
    MarginalEmission_CO2=FALSE;
    MarginalEmission_CH4=TRUE;
    playground_fund_climate();
    s_temp[is.nan(s_temp)] = 0
    sea[is.nan(sea)] = 0
    acco2[is.nan(acco2)] = 0
    FUND_temp_anomaly_CH4[,i+1]=s_temp;
    FUND_slr_anomaly_CH4[,i+1]=sea;
    FUND_acco2_anomaly_CH4[,i+1]=acco2;
    #
    MarginalEmission_CH4=FALSE;
    MarginalEmission_N2O=TRUE;
    playground_fund_climate();
    s_temp[is.nan(s_temp)] = 0
    sea[is.nan(sea)] = 0
    acco2[is.nan(acco2)] = 0
    FUND_temp_anomaly_N2O[,i+1]=s_temp;
    FUND_slr_anomaly_N2O[,i+1]=sea;
    FUND_acco2_anomaly_N2O[,i+1]=acco2;
}

write.csv(FUND_temp_base,"Climate/FUND/outputs/rffsps/T_base.csv",quote = F,row.names = F)
write.csv(FUND_temp_anomaly_CO2,"Climate/FUND/outputs/rffsps/T_extra_CO2.csv",quote = F,row.names = F)
write.csv(FUND_temp_anomaly_CH4,"Climate/FUND/outputs/rffsps/T_extra_CH4.csv",quote = F,row.names = F)
write.csv(FUND_temp_anomaly_N2O,"Climate/FUND/outputs/rffsps/T_extra_N2O.csv",quote = F,row.names = F)

write.csv(FUND_slr_base,"Climate/FUND/outputs/rffsps/S_base.csv",quote = F,row.names = F)
write.csv(FUND_slr_anomaly_CO2,"Climate/FUND/outputs/rffsps/S_extra_CO2.csv",quote = F,row.names = F)
write.csv(FUND_slr_anomaly_CH4,"Climate/FUND/outputs/rffsps/S_extra_CH4.csv",quote = F,row.names = F)
write.csv(FUND_slr_anomaly_N2O,"Climate/FUND/outputs/rffsps/S_extra_N2O.csv",quote = F,row.names = F)

write.csv(FUND_acco2_base,"Climate/FUND/outputs/rffsps/C_base.csv",quote = F,row.names = F)
write.csv(FUND_acco2_anomaly_CO2,"Climate/FUND/outputs/rffsps/C_extra_CO2.csv",quote = F,row.names = F)
write.csv(FUND_acco2_anomaly_CH4,"Climate/FUND/outputs/rffsps/C_extra_CH4.csv",quote = F,row.names = F)
write.csv(FUND_acco2_anomaly_N2O,"Climate/FUND/outputs/rffsps/C_extra_N2O.csv",quote = F,row.names = F)
