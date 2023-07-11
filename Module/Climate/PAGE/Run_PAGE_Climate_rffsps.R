# PAGE model climate module
library(data.table)
library(openxlsx)

setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise/Module");

TimeStep_FUND=251;
TimeStep_PAGE=10; #时间跨度
Region_No_PAGE=8;
MarginalEmission_CO2=FALSE
sens_climatesensitivity=3

stimulation_no=10000;
emissions_pulse = 3;
# emissions_pulse = 6;

PAGE_temp_base=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_temp_anomaly_CO2=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_temp_anomaly_CH4=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_temp_anomaly_N2O=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_temp_base[,1] = 0:(TimeStep_FUND-1)
PAGE_temp_anomaly_CO2[,1] = 0:(TimeStep_FUND-1)
PAGE_temp_anomaly_CH4[,1] = 0:(TimeStep_FUND-1)
PAGE_temp_anomaly_N2O[,1] = 0:(TimeStep_FUND-1)

PAGE_slr_base=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_slr_anomaly_CO2=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_slr_anomaly_CH4=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_slr_anomaly_N2O=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_slr_base[,1] = 0:(TimeStep_FUND-1)
PAGE_slr_anomaly_CO2[,1] = 0:(TimeStep_FUND-1)
PAGE_slr_anomaly_CH4[,1] = 0:(TimeStep_FUND-1)
PAGE_slr_anomaly_N2O[,1] = 0:(TimeStep_FUND-1)

PAGE_acco2_base=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_acco2_anomaly_CO2=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_acco2_anomaly_CH4=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_acco2_anomaly_N2O=matrix(nrow = TimeStep_FUND, ncol = stimulation_no+1 );
PAGE_acco2_base[,1] = 0:(TimeStep_FUND-1)
PAGE_acco2_anomaly_CO2[,1] = 0:(TimeStep_FUND-1)
PAGE_acco2_anomaly_CH4[,1] = 0:(TimeStep_FUND-1)
PAGE_acco2_anomaly_N2O[,1] = 0:(TimeStep_FUND-1)

MarginalEmission_CO2=FALSE
MarginalEmission_CH4=FALSE
MarginalEmission_N2O=FALSE
MarginalEmission_SF6=FALSE


GHGem<-read.csv("Climate/PAGE/rffsps/emissions_rffsps_template.csv",header = T,skip = 3);
GHGem=as.matrix(GHGem);
LGem <- read.csv("Climate/PAGE/rffsps/LG_emissions_ssp245.csv");
LGem=LGem$ssp245;
other_gases_forcing<-read.csv("Climate/PAGE/rffsps/otherforcing_ssp245.csv");
other_gases_forcing<-other_gases_forcing$ssp245

model_choose="MonteCarlo"
source("Climate/PAGE/PAGE_Climate.R");
source("Climate/PAGE/Monte_Carlo_climate.R");

ECS_distribution<-read.csv("Sample/ecs_sample.csv",header=T);
scenario_sample<-read.csv("Sample/climate_scenario_sample.csv",header=T);

rffsp_co2_emissions = fread("./rffsps_v5/emissions/rffsp_co2_emissions.csv",header = T)
rffsp_ch4_emissions = fread("./rffsps_v5/emissions/rffsp_ch4_emissions.csv",header = T)
rffsp_n2o_emissions = fread("./rffsps_v5/emissions/rffsp_n2o_emissions.csv",header = T)

for (i in 1:stimulation_no) {
    print(i)
    ecs_num = i;
    sample_num = scenario_sample$x[i];
    GHGem[(2020-1765+1):(2300-1765+1),2] = rffsp_co2_emissions[rffsp_co2_emissions$sample==sample_num,]$value - GHGem[(2020-1765+1):(2300-1765+1),3]
    GHGem[(2020-1765+1):(2300-1765+1),4] = rffsp_ch4_emissions[rffsp_ch4_emissions$sample==sample_num,]$value
    GHGem[(2020-1765+1):(2300-1765+1),5] = rffsp_n2o_emissions[rffsp_n2o_emissions$sample==sample_num,]$value
    
    model_choose="MonteCarlo"
    Monte_Carlo();
    MarginalEmission_CO2=FALSE
    MarginalEmission_CH4=FALSE
    MarginalEmission_N2O=FALSE
    MarginalEmission_SF6=FALSE
    playground_page_climate()
    PAGE_temp_base[,i+1]=rt_g_globaltemperature_fund;
    PAGE_slr_base[,i+1]=sea_fund;
    PAGE_acco2_base[,i+1]=acco2_fund;

    MarginalEmission_CO2=TRUE
    playground_page_climate()
    PAGE_temp_anomaly_CO2[,i+1]=rt_g_globaltemperature_fund;
    PAGE_slr_anomaly_CO2[,i+1]=sea_fund;
    PAGE_acco2_anomaly_CO2[,i+1]=acco2_fund;

    MarginalEmission_CO2=FALSE
    MarginalEmission_CH4=TRUE
    playground_page_climate()
    PAGE_temp_anomaly_CH4[,i+1]=rt_g_globaltemperature_fund;
    PAGE_slr_anomaly_CH4[,i+1]=sea_fund;
    PAGE_acco2_anomaly_CH4[,i+1]=acco2_fund;
    
    MarginalEmission_CH4=FALSE
    MarginalEmission_N2O=TRUE
    playground_page_climate()
    PAGE_temp_anomaly_N2O[,i+1]=rt_g_globaltemperature_fund;
    PAGE_slr_anomaly_N2O[,i+1]=sea_fund;
    PAGE_acco2_anomaly_N2O[,i+1]=acco2_fund;
    
    PAGE_temp_anomaly_CO2[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_temp_base[1:(y_year[emissions_pulse]-1950),i+1]
    PAGE_temp_anomaly_CH4[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_temp_base[1:(y_year[emissions_pulse]-1950),i+1]
    PAGE_temp_anomaly_N2O[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_temp_base[1:(y_year[emissions_pulse]-1950),i+1]
    
    PAGE_slr_anomaly_CO2[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_slr_base[1:(y_year[emissions_pulse]-1950),i+1]
    PAGE_slr_anomaly_CH4[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_slr_base[1:(y_year[emissions_pulse]-1950),i+1]
    PAGE_slr_anomaly_N2O[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_slr_base[1:(y_year[emissions_pulse]-1950),i+1]
    
    PAGE_acco2_anomaly_CO2[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_acco2_base[1:(y_year[emissions_pulse]-1950),i+1]
    PAGE_acco2_anomaly_CH4[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_acco2_base[1:(y_year[emissions_pulse]-1950),i+1]
    PAGE_acco2_anomaly_N2O[1:(y_year[emissions_pulse]-1950),i+1] = PAGE_acco2_base[1:(y_year[emissions_pulse]-1950),i+1]
}

write.csv(PAGE_temp_base,"Climate/PAGE/outputs/rffsps/T_base.csv",quote = F,row.names = F)
write.csv(PAGE_temp_anomaly_CO2,"Climate/PAGE/outputs/rffsps/T_extra_CO2.csv",quote = F,row.names = F)
write.csv(PAGE_temp_anomaly_CH4,"Climate/PAGE/outputs/rffsps/T_extra_CH4.csv",quote = F,row.names = F)
write.csv(PAGE_temp_anomaly_N2O,"Climate/PAGE/outputs/rffsps/T_extra_N2O.csv",quote = F,row.names = F)

write.csv(PAGE_slr_base,"Climate/PAGE/outputs/rffsps/S_base.csv",quote = F,row.names = F)
write.csv(PAGE_slr_anomaly_CO2,"Climate/PAGE/outputs/rffsps/S_extra_CO2.csv",quote = F,row.names = F)
write.csv(PAGE_slr_anomaly_CH4,"Climate/PAGE/outputs/rffsps/S_extra_CH4.csv",quote = F,row.names = F)
write.csv(PAGE_slr_anomaly_N2O,"Climate/PAGE/outputs/rffsps/S_extra_N2O.csv",quote = F,row.names = F)

write.csv(PAGE_acco2_base,"Climate/PAGE/outputs/rffsps/C_base.csv",quote = F,row.names = F)
write.csv(PAGE_acco2_anomaly_CO2,"Climate/PAGE/outputs/rffsps/C_extra_CO2.csv",quote = F,row.names = F)
write.csv(PAGE_acco2_anomaly_CH4,"Climate/PAGE/outputs/rffsps/C_extra_CH4.csv",quote = F,row.names = F)
write.csv(PAGE_acco2_anomaly_N2O,"Climate/PAGE/outputs/rffsps/C_extra_N2O.csv",quote = F,row.names = F)
