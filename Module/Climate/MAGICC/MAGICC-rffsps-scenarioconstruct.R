library(dplyr)
library(data.table)
library(openxlsx)
library(arrow)

setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise")

MAGICC_SSP245_template = read.csv("Module/Climate/MAGICC/ssp2/MAGICC_ssp245.csv",header = F)
MAGICC_SSP245_world = read.xlsx("Module/Climate/MAGICC/MAGICC_data_process.xlsx",sheet =  "SSP2-45-global",startRow = 1)
MAGICC_SSP245_world_sample = MAGICC_SSP245_world
MAGICC_SSP245_weight = read.xlsx("Module/Climate/MAGICC/MAGICC_data_process.xlsx",sheet =  "SSP2-45-region-weight",startRow = 1)
MAGICC_SSP245_weight = MAGICC_SSP245_weight[,-(1:7)]
rffsp_co2_emissions = fread("Module/rffsps_v5/emissions/rffsp_co2_emissions.csv",header = T)
rffsp_ch4_emissions = fread("Module/rffsps_v5/emissions/rffsp_ch4_emissions.csv",header = T)
rffsp_n2o_emissions = fread("Module/rffsps_v5/emissions/rffsp_n2o_emissions.csv",header = T)

sample_no = 10000;

for (i in 1:sample_no) {
  print(i)
  rffsp_co2_emissions_sample = rffsp_co2_emissions[rffsp_co2_emissions$sample==i,]
  rffsp_ch4_emissions_sample = rffsp_ch4_emissions[rffsp_ch4_emissions$sample==i,]
  rffsp_n2o_emissions_sample = rffsp_n2o_emissions[rffsp_n2o_emissions$sample==i,]
  
  rffsp_co2_emissions_sample_targetyear = rffsp_co2_emissions_sample[rffsp_co2_emissions_sample$year %in% MAGICC_SSP245_world$year,]
  rffsp_ch4_emissions_sample_targetyear = rffsp_ch4_emissions_sample[rffsp_ch4_emissions_sample$year %in% MAGICC_SSP245_world$year,]
  rffsp_n2o_emissions_sample_targetyear = rffsp_n2o_emissions_sample[rffsp_n2o_emissions_sample$year %in% MAGICC_SSP245_world$year,]
  
  MAGICC_SSP245_world_sample$`Emissions|CO2|MAGICC.Fossil.and.Industrial`[10:45] = rffsp_co2_emissions_sample_targetyear$value - MAGICC_SSP245_world_sample$`Emissions|CO2|MAGICC.AFOLU`[10:45]
  MAGICC_SSP245_world_sample$`Emissions|CH4`[10:45] = rffsp_ch4_emissions_sample_targetyear$value
  MAGICC_SSP245_world_sample$`Emissions|N2O`[10:45] = rffsp_n2o_emissions_sample_targetyear$value
  
  MAGICC_SSP245_world_sample$`Emissions|CO2|MAGICC.Fossil.and.Industrial`[46:49] = MAGICC_SSP245_world_sample$`Emissions|CO2|MAGICC.Fossil.and.Industrial`[45]
  MAGICC_SSP245_world_sample$`Emissions|CH4`[46:49] = MAGICC_SSP245_world_sample$`Emissions|CH4`[45]
  MAGICC_SSP245_world_sample$`Emissions|N2O`[46:49] = MAGICC_SSP245_world_sample$`Emissions|N2O`[45]
  
  MAGICC_SSP245_global_template_sample = rbind(t(MAGICC_SSP245_world_sample[,-1]),
                                        t(MAGICC_SSP245_world_sample[,-1]),
                                        t(MAGICC_SSP245_world_sample[,-1]),
                                        t(MAGICC_SSP245_world_sample[,-1]),
                                        t(MAGICC_SSP245_world_sample[,-1]),
                                        t(MAGICC_SSP245_world_sample[,-1]),
                                        t(MAGICC_SSP245_world_sample[,-1]))
  MAGICC_SSP245_template_sample = MAGICC_SSP245_global_template_sample*MAGICC_SSP245_weight
  
  write.table(MAGICC_SSP245_template_sample,paste("Module/Climate/MAGICC/rffsps/MAGICC_rffsps_",i,".csv",sep = ""),row.names = F,col.names = F,sep = ",")
}




