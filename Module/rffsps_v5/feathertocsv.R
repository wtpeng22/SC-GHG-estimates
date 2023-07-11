library(arrow)

setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise")

for (i in 1:10000) {
  print(i)
  target_scenario = i
  data_pop_income = arrow::read_feather(paste("Module/rffsps_v5/pop_income/rffsp_pop_income_run_",target_scenario,".feather",sep = ""))
  write.csv(data_pop_income,paste("Module/rffsps_v5/pop_income_csv/rffsp_pop_income_run_",target_scenario,".csv",sep = ""),quote = F,row.names = F)
}

