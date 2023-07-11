library(openxlsx)
library(arrow)
library(pracma)
library(dplyr)
library(data.table)

setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise")

#####country list in our analysis
country_data <- read.xlsx("Module/GAMD_ISO.xlsx",sheet = "Country",startRow = 1)

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

project_gdpcap <- function(SD){
  g_years <- seq(min(SD$year), max(SD$year), by = 1)
  SD <- SD[order(SD$year),]
  gdpcap <- SD$gdppercap
  pop <- SD$pop
  gdpcap_tm1 <- gdpcap[1] # gdpcap nocc in 2019
  pop_tm1 <- pop[1] # gdpcap nocc in 2019
  for (i in seq_along(c(g_years))) {
    gdpcap[i] <- gdpcap_tm1 * (1 + SD$gdpcapr[i])
    gdpcap_tm1 <- gdpcap[i]
    pop[i] <- pop_tm1 * (1 + SD$popr[i])
    pop_tm1 <- pop[i]
  }
  gdp <- gdpcap*pop;
  return(list(year = g_years, 
              gdpcap = gdpcap,
              gdp = gdp,
              pop=pop))
}


########construct scenario dataset for the DICE model
gdppercap_baseline = read.xlsx("Module/Socioeconomic/outputs/DICE_data/Baseline_data.xlsx")
sample_no = 10000;
global_gdp_dice = matrix(nrow = length(seq(2015,2300,5)),ncol = (sample_no+1));
global_gdp_dice[,1] = seq(2015,2300,5)
global_pop_dice = matrix(nrow = length(seq(2015,2300,5)),ncol = (sample_no+1));
global_pop_dice[,1] = seq(2015,2300,5)

for (i in 1:sample_no) {
  print(i)
  rffsp_pop_income_run_sample <- as.data.table(read_feather(paste("Module/rffsps_v5/pop_income/rffsp_pop_income_run_",i,".feather",sep = "")))
  colnames(rffsp_pop_income_run_sample) = c("ISO3","year","pop","gdp")
  rffsp_pop_income_run_sample$gdpcap = rffsp_pop_income_run_sample$gdp/rffsp_pop_income_run_sample$pop
  rffsp_growthrate_sample_pro = rffsp_pop_income_run_sample[, annual_gdp_pop_g(.SD), by = c("ISO3")]
  rffsp_growthrate_sample_2020 = rffsp_growthrate_sample_pro[rffsp_growthrate_sample_pro$year==2021,]
  rffsp_growthrate_sample_2020$year=2020
  rffsp_growthrate_sample = rbind(rffsp_growthrate_sample_2020,rffsp_growthrate_sample_pro)

  growthrate_sample = rffsp_growthrate_sample[rffsp_growthrate_sample$ISO3 %in% gdppercap_baseline$ISO3,]
  gdppercap_merge <- as.data.table(merge(gdppercap_baseline,growthrate_sample,by = c("ISO3"))); # add basetemp
  gdppop_projection <- gdppercap_merge[,project_gdpcap(.SD),by = c("ISO3")]
  global_gdp_dice[-1,i+1] <- gdppop_projection[year %in% seq(2020,2300,5),sum(gdp),by="year"]$V1/10^12
  global_pop_dice[-1,i+1] <- gdppop_projection[year %in% seq(2020,2300,5),sum(pop),by="year"]$V1/10^6
  global_gdp_dice[1,i+1] <- 73.8    ####2015 gdp data 
  global_pop_dice[1,i+1] <- 7405                 ####2015 pop data 
}

write.csv(global_gdp_dice,"Module/Socioeconomic/outputs/DICE_data/rffsps_global_gdp.csv",quote = F,row.names = F)
write.csv(global_pop_dice,"Module/Socioeconomic/outputs/DICE_data/rffsps_global_pop.csv",quote = F,row.names = F)


########construct scenario dataset for the PAGE model
gdppercap_baseline = read.xlsx("Module/Socioeconomic/outputs/PAGE_data/Baseline_data.xlsx",sheet = "Country-level-data")
gdp_pop_historic = read.xlsx("Module/Socioeconomic/outputs/PAGE_data/Baseline_data.xlsx",sheet = "gdp-pop-historic")
sample_no = 10000;
region_no_page = 8;
region_page = c("EU","US","OT","EE","CA","IA","AF","LA")
time_page = c(2009,2010,2020,2030,2040,2050,2075,2100,2150,2200)
global_gdp_page = matrix(nrow = length(time_page)*region_no_page,ncol = (sample_no+1));
global_gdp_page[,1] = rep(time_page,region_no_page)
global_pop_page = matrix(nrow = length(time_page)*region_no_page,ncol = (sample_no+1));
global_pop_page[,1] = rep(time_page,region_no_page)

for (i in 1:sample_no) {
  print(i)
  rffsp_pop_income_run_sample <- as.data.table(read_feather(paste("Module/rffsps_v5/pop_income/rffsp_pop_income_run_",i,".feather",sep = "")))
  colnames(rffsp_pop_income_run_sample) = c("ISO3","year","pop","gdp")
  rffsp_pop_income_run_sample$gdpcap = rffsp_pop_income_run_sample$gdp/rffsp_pop_income_run_sample$pop
  rffsp_growthrate_sample_pro = rffsp_pop_income_run_sample[, annual_gdp_pop_g(.SD), by = c("ISO3")]
  rffsp_growthrate_sample_2020 = rffsp_growthrate_sample_pro[rffsp_growthrate_sample_pro$year==2021,]
  rffsp_growthrate_sample_2020$year=2020
  rffsp_growthrate_sample = rbind(rffsp_growthrate_sample_2020,rffsp_growthrate_sample_pro)

  growthrate_sample = rffsp_growthrate_sample[rffsp_growthrate_sample$ISO3 %in% gdppercap_baseline$ISO3,]
  gdppercap_merge <- as.data.table(merge(gdppercap_baseline,growthrate_sample,by = c("ISO3"))); # add basetemp
  gdppop_projection <- gdppercap_merge[,project_gdpcap(.SD),by = c("ISO3")]
  gdppop_projection$region = gdppercap_merge$region;
  for (rr in 1:length(region_page)) {
    region_page_target = region_page[rr]
    global_gdp_page[((rr-1)*length(time_page)+3):(rr*length(time_page)),i+1] <- gdppop_projection[year %in% time_page & region ==region_page_target,sum(gdp),by=c("year","region")]$V1
    global_pop_page[((rr-1)*length(time_page)+3):(rr*length(time_page)),i+1] <- gdppop_projection[year %in% time_page & region ==region_page_target,sum(pop),by=c("year","region")]$V1
    global_gdp_page[((rr-1)*length(time_page)+1):((rr-1)*length(time_page)+2),i+1] <-as.numeric(gdp_pop_historic[rr,c(2,5)])
    global_pop_page[((rr-1)*length(time_page)+1):((rr-1)*length(time_page)+2),i+1] <-as.numeric(gdp_pop_historic[rr,c(3,6)])
  }
}

write.csv(global_gdp_page,"Module/Socioeconomic/outputs/PAGE_data/rffsps_global_gdp.csv",quote = F,row.names = F)
write.csv(global_pop_page,"Module/Socioeconomic/outputs/PAGE_data/rffsps_global_pop.csv",quote = F,row.names = F)

# 
# ########construct scenario dataset for the FUND model
# gdppercap_baseline = read.xlsx("Module/Socioeconomic/outputs/FUND_data/Baseline_data.xlsx",sheet = "Country-level-data")
# gdp_pop_historic = read.xlsx("Module/Socioeconomic/outputs/FUND_data/Baseline_data.xlsx",sheet = "gdp-pop-historic")
# sample_no = 10000;
# region_no_fund = 16;
# region_fund = c("USA","CAN","WEU","JPK","ANZ","CEE","FSU","MDE","CAM","SAM","SAS","SEA","CHI","NAF","SSA","SIS")
# time_fund = c(1995:2300)
# global_gdp_fund = matrix(nrow = length(time_fund)*region_no_fund,ncol = (sample_no+1));
# global_gdp_fund[,1] = rep(time_fund,region_no_fund)
# global_pop_fund = matrix(nrow = length(time_fund)*region_no_fund,ncol = (sample_no+1));
# global_pop_fund[,1] = rep(time_fund,region_no_fund)
# 
# for (i in 1:sample_no) {
#   print(i)
#   rffsp_pop_income_run_sample <- as.data.table(read_feather(paste("Module/rffsps_v5/pop_income/rffsp_pop_income_run_",i,".feather",sep = "")))
#   colnames(rffsp_pop_income_run_sample) = c("ISO3","year","pop","gdp")
#   rffsp_pop_income_run_sample$gdpcap = rffsp_pop_income_run_sample$gdp/rffsp_pop_income_run_sample$pop
#   rffsp_growthrate_sample_pro = rffsp_pop_income_run_sample[, annual_gdp_pop_g(.SD), by = c("ISO3")]
#   rffsp_growthrate_sample_2020 = rffsp_growthrate_sample_pro[rffsp_growthrate_sample_pro$year==2021,]
#   rffsp_growthrate_sample_2020$year=2020
#   rffsp_growthrate_sample = rbind(rffsp_growthrate_sample_2020,rffsp_growthrate_sample_pro)
# 
#   growthrate_sample = rffsp_growthrate_sample[rffsp_growthrate_sample$ISO3 %in% gdppercap_baseline$ISO3,]
#   gdppercap_merge <- as.data.table(merge(gdppercap_baseline,growthrate_sample,by = c("ISO3"))); # add basetemp
#   gdppop_projection <- gdppercap_merge[,project_gdpcap(.SD),by = c("ISO3")]
#   gdppop_projection$region = gdppercap_merge$region;
#   for (rr in 1:length(region_fund)) {
#     region_fund_target = region_fund[rr]
#     global_gdp_fund[((rr-1)*length(time_fund)+26):(rr*length(time_fund)),i+1] <- gdppop_projection[year %in% time_fund & region ==region_fund_target,sum(gdp),by=c("year","region")]$V1
#     global_pop_fund[((rr-1)*length(time_fund)+26):(rr*length(time_fund)),i+1] <- gdppop_projection[year %in% time_fund & region ==region_fund_target,sum(pop),by=c("year","region")]$V1
#     global_gdp_fund[((rr-1)*length(time_fund)+1):((rr-1)*length(time_fund)+25),i+1] <-as.numeric(gdp_pop_historic[rr,c(2:26)])
#     global_pop_fund[((rr-1)*length(time_fund)+1):((rr-1)*length(time_fund)+25),i+1] <-as.numeric(gdp_pop_historic[rr,c(27:51)])
#   }
# }
# 
# write.csv(global_gdp_fund,"Module/Socioeconomic/outputs/fund_data/rffsps_global_gdp.csv",quote = F,row.names = F)
# write.csv(global_pop_fund,"Module/Socioeconomic/outputs/fund_data/rffsps_global_pop.csv",quote = F,row.names = F)




