setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise")
####sampling the climate scenarios and the parameter ECS
sample_no = 10000;
####sampling the climate scenarios for the climate model;
####all climate models share the same scenario sample order
set.seed(1234)
climate_scenario_sample = sample(1:sample_no,sample_no,replace = TRUE)

write.csv(climate_scenario_sample,"Module/Sample/climate_scenario_sample.csv",quote = F,row.names = F)


####all climate models share the same ECS sample order
ECS_distribution<-read.csv("Module/Sample/ECS.csv",header=T);
AR5_ECS_distribution = matrix(nrow =((length(ECS_distribution$Temperature)-1)*100+1),ncol=2);
for (i in 1:(length(ECS_distribution$Temperature)-1)) {
  AR5_ECS_distribution[((i-1)*100+1):(i*100+1),1]= seq(ECS_distribution$Temperature[i],ECS_distribution$Temperature[i+1],length.out = 101);
  AR5_ECS_distribution[((i-1)*100+1):(i*100+1),2]= seq(ECS_distribution$AR5[i],ECS_distribution$AR5[i+1],length.out = 101);
}
AR5_ECS_distribution= as.data.frame(AR5_ECS_distribution);
colnames(AR5_ECS_distribution) = c("Temperature","AR5");
# AR5_ECS_distribution$AR5 = AR5_ECS_distribution$AR5/sum(AR5_ECS_distribution$AR5)
AR5_ECS_distribution_bound = AR5_ECS_distribution[AR5_ECS_distribution$Temperature>=1 & AR5_ECS_distribution$Temperature<=10,]

set.seed(1234)
ECS_sample = sample(AR5_ECS_distribution_bound$Temperature,sample_no,replace = TRUE,prob = AR5_ECS_distribution_bound$AR5)

write.csv(ECS_sample,"Module/Sample/ecs_sample.csv",quote = F,row.names = F)


####sampling the nids for the bootstrap method for the BHM damage model
sample_no = 10000;
####sampling the climate scenarios for the climate model;
####all climate models share the same scenario sample order
set.seed(2345)
nid_sample = sample(1:sample_no,sample_no,replace = TRUE)

write.csv(nid_sample,"Module/Sample/nid_sample.csv",quote = F,row.names = F)


