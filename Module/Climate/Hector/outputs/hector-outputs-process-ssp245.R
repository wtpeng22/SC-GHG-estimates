setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise/Module/Climate/Hector/outputs/ssp245")
library(data.table)
simulation_no = 10000;
variable_no = 3;
year_target = 2000:2300;

# n20_ssp245_3 = read.csv("n20_ssp245_ssp245_3.csv",header = T)
# n20_ssp245_4 = read.csv("n20_ssp245_ssp245_4.csv",header = T)
# 
# for (i in 5500:10000) {
#   print(i)
#   n20_ssp245_3_target = n20_ssp245_3[n20_ssp245_3$run_number == i,]
#   n20_ssp245_4_target = n20_ssp245_4[n20_ssp245_4$run_number == i,]
#   write.csv(n20_ssp245_3_target,paste("./ssp245-3/ssp245_3_",i,".csv",sep=""),quote = F,row.names = F)
#   write.csv(n20_ssp245_4_target,paste("./ssp245-4/ssp245_4_",i,".csv",sep=""),quote = F,row.names = F)
#   
# }


T_base = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
T_extra_CO2 = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
T_extra_CH4 = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
T_extra_N2O = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))

C_base = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
C_extra_CO2 = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
C_extra_CH4 = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
C_extra_N2O = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))

S_base = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
S_extra_CO2 = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
S_extra_CH4 = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))
S_extra_N2O = matrix(nrow = (length(year_target)), ncol = (simulation_no+1))

T_base[,1] = 0:(length(year_target)-1); T_extra_CO2[,1] = 0:(length(year_target)-1); T_extra_CH4[,1] = 0:(length(year_target)-1); T_extra_N2O[,1] = 0:(length(year_target)-1)
C_base[,1] = 0:(length(year_target)-1); C_extra_CO2[,1] = 0:(length(year_target)-1); C_extra_CH4[,1] = 0:(length(year_target)-1); C_extra_N2O[,1] = 0:(length(year_target)-1)
S_base[,1] = 0:(length(year_target)-1); S_extra_CO2[,1] = 0:(length(year_target)-1); S_extra_CH4[,1] = 0:(length(year_target)-1); S_extra_N2O[,1] = 0:(length(year_target)-1)

for (i in 1:10000) {
  print(i)
  data_sample_1 = read.csv(paste("ssp245-1/ssp245_1_",i,".csv",sep=""))
  data_sample_2 = read.csv(paste("ssp245-2/ssp245_2_",i,".csv",sep=""))
  data_sample_3 = read.csv(paste("ssp245-3/ssp245_3_",i,".csv",sep=""))
  data_sample_4 = read.csv(paste("ssp245-4/ssp245_4_",i,".csv",sep=""))
  if(nrow(data_sample_1) == length(year_target)*variable_no){
    T_base[,i+1] = data_sample_1[data_sample_1$variable=="Tgav",]$value
    C_base[,i+1] = data_sample_1[data_sample_1$variable=="Ca",]$value
    S_base[,i+1] = data_sample_1[data_sample_1$variable=="slr",]$value
  }
  if(nrow(data_sample_2) == length(year_target)*variable_no){
    T_extra_CO2[,i+1] = data_sample_2[data_sample_2$variable=="Tgav",]$value
    C_extra_CO2[,i+1] = data_sample_2[data_sample_2$variable=="Ca",]$value
    S_extra_CO2[,i+1] = data_sample_2[data_sample_2$variable=="slr",]$value
  }
  if(nrow(data_sample_3) == length(year_target)*variable_no){
    T_extra_CH4[,i+1] = data_sample_3[data_sample_3$variable=="Tgav",]$value
    C_extra_CH4[,i+1] = data_sample_3[data_sample_3$variable=="Ca",]$value
    S_extra_CH4[,i+1] = data_sample_3[data_sample_3$variable=="slr",]$value
  }
  if(nrow(data_sample_4) == length(year_target)*variable_no){
    T_extra_N2O[,i+1] = data_sample_4[data_sample_4$variable=="Tgav",]$value
    C_extra_N2O[,i+1] = data_sample_4[data_sample_4$variable=="Ca",]$value
    S_extra_N2O[,i+1] = data_sample_4[data_sample_4$variable=="slr",]$value
  }
  
  if(nrow(data_sample_1) < length(year_target)*variable_no){
    T_base[1:(nrow(data_sample_1)/3),i+1] = data_sample_1[data_sample_1$variable=="Tgav",]$value
    C_base[1:(nrow(data_sample_1)/3),i+1] = data_sample_1[data_sample_1$variable=="Ca",]$value
    S_base[1:(nrow(data_sample_1)/3),i+1] = data_sample_1[data_sample_1$variable=="slr",]$value
    
    T_base[-(1:(nrow(data_sample_1)/3)),i+1] = tail(data_sample_1[data_sample_1$variable=="Tgav",]$value,1)
    C_base[-(1:(nrow(data_sample_1)/3)),i+1] = tail(data_sample_1[data_sample_1$variable=="Ca",]$value,1)
    S_base[-(1:(nrow(data_sample_1)/3)),i+1] = tail(data_sample_1[data_sample_1$variable=="slr",]$value,1)
  }
  if(nrow(data_sample_2) < length(year_target)*variable_no){
    T_extra_CO2[1:(nrow(data_sample_2)/3),i+1] = data_sample_2[data_sample_2$variable=="Tgav",]$value
    C_extra_CO2[1:(nrow(data_sample_2)/3),i+1] = data_sample_2[data_sample_2$variable=="Ca",]$value
    S_extra_CO2[1:(nrow(data_sample_2)/3),i+1] = data_sample_2[data_sample_2$variable=="slr",]$value
    
    T_extra_CO2[-(1:(nrow(data_sample_2)/3)),i+1] = tail(data_sample_2[data_sample_2$variable=="Tgav",]$value,1)
    C_extra_CO2[-(1:(nrow(data_sample_2)/3)),i+1] = tail(data_sample_2[data_sample_2$variable=="Ca",]$value,1)
    S_extra_CO2[-(1:(nrow(data_sample_2)/3)),i+1] = tail(data_sample_2[data_sample_2$variable=="slr",]$value,1)
  }
  if(nrow(data_sample_3) < length(year_target)*variable_no){
    T_extra_CH4[1:(nrow(data_sample_3)/3),i+1] = data_sample_3[data_sample_3$variable=="Tgav",]$value
    C_extra_CH4[1:(nrow(data_sample_3)/3),i+1] = data_sample_3[data_sample_3$variable=="Ca",]$value
    S_extra_CH4[1:(nrow(data_sample_3)/3),i+1] = data_sample_3[data_sample_3$variable=="slr",]$value
    
    T_extra_CH4[-(1:(nrow(data_sample_3)/3)),i+1] = tail(data_sample_3[data_sample_3$variable=="Tgav",]$value,1)
    C_extra_CH4[-(1:(nrow(data_sample_3)/3)),i+1] = tail(data_sample_3[data_sample_3$variable=="Ca",]$value,1)
    S_extra_CH4[-(1:(nrow(data_sample_3)/3)),i+1] = tail(data_sample_3[data_sample_3$variable=="slr",]$value,1)
  }
  if(nrow(data_sample_4) < length(year_target)*variable_no){
    T_extra_N2O[1:(nrow(data_sample_4)/3),i+1] = data_sample_4[data_sample_4$variable=="Tgav",]$value
    C_extra_N2O[1:(nrow(data_sample_4)/3),i+1] = data_sample_4[data_sample_4$variable=="Ca",]$value
    S_extra_N2O[1:(nrow(data_sample_4)/3),i+1] = data_sample_4[data_sample_4$variable=="slr",]$value
    
    T_extra_N2O[-(1:(nrow(data_sample_4)/3)),i+1] = tail(data_sample_4[data_sample_4$variable=="Tgav",]$value,1)
    C_extra_N2O[-(1:(nrow(data_sample_4)/3)),i+1] = tail(data_sample_4[data_sample_4$variable=="Ca",]$value,1)
    S_extra_N2O[-(1:(nrow(data_sample_4)/3)),i+1] = tail(data_sample_4[data_sample_4$variable=="slr",]$value,1)
  }
}

write.csv(T_base,"./T_base.csv",quote = F,row.names = F)
write.csv(C_base,"./C_base.csv",quote = F,row.names = F)
write.csv(S_base,"./S_base.csv",quote = F,row.names = F)

write.csv(T_extra_CO2,"./T_extra_CO2.csv",quote = F,row.names = F)
write.csv(C_extra_CO2,"./C_extra_CO2.csv",quote = F,row.names = F)
write.csv(S_extra_CO2,"./S_extra_CO2.csv",quote = F,row.names = F)

write.csv(T_extra_CH4,"./T_extra_CH4.csv",quote = F,row.names = F)
write.csv(C_extra_CH4,"./C_extra_CH4.csv",quote = F,row.names = F)
write.csv(S_extra_CH4,"./S_extra_CH4.csv",quote = F,row.names = F)

write.csv(T_extra_N2O,"./T_extra_N2O.csv",quote = F,row.names = F)
write.csv(C_extra_N2O,"./C_extra_N2O.csv",quote = F,row.names = F)
write.csv(S_extra_N2O,"./S_extra_N2O.csv",quote = F,row.names = F)



