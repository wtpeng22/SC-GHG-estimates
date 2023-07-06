library(reshape2) # added by CK (needed for `acast` function)
library(Hmisc)    # added by CK (needed for `approxExtrap` function)

# ImpactAgricultureComponent
agrate[t,r,'ref'] = if (t==1) agrbm * (0.005 / 0.04)^agnl * agtime else agrbm * (dtemp / 0.04)^agnl + (1- 1/ agtime) * agrate[t-1,r,'ref']
aglevel[t,r,'ref'] = aglparl * deltat[t] + aglparq * (deltat[t]^2)
agco2[t,r,'ref'] = if (t==1) 0 else agcbm /log(2) * log(conc[t - 1] / concpre) 

# Ag damages are fractional loss of Regional Ag Product - damages are negative
AgLossFUND[t,r,'ref']=max(-1,agrate[t,r,'ref'] + aglevel[t,r,'ref'] + agco2[t,r,'ref'])

damages[t,r,'Agriculture',i] = -1*income[t, r]*agrish0*( ypc_F[t,r] / ypc90[r])^-agel*AgLossFUND[t,r,'ref']



# Read data for GTAP DFs ---------------------------------------------------------------
regiondata=read.csv("welfarewithFUNDregions.csv") # added by CK
# load("welfarewithFUNDregions.Rdat")             # commented out by CK
# Formulate alternative, linear damage functions of temperature change using point estimates 
# midDF takes temperature as x and returns welfare % change (neg is loss)
midDF=acast(regiondata[which(regiondata$Model=='Meta-Analysis'),c('Temp','FUND','mid')], Temp ~ FUND)
lowDF=acast(regiondata[which(regiondata$Model=='Meta-Analysis'),c('Temp','FUND','low')], Temp ~ FUND)
highDF=acast(regiondata[which(regiondata$Model=='Meta-Analysis'),c('Temp','FUND','high')], Temp ~ FUND)
AgMIP_NoNDF=acast(regiondata[which(regiondata$Model=='AgMIP_NoN'),c('Temp','FUND','mid')], Temp ~ FUND)
AgMIP_AllDF=acast(regiondata[which(regiondata$Model=='AgMIP_All'),c('Temp','FUND','mid')], Temp ~ FUND)

# added by CK; to use as data input to Mimi component
write.csv(midDF, "data/GTAP DFs/midDF.csv", row.names=FALSE)
write.csv(lowDF, "data/GTAP DFs/lowDF.csv", row.names=FALSE)
write.csv(highDF, "data/GTAP DFs/highDF.csv", row.names=FALSE)
write.csv(AgMIP_NoNDF, "data/GTAP DFs/AgMIP_NoNDF.csv", row.names=FALSE)
write.csv(AgMIP_AllDF, "data/GTAP DFs/AgMIP_AllDF.csv", row.names=FALSE)

selectedAgDF = lowDF  # added by CK, can change to any of the above DFs
# GTAP damages are not time dependent, so compute outside of the time loop
# selectedAgDF is one of the DFs constructed above
# deltat and Pulsedeltat are the timeseries of temperature change
FUNDregions = colnames(selectedAgDF) # added by CK for the following line to work
for (r in FUNDregions) {
  AgLossGTAP[,r,'ref']=pmax(-100,approxExtrap(0:3,c(0,selectedAgDF[,r]),xout=deltat)$y)/100
  AgLossGTAP[,r,'pulse']=pmax(-100,approxExtrap(0:3,c(0,selectedAgDF[,r]),xout=Pulsedeltat)$y)/100
}

damages[t,r,'Agriculture',i] = -1*income[t, r]*agrish0*( ypc_F[t,r] / ypc90[r])^-agel*AgLossGTAP[t,r,'ref']
