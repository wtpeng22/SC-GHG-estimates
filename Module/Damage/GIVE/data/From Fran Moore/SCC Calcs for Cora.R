library(plyr)
library(reshape2)
library(ggplot2)
library(grid)

load("AGincDam.rdat")

data=incDam[which(incDam$discountrate=="3% constant"&incDam$IAWGscenario=="deterministic USG2 (incremental)"),]
#make two plots - one of agriculture, split by region. One of total SCC
ag=data[which(data$sector=="Agriculture"),]
ag=ag[-which(ag$model=="AgMIP_All"),] #remove alternative AgMIP results for main figure
data=ddply(data,.(model),function(x) sum(x$value))
data=data[-which(data$model=="AgMIP_All"),] #remove alternative AgMIP results for main figure
dataerrorbars=data.frame(Model=data$model[c(1,2,5)],SCC=data$V1[c(1,2,5)],ymin=c(NA,NA,data$V1[3]),ymax=c(NA,NA,data$V1[4]))
dataerrorbars$Model=ordered(dataerrorbars$Model,levels=c("FUNDAg","AgMIP_NoN","GTAPAgMid"));dataerrorbars$Model=revalue(dataerrorbars$Model,c("FUNDAg"="FUND","AgMIP_NoN"="AgMIP","GTAPAgMid"="Meta-Analysis"))
agerrorbars=data.frame(Model=ag$model[-c(grep("Low",ag$model),grep("High",ag$model))],SCC=ag$value[-c(grep("Low",ag$model),grep("High",ag$model))],Region=ag$region[-c(grep("Low",ag$model),grep("High",ag$model))])
ymin=sum(ag$value[which(ag$model=="GTAPAgHigh")])
ymax=sum(ag$value[which(ag$model=="GTAPAgLow")])
agerrorbars$Model=ordered(agerrorbars$Model,levels=c("FUNDAg","AgMIP_NoN","GTAPAgMid"))
agerrorbars$Model=revalue(agerrorbars$Model,c("FUNDAg"="FUND","AgMIP_NoN"="AgMIP","GTAPAgMid"="Meta-Analysis"))
agerrorbars1=agerrorbars[which(agerrorbars$SCC<0),];agerrorbars2=agerrorbars[which(agerrorbars$SCC>0),]
my.cols=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a',"#8dd3c7",'#ffff99',"gold","lightgrey","black",'#b15928')
errordata=data.frame(Model=c("FUND","AgMIP","Meta-Analysis"),ymin=c(NA,NA,ymin),ymax=c(NA,NA,ymax),SCC=ddply(agerrorbars,.(Model),function(x) sum(x$SCC))[,2],Region=rep("JPK",3))

#add in extra data-points from PAGE and DICE (from Delavane's email Feb 6th)
#agerrorbars1=rbind(agerrorbars1,data.frame(Model=c("PAGE Market","DICE Non-SLR"),SCC=rep(NA,2),Region=rep(NA,2)))
#agerrorbars1$Model=ordered(agerrorbars1$Model,levels=c("FUND","AgMIP","Meta-Analysis","PAGE Market","DICE Non-SLR"))
#comparison=data.frame(Model=c("PAGE Market","DICE Non-SLR"),SCC=c(6.642,18.869))

a=ggplot(agerrorbars1,aes(x=Model,y=SCC,fill=Region))+geom_bar(stat="identity",width=0.7)
a=a+geom_bar(data=agerrorbars2,stat="identity",width = 0.7)+scale_fill_manual(values=my.cols)+theme_bw()+labs(x="",y="Social Cost of Carbon ($ per ton CO2)",title="Agricultural Impacts")
a=a+geom_errorbar(data=errordata,aes(ymin=ymin,ymax=ymax),width=0.2,lwd=1)+geom_hline(yintercept=0,col="black",lwd=1)
a=a+expand_limits(y=c(-5,50))+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())
#a=a+geom_bar(data=comparison,stat="identity",width=0.7,fill="lightgrey",colour="black", lty=3)+geom_vline(xintercept="")

b=ggplot(dataerrorbars,aes(x=Model,y=SCC,ymin=ymin,ymax=ymax))+geom_bar(stat="identity",width=0.7,fill="dodgerblue",col="black")+geom_errorbar(stat="identity",width=0.2,lwd=1,col="black")
b=b+theme_bw()+labs(x="",y="Social Cost of Carbon ($ per ton CO2)",title="Total Impacts, All Sectors")+geom_hline(yintercept = 0,lwd=1,col="black")
b=b+expand_limits(y=c(-5,50))+theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank())

source("usefulfunctions.R")
x11()
arrange(a,b,nrow=1)

#same graph but including all AgMIP for supplemental
data=incDam[which(incDam$discountrate=="3% constant"&incDam$IAWGscenario=="deterministic USG2 (incremental)"),]
#make two plots - one of agriculture, split by region. One of total SCC
ag=data[which(data$sector=="Agriculture"),]
data=ddply(data,.(model),function(x) sum(x$value))
dataerrorbars=data.frame(Model=data$model[c(1,2,3,6)],SCC=data$V1[c(1,2,3,6)],ymin=c(NA,NA,NA,data$V1[4]),ymax=c(NA,NA,NA,data$V1[5]))
dataerrorbars$Model=ordered(dataerrorbars$Model,levels=c("FUNDAg","AgMIP_All","AgMIP_NoN","GTAPAgMid"));dataerrorbars$Model=revalue(dataerrorbars$Model,c("FUNDAg"="FUND","AgMIP_NoN"="AgMIP_NStress","GTAPAgMid"="Meta-Analysis"))
agerrorbars=data.frame(Model=ag$model[-c(grep("Low",ag$model),grep("High",ag$model))],SCC=ag$value[-c(grep("Low",ag$model),grep("High",ag$model))],Region=ag$region[-c(grep("Low",ag$model),grep("High",ag$model))])
ymin=sum(ag$value[which(ag$model=="GTAPAgHigh")])
ymax=sum(ag$value[which(ag$model=="GTAPAgLow")])
agerrorbars$Model=ordered(agerrorbars$Model,levels=c("FUNDAg","AgMIP_All","AgMIP_NoN","GTAPAgMid"))
agerrorbars$Model=revalue(agerrorbars$Model,c("FUNDAg"="FUND","AgMIP_NoN"="AgMIP_NStress","GTAPAgMid"="Meta-Analysis"))
agerrorbars1=agerrorbars[which(agerrorbars$SCC<0),];agerrorbars2=agerrorbars[which(agerrorbars$SCC>0),]
my.cols=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a',"#8dd3c7",'#ffff99',"gold","lightgrey","black",'#b15928')
errordata=data.frame(Model=c("FUND","AgMIP_All","AgMIP_NStress","Meta-Analysis"),ymin=c(NA,NA,NA,ymin),ymax=c(NA,NA,NA,ymax),SCC=ddply(agerrorbars,.(Model),function(x) sum(x$SCC))[,2],Region=rep("JPK",4))

a=ggplot(agerrorbars1,aes(x=Model,y=SCC,fill=Region))+geom_bar(stat="identity",width=0.7)
a=a+geom_bar(data=agerrorbars2,stat="identity",width = 0.7)+scale_fill_manual(values=my.cols)+theme_bw()+labs(x="",y="Social Cost of Carbon ($ per ton CO2)",title="Agricultural Impacts Only")
a=a+geom_errorbar(data=errordata,aes(ymin=ymin,ymax=ymax),width=0.2,lwd=1)+geom_hline(yintercept=0,col="black",lwd=1)
a=a+expand_limits(y=c(-5,50))

b=ggplot(dataerrorbars,aes(x=Model,y=SCC,ymin=ymin,ymax=ymax))+geom_bar(stat="identity",width=0.7,fill="dodgerblue",col="black")+geom_errorbar(stat="identity",width=0.2,lwd=1,col="black")
b=b+theme_bw()+labs(x="",y="Social Cost of Carbon ($ per ton CO2)",title="Total Impacts, All Sectors")+geom_hline(yintercept = 0,lwd=1,col="black")
b=b+expand_limits(y=c(-5,50))

x11()
arrange(a,b,nrow=1)

#now make graph with sensitivity to discount rate for total SCC
data=incDam[which(incDam$IAWGscenario=="deterministic USG2 (incremental)"),]
data=data[-grep("AgMIP_All",data$model),]
data=ddply(data,.(model,discountrate),function(x) sum(x$value))
errors_high=data[grep("Low",data$model),];errors_low=data[grep("High",data$model),]
data=data[-c(grep("High",data$model),grep("Low",data$model)),]
data$ymin=c(rep(NA,6),errors_low$V1);data$ymax=c(rep(NA,6),errors_high$V1)
data$model=ordered(data$model,levels=c("FUNDAg","AgMIP_NoN","GTAPAgMid"));data$model=revalue(data$model,c("FUNDAg"="FUND","AgMIP_NoN"="AgMIP","GTAPAgMid"="Meta-Analysis"))

a=ggplot(data,aes(x=model,y=V1,ymin=ymin,ymax=ymax,fill=discountrate))+geom_bar(position=position_dodge(0.7),stat="identity",width=0.7,col="black")+geom_errorbar(position=position_dodge(0.7),width=0.2,lwd=1)
a=a+theme_bw()+labs(x="",y="Social Cost of Carbon ($ per ton CO2)",title="Total Impacts, All Sectors")+geom_hline(yintercept = 0,lwd=1,col="black")
a=a+scale_fill_manual(values=c("aquamarine2","cadetblue2","deepskyblue4"),name="Discount Rate")
x11()
a

#Sensitivity to extrapolation beyond 3 degrees
load("FUND Results\\Extrapolation Sensitivity\\AgIncDam.Rdat")
data=incDam[which(incDam$IAWGscenario=="deterministic USG2 (incremental)"),]
data=ddply(data,.(model,discountrate), summarize, sum(value))