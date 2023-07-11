# FUND climate module
a=list.files("./Climate/FUND/FUND_base_input");
datalist=lapply(a,function(name){
  read.table(paste("./Climate/FUND/FUND_base_input/",name,sep=""),sep=",")
});

# emissionperiod<-71;
impulselength<-10;
options(digits = 10);
file_num=195;
t_year0=1; #起始年份，模型默认为1950年

Region_No=16; #区域数量
Sector_No=15;

library(stringr);
clock_Current<-function(year_current){
  t_year<<-year_current; 
}
# 编写best_guess函数
library(triangle);

Best_Guess<-function(a)
{
  if (model_choose=="MonteCarlo"){
    if(is.numeric(a))
    {
      Best_Guess=a;
    }
    else 
    {
      if(substr(a,start=1,stop=1)=="0"){
        Best_Guess=0;
      }
      #当为正态分布时的数据处理办法
      else if(substr(a,start=2,stop=2)=="N")
      {
        intermediate=str_split(gsub("[()]","",sub("~N","",a)),";");   #去除括号和～N的标志符，再去除封号，得到各个变量
        if(is.na(intermediate[[1]][3])){
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          Best_Guess=rnorm(1,mean,variance);
        }
        else if(substring(intermediate[[1]][3],1,3)=="min" && is.na(intermediate[[1]][4])){
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rnorm(1,mean,variance);
            if(t>=min) break;
          }
          Best_Guess=t;
        }
        else if(substring(intermediate[[1]][3],1,3)=="max" && is.na(intermediate[[1]][4])){
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          max=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rnorm(1,mean,variance);
            if(t<=max) break;
          }
          Best_Guess=t;
        }
        else{
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          max=as.numeric(substring(intermediate[[1]][4],5));#求最大值
          repeat{
            t=rnorm(1,mean,variance);
            if(t>=min && t<=max) break;
          }
          Best_Guess=t;
        }
      }
      #当为gamma分布时的数据处理方法
      else if(substr(a,start=2,stop=2)=="G")
      {
        intermediate=str_split(gsub("[()]","",sub("~Gamma","",a)),";");   #去除括号和～Gamma的标志符，再去除封号，得到各个变量
        if(is.na(intermediate[[1]][3])){
          shape=as.numeric(intermediate[[1]][1]);#均值
          scale=as.numeric(intermediate[[1]][2]);#方差
          Best_Guess=rgamma(1,shape,1/scale);
        }
        else if(substring(intermediate[[1]][3],1,3)=="min"&&is.na(intermediate[[1]][4])){
          shape=as.numeric(intermediate[[1]][1]);
          scale=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rgamma(1,shape,1/scale);
            if(t>=min) break;
          }
          Best_Guess=t;#计算方法见FUND模型
        }
        else if(substring(intermediate[[1]][3],1,3)=="max" && is.na(intermediate[[1]][4])){
          shape=as.numeric(intermediate[[1]][1]);
          scale=as.numeric(intermediate[[1]][2]);#方差
          max=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rgamma(1,shape,1/scale);
            if(t<=max) break;
          }
          Best_Guess=t;#计算方法见FUND模型
        }
        else{
          shape=as.numeric(intermediate[[1]][1]);
          scale=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          max=as.numeric(substring(intermediate[[1]][4],5));#求最小值
          repeat{
            t=rgamma(1,shape,1/scale);
            if(t<=max && t>=min) break;
          }
          Best_Guess=t;#计算方法见FUND模型
        }
      }
      #当为三角分布时的处理方法:拒绝抽样法进行三角分布抽样
      else if(substr(a,start=2,stop=2)=="T")
      {
        intermediate=str_split(gsub("[()]","",sub("~Triangular","",a)),";");   #去除括号和～Triangular的标志符，再去除封号，得到各个变量
        min=as.numeric(intermediate[[1]][1]);
        max=as.numeric(intermediate[[1]][2])
        expect=as.numeric(intermediate[[1]][3]);#提取第三个数字变量
        Best_Guess=rtriangle(1,min,max,expect);
        
      }
    }
  }
  else if(model_choose=="BestGuess"){
    if(is.numeric(a)==TRUE)
    {
      Best_Guess=a;
    }
    else 
    {
      if(substr(a,start=1,stop=1)=="0"){
        Best_Guess=0;
      }
      #当为正态分布时的数据处理办法
      else if(substr(a,start=2,stop=2)=="N")
      {
        intermediate=str_split(gsub("[()]","",sub("~N","",a)),";");   #去除括号和～N的标志符，再去除封号，得到各个变量
        Best_Guess=as.numeric(intermediate[[1]][1]);#提取第一个数字变量
      }
      #当为gamma分布时的数据处理方法
      else if(substr(a,start=2,stop=2)=="G")
      {
        intermediate=str_split(gsub("[()]","",sub("~Gamma","",a)),";");   #去除括号和～Gamma的标志符，再去除封号，得到各个变量
        Best_Guess=(as.numeric(intermediate[[1]][1])-1)*as.numeric(intermediate[[1]][2]);#计算方法见FUND模型
      }
      #当为三角分布时的处理方法
      else if(substr(a,start=2,stop=2)=="T")
      {
        intermediate=str_split(gsub("[()]","",sub("~Triangular","",a)),";");   #去除括号和～Triangular的标志符，再去除封号，得到各个变量
        Best_Guess=as.numeric(intermediate[[1]][3]);#提取第三个数字变量
      } 
    }
  }
}
#计算CO2气体循环模式
lifeco1=Best_Guess(datalist[[109]]$V1);
lifeco2=Best_Guess(datalist[[110]]$V1);
lifeco3=Best_Guess(datalist[[111]]$V1);
lifeco4=Best_Guess(datalist[[112]]$V1);
lifeco5=Best_Guess(datalist[[113]]$V1);
lifen2o=Best_Guess(datalist[[114]]$V1);
cbox10=Best_Guess(datalist[[18]]$V1);
cbox20=Best_Guess(datalist[[19]]$V1);
cbox30=Best_Guess(datalist[[20]]$V1);
cbox40=Best_Guess(datalist[[21]]$V1);
cbox50=Best_Guess(datalist[[22]]$V1);
co2fra1=Best_Guess(datalist[[41]]$V1);
co2fra2=Best_Guess(datalist[[42]]$V1);
co2fra3=Best_Guess(datalist[[43]]$V1);
co2fra4=Best_Guess(datalist[[44]]$V1);
co2fra5=Best_Guess(datalist[[45]]$V1);
TerrCO2Stock0=Best_Guess(datalist[[171]]$V1);
TerrCO2Sens=Best_Guess(datalist[[170]]$V1);
TerrCO2Stock=matrix(0,nrow=TimeStep,ncol=1);
cbox1=matrix(0,nrow=TimeStep,ncol=1);
cbox2=matrix(0,nrow=TimeStep,ncol=1);
cbox3=matrix(0,nrow=TimeStep,ncol=1);
cbox4=matrix(0,nrow=TimeStep,ncol=1);
cbox5=matrix(0,nrow=TimeStep,ncol=1);
acco2=matrix(0,nrow=TimeStep,ncol=1);
globc=matrix(0,nrow=TimeStep,ncol=1);
TerrestrialCO2=matrix(0,nrow=TimeStep,ncol=1);
TerrCO2Stock[t_year0,1]=Best_Guess(datalist[[171]]$V1);
co2decay1=lifeco1;
co2decay2=exp(-1/lifeco2);
co2decay3=exp(-1/lifeco3);
co2decay4=exp(-1/lifeco4);
co2decay5=exp(-1/lifeco5);
cbox1[t_year0,1]=cbox10;
cbox2[t_year0,1]=cbox20;
cbox3[t_year0,1]=cbox30;
cbox4[t_year0,1]=cbox40;
cbox5[t_year0,1]=cbox50;
acco2[t_year0,1]=cbox1[t_year0,1]+cbox2[t_year0,1]+cbox3[t_year0,1]+cbox4[t_year0,1]+cbox5[t_year0,1];
mco2=matrix(0,nrow=TimeStep,ncol=1);
globch4=matrix(0,nrow=TimeStep,ncol=1);
globn2o=matrix(0,nrow=TimeStep,ncol=1);
globsf6=matrix(0,nrow=TimeStep,ncol=1);

ClimateCO2CycleComponent<-function(mco2,s_temp){
  t_year=clock_Current(year_current);
  t_value=t_year-1;
  if(t_value==(2011-1950)){
    tempIn2010<<-s_temp[(2010-1950+1),1];
  }
  if(t_value>(2010-1950)){
    TerrestrialCO2[t_year,1]<<-(s_temp[t_year-1,1]-tempIn2010)*TerrCO2Sens*TerrCO2Stock[t_year-1,1]/TerrCO2Stock0;
  }
  else{
    TerrestrialCO2[t_year,1]<<-0;
  }
  TerrCO2Stock[t_year,1]<<-max(TerrCO2Stock[t_year-1,1]-TerrestrialCO2[t_year,1],0);
  globc[t_year,1]<<-mco2[t_year,1]+TerrestrialCO2[t_year,1];
  cbox1[t_year,1]<<-cbox1[t_year-1,1]*co2decay1+0.000471*co2fra1*globc[t_year,1];
  cbox2[t_year,1]<<-cbox2[t_year-1,1]*co2decay2+0.000471*co2fra2*globc[t_year,1];
  cbox3[t_year,1]<<-cbox3[t_year-1,1]*co2decay3+0.000471*co2fra3*globc[t_year,1];
  cbox4[t_year,1]<<-cbox4[t_year-1,1]*co2decay4+0.000471*co2fra4*globc[t_year,1];
  cbox5[t_year,1]<<-cbox5[t_year-1,1]*co2decay5+0.000471*co2fra5*globc[t_year,1];
  acco2[t_year,1]<<-cbox1[t_year,1]+cbox2[t_year,1]+cbox3[t_year,1]+cbox4[t_year,1]+cbox5[t_year,1];
}
#计算CH4循环模式
lifech4=Best_Guess(datalist[[108]]$V1);
ch4pre=Best_Guess(datalist[[35]]$V1);
acch4=matrix(0,nrow=TimeStep,ncol=1);
ch4decay=1/lifech4;
acch4[t_year0,1]=1222;
ClimateCH4CycleComponent<-function(globch4){
  t_year=clock_Current(year_current);
  acch4[t_year,1]<<-acch4[t_year-1,1]+0.3397*globch4[t_year,1]-ch4decay*(acch4[t_year-1,1]-ch4pre);
  if(acch4[t_year,1]<0){
    print("CH4浓度超出范围");
  }
}
#计算n2o循环模式
lifen2o=Best_Guess(datalist[[114]]$V1);
n2opre=Best_Guess(datalist[[131]]$V1);
acn2o=matrix(0,nrow=TimeStep,ncol=1);
n2odecay=1/lifen2o;
acn2o[t_year0,1]=296;
ClimateN2OCycleComponent<-function(globn2o){
  t_year=clock_Current(year_current);
  acn2o[t_year,1]<<-acn2o[t_year-1,1]+0.2079*globn2o[t_year,1]-n2odecay*(acn2o[t_year-1,1]-n2opre);
  if(acn2o[t_year,1]<0){
    print("N2O浓度超出范围");
  }
}
#计算sf6循环模式
lifesf6=Best_Guess(datalist[[116]]$V1);
sf6pre=Best_Guess(datalist[[155]]$V1);
acsf6=matrix(0,nrow=TimeStep,ncol=1);
sf6decay=1/lifesf6;
acsf6[t_year0,1]=sf6pre;
ClimateSF6CycleComponent<-function(globsf6){
  t_year=clock_Current(year_current);
  acsf6[t_year,1]<<-sf6pre+(acsf6[t_year-1,1]-sf6pre)*(1-sf6decay)+globsf6[t_year,1]/25.1;
  if(acsf6[t_year,1]<0){
    print("SF6浓度超出范围");
  }
}
#计算Climate Forcing
rfCO2=matrix(0,nrow=TimeStep,ncol=1);
rfCH4=matrix(0,nrow=TimeStep,ncol=1);
rfN2O=matrix(0,nrow=TimeStep,ncol=1);
rfSF6=matrix(0,nrow=TimeStep,ncol=1);
radforc=matrix(0,nrow=TimeStep,ncol=1);
rfEMF22=matrix(0,nrow=TimeStep,ncol=1);
ch4ind=Best_Guess(datalist[[32]]$V1);
co2pre=Best_Guess(datalist[[46]]$V1);
ch4pre=Best_Guess(datalist[[35]]$V1);
rfSO2=matrix(0,nrow=TimeStep,ncol=1);
# for(t_year in 1:TimeStep){
#   rfSO2[t_year,1]=Best_Guess(datalist[[141]]$V2[t_year]);
# }
Interact<-function(M,N){
  d=1+((M*N)^0.75)*2.01*10^(-5)+((M*N)^1.52)*M*5.31*10^(-15);
  return(0.47*log(d));
}
ClimateForcingComponent<-function(acco2,acch4,acn2o,acsf6){
  t_year=clock_Current(year_current);
  ch4n2o=Interact(ch4pre,n2opre);
  rfCO2[t_year,1]<<-5.35*log(acco2[t_year,1]/co2pre);
  rfCH4[t_year,1]<<-0.036*(1+ch4ind)*(acch4[t_year,1]^0.5-ch4pre^0.5)-Interact(acch4[t_year,1],n2opre)+ch4n2o;
  rfN2O[t_year,1]<<-0.12*(acn2o[t_year,1]^0.5-n2opre^0.5)-Interact(ch4pre,acn2o[t_year,1])+ch4n2o;
  rfSF6[t_year,1]<<-0.00052*(acsf6[t_year,1]-sf6pre);
  radforc[t_year,1]<<-rfCO2[t_year,1]+rfCH4[t_year,1]+rfN2O[t_year,1]+rfSF6[t_year,1]+rfSO2[t_year,1];
  rfEMF22[t_year,1]<<-rfCO2[t_year,1]+rfCH4[t_year,1]+rfN2O[t_year,1];
}
#计算气候变化导致的温升效应
s_temp=matrix(0,nrow=TimeStep,ncol=1);
LifeTempConst=Best_Guess(datalist[[117]]$V1);

# ClimateSensitivity=Best_Guess(datalist[[40]]$V1);


LifeTempLin=Best_Guess(datalist[[118]]$V1);
LifeTempQd=Best_Guess(datalist[[119]]$V1);
s_temp[t_year0,1]=0.2;
ClimateDynamicComponent<-function(radforc){
  LifeTemp<<-max(LifeTempConst+LifeTempLin*ClimateSensitivity+LifeTempQd*(ClimateSensitivity^2),1);
  delaytemp<<-1/LifeTemp;
  temps<<-ClimateSensitivity/5.35/log(2);
  dtemp<<-delaytemp*temps*radforc[t_year,1]-delaytemp*s_temp[t_year-1,1];
  s_temp[t_year,1]<<-s_temp[t_year-1,1]+dtemp;
}


#计算海洋
lifesea=Best_Guess(datalist[[115]]$V1);
sea=matrix(0,nrow=TimeStep,ncol=1);
seas=Best_Guess(datalist[[151]]$V1);
delaysea=1/lifesea;
sea[t_year0,1]=0;
OceanComponent<-function(s_temp){
  t_year=clock_Current(year_current);
  ds=delaysea*seas*s_temp[t_year,1]-delaysea*sea[t_year-1,1];
  sea[t_year,1]<<-sea[t_year-1,1]+ds;
}

t_year0=1
mco2[t_year0,1]=(GHGem[GHGem$Date==1949+t_year0,2]+GHGem[GHGem$Date==1949+t_year0,3])*1000;
globch4[t_year0,1]=GHGem[GHGem$Date==1949+t_year0,4];
globn2o[t_year0,1]=GHGem[GHGem$Date==1949+t_year0,5];
globsf6[t_year0,1]=GHGem[GHGem$Date==1949+t_year0,25];

TATM<-matrix(0, nrow =39,ncol=1)
slr<-matrix(0, nrow =39,ncol=1)

playground_fund_climate<-function(){
  
  rfSO2[t_year0,1]<<-rfso2[1,1];
  for(q in 2:TimeStep){
    rfSO2[q,1]<<-rfso2[q,1];
  }
  
  if(MarginalEmission_CO2==TRUE){
    GHGem[GHGem$Date==1949+emissionperiod,2]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,2]+100*12/44
  }
  
  if(MarginalEmission_CH4==TRUE){
    GHGem[GHGem$Date==1949+emissionperiod,4]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,4]+1000
  }
  
  if(MarginalEmission_N2O==TRUE){
    GHGem[GHGem$Date==1949+emissionperiod,5]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,5]+100*28/44
  }
  
  if(MarginalEmission_SF6==TRUE){
    GHGem[GHGem$Date==1949+emissionperiod,25]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,25]+1000
  }
  
  if(MarginalEmission_CO2==FALSE){
    GHGem[GHGem$Date==1949+emissionperiod,2]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,2]
  }
  
  if(MarginalEmission_CH4==FALSE){
    GHGem[GHGem$Date==1949+emissionperiod,4]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,4]
  }
  
  if(MarginalEmission_N2O==FALSE){
    GHGem[GHGem$Date==1949+emissionperiod,5]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,5]
  }
  
  if(MarginalEmission_SF6==FALSE){
    GHGem[GHGem$Date==1949+emissionperiod,25]<<-GHGem_base[GHGem_base$Date==1949+emissionperiod,25]
  }
  
  
  for(t_year in 2:TimeStep){
  
  year_current<<-clock_Current(t_year);

  mco2[t_year,1]<<-(GHGem[GHGem$Date==1949+year_current,2]+GHGem[GHGem$Date==1949+year_current,3])*1000;
  globch4[t_year,1]<<-GHGem[GHGem$Date==1949+year_current,4];
  globn2o[t_year,1]<<-GHGem[GHGem$Date==1949+year_current,5];
  globsf6[t_year,1]<<-GHGem[GHGem$Date==1949+year_current,25];
  
  ClimateCO2CycleComponent(mco2,s_temp)
  ClimateCH4CycleComponent(globch4) 
  ClimateN2OCycleComponent(globn2o) 
  ClimateSF6CycleComponent(globsf6)
  ClimateForcingComponent(acco2,acch4,acn2o,acsf6)
  ClimateDynamicComponent(radforc)
  OceanComponent(s_temp);
  }
}

Monte_carlo_fund_climate<-function(){
  
  if(model_choose=="MonteCarlo"){
    ClimateSensitivity<<-ECS_distribution$x[ecs_num];
  }
  if(model_choose=="BestGuess"){
    ClimateSensitivity<<-3;
  }
  
  lifeco1<<-Best_Guess(datalist[[109]]$V1);
  lifeco2<<-Best_Guess(datalist[[110]]$V1);
  lifeco3<<-Best_Guess(datalist[[111]]$V1);
  lifeco4<<-Best_Guess(datalist[[112]]$V1);
  lifeco5<<-Best_Guess(datalist[[113]]$V1);
  lifen2o<<-Best_Guess(datalist[[114]]$V1);
  cbox10<<-Best_Guess(datalist[[18]]$V1);
  cbox20<<-Best_Guess(datalist[[19]]$V1);
  cbox30<<-Best_Guess(datalist[[20]]$V1);
  cbox40<<-Best_Guess(datalist[[21]]$V1);
  cbox50<<-Best_Guess(datalist[[22]]$V1);
  co2fra1<<-Best_Guess(datalist[[41]]$V1);
  co2fra2<<-Best_Guess(datalist[[42]]$V1);
  co2fra3<<-Best_Guess(datalist[[43]]$V1);
  co2fra4<<-Best_Guess(datalist[[44]]$V1);
  co2fra5<<-Best_Guess(datalist[[45]]$V1);
  TerrCO2Stock0<<-Best_Guess(datalist[[171]]$V1);
  TerrCO2Sens<<-Best_Guess(datalist[[170]]$V1);
  TerrCO2Stock[t_year0,1]<<-Best_Guess(datalist[[171]]$V1);
  lifech4<<-Best_Guess(datalist[[108]]$V1);
  ch4pre<<-Best_Guess(datalist[[35]]$V1);
  #计算n2o循环模式
  lifen2o<<-Best_Guess(datalist[[114]]$V1);
  n2opre<<-Best_Guess(datalist[[131]]$V1);
  lifesf6<<-Best_Guess(datalist[[116]]$V1);
  sf6pre<<-Best_Guess(datalist[[155]]$V1);
  ch4ind<<-Best_Guess(datalist[[32]]$V1);
  co2pre<<-Best_Guess(datalist[[46]]$V1);
  ch4pre<<-Best_Guess(datalist[[35]]$V1);
  LifeTempConst<<-Best_Guess(datalist[[117]]$V1);
  lifesea<<-Best_Guess(datalist[[115]]$V1);
  seas<<-Best_Guess(datalist[[151]]$V1);
  # ClimateSensitivity=Best_Guess(datalist[[40]]$V1);
  LifeTempLin=Best_Guess(datalist[[118]]$V1);
  LifeTempQd=Best_Guess(datalist[[119]]$V1);
}



