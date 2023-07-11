library(readxl)
library("triangle");

a_PAGE=list.files("./Climate/PAGE/PAGE_base_input");
model_choose="MonteCarlo";
datalist_PAGE=lapply(a_PAGE,function(name){
  read.table(paste("./Climate/PAGE/PAGE_base_input/",name,sep=""),sep=",",header = TRUE)
})
impulselength<-10;
options(digits = 10);
file_num=53;
t0=1; #起始年份，模型默认为1950年
TimeStep_PAGE=10; #时间跨度
Region_No_PAGE=8; #区域数量
Sector_No=15;

library(stringr);
clock_Current<-function(year_current){
  t_year<<-year_current; 
}
y_year=c(2009,2010,2020,2030,2040,2050,2075,2100,2150,2200);

y_year_0=2008;
# 编写best_guess函数
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


# CO2 cycle
c_CO2concentration=matrix(nrow =TimeStep_PAGE,ncol = 1 );
pic_preindustconcCO2=278000;
#exc_excessconcCO2;
c0_CO2concbaseyr=395000;
re_remainCO2=matrix(nrow =TimeStep_PAGE,ncol = 1 );
#re_remainCO2base;
renoccf_remainCO2wocc=matrix(nrow =TimeStep_PAGE,ncol = 1 );
air_CO2fractioninatm=62.00;
stay_fractionCO2emissionsinatm=0.3;
tea_CO2emissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
teay_CO2emissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
ccf_CO2feedback=9.66666666666666;
ccfmax_maxCO2feedback=53.3333333333333;
cea_cumCO2emissionsatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
tea_CO2emissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
teay_CO2emissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
ce_0_basecumCO2emissions=2050000;
#y_year=matrix(nrow =TimeStep_PAGE,ncol = 1 );
#y_year_0;
res_CO2atmlifetime=73.3333333333333;
den_CO2density=7.8;
rt_g0_baseglobaltemp=0.735309967925382;
e0_globalCO2emissions=38191.0315797948;
rt_g_globaltemperature=matrix(nrow =TimeStep_PAGE,ncol = 1 );
# gain=ccf_CO2feedback*rt_g0_baseglobaltemp
# tea0=e0_globalCO2emissions*air_CO2fractioninatm/100

# for(t_year in 1:TimeStep_PAGE){
# tea_CO2emissionstoatm[t_year]=(e_globalCO2emissions[t_year])*air_CO2fractioninatm/100
# teay_CO2emissionstoatm[t_year]=(tea_CO2emissionstoatm[t_year]+tea0)/2
# }
# exc_excessconcCO2=c0_CO2concbaseyr-pic_preindustconcCO2;
# re_remainCO2base=exc_excessconcCO2*den_CO2density;
#PAGE 2009 initial remaining emissions without CO2 feedback
# renoccf0_remainCO2wocc=re_remainCO2base/(1+gain/100)
#eq. 8 from Hope (2006) - baseline cumulative emissions to atmosphere
# ceabase=ce_0_basecumCO2emissions*air_CO2fractioninatm/100
#eq.9 from Hope(2006) - cumulative emissions in atmosphere

CO2_cycle<-function(){
  t_year=clock_Current(year_current);
  if (t_year==1){
    #CO2 emissions gain calculated based on PAGE 2009
    gain=ccf_CO2feedback*rt_g0_baseglobaltemp
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea0=e0_globalCO2emissions*air_CO2fractioninatm/100
    tea_CO2emissionstoatm[t_year]<<-(e_globalCO2emissions[t_year])*air_CO2fractioninatm/100
    teay_CO2emissionstoatm[t_year]<<-(tea_CO2emissionstoatm[t_year]+tea0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcCO2<<-c0_CO2concbaseyr-pic_preindustconcCO2
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainCO2base<<-exc_excessconcCO2*den_CO2density
    #PAGE 2009 initial remaining emissions without CO2 feedback
    renoccf0_remainCO2wocc=re_remainCO2base/(1+gain/100)
    #eq. 8 from Hope (2006) - baseline cumulative emissions to atmosphere
    ceabase=ce_0_basecumCO2emissions*air_CO2fractioninatm/100
    #eq.9 from Hope(2006) - cumulative emissions in atmosphere
    cea_cumCO2emissionsatm[t_year]<<-ceabase+teay_CO2emissionstoatm[t_year]
    #eq.11 from Hope (2006) - anthropogenic remaining emissions
    renoccf_remainCO2wocc[t_year]<<-stay_fractionCO2emissionsinatm*ceabase*
      (1-exp(-(y_year[t_year]-y_year_0)/
               res_CO2atmlifetime))+renoccf0_remainCO2wocc*
      exp(-(y_year[t_year]-y_year_0)/res_CO2atmlifetime)+
      teay_CO2emissionstoatm[t_year]*exp(-(y_year[t_year]-y_year_0)/
                                      (2*res_CO2atmlifetime))
    #Hope 2009 - remaining emissions with CO2 feedback
    re_remainCO2[t_year]<<-renoccf_remainCO2wocc[t_year]*(1+gain/100)
  }
  else{
    #CO2 emissions gain calculated based on PAGE 2009
    gain=min(ccf_CO2feedback*rt_g_globaltemperature[t_year-1],ccfmax_maxCO2feedback)
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_CO2emissionstoatm[t_year]<<-(e_globalCO2emissions[t_year])*air_CO2fractioninatm/100
    #eq.7 from Hope (2006) - total emissions over time period
    teay_CO2emissionstoatm[t_year]<<-(tea_CO2emissionstoatm[t_year]+tea_CO2emissionstoatm[t_year-1])*
      (y_year[t_year]-y_year[t_year-1])/2
    #eq.9 from Hope(2006) - cumulative emissions in atmosphere
    cea_cumCO2emissionsatm[t_year]<<-cea_cumCO2emissionsatm[t_year-1]+teay_CO2emissionstoatm[t_year]
    #eq.11 from Hope (2006) - anthropogenic remaining emissions
    renoccf_remainCO2wocc[t_year]<<-stay_fractionCO2emissionsinatm*cea_cumCO2emissionsatm[t_year-1]*
      (1-exp(-(y_year[t_year]-y_year[t_year-1])/
               res_CO2atmlifetime))+renoccf_remainCO2wocc[t_year-1]*
      exp(-(y_year[t_year]-y_year[t_year-1])/res_CO2atmlifetime)+
      teay_CO2emissionstoatm[t_year]*exp(-(y_year[t_year]-y_year[t_year-1])/
                                      (2*res_CO2atmlifetime))
    #Hope 2009 - remaining emissions with CO2 feedback
    re_remainCO2[t_year]<<-renoccf_remainCO2wocc[t_year]*(1+gain/100)
  }
  #eq.11 from Hope(2006) - CO2 concentration
  c_CO2concentration[t_year]<<-pic_preindustconcCO2+exc_excessconcCO2 * re_remainCO2[t_year]/re_remainCO2base
  
}

# CO2 forcing
f0_CO2baseforcing=1.735;
fslope_CO2forcingslope=5.5;
c0_baseCO2conc=395000;
f_CO2forcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );
CO2_forcing<-function(){
  f_CO2forcing[t_year]<<-f0_CO2baseforcing+fslope_CO2forcingslope*log(c_CO2concentration[t_year]/c0_baseCO2conc);
}



# CH4 cycle
pic_preindustconcCH4 = 700.
den_CH4density = 2.78
stim_CH4emissionfeedback= 0.
air_CH4fractioninatm = 100.
res_CH4atmlifetime = 10.5
c0_CH4concbaseyr = 1860.
rtl_g0_baselandtemp = 0.9258270139190647
e_0globalCH4emissions = 363.00000000000006
nte_natCH4emissions=matrix(nrow =TimeStep_PAGE,ncol = 1 );
re_remainCH4=matrix(nrow =TimeStep_PAGE,ncol = 1 );
nte_natCH4emissions=matrix(nrow =TimeStep_PAGE,ncol = 1 );
tea_CH4emissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
teay_CH4emissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
rtl_g_landtemperature=matrix(nrow =TimeStep_PAGE,ncol = 1 );
CH4_cycle<-function(){
  t_year=clock_Current(year_current);
  if (t_year==1){
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    nte_0<<-stim_CH4emissionfeedback*rtl_g0_baselandtemp;
    nte_natCH4emissions[t_year]<<-stim_CH4emissionfeedback*rtl_g0_baselandtemp;
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_CH4emissionstoatm[t_year]<<-(e_globalCH4emissions[t_year]+nte_natCH4emissions[t_year])*air_CH4fractioninatm/100
    tea_0=(e_0globalCH4emissions+nte_0)*air_CH4fractioninatm/100
    teay_CH4emissionstoatm[t_year]<<-(tea_CH4emissionstoatm[t_year]+tea_0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcCH4<<-c0_CH4concbaseyr-pic_preindustconcCH4
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainCH4base<<-exc_excessconcCH4*den_CH4density
    re_remainCH4[t_year]<<-re_remainCH4base*exp(-(y_year[t_year]-y_year_0)/res_CH4atmlifetime)+
      teay_CH4emissionstoatm[t_year]*res_CH4atmlifetime*(1-exp(-(y_year[t_year]-y_year_0)/res_CH4atmlifetime))/(y_year[t_year]-y_year_0)
  }
  else{
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    #Here assume still using area-weighted average regional temperatures (i.e. land temperatures) for natural emissions feedback
    nte_natCH4emissions[t_year]<<-stim_CH4emissionfeedback*rtl_g_landtemperature[t_year-1] #askChrisHope
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_CH4emissionstoatm[t_year]<<-(e_globalCH4emissions[t_year]+nte_natCH4emissions[t_year])*air_CH4fractioninatm/100
    #eq.7 from Hope (2006) - average emissions to atm over time period
    teay_CH4emissionstoatm[t_year]<<-(tea_CH4emissionstoatm[t_year]+tea_CH4emissionstoatm[t_year-1])*(y_year[t_year]-y_year[t_year-1])/2
    #eq.10 from Hope (2006) - remaining emissions in atmosphere
    re_remainCH4[t_year]<<-re_remainCH4[t_year-1]*exp(-(y_year[t_year]-y_year[t_year-1])/res_CH4atmlifetime)+
      teay_CH4emissionstoatm[t_year]*res_CH4atmlifetime*(1-exp(-(y_year[t_year]-y_year[t_year-1])/res_CH4atmlifetime))/(y_year[t_year]-y_year[t_year-1])
    
  }
  #eq.11 from Hope(2006) - CH4 concentration
  c_CH4concentration[t_year]<<-pic_preindustconcCH4+exc_excessconcCH4*re_remainCH4[t_year]/re_remainCH4base;
}

#CH4 forcing
fslope_CH4forcingslope = 0.036
f0_CH4baseforcing = 0.550
c0_baseN2Oconc = 322.
c0_baseCH4conc = 1860.
c_N2Oconcentration = matrix(nrow =TimeStep_PAGE,ncol = 1 );
c_CH4concentration=matrix(nrow =TimeStep_PAGE,ncol = 1 );
f_CH4forcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );
over=matrix(nrow =TimeStep_PAGE,ncol = 1 );
#CH4forcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );
CH4_forcing<-function(){
  t_year=clock_Current(year_current);
  # if (t_year==1){
  #calculate baseline forcing overlap in first time period
  over_baseoverlap= -0.47*log(1+2.0e-5*(c0_baseN2Oconc*c0_baseCH4conc)^0.75+5.3e-15*c0_baseCH4conc*(c0_baseCH4conc*c0_baseN2Oconc)^1.52)
  # }
  
  over[t_year]= -0.47*log(1+2.0e-5*(c_CH4concentration[t_year]*c0_baseN2Oconc)^0.75+5.3e-15*c_CH4concentration[t_year]*(c0_baseN2Oconc*c_CH4concentration[t_year])^1.52)
  f_CH4forcing[t_year]<<-f0_CH4baseforcing+fslope_CH4forcingslope*(sqrt(c_CH4concentration[t_year])-sqrt(c0_baseCH4conc))+over[t_year]-over_baseoverlap;

}



# N2O cycle
pic_preindustconcN2O = 270.
den_N2Odensity = 7.8
stim_N2Oemissionfeedback = 0.
air_N2Ofractioninatm = 100.
res_N2Oatmlifetime = 114.
c0_N2Oconcbaseyr = 322.
rtl_g0_baselandtemp = 0.9258270139190647
e_0globalN2Oemissions = 11.046520000000001
re_remainN2O=matrix(nrow =TimeStep_PAGE,ncol = 1 );
nte_natN2Oemissions=matrix(nrow =TimeStep_PAGE,ncol = 1 );
tea_N2Oemissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
teay_N2Oemissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
#rtl_g_landtemperature=matrix(nrow =TimeStep_PAGE,ncol = 1 );

N2O_cycle<-function(){
  t_year=clock_Current(year_current )
  if (t_year==1){
    #eq.3 from Hope (2006) - natural emissions feedback, using global temperatures calculated in ClimateTemperature component
    nte_0<<-stim_N2Oemissionfeedback*rtl_g0_baselandtemp
    nte_natN2Oemissions[t_year]<<-stim_N2Oemissionfeedback*rtl_g0_baselandtemp
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_N2Oemissionstoatm[t_year]<<-(e_globalN2Oemissions[t_year]+nte_natN2Oemissions[t_year])*air_N2Ofractioninatm/100
    tea_0<<-(e_0globalN2Oemissions+nte_0)*air_N2Ofractioninatm/100
    teay_N2Oemissionstoatm[t_year]<<-(tea_N2Oemissionstoatm[t_year]+tea_0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcN2O<<-c0_N2Oconcbaseyr-pic_preindustconcN2O
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainN2Obase<<-exc_excessconcN2O*den_N2Odensity
    re_remainN2O[t_year]<<-re_remainN2Obase*exp(-(y_year[t_year]-y_year_0)/res_N2Oatmlifetime)+
      teay_N2Oemissionstoatm[t_year]*res_N2Oatmlifetime*(1-exp(-(y_year[t_year]-y_year_0)/res_N2Oatmlifetime))/(y_year[t_year]-y_year_0)
  }
  else{
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    #Here assume still using area-weighted average regional temperatures (i.e. land temperatures) for natural emissions feedback
    nte_natN2Oemissions[t_year]<<-stim_N2Oemissionfeedback*rtl_g_landtemperature[t_year-1]
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_N2Oemissionstoatm[t_year]<<-(e_globalN2Oemissions[t_year]+nte_natN2Oemissions[t_year])*air_N2Ofractioninatm/100
    #eq.7 from Hope (2006) - average emissions to atm over time period
    teay_N2Oemissionstoatm[t_year]<<-(tea_N2Oemissionstoatm[t_year]+tea_N2Oemissionstoatm[t_year-1])*(y_year[t_year]-y_year[t_year-1])/2
    #eq.10 from Hope (2006) - remaining emissions in atmosphere
    re_remainN2O[t_year]<<-re_remainN2O[t_year-1]*exp(-(y_year[t_year]-y_year[t_year-1])/res_N2Oatmlifetime)+
      teay_N2Oemissionstoatm[t_year]*res_N2Oatmlifetime*(1-exp(-(y_year[t_year]-y_year[t_year-1])/res_N2Oatmlifetime))/(y_year[t_year]-y_year[t_year-1])
  }
  
  #eq.11 from Hope(2006) - N2O concentration
  c_N2Oconcentration[t_year]<<-pic_preindustconcN2O+exc_excessconcN2O*re_remainN2O[t_year]/re_remainN2Obase
  
}

#N2O forcing
fslope_N2Oforcingslope = 0.12
f0_N2Obaseforcing = 0.180
c0_baseN2Oconc = 322.
c0_baseCH4conc = 1860.
f_N2Oforcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );
N2O_forcing<-function(){
  t_year=clock_Current(year_current)
  # if (t_year==1){
  #calculate baseline forcing overlap in first time period
  
  #calculate baseline forcing overlap in first time period
  over_baseoverlap= -0.47*log(1+2.01e-5*(c0_baseN2Oconc*c0_baseCH4conc)^0.75+5.31e-15*c0_baseCH4conc*(c0_baseCH4conc*c0_baseN2Oconc)^1.52)
  
  over[t_year]= -0.47*log(1+2.01e-5*(c0_baseCH4conc*c_N2Oconcentration[t_year])^0.75+5.31e-15*c0_baseCH4conc*(c0_baseCH4conc*c_N2Oconcentration[t_year])^1.52)
  f_N2Oforcing[t_year]<<- f0_N2Obaseforcing+fslope_N2Oforcingslope*(sqrt(c_N2Oconcentration[t_year])-sqrt(c0_baseN2Oconc))+over[t_year]-over_baseoverlap
  
  
}


#LG cycle
pic_preindustconcLG = 0.
den_LGdensity = 100000.
stim_LGemissionfeedback = 0.
air_LGfractioninatm = 100.
res_LGatmlifetime = 1000.
c0_LGconcbaseyr = 0.11
rtl_g0_baselandtemp = 0.9258270139190647
e_0globalLGemissions = 557.2112715473608
c_LGconcentration=matrix(nrow =TimeStep_PAGE,ncol = 1 );
re_remainLG=matrix(nrow =TimeStep_PAGE,ncol = 1 );
nte_natLGemissions=matrix(nrow =TimeStep_PAGE,ncol = 1 );
tea_LGemissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );
teay_LGemissionstoatm=matrix(nrow =TimeStep_PAGE,ncol = 1 );

LG_cycle<-function(){
  t_year=clock_Current(year_current)
  if (t_year==1){
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    nte0<<- stim_LGemissionfeedback* rtl_g0_baselandtemp
    nte_natLGemissions[t_year]<<- stim_LGemissionfeedback* rtl_g0_baselandtemp
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea0<<-( e_0globalLGemissions+nte0)* air_LGfractioninatm/100
    tea_LGemissionstoatm[t_year]<<-( e_globalLGemissions[t_year]+ nte_natLGemissions[t_year])* air_LGfractioninatm/100
    teay_LGemissionstoatm[t_year]<<-( tea_LGemissionstoatm[t_year]+tea0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcLG<<- c0_LGconcbaseyr- pic_preindustconcLG
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainLGbase<<- exc_excessconcLG* den_LGdensity
    re_remainLG[t_year]<<- re_remainLGbase*exp(-(  y_year[t_year]- y_year_0)/ res_LGatmlifetime)+
      teay_LGemissionstoatm[t_year]* res_LGatmlifetime*(1-exp(-(  y_year[t_year]- y_year_0)/ res_LGatmlifetime))/(  y_year[t_year]- y_year_0)
  }
  else{
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    #Here assume still using area-weighted average regional temperatures (i.e. land temperatures) for natural emissions feedback
    nte_natLGemissions[t_year]<<- stim_LGemissionfeedback* rtl_g_landtemperature[t_year-1]
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_LGemissionstoatm[t_year]<<-( e_globalLGemissions[t_year]+ nte_natLGemissions[t_year])* air_LGfractioninatm/100
    #eq.7 from Hope (2006) - average emissions to atm over time period
    teay_LGemissionstoatm[t_year]<<-( tea_LGemissionstoatm[t_year]+ tea_LGemissionstoatm[t_year-1])*(  y_year[t_year]-  y_year[t_year-1])/2
    #eq.10 from Hope (2006) - remaining emissions in atmosphere
    re_remainLG[t_year]<<- re_remainLG[t_year-1]*exp(-(  y_year[t_year]-  y_year[t_year-1])/ res_LGatmlifetime)+
      teay_LGemissionstoatm[t_year]* res_LGatmlifetime*(1-exp(-(  y_year[t_year]-  y_year[t_year-1])/ res_LGatmlifetime))/(  y_year[t_year]-  y_year[t_year-1])
    end
  }
  #eq.11 from Hope(2006) - LG concentration
  c_LGconcentration[t_year]<<- pic_preindustconcLG+ exc_excessconcLG* re_remainLG[t_year]/ re_remainLGbase
}

# LG forcing
f0_LGforcingbase = 0.022
fslope_LGforcingslope = 0.2
c0_LGconcbaseyr = 0.11;
f_LGforcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );

LG_forcing<-function(){
  t_year=clock_Current(year_current );
  f_LGforcing[t_year]<<-f0_LGforcingbase+fslope_LGforcingslope*(c_LGconcentration[t_year]-c0_LGconcbaseyr);
}

#SulphateForcing
d_sulphateforcingbase = -0.46666666666666673
ind_slopeSEforcing_indirect = -0.4000000000000001
se0_sulphateemissionsbase=matrix(nrow =1,ncol = Region_No_PAGE );
pse_sulphatevsbase=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
se_sulphateemissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
area=matrix(nrow =1,ncol = Region_No_PAGE );
sfx_sulphateflux=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
nf_naturalsfx=matrix(nrow =1,ncol = Region_No_PAGE );
fs_sulphateforcing=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );

for(r in 1:Region_No_PAGE){
  se0_sulphateemissionsbase[1,r]=Best_Guess(datalist_PAGE[[45]]$se_0[r]);
  area[1,r]=Best_Guess(datalist_PAGE[[1]]$area[r]);
  nf_naturalsfx[1,r]=Best_Guess(datalist_PAGE[[32]]$Natural.S[r]);
} 
for(t_year in 1:TimeStep_PAGE){
  for(r in 1:Region_No_PAGE){
    pse_sulphatevsbase[t_year,r]=Best_Guess(datalist_PAGE[[37]][t_year,r+1]);
  }
}


SulphateForcing<-function(){
  bigSFX0 <<- se0_sulphateemissionsbase /area;
  t_year=clock_Current(year_current);
  
  for(p in 1:TimeStep_PAGE){
    for(q in 1:Region_No_PAGE){
      se_sulphateemissions[p, q] <<-  se0_sulphateemissionsbase[q] *  pse_sulphatevsbase[p, q] / 100;
    }
  }
  
  se_sulphateemissions <<- se_sulphateemissions*(e_globalSO2emissions/rowSums(se_sulphateemissions));
  
  for(r in 1:Region_No_PAGE){

    # Eq.17 from Hope (2006) - sulfate flux
    sfx_sulphateflux[t_year,r] <<- se_sulphateemissions[t_year,r] /  area[r]
    # Update for Eq. 18 from Hope (2009) - sulfate radiative forcing effect
    bigSFD0 <<- d_sulphateforcingbase * bigSFX0[r] / (sum(bigSFX0 *  area) / sum( area))
    fsd_term <<- bigSFD0 * sfx_sulphateflux[t_year,r] / bigSFX0[r]
    fsi_term <<-  ind_slopeSEforcing_indirect/log(2) * log(( nf_naturalsfx[r] + sfx_sulphateflux[t_year, r]) /  nf_naturalsfx[r])
    
    fs_sulphateforcing[t_year, r] <<- fsd_term + fsi_term;
  }
}


f_lineargasforcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );
exf_excessforcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );
ft_totalforcing=matrix(nrow =TimeStep_PAGE,ncol = 1 );
# for(t_year in 1:TimeStep_PAGE){
  # exf_excessforcing[t_year]=emission_data$extraforcing[t_year];
  exf_excessforcing<<-other_gases_forcing;
  
# }
Total_forcing<-function(){
  # ft_totalforcing[t_year] =  f_CO2forcing[t_year] +  f_CH4forcing[t_year] +  f_N2Oforcing[t_year] +  f_lineargasforcing[t_year] +  exf_excessforcing[t_year]
  ft_totalforcing[t_year] <<- f_CO2forcing[t_year] + f_CH4forcing[t_year] + f_N2Oforcing[t_year] + f_LGforcing[t_year] + exf_excessforcing[t_year]
  
}

# climate temperature
rlo_ratiolandocean = 1.40
pole_polardifference = 1.50
lat_g_meanlatitude =  30.21989459076828
fslope_CO2forcingslope = 5.5
tcr_transientresponse = 1.70
frt_warminghalflife = 35.00
et_equilibriumtemperature=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
rt_realizedtemperature=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
lat_latitude=matrix(nrow =1,ncol = Region_No_PAGE );
rtl_0_realizedtemperature=matrix(nrow =1,ncol = Region_No_PAGE );
rtl_realizedtemperature=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
rtl_g_landtemperature=matrix(nrow =TimeStep_PAGE,ncol = 1 );
rto_g_oceantemperature=matrix(nrow =TimeStep_PAGE,ncol = 1 );
rt_g_globaltemperature=matrix(nrow =TimeStep_PAGE,ncol = 1 );
rt_0_realizedtemperature=matrix(nrow =1,ncol = Region_No_PAGE );
rt_adj_temperatureadjustment=matrix(nrow =1,ncol = Region_No_PAGE );
for(r in 1:Region_No_PAGE){
  rtl_0_realizedtemperature[r]=Best_Guess(datalist_PAGE[[44]]$rtl_0[r])
  lat_latitude[r]=Best_Guess(datalist_PAGE[[31]]$Latitude[r])
}
ocean_prop_ortion = 1. - sum( area) / 510000000.

# Equation 21 from Hope (2006): initial global land temperature
rtl_g0_baselandtemp = sum( rtl_0_realizedtemperature *  area) / sum( area)
#待确认
# initial ocean and global temperatures
rto_g0_baseoceantemp =  rtl_g0_baselandtemp/ rlo_ratiolandocean
rt_g0_baseglobaltemp = ocean_prop_ortion * rto_g0_baseoceantemp + (1. - ocean_prop_ortion) *  rtl_g0_baselandtemp;
ClimateTemperature<-function(){
  t_year=clock_Current(year_current )
  if (t_year == 1) {    # only calculate once
    # sens_climatesensitivity<<-tcr_transientresponse / (1. - ( frt_warminghalflife / 70.) * (1. - exp(-70. /  frt_warminghalflife)))
    # sens_climatesensitivity<<-3;
     }
  
  ## Adjustment for latitude and land
  ocean_prop_ortion <<- 1. - (sum( area) / 510000000.)
  for(r in 1:Region_No_PAGE){
    rt_adj_temperatureadjustment[r]  <<- ( pole_polardifference / 90.) * (abs( lat_latitude[r]) -  lat_g_meanlatitude)
  }
  ## Unadjusted realized temperature
  
  # Equation 19 from Hope (2006): equilibrium temperature estimate
  for (r in 1:Region_No_PAGE){
    #et_equilibriumtemperature[t_year, r] = ( sens_climatesensitivity / log(2.0)) * ( ft_totalforcing[t_year] +  fs_sulfateforcing[t_year, r]) /  fslope_CO2forcingslope
    et_equilibriumtemperature[t_year, r] <<- ( sens_climatesensitivity / log(2.0)) * ( ft_totalforcing[t_year] +  fs_sulphateforcing[t_year, r]) /  fslope_CO2forcingslope
    
  }
  # Equation 20 from Hope (2006): realized temperature estimate
  # Hope (2009) replaced OCEAN with FRT
  if (t_year == 1){
    # Calculate baseline realized temperature by subtracting off adjustment
    
    for (r in 1:Region_No_PAGE){
      rt_0_realizedtemperature[r]  <<-  ( rtl_0_realizedtemperature[r] - rt_adj_temperatureadjustment[r]) * (1. + (ocean_prop_ortion /  rlo_ratiolandocean) - ocean_prop_ortion)
      rt_realizedtemperature[t_year, r] <<- rt_0_realizedtemperature[r] + (1 - exp(-(  y_year[t_year] -  y_year_0) /  frt_warminghalflife)) * ( et_equilibriumtemperature[t_year, r] - rt_0_realizedtemperature[r])
    }
  }
  else{
    for (r in 1:Region_No_PAGE){
      rt_realizedtemperature[t_year, r] <<-  rt_realizedtemperature[t_year-1, r] + (1 - exp(-(  y_year[t_year] -   y_year[t_year-1]) /  frt_warminghalflife)) * ( et_equilibriumtemperature[t_year, r] -  rt_realizedtemperature[t_year-1, r])
    }
  }
  
  ## Adjusted realized temperature
  
  # Adding adjustment, from Hope (2009)
  for (r in 1:Region_No_PAGE){
    rtl_realizedtemperature[t_year, r] <<-  rt_realizedtemperature[t_year, r] / (1. + (ocean_prop_ortion /  rlo_ratiolandocean) - ocean_prop_ortion) + rt_adj_temperatureadjustment[r]
  }
  
  # Equation 21 from Hope (2006): global realized temperature estimate
  rtl_g_landtemperature[t_year] <<- sum( rtl_realizedtemperature[t_year, ]*  area) / sum( area)
  
  # Ocean and global average temperature from Hope (2009)
  rto_g_oceantemperature[t_year] <<-  rtl_g_landtemperature[t_year] /  rlo_ratiolandocean
  rt_g_globaltemperature[t_year] <<- ocean_prop_ortion *  rto_g_oceantemperature[t_year] + (1. - ocean_prop_ortion) *  rtl_g_landtemperature[t_year]
  
}


# Sea Level rise
sltemp_SLtemprise = 1.7333333333333334
sla_SLbaselinerise = 1.00
sltau_SLresponsetime = 1000.
s0_initialSL = 0.15
es_equilibriumSL=matrix(nrow =TimeStep_PAGE,ncol = 1 );
s_sealevel=matrix(nrow =TimeStep_PAGE,ncol = 1 );
expfs_exponential=matrix(nrow =TimeStep_PAGE,ncol = 1 );
yp_TimeStep_PAGE=matrix(nrow =TimeStep_PAGE,ncol = 1 );
Sea_level_rise<-function(){
  t_year=clock_Current(year_current)
  if(t_year==1){
    yp_TimeStep_PAGE[t_year]<<-y_year[1] - y_year_0
    es_equilibriumSL[t_year]<<-sltemp_SLtemprise*rt_g_globaltemperature[t_year] + sla_SLbaselinerise
    expfs_exponential[t_year]<<-exp(-yp_TimeStep_PAGE[t_year]/sltau_SLresponsetime)
    s_sealevel[t_year]<<-s0_initialSL + (es_equilibriumSL[t_year] - s0_initialSL)*(1-expfs_exponential[t_year])
  }
  else{
    yp_TimeStep_PAGE[t_year]<<-y_year[t_year] - y_year[t_year-1]
    es_equilibriumSL[t_year]<<-sltemp_SLtemprise*rt_g_globaltemperature[t_year] + sla_SLbaselinerise
    expfs_exponential[t_year]<<-exp(-yp_TimeStep_PAGE[t_year]/sltau_SLresponsetime)
    s_sealevel[t_year]<<-s_sealevel[t_year-1] + (es_equilibriumSL[t_year] -s_sealevel[t_year-1])*(1-expfs_exponential[t_year])
  }
}

rt_g_globaltemperature_fund = matrix(nrow = TimeStep_FUND,ncol = 1);
acco2_fund = matrix(nrow = TimeStep_FUND,ncol = 1);
sea_fund = matrix(nrow = TimeStep_FUND,ncol = 1);

playground_page_climate<-function(){
  e_globalCO2emissions<<-(as.numeric(GHGem[y_year-1765+1,2])+as.numeric(GHGem[y_year-1765+1,3]))*44/12*1000
  e_globalCH4emissions<<-as.numeric(GHGem[y_year-1765+1,4])
  e_globalN2Oemissions<<-as.numeric(GHGem[y_year-1765+1,5])*44/28
  # e_globalLGemissions<<-as.numeric(GHGem[y_year-1765+1,25])*100;
  e_globalLGemissions<<-as.numeric(LGem);
  
  e_globalSO2emissions<<-as.numeric(GHGem[y_year-1765+1,6])*2
  
  if(emissions_pulse<=5){
    if(MarginalEmission_CO2==TRUE){
      e_globalCO2emissions[emissions_pulse]<<-e_globalCO2emissions[emissions_pulse]+100000/10
    }
    
    if(MarginalEmission_CH4==TRUE){
      e_globalCH4emissions[emissions_pulse]<<-e_globalCH4emissions[emissions_pulse]+1000/10
    }
    
    if(MarginalEmission_N2O==TRUE){
      e_globalN2Oemissions[emissions_pulse]<<-e_globalN2Oemissions[emissions_pulse]+100/10
    }
    
    if(MarginalEmission_CO2==FALSE){
      e_globalCO2emissions[emissions_pulse]<<-e_globalCO2emissions[emissions_pulse]
    }
    
    if(MarginalEmission_CH4==FALSE){
      e_globalCH4emissions[emissions_pulse]<<-e_globalCH4emissions[emissions_pulse]
    }
    
    if(MarginalEmission_N2O==FALSE){
      e_globalN2Oemissions[emissions_pulse]<<-e_globalN2Oemissions[emissions_pulse]
    }
  }
  
  if(emissions_pulse==6){
  if(MarginalEmission_CO2==TRUE){
    e_globalCO2emissions[emissions_pulse]<<-e_globalCO2emissions[emissions_pulse]+100000/17.5
  }
  
  if(MarginalEmission_CH4==TRUE){
    e_globalCH4emissions[emissions_pulse]<<-e_globalCH4emissions[emissions_pulse]+1000/17.5
  }
  
  if(MarginalEmission_N2O==TRUE){
    e_globalN2Oemissions[emissions_pulse]<<-e_globalN2Oemissions[emissions_pulse]+100/17.5
  }
  
  if(MarginalEmission_CO2==FALSE){
    e_globalCO2emissions[emissions_pulse]<<-e_globalCO2emissions[emissions_pulse]
  }
  
  if(MarginalEmission_CH4==FALSE){
    e_globalCH4emissions[emissions_pulse]<<-e_globalCH4emissions[emissions_pulse]
  }
  
  if(MarginalEmission_N2O==FALSE){
    e_globalN2Oemissions[emissions_pulse]<<-e_globalN2Oemissions[emissions_pulse]
  }
  }
  for(i in 1:10){
    year_current<<-i;
#    CO2_emissions();
    CO2_cycle();
    CO2_forcing();
 #   CH4_emissions();
    CH4_cycle();
    CH4_forcing();
 #   N2O_emissions();
    N2O_cycle();
    N2O_forcing();
#    LG_emissions();
    LG_cycle();
    LG_forcing();
    SulphateForcing();
    Total_forcing();
    ClimateTemperature();
    Sea_level_rise();
  }
  rt_g_globaltemperature_fund[1:(2008-1950+1)] <<- c(0.136, 0.259, 0.343, 0.410, 0.183, 0.123, 0.047, 0.305, 0.359, 0.330, 0.264, 0.353, 0.328, 0.362, 0.091, 0.172, 0.245, 0.239, 0.200, 0.346, 0.286, 0.128, 0.247, 0.376, 0.100, 0.165, 0.072, 0.361, 0.252, 0.371, 0.406, 0.453, 0.325, 0.508,	0.299, 0.284, 0.359, 0.506, 0.512, 0.432, 0.610, 0.568, 0.419, 0.462, 0.523, 0.639, 0.496, 0.704, 0.853, 0.620, 0.609, 0.755, 0.811, 0.821, 0.761, 0.860, 0.819, 0.805, 0.709);
  rt_g_globaltemperature_fund[(2009-1950+1)]<<- rt_g_globaltemperature[1];
  rt_g_globaltemperature_fund[(2010-1950+1)]<<- rt_g_globaltemperature[2];
  rt_g_globaltemperature_fund[(2010-1950+1):(2020-1950+1)] <<- seq(rt_g_globaltemperature[2],rt_g_globaltemperature[3],length.out = 11);
  rt_g_globaltemperature_fund[(2020-1950+1):(2030-1950+1)] <<- seq(rt_g_globaltemperature[3],rt_g_globaltemperature[4],length.out = 11);
  rt_g_globaltemperature_fund[(2030-1950+1):(2040-1950+1)] <<- seq(rt_g_globaltemperature[4],rt_g_globaltemperature[5],length.out = 11);
  rt_g_globaltemperature_fund[(2040-1950+1):(2050-1950+1)] <<- seq(rt_g_globaltemperature[5],rt_g_globaltemperature[6],length.out = 11);
  rt_g_globaltemperature_fund[(2050-1950+1):(2075-1950+1)] <<- seq(rt_g_globaltemperature[6],rt_g_globaltemperature[7],length.out = 26);
  rt_g_globaltemperature_fund[(2075-1950+1):(2100-1950+1)] <<- seq(rt_g_globaltemperature[7],rt_g_globaltemperature[8],length.out = 26);
  rt_g_globaltemperature_fund[(2100-1950+1):(2150-1950+1)] <<- seq(rt_g_globaltemperature[8],rt_g_globaltemperature[9],length.out = 51);
  rt_g_globaltemperature_fund[(2150-1950+1):(2200-1950+1)] <<- seq(rt_g_globaltemperature[9],rt_g_globaltemperature[10],length.out = 51);

  acco2_fund[1:(2008-1950+1)] <<- c(311.232, 311.732, 312.232, 312.732, 313.232, 313.732, 314.232, 314.732, 315.232, 315.9808333, 316.91, 317.6441667, 318.4541667, 318.9925, 319.6166667, 320.0358333, 321.3691667, 322.1808333, 323.05, 324.6233333, 325.6816667, 326.3191667, 327.4566667, 329.6775, 330.1841667, 331.1191667, 332.0391667, 333.8316667, 335.4033333, 336.8408333, 338.7508333, 340.105, 341.4475, 343.0541667, 344.66, 346.1158333, 347.4325, 349.1825, 351.5691667, 353.1225, 354.3933333, 355.6116667, 356.4458333, 357.1, 358.8325, 360.82, 362.6075, 363.7291667, 366.7, 368.3816667, 369.55, 371.1441667, 373.2791667, 375.8016667, 377.5225, 379.7958333, 381.8991667, 383.7908333, 385.595)
  acco2_fund[(2009-1950+1)]<<- c_CO2concentration[1]/1000;
  acco2_fund[(2010-1950+1)]<<- c_CO2concentration[2]/1000;
  acco2_fund[(2010-1950+1):(2020-1950+1)] <<- seq(c_CO2concentration[2],c_CO2concentration[3],length.out = 11)/1000;
  acco2_fund[(2020-1950+1):(2030-1950+1)] <<- seq(c_CO2concentration[3],c_CO2concentration[4],length.out = 11)/1000;
  acco2_fund[(2030-1950+1):(2040-1950+1)] <<- seq(c_CO2concentration[4],c_CO2concentration[5],length.out = 11)/1000;
  acco2_fund[(2040-1950+1):(2050-1950+1)] <<- seq(c_CO2concentration[5],c_CO2concentration[6],length.out = 11)/1000;
  acco2_fund[(2050-1950+1):(2075-1950+1)] <<- seq(c_CO2concentration[6],c_CO2concentration[7],length.out = 26)/1000;
  acco2_fund[(2075-1950+1):(2100-1950+1)] <<- seq(c_CO2concentration[7],c_CO2concentration[8],length.out = 26)/1000;
  acco2_fund[(2100-1950+1):(2150-1950+1)] <<- seq(c_CO2concentration[8],c_CO2concentration[9],length.out = 51)/1000;
  acco2_fund[(2150-1950+1):(2200-1950+1)] <<- seq(c_CO2concentration[9],c_CO2concentration[10],length.out = 51)/1000;
  
  sea_fund[1:(2008-1950+1)] <<- c(0.080,0.089,	0.087,	0.091,	0.088,	0.089,	0.084,	0.097,	0.099,	0.100,	0.103,	0.109,	0.104,	0.102,	0.094,	0.105,	0.100,	0.101,	0.102,	0.109,	0.107,	0.112,	0.121,	0.115,	0.127,	0.126,	0.125,	0.123,	0.129,	0.124,	0.130,	0.143,	0.137,	0.146,	0.145,	0.134,	0.135,	0.136,	0.140,	0.145,	0.146,	0.149,	0.152,	0.148,	0.151,	0.156,	0.160,	0.168,	0.160,	0.167,	0.168,	0.172,	0.173,	0.181,	0.180,	0.179,	0.181,	0.184,	0.194)
  sea_fund[(2009-1950+1)]<<- s_sealevel[1];
  sea_fund[(2010-1950+1)]<<- s_sealevel[2];
  sea_fund[(2010-1950+1):(2020-1950+1)] <<- seq(s_sealevel[2],s_sealevel[3],length.out = 11);
  sea_fund[(2020-1950+1):(2030-1950+1)] <<- seq(s_sealevel[3],s_sealevel[4],length.out = 11);
  sea_fund[(2030-1950+1):(2040-1950+1)] <<- seq(s_sealevel[4],s_sealevel[5],length.out = 11);
  sea_fund[(2040-1950+1):(2050-1950+1)] <<- seq(s_sealevel[5],s_sealevel[6],length.out = 11);
  sea_fund[(2050-1950+1):(2075-1950+1)] <<- seq(s_sealevel[6],s_sealevel[7],length.out = 26);
  sea_fund[(2075-1950+1):(2100-1950+1)] <<- seq(s_sealevel[7],s_sealevel[8],length.out = 26);
  sea_fund[(2100-1950+1):(2150-1950+1)] <<- seq(s_sealevel[8],s_sealevel[9],length.out = 51);
  sea_fund[(2150-1950+1):(2200-1950+1)] <<- seq(s_sealevel[9],s_sealevel[10],length.out = 51);
};
