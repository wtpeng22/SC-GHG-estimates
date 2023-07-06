# PAGE model R version: authorized by Tianpeng Wang
# setwd("/home/wangtianpeng/Downloads/")
library("triangle");

a=list.files("Module/Damage/PAGE/PAGE_base_input");

datalist_PAGE=lapply(a,function(name){
  read.table(paste("Module/Damage/PAGE/PAGE_base_input/",name,sep=""),sep=",",header = TRUE)
})

impulselength<-10;
options(digits = 15);
file_num=53;
t0=1; #起始年份，模型默认为1950年
TimeStep_PAGE=10; #时间跨度
Region_No_PAGE=8; #区域数量
Sector_No=15;

library(stringr);
clock_Current<-function(year_current){
  t<<-year_current; 
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

ClimateTemperature<-function(){
  t=clock_Current(year_current )
  # if (t == 1) {    # only calculate once
  #   sens_climatesensitivity<<-tcr_transientresponse / (1. - ( frt_warminghalflife / 70.) * (1. - exp(-70. /  frt_warminghalflife)))
  # }
  # 
  # ## Adjustment for latitude and land
  # ocean_prop_ortion <<- 1. - (sum( area) / 510000000.)
  # for(r in 1:Region_No){
  #   rt_adj_temperatureadjustment[r]  <<- ( pole_polardifference / 90.) * (abs( lat_latitude[r]) -  lat_g_meanlatitude)
  # }
  # ## Unadjusted realized temperature
  # 
  # # Equation 19 from Hope (2006): equilibrium temperature estimate
  # for (r in 1:Region_No){
  #   #et_equilibriumtemperature[t, r] = ( sens_climatesensitivity / log(2.0)) * ( ft_totalforcing[t] +  fs_sulfateforcing[t, r]) /  fslope_CO2forcingslope
  #   et_equilibriumtemperature[t, r] <<- ( sens_climatesensitivity / log(2.0)) * ( ft_totalforcing[t] +  fs_sulphateforcing[t, r]) /  fslope_CO2forcingslope
  #   
  # }
  # # Equation 20 from Hope (2006): realized temperature estimate
  # # Hope (2009) replaced OCEAN with FRT
  # if (t == 1){
  #   # Calculate baseline realized temperature by subtracting off adjustment
  #   
  #   for (r in 1:Region_No){
  #     rt_0_realizedtemperature[r]  <<-  ( rtl_0_realizedtemperature[r] - rt_adj_temperatureadjustment[r]) * (1. + (ocean_prop_ortion /  rlo_ratiolandocean) - ocean_prop_ortion)
  #     rt_realizedtemperature[t, r] <<- rt_0_realizedtemperature[r] + (1 - exp(-(  y_year[t] -  y_year_0) /  frt_warminghalflife)) * ( et_equilibriumtemperature[t, r] - rt_0_realizedtemperature[r])
  #   }
  # }
  # else{
  #   for (r in 1:Region_No){
  #     rt_realizedtemperature[t, r] <<-  rt_realizedtemperature[t-1, r] + (1 - exp(-(  y_year[t] -   y_year[t-1]) /  frt_warminghalflife)) * ( et_equilibriumtemperature[t, r] -  rt_realizedtemperature[t-1, r])
  #   }
  # }
  # 
  # ## Adjusted realized temperature
  # 
  # # Adding adjustment, from Hope (2009)
  # for (r in 1:Region_No){
  #   rtl_realizedtemperature[t, r] <<-  rt_realizedtemperature[t, r] / (1. + (ocean_prop_ortion /  rlo_ratiolandocean) - ocean_prop_ortion) + rt_adj_temperatureadjustment[r]
  # }
  # 
  # # Equation 21 from Hope (2006): global realized temperature estimate
  # rtl_g_landtemperature[t] <<- sum( rtl_realizedtemperature[t, ]*  area) / sum( area)
  # 
  # # Ocean and global average temperature from Hope (2009)
  # rto_g_oceantemperature[t] <<-  rtl_g_landtemperature[t] /  rlo_ratiolandocean
  # rt_g_globaltemperature[t] <<- ocean_prop_ortion *  rto_g_oceantemperature[t] + (1. - ocean_prop_ortion) *  rtl_g_landtemperature[t]
  # 
  for (r in 1:Region_No_PAGE) {
   rtl_realizedtemperature[t,r]<<-rt_g_globaltemperature[t]*globaltoregionaltemp[r];
  }
}

# Sea Level rise
sltemp_SLtemprise = Best_Guess(datalist_PAGE[[143]]$Value)
sla_SLbaselinerise = Best_Guess(datalist_PAGE[[141]]$Value)
sltau_SLresponsetime = Best_Guess(datalist_PAGE[[142]]$Value)
s0_initialSL = Best_Guess(datalist_PAGE[[138]]$Value)
es_equilibriumSL=matrix(nrow =TimeStep_PAGE,ncol = 1 );
s_sealevel=matrix(nrow =TimeStep_PAGE,ncol = 1 );
expfs_exponential=matrix(nrow =TimeStep_PAGE,ncol = 1 );
yp_TimeStep_PAGE=matrix(nrow =TimeStep_PAGE,ncol = 1 );
Sea_level_rise<-function(){
  t=clock_Current(year_current)
  if(t==1){
    yp_TimeStep_PAGE[t]<<-y_year[1] - y_year_0
    es_equilibriumSL[t]<<-sltemp_SLtemprise*rt_g_globaltemperature[t] + sla_SLbaselinerise
    expfs_exponential[t]<<-exp(-yp_TimeStep_PAGE[t]/sltau_SLresponsetime)
    s_sealevel[t]<<-s0_initialSL + (es_equilibriumSL[t] - s0_initialSL)*(1-expfs_exponential[t])
  }
  else{
    yp_TimeStep_PAGE[t]<<-y_year[t] - y_year[t-1]
    es_equilibriumSL[t]<<-sltemp_SLtemprise*rt_g_globaltemperature[t] + sla_SLbaselinerise
    expfs_exponential[t]<<-exp(-yp_TimeStep_PAGE[t]/sltau_SLresponsetime)
    s_sealevel[t]<<-s_sealevel[t-1] + (es_equilibriumSL[t] -s_sealevel[t-1])*(1-expfs_exponential[t])
  }
}

# GDP
save_savingsrate = Best_Guess(datalist_PAGE[[139]]$Value) #pp33 PAGE09 documentation, "savings rate".
isat0_initialimpactfxnsaturation =  Best_Guess(datalist_PAGE[[125]]$Value) #pp34 PAGE09 documentation
gdp=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cons_consumption=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cons_percap_consumption=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cons_percap_consumption_0=matrix(nrow =1,ncol = Region_No_PAGE );
yagg_periodspan=matrix(nrow =TimeStep_PAGE,ncol = 1 );
grw_gdpgrowthrate=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
popgrw_populationgrowth=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
gdp_0=matrix(nrow =1,ncol = Region_No_PAGE );
pop0_initpopulation=matrix(nrow =1,ncol = Region_No_PAGE );
pop_population=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
isatg_impactfxnsaturation = isat0_initialimpactfxnsaturation * (1 - save_savingsrate/100);
for (r in 1:Region_No_PAGE){
  gdp_0[r]=Best_Guess(datalist_PAGE[[18]]$gdp_0[r]);
  pop0_initpopulation[r]=Best_Guess(datalist_PAGE[[35]]$pop_0[r]);
  cons_percap_consumption_0[r] = (gdp_0[r] / pop0_initpopulation[r])*(1 - save_savingsrate / 100)
}
for(t in 1:TimeStep_PAGE){
  for(r in 1:Region_No_PAGE){
    grw_gdpgrowthrate[t,r]=Best_Guess(datalist_PAGE[[19]][t,r+1]);
    popgrw_populationgrowth[t,r]=Best_Guess(datalist_PAGE[[36]][t,r+1]);
    #pop_population[t,r]=Best_Guess(datalist_PAGE[[61]][t,r+1]);
  }
}

GDP<-function(){
  t=clock_Current(year_current);
  if (t == 1){
    ylo_periodstart <<- y_year_0
  }
  else{
    ylo_periodstart <<- (y_year[t] + y_year[t-1]) / 2
  }
  
  if (t == length(y_year)){
    yhi_periodend <<- y_year[t]
  }
  else{
    yhi_periodend <<- (y_year[t] + y_year[t+1]) / 2
  }
  
  yagg_periodspan[t] <<- yhi_periodend- ylo_periodstart
  
  for (r in 1:Region_No_PAGE){
    #eq.28 in Hope 2002
    if (t == 1){
      gdp[t, r] <<- gdp_0[r] * (1 + (grw_gdpgrowthrate[t,r]/100))^(y_year[t] - y_year_0)
    }
    else{
      gdp[t, r] <<- gdp[t-1, r] * (1 + (grw_gdpgrowthrate[t,r]/100))^(y_year[t] - y_year[t-1])
    }
    
    
    gdp[t,r]<<-gdp_target[t,r];
    
    cons_consumption[t, r] <<- gdp[t, r] * (1 - save_savingsrate / 100)
    cons_percap_consumption[t, r] <<- cons_consumption[t, r] / pop_population[t, r]
  }
}

# Market damage
tcal_CalibrationTemp= Best_Guess(datalist_PAGE[[145]]$Value);
iben_MarketInitialBenefit = Best_Guess(datalist_PAGE[[118]]$Value)
ipow_MarketIncomeFxnExponent = Best_Guess(datalist_PAGE[[122]]$Value)
#save_savingsrate= Best_Guess(datalist_PAGE[[139]]$Value)
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
pow_MarketImpactExponent=Best_Guess(datalist_PAGE[[129]]$Value);
W_MarketImpactsatCalibrationTemp = Best_Guess(datalist_PAGE[[148]]$Value);
atl_adjustedtolerableleveloftemprise=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
imp_actualreduction=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
i_regionalimpact=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
rcons_per_cap_SLRRemainConsumption=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
rgdp_per_cap_SLRRemainGDP=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
WINCF_weightsfactor=matrix(nrow =1,ncol = Region_No_PAGE );
rcons_per_cap_MarketRemainConsumption=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
rgdp_per_cap_MarketRemainGDP=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
#ref_ImpactatReferenceGDPperCap=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
igdp_ImpactatActualGDPperCap=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
iref_ImpactatReferenceGDPperCap=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
impmax_maxtempriseforadaptpolicyM=matrix(nrow =1,ncol = Region_No_PAGE );
isat_ImpactinclSaturationandAdaptation_market=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
isat_per_cap_ImpactperCapinclSaturationandAdaptation=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
i_regionalimpact_market=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );

for(r in 1:Region_No_PAGE){
  WINCF_weightsfactor[r]=Best_Guess(datalist_PAGE[[152]][2,r]);
  impmax_maxtempriseforadaptpolicyM[r]=Best_Guess(datalist_PAGE[[21]]$impmax_1_a[r]);
}
# for(t in 1:TimeStep_PAGE){
#   for(r in 1:Region_No_PAGE){
#     rgdp_per_cap_SLRRemainGDP[t,r]=Best_Guess(datalist_PAGE[[58]][t,r+1]);
#     rcons_per_cap_SLRRemainConsumption[t,r]=Best_Guess(datalist_PAGE[[64]][t,r+1]);
#   }
# }
Market_damage<-function(){
  t=clock_Current(year_current);
  for (r in 1:Region_No_PAGE){
    #calculate tolerability
    imp_actualreduction[t,r]<<-Best_Guess(datalist_PAGE[[59]][t,r+1]);
    
    atl_adjustedtolerableleveloftemprise[t,r]<<-Best_Guess(datalist_PAGE[[54]][t,r+1]);
    if ((rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r]) < 0){
      i_regionalimpact[t,r] <<- 0
    }
    else{
      i_regionalimpact[t,r] <<- rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r]
    }
    
    iref_ImpactatReferenceGDPperCap[t,r]<<- WINCF_weightsfactor[r]*((W_MarketImpactsatCalibrationTemp + iben_MarketInitialBenefit * tcal_CalibrationTemp)*
                                                                      (i_regionalimpact[t,r]/tcal_CalibrationTemp)^pow_MarketImpactExponent - i_regionalimpact[t,r] * iben_MarketInitialBenefit)
    
    igdp_ImpactatActualGDPperCap[t,r]<<- iref_ImpactatReferenceGDPperCap[t,r]*
      (rgdp_per_cap_SLRRemainGDP[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_MarketIncomeFxnExponent
    
    if (igdp_ImpactatActualGDPperCap[t,r] < isatg_impactfxnsaturation){
      isat_ImpactinclSaturationandAdaptation_market[t,r] <<- igdp_ImpactatActualGDPperCap[t,r]
    }
    else{
      isat_ImpactinclSaturationandAdaptation_market[t,r] <<- isatg_impactfxnsaturation+
        ((100-save_savingsrate)-isatg_impactfxnsaturation)*
        ((igdp_ImpactatActualGDPperCap[t,r]-isatg_impactfxnsaturation)/
           (((100-save_savingsrate)-isatg_impactfxnsaturation)+
              (igdp_ImpactatActualGDPperCap[t,r]-
                 isatg_impactfxnsaturation)))
    }
    
    if (i_regionalimpact[t,r] < impmax_maxtempriseforadaptpolicyM[r]){
      isat_ImpactinclSaturationandAdaptation_market[t,r]<<-isat_ImpactinclSaturationandAdaptation_market[t,r]*(1-imp_actualreduction[t,r]/100)
    }
    else{
      isat_ImpactinclSaturationandAdaptation_market[t,r] <<- isat_ImpactinclSaturationandAdaptation_market[t,r] *
        (1-(imp_actualreduction[t,r]/100)* impmax_maxtempriseforadaptpolicyM[r] /
           i_regionalimpact[t,r])
    }
    i_regionalimpact_market[t,r]<<-i_regionalimpact[t,r];
    
    isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r] <<- (isat_ImpactinclSaturationandAdaptation_market[t,r]/100)*rgdp_per_cap_SLRRemainGDP[t,r]
    
    # isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r] <<- 0;
    
    rcons_per_cap_MarketRemainConsumption[t,r] <<- rcons_per_cap_SLRRemainConsumption[t,r] - isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r]
    rgdp_per_cap_MarketRemainGDP[t,r] <<- rcons_per_cap_MarketRemainConsumption[t,r]/(1-save_savingsrate/100)
  }
}

# non_market damage
#tcal_CalibrationTemp= Best_Guess(datalist_PAGE[[145]]$Value);
w_NonImpactsatCalibrationTemp = Best_Guess(datalist_PAGE[[149]]$Value);
iben_NonMarketInitialBenefit = Best_Guess(datalist_PAGE[[119]]$Value)
ipow_NonMarketIncomeFxnExponent = Best_Guess(datalist_PAGE[[123]]$Value)
#save_savingsrate= 15.
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
pow_NonMarketExponent = Best_Guess(datalist_PAGE[[130]]$Value)
impmax_maxtempriseforadaptpolicyNM=matrix(nrow =1,ncol = Region_No_PAGE );
i_regionalimpact_nonmarket=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
isat_ImpactinclSaturationandAdaptation_nonmarket=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );

for(r in 1:Region_No_PAGE){
  impmax_maxtempriseforadaptpolicyNM[r]=Best_Guess(datalist_PAGE[[22]]$impmax_2_a[r]);
}
rcons_per_cap_NonMarketRemainConsumption=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
rgdp_per_cap_NonMarketRemainGDP=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
# for(t in 1:TimeStep_PAGE){
#   for(r in 1:Region_No_PAGE){
#     #rcons_per_cap_NonMarketRemainConsumption[t,r]=Best_Guess(datalist_PAGE[[63]][t,r+1]);
#   #  rgdp_per_cap_NonMarketRemainGDP[t,r]=Best_Guess(datalist_PAGE[[57]][t,r+1]);
#   }
# }

non_market_damage<-function(){
  t=clock_Current(year_current);
  for (r in 1:Region_No_PAGE){
    imp_actualreduction[t,r]<<-Best_Guess(datalist_PAGE[[60]][t,r+1]);
    
    atl_adjustedtolerableleveloftemprise[t,r]<<-Best_Guess(datalist_PAGE[[55]][t,r+1]);
    
    if (rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r] < 0){
      i_regionalimpact[t,r] <<- 0
    }
    else{
      i_regionalimpact[t,r] <<- rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r]
    }
    
    iref_ImpactatReferenceGDPperCap[t,r]<<- WINCF_weightsfactor[r]*
      ((w_NonImpactsatCalibrationTemp + iben_NonMarketInitialBenefit *tcal_CalibrationTemp)*
         (i_regionalimpact[t,r]/tcal_CalibrationTemp)^pow_NonMarketExponent - i_regionalimpact[t,r] * iben_NonMarketInitialBenefit)
    
    igdp_ImpactatActualGDPperCap[t,r]<<- iref_ImpactatReferenceGDPperCap[t,r]*
      (rgdp_per_cap_MarketRemainGDP[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_NonMarketIncomeFxnExponent
    
    if (igdp_ImpactatActualGDPperCap[t,r] < isatg_impactfxnsaturation){
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] <<- igdp_ImpactatActualGDPperCap[t,r]
    }
    else{
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] <<- isatg_impactfxnsaturation+
        ((100-save_savingsrate)-isatg_impactfxnsaturation)*
        ((igdp_ImpactatActualGDPperCap[t,r]-isatg_impactfxnsaturation)/
           (((100-save_savingsrate)-isatg_impactfxnsaturation)+
              (igdp_ImpactatActualGDPperCap[t,r]-
                 isatg_impactfxnsaturation)))
    }
    
    if (i_regionalimpact[t,r] < impmax_maxtempriseforadaptpolicyNM[r]){
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r]<<-isat_ImpactinclSaturationandAdaptation_nonmarket[t,r]*(1-imp_actualreduction[t,r]/100)
    }
    else{
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] <<- isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] *
        (1-(imp_actualreduction[t,r]/100)* impmax_maxtempriseforadaptpolicyNM[r] /
           i_regionalimpact[t,r])
    }
    i_regionalimpact_nonmarket[t,r]<<-i_regionalimpact[t,r];
    
    isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r] <<- (isat_ImpactinclSaturationandAdaptation_nonmarket[t,r]/100)*rgdp_per_cap_MarketRemainGDP[t,r]
    
    # isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r] <<- 0;
    
    rcons_per_cap_NonMarketRemainConsumption[t,r] <<- rcons_per_cap_MarketRemainConsumption[t,r] - isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r]
    rgdp_per_cap_NonMarketRemainGDP[t,r] <<- rcons_per_cap_NonMarketRemainConsumption[t,r]/(1-save_savingsrate/100)
  }
}

#Discontinuity

rand_discontinuity = runif(1,0,1);# rand_discontinuity为0-1均匀分布
wdis_gdplostdisc=Best_Guess(datalist_PAGE[[151]]$Value);
ipow_incomeexponent=Best_Guess(datalist_PAGE[[121]]$Value);
distau_discontinuityexponent=Best_Guess(datalist_PAGE[[114]]$Value);
tdis_tolerabilitydisc=Best_Guess(datalist_PAGE[[147]]$Value);
pdis_probability=Best_Guess(datalist_PAGE[[127]]$Value);
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
isatg_saturationmodification=28.333333333333336;
irefeqdis_eqdiscimpact=matrix(nrow =1,ncol = Region_No_PAGE );
igdpeqdis_eqdiscimpact=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
igdp_realizeddiscimpact=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
occurdis_occurrencedummy=matrix(nrow =TimeStep_PAGE,ncol = 1 );
expfdis_discdecay=matrix(nrow =TimeStep_PAGE,ncol = 1 );
idis_lossfromdisc=matrix(nrow =TimeStep_PAGE,ncol = 1 );
isat_satdiscimpact=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
isat_per_cap_DiscImpactperCapinclSaturation=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
rcons_per_cap_DiscRemainConsumption=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );

Discontinuity<-function(){
  t=clock_Current(year_current)
  idis_lossfromdisc[t] <<- max(0, rt_g_globaltemperature[t] - tdis_tolerabilitydisc)
  
  if (t == 1){
    if (idis_lossfromdisc[t]*(pdis_probability/100) > rand_discontinuity){
      occurdis_occurrencedummy[t] <<- 1
    }
    else{
      occurdis_occurrencedummy[t] <<- 0
    }
    expfdis_discdecay[t]<<-exp(-(y_year[t] - y_year_0)/distau_discontinuityexponent)
  }
  else{
    if (idis_lossfromdisc[t]*(pdis_probability/100) > rand_discontinuity){
      occurdis_occurrencedummy[t] <<- 1
    }
    else if(occurdis_occurrencedummy[t-1] == 1) {
      occurdis_occurrencedummy[t] <<- 1
    }
    else{
      occurdis_occurrencedummy[t] <<- 0
    }
    expfdis_discdecay[t]<<-exp(-(y_year[t] - y_year[t-1])/distau_discontinuityexponent)
  }
  
  for (r in 1:Region_No_PAGE){
    irefeqdis_eqdiscimpact[r] <<- WINCF_weightsfactor[r]*wdis_gdplostdisc
    
    igdpeqdis_eqdiscimpact[t,r] <<- irefeqdis_eqdiscimpact[r] * (rgdp_per_cap_NonMarketRemainGDP[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_incomeexponent
    
    if (t==1){
      igdp_realizeddiscimpact[t,r]<<-occurdis_occurrencedummy[t]*(1-expfdis_discdecay[t])*igdpeqdis_eqdiscimpact[t,r]
    }
    else{
      igdp_realizeddiscimpact[t,r]<<-igdp_realizeddiscimpact[t-1,r]+occurdis_occurrencedummy[t]*(1-expfdis_discdecay[t])*(igdpeqdis_eqdiscimpact[t,r]-igdp_realizeddiscimpact[t-1,r])
    }
    
    if (igdp_realizeddiscimpact[t,r] < isatg_saturationmodification){
      isat_satdiscimpact[t,r] <<- igdp_realizeddiscimpact[t,r]
    }
    else{
      isat_satdiscimpact[t,r] <<- isatg_saturationmodification + (100-isatg_saturationmodification)*((igdp_realizeddiscimpact[t,r]-isatg_saturationmodification)/((100-isatg_saturationmodification)+(igdp_realizeddiscimpact[t,r] - isatg_saturationmodification)))
    }
    isat_per_cap_DiscImpactperCapinclSaturation[t,r] <<- (isat_satdiscimpact[t,r]/100)*rgdp_per_cap_NonMarketRemainGDP[t,r]
    
    # isat_per_cap_DiscImpactperCapinclSaturation[t,r] <<- 0;
    
    rcons_per_cap_DiscRemainConsumption[t,r] <<- rcons_per_cap_NonMarketRemainConsumption[t,r] - isat_per_cap_DiscImpactperCapinclSaturation[t,r]
  }
}

#adaptation cost
automult_autonomouschange = Best_Guess(datalist_PAGE[[102]]$Value)
cf_costregional=matrix(nrow =1,ncol = Region_No_PAGE );
impmax_maximumadaptivecapacity=matrix(nrow =1,ncol = Region_No_PAGE );
plateau_increaseintolerableplateaufromadaptation=matrix(nrow =1,ncol = Region_No_PAGE );
pstart_startdateofadaptpolicy=matrix(nrow =1,ncol = Region_No_PAGE );
pyears_yearstilfulleffect=matrix(nrow =1,ncol = Region_No_PAGE );
impred_eventualpercentreduction=matrix(nrow =1,ncol = Region_No_PAGE );
istart_startdate=matrix(nrow =1,ncol = Region_No_PAGE );
iyears_yearstilfulleffect=matrix(nrow =1,ncol = Region_No_PAGE );
atl_adjustedtolerablelevel=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
imp_adaptedimpacts=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
autofac_autonomouschangefraction=matrix(nrow =TimeStep_PAGE,ncol = 1 );
acp_adaptivecostplateau=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
aci_adaptivecostimpact=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
ac_adaptivecosts=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );

ac_adaptationcosts_economic=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
ac_adaptationcosts_noneconomic=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
ac_adaptationcosts_sealevelrise=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
for(r in 1:Region_No_PAGE){
  cf_costregional[r]=Best_Guess(datalist_PAGE[[107]][2,r]);
}

cp_costplateau_eu_SLR=Best_Guess(datalist_PAGE[[100]]$Value);

ci_costimpact_eu_SLR=Best_Guess(datalist_PAGE[[99]]$Value);

Adaptation_cost_sealevel<-function(){
  t=clock_Current(year_current );
  
  for(r in 1:Region_No_PAGE){
    plateau_increaseintolerableplateaufromadaptation[r]<<-Best_Guess(datalist_PAGE[[46]]$plateau_s_a[r]);
    pstart_startdateofadaptpolicy[r]<<-Best_Guess(datalist_PAGE[[47]]$pstart_s_a[r]);
    pyears_yearstilfulleffect[r]<<-Best_Guess(datalist_PAGE[[48]]$pyears_s_a[r]);
    impmax_maximumadaptivecapacity[r]<<-Best_Guess(datalist_PAGE[[24]]$impmax_s_a[r]);
    impred_eventualpercentreduction[r]<<-Best_Guess(datalist_PAGE[[49]]$impred_s_a[r]);
    istart_startdate[r]<<-Best_Guess(datalist_PAGE[[50]]$istart_s_a[r]);
    iyears_yearstilfulleffect[r]<<-Best_Guess(datalist_PAGE[[51]]$iyears_s_a[r]);
  }
  
  auto_autonomouschangepercent <<- (1 - automult_autonomouschange^(1/(y_year[TimeStep_PAGE] - y_year_0)))*100 # % per year
  autofac_autonomouschangefraction[t] <<- (1 - auto_autonomouschangepercent/100)^(y_year[t] - y_year_0) # Varies by year
  
  for (r in 1:Region_No_PAGE){
    #calculate adjusted tolerable level and max impact based on adaptation policy
    if ((y_year[t] - pstart_startdateofadaptpolicy[r]) < 0){
      atl_adjustedtolerablelevel[t,r]<<- 0
    }
    else if (((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r])<1.){
      atl_adjustedtolerablelevel[t,r]<<-
        ((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r]) *
        plateau_increaseintolerableplateaufromadaptation[r]
    }
    else{
      atl_adjustedtolerablelevel[t,r] <<- plateau_increaseintolerableplateaufromadaptation[r]
    }
    
    if ((y_year[t]- istart_startdate[r]) < 0){
      imp_adaptedimpacts[t,r] <<- 0
    }
    else if (((y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]) < 1){
      imp_adaptedimpacts[t,r] <<-
        (y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]*
        impred_eventualpercentreduction[r]
    }
    else{
      imp_adaptedimpacts[t,r] <<- impred_eventualpercentreduction[r]
    }
    
    # Hope (2009),  25, equations 1-2
    cp_costplateau_regional <<- cp_costplateau_eu_SLR * cf_costregional[r]
    ci_costimpact_regional <<- ci_costimpact_eu_SLR * cf_costregional[r]
    
    # Hope (2009),  25, equations 3-4
    acp_adaptivecostplateau[t, r] <<- atl_adjustedtolerablelevel[t, r] * cp_costplateau_regional * gdp[t, r] * autofac_autonomouschangefraction[t] / 100
    aci_adaptivecostimpact[t, r] <<- imp_adaptedimpacts[t, r] * ci_costimpact_regional * gdp[t, r] * impmax_maximumadaptivecapacity[r] * autofac_autonomouschangefraction[t] / 100
    
    # Hope (2009),  25, equation 5
    ac_adaptivecosts[t, r] <<- acp_adaptivecostplateau[t, r] + aci_adaptivecostimpact[t, r]
    ac_adaptationcosts_sealevelrise[t,r]<<- ac_adaptivecosts[t, r]
  }
  
}

cp_costplateau_eu_economic=Best_Guess(datalist_PAGE[[96]]$Value);

ci_costimpact_eu_economic=Best_Guess(datalist_PAGE[[95]]$Value);


Adaptation_cost_economic<-function(){
  t=clock_Current(year_current );
  
  for(r in 1:Region_No_PAGE){
    impmax_maximumadaptivecapacity[r]<<-Best_Guess(datalist_PAGE[[20]]$impmax_1_a[r]);
    plateau_increaseintolerableplateaufromadaptation[r]=Best_Guess(datalist_PAGE[[33]]$plateau_1_a[r]);
    pstart_startdateofadaptpolicy[r]<<-Best_Guess(datalist_PAGE[[38]]$pstart_1_a[r]);
    pyears_yearstilfulleffect[r]<<-Best_Guess(datalist_PAGE[[40]]$pyears_1_a[r]);
    impred_eventualpercentreduction[r]<<-Best_Guess(datalist_PAGE[[25]]$impred_1_a[r]);
    istart_startdate[r]<<-Best_Guess(datalist_PAGE[[27]]$istart_1_a[r]);
    iyears_yearstilfulleffect[r]<<-Best_Guess(datalist_PAGE[[29]]$iyears_1_a[r]);
  }
  auto_autonomouschangepercent <<- (1 - automult_autonomouschange^(1/(y_year[TimeStep_PAGE] - y_year_0)))*100 # % per year
  autofac_autonomouschangefraction[t] <<- (1 - auto_autonomouschangepercent/100)^(y_year[t] - y_year_0) # Varies by year
  
  for (r in 1:Region_No_PAGE){
    #calculate adjusted tolerable level and max impact based on adaptation policy
    if ((y_year[t] - pstart_startdateofadaptpolicy[r]) < 0){
      atl_adjustedtolerablelevel[t,r]<<- 0
    }
    else if (((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r])<1.){
      atl_adjustedtolerablelevel[t,r]<<-
        ((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r]) *
        plateau_increaseintolerableplateaufromadaptation[r]
    }
    else{
      atl_adjustedtolerablelevel[t,r] <<- plateau_increaseintolerableplateaufromadaptation[r]
    }
    
    if ((y_year[t]- istart_startdate[r]) < 0){
      imp_adaptedimpacts[t,r] <<- 0
    }
    else if (((y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]) < 1){
      imp_adaptedimpacts[t,r] <<-
        (y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]*
        impred_eventualpercentreduction[r]
    }
    else{
      imp_adaptedimpacts[t,r] <<- impred_eventualpercentreduction[r]
    }
    
    # Hope (2009),  25, equations 1-2
    cp_costplateau_regional <<- cp_costplateau_eu_economic * cf_costregional[r]
    ci_costimpact_regional <<- ci_costimpact_eu_economic * cf_costregional[r]
    
    # Hope (2009),  25, equations 3-4
    acp_adaptivecostplateau[t, r] <<- atl_adjustedtolerablelevel[t, r] * cp_costplateau_regional * gdp[t, r] * autofac_autonomouschangefraction[t] / 100
    aci_adaptivecostimpact[t, r] <<- imp_adaptedimpacts[t, r] * ci_costimpact_regional * gdp[t, r] * impmax_maximumadaptivecapacity[r] * autofac_autonomouschangefraction[t] / 100
    
    # Hope (2009),  25, equation 5
    ac_adaptivecosts[t, r] <<- acp_adaptivecostplateau[t, r] + aci_adaptivecostimpact[t, r]
    ac_adaptationcosts_economic[t,r]<<- ac_adaptivecosts[t, r]
    
  }
  
}

cp_costplateau_eu_noneconomic=Best_Guess(datalist_PAGE[[98]]$Value);
ci_costimpact_eu_noneconomic=Best_Guess(datalist_PAGE[[97]]$Value);

Adaptation_cost_noneconomic<-function(){
  t=clock_Current(year_current );
  for(r in 1:Region_No_PAGE){
    impmax_maximumadaptivecapacity[r]<<-Best_Guess(datalist_PAGE[[23]]$impmax_2_a[r]);
    plateau_increaseintolerableplateaufromadaptation[r]=Best_Guess(datalist_PAGE[[34]]$plateau_2_a[r]);
    pstart_startdateofadaptpolicy[r]<<-Best_Guess(datalist_PAGE[[39]]$pstart_2_a[r]);
    pyears_yearstilfulleffect[r]<<-Best_Guess(datalist_PAGE[[41]]$pyears_2_a[r]);
    impred_eventualpercentreduction[r]<<-Best_Guess(datalist_PAGE[[26]]$impred_2_a[r]);
    istart_startdate[r]<<-Best_Guess(datalist_PAGE[[28]]$istart_2_a[r]);
    iyears_yearstilfulleffect[r]<<-Best_Guess(datalist_PAGE[[30]]$iyears_2_a[r]);
  }
  auto_autonomouschangepercent <<- (1 - automult_autonomouschange^(1/(y_year[TimeStep_PAGE] - y_year_0)))*100 # % per year
  autofac_autonomouschangefraction[t] <<- (1 - auto_autonomouschangepercent/100)^(y_year[t] - y_year_0) # Varies by year
  
  for (r in 1:Region_No_PAGE){
    #calculate adjusted tolerable level and max impact based on adaptation policy
    if ((y_year[t] - pstart_startdateofadaptpolicy[r]) < 0){
      atl_adjustedtolerablelevel[t,r]<<- 0
    }
    else if (((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r])<1.){
      atl_adjustedtolerablelevel[t,r]<<-
        ((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r]) *
        plateau_increaseintolerableplateaufromadaptation[r]
    }
    else{
      atl_adjustedtolerablelevel[t,r] <<- plateau_increaseintolerableplateaufromadaptation[r]
    }
    
    if ((y_year[t]- istart_startdate[r]) < 0){
      imp_adaptedimpacts[t,r] <<- 0
    }
    else if (((y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]) < 1){
      imp_adaptedimpacts[t,r] <<-
        (y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]*
        impred_eventualpercentreduction[r]
    }
    else{
      imp_adaptedimpacts[t,r] <<- impred_eventualpercentreduction[r]
    }
    
    # Hope (2009),  25, equations 1-2
    cp_costplateau_regional <<- cp_costplateau_eu_noneconomic * cf_costregional[r]
    ci_costimpact_regional <<- ci_costimpact_eu_noneconomic * cf_costregional[r]
    
    # Hope (2009),  25, equations 3-4
    acp_adaptivecostplateau[t, r] <<- atl_adjustedtolerablelevel[t, r] * cp_costplateau_regional * gdp[t, r] * autofac_autonomouschangefraction[t] / 100
    aci_adaptivecostimpact[t, r] <<- imp_adaptedimpacts[t, r] * ci_costimpact_regional * gdp[t, r] * impmax_maximumadaptivecapacity[r] * autofac_autonomouschangefraction[t] / 100
    
    # Hope (2009),  25, equation 5
    ac_adaptivecosts[t, r] <<- acp_adaptivecostplateau[t, r] + aci_adaptivecostimpact[t, r]
    ac_adaptationcosts_noneconomic[t,r]<<- ac_adaptivecosts[t, r]
    
  }
  
}

#SLR damage
pow_SLRImpactFxnExponent = Best_Guess(datalist_PAGE[[131]]$Value)
ipow_SLRIncomeFxnExponent = Best_Guess(datalist_PAGE[[124]]$Value)
iben_SLRInitialBenefit = 0.00
scal_calibrationSLR = Best_Guess(datalist_PAGE[[140]]$Value)
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
W_SatCalibrationSLR = Best_Guess(datalist_PAGE[[150]]$Value) #pp33 PAGE09 documentation, "Sea level impact at calibration sea level rise"
#save_savingsrate = 15.00 #pp33 PAGE09 documentation, "savings rate".
tct_per_cap_totalcostspercap=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
act_percap_adaptationcosts=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
impmax_maxSLRforadaptpolicySLR=matrix(nrow =1,ncol = Region_No_PAGE );
cons_percap_aftercosts=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
gdp_percap_aftercosts=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
atl_adjustedtolerablelevelofsealevelrise=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
imp_actualreductionSLR=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
i_regionalimpactSLR=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
iref_ImpactatReferenceGDPperCapSLR=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
igdp_ImpactatActualGDPperCapSLR=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
isat_ImpactinclSaturationandAdaptationSLR=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
isat_per_cap_SLRImpactperCapinclSaturationandAdaptation=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
for(r in 1:Region_No_PAGE){
  impmax_maxSLRforadaptpolicySLR[r]=Best_Guess(datalist_PAGE[[52]]$Sea.level.max.rise[r]);
}
for(t in 1:TimeStep_PAGE){
  for(r in 1:Region_No_PAGE){
    atl_adjustedtolerablelevelofsealevelrise[t,r]=Best_Guess(datalist_PAGE[[65]][t,r+1]);
    #  tct_per_cap_totalcostspercap[t,r]=Best_Guess(datalist_PAGE[[66]][t,r+1]);
    #  act_percap_adaptationcosts[t,r]=Best_Guess(datalist_PAGE[[67]][t,r+1]);
    imp_actualreductionSLR[t,r]=Best_Guess(datalist_PAGE[[68]][t,r+1]);
  }
}
SLR_damage<-function(){
  t=clock_Current(year_current );
  for (r in 1:Region_No_PAGE){
    cons_percap_aftercosts[t, r] <<- cons_percap_consumption[t, r] - tct_per_cap_totalcostspercap[t, r] - act_percap_adaptationcosts[t, r]
    gdp_percap_aftercosts[t,r]<<-cons_percap_aftercosts[t, r]/(1 - save_savingsrate/100)
    
    if ((s_sealevel[t]-atl_adjustedtolerablelevelofsealevelrise[t,r]) < 0){
      i_regionalimpactSLR[t,r] <<- 0
    }
    else{
      i_regionalimpactSLR[t,r] <<- s_sealevel[t]-atl_adjustedtolerablelevelofsealevelrise[t,r]
    }
    
    iref_ImpactatReferenceGDPperCapSLR[t,r]<<- WINCF_weightsfactor[r]*((W_SatCalibrationSLR + iben_SLRInitialBenefit * scal_calibrationSLR)*
                                                                         (i_regionalimpactSLR[t,r]/scal_calibrationSLR)^pow_SLRImpactFxnExponent - i_regionalimpactSLR[t,r] * iben_SLRInitialBenefit)
    
    igdp_ImpactatActualGDPperCapSLR[t,r]<<- iref_ImpactatReferenceGDPperCapSLR[t,r]*
      (gdp_percap_aftercosts[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_SLRIncomeFxnExponent
    
    if (igdp_ImpactatActualGDPperCapSLR[t,r] < isatg_impactfxnsaturation){
      isat_ImpactinclSaturationandAdaptationSLR[t,r] <<- igdp_ImpactatActualGDPperCapSLR[t,r]
    }
    else{
      isat_ImpactinclSaturationandAdaptationSLR[t,r] <<- isatg_impactfxnsaturation+
        ((100-save_savingsrate)-isatg_impactfxnsaturation)*
        ((igdp_ImpactatActualGDPperCapSLR[t,r]-isatg_impactfxnsaturation)/
           (((100-save_savingsrate)-isatg_impactfxnsaturation)+
              (igdp_ImpactatActualGDPperCapSLR[t,r]- isatg_impactfxnsaturation)))
    }
    if (i_regionalimpactSLR[t,r] < impmax_maxSLRforadaptpolicySLR[r]){
      isat_ImpactinclSaturationandAdaptationSLR[t,r]<<-isat_ImpactinclSaturationandAdaptationSLR[t,r]*(1-imp_actualreductionSLR[t,r]/100)
    }
    else{
      isat_ImpactinclSaturationandAdaptationSLR[t,r]<<-isat_ImpactinclSaturationandAdaptationSLR[t,r]*(1-(imp_actualreductionSLR[t,r]/100)* impmax_maxSLRforadaptpolicySLR[r] /
                                                                                                         i_regionalimpactSLR[t,r])
    }
    
    isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t,r] <<- (isat_ImpactinclSaturationandAdaptationSLR[t,r]/100)*gdp_percap_aftercosts[t,r]
    
    # isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t,r] <<- 0;
    
    rcons_per_cap_SLRRemainConsumption[t,r] <<- cons_percap_aftercosts[t,r] - isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t,r]
    rgdp_per_cap_SLRRemainGDP[t,r] <<- rcons_per_cap_SLRRemainConsumption[t,r]/(1-save_savingsrate/100)
  } 
}

# Abatement cost
q0propmult_cutbacksatnegativecostinfinalyear = Best_Guess(datalist_PAGE[[134]]$Value)
qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear = Best_Guess(datalist_PAGE[[135]]$Value)
c0mult_mostnegativecostinfinalyear = Best_Guess(datalist_PAGE[[104]]$Value)
curve_below_curvatureofMACcurvebelowzerocost = Best_Guess(datalist_PAGE[[112]]$Value)
curve_above_curvatureofMACcurveabovezerocost = Best_Guess(datalist_PAGE[[111]]$Value)
cross_experiencecrossoverratio = Best_Guess(datalist_PAGE[[110]]$Value)
learn_learningrate = Best_Guess(datalist_PAGE[[126]]$Value)
automult_autonomoustechchange = Best_Guess(datalist_PAGE[[102]]$Value)
equity_prop_equityweightsproportion = 1.

er_emissionsgrowth=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
e0_baselineemissions=matrix(nrow =1,ncol = Region_No_PAGE );
emitf_uncertaintyinBAUemissfactor=matrix(nrow =1,ncol = Region_No_PAGE );
q0f_negativecostpercentagefactor=matrix(nrow =1,ncol = Region_No_PAGE );
cmaxf_maxcostfactor=matrix(nrow =1,ncol = Region_No_PAGE );
bau_businessasusualemissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
yagg_periodspan=matrix(nrow =TimeStep_PAGE,ncol = 1 );
emit_UncertaintyinBAUEmissFactor=matrix(nrow =1,ncol = Region_No_PAGE );
q0propinit_CutbacksinNegativeCostinBaseYear=matrix(nrow =1,ncol = Region_No_PAGE );
cmaxinit_MaxCutbackCostinBaseYear=matrix(nrow =1,ncol = Region_No_PAGE );
zc_zerocostemissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cb_reductionsfromzerocostemissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cbe_absoluteemissionreductions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cumcbe_cumulativereductionssincebaseyear=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cumcbe_g_totalreductions=matrix(nrow =TimeStep_PAGE,ncol = 1 );
learnfac_learning=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
autofac=matrix(nrow =TimeStep_PAGE,ncol = 1 );
c0=matrix(nrow =TimeStep_PAGE,ncol = 1 );
q0prop=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
q0_absolutecutbacksatnegativecost=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
qmax_maxreferencereductions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
cmax=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
blo=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
alo=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
bhi=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
ahi=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
mc_marginalcost=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
tcq0=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
tc_totalcost=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
bau_co2emissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
bau_ch4emissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
bau_n2oemissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
bau_linemissions=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
for(r in 1:Region_No_PAGE){
  emitf_uncertaintyinBAUemissfactor[r]=Best_Guess(datalist_PAGE[[115]][2,r]);
  q0f_negativecostpercentagefactor[r]=Best_Guess(datalist_PAGE[[133]][2,r]);
  cmaxf_maxcostfactor[r]=Best_Guess(datalist_PAGE[[109]][2,r]);
}
for(t in 1:TimeStep_PAGE){
  #  yagg_periodspan[t]=Best_Guess(datalist_PAGE[[69]][t,2]);
  for(r in 1:Region_No_PAGE){
    bau_co2emissions[t,r]=Best_Guess(datalist_PAGE[[3]][t,r+1]);
    bau_ch4emissions[t,r]=Best_Guess(datalist_PAGE[[2]][t,r+1]);
    bau_n2oemissions[t,r]=Best_Guess(datalist_PAGE[[5]][t,r+1]);
    bau_linemissions[t,r]=Best_Guess(datalist_PAGE[[4]][t,r+1]);
    
  }
}
tc_totalcosts_co2=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
tc_totalcosts_ch4=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
tc_totalcosts_n2o=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
tc_totalcosts_linear=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CO2=Best_Guess(datalist_PAGE[[79]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CO2=Best_Guess(datalist_PAGE[[81]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_CO2= Best_Guess(datalist_PAGE[[77]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CO2=Best_Guess(datalist_PAGE[[82]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CO2 =Best_Guess(datalist_PAGE[[78]]$Value)
ies_InitialExperienceStockofCutbacks_CO2 =Best_Guess(datalist_PAGE[[80]]$Value)
Abatement_cost_CO2<-function(){
  
  er_emissionsgrowth<<-er_CO2emissionsgrowth;
  e0_baselineemissions<<-e0_baselineCO2emissions;
  bau_businessasusualemissions<<-bau_co2emissions;
  t=clock_Current(year_current)
  for (r in 1:Region_No_PAGE){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CO2 *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CO2 *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CO2 *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep_PAGE]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep_PAGE]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_CO2* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep_PAGE]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CO2 * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  
  for (r in 1:Region_No_PAGE){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_CO2)/  ies_InitialExperienceStockofCutbacks_CO2)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_co2[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_co2[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}


emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CH4= Best_Guess(datalist_PAGE[[73]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CH4 = Best_Guess(datalist_PAGE[[75]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_CH4 = Best_Guess(datalist_PAGE[[71]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CH4 = Best_Guess(datalist_PAGE[[76]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CH4 = Best_Guess(datalist_PAGE[[72]]$Value)
ies_InitialExperienceStockofCutbacks_CH4 = Best_Guess(datalist_PAGE[[74]]$Value)

Abatement_cost_CH4<-function(){
  er_emissionsgrowth=er_CH4emissionsgrowth;
  e0_baselineemissions=e0_baselineCH4emissions;
  bau_businessasusualemissions=bau_ch4emissions;
  
  t=clock_Current(year_current)
  for (r in 1:Region_No_PAGE){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CH4 *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CH4 *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CH4 *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep_PAGE]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep_PAGE]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_CH4* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep_PAGE]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CH4 * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  
  for (r in 1:Region_No_PAGE){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_CH4)/  ies_InitialExperienceStockofCutbacks_CH4)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_ch4[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_ch4[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}


emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_N2O<<-Best_Guess(datalist_PAGE[[91]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_N2O<<-Best_Guess(datalist_PAGE[[93]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_N2O<<- Best_Guess(datalist_PAGE[[89]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_N2O<<-Best_Guess(datalist_PAGE[[94]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_N2O <<-Best_Guess(datalist_PAGE[[90]]$Value)
ies_InitialExperienceStockofCutbacks_N2O <<-Best_Guess(datalist_PAGE[[92]]$Value)

Abatement_cost_N2O<-function(){
  er_emissionsgrowth<<-er_N2Oemissionsgrowth;
  e0_baselineemissions<<-e0_baselineN2Oemissions;
  bau_businessasusualemissions<<-bau_n2oemissions;
  t=clock_Current(year_current)
  for (r in 1:Region_No_PAGE){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_N2O *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_N2O *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_N2O *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep_PAGE]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep_PAGE]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_N2O* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep_PAGE]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_N2O * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  
  for (r in 1:Region_No_PAGE){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_N2O)/  ies_InitialExperienceStockofCutbacks_N2O)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_n2o[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_n2o[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}

emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_LG = Best_Guess(datalist_PAGE[[85]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_LG=Best_Guess(datalist_PAGE[[87]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_LG=Best_Guess(datalist_PAGE[[83]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_LG=Best_Guess(datalist_PAGE[[88]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_LG=Best_Guess(datalist_PAGE[[84]]$Value)
ies_InitialExperienceStockofCutbacks_LG=Best_Guess(datalist_PAGE[[86]]$Value)
Abatement_cost_LG<-function(){
  
  
  er_emissionsgrowth<<-er_LGemissionsgrowth;
  e0_baselineemissions<<-e0_baselineLGemissions;
  bau_businessasusualemissions<<-bau_linemissions;
  t=clock_Current(year_current)
  for (r in 1:Region_No_PAGE){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_LG *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_LG *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_LG *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep_PAGE]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep_PAGE]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_LG* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep_PAGE]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_LG * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep_PAGE]- y_year_0))-1)*100
  
  for (r in 1:Region_No_PAGE){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_LG)/  ies_InitialExperienceStockofCutbacks_LG)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_linear[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_linear[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}

# TotalAbatementCosts

tct_totalcosts=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
#tct_per_cap_totalcostspercap=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );

TotalAbatementCosts<-function(){
  t=clock_Current(year_current)
  for (r in 1:Region_No_PAGE){
    tct_totalcosts[t,r] <<- tc_totalcosts_co2[t,r] + tc_totalcosts_n2o[t,r] + tc_totalcosts_ch4[t,r] + tc_totalcosts_linear[t,r]
    tct_per_cap_totalcostspercap[t,r] <<- tct_totalcosts[t,r]/pop_population[t,r]
    
    tct_totalcosts[t,r] <<- 0;
    tct_per_cap_totalcostspercap[t,r] <<-0;
  }
}

# TotalAdaptationCosts

act_adaptationcosts_total=matrix(nrow =TimeStep_PAGE,ncol = Region_No_PAGE );
TotalAdaptationCosts<-function(){
  t=clock_Current(year_current)
  for (r in 1:Region_No_PAGE){
    act_adaptationcosts_total[t,r] <<- ac_adaptationcosts_economic[t,r] + ac_adaptationcosts_sealevelrise[t,r] + ac_adaptationcosts_noneconomic[t,r]
    act_percap_adaptationcosts[t,r] <<- act_adaptationcosts_total[t,r]/pop_population[t,r]
  }
}



# population
# 
#pop_population=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)

Population<-function(){
  t=clock_Current(year_current);
  for (r in 1:Region_No_PAGE){
    # Eq.28 in Hope 2002 (defined for GDP, but also applies to population)
    if (t == 1){
      pop_population[t, r] <<- pop0_initpopulation[r] * (1 + popgrw_populationgrowth[t, r]/100)^(y_year[t] - y_year_0)
    }
    else{
      pop_population[t, r] <<- pop_population[t-1, r] * (1 + popgrw_populationgrowth[t, r]/100)^(y_year[t] - y_year[t-1])
    }
    pop_population[t, r]<<-pop_target[t,r];
    
  }
}


# equlity weight
ptp_timepreference = Best_Guess(datalist_PAGE[[132]]$Value) # <0.1,1, 2>
equity_proportion = 1.0
emuc_utilityconvexity = Best_Guess(datalist_PAGE[[116]]$Value) ;
civvalue_civilizationvalue = Best_Guess(datalist_PAGE[[108]]$Value)

ptp_timepreference = 2.5 # <0.1,1, 2>
equity_proportion = 1.0
emuc_utilityconvexity = 0

#tct_percap_totalcosts_total=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
# act_adaptationcosts_total=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
# act_percap_adaptationcosts=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
wtct_percap_weightedcosts=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
eact_percap_weightedadaptationcosts=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
wact_percap_partiallyweighted=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
wact_partiallyweighted=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
pct_percap_partiallyweighted=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
pct_partiallyweighted=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
pct_g_partiallyweighted_global=matrix(nrow = TimeStep_PAGE,ncol = 1)
dr_discountrate=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
yp_yearsperiod=matrix(nrow = TimeStep_PAGE,ncol = 1)
dfc_consumptiondiscountrate=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
dfc_consumptiondiscountrate_ces=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
df_utilitydiscountrate=matrix(nrow = TimeStep_PAGE,ncol = 1)
pcdt_partiallyweighted_discounted=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
pcdt_g_partiallyweighted_discountedglobal=matrix(nrow = TimeStep_PAGE,ncol = 1)
pcdat_partiallyweighted_discountedaggregated=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
wacdt_partiallyweighted_discounted=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
#rcons_percap_dis=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
wit_equityweightedimpact=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
widt_equityweightedimpact_discounted=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
addt_equityweightedimpact_discountedaggregated=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
aact_equityweightedadaptation_discountedaggregated=matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
# for(t in 1:TimeStep_PAGE){
#   for(r in 1:Region_No_PAGE){
#     rcons_percap_dis[t,r]=Best_Guess(datalist_PAGE[[70]][t,r+1]);
#   }
# }


equalityweighting<-function(){
  t=clock_Current(year_current);
  if (t == 1){
    tpc_totalaggregatedcosts <<- 0
    addt_gt_equityweightedimpact_discountedglobal <<- 0
    tac_totaladaptationcosts <<- 0
    te_totaleffect <<- 0
  }
  
  # 对减排成本和适应成本进行贴现，只在equity_proportion不等于0时起作用
  # need to adjust the discount rate when changing the simulation year of SCC
  # df_utilitydiscountrate[t] <<- (1 + ptp_timepreference / 100)^(-(y_year[t] - y_year_0))
  if(t < simulation_year){
    df_utilitydiscountrate[t] <<-1;
  }else{
    df_utilitydiscountrate[t] <<- (1 + ptp_timepreference / 100)^(-(y_year[t] - y_year[simulation_year]))
  }
  
  for (r in 1:Region_No_PAGE){
    
    ## Gas Costs Accounting
    # Weighted costs (Page 23 of Hope 2009)
    wtct_percap_weightedcosts[t, r] <<- ((cons_percap_consumption_0[1]^emuc_utilityconvexity) / (1 - emuc_utilityconvexity)) * (cons_percap_consumption[t, r]^(1 - emuc_utilityconvexity) - (cons_percap_consumption[t, r] - tct_per_cap_totalcostspercap[t, r])^(1 - emuc_utilityconvexity))
    
    # Add these into consumption
    eact_percap_weightedadaptationcosts[t, r] <<- ((cons_percap_consumption_0[1]^emuc_utilityconvexity) / (1 - emuc_utilityconvexity)) * (cons_percap_consumption[t, r]^(1 - emuc_utilityconvexity) - (cons_percap_consumption[t, r] - act_percap_adaptationcosts[t, r])^(1 - emuc_utilityconvexity))
    
    # Do partial weighting
    if (equity_proportion == 0){
      pct_percap_partiallyweighted[t, r] <<- tct_per_cap_totalcostspercap[t, r]
      wact_percap_partiallyweighted[t, r] <<- act_percap_adaptationcosts[t, r]
    }
    else{
      pct_percap_partiallyweighted[t, r] <<- (1 - equity_proportion) * tct_per_cap_totalcostspercap[t, r] + equity_proportion * wtct_percap_weightedcosts[t, r]
      wact_percap_partiallyweighted[t, r] <<- (1 - equity_proportion) * act_percap_adaptationcosts[t, r] + equity_proportion * eact_percap_weightedadaptationcosts[t, r]
    }
    
    pct_partiallyweighted[t, r] <<- pct_percap_partiallyweighted[t, r] * pop_population[t, r]
    wact_partiallyweighted[t, r] <<- wact_percap_partiallyweighted[t, r] * pop_population[t, r]
    
    # Discount rate calculations
    # dr_discountrate[t, r] <<- ptp_timepreference + emuc_utilityconvexity * (grw_gdpgrowthrate[t, r] - popgrw_populationgrowth[t, r])
    if (t == 1){
      yp_yearsperiod[1] <<- y_year[1] - y_year_0
    }else{
      yp_yearsperiod[t] <<- y_year[t] - y_year[t-1]
    }
    
    # 对气候变化损失进行贴现
    #计算2020年碳社会成本需要改变贴现率计算方法
    # if(t==1){
    #   dr_discountrate[t, r]<<-0;
    # }else{
    # dr_discountrate[t, r] <<- ptp_timepreference + risk_aversion * 100*(((gdp[t, r]/pop_population[t,r])/(gdp[t-1, r]/pop_population[t-1,r]))^(1/yp_yearsperiod[t])-1)
    # }
    # if (t == 1){
    #   dfc_consumptiondiscountrate[t, r] <<- (1 + dr_discountrate[1, r] / 100)^(-(yp_yearsperiod[3]))
    # }
    # else{
    #   dfc_consumptiondiscountrate[t, r] <<- dfc_consumptiondiscountrate[t - 1, r] * (1 + dr_discountrate[t, r] / 100)^(-yp_yearsperiod[t])
    # }
    globalconsumption_percap_afterdamage <<- rowSums(rcons_per_cap_DiscRemainConsumption * pop_population)/rowSums(pop_population)

    if(certainty_eq == "false"){
      dfc_consumptiondiscountrate[t, r] <<- 1/((1+ptp_timepreference/100)^(y_year[t]-y_year[simulation_year]))*((globalconsumption_percap_afterdamage[t]/globalconsumption_percap_afterdamage[simulation_year])^(0-risk_aversion))
    }
    if(certainty_eq == "true"){
      dfc_consumptiondiscountrate[t, r] <<- 1/((1+ptp_timepreference/100)^(y_year[t]-y_year[simulation_year]))*((globalconsumption_percap_afterdamage[t])^(0-risk_aversion))
    }
    dfc_consumptiondiscountrate[1:simulation_year,] <<- 0
    
    # Discounted costs
    if (equity_proportion == 0){
      pcdt_partiallyweighted_discounted[t, r] <<- pct_partiallyweighted[t, r] * dfc_consumptiondiscountrate[t, r]
      wacdt_partiallyweighted_discounted[t, r] <<- act_adaptationcosts_total[t, r] * dfc_consumptiondiscountrate[t, r]
    }else{
      pcdt_partiallyweighted_discounted[t, r] <<- pct_partiallyweighted[t, r] * df_utilitydiscountrate[t]
      wacdt_partiallyweighted_discounted[t, r] <<- wact_partiallyweighted[t, r] * df_utilitydiscountrate[t]
    }
    
    pcdat_partiallyweighted_discountedaggregated[t, r] <<- pcdt_partiallyweighted_discounted[t, r] * yagg_periodspan[t]
    
    ## Equity weighted impacts (end of page 28, Hope 2009)
    wit_equityweightedimpact[t, r] <<- ((cons_percap_consumption_0[1]^emuc_utilityconvexity) / (1 - emuc_utilityconvexity)) * (cons_percap_aftercosts[t, r]^(1 - emuc_utilityconvexity) - rcons_per_cap_DiscRemainConsumption[t, r]^(1 - emuc_utilityconvexity)) * pop_population[t, r]
    
    # widt_equityweightedimpact_discounted[t, r] <<- wit_equityweightedimpact[t, r] * df_utilitydiscountrate[t]
    widt_equityweightedimpact_discounted[t, r] <<- wit_equityweightedimpact[t, r] * dfc_consumptiondiscountrate[t,r]
    
    addt_equityweightedimpact_discountedaggregated[t, r] <<- widt_equityweightedimpact_discounted[t, r] * yagg_periodspan[t]
    aact_equityweightedadaptation_discountedaggregated[t, r] <<- wacdt_partiallyweighted_discounted[t, r] * yagg_periodspan[t]
  }
  
  pct_g_partiallyweighted_global[t] <<- sum(pct_partiallyweighted[t, ])
  pcdt_g_partiallyweighted_discountedglobal[t] <<- sum(pcdt_partiallyweighted_discounted[t, ])
  tpc_totalaggregatedcosts <<- tpc_totalaggregatedcosts + sum(pcdat_partiallyweighted_discountedaggregated[t, ])
  
  addt_gt_equityweightedimpact_discountedglobal <<- addt_gt_equityweightedimpact_discountedglobal + sum(addt_equityweightedimpact_discountedaggregated[t, ])
  
  tac_totaladaptationcosts <<- tac_totaladaptationcosts + sum(aact_equityweightedadaptation_discountedaggregated[t, ])
  
  td_totaldiscountedimpacts <<- min(addt_gt_equityweightedimpact_discountedglobal, civvalue_civilizationvalue)
  
  # Total effect of climate change
  te_totaleffect <<- min(td_totaldiscountedimpacts + tpc_totalaggregatedcosts + tac_totaladaptationcosts, civvalue_civilizationvalue)
  
}

playground_page_damage<-function(ptp_timepreference,equity_proportion,certainty_eq,emuc_utilityconvexity,risk_aversion){
  ptp_timepreference <<- ptp_timepreference
  equity_proportion <<- equity_proportion
  emuc_utilityconvexity <<- emuc_utilityconvexity
  risk_aversion <<- risk_aversion
  certainty_eq <<- certainty_eq
  
  for(i in 1:TimeStep){
    year_current<<-i;
    ClimateTemperature();
    #Sea_level_rise();
    Population();
    GDP();
    Abatement_cost_CO2();
    Abatement_cost_CH4();
    Abatement_cost_N2O();
    Abatement_cost_LG();
    Adaptation_cost_sealevel();
    Adaptation_cost_economic();
    Adaptation_cost_noneconomic();
    TotalAbatementCosts();
    TotalAdaptationCosts();
    SLR_damage();
    Market_damage();
    non_market_damage();
    Discontinuity();
    equalityweighting();
  }
  # te_totaleffect_growth<<-te_totaleffect;
}

