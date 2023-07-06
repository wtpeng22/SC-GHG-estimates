
Monte_Carlo_PAGE_damage<-function(){
  
  # equlity weight
  ptp_timepreference <<-Best_Guess(datalist_PAGE[[132]]$Value) # <0.1,1, 2>
  equity_proportion <<- 1.0
  emuc_utilityconvexity <<-Best_Guess(datalist_PAGE[[116]]$Value) ;
  civvalue_civilizationvalue <<-Best_Guess(datalist_PAGE[[108]]$Value)
  
  ptp_timepreference <<- 3 # <0.1,1, 2>
  equity_proportion<<-0
  emuc_utilityconvexity<<-0
  
  # ptp_timepreference<<-1.0333333333 # <0.1,1, 2>
  # equity_proportion<<-0
  # emuc_utilityconvexity<<-1.1666666667
  
  # CO2 emissions part
  e_globalCO2emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  e0_baselineCO2emissions<<-matrix(nrow =1,ncol = Region_No_PAGE );
  e_regionalCO2emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE);
  er_CO2emissionsgrowth<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE);
  for(r in 1:Region_No_PAGE){
    e0_baselineCO2emissions[r]<<-Best_Guess(datalist_PAGE[[9]]$e_co2_0[r]);
    e_globalCO2emissions[t0]<<-sum(e0_baselineCO2emissions[1,]);
    e_regionalCO2emissions[t0,r]<<-e0_baselineCO2emissions[r];
  }
  for(t in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      er_CO2emissionsgrowth[t,r]<<-Best_Guess(datalist_PAGE[[14]][t,r+1]);
    }
  }
  
  
  # CO2 cycle
  c_CO2concentration<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  re_remainCO2<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  #re_remainCO2base;
  renoccf_remainCO2wocc<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  air_CO2fractioninatm<<-Best_Guess(datalist_PAGE[[101]]$Value);
  stay_fractionCO2emissionsinatm<<-Best_Guess(datalist_PAGE[[144]]$Value);
  tea_CO2emissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  teay_CO2emissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  ccf_CO2feedback<<-Best_Guess(datalist_PAGE[[105]]$Value);
  ccfmax_maxCO2feedback<<-Best_Guess(datalist_PAGE[[106]]$Value);
  cea_cumCO2emissionsatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  tea_CO2emissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  teay_CO2emissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  ce_0_basecumCO2emissions<<-2050000;
  #y_year<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  #y_year_0;
  res_CO2atmlifetime<<-Best_Guess(datalist_PAGE[[136]]$Value);
  den_CO2density<<-7.8;
  rt_g0_baseglobaltemp<<-0.735309967925382;
  e0_globalCO2emissions<<-38191.0315797948;
  rt_g_globaltemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  # CO2 forcing
  f0_CO2baseforcing<<-1.735;
  fslope_CO2forcingslope<<-5.5;
  c0_baseCO2conc<<-395000;
  f_CO2forcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  
  # CH4 emissions
  e_globalCH4emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  e0_baselineCH4emissions<<-matrix(nrow =1,ncol = Region_No_PAGE );
  e_regionalCH4emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  er_CH4emissionsgrowth<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  for(r in 1:Region_No_PAGE){
    e0_baselineCH4emissions[r]<<-Best_Guess(datalist_PAGE[[8]]$e_ch4_0[r]);
    e_globalCH4emissions[t0]<<-sum(e0_baselineCH4emissions[1,]);
    e_regionalCH4emissions[t0,r]<<-e0_baselineCH4emissions[r];
  }
  for(t in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      er_CH4emissionsgrowth[t,r]<<-Best_Guess(datalist_PAGE[[13]][t,r+1]);
    }
  }
  
  # CH4 cycle
  pic_preindustconcCH4 <<- 700.
  den_CH4density <<- 2.78
  stim_CH4emissionfeedback<<- 0.
  air_CH4fractioninatm <<- 100.
  res_CH4atmlifetime <<- 10.5
  c0_CH4concbaseyr <<- 1860.
  rtl_g0_baselandtemp <<- 0.9258270139190647
  e_0globalCH4emissions <<- 363.00000000000006
  nte_natCH4emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  re_remainCH4<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  nte_natCH4emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  tea_CH4emissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  teay_CH4emissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  rtl_g_landtemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  #CH4 forcing
  fslope_CH4forcingslope <<- 0.036
  f0_CH4baseforcing <<- 0.550
  c0_baseN2Oconc <<- 322.
  c0_baseCH4conc <<- 1860.
  c_N2Oconcentration <<- matrix(nrow =   TimeStep_PAGE,ncol = 1 );
  c_CH4concentration<<-matrix(nrow =  TimeStep_PAGE,ncol = 1 );
  f_CH4forcing<<-matrix(nrow = TimeStep_PAGE,ncol = 1 );
  # over <<- matrix(nrow = TimeStep_PAGE,ncol = 1 );
  #CH4forcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  #N2O emissions
  e_globalN2Oemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  e0_baselineN2Oemissions<<-matrix(nrow =1,ncol = Region_No_PAGE );
  e_regionalN2Oemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  er_N2Oemissionsgrowth<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  for(r in 1:Region_No_PAGE){
    e0_baselineN2Oemissions[1,r]<<-Best_Guess(datalist_PAGE[[11]]$e_n2o_0[r]);
    e_globalN2Oemissions[t0,1]<<-sum(e0_baselineN2Oemissions[1,]);
    e_regionalN2Oemissions[t0,r]<<-e0_baselineN2Oemissions[1,r];
  }
  for(t in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      er_N2Oemissionsgrowth[t,r]<<-Best_Guess(datalist_PAGE[[16]][t,r+1]);
    }
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
  re_remainN2O<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  nte_natN2Oemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  tea_N2Oemissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  teay_N2Oemissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  #rtl_g_landtemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  
  #N2O forcing
  fslope_N2Oforcingslope <<- 0.12
  f0_N2Obaseforcing <<- 0.180
  c0_baseN2Oconc <<- 322.
  c0_baseCH4conc <<- 1860.
  f_N2Oforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  #LG emissions
  e_globalLGemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  e0_baselineLGemissions<<-matrix(nrow =1,ncol = Region_No_PAGE );
  e_regionalLGemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  er_LGemissionsgrowth<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  for(r in 1:Region_No_PAGE){
    e0_baselineLGemissions[1,r]<<-Best_Guess(datalist_PAGE[[10]]$e_glin_0[r]);
    e_globalLGemissions[t0,1]<<-sum(e0_baselineN2Oemissions[1,]);
    e_regionalLGemissions[t0,r]<<-e0_baselineN2Oemissions[1,r];
  }
  for(t in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      er_LGemissionsgrowth[t,r]<<-Best_Guess(datalist_PAGE[[15]][t,r+1]);
    }
  }
  
  #LG cycle
  pic_preindustconcLG <<- 0.
  den_LGdensity <<- 100000.
  stim_LGemissionfeedback <<- 0.
  air_LGfractioninatm <<- 100.
  res_LGatmlifetime <<- 1000.
  c0_LGconcbaseyr <<- 0.11
  rtl_g0_baselandtemp <<- 0.9258270139190647
  e_0globalLGemissions <<- 557.2112715473608
  c_LGconcentration<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  re_remainLG<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  nte_natLGemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  tea_LGemissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  teay_LGemissionstoatm<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  
  # LG forcing
  f0_LGforcingbase <<- 0.022
  fslope_LGforcingslope <<- 0.2
  c0_LGconcbaseyr<<-0.11;
  f_LGforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  #SulphateForcing
  d_sulphateforcingbase <<-Best_Guess(datalist_PAGE[[113]]$Value)
  ind_slopeSEforcing_indirect <<-Best_Guess(datalist_PAGE[[120]]$Value)
  se0_sulphateemissionsbase<<-matrix(nrow =1,ncol = Region_No_PAGE );
  pse_sulphatevsbase<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  se_sulphateemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  area<<-matrix(nrow =1,ncol = Region_No_PAGE );
  sfx_sulphateflux<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  nf_naturalsfx<<-matrix(nrow =1,ncol = Region_No_PAGE );
  fs_sulphateforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  
  for(r in 1:Region_No_PAGE){
    se0_sulphateemissionsbase[1,r]<<-Best_Guess(datalist_PAGE[[45]]$se_0[r]);
    area[1,r]<<-Best_Guess(datalist_PAGE[[1]]$area[r]);
    nf_naturalsfx[1,r]<<-Best_Guess(datalist_PAGE[[32]]$Natural.S[r]);
  } 
  for(t in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      pse_sulphatevsbase[t,r]<<-Best_Guess(datalist_PAGE[[37]][t,r+1]);
    }
  }
  
  
  # total forcing
  f_lineargasforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  exf_excessforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  ft_totalforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  for(t in 1:TimeStep_PAGE){
    exf_excessforcing[t]<<-Best_Guess(datalist_PAGE[[17]]$exf_a[t]);
  }
  
  
  # climate temperature
  rlo_ratiolandocean <<-Best_Guess(datalist_PAGE[[137]]$Value)
  pole_polardifference <<-Best_Guess(datalist_PAGE[[128]]$Value)
  lat_g_meanlatitude<<-30.21989459076828
  fslope_CO2forcingslope<<-5.5
  tcr_transientresponse <<-Best_Guess(datalist_PAGE[[146]]$Value)
  frt_warminghalflife <<-Best_Guess(datalist_PAGE[[117]]$Value)
  et_equilibriumtemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  rt_realizedtemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  lat_latitude<<-matrix(nrow =1,ncol = Region_No_PAGE );
  
  
  
  rtl_0_realizedtemperature<<-matrix(nrow =1,ncol = Region_No_PAGE );
  rtl_realizedtemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  rtl_g_landtemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  rto_g_oceantemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  rt_g_globaltemperature<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  rt_0_realizedtemperature<<-matrix(nrow =1,ncol = Region_No_PAGE );
  rt_adj_temperatureadjustment<<-matrix(nrow =1,ncol = Region_No_PAGE );
  for(r in 1:Region_No_PAGE){
    rtl_0_realizedtemperature[r]<<-Best_Guess(datalist_PAGE[[44]]$rtl_0[r])
    lat_latitude[r]<<-Best_Guess(datalist_PAGE[[31]]$Latitude[r])
  }
  ocean_prop_ortion<<- 1. - sum( area) / 510000000.
  
  # Equation 21 from Hope (2006): initial global land temperature
  rtl_g0_baselandtemp<<-sum( rtl_0_realizedtemperature *  area) / sum( area)
  #待确认
  # initial ocean and global temperatures
  rto_g0_baseoceantemp<<-rtl_g0_baselandtemp/ rlo_ratiolandocean
  rt_g0_baseglobaltemp<<-ocean_prop_ortion * rto_g0_baseoceantemp + (1. - ocean_prop_ortion) *  rtl_g0_baselandtemp;
  
  # Sea Level rise
  sltemp_SLtemprise <<-Best_Guess(datalist_PAGE[[143]]$Value)
  sla_SLbaselinerise <<-Best_Guess(datalist_PAGE[[141]]$Value)
  sltau_SLresponsetime <<-Best_Guess(datalist_PAGE[[142]]$Value)
  s0_initialSL <<-Best_Guess(datalist_PAGE[[138]]$Value)
  es_equilibriumSL<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  s_sealevel<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  expfs_exponential<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  yp_TimeStep_PAGE<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  
  
  # GDP
  save_savingsrate <<-Best_Guess(datalist_PAGE[[139]]$Value) #pp33 PAGE09 documentation, "savings rate".
  save_savingsrate <<-20 #pp33 PAGE09 documentation, "savings rate".
  
  isat0_initialimpactfxnsaturation <<-  Best_Guess(datalist_PAGE[[125]]$Value) #pp34 PAGE09 documentation
  gdp<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cons_consumption<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cons_percap_consumption<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cons_percap_consumption_0<<-matrix(nrow =1,ncol = Region_No_PAGE );
  yagg_periodspan<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  grw_gdpgrowthrate<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  popgrw_populationgrowth<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  gdp_0<<-matrix(nrow =1,ncol = Region_No_PAGE );
  pop0_initpopulation<<-matrix(nrow =1,ncol = Region_No_PAGE );
  pop_population<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  isatg_impactfxnsaturation = isat0_initialimpactfxnsaturation * (1 - save_savingsrate/100);
  for (r in 1:Region_No_PAGE){
    gdp_0[r]<<-Best_Guess(datalist_PAGE[[18]]$gdp_0[r]);
    pop0_initpopulation[r]<<-Best_Guess(datalist_PAGE[[35]]$pop_0[r]);
    cons_percap_consumption_0[r] <<- (gdp_0[r] / pop0_initpopulation[r])*(1 - save_savingsrate / 100)
  }
  for(t in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      grw_gdpgrowthrate[t,r]<<-Best_Guess(datalist_PAGE[[19]][t,r+1]);
      popgrw_populationgrowth[t,r]<<-Best_Guess(datalist_PAGE[[36]][t,r+1]);
      #pop_population[t,r]<<-Best_Guess(datalist_PAGE[[61]][t,r+1]);
    }
  }
  
  
  # Market damage
  tcal_CalibrationTemp<<-Best_Guess(datalist_PAGE[[145]]$Value);
  iben_MarketInitialBenefit <<-Best_Guess(datalist_PAGE[[118]]$Value)
  ipow_MarketIncomeFxnExponent <<-Best_Guess(datalist_PAGE[[122]]$Value)
  #save_savingsrate<<-Best_Guess(datalist_PAGE[[139]]$Value)
  GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
  pow_MarketImpactExponent<<-Best_Guess(datalist_PAGE[[129]]$Value);
  W_MarketImpactsatCalibrationTemp <<-Best_Guess(datalist_PAGE[[148]]$Value);
  atl_adjustedtolerableleveloftemprise<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  imp_actualreduction<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  i_regionalimpact<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  rcons_per_cap_SLRRemainConsumption<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  rgdp_per_cap_SLRRemainGDP<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  WINCF_weightsfactor<<-matrix(nrow =1,ncol = Region_No_PAGE );
  rcons_per_cap_MarketRemainConsumption<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  rgdp_per_cap_MarketRemainGDP<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  #ref_ImpactatReferenceGDPperCap<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  igdp_ImpactatActualGDPperCap<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  iref_ImpactatReferenceGDPperCap<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  impmax_maxtempriseforadaptpolicyM<<-matrix(nrow =1,ncol = Region_No_PAGE );
  isat_ImpactinclSaturationandAdaptation_market<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  isat_per_cap_ImpactperCapinclSaturationandAdaptation<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  i_regionalimpact_market<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  
  for(r in 1:Region_No_PAGE){
    WINCF_weightsfactor[r]<<-Best_Guess(datalist_PAGE[[152]][2,r]);
    impmax_maxtempriseforadaptpolicyM[r]<<-Best_Guess(datalist_PAGE[[21]]$impmax_1_a[r]);
  }
  # for(t in 1:TimeStep_PAGE){
  #   for(r in 1:Region_No_PAGE){
  #     rgdp_per_cap_SLRRemainGDP[t,r]<<-Best_Guess(datalist_PAGE[[58]][t,r+1]);
  #     rcons_per_cap_SLRRemainConsumption[t,r]<<-Best_Guess(datalist_PAGE[[64]][t,r+1]);
  #   }
  # }
  
  # non_market damage
  #tcal_CalibrationTemp<<-Best_Guess(datalist_PAGE[[145]]$Value);
  w_NonImpactsatCalibrationTemp <<-Best_Guess(datalist_PAGE[[149]]$Value);
  iben_NonMarketInitialBenefit <<-Best_Guess(datalist_PAGE[[119]]$Value)
  ipow_NonMarketIncomeFxnExponent <<-Best_Guess(datalist_PAGE[[123]]$Value)
  #save_savingsrate= 15.
  GDP_per_cap_focus_0_FocusRegionEU<<- 27934.244777382406
  pow_NonMarketExponent <<-Best_Guess(datalist_PAGE[[130]]$Value)
  impmax_maxtempriseforadaptpolicyNM<<-matrix(nrow =1,ncol = Region_No_PAGE );
  i_regionalimpact_nonmarket<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  isat_ImpactinclSaturationandAdaptation_nonmarket<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  
  for(r in 1:Region_No_PAGE){
    impmax_maxtempriseforadaptpolicyNM[r]<<-Best_Guess(datalist_PAGE[[22]]$impmax_2_a[r]);
  }
  rcons_per_cap_NonMarketRemainConsumption<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  rgdp_per_cap_NonMarketRemainGDP<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  # for(t in 1:TimeStep_PAGE){
  #   for(r in 1:Region_No_PAGE){
  #     #rcons_per_cap_NonMarketRemainConsumption[t,r]<<-Best_Guess(datalist_PAGE[[63]][t,r+1]);
  #   #  rgdp_per_cap_NonMarketRemainGDP[t,r]<<-Best_Guess(datalist_PAGE[[57]][t,r+1]);
  #   }
  # }
  
  
  
  #Discontinuity
  
  rand_discontinuity<<-runif(1,0,1);# rand_discontinuity为0-1均匀分布
  wdis_gdplostdisc<<-Best_Guess(datalist_PAGE[[151]]$Value);
  ipow_incomeexponent<<-Best_Guess(datalist_PAGE[[121]]$Value);
  distau_discontinuityexponent<<-Best_Guess(datalist_PAGE[[114]]$Value);
  tdis_tolerabilitydisc<<-Best_Guess(datalist_PAGE[[147]]$Value);
  pdis_probability<<-Best_Guess(datalist_PAGE[[127]]$Value);
  GDP_per_cap_focus_0_FocusRegionEU<<- 27934.244777382406
  isatg_saturationmodification<<-28.333333333333336;
  irefeqdis_eqdiscimpact<<-matrix(nrow =1,ncol = Region_No_PAGE );
  igdpeqdis_eqdiscimpact<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  igdp_realizeddiscimpact<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  occurdis_occurrencedummy<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  expfdis_discdecay<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  idis_lossfromdisc<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  isat_satdiscimpact<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  isat_per_cap_DiscImpactperCapinclSaturation<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  rcons_per_cap_DiscRemainConsumption<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  
  
  
  #adaptation cost
  automult_autonomouschange <<-Best_Guess(datalist_PAGE[[102]]$Value)
  cf_costregional<<-matrix(nrow =1,ncol = Region_No_PAGE );
  impmax_maximumadaptivecapacity<<-matrix(nrow =1,ncol = Region_No_PAGE );
  plateau_increaseintolerableplateaufromadaptation<<-matrix(nrow =1,ncol = Region_No_PAGE );
  pstart_startdateofadaptpolicy<<-matrix(nrow =1,ncol = Region_No_PAGE );
  pyears_yearstilfulleffect<<-matrix(nrow =1,ncol = Region_No_PAGE );
  impred_eventualpercentreduction<<-matrix(nrow =1,ncol = Region_No_PAGE );
  istart_startdate<<-matrix(nrow =1,ncol = Region_No_PAGE );
  iyears_yearstilfulleffect<<-matrix(nrow =1,ncol = Region_No_PAGE );
  atl_adjustedtolerablelevel<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  imp_adaptedimpacts<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  autofac_autonomouschangefraction<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  acp_adaptivecostplateau<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  aci_adaptivecostimpact<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  ac_adaptivecosts<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  
  ac_adaptationcosts_economic<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  ac_adaptationcosts_noneconomic<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  ac_adaptationcosts_sealevelrise<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  for(r in 1:Region_No_PAGE){
    cf_costregional[r]<<-Best_Guess(datalist_PAGE[[107]][2,r]);
  }
  
  cp_costplateau_eu_SLR<<-Best_Guess(datalist_PAGE[[100]]$Value);
  
  ci_costimpact_eu_SLR<<-Best_Guess(datalist_PAGE[[99]]$Value);
  
  cp_costplateau_eu_economic<<-Best_Guess(datalist_PAGE[[96]]$Value);
  
  ci_costimpact_eu_economic<<-Best_Guess(datalist_PAGE[[95]]$Value);
  
  cp_costplateau_eu_noneconomic<<-Best_Guess(datalist_PAGE[[98]]$Value);
  
  ci_costimpact_eu_noneconomic<<-Best_Guess(datalist_PAGE[[97]]$Value);
  
  
  
  
  #SLR damage
  pow_SLRImpactFxnExponent <<-Best_Guess(datalist_PAGE[[131]]$Value)
  ipow_SLRIncomeFxnExponent <<-Best_Guess(datalist_PAGE[[124]]$Value)
  iben_SLRInitialBenefit<<-0.00
  scal_calibrationSLR <<-Best_Guess(datalist_PAGE[[140]]$Value)
  GDP_per_cap_focus_0_FocusRegionEU<<-27934.244777382406
  W_SatCalibrationSLR <<-Best_Guess(datalist_PAGE[[150]]$Value) #pp33 PAGE09 documentation, "Sea level impact at calibration sea level rise"
  #save_savingsrate = 15.00 #pp33 PAGE09 documentation, "savings rate".
  tct_per_cap_totalcostspercap<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  act_percap_adaptationcosts<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  impmax_maxSLRforadaptpolicySLR<<-matrix(nrow =1,ncol = Region_No_PAGE );
  cons_percap_aftercosts<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  gdp_percap_aftercosts<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  atl_adjustedtolerablelevelofsealevelrise<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  imp_actualreductionSLR<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  i_regionalimpactSLR<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  iref_ImpactatReferenceGDPperCapSLR<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  igdp_ImpactatActualGDPperCapSLR<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  isat_ImpactinclSaturationandAdaptationSLR<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  isat_per_cap_SLRImpactperCapinclSaturationandAdaptation<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  for(r in 1:Region_No_PAGE){
    impmax_maxSLRforadaptpolicySLR[r]<<-Best_Guess(datalist_PAGE[[52]]$Sea.level.max.rise[r]);
  }
  for(t in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      atl_adjustedtolerablelevelofsealevelrise[t,r]<<-Best_Guess(datalist_PAGE[[65]][t,r+1]);
      #  tct_per_cap_totalcostspercap[t,r]<<-Best_Guess(datalist_PAGE[[66]][t,r+1]);
      #  act_percap_adaptationcosts[t,r]<<-Best_Guess(datalist_PAGE[[67]][t,r+1]);
      imp_actualreductionSLR[t,r]<<-Best_Guess(datalist_PAGE[[68]][t,r+1]);
    }
  }
  
  
  # Abatement cost
  q0propmult_cutbacksatnegativecostinfinalyear <<-Best_Guess(datalist_PAGE[[134]]$Value)
  qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear <<-Best_Guess(datalist_PAGE[[135]]$Value)
  c0mult_mostnegativecostinfinalyear <<-Best_Guess(datalist_PAGE[[104]]$Value)
  curve_below_curvatureofMACcurvebelowzerocost <<-Best_Guess(datalist_PAGE[[112]]$Value)
  curve_above_curvatureofMACcurveabovezerocost <<-Best_Guess(datalist_PAGE[[111]]$Value)
  cross_experiencecrossoverratio <<-Best_Guess(datalist_PAGE[[110]]$Value)
  learn_learningrate <<-Best_Guess(datalist_PAGE[[126]]$Value)
  automult_autonomoustechchange <<-Best_Guess(datalist_PAGE[[102]]$Value)
  equity_prop_equityweightsproportion <<- 1.
  
  er_emissionsgrowth<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  e0_baselineemissions<<-matrix(nrow =1,ncol = Region_No_PAGE );
  emitf_uncertaintyinBAUemissfactor<<-matrix(nrow =1,ncol = Region_No_PAGE );
  q0f_negativecostpercentagefactor<<-matrix(nrow =1,ncol = Region_No_PAGE );
  cmaxf_maxcostfactor<<-matrix(nrow =1,ncol = Region_No_PAGE );
  bau_businessasusualemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  yagg_periodspan<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  emit_UncertaintyinBAUEmissFactor<<-matrix(nrow =1,ncol = Region_No_PAGE );
  q0propinit_CutbacksinNegativeCostinBaseYear<<-matrix(nrow =1,ncol = Region_No_PAGE );
  cmaxinit_MaxCutbackCostinBaseYear<<-matrix(nrow =1,ncol = Region_No_PAGE );
  zc_zerocostemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cb_reductionsfromzerocostemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cbe_absoluteemissionreductions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cumcbe_cumulativereductionssincebaseyear<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cumcbe_g_totalreductions<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  learnfac_learning<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  autofac<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  c0<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  q0prop<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  q0_absolutecutbacksatnegativecost<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  qmax_maxreferencereductions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  cmax<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  blo<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  alo<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  bhi<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  ahi<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  mc_marginalcost<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  tcq0<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  tc_totalcost<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  bau_co2emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  bau_ch4emissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  bau_n2oemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  bau_linemissions<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  for(r in 1:Region_No_PAGE){
    emitf_uncertaintyinBAUemissfactor[r]<<-Best_Guess(datalist_PAGE[[115]][2,r]);
    q0f_negativecostpercentagefactor[r]<<-Best_Guess(datalist_PAGE[[133]][2,r]);
    cmaxf_maxcostfactor[r]<<-Best_Guess(datalist_PAGE[[109]][2,r]);
  }
  for(t in 1:TimeStep_PAGE){
    #  yagg_periodspan[t]<<-Best_Guess(datalist_PAGE[[69]][t,2]);
    for(r in 1:Region_No_PAGE){
      bau_co2emissions[t,r]<<-Best_Guess(datalist_PAGE[[3]][t,r+1]);
      bau_ch4emissions[t,r]<<-Best_Guess(datalist_PAGE[[2]][t,r+1]);
      bau_n2oemissions[t,r]<<-Best_Guess(datalist_PAGE[[5]][t,r+1]);
      bau_linemissions[t,r]<<-Best_Guess(datalist_PAGE[[4]][t,r+1]);
      
    }
  }
  tc_totalcosts_co2<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  tc_totalcosts_ch4<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  tc_totalcosts_n2o<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  tc_totalcosts_linear<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CO2<<-Best_Guess(datalist_PAGE[[79]]$Value)
  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CO2<<-Best_Guess(datalist_PAGE[[81]]$Value)
  c0init_MostNegativeCostCutbackinBaseYear_CO2<<-Best_Guess(datalist_PAGE[[77]]$Value)
  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CO2<<-Best_Guess(datalist_PAGE[[82]]$Value)
  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CO2 <<-Best_Guess(datalist_PAGE[[78]]$Value)
  ies_InitialExperienceStockofCutbacks_CO2 <<-Best_Guess(datalist_PAGE[[80]]$Value)
  
  
  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CH4<<-Best_Guess(datalist_PAGE[[73]]$Value)
  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CH4 <<-Best_Guess(datalist_PAGE[[75]]$Value)
  c0init_MostNegativeCostCutbackinBaseYear_CH4 <<-Best_Guess(datalist_PAGE[[71]]$Value)
  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CH4 <<-Best_Guess(datalist_PAGE[[76]]$Value)
  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CH4 <<-Best_Guess(datalist_PAGE[[72]]$Value)
  ies_InitialExperienceStockofCutbacks_CH4 <<-Best_Guess(datalist_PAGE[[74]]$Value)
  
  
  
  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_N2O<<-Best_Guess(datalist_PAGE[[91]]$Value)
  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_N2O<<-Best_Guess(datalist_PAGE[[93]]$Value)
  c0init_MostNegativeCostCutbackinBaseYear_N2O<<- Best_Guess(datalist_PAGE[[89]]$Value)
  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_N2O<<-Best_Guess(datalist_PAGE[[94]]$Value)
  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_N2O <<-Best_Guess(datalist_PAGE[[90]]$Value)
  ies_InitialExperienceStockofCutbacks_N2O <<-Best_Guess(datalist_PAGE[[92]]$Value)
  
  
  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_LG <<-Best_Guess(datalist_PAGE[[85]]$Value)
  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_LG<<-Best_Guess(datalist_PAGE[[87]]$Value)
  c0init_MostNegativeCostCutbackinBaseYear_LG<<-Best_Guess(datalist_PAGE[[83]]$Value)
  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_LG<<-Best_Guess(datalist_PAGE[[88]]$Value)
  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_LG<<-Best_Guess(datalist_PAGE[[84]]$Value)
  ies_InitialExperienceStockofCutbacks_LG<<-Best_Guess(datalist_PAGE[[86]]$Value)
  
  # TotalAbatementCosts
  
  tct_totalcosts<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  #tct_per_cap_totalcostspercap<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  
  # TotalAdaptationCosts
  
  act_adaptationcosts_total<<-matrix(nrow =       TimeStep_PAGE,ncol = Region_No_PAGE );
  
  # population
  # 
  #pop_population<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  
  #tct_percap_totalcosts_total<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  # act_adaptationcosts_total<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  # act_percap_adaptationcosts<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  wtct_percap_weightedcosts<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  eact_percap_weightedadaptationcosts<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  wact_percap_partiallyweighted<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  wact_partiallyweighted<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  pct_percap_partiallyweighted<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  pct_partiallyweighted<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  pct_g_partiallyweighted_global<<-matrix(nrow = TimeStep_PAGE,ncol = 1)
  dr_discountrate<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  yp_yearsperiod<<-matrix(nrow = TimeStep_PAGE,ncol = 1)
  dfc_consumptiondiscountrate<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  df_utilitydiscountrate<<-matrix(nrow = TimeStep_PAGE,ncol = 1)
  pcdt_partiallyweighted_discounted<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  pcdt_g_partiallyweighted_discountedglobal<<-matrix(nrow = TimeStep_PAGE,ncol = 1)
  pcdat_partiallyweighted_discountedaggregated<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  wacdt_partiallyweighted_discounted<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  #rcons_percap_dis<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  wit_equityweightedimpact<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  widt_equityweightedimpact_discounted<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  addt_equityweightedimpact_discountedaggregated<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  aact_equityweightedadaptation_discountedaggregated<<-matrix(nrow = TimeStep_PAGE,ncol = Region_No_PAGE)
  # for(t in 1:TimeStep_PAGE){
  #   for(r in 1:Region_No_PAGE){
  #     rcons_percap_dis[t,r]<<-Best_Guess(datalist_PAGE[[70]][t,r+1]);
  #   }
  # }
}







