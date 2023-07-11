
Monte_Carlo<-function(){
  
  if(model_choose=="MonteCarlo"){
    sens_climatesensitivity<<-ECS_distribution$x[ecs_num];
  }
  if(model_choose=="BestGuess"){
    sens_climatesensitivity<<-3;
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
  c_N2Oconcentration <<- matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  c_CH4concentration<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  f_CH4forcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  over<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  #CH4forcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );


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
  for(t_year in 1:TimeStep_PAGE){
    for(r in 1:Region_No_PAGE){
      pse_sulphatevsbase[t_year,r]<<-Best_Guess(datalist_PAGE[[37]][t_year,r+1]);
    }
  }


  # total forcing
  f_lineargasforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  # exf_excessforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  ft_totalforcing<<-matrix(nrow =       TimeStep_PAGE,ncol = 1 );
  for(t in 1:TimeStep_PAGE){
    # exf_excessforcing[t_year]<<-Best_Guess(datalist_PAGE[[17]]$exf_a[t_year]);
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

}







