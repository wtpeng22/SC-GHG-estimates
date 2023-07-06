
Monte_Carlo_FUND_damage<-function(){
  
  for(i in 1:TimeStep)    
  {
    for(j in 1:Region_No){
      scenypcgrowth[i,j]<<-Best_Guess(datalist[[150]]$V3[(16*(i-1)+j)]);
      scenpgrowth[i,j]<<-Best_Guess(datalist[[148]]$V3[(16*(i-1)+j)]);
      scen_aeei[i,j]<<-Best_Guess(datalist[[146]]$V3[(16*(i-1)+j)]);
      scen_acei[i,j]<<-Best_Guess(datalist[[145]]$V3[(16*(i-1)+j)]);
      scenforestemm[i,j]<<-Best_Guess(datalist[[147]]$V3[(16*(i-1)+j)]);
    }
  }
  
  for(j in 1:Region_No){
    ecgradd[1,j]<<-Best_Guess(datalist[[77]]$V2[j]);
    pgadd[1,j]<<-Best_Guess(datalist[[134]]$V2[j]);
    aeei_add[1,j]<<-Best_Guess(datalist[[2]]$V2[j]);
    acei_add[1,j]<<-Best_Guess(datalist[[1]]$V2[j]);
    foremadd[1,j]<<-Best_Guess(datalist[[89]]$V2[j]);
  }
  for(i in 1:TimeStep)    
  {
    for(j in 1:Region_No)
    {
      YearsFromUncertaintyStart<<-i-TimeOfUncertaintyStart_value;
      SdTimeFactor<<-(YearsFromUncertaintyStart/50)/(1+(YearsFromUncertaintyStart/50));
      ypcgrowth[i,j]<<-scenypcgrowth[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,ecgradd[1,j]*SdTimeFactor,0.0);
      pgrowth[i,j]<<-scenpgrowth[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,pgadd[1,j]*SdTimeFactor,0.0);
      aeei[i,j]<<-scen_aeei[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,aeei_add[1,j]*SdTimeFactor,0.0);
      acei[i,j]<<-scen_acei[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,acei_add[1,j]*SdTimeFactor,0.0);
      # forestemm[i,j]<<-scenforestemm[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,foremadd[1,j]*SdTimeFactor,0.0);
    }
  }
  
  aeei[dataupdate_year_index:TimeStep,] <<- AEEI_data_ssp[1:(TimeStep-dataupdate_year_index+1),-1]*100;
  
  for(r in 1:Region_No){
    Area[t0,r]<<-Best_Guess(datalist[[11]]$V2[r]);
  }
  
  for(r in 1:Region_No){
    population[t0,r]<<-Best_Guess(datalist[[137]]$V2[r]);#计算全球1950年各个地区的人口
    population1[t0,r]<<-population[t0,r]*1000000;
    globalpopulation[t0,1]<<-globalpopulation[t0,1]+population1[t0,r];#计算全球1950年总人口
  }
  plusel<<-Best_Guess(datalist[[136]]$V1);
  for(r in 1:Region_No){
    income[t0,r]<<-Best_Guess(datalist[[91]]$V2[r]);
    ypc[t0,r]<<-income[t0,r]/population[t0,r]*1000;
    consumption[t0,r]<<-income[t0,r]*1000000000*(1-savingsrate);
    globalconsumpation[t0,1]<<-globalconsumpation[t0,1]+consumption[t0,r];
    plus90[1,r]<<-Best_Guess(datalist[[135]]$V2[r]);
    gdp90[1,r]<<-Best_Guess(datalist[[92]]$V2[r]);
    pop90[1,r]<<-Best_Guess(datalist[[138]]$V2[r]);
    ypc90[1,r]<<-gdp90[1,r]/pop90[1,r]*1000;
    urbcorr[1,r]<<-Best_Guess(datalist[[173]]$V2[r])
  }
  consleak<<-Best_Guess(datalist[[48]]$V1);
  sf6gdp<<-Best_Guess(datalist[[154]]$V1);
  sf6pre<<-Best_Guess(datalist[[155]]$V1);
  sf6ypc<<-Best_Guess(datalist[[156]]$V1);
  TaxConstant<<-Best_Guess(datalist[[164]]$V1);
  TaxEmInt<<-Best_Guess(datalist[[166]]$V1);
  TaxThreshold<<-Best_Guess(datalist[[168]]$V1);
  TaxDepreciation<<-Best_Guess(datalist[[168]]$V1);
  knowpar<<-Best_Guess(datalist[[107]]$V1);
  knowgpar<<-Best_Guess(datalist[[106]]$V1);
  MaxCostFall<<-Best_Guess(datalist[[124]]$V1);
  gwpn2o<<-Best_Guess(datalist[[94]]$V1);
  gwpch4<<-Best_Guess(datalist[[93]]$V1);
  ch4add<<-Best_Guess(datalist[[30]]$V1);
  n2oadd<<-Best_Guess(datalist[[127]]$V1);
  sf6add<<-Best_Guess(datalist[[153]]$V1);
  for(t in 1:TimeStep)
  {
    for(r in 1:Region_No){
      ch4em[t,r]<<-Best_Guess(datalist[[31]]$V3[r+(t-1)*Region_No]);
      n2oem[t,r]<<-Best_Guess(datalist[[128]]$V3[r+(t-1)*Region_No]);
      currtax[t,r]<<-Best_Guess(datalist[[49]]$V3[r+(t-1)*Region_No]);
      currtaxch4[t,r]<<-Best_Guess(datalist[[49]]$V3[r+(t-1)*Region_No]);
      currtaxn2o[t,r]<<-Best_Guess(datalist[[49]]$V3[r+(t-1)*Region_No]);
    }
  }
  for(r in 1:Region_No){
    energint[t0,r]=1;
    energuse[t0,r]<<-Best_Guess(datalist[[91]]$V2[r]);
    emissionint[t0,r]<<-Best_Guess(datalist[[79]]$V2[r]);
    emission[t0,r]<<-emissionint[t0,r]/energuse[t0,r];
    ch4par1[t0,r]<<-Best_Guess(datalist[[33]]$V2[r]);
    ch4par2[t0,r]<<-Best_Guess(datalist[[34]]$V2[r]);
    n2opar1[t0,r]<<-Best_Guess(datalist[[129]]$V2[r]);
    n2opar2[t0,r]<<-Best_Guess(datalist[[130]]$V2[r]);
    taxmp[t0,r]<<-Best_Guess(datalist[[167]]$V2[r]);
    sf60[t0,r]<<-Best_Guess(datalist[[152]]$V2[r]);
  }
  for(r in 1:Region_No){
    
    if(emission[t0,r]/income[t0,r]<minint )
      minint<<-emission[t0,r]/income[t0,r];
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
  TerrCO2Stock[t0,1]<<-Best_Guess(datalist[[171]]$V1);
  # TerrCO2Stock[t0,1]<<-TerrCO2Stock0;
  
  co2decay1<<-lifeco1;
  co2decay2<<-exp(-1/lifeco2);
  co2decay3<<-exp(-1/lifeco3);
  co2decay4<<-exp(-1/lifeco4);
  co2decay5<<-exp(-1/lifeco5);
  cbox1[t0,1]<<-cbox10;
  cbox2[t0,1]<<-cbox20;
  cbox3[t0,1]<<-cbox30;
  cbox4[t0,1]<<-cbox40;
  cbox5[t0,1]<<-cbox50;
  acco2[t0]<<-cbox1[t0,1]+cbox2[t0,1]+cbox3[t0,1]+cbox4[t0,1]+cbox5[t0,1];
  lifech4<<-Best_Guess(datalist[[108]]$V1);
  ch4pre<<-Best_Guess(datalist[[35]]$V1);
  ch4decay<<-1/lifech4;
  acch4[t0,1]<<-1222;
  lifen2o<<-Best_Guess(datalist[[114]]$V1);
  n2opre<<-Best_Guess(datalist[[131]]$V1);
  n2odecay<<-1/lifen2o;
  acn2o[t0,1]<<-296;
  lifesf6<<-Best_Guess(datalist[[116]]$V1);
  sf6pre<<-Best_Guess(datalist[[155]]$V1);
  sf6decay<<-1/lifesf6;
  acsf6[t0,1]<<-sf6pre;
  ch4ind<<-Best_Guess(datalist[[32]]$V1);
  co2pre<<-Best_Guess(datalist[[46]]$V1);
  ch4pre<<-Best_Guess(datalist[[35]]$V1);
  for(t in 1:TimeStep){
    rfSO2[t,1]<<-Best_Guess(datalist[[141]]$V2[t]);
  }
  LifeTempConst<<-Best_Guess(datalist[[117]]$V1);
  # ClimateSensitivity<<-Best_Guess(datalist[[40]]$V1);
  LifeTempLin<<-Best_Guess(datalist[[118]]$V1);
  LifeTempQd<<-Best_Guess(datalist[[119]]$V1);
  for(r in 1:Region_No){
    bregstemp[1,r]<<-Best_Guess(datalist[[15]]$V2[r]);
    bregtemp[1,r]<<-Best_Guess(datalist[[16]]$V2[r]);
    bregtemp[1,r]<<-globaltoregionaltemp[r];
  }
  for(t in 1:TimeStep){
    for(r in 1:Region_No){
      scentemp[t,r]<<-Best_Guess(datalist[[149]]$V3[(t-1)*Region_No+r]);
    }
  }
  lifesea<<-Best_Guess(datalist[[115]]$V1);
  seas<<-Best_Guess(datalist[[151]]$V1);
  delaysea<<-1/lifesea;
  bioloss<<-Best_Guess(datalist[[12]]$V1);
  biosens<<-Best_Guess(datalist[[13]]$V1);
  nospacebase<<-Best_Guess(datalist[[132]]$V1);
  dbsta<<-Best_Guess(datalist[[63]]$V1);
  nospieces[t0,1]<<-nospacebase;
  bioshare<<-Best_Guess(datalist[[14]]$V1);
  spbm<<-Best_Guess(datalist[[163]]$V1);
  valbase<<-Best_Guess(datalist[[174]]$V1);
  for(r in 1:Region_No){
    valinc[1,r]<<-Best_Guess(datalist[[175]]$V2[r]);
  }
  for(r in 1:Region_No){
    hebm[1,r]<<-Best_Guess(datalist[[95]]$V2[r])
  }
  heel<<-Best_Guess(datalist[[96]]$V1);
  ceel<<-Best_Guess(datalist[[28]]$V1);
  cenl<<-Best_Guess(datalist[[29]]$V1);
  for(r in 1:Region_No){
    cebm[1,r]<<-Best_Guess(datalist[[27]]$V2[r])
  }
  
  agnl<<-Best_Guess(datalist[[7]]$V1);
  agel<<-Best_Guess(datalist[[4]]$V1);
  
  for(r in 1:Region_No){
    agrbm[1,r]<<-Best_Guess(datalist[[8]]$V2[r]);
    agtime[1,r]<<-Best_Guess(datalist[[10]]$V2[r]);
    agrish0[1,r]<<-Best_Guess(datalist[[9]]$V2[r]);
    agrate[t0,r]<<-agrbm[1,r]*((0.005/DBsT)^agnl)*agtime[1,r];
    agcbm[t0,r]<<-Best_Guess(datalist[[3]]$V2[r]);
    aglpar1[t0,r]<<-Best_Guess(datalist[[5]]$V2[r]);
    aglparq[t0,r]<<-Best_Guess(datalist[[6]]$V2[r]);
  }
  watechrate<<-Best_Guess(datalist[[183]]$V1);
  
  wrpl<<-Best_Guess(datalist[[191]]$V1);
  wrnl<<-Best_Guess(datalist[[190]]$V1);
  wrel<<-Best_Guess(datalist[[189]]$V1);
  for(r in 1:Region_No){
    wrbm[1,r]<<-Best_Guess(datalist[[188]]$V2[r]);
  }
  diamortel<<-Best_Guess(datalist[[68]]$V1);
  diamortnl<<-Best_Guess(datalist[[69]]$V1);
  diayldel<<-Best_Guess(datalist[[71]]$V1);
  diayldnl<<-Best_Guess(datalist[[72]]$V1);
  
  for(r in 1:Region_No){
    diamort[1,r]<<-Best_Guess(datalist[[67]]$V2[r]);
    diayld[1,r]<<-Best_Guess(datalist[[70]]$V2[r]);
    temp90[1,r]<<-Best_Guess(datalist[[169]]$V2[r]);
  }
  
  
  for(r in 1:Region_No){
    hurrbasedam[1,r]<<-Best_Guess(datalist[[98]]$V2[r]);
    hurrbasedead[1,r]<<-Best_Guess(datalist[[99]]$V2[r]);
  }
  hurrpar<<-Best_Guess(datalist[[103]]$V1);
  hurrdeadel<<-Best_Guess(datalist[[101]]$V1);
  hurrdamel<<-Best_Guess(datalist[[100]]$V1);
  hurrnl<<-Best_Guess(datalist[[102]]$V1);
  extratropicalstormsdamel<<-Best_Guess(datalist[[82]]$V1);
  extratropicalstormsdeadel<<-Best_Guess(datalist[[83]]$V1);
  extratropicalstormsnl<<-Best_Guess(datalist[[84]]$V1);
  for(r in 1:Region_No){
    extratropicalstormsbasedam[1,r]<<-Best_Guess(datalist[[80]]$V2[r]);
    extratropicalstormsbasedead[1,r]<<-Best_Guess(datalist[[81]]$V2[r]);
    extratropicalstormspar[1,r]<<-Best_Guess(datalist[[85]]$V2[r]);
  }
  for(r in 1:Region_No){
    pc[1,r]<<-Best_Guess(datalist[[133]]$V2[r]);
    slrprtp[1,r]<<-Best_Guess(datalist[[157]]$V2[r]);
    wmbm[1,r]<<-Best_Guess(datalist[[187]]$V2[r]);
    dlbm[1,r]<<-Best_Guess(datalist[[73]]$V2[r]);
    drylandlossparam[1,r]<<-Best_Guess(datalist[[74]]$V2[r]);
    wlbm[1,r]<<-Best_Guess(datalist[[186]]$V2[r]);
    coastpd[1,r]<<-Best_Guess(datalist[[47]]$V2[r]);
    wetmax[1,r]<<-Best_Guess(datalist[[185]]$V2[r]);
    wetland90[1,r]<<-Best_Guess(datalist[[184]]$V2[r]);
    maxlandloss[1,r]<<-Best_Guess(datalist[[125]]$V2[r]);
  }
  dvbm<<-Best_Guess(datalist[[75]]$V1);
  dvydl<<-Best_Guess(datalist[[76]]$V1);
  incdens<<-Best_Guess(datalist[[105]]$V1);
  emcst<<-Best_Guess(datalist[[78]]$V1);
  immcst<<-Best_Guess(datalist[[104]]$V1);
  wvel<<-Best_Guess(datalist[[193]]$V1);
  wvbm<<-Best_Guess(datalist[[192]]$V1);
  slrwvpopdens0<<-Best_Guess(datalist[[158]]$V1);
  wvpdl<<-Best_Guess(datalist[[194]]$V1);
  wvsl<<-Best_Guess(datalist[[195]]$V1);
  slrwvypc0<<-Best_Guess(datalist[[159]]$V1);
  for(r1 in 1:Region_No){
    for(r2 in 1:Region_No){
      migrate[r1,r2]<<-Best_Guess(datalist[[126]]$V3[(r1-1)*Region_No+r2]);
    }
  }
  for(r1 in 1:Region_No){
    for(r2 in 1:Region_No){
      immsum<<-0;
      for(i in 1:Region_No){
        immsum<<-immsum+migrate[i,r1];
      }
      imigrate[r1,r2]<<-migrate[r2,r1]/immsum;
    }
    landloss[t0,r1]<<-0;
    cumlandloss[t0,r1]<<-0;
    cumwetlandloss[t0,r1]<<-0;
    wetlandgrowth[t0,r]<<-0;
  }
  forel<<-Best_Guess(datalist[[88]]$V1);
  fornl<<-Best_Guess(datalist[[90]]$V1);
  forco2<<-Best_Guess(datalist[[87]]$V1);
  for(r in 1:Region_No){
    forbm[1,r]<<-Best_Guess(datalist[[86]]$V2[r]);
  }
  for(r in 1:Region_No){
    dfbs[1,r]<<-Best_Guess(datalist[[64]]$V2[r]);
    dfch[1,r]<<-Best_Guess(datalist[[65]]$V2[r]);
    smbs[1,r]<<-Best_Guess(datalist[[160]]$V2[r]);
    smch[1,r]<<-Best_Guess(datalist[[161]]$V2[r]);
    malbs[1,r]<<-Best_Guess(datalist[[120]]$V2[r]);
    malch[1,r]<<-Best_Guess(datalist[[121]]$V2[r]);
  }
  dfnl<<-Best_Guess(datalist[[66]]$V1);
  vbel<<-Best_Guess(datalist[[176]]$V1);
  smnl<<-Best_Guess(datalist[[162]]$V1);
  malnl<<-Best_Guess(datalist[[122]]$V1);
  for(r in 1:Region_No){
    cardvasc90[1,r]<<-Best_Guess(datalist[[17]]$V2[r]);
    resp90[1,r]<<-Best_Guess(datalist[[140]]$V2[r]);
    chplbm[1,r]<<-Best_Guess(datalist[[38]]$V2[r]);
    chmlbm[1,r]<<-Best_Guess(datalist[[36]]$V2[r]);
    chpqbm[1,r]<<-Best_Guess(datalist[[39]]$V2[r]);
    chmqbm[1,r]<<-Best_Guess(datalist[[37]]$V2[r]);
    rlbm[1,r]<<-Best_Guess(datalist[[142]]$V2[r]);
    rqbm[1,r]<<-Best_Guess(datalist[[144]]$V2[r]);
    ccplbm[1,r]<<-Best_Guess(datalist[[25]]$V2[r]);
    ccmlbm[1,r]<<-Best_Guess(datalist[[23]]$V2[r]);
    ccpqbm[1,r]<<-Best_Guess(datalist[[26]]$V2[r]);
    ccmqbm[1,r]<<-Best_Guess(datalist[[24]]$V2[r]);
  }
  cvlin<<-Best_Guess(datalist[[52]]$V1);
  rlin<<-Best_Guess(datalist[[143]]$V1);
  maxcardvasc<<-Best_Guess(datalist[[123]]$V1);
  for(r in 1:Region_No){
    d2lc[1,r]<<-Best_Guess(datalist[[58]]$V2[r]);
    d2ld[1,r]<<-Best_Guess(datalist[[59]]$V2[r]);
    d2ls[1,r]<<-Best_Guess(datalist[[62]]$V2[r]);
    d2lr[1,r]<<-Best_Guess(datalist[[61]]$V2[r]);
    d2lm[1,r]<<-Best_Guess(datalist[[60]]$V2[r]);
    d2dr[1,r]<<-Best_Guess(datalist[[56]]$V2[r]);
    d2dm[1,r]<<-Best_Guess(datalist[[55]]$V2[r]);
    d2dc[1,r]<<-Best_Guess(datalist[[53]]$V2[r]);
    d2dd[1,r]<<-Best_Guess(datalist[[54]]$V2[r]);
    d2ds[1,r]<<-Best_Guess(datalist[[57]]$V2[r]);
  }
  vslbm<<-Best_Guess(datalist[[180]]$V1);
  vslel<<-Best_Guess(datalist[[181]]$V1);
  vmorbbm<<-Best_Guess(datalist[[177]]$V1);
  vmorbel<<-Best_Guess(datalist[[178]]$V1);
  vslypc0<<-Best_Guess(datalist[[182]]$V1);
  vmorbypc0<<-Best_Guess(datalist[[179]]$V1);
  
  
}