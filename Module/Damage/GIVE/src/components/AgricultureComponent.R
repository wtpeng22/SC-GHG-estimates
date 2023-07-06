# Moore et al Agriculture component (with linear interpolation between gtap temperature points)

fund_income <- output_gdp

agrish <- matrix(nrow = length(timestep_damage),ncol = length(fund_regions)) # agricultural share of the economy
agel <-  0.31            # elasticity

agcost <- matrix(nrow = length(timestep_damage),ncol = length(fund_regions))     # This is the main damage variable
agcost_ration <- matrix(nrow = length(timestep_damage),ncol = length(fund_regions))     # This is the main damage variable
agcost_country <- matrix(nrow = length(timestep_damage),ncol = length(countries))     # This is the main damage variable

# Moore additions:

AgLossGTAP <- matrix(nrow = length(timestep_damage),ncol = length(fund_regions)) # Moore's fractional loss (intermediate variable for calculating agcost)

Ag_run_timestep <- function(t) {
  fund_income <<- output_gdp
  for (r in 1:length(fund_regions)) {
    ypc <<- output_gdp[t, r] / output_pop[t, r] * 1000.0
    ypc90 <<- gdp90[r] / pop90[r] * 1000.0
    
    agrish[t, r] <<- agrish0[r] * (ypc / ypc90)^(-agel)
    
    # Interpolate for temp, using the three gtap welfare points with the additional origin (0,0) point
    impact <<- approx(x = c(0, 1, 2, 3), y = c(0, gtap_df[r, ]),  xout = Ag_temp[t])$y
    if(Ag_temp[t]>3){
      impact<<- (gtap_df[r,3] - gtap_df[r, 2])*(Ag_temp[t]-3)+gtap_df[r,3]
    }
    if(Ag_temp[t]<0){
      impact<<- gtap_df[r,1]*Ag_temp[t]
    }
    
    impact <<- ifelse(floor_on_damages, pmax(-100, impact), impact)
    impact <<- ifelse(ceiling_on_benefits, pmin(100, impact), impact)
    AgLossGTAP[t, r] <<- - impact / 100 # We take the negative to go from impact to loss
    
    # Calculate total cost for the ag sector based on the percent loss
    agcost[t, r] <<- fund_income[t, r] * agrish[t, r] * AgLossGTAP[t, r]
    agcost_ration[t, r] <<- agrish[t, r] * AgLossGTAP[t, r]
    
    ####Map the agcost to country level
    agcost_mapping_index <<- match(ag_mapping,fund_regions)
    agcost_country[t, which(agcost_mapping_index==r)] <<- agcost_ration[t,r]*gdp[t,which(agcost_mapping_index==r)]
  }
}

