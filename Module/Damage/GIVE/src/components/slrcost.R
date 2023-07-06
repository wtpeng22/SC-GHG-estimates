# Define all variables, parameters and indices used by this module
# --- Indices ---
ciam_country <- as.vector(ciam_countries)
ciam_segments <- as.vector(segments)
adaptPers <- as.vector(adaptPers)

# --- Region / segment mapping ---
segID <- matrix(nrow = length(ciam_segments),ncol = 1) # Unique segment numeric identifier
xsc <- ciam_params[[3]]$xsc  # Region to segment mapping (dictionary) to keep track of which segments belong to each region
rcp <- ciam_params[[3]]$rcp           # RCP being run (metadata; not used in run)
percentile <- ciam_params[[3]]$percentile            # Percentile of RCP being run (metadata; not used in run)
ssp <- ciam_params[[3]]$ssp                   # SSP being used (0 for base case)

# ---Time-related Parameters---
tstep <- ciam_params[[3]]$tstep         # Length of individual time-step (years)
at <- ciam_params[[3]]$at   # Array of time indices that mark starts of adaptation periods
ntsteps <- length(seq(from=ciam_start_year, to=ciam_end_year, by=tstep))              # Number of time-steps

# ---Model Parameters ---
fixed <- ciam_params[[3]]$fixed               # Run model as fixed (T) or flexible (F) with respect to adaptation
noRetreat <- ciam_params[[3]]$noRetreat           # Default (F). If T, ciam_segments will either protect or not adapt.
allowMaintain <- ciam_params[[3]]$allowMaintain       # Default F. If T, ciam_segments will have the option to maintain current defenses

# ---Socioeconomic Parameters---
popinput <- ciam_params[[3]]$popinput           # Input for population data source: 0 (default), 1 (Jones & O'Neill, 2016), 2 (Merkens et al, 2016)
# slr_pop <- slr_pop          # Population of region (million people) (from MERGE or SSPs)
refpopdens <- ciam_params[[3]]$refpopdens         # Reference population density of region (people / km^2)
rgn_ind_usa <- ciam_params[[3]]$rgn_ind_usa                     # Lookup parameter for USA region index, used in refpopdens and ypc for USA benchmark in vsl, rho and fundland calculations
popdens <- ciam_params[[3]]$popdens          # Pop density of segment in time t = 1 (people/km^2)
# slr_ypcc <- slr_ypcc          # GDP per capita per region ($2010 per capita)

popdens_seg <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))  # Population density of segment extrapolated forward in time (people / km^2)
#popdens_seg_jones <- array(dim=c(length(ciam_times),segments), unit="persons/km2") # Holder for Jones and O'Neill population density (not currently supported)
#popdens_seg_merkens <- array(dim=c(length(ciam_times),segments), unit="persons/km2") # Holder for Merkens et al population density (not currently supported)
ypc_seg <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments)) # GDP per capita by segment ($2010 per capita) (multiplied by scaling factor)
refA_R <- ciam_params[[3]]$refA_R  # Reference retreat level of adaptation in 0 period
refA_H <- ciam_params[[3]]$refA_H  # Reference height for adaptation in 0 period

# ---Land Parameters---
landinput <- T  # Set to T for FUND or F for GTAP

gtapland <- ciam_params[[3]]$gtapland # GTAP land value in 2007 (million 2010$ / km^2)
dvbm <- 5.376 # FUND value of OECD dryland per Darwin et al 1995 converted from $1995 ($2010M per sqkm) (5.376)
kgdp <- 3  # Capital output ratio (per MERGE) (3 by default)
discountrate <- 0.04 # Discount rate (0.04 by default)
depr <- 1 # Fraction of capital that has not been depreciated over adaptation period (retreat cases)

landdata <- matrix(nrow = length(ciam_country),ncol = 1) # Takes on value of either fundland or gtapland
fundland <- matrix(nrow = length(ciam_country),ncol = 1) # FUND land value in 1995 (calculated in run_timestep) (million 2010$ / km^2),

rgn_ind_canada <- ciam_params[[3]]$rgn_ind_canada  # Region index for Canada (Used as reference for Greenland land appreciation)
land_appr <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments)) # Land appreciation rate (calculated as regression by Yohe ref Abraham and Hendershott)
coastland <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))  # Coastal land value (function of interior land value * scaling factor) ($2010M per sqkm)
landvalue <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))  # Total endowment value of land ($2010M per sqkm)
landrent <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))  # Annual rental value of land ($2010M/sqkm/year)

rho <- matrix(nrow = length(ciam_times),ncol = length(ciam_country))  # Country-wide resilience parameter (logistic function related to GDP)
capital <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments)) # Total endowment value of capital stock (million $2010 / km^2)
discountfactor <- matrix(nrow = length(ciam_times),ncol = 1) # Discount factor (derived from discount rate)

# ---Coastal Parameters---
length <- ciam_params[[3]]$length

# ---Protection Parameters---
cci <- ciam_params[[3]]$cci
pcfixed <- 0.3          # Fraction of protection cost that is fixed (not variable in height) (0.3)
mc <- 0.02               # Maintenance cost (Hillen et al, 2010) (2%/yr)
pc0 <- 6.02   # Reference cost of protection (million 2010$ / km / vert m^2) (6.02 by default)

# ---Retreat / No Adapt Parameters---
mobcapfrac <- 0.25       # Fraction of capital that is mobile (0.25)
movefactor <- 1       # Cost to relocate people as a factor of annual income (Tol 3x RMendelsohn 0.5x) (1)
capmovefactor <- 0.1    # Cost to relocate mobile capital as a fraction of asset value (0.1)
democost <- 0.05         # Cost to demolish immobile capital as fraction of asset (0.05)

# # ---Surge Exposure Parameters---
# Protection case
psig0 <- ciam_params[[3]]$psig0
psig0coef <- ciam_params[[3]]$psig0coef
psigA <- ciam_params[[3]]$psigA          # psigA in GAMS code
psigB <- ciam_params[[3]]$psigB          # psigB in GAMS code

# Retreat / No Adapt Cases
rsig0 <- ciam_params[[3]]$rsig0
rsigA <- ciam_params[[3]]$rsigA          # rsigA in GAMS code
rsigB <- ciam_params[[3]]$rsigB          # rsigB in GAMS code

# ---Storm damage parameters---
floodmortality <- 0.01    # Flood deaths as percent of exposed population; (Jonkman Vrijling 2008) (0.01)

# if TRUE, then VSL is exogenously calculated for each country and set in vsl_ciam_country, then each segment is set by looking up its country
# if FALSE, then VSL is endogenously calculated using vslel and vslmult as well as other endogenous socioeconomics for each segment
vsl_exogenous <- TRUE

vsl_ciam_country <- vsl_ciam_country    # Value of statistical life (million 2010$) (only used for exogenous calculation of vsl)
vslel <- 0.5          # Elasticity of vsl (0.5) (only used for endogenous calculation of vsl)
vslmult <- 216        # multiplier on USA GDP (216)(only used for endogenous calculation of vsl)

vsl <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))    # Value of statistical life (million 2010$)

# ---Wetland Loss Parameters---
wvbm <-  0.376    # Annual value of wetland services (million 2010$ / km^2 / yr); (Brander et al 2006)  (0.376)
wetland <- ciam_params[[3]]$wetland             # Initial wetland area in coastal segment (km^2)
wmaxrate <- 0.01                   # Maximum rate of wetland accretion (m per yr) per Kirwan et al 2010 (0.01)
wvel <- 1.16                                      # income elasticity of wetland value (1.16) (Brander et al, 2006)
wvpdl <- 0.47                                     # Population density elasticity of wetland value (0.47) (Brander et al, 2006)

wetlandservice <- matrix(nrow = length(ciam_times),ncol = length(ciam_country))           # Annual value of wetland services adjusted for income and density (Brander et al 2006) ($2010M/km^2/year)
wetlandloss <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))                  # Fractional loss of wetland due to slr

# ---Sea Level Rise Parameters---
# lslr <- lslr            # Local sea level rise (m)
adaptoptions <- ciam_params[[3]]$adaptoptions                                # Index of available adaptation levels for protect and retreat (0 is no adaptation)
surgeexposure <- ciam_params[[3]]$surgeexposure          # Storm surge exposure levels (corresponding to each designated adaptation option)

# ---Coastal Area Parameters---
area1 <- ciam_params[[3]]$area1
area2 <- ciam_params[[3]]$area2
area3 <- ciam_params[[3]]$area3
area4 <- ciam_params[[3]]$area4
area5 <- ciam_params[[3]]$area5
area6 <- ciam_params[[3]]$area6
area7 <- ciam_params[[3]]$area7
area8 <- ciam_params[[3]]$area8
area9 <- ciam_params[[3]]$area9
area10 <- ciam_params[[3]]$area10
area11 <- ciam_params[[3]]$area11
area12 <- ciam_params[[3]]$area12
area13 <- ciam_params[[3]]$area13
area14 <- ciam_params[[3]]$area14
area15 <- ciam_params[[3]]$area15
areaparams <- matrix(nrow = length(ciam_segments),ncol = 15)        # Nothing is computed; this is just a convenient container for area params
coastArea <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))      # Coast area inundated (km^2)

# ---Intermediate Variables---
WetlandNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
FloodNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
StormCapitalNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
StormPopNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
RelocateNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
StormLossNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
DryLandLossNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))

Construct <- array(0, dim = c(length(ciam_times), length(ciam_segments), 5))
WetlandProtect <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
StormCapitalProtect <- array(0, dim = c(length(ciam_times), length(ciam_segments), 5))
StormPopProtect <- array(0, dim = c(length(ciam_times), length(ciam_segments), 5))
StormLossProtect <- array(0, dim = c(length(ciam_times), length(ciam_segments), 5))
FloodProtect <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))

WetlandRetreat <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
StormCapitalRetreat <- array(0, dim = c(length(ciam_times), length(ciam_segments), 6))
StormPopRetreat <- array(0, dim = c(length(ciam_times), length(ciam_segments), 6))
StormLossRetreat <- array(0, dim = c(length(ciam_times), length(ciam_segments), 6))
FloodRetreat <- array(0, dim = c(length(ciam_times), length(ciam_segments), 6))
RelocateRetreat <- array(0, dim = c(length(ciam_times), length(ciam_segments), 6))
DryLandLossRetreat <- array(0, dim = c(length(ciam_times), length(ciam_segments), 6))
coastAreaRetreat <- array(0, dim = c(length(ciam_times), length(ciam_segments), 6))
coastAreaNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))

# --- Decision Variables --- (evaluated brute force)
library(abind)
H <- array(dim=c(length(ciam_times), length(ciam_segments), 5))
R <- array(dim=c(length(ciam_times), length(ciam_segments), 6))
SIGMA <- array(dim=c(length(ciam_times), length(ciam_segments), 12))
OptimalH <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalR <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
WetlandLossOptimal <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
DryLandLossOptimal <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
WetlandLost <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
NoAdaptCost <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
ProtectCost <- array(dim=c(length(ciam_times), length(ciam_segments), 5))
RetreatCost <- array(dim=c(length(ciam_times), length(ciam_segments), 6))
OptimalRetreatLevel <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalProtectLevel <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalCost <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalLevel <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalOption <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
NPVRetreat <- array(dim=c(length(ciam_times), length(ciam_segments), 6))
NPVProtect <- array(dim=c(length(ciam_times), length(ciam_segments), 5))
NPVNoAdapt <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
NPVOptimal <- matrix(nrow = length(ciam_segments),ncol = 1)
NPVOptimalTotal <- 0
StormLossOptimal <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalStormCapital <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalStormPop <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalConstruct <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalWetland <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalFlood <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))
OptimalRelocate <- matrix(nrow = length(ciam_times),ncol = length(ciam_segments))

slrcost_run_timestep <- function(t) {
  # This is a workaround for a type instability that should be fixed in Mimi.jl
  d_ciam_country <<- as.vector(ciam_country)
  d_segments <<- as.vector(ciam_segments)
  
  ti1 <<- 1 # used a lot
  # In first period, initialize all non-adaptation dependent intermediate variables for all timesteps
  if (t==1) {
    #  1. Initialize non-region dependent intermediate variables
    for (i in 1:ntsteps) {
      discountfactor[i] <<- 1 / (1 + discountrate)^(tstep * (i - 1))
    }
    
    # 2. Initialize region-dependent intermediate variables
    for (r in 1:length(d_ciam_country)) {
      # Determine land input value (true = FUND, false = GTAP)
      if (landinput) {
        fundland[r] <<- min(dvbm, max(0.005, dvbm * slr_ypcc[t, r] * refpopdens[r] / (slr_ypcc[ti1, rgn_ind_usa] * refpopdens[rgn_ind_usa])))
        landdata[r] <<- fundland[r]
      } else {
        landdata[r] <<- gtapland[r]
      }
      
      # Calculate regional wetland service, resilience (rho), and land appreciation variables for the first period and
      #   subsequent periods
      wetlandservice[t, r] <<- wvbm * ((slr_ypcc[t, r] / slr_ypcc[ti1, rgn_ind_usa])^wvel * (refpopdens[r] / 27.59)^wvpdl)
      rho[t, r] <<- slr_ypcc[t, r] / (slr_ypcc[t, r] + slr_ypcc[ti1, rgn_ind_usa])
      land_appr[t, r] <<- 1.0
      
      for (i in 2:ntsteps) {
        tim <<- i
        tim1 <<- i - 1
        land_appr[tim, r] <<- land_appr[tim1, r] * exp(0.565 * growthrate(slr_ypcc[tim1, r], slr_ypcc[tim, r]) + 0.313 * growthrate(slr_pop[tim1, r], slr_pop[tim, r]))
        wetlandservice[tim, r] <<- land_appr[tim, r] * wetlandservice[ti1, r]
        rho[tim, r] <<- slr_ypcc[tim, r] / (slr_ypcc[tim, r] + slr_ypcc[ti1, rgn_ind_usa])
      }
    }
    
  # 3. Initialize segment-dependent variables
  for (m in 1:length(d_segments)) {
    rgn_ind <<- as.integer(getregion(m, xsc)) # Identify the region the segment belongs to
    
    # Initialize first-period population density, coast area and surge parameters
    if (popinput == 0) {
      popdens_seg[t, m] <<- popdens[m]
    } else if (popinput == 1) {
      stop("The `popinput` argument values of 1 and 2 are not supported at this time. In the future they will indicate use of Jones and O'Neill 2016 or Merkens et al 2016 population data, respectively.")
      # popdens_seg[t,m]=popdens_seg_jones[ti1,m]
    } else if (popinput == 2) {
      stop("The `popinput` argument values of 1 and 2 are not supported at this time. In the future they will indicate use of Jones and O'Neill 2016 or Merkens et al 2016 population data, respectively.")
      # popdens_seg[t,m]=popdens_seg_merkens[ti1,m]
    }
    
    areaparams[m, ] <<- c(area1[m], area2[m], area3[m], area4[m], area5[m], area6[m], area7[m], area8[m], area9[m], area10[m], area11[m], area12[m], area13[m], area14[m], area15[m])
    
    # Greenland segments are treated differently
    if (as.integer(isgreenland(m, xsc)) == 1) {
      ypc_seg[t, m] <<- 22642 * 1.01^1 # FLAG: assumes t is an index (1-20)
      coastland[t, m] <<- (land_appr[ti1, rgn_ind_canada] * landdata[rgn_ind_canada]) * max(0.5, log(1 + popdens_seg[t, m]) / log(25))
      landvalue[t, m] <<- min(coastland[t, m], (land_appr[ti1, rgn_ind_canada] * landdata[rgn_ind_canada]))
    } else {
      ypc_seg[t, m] <<- slr_ypcc[t, rgn_ind] * max(0.9, (popdens_seg[ti1, m] / 250.0)^0.05)
      coastland[t, m] <<- max(0.5, log(1 + popdens_seg[t, m]) / log(25)) * (land_appr[t, rgn_ind] * landdata[rgn_ind]) # Interior * scaling factor
      landvalue[t, m] <<- min(coastland[t, m], (land_appr[t, rgn_ind] * landdata[rgn_ind]))
    }
    # Calculate VSL
    if (vsl_exogenous) {
      vsl[t, m] <<- vsl_ciam_country[ti1, rgn_ind] # pull VSL from vsl_ciam_country parameter for the proper region
    } else { # endogenous calculation of VSL
      if (isgreenland(m, xsc) == 1) { # Greenland segments are treated differently
        vsl[t, m] <<- 1e-6 * vslmult * slr_ypcc[ti1, rgn_ind_usa] * (ypc_seg[t, m] / slr_ypcc[ti1, rgn_ind_usa])^vslel
      } else {
        vsl[t, m] <<- 1e-6 * vslmult * slr_ypcc[ti1, rgn_ind_usa] * (slr_ypcc[t, rgn_ind] / slr_ypcc[ti1, rgn_ind_usa])^vslel
      }
    }
  
  capital[t, m] <<- kgdp * ypc_seg[t, m] * popdens_seg[t, m] * 1e-6
  coastArea[t, m] <<- calcCoastArea(areaparams[m,], lslr[t, m])
  
  for (i in 2:ntsteps) {
    ti <<- i
    tim1 <<- i-1
    
    if (popinput == 0) {
      popdens_seg[ti, m] <<- popdens_seg[tim1, m] * (1 + growthrate(slr_pop[tim1, rgn_ind], slr_pop[ti, rgn_ind]))
    } else if (popinput == 1) {
      stop("The `popinput` argument values of 1 and 2 are not supported at this time.  In the future they will indicate use of Jones and O'Neill 2016 or Merkens et al 2016 population data, respectively.")
      # popdens_seg[ti,m]=popdens_seg_jones[ti,m]
    } else if (popinput == 2) {
      stop("The `popinput` argument values of 1 and 2 are not supported at this time.  In the future they will indicate use of Jones and O'Neill 2016 or Merkens et al 2016 population data, respectively.")
      # popdens_seg[ti,m]=popdens_seg_merkens[ti,m]
    }
    
    # Special treatment for Greenland segments
    if (isgreenland(m, xsc) == 1) {
      ypc_seg[ti, m] <<- 22642 * 1.01^i   # FLAG: assumes i is an index (1-20)
      coastland[ti, m] <<- (land_appr[ti, rgn_ind_canada] * landdata[rgn_ind_canada]) * max(0.5, log(1 + popdens_seg[ti, m]) / log(25))
      landvalue[ti, m] <<- min(coastland[ti, m], (land_appr[ti, rgn_ind_canada] * landdata[rgn_ind_canada]))
    } else {
      ypc_seg[ti, m] <<- slr_ypcc[ti, rgn_ind] * max(0.9, (popdens_seg[ti1, m] / 250.0)^0.05) # slr_ypcc * popdens scaling factor
      coastland[ti, m] <<- max(0.5, log(1 + popdens_seg[ti, m]) / log(25)) * (land_appr[ti, rgn_ind] * landdata[rgn_ind])
      landvalue[ti, m] <<- min(coastland[ti, m], (land_appr[ti, rgn_ind] * landdata[rgn_ind]))
    }
    if (vsl_exogenous) {
      vsl[ti, m] <<- vsl_ciam_country[ti, rgn_ind]
    } else {
      if (isgreenland(m, xsc) == 1) {
        vsl[ti, m] <<- 1e-6 * vslmult * slr_ypcc[ti, rgn_ind_usa] * (ypc_seg[ti, m] / slr_ypcc[ti, rgn_ind_usa]) ^ vslel
      } else {
        vsl[ti, m] <<- 1e-6 * vslmult * slr_ypcc[ti, rgn_ind_usa] * (slr_ypcc[ti, rgn_ind] / slr_ypcc[ti, rgn_ind_usa]) ^ vslel
      }
    }
    
    capital[ti, m] <<- kgdp * ypc_seg[ti, m] * popdens_seg[ti, m] * 1e-6
    coastArea[ti, m] <<- calcCoastArea(areaparams[m, ], lslr[ti, m])
    wetlandloss[tim1, m] <<- min(1, (localrate(lslr[tim1, m], lslr[ti, m], tstep) / wmaxrate) ^ 2)
  }
    wetlandloss[ntsteps, m] <<- min(1, (localrate(lslr[ntsteps - 1, m], lslr[ntsteps, m], tstep) / wmaxrate) ^ 2)
  }
  }
  
  if (t %in% at) {
    adapt_range <<- 1:length(adaptoptions)
    
    # Determine length of adaptation period ("atstep")
    g <- function(c) c == t
    at_index <<- which(g(at))[1] # Find index corresponding to adaptation period in at
    at_index_next <<- at_index + 1    # Find index corresponding to next adaptation period
    at_index_prev <<- max(1, at_index - 1)
    
    at_prev <<- as.integer(at[at_index_prev])      # TODO pick one - either index into vector or use the period, it's confusing
    
    if (at_index_next <= length(at)) {
      atstep <<- (at[at_index_next] - at[at_index]) * tstep   # In years
      at_next <<- as.integer(at[at_index_next])
      last_t <<- at_next - 1
      last_idx <- 0
    } else {
      # Deal with special case of last adaptation period
      atstep <<- tstep * ntsteps - ((at[at_index] - 1) * tstep)
      at_next <<- ntsteps # Flag this assumes timesteps are indices not years (1:20 not 2010:2100)
      last_t <<- at_next
      last_idx <- 1
    }
    t_range <<- t:last_t
    
    for (m in 1:length(d_segments)) {
      if (atstep == 0) {
      } else {
        rgn_ind <<- as.integer(getregion(m, xsc))
        
        # ** Calculate No Adaptation Costs **
        for (i in t_range) {
          ti <<- (i)
          tim1 <<- (i - 1)
          R_NoAdapt <<- max(0, lslr[ti, m])
          
          # For initial state in SLR cases, make adaptation decision relative to baseline (refA_H or R)
          #if rcp>0
          if (rcp >= 0) {
            R_NoAdapt <<- max(R_NoAdapt, refA_H[m], refA_R[m])
          }
          
          # Incorporate any previous period adaptation
          if (!fixed && !(t==1)) {
            R_NoAdapt <<- max(R_NoAdapt, OptimalR[(t - 1), m])
            WetlandNoAdapt[ti, m] <<- tstep * wetlandservice[ti, rgn_ind] * max(WetlandLossOptimal[(t - 1), m], wetlandloss[ti, m] * min(coastArea[ti, m], wetland[m]))
            if (i == t) {
              # For start of new adaptation period, take into account (lack of) retreat done in previous periods (i.e. if they protected instead)
              # This results in double-costs for this period b/c no adaptation is set up to compute relative to t+1 lslr
              coastAreaNoAdapt[ti, m] <<- calcCoastArea(areaparams[m, ], OptimalR[(t - 1), m])
            } else {
              coastAreaNoAdapt[ti, m] <<- calcCoastArea(areaparams[m, ], R_NoAdapt)
            }
          } else {
            coastAreaNoAdapt[ti, m] <<- coastArea[ti, m]
            WetlandNoAdapt[ti, m] <<- tstep * wetlandservice[ti, rgn_ind] * wetlandloss[ti, m] * min(coastArea[ti, m], wetland[m])
          }
          
          # Storm Costs
          SIGMA[ti, m, 1] <<- rsig0[m] / (1 + rsigA[m] * exp(rsigB[m] * max(0, R_NoAdapt - lslr[ti, m]))) # expected value of exposure area
          StormCapitalNoAdapt[ti, m] <<- tstep * (1 - rho[ti, rgn_ind]) * SIGMA[ti, m, 1] * capital[ti, m]
          StormPopNoAdapt[ti, m] <<- tstep * (1 - rho[ti, rgn_ind]) * popdens_seg[ti, m] * vsl[ti, m] * floodmortality * SIGMA[ti, m, 1]
          
          StormLossNoAdapt[ti, m] <<- tstep * (1 - rho[ti, rgn_ind]) * popdens_seg[ti, m] * floodmortality * SIGMA[ti, m, 1]
          if (i == ntsteps) {
            DryLandLossNoAdapt[ti, m] <<- max(0, coastAreaNoAdapt[ti, m]) # km^2
          } else {
            # In case of negative or decreasing slr, can assume that previous inundated area is reclaimed
            DryLandLossNoAdapt[ti, m] <<- max(0, coastAreaNoAdapt[ti, m], coastArea[i + 1, m]) # includes future period loss and previous adaptation if applicable
          }
          
          # Flood and relocation costs
          if (i == ntsteps) {
            FloodNoAdapt[ti, m] <<- FloodNoAdapt[tim1, m]
            RelocateNoAdapt[ti, m] <<- RelocateNoAdapt[tim1, m]
          } else {
            FloodNoAdapt[ti, m] <<- tstep * landvalue[ti, m] * 0.04 * DryLandLossNoAdapt[ti, m] + max(0, DryLandLossNoAdapt[ti, m] - coastAreaNoAdapt[ti, m]) * (1 - mobcapfrac) * capital[ti, m]
            RelocateNoAdapt[ti, m] <<- max(0, DryLandLossNoAdapt[ti, m] - coastAreaNoAdapt[ti, m]) * (5 * movefactor * ypc_seg[ti, m] * 1e-6 * popdens_seg[ti, m] + capmovefactor * mobcapfrac * capital[ti, m] + democost * (1 - mobcapfrac) * capital[ti, m])
          }
          
          # Put all costs into $Billions and divide by 10 to account for 10-y time step
          WetlandNoAdapt[ti, m] <<- WetlandNoAdapt[ti, m] * 1e-4
          if (i < ntsteps) { # already occurred in previous timestep
            FloodNoAdapt[ti, m] <<- FloodNoAdapt[ti, m] * 1e-4
            RelocateNoAdapt[ti, m] <<- RelocateNoAdapt[ti, m] * 1e-4
          }
          StormCapitalNoAdapt[ti, m] <<- StormCapitalNoAdapt[ti, m] * 1e-4
          StormPopNoAdapt[ti, m] <<- StormPopNoAdapt[ti, m] * 1e-4
          
          NoAdaptCost[ti, m] <<- WetlandNoAdapt[ti, m] + FloodNoAdapt[ti, m] + RelocateNoAdapt[ti, m] + StormCapitalNoAdapt[ti, m] + StormPopNoAdapt[ti, m]
          
        }
        
        if (t==1) {
          NPVNoAdapt[t, m] <<- sum(discountfactor[t_range] * NoAdaptCost[t_range, m] * 10) 
        } else {
          # Compute NPV Relative to planner's perspective (discounting relative to time t)
          NPVNoAdapt[t, m] <<- sum(discountfactor[match(t_range, t_range)] * NoAdaptCost[t_range, m] * 10)
        }
          
        for (j in t_range) {
          NPVNoAdapt[j, m] <<- NPVNoAdapt[t, m]
        }
          
        # ** Calculate Protection and Retreat Costs for Each Adaptation Option **
        lslrPlan_at <<- lslr[(at_next), m]
        lslrPlan_atprev <<- lslr[t, m]
          
        for (i in 1:length(adaptoptions)) {
            if (t==1) {
              Rprev <<- calcHorR(-2, adaptoptions[i], lslr[ti1, m], as.numeric(surgeexposure[m,]), adaptoptions)
              R[t, m, i] <<- calcHorR(-2, adaptoptions[i], lslrPlan_at, as.numeric(surgeexposure[m,]), adaptoptions)
            } else {
              if (fixed == FALSE) {
                Rprev <<- OptimalR[(t - 1), m]
                # Assumption: prior protection does not count because it is no longer maintained
                R[t, m, i] <<- max(OptimalR[(t - 1), m], calcHorR(-2, adaptoptions[i], lslrPlan_at, as.numeric(surgeexposure[m,]), adaptoptions))
              } else {
                Rprev <<- R[(as.integer(at[at_index_prev])), m, i]
                R[t, m, i] <<- calcHorR(-2, adaptoptions[i], lslrPlan_at, as.numeric(surgeexposure[m,]), adaptoptions)
              }
            }
          
          SIGMA[t, m, i+1] <<- (rsig0[m] / (1 + rsigA[m] * exp(rsigB[m] * max(0, R[t, m, i] - lslr[t, m]))))
          coastAreaRetreat[t, m, i] <<- calcCoastArea(areaparams[m,], R[t, m, i])
          
          FloodRetreat[t, m, i] <<- (tstep / atstep) * (atstep * landvalue[t, m] * 0.04 * calcCoastArea(areaparams[m,], R[t, m, i]) +
                                                             max(0, calcCoastArea(areaparams[m,], R[t, m, i]) - calcCoastArea(areaparams[m,], Rprev)) *
                                                             (1 - depr) * (1 - mobcapfrac) * capital[t, m]) * 1e-4
          
          RelocateRetreat[t, m, i] <<- (tstep / atstep) *
            max(0, calcCoastArea(areaparams[m,], R[t, m, i]) - calcCoastArea(areaparams[m,], Rprev)) * (movefactor * ypc_seg[t, m] * 1e-6 * popdens_seg[t, m] + capmovefactor * mobcapfrac * capital[t, m] + democost * (1 - mobcapfrac) * capital[t, m]) * 1e-4
          
          DryLandLossRetreat[t, m, i] <<- max(0, coastAreaRetreat[t, m, i]) # Already takes into account prior adaptation
          
        ########
          if (adaptoptions[i] >= 10 || adaptoptions[i] == 0) {
            if (t==1) {
              Hprev <<- calcHorR(-1, adaptoptions[i], lslr[ti1, m], as.numeric(surgeexposure[m,]), adaptoptions)
              H[t, m, i-1] <<- calcHorR(-1, adaptoptions[i], lslrPlan_at, as.numeric(surgeexposure[m,]), adaptoptions)
              SIGMA[t, m, (i-1)+7] <<- (psig0[m] + psig0coef[m] * max(0, lslr[t, m])) / (1.0 + psigA[m] * exp(psigB[m] * max(0, (H[t, m, i-1] - lslr[t, m]))))
              FloodProtect[t, m] <<- 0
            } else {
              if (fixed == FALSE) {
                Hprev <<- OptimalH[t-1, m]
                # Assumption: any prior retreat is credited toward required height, since not starting from original position on coast
                lslrPlan_Prot <<- lslrPlan_at - OptimalR[t-1, m]
                H[t, m, i-1] <<- max(OptimalH[t-1, m], calcHorR(-1, adaptoptions[i], lslrPlan_Prot, as.numeric(surgeexposure[m,]), adaptoptions))
                SIGMA[t, m, (i-1)+7] <<- (psig0[m] + psig0coef[m] * max(0, lslr[t, m])) / (1.0 + psigA[m] * exp(psigB[m] * max(0, (H[t, m, i-1] + OptimalR[TimestepIndex(gettime(t) - 1), m] - lslr[t, m]))))
                FloodProtect[t, m] <<- tstep * landvalue[t, m] * 0.04 * DryLandLossOptimal[t-1, m]
              } else {
                Hprev <<- H[as.integer(at[at_index_prev]), m, i-1]
                H[t, m, i-1] <<- calcHorR(-1, adaptoptions[i], lslrPlan_at, as.numeric(surgeexposure[m,]), adaptoptions)
                SIGMA[t, m, (i-1)+7] <<- (psig0[m] + psig0coef[m] * max(0, lslr[t, m])) / (1.0 + psigA[m] * exp(psigB[m] * max(0, (H[t, m, i-1] - lslr[t, m]))))
                FloodProtect[t, m] <<- 0
              }
            }
            # Island protection costs are higher
            if (isisland(m, xsc) == 1) {
              pc <<- 2 * pc0 * cci[rgn_ind]
            } else {
              pc <<- pc0 * cci[rgn_ind]
            }
            
            Construct[t, m, i-1] <<- (tstep / atstep) *
              (length[m] * pc * (pcfixed + (1 - pcfixed) * (H[t, m, i-1]^2 - Hprev^2) +
                                     mc * atstep * H[t, m, i-1]) + length[m] * 1.7 * H[t, m, i-1] * landvalue[t, m] * 0.04 / 2 * atstep) * 1e-4
            
            ##
            ## comment out this if block to match the Diaz (2016) GAMS results
            ## This should NOT be commented out moving forward
            ##
            if (Hprev >= H[t, m, i-1]) {
              H[t, m, i-1] <<- Hprev
              # Just maintenance cost + land value
              Construct[t, m, i-1] <<- (tstep / atstep) * (length[m] * pc * mc * atstep * H[t, m, i-1] + length[m] * 1.7 * H[t, m, i-1] * landvalue[t, m] * 0.04 / 2 * atstep) * 1e-4
            }
          }
          
          for (j in t_range) {
            tj <<- j
            R[tj, m, i] <<- R[t, m, i]
            SIGMA[tj, m, i+1] <<- (rsig0[m] / (1 + rsigA[m] * exp(rsigB[m] * max(0, R[tj, m, i] - lslr[tj, m]))))
            coastAreaRetreat[tj, m, i] <<- coastAreaRetreat[t, m, i]
            
            if (!fixed && !(t==1)) {
              WetlandRetreat[tj, m] <<- tstep * wetlandservice[tj, rgn_ind] * max(WetlandLossOptimal[(t - 1), m], wetlandloss[(i), m] * min(coastArea[(i), m], wetland[m]))
            } else {
              WetlandRetreat[tj, m] <<- tstep * wetlandservice[tj, rgn_ind] * wetlandloss[tj, m] * min(coastArea[tj, m], wetland[m])
            }
            
            StormCapitalRetreat[tj, m, i] <<- tstep * (1 - rho[tj, rgn_ind]) * SIGMA[tj, m, i+1] * capital[tj, m]
            StormPopRetreat[tj, m, i] <<- tstep * (1 - rho[tj, rgn_ind]) * SIGMA[tj, m, i+1] * popdens_seg[tj, m] * vsl[tj, m] * floodmortality
            StormLossRetreat[tj, m, i] <<- tstep * (1 - rho[tj, rgn_ind]) * SIGMA[tj, m, i+1] * popdens_seg[tj, m] * floodmortality
            
            FloodRetreat[tj, m, i] <<- FloodRetreat[t, m, i]
            RelocateRetreat[tj, m, i] <<- RelocateRetreat[t, m, i]
            DryLandLossRetreat[tj, m, i] <<- DryLandLossRetreat[t, m, i]
            
            # Put all other costs intp $Billions from $M and divide by 10
            StormCapitalRetreat[tj, m, i] <<- StormCapitalRetreat[tj, m, i] * 1e-4
            StormPopRetreat[tj, m, i] <<- StormPopRetreat[tj, m, i] * 1e-4
            WetlandRetreat[tj, m] <<- WetlandRetreat[tj, m] * 1e-4
            
            RetreatCost[tj, m, i] <<- FloodRetreat[tj, m, i] + RelocateRetreat[tj, m, i] + StormCapitalRetreat[tj, m, i] + StormPopRetreat[tj, m, i] + WetlandRetreat[tj, m]
            
            if (adaptoptions[i] >= 10 || adaptoptions[i] == 0) {
              H[tj, m, i-1] <<- H[t, m, i-1]
              
              if (!fixed && !t==1) {
                SIGMA[tj, m, (i-1)+7] <<- (psig0[m] + psig0coef[m] * max(0, lslr[tj, m])) / (1.0 + psigA[m] * exp(psigB[m] * max(0, (H[tj, m, i-1] + OptimalR[(t - 1), m] - lslr[tj, m]))))
                FloodProtect[tj, m] <<- tstep * landvalue[tj, m] * 0.04 * DryLandLossOptimal[(t - 1), m]
              } else {
                SIGMA[tj, m, (i-1)+7] <<- (psig0[m] + psig0coef[m] * max(0, lslr[tj, m])) / (1.0 + psigA[m] * exp(psigB[m] * max(0, (H[tj, m, i-1] - lslr[tj, m]))))
                FloodProtect[tj, m] <<- FloodProtect[t, m]
              }
              
              WetlandProtect[tj, m] <<- tstep * wetland[m] * wetlandservice[tj, rgn_ind]
              
              StormCapitalProtect[tj, m, i-1] <<- tstep * (1 - rho[tj, rgn_ind]) * SIGMA[tj, m, (i-1)+7] * capital[tj, m]
              StormPopProtect[tj, m, i-1] <<- tstep * (1 - rho[tj, rgn_ind]) * SIGMA[tj, m, (i-1)+7] * popdens_seg[tj, m] * vsl[tj, m] * floodmortality
              StormLossProtect[tj, m, i-1] <<- tstep * (1 - rho[tj, rgn_ind]) * SIGMA[tj, m, (i-1)+7] * popdens_seg[tj, m] * floodmortality
              
              Construct[tj, m, i-1] <<- Construct[t, m, i-1]
              
              # Put all other costs into $Billions from $M and divide by 10
              # Note this is an annual protect cost ($B/year)
              WetlandProtect[tj, m] <<- WetlandProtect[tj, m] * 1e-4
              StormCapitalProtect[tj, m, i-1] <<- StormCapitalProtect[tj, m, i-1] * 1e-4
              StormPopProtect[tj, m, i-1] <<- StormPopProtect[tj, m, i-1] * 1e-4
              FloodProtect[tj, m] <<- FloodProtect[tj, m] * 1e-4
              
              ProtectCost[tj, m, i-1] <<- Construct[tj, m, i-1] + WetlandProtect[tj, m] + StormCapitalProtect[tj, m, i-1] + StormPopProtect[tj, m, i-1] + FloodProtect[tj, m]
            }
          }
          if (t==1) {
            NPVRetreat[t, m, i] <<- sum(discountfactor[t_range]*RetreatCost[t_range, m, i] * 10 )
          } else {
            # Compute NPV Relative to planner's perspective (discounting relative to time t)
            NPVRetreat[t, m, i] <<- sum(discountfactor[match(t_range,t_range)] * RetreatCost[t_range, m, i] * 10)
            #NPVRetreat[t-1,m,i] + sum([discountfactor[j] * RetreatCost[j,m,i] for j in t_range])
          }
          
          for (j in t_range) {
            NPVRetreat[j, m, i] <<- NPVRetreat[t, m, i]
          }
          
          if (adaptoptions[i] >= 10 || adaptoptions[i] == 0) {
            if (t==1) {
              NPVProtect[t, m, i-1] <<- sum(discountfactor[t_range] * ProtectCost[t_range, m, i-1] * 10) # Protect
            } else {
              # Compute NPV Relative to planner's perspective (discounting relative to time t)
              NPVProtect[t, m, i-1] <<- sum(discountfactor[match(t_range,t_range)] * ProtectCost[t_range, m, i-1] * 10)
              #NPVProtect[t-1,m,i-1] + sum( [ discountfactor[j] * ProtectCost[j,m,i-1]) # Protect
            }
            
            for (j in t_range) {
              NPVProtect[j, m, i-1] <<- NPVProtect[t, m, i-1]
            }
          }
          
        }
        
        # ** Choose Least Cost Option **
        if (t > 1 && fixed) {
            # if fixed==T and t>1, take first-period choices
            for (j in t_range) {
              tj <<- j
              OptimalProtectLevel[tj, m] <<- OptimalProtectLevel[ti1, m]
              OptimalRetreatLevel[tj, m] <<- OptimalRetreatLevel[ti1, m]
              OptimalOption[tj, m] <<- OptimalOption[ti1, m]
              OptimalLevel[tj, m] <<- OptimalLevel[ti1, m]
            }
        } else {
            # If fixed==F or if fixed==T and t==1, calculate optimal level.
            if (allowMaintain == TRUE) {
              protectInd <<- which.min(NPVProtect[(as.integer(at[at_index])), m, ])
              retreatInd <<- which.min(NPVRetreat[(as.integer(at[at_index])), m, ])
            } else {
              protDims <<- dim(NPVProtect)[3]
              retDims <<- dim(NPVRetreat)[3]
              protectInd <<- which.min(NPVProtect[(as.integer(at[at_index])), m, 1:(protDims-1)])
              retreatInd <<- which.min(NPVRetreat[(as.integer(at[at_index])), m, 1:(retDims-1)])
            }
            for (j in t_range) {
              OptimalProtectLevel[j, m] <<- adaptoptions[protectInd+1]
            }

            if (noRetreat == TRUE) {
              minLevels <<- c(adaptoptions[protectInd+1], 0.0)
              choices <<- c(NPVProtect[(as.integer(at[at_index])), m, protectInd], NPVNoAdapt[(as.integer(at[at_index])), m])

              leastcost <<- -1 * which.min(choices)
              if (leastcost == -2) {
                leastcost <<- -3 # Account for retreat being removed from choice set
              }
              leastlevel <<- minLevels[which.min(choices)]
            } else {
              for (j in t_range) {
                OptimalRetreatLevel[j, m] <<- adaptoptions[retreatInd]
              }
              minLevels <<- c(adaptoptions[protectInd+1], adaptoptions[retreatInd], 0.0)

              choices <<- c(NPVProtect[(as.integer(at[at_index])), m, protectInd], NPVRetreat[(as.integer(at[at_index])), m, retreatInd], NPVNoAdapt[(as.integer(at[at_index])), m])
              leastcost <<- -1 * which.min(choices)
              leastlevel <<- minLevels[which.min(choices)]
            }
            for (j in t_range) {
              tj <<- j
              OptimalOption[tj, m] <<- leastcost
              OptimalLevel[tj, m] <<- leastlevel
            }
          }

          # Assign costs to optimal variables
          if (OptimalOption[t, m] == -1) {

            # Protect Cost
            protInd <<- which(adaptoptions == OptimalLevel[t, m])[1] - 1
            for (j in t_range) {
              tj <<- j
              OptimalCost[tj, m] <<- ProtectCost[tj, m, protInd]
              # Assign Subcosts
              OptimalStormCapital[tj, m] <<- StormCapitalProtect[tj, m, protInd]
              OptimalStormPop[tj, m] <<- StormPopProtect[tj, m, protInd]
              OptimalConstruct[tj, m] <<- Construct[tj, m, protInd]
              OptimalWetland[tj, m] <<- WetlandProtect[tj, m]
              OptimalRelocate[tj, m] <<- 0
              OptimalFlood[tj, m] <<- FloodProtect[tj, m]
              # Assign Alternative Metrics
              # Assume once seawall is built, wetland area is permanently destroyed
              WetlandLossOptimal[tj, m] <<- wetland[m]
            }
            if (t == 1) {
              for (i in t_range) {
                ti <<- (i)
                DryLandLossOptimal[ti, m] <<- 0
                OptimalH[ti, m] <<- max(0, H[ti, m, protInd])
                OptimalR[ti, m] <<- 0
                if (i == 1) {
                  StormLossOptimal[ti, m] <<- StormLossProtect[ti, m, protInd]
                } else {
                  StormLossOptimal[ti, m] <<- StormLossOptimal[(i - 1), m] + StormLossProtect[ti, m, protInd]
                }
              }
            } else {
              for (j in t_range) {
                OptimalR[j, m] <<- max(0, OptimalR[(t - 1), m])
              }
              for (i in t_range) {
                ti <<- (i)
                OptimalH[ti, m] <<- max(H[ti, m, protInd], OptimalH[(t - 1), m])

                DryLandLossOptimal[ti, m] <<- max(0, DryLandLossOptimal[(i - 1), m])
                StormLossOptimal[ti, m] <<- StormLossOptimal[(i - 1), m] + StormLossProtect[ti, m, protInd]
              }
            }
          } else if (OptimalOption[t, m] == -2) {
              # Retreat Cost

              retInd <<- which(adaptoptions == OptimalLevel[t, m])[1]
              for (j in t_range) {
                tj <<- j
                OptimalCost[tj, m] <<- RetreatCost[tj, m, retInd]
                # Assign Subcosts
                OptimalStormCapital[tj, m] <<- StormCapitalRetreat[tj, m, retInd]
                OptimalStormPop[tj, m] <<- StormPopRetreat[tj, m, retInd]
                OptimalConstruct[tj, m] <<- 0
                OptimalWetland[tj, m] <<- WetlandRetreat[tj, m]
                OptimalFlood[tj, m] <<- FloodRetreat[tj, m, retInd]
                OptimalRelocate[tj, m] <<- RelocateRetreat[tj, m, retInd]
              }

              if (t==1) {
                for (j in t_range) {
                  tj <<- j
                  DryLandLossOptimal[tj, m] <<- DryLandLossRetreat[tj, m, retInd]
                  OptimalH[tj, m] <<- 0
                }

                for (i in t_range) {
                  ti <<- (i)
                  OptimalR[ti, m] <<- max(0, R[ti, m, retInd])
                  WetlandLossOptimal[ti, m] <<- wetlandloss[ti, m] * min(coastArea[ti, m], wetland[m])
                  if (i == 1) {
                    StormLossOptimal[ti, m] <<- StormLossRetreat[ti, m, which(adaptoptions == OptimalLevel[t, m])]
                  } else {
                    StormLossOptimal[ti, m] <<- StormLossOptimal[(i - 1), m] + StormLossRetreat[ti, m, retInd]
                  }
                }
            } else{
              for (j in t_range) {
                OptimalH[j, m] <<- OptimalH[(t - 1), m]
              }

              for (i in t_range) {
                ti <<- (i)
                OptimalR[ti, m] <<- max(R[ti, m, retInd], OptimalR[(t - 1), m])
                # Cumulative total wetland area lost; if protected previously, all wetland is lost
                WetlandLossOptimal[ti, m] <<- max(WetlandLossOptimal[(t - 1), m], wetlandloss[ti, m] * min(coastArea[ti, m], wetland[m]))
                StormLossOptimal[ti, m] <<- StormLossOptimal[(i - 1), m] + tstep * (1 - rho[ti, rgn_ind]) * popdens_seg[ti, m] * floodmortality * SIGMA[ti, m, retInd]
                DryLandLossOptimal[ti, m] <<- max(DryLandLossOptimal[(i - 1), m], DryLandLossRetreat[ti, m, retInd])
              }
            }
          } else{
            # No Adaptation
            for (j in t_range) {
              tj <<- j
              OptimalCost[tj, m] <<- NoAdaptCost[tj, m]
              # Assign Subcosts
              OptimalStormCapital[tj, m] <<- StormCapitalNoAdapt[tj, m]
              OptimalStormPop[tj, m] <<- StormPopNoAdapt[tj, m]
              OptimalConstruct[tj, m] <<- 0
              OptimalWetland[tj, m] <<- WetlandNoAdapt[tj, m]
              OptimalFlood[tj, m] <<- FloodNoAdapt[tj, m]
              OptimalRelocate[tj, m] <<- RelocateNoAdapt[tj, m]
            }
            if (t==1) {
              for (j in t_range) {
                tj <<- j
                OptimalH[tj, m] <<- 0
                DryLandLossOptimal[tj, m] <<- DryLandLossNoAdapt[tj, m]
              }
              OptimalR[t, m] <<- max(0, lslr[t, m])
              for (i in t_range) {
                ti <<- (i)
                if (i > 1) {
                  OptimalR[ti, m] <<- max(OptimalR[t, m], OptimalR[(i - 1), m], lslr[ti, m])
                } else {
                  OptimalR[ti, m] <<- max(OptimalR[t, m], lslr[ti, m])
                }
                WetlandLossOptimal[ti, m] <<- wetlandloss[ti, m] * min(coastArea[ti, m], wetland[m])
                if (i == 1) {
                  StormLossOptimal[ti, m] <<- StormLossNoAdapt[ti, m]
                } else {
                  StormLossOptimal[ti, m] <<- StormLossOptimal[(i - 1), m] + StormLossNoAdapt[ti, m]
                }
              }
            } else {
              for (j in t_range) {
                OptimalH[j, m] <<- OptimalH[(t - 1), m]
              }
              for (i in t_range) {
                ti <<- (i)
                OptimalR[ti, m] <<- max(OptimalR[(t - 1), m], OptimalR[(i - 1), m], lslr[ti, m])
                WetlandLossOptimal[ti, m] <<- max(WetlandLossOptimal[(t - 1), m], wetlandloss[ti, m] * min(coastArea[ti, m], wetland[m]))
                StormLossOptimal[ti, m] <<- StormLossOptimal[(i - 1), m] + StormLossNoAdapt[ti, m]
                DryLandLossOptimal[ti, m] <<- max(DryLandLossOptimal[(i - 1), m], DryLandLossNoAdapt[ti, m])
              }
            }
          }
          if (last_idx == 1) {
            NPVOptimal[m] <<- sum(discountfactor[t_range] * OptimalCost[t_range, m] * 10)
          }
        }
      }
      
      if (last_idx == 1) {
        NPVOptimalTotal <<- sum(NPVOptimal)
      }
  }# end if t in adaptation period statement
}# end function

