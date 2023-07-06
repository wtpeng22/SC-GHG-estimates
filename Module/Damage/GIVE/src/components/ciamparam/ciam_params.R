library(dplyr)

# Supporting functions to support setting CIAM parameters
# Adapted from scripts in MimiCIAM.jl
# Process the segment-country mapping file (xsc) in CIAM by (1) Reads from CSV
# and outputs list of dictionaries and arrays (2) Filters xsc file to desired
# segments/regions


prep_ciam_xsc <- function(xsc_params_path) {
  
  # Read in csv and convert to list format
  xsc_params <- read.csv(xsc_params_path) %>% as.data.frame()
  
  # Create dictionary (list) with segment as key and region/greenland/island as value
  xsc_char <- list()
  for (i in 1:length(xsc_params$seg)) {
    xsc_char[[xsc_params$seg[i]]] <- list(xsc_params$rgn[i], xsc_params$greenland[i], xsc_params$island[i])
  }
  
  # Create region and segment indices
  # rgns <- sort(unlist(unique(sapply(xsc_char, function(i) i[1]))))
  # segs <- sort(unlist(unique(sapply(names(xsc_char), as.character))))
  rgns <- sort(unlist(unique(sapply(xsc_params$rgn, as.character))))
  segs <- sort(unlist(unique(sapply(xsc_params$seg, as.character))))
  segs_rgns_idx <- match(segs,xsc_params$seg)
  
  xsc_ind <- list() # numeric seg -> (numeric rgn, greenland bool)
  xsc_segmap <- list() # Numeric seg/rgn -> char seg/rgn
  xsc_rgnmap <- list()
  
  for (i in 1:length(segs)) {
    r <- xsc_char[[segs_rgns_idx[i]]][[1]]   # Region character
    grn <- xsc_char[[segs_rgns_idx[i]]][[2]] # 0 = non-Greenland, 1 = greenland bool
    isl <- xsc_char[[segs_rgns_idx[i]]][[3]] # 0 = non-island, 1 = island bool
    r_ind <- which(rgns == r)   # Region index
    
    new_val <- list(r_ind, grn, isl)     # New list w/ region index instead of character
    
    # Build XSC Seg/rgn Maps
    r2 <- rgns[r_ind]           # New region char
    s <- segs[i]
    xsc_segmap[[i]] <- s
    if (!(r2 %in% unlist(xsc_rgnmap))) {
      xsc_rgnmap[[r_ind]] <- r2
    }
    xsc_ind[[i]] <- new_val
  }
  
  return(list(xsc_ind, rgns, segs, xsc_rgnmap))
}

# """
# Obtain the CIAM parameters for the ciam_countries using the key in xsc_params_path
# for a model with time dimension first:tstep:last and adaptation starting in `adaptation_firsts`.
# """

get_ciam_params <- function(tstep, first, last, ciam_countries, xsc_params_path, adaptation_firsts) {
  
  # --------------------------------------------------------------------------
  # Get CIAM Default Parameters
  # Pull in main parameters and select just our countries
  ciam_params <- load_ciam_params()
  
  for (k in names(ciam_params)) {
    v <- ciam_params[[k]]
    if ("country" %in% names(v)) {
      ciam_params[[k]] <- subset(v, country %in% ciam_countries)
    }
  }
  
  # Process XSC (segment-country mapping dictionary)
  xsc <- prep_ciam_xsc(xsc_params_path)
  xsc_ind <- xsc[[1]]
  rgns <- xsc[[2]]
  segs <- xsc[[3]]
  xsc_rgnmap <- xsc[[4]]
  # stopifnot(identical(rgns, ciam_countries), "The provided ciam_countries in the get_ciam_params function must match those in the provided xsc_params_path File.")
  
  # Process params using xsc
  ciam_params =  parse_ciam_params(ciam_params, rgns, segs, 0)
  
  # --------------------------------------------------------------------------
  # Adjust, Delete, and Add Parameters
  
  # --> Delete Parameters that never get used
  for (p in c("s1000", "s100", "s10", "smax", "land_appr_canada", "ypc_usa", "gtapland_canada", "wbvm", "fundland_canada", "refpopdens_usa")) {
    ciam_params[[p]] <- NULL
  }
  
  # --> Time Related
  ciam_params[["tstep"]] <- tstep # Length of individual time-step (years)
  ciam_params[["at"]] <- adaptation_firsts # times that start each adaptation period
  ciam_params[["ntsteps"]] <- length(seq(from=first, to=last, by=tstep))
  
  # --> Metadata; not used in run
  ciam_params[["rcp"]] <- 0
  ciam_params[["percentile"]] <- 50
  ciam_params[["ssp"]] <- 0
  
  # --> Default Settings
  ciam_params[["fixed"]] <- TRUE
  ciam_params[["noRetreat"]] <- FALSE
  ciam_params[["allowMaintain"]] <- FALSE
  ciam_params[["popinput"]] <- 0
  ciam_params[["discountrate"]] <- 0.04
  
  # --> IDs and Dimensions
  
  # Dynamically find indices corresponding to USA and CAN and manually set time steps
  # If the lengths are 0, then assume those segments are not used. Note that
  # if including Greenland, need Canada too as a reference for land appreciation
  
  rgn_ind_canada <- which(xsc_rgnmap == "CAN")
  rgn_ind_canada <- ifelse(length(rgn_ind_canada) > 0, rgn_ind_canada[1], 0)
  
  rgn_ind_usa <- which(xsc_rgnmap == "USA")
  rgn_ind_usa <- ifelse(length(rgn_ind_usa) > 0, rgn_ind_usa[1], 0)
  
  segID <- segStr_to_segID(segs)
  
  ciam_params[["segID"]] <- segID
  ciam_params[["xsc"]] <- xsc_ind
  ciam_params[["rgn_ind_canada"]] <- rgn_ind_canada
  ciam_params[["rgn_ind_usa"]] <- rgn_ind_usa
  
  # --> Population and GDP Parameters - need to be connected to Socioeconomics
  
  ciam_params <- ciam_params[c(-which(names(ciam_params)=="pop"), -which(names(ciam_params)=="ypcc"))] # pop = Parameter(index = [time, regions])      # Population of region (million people)
  # ypcc = Parameter(index = [time, regions])     # GDP per capita per region ($2010 per capita)
  
  # # --> Storm Damage Parameters - we adjust these to be consistent with the VSL
  # # component, so remove these two parameters (see calc of vsl_ciam_country in
  # # main_ciam.jl))
  # ciam_params <- ciam_params[c(-which(names(ciam_params)=="vslel"), -which(names(ciam_params)=="vslmult"))]
  
  return(list(rgns, segs, ciam_params))
}


