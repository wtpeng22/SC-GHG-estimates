library(readr)
library(dplyr)

# ---------------------------------
#   1. Read In Helper Functions
# ---------------------------------
load_ciam_params <- function() {
    
  data_dir <- file.path("..","data","input")
  files <- list.files(data_dir)
  files <- files[!(files %in% c("desktop.ini", ".DS_Store", "xsc.csv"))]

  params <- list()
  for (m in files) {
    name <- tolower(tools::file_path_sans_ext(m))
    params[[name]] <- read_csv(file.path(data_dir, m))
  }
  
  return(params)
}


# """
#     preplsl!(lslfile,subset, params,segnames)
# 
# Read in LSLR from file and filter to desired set of segments, note that this modifies
# the input parameter dictionary `params`.  The arguments are as fullows:
# 
# - lslfile - name of lslr file to use; location relative to data/input-data directory
# - subset - list of segments you want
# - params - parameter dictionary you want to add lslr to
# - segnames - names of segments
# """

preplsl <- function(lslfile, subset, params, segnames) {
  
  data_dir <- file.path("data", "lslr")
  lsl_params <- read.csv(file.path(data_dir, lslfile), header = TRUE, stringsAsFactors = FALSE)
  
  # Filter according to subset segments
  if (subset != FALSE) {
    col_names <- names(lsl_params)[grep(paste(subset, collapse = "|"), names(lsl_params))]
    lsl_params <- lsl_params[, col_names, drop = FALSE]
  }
  
  # Chomp off unrelated rows and sort alphabetically (do this regardless of whether there's a subset)
  col_names <- names(lsl_params)[grep(paste(segnames, collapse = "|"), names(lsl_params))]
  col_names <- sort(col_names)
  lsl_params <- lsl_params[, col_names, drop = FALSE]
  
  params[["lslr"]] <- as.matrix(lsl_params)
  
  return(params)
}


# """
#     prepssp!(ssp, ssp_simplified, params, rgnnames, segnames, popinput)
# 
# Read in SSP from file and filter to desired set of segments, note that this modifies
# the input parameter dictionary `params`.  The arguments are as fullows:
# 
# - ssp - name of lslr file to use; location relative to data/input-data directory
# - ssp_simplified - list of segments you want
# - params - parameter dictionary you want to add lslr to
# - rgnnames - names of regions (ciam_country)
# - segnames - names of segments
# - popinput - population density data set (0=original CIAM, 1=Jones and O'Neill 2016 (not supported), 2=Merkens et al 2016 (not supported))
# """

# 2=Merkens et al 2016 (not supported))
prepssp <- function(ssp, ssp_simplified, params, rgnnames, segnames, popinput) {
  
  data_dir <- file.path(dirname(rappdirs::user_data_dir()), "pkgname", "data", "ssp")
  
  # read and set population densities for Jones and Merkens data sets whether or not
  # we are using  them, so they have some defaults.
  # popinput=0 is the only supported option at this time
  if (popinput != 0) {
    stop("The `popinput` argument values of 1 and 2 are not supported at this time. In the future they will indicate use of Jones and O'Neill 2016 or Merkens et al 2016 population data, respectively.")
  }
  
  if (ssp == FALSE) { # Do nothing, base ssp data already loaded
    return(params)
  } else {
    pop <- read.csv(file.path(data_dir, paste0("pop_",ssp,".csv")), header = TRUE)
    ypc <- read.csv(file.path(data_dir, paste0("ypcc_",ssp,".csv")), header = TRUE)
    
    col_names <- names(pop)[stringi::stri_detect_fixed(names(pop), rgnnames)]
    col_names <- sort(col_names)
    pop <- pop[, col_names]
    ypc <- ypc[, col_names]
    
    params[["pop"]]  <- as.matrix(pop, ncol = length(col_names))
    params[["ypcc"]] <- as.matrix(ypc, ncol = length(col_names))
  }
  
  return(params)
}


# 
# """
#     parse_ciam_params!(params, rgn_order, seg_order, surgeoption)
# 
# Process CIAM data from csv to usable format and store outputs in params, note that
# this modifies the input parameter dictionary `params`, and the funciton is
# specific to CIAM data so there are some hard-coded names and assumptions. The
# arguments are as fullows:
# 
# - rgn_order - alphabetized lists of regions used (ciam_country)
# - seg_order - alphabetized lists of segments used
# - surgeoption - which surge exposure data set to be used
# """

parse_ciam_params <- function(params, rgn_order, seg_order, surgeoption) {
  
  # we need to grab the original keys so it doesn't try to recurse when we
  # make new entries into the dictionary
  original_keys <- names(params)
  
  for (k in original_keys) {
    p <- params[[k]] # Data frame
    # data key case
    if (k == "data") {
      colnames <- names(p)
      colnames <- colnames[!is.na(colnames)] # Preserve column names
      
      # Filter segments to subset
      segs <- as.character(as.matrix(p[,1]))
      seg_inds <- which(segs %in% seg_order)
      p <- p[seg_inds,]
      
      # Sort alphabetically
      seg_alpha <- order(as.character(as.matrix(p[,1])))
      p <- p[seg_alpha,]
      
      # Process all variables
      if (length(seg_inds) >= 1) {
        for (i in 2:(length(colnames) + 1)) {
          varname <- colnames[i-1]
          newvars <- p[,i-1]
          newvars <- as.numeric(as.matrix(newvars))
          params[[varname]] <- newvars
        }
        params[["data"]] <- NULL
      } else {
        stop("Segments in dictionary do not match supplied segments")
      }
      
      # globalparams key case
    } else if (k == "globalparams") {
      
      for (i in 1:nrow(p)) {
        varname <- p[i, 1]
        newval <- p[i, 2]
        
        if (varname == "ntsteps" || varname == "adaptPers") {
          newval <- as.integer(as.matrix(newval))
        } else if (tolower(newval) == "true") {
          newval <- TRUE
        } else if (tolower(newval) == "false") {
          newval <- FALSE
        } else {
          newval <- as.numeric(as.matrix(newval))
        }
        params[[as.character(varname)]] <- newval
      }
      params[["globalparams"]] <- NULL
      
      # surge exposure key case
    } else if (grepl("surgeexposure", k)) { # generalize for multiple possible surge data sets
      
      p <- subset(p, segments %in% seg_order)
      
      # Sort alphabetically
      p <- p[order(p[, "segments"]),]
      params[[k]] <- as.matrix(p[,2:6], ncol = 5)
      
      # refa key case
    } else if (k == "refa_h" || k == "refa_r") {
      p <- p %>% filter(segments %in% seg_order) %>% arrange(segments)
      params[[k]] <- NULL
      
      if (k == "refa_h") {
        params[["refA_H"]] <- as.numeric(p[["value"]])
      } else {
        params[["refA_R"]] <- as.numeric(p[["value"]])
      }
    }
    
    # Country data matrices parameter case
    else if (ncol(p) == 2) {
      r_inds <- which(as.matrix(p[,1]) %in% rgn_order)
      p <- p[r_inds,]
      p <- p[order(as.matrix(p[,1])),]
      
      if (!identical(as.character(as.matrix(p[,1])), rgn_order)) {
        stop(paste("Regions in dictionary do not match supplied regions, ", k))
      } else {
        newvals <- as.numeric(as.matrix(p[,2]))
        params[[k]] <- newvals
      }
    }
    
    # Time-country data matrices parameter case
    else if (ncol(p) > 3) {
      col_names <- names(p)[which(names(p) %in% rgn_order)]
      p <- p[, col_names, drop=FALSE]
      params[[k]] <- as.matrix(p)
    }
    
    # Single dimension parameter case
    else if (ncol(p) == 1) {
      params[[k]] <- as.numeric(as.matrix(p[,1]))
    }
    
    # set p["surgeexposure"] to the one designated by the surgeoption argument
    # by default the original data file is read in as p["surgeexposure"], so in
    # that case no action needed. Otherwise...
    if (surgeoption == 1) {
      params[["surgeexposure"]] <- params[["surgeexposure_dc-gtsr"]]
    } else if (surgeoption == 2) {
      params[["surgeexposure"]] <- params[["surgeexposure_gtsr"]]
    } else if (surgeoption != 0) {
      stop("The `surgeoption` argument must be 0, 1 or 2.")
    }
      
  }
  return(params)
}


# """
#     filter_index(v1, v2)
# 
# Filter a vector (v1) by a second vector (v2) and return indices of contained elements
# """

filter_index <- function(v1, v2) {
  out <- vector(mode = "list", length = 0)
  for (i in seq_along(v1)) {
    if (v1[i] %in% v2) {
      out <- c(out, i)
    }
  }
  return(out)
}

# """
# Process the segment-country mapping file (xsc) in CIAM by (1) Reads from CSV
# and outputs list of dictionaries and arrays (2) Filters xsc file to desired
# segments/regions
# """

prepxsc <- function(subset) {
  data_dir <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "..", "data", "input")
  xscfile <- "xsc.csv"
  xsc_name <- gsub("\\.csv$", "", xscfile)
  
  # Read in csv and convert to dictionary format
  xsc_params <- list(lowercase(read.csv(file.path(data_dir, xscfile))))
  xsc_char <- setNames(data.frame(xsc_params[[1]]$x2, xsc_params[[1]]$x3, xsc_params[[1]]$x4), xsc_params[[1]]$x1)
  
  # If only a subset of segments is used, filter down to relevant segments
  if (subset != FALSE) {
    xsc_char <- xsc_char[grepl(paste(subset, collapse = "|"), rownames(xsc_char)), , drop = FALSE]
  }
  
  # Create region and segment indices
  rgns <- sort(unique(xsc_char[, 1]))
  segs <- sort(unique(rownames(xsc_char)))
  
  xsc_ind <- list()    # numeric seg -> (numeric rgn, greenland bool)
  xsc_segmap <- list() # Numeric seg/rgn -> char seg/rgn
  xsc_rgnmap <- list()
  
  for (i in seq_along(segs)) {
    r <- xsc_char[segs[i], 1]   # Region character
    grn <- xsc_char[segs[i], 2] # 0 = non-Greenland, 1 = greenland bool
    isl <- xsc_char[segs[i], 3] # 0 = non-island, 1 = island bool
    r_ind <- match(r, rgns)     # Region index
    
    new_val <- list(r_ind, grn, isl) # New tuple w/ region index instead of character
    
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
#     findind(val, vec)
# Look up index corresponding to name with arguments `vec`, a vector of region or
# segment names (strings) and `val`, a string corresponding to value in 'vec'
# """
# Function to find the index of a value in a vector
findind <- function(val, vec) {
  name_ind <- which(vec == val)[1]
  return(name_ind)
}

# Function to load a subset of data
load_subset <- function(subset = FALSE) {
  dir <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "data", "subsets")
  if (subset != FALSE) {
    subs <- readLines(file.path(dir, subset))
    return(subs)
  } else {
    return(FALSE)
  }
}

# Function to initialize variables
init <- function(f = NULL) {
  if (is.null(f)) {
    f <- file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "data", "batch", "init.csv")
  }
  varnames <- read.csv(f, header = TRUE)
  vardict <- as.list(setNames(varnames, names(varnames)))
  return(vardict)
}


# """
#     import_model_data(lslfile, sub, ssp, ssp_simplified, popinput)
# 
# Wrapper for importing model data with the arguments:
# - lslfile - filename for lsl (string)
# - sub - filename with names of segments to use (string) or false (bool) to run all segments
# - ssp - SSP scenario + modeling group (specific long name)
# - ssp_simplified  - SSP scenario (1-5)
# - popinput - population density data set to be used (0=original CIAM, 1=Jones and O'Neill 2016 (not supported), 2=Merkens et al 2016 (not supported))
# - surgeoption - surge exposure data set to use (0=original CIAM/DINAS-COAST, 1=D-C corrected by GTSR/D-C bias, 2=GTSR nearest data point(s))
# """

import_model_data <- function(lslfile, sub, ssp, ssp_simplified, popinput, surgeoption) {
  subset <- if (sub == "false") FALSE else load_subset(sub)
  
  # Process main and lsl params
  params <- load_ciam_params()
  
  # Process XSC (segment-country mapping dictionary)
  xsc <- prepxsc(subset)
  
  # Process params using xsc and format lsl file
  parse_ciam_params(params, xsc[[2]], xsc[[3]], surgeoption)
  preplsl(lslfile, subset, params, xsc[[3]])
  prepssp(ssp, ssp_simplified, params, xsc[[2]], xsc[[3]], popinput)
  
  return(list(params = params, xsc = xsc))
}

load_meta <- function() {
  metadir <- file.path(dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path))), "data", "meta")
  
  # Read header and mappings
  header <- readLines(file.path(metadir, "header.txt"))
  
  varnames <- readLines(file.path(metadir, "variablenames.csv"))
  vardict <- setNames(as.data.frame(do.call(rbind, strsplit(varnames, ","))), c("var", "desc", "unit"))
  vardict <- as.list(vardict[, c("desc", "unit")])
  
  protect <- readLines(file.path(metadir, "protectlevels.csv"))
  protectdict <- setNames(as.data.frame(do.call(rbind, strsplit(protect, ","))), c("level", "desc"))$desc
  names(protectdict) <- as.integer(setNames(as.data.frame(do.call(rbind, strsplit(protect, ","))), c("level", "desc"))$level)
  
  retreat <- readLines(file.path(metadir, "retreatlevels.csv"))
  retreatdict <- setNames(as.data.frame(do.call(rbind, strsplit(retreat, ","))), c("level", "desc"))$desc
  names(retreatdict) <- as.integer(setNames(as.data.frame(do.call(rbind, strsplit(retreat, ","))), c("level", "desc"))$level)
  
  return(list(header, vardict, protectdict, retreatdict))
}




## ---------------------------------
## 2. Write Out Helper Functions
## ---------------------------------
# 
# """
#     write_ciam(m; runname::String = "base", sumsegs::String = "seg", varnames::Bool = false, tag::Bool = false)
# 
# Write out model results to CSV file using arguments:
# 
# - model - output from get_model()
# - runname
# - sumsegs - whether to sum across all segments, to region level, or no sums
# - varnames - if not false, write the passed variable names; if false get defaults from file
# - tag
# To do: possibly modify to work with DataVoyager()
# """

write_ciam <- function(model, outputdir = file.path(dirname(sys.frame(1)$ofile), "..", "output"), 
                       runname = "base", sumsegs = "seg", varnames = FALSE, tag = FALSE) {
  
  meta_output <- load_meta()
  rcp <- model$slrcost$rcp
  pctl <- model$slrcost$percentile
  ssp <- model$slrcost$ssp
  fixed <- ifelse(model$slrcost$fixed, "fixed", "flex")
  rcp_str <- paste0(rcp, "p", pctl, "ssp", ssp, fixed)
  
  xsc <- load_xsc()
  segmap <- load_segmap()
  segRgnDict <- setNames(xsc$rgn, xsc$seg)
  
  if (!varnames) {
    varnames <- names(meta_output[[2]])
  }
  
  vargroup1 <- c()
  vargroup2 <- c()
  
  for (v in varnames) {
    if (length(dim(model$slrcost[v])) > 2) {
      vargroup2 <- c(vargroup2, as.symbol(v))
    } else {
      vargroup1 <- c(vargroup1, as.symbol(v))
    }
  }
  
  # Assign 2D variables to dataframe
  # 2 cases: 1. adapt pers is first; 2. adapt pers is second
  common_order <- c("time", "ciam_country", "segments", "level")
  
  for (i in seq_along(vargroup1)) {
    temp <- getdataframe(model, "slrcost", vargroup1[i])
    missing_names <- common_order[!common_order %in% colnames(temp)]
    
    if (length(missing_names) >= 1) {
      for (name in missing_names) {
        temp[, name] <- rep(NA, nrow(temp))
      }
    }
    
    if ("ciam_country" %in% missing_names && !"segments" %in% missing_names) {
      temp <- merge(temp, data.frame(segments = xsc$seg, ciam_country = segRgnDict[xsc$seg]), by = "segments")
    }
    
    temp$variable <- as.character(vargroup1[i])
    names(temp)[names(temp) == as.character(vargroup1[i])] <- "value"
    temp <- temp[, c("time", "ciam_country", "segments", "level", "variable", "value")]
    
    if (i == 1) {
      df <<- temp
    } else {
      df <<- rbind(df, temp)
    }
  }
  
  # Assign 3D variables to second data frame and join
  ntime <- model$slrcost$ntsteps
  segID <- model$slrcost$segID
  colnames <- segID_to_seg(as.integer(segID), segmap)
  
  for (j in 1:length(vargroup2)) {
    
    ndim1 <- dim(model$slrcost$vargroup2[[j]])[3]
    
    for (k in 1:ndim1) {
      
      temp <- as.data.frame(model$slrcost$vargroup2[[j]][,,k])
      colnames(temp) <- colnames
      temp$time <- 1:ntime
      
      if (as.character(vargroup2[j]) == "Construct" || grepl("Protect", as.character(vargroup2[j]))) {
        dim1 <- k + 1
        adapt <- model$slrcost$adaptoptions[dim1]
      } else {
        dim1 <- k
        adapt <- model$slrcost$adaptoptions[dim1]
      }
      
      temp$level <- rep(adapt, ntime)
      temp <- reshape2::melt(temp, id.vars = c("time", "level"), variable.name = "segments")
      names(temp)[names(temp) == "value"] <- "variable"
      
      temp$segments <- as.character(temp$segments)
      temp$variable <- rep(as.character(vargroup2[j]), nrow(temp))
      
      temp$ciam_country <- segRgnDict[temp$segments]
      temp <- temp[,c("time", "ciam_country", "segments", "level", "variable", "variable")]
      
      if (j == 1 && k == 1) {
        df2 <<- temp
      } else {
        df2 <- rbind(df2, temp)
      }
    }
  }
  # Sum to either region-level, global-level, or leave as seg-level
  outdf <- rbind(df, df2)
  outfile <- if (tag) {paste0(runname, "_", sumsegs, "_", rcp_str, "_", tag, ".csv")} else {paste0(runname, "_", sumsegs, "_", rcp_str, ".csv")}
  
  if (sumsegs == "rgn") {
    rgndf <- outdf %>% dplyr::group_by(time, ciam_country, level, variable) %>% dplyr::summarise(value = sum(value))
    write.csv(rgndf, file.path(outputdir, outfile), row.names = FALSE)
  } else if (sumsegs == "seg") {
    write.csv(outdf, file.path(outputdir, outfile), row.names = FALSE)
  } else if (sumsegs == "global") {
    globdf <- outdf %>% dplyr::group_by(time, level, variable) %>% dplyr::summarise(value = sum(value))
    write.csv(globdf, file.path(outputdir, outfile), row.names = FALSE)
  }
}

end

# """
#     write_optimal_costs(model; outputdir::String = joinpath(@__DIR__,"..","output"), runname="base")
# 
# Streamline writing results for optimal adaptation costs.
# """

write_optimal_costs <- function(model, outputdir = file.path(dirname(rstudioapi::getSourceEditorContext()$path),"..","output"), runname = "base") {
  
  # Output: Data Frame with segment,region,time,level,option, suboption
  #   E.g. 'OptimalProtect', 'Construct'
  # Should output 2 CSVs: 1 with just the 3 main categories, 2nd with
  #   detailed subcategories
  
  rcp     <- model[["slrcost","rcp"]]
  pctl    <- model[["slrcost","percentile"]]
  ssp     <- model[["slrcost","ssp"]]
  fixed   <- ifelse(model[["slrcost", "fixed"]], "fixed", "flex")
  rcp_str <- paste0(rcp, "p", pctl, "ssp", ssp, fixed)
  xsc     <- load_xsc()
  segRgnDict <- setNames(xsc[,"rgn"], xsc[,"seg"])
  
  # 1. Create aggregate adaptation decision DF
  temp1 <- getdataframe(model, slrcost = "OptimalCost")
  temp1$ciam_country <- segRgnDict[temp1$segments]
  
  temp2 <- getdataframe(model, slrcost = "OptimalLevel")
  temp3 <- getdataframe(model, slrcost = "OptimalOption")
  
  # Join dataframes and reorganize
  # Inner join to restrict to only rows in both dataframes
  # (which should be all of time, because all segments should have values at all time steps)
  out <- merge(temp1, temp2, by = c("time", "segments"))
  out <- merge(out, temp3, by = c("time", "segments"))
  
  # Replace OptimalOption numeric value with string
  lookup <- list("RetreatCost" =  -2.0, "ProtectCost" = -1.0 , "NoAdaptCost" = -3.0 )
  out$variable <- lookup[as.character(out$OptimalOption)]
  names(out)[which(names(out) == "OptimalLevel")] <- "level"
  out <- out[,c("time", "ciam_country", "segments", "variable", "level", "OptimalCost")]
  
  # Write to file
  outfile <- paste0(runname, "_seg_", rcp_str, "_optimal.csv")
  write.csv(out, file.path(outputdir, outfile), row.names = FALSE)
  
  # Write Sub-Costs
  vars <- c("OptimalStormCapital", "OptimalStormPop", "OptimalConstruct",
            "OptimalFlood", "OptimalRelocate", "OptimalWetland")
  
  for (i in 1:length(vars)) {
    
    temp <- getdataframe(model, slrcost = vars[i])
    temp <- merge(temp, data.frame(ciam_country = segRgnDict[temp$segments]), by = "segments")
    
    temp$variable <- rep(vars[i], nrow(temp))
    
    temp2 <- getdataframe(model, slrcost = "OptimalLevel")
    temp3 <- getdataframe(model, slrcost = "OptimalOption")
    
    # Join dataframes and reorganize
    out <- merge(temp, temp2, by = c("time", "segments"))
    out <- merge(out, temp3, by = c("time", "segments"))
    
    # Replace OptimalOption numeric value with string
    lookup <- c("-2.0" = "RetreatCost", "-1.0" = "ProtectCost", "-3.0" = "NoAdaptCost")
    out$AdaptCategory <- lookup[as.character(out$OptimalOption)]
    
    names(out)[names(out) == "OptimalLevel"] <- "level"
    names(out)[names(out) == vars[i]] <- "value"
    
    out <- out[, c("time", "ciam_country", "segments", "AdaptCategory", "variable", "level", "value")]
    
    if (i == 1) {
      df <- out
    } else {
      df <- rbind(df, out)
    }
  }
}
# 
# write_optimal_protect_retreat <- function(model, runname="base") {
#   outputdir <- file.path(dirname(dirname(dirname(rprojroot::find_rstudio_root_file()))), "output")
#   rcp <- get(model, [:slrcost, :rcp])
#   pctl <- get(model, [:slrcost, :percentile])
#   ssp <- get(model, [:slrcost, :ssp])
#   fixed <- ifelse(get(model, [:slrcost, :fixed]), "fixed", "flex")
#   rcp_str <- paste0(rcp, "p", pctl, "ssp", ssp, fixed)
#   xsc <- load_xsc()
#   
#   # 1. Create aggregate adaptation decision DF
#   pl <- getdataframe(model, slrcost = :OptimalProtectLevel)
#   rl <- getdataframe(model, slrcost = :OptimalRetreatLevel)
#   out <- inner_join(pl, rl, by = c("time", "segments"))
#   
#   # Read in protect, retreat variables and filter to optimal levels; add to out
#   protect <- get(model, [:slrcost, :ProtectCost])
#   retreat <- get(model, [:slrcost, :RetreatCost])
#   for (i in 1:5) {
#     if (i > 1) {
#       prot <- as.data.frame(get(model, [:slrcost, :ProtectCost])[,,i-1])
#       ret <- as.data.frame(get(model, [:slrcost, :RetreatCost])[,,i])
#     } else {
#       ret <- as.data.frame(get(model, [:slrcost, :RetreatCost])[,,i])
#     }
#   }
#   
#   outfile <- paste0(runname, "_seg_", rcp_str, "_ProtectRetreat.csv")
#   write.csv(out, file.path(outputdir, outfile), row.names = FALSE)
# }


## ---------------------------------
## 3. Basic Functions for Segment-Region Lookup
## ---------------------------------

load_segmap <- function() {
  segmap <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "data", "meta", "segIDmap.csv")) %>%
    as.data.frame()
  segmap$segID <- as.integer(segmap$segID)
  return(segmap)
}

load_xsc <- function() {
  xsc <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "..", "data", "input", "xsc.csv")) %>%
    as.data.frame()
  return(xsc)
}

# """
#     segID_to_seg(segID, segmap)
# 
# Look up string name of segment from segID and return only first result for each
# ID entry as an array. The arguments are as follows:
# 
# - segID - int or array of ints
# - segmap - output of load_rgnmap or load_segmap() (DataFrame)
# """

# segID_to_seg function
segID_to_seg <- function(segID, segmap) {
  seg <- sapply(segID, function(i) {
    segmap$seg[segmap$segID == i][1]
  })
  return(seg)
}

# segStr_to_segID function
segStr_to_segID <- function(segstr) {
  ids <- sapply(segstr, function(i) {
    as.numeric(gsub("[^0-9]", "", i))
  })
  return(ids)
}

# 
# """
#     getTimeSeries(model, ensnum; segIDs = false, rgns = false, sumsegs = "global")
# 
# Return time series of costs at global, regional and/or segment level scale and also
# compute cost as percent of regional or global gdp.
# """
getTimeSeries <- function(model, ensnum, segIDs = FALSE, rgns = FALSE, sumsegs = "global") {
  
  # If not using segment-level aggregation, segIDs refers to
  # individual segments to report in addition to global/regional
  
  if (sumsegs == "seg") { # Report all segments in model or those specified
    if (segIDs == FALSE) {
      segIDs <- model$slrcost$segID
    }
  }
  
  xsc <- MimiCIAM::load_xsc()
  segRgnDict <- setNames(as.list(xsc[, c("rgn", "segID")]), xsc$seg)
  
  # Write Main and Sub-Costs
  vars <- c("OptimalCost", "OptimalStormCapital", "OptimalStormPop", "OptimalConstruct",
            "OptimalFlood", "OptimalRelocate", "OptimalWetland")
  df <- data.frame()
  
  for (i in 1:length(vars)) {
    
    temp <- MimiCIAM::getdataframe(model, slrcost = vars[i])
    temp <- merge(temp, data.frame(ciam_country = sapply(temp$segments, function(x) segRgnDict[[x]][1]),
                                   segID = sapply(temp$segments, function(x) segRgnDict[[x]][2])),
                  by = c("time", "segments"))
    
    temp2 <- MimiCIAM::getdataframe(model, slrcost = "OptimalLevel")
    temp3 <- MimiCIAM::getdataframe(model, slrcost = "OptimalOption")
    temp4 <- MimiCIAM::getdataframe(model, slrcost = "ypcc")
    temp5 <- MimiCIAM::getdataframe(model, slrcost = "pop")
    
    # Join dataframes and reorganize
    out <- merge(temp, temp2, by = c("time", "segments"))
    out <- merge(out, temp3, by = c("time", "segments"))
    out <- merge(out, temp4, by.x = c("time", "ciam_country"), by.y = c("time", "segments"))
    out <- merge(out, temp5, by.x = c("time", "ciam_country"), by.y = c("time", "segments"))
    
    # Replace OptimalOption numeric value with string
    lookup <- list("-2" = "Retreat", "-1" = "Protection", "-3" = "No Adaptation")
    out$category <- lookup[as.character(out$OptimalOption)]
    colnames(out)[colnames(out) == "OptimalLevel"] <- "level"
    colnames(out)[colnames(out) == vars[i]] <- "cost"
    
    out$ens <- ensnum
    col_order <- c("ens", "time", "ciam_country", "segments", "segID", "category", "level", "cost", "ypcc", "pop")
    out <- out[, col_order]
    
    # Aggregate to geographic level
    subset <- out[out$segID %in% segIDs, ]
    
    if (sumsegs == "rgn") {
      rgndf <- out %>%
        group_by(ens, time, ciam_country, level, category, ypcc, pop) %>%
        summarise(cost = sum(cost)) %>%
        ungroup() %>%
        mutate(segID = 0, segments = "regional") %>%
        select(col_order) %>%
        bind_rows(subset)
      
      rgndf$gdp <- rgndf$ypcc * rgndf$pop / 1e3  # GDP is in $Billion (this will be regional for subset too)
      rgndf$pct_gdp <- rgndf$cost / rgndf$gdp  # Annual cost / annual GDP
      out <- rgndf
    } else if (sumsegs == "global") {
      globdf <- out %>%
        group_by(ens, time, level, category) %>%
        summarise(cost = sum(cost)) %>%
        ungroup() %>%
        left_join(temp4, temp5, by = c("time", "ciam_country")) %>%
        mutate(gdp = ypcc * pop / 1e3, ens = ensnum) %>%
        group_by(ens, time) %>%
        summarise(ypcc = sum(gdp), pop = sum(pop), gdp = sum(gdp)) %>%
        ungroup()
      
      globdf <- globdf %>%
        left_join(globdf %>% summarise(gdp = sum(gdp)) %>% mutate(ypcc = (gdp * 1e9) / (sum(pop) * 1e6)), by = "ypcc") %>%
        mutate(segID = 0, ciam_country = "global", segments = "global") %>%
        select(ens, time, ciam_country, segments, segID, category, level, cost, ypcc, pop, gdp)
      
      subset$gdp <- subset$ypcc * subset$pop / 1e3
      globdf <- globdf %>% bind_rows(subset)
      
      globdf$pct_gdp <- globdf$cost / globdf$gdp
      
      if (!is.null(rgns)) {
        rgndf <- out[out$ciam_country %in% rgns, ] %>%
          group_by(ens, time, ciam_country, level, category, ypcc, pop) %>%
          summarise(cost = sum(cost)) %>%
          ungroup() %>%
          mutate(segID = 0, segments = "regional") %>%
          select(col_order)
        
        rgndf$gdp <- rgndf$ypcc * rgndf$pop / 1e3  # GDP is in $Billion (this will be regional for subset too)
        rgndf$pct_gdp <- rgndf$cost / rgndf$gdp  # Annual cost / annual GDP
        
        globdf <- globdf %>% bind_rows(rgndf)
      }
      
      out <- globdf
    } else {
      out$gdp <- out$ypcc * out$pop / 1e3  # Regional gdp by segment
      out$pct_gdp <- out$cost / out$gdp  # Segment cost or subcost as % of regional gdp
    }
    
    if (i == 1) {
      df <- out
    } else {
      df <- bind_rows(df, out)
    }
    
    # Remove ypcc
    # Remove ypcc and pop from final df
    df <- df[, c("ens", "time", "ciam_country", "segments", "segID", "category", "level", "cost", "gdp", "pct_gdp")]
    return(df)
  }
}


## ---------------------------------
## 4. In Progress Helper Functions
## ---------------------------------

# """
#     writelog()
# 
# IN PROGRESS - Create a writelog function to go with a wrapper for the run function
# automatically produce a logfile
# """

writelog <- function() {
  dir <- file.path(dirname(getwd()), "data", "batch", "logs")
  d <- init()
  run <- d[["run_name"]]
  date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  file.copy("../data/batch/init.csv", file.path(dir, paste0(run, "_", date, ".csv")))
}


## ---------------------------------
## 5. Write-Out Helper Functions
## ---------------------------------
# 
# """
#     write_init_file(run_name::String, outputdir::String, init_settings::Dict)
# 
# Write the init.csv file for a specificied `run_name` into `outputdir` using init_settings
# found in `init_settings`.
# """

write_init_file <- function(run_name, outputdir, init_settings) {
  textheader <- "run_name,lslr,subset,ssp,ssp_simplified\n"
  textstr <- paste(run_name, ",", init_settings[["lslrfile"]], ",", init_settings[["subset"]], ",", init_settings[["ssp"]], ",", init_settings[["ssp_simplified"]], sep = "")
  txtfile <- file.path(outputdir, init_settings[["init_filename"]])
  writeLines(con = txtfile, text = c(textheader, textstr))
}

write_output_files <- function(m, outputdir, run_name) {
  # write out the results
  cat("Writing out ciam 'subsegs = seg' file for run", run_name, "to directory", outputdir, "\n")
  MimiCIAM::write_ciam(m, outputdir = outputdir, runname = run_name, sumsegs = "seg", varnames = FALSE)
  cat("Writing out ciam 'subsegs = global' file for run", run_name, "to directory", outputdir, "\n")
  MimiCIAM::write_ciam(m, outputdir = outputdir, runname = run_name, sumsegs = "global", varnames = FALSE)
  cat("Writing out optimal costs file for run", run_name, "to directory", outputdir, "\n")
  MimiCIAM::write_optimal_costs(m, outputdir = outputdir, runname = run_name)
}


## ---------------------------------
## 6. SLRCost Component Helper Functions
## ---------------------------------

# Growth rate helper function
growthrate <- function(x1, x2) {
  epsilon <- 1e-9
  return((x2 / (x1 + epsilon) - 1))
}

# Calculate Coast Area function
calcCoastArea <- function(areaparams, var) {
  area <- (areaparams[1]*max(0,min(0.5,var-0))
           +(areaparams[1]+areaparams[2])/2*max(0,min(1,var-0.5))
           +areaparams[2]*max(0,min(0.5,var-1.5))
           +areaparams[3]*max(0,min(1,var-2))
           +areaparams[4]*max(0,min(1,var-3))
           +areaparams[5]*max(0,min(1,var-4))
           +areaparams[6]*max(0,min(1,var-5))
           +areaparams[7]*max(0,min(1,var-6))
           +areaparams[8]*max(0,min(1,var-7))
           +areaparams[9]*max(0,min(1,var-8))
           +areaparams[10]*max(0,min(1,var-9))
           +areaparams[11]*max(0,min(1,var-10))
           +areaparams[12]*max(0,min(1,var-11))
           +areaparams[13]*max(0,min(1,var-12))
           +areaparams[14]*max(0,min(1,var-13))
           +areaparams[15]*max(0,var-14))
  return(area)
}

# Local rate helper function
localrate <- function(lslr1, lslr2, tstep) {
  return(max(0, (lslr2 - lslr1))/tstep)
}

# Get segments function
getsegments <- function(rgn_name, xsc) {
  segs <- names(Filter(function(v) v[1] == rgn_name, xsc))
  return(segs)
}

getregion <- function(seg_ind, xsc) {
  rgn <- xsc[[seg_ind]][[1]]
  return(rgn)
}

isgreenland <- function(seg_ind, xsc) {
  greenland <- xsc[[seg_ind]][[2]]
  return(greenland)
}

isisland <- function(seg_ind, xsc) {
  island <- xsc[[seg_ind]][[3]]
  return(island)
}


calcHorR <- function(option, level, lslrPlan, surgeExpLevels, adaptOptions) {
  ind <- which(level == adaptOptions)
  if (option == -1 && level == 10) {
    # Protect height differs from retreat radius only in case of 10 yr surge exposure
    H <- max(0, lslrPlan + surgeExpLevels[ind] / 2)
    return(H)
  } else if (level == 0) { # Maintain existing defenses
    H_R <- 0
    return(H_R)
  } else {
    H_R <- max(0, lslrPlan + surgeExpLevels[ind])
    return(H_R)
  }
}

pos <- function(x) {
  return(max(0, x))
}

