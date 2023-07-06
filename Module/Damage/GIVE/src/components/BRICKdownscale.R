##==============================================================================
## Supporting Functions to Downscale BRICK from GMSL to LSL
# 
# """
#     get_fingerprints()
# Retrieve BRICK fingerprints from NetCDF file - will download the file to a
# folder `data` directory
# """

library(ncdf4)

get_fingerprints <- function() {
  fp_dir <- file.path(paste("..", "data", "brick_model_data",sep="/"))
  if (!dir.exists(fp_dir)) dir.create(fp_dir, recursive = TRUE)
  fp_file <- file.path(fp_dir, "FINGERPRINTS_SLANGEN_Bakker.nc")
  if (!file.exists(fp_file)) {
    url <- "https://github.com/scrim-network/BRICK/raw/master/fingerprints/FINGERPRINTS_SLANGEN_Bakker.nc"
    download.file(url, fp_file)
  }
  fp_target <<- nc_open(fp_file)
  fplat <<- ncvar_get(fp_target, "lat")
  fplon <<- ncvar_get(fp_target, "lon")
  fpAIS <<- ncvar_get(fp_target, "AIS")
  fpGSIC <<- ncvar_get(fp_target, "GLAC")
  fpGIS <<- ncvar_get(fp_target, "GIS")
  nc_close(fp_target)
  
  return(list(fplat = fplat, fplon = fplon, fpAIS = fpAIS, fpGSIC = fpGSIC, fpGIS = fpGIS))
}

# Small Helper Functions for dealing with sea level fingerprints near land

#' Increment latitude by `inc` in either positive direction (`direction="increase"`)
#' or in the negative direction (`direction="decrease"`).
#' Assumes latitude runs from -90 to 90 (deg N).
#'
#' @param lat Latitude (deg N)
#' @param inc Increment (in degrees)
#' @param direction Direction to increment (either "increase" or "decrease")
#' @return New latitude value
next_lat <- function(lat, inc, direction) {
  if (direction == "increase") {
    new_lat <- lat + inc
    if (new_lat > 90) new_lat <- 90
  } else if (direction == "decrease") {
    new_lat <- lat - inc
    if (new_lat < -90) new_lat <- -90
  } else {
    stop("Invalid direction argument. Must be 'increase' or 'decrease'.")
  }
  return(new_lat)
}

# 
# """
#     next_lon(lon::Float64, inc::Int, direction::Symbol)
# Increment longitude by `inc` in either positive direction
# (`direction=:increase`) or in the negative direction (`direction=:decrease`).
# Assumes longitude runs from 0 to 360 (deg E).
# """

next_lon <- function(lon, inc, direction) {
  if (lon < 0 || lon > 360) {
    stop("Longitude must be between 0 and 360")
  }
  
  if (direction == "increase") {
    new_lon <- lon + inc
    if (new_lon > 360) {
      new_lon <- new_lon - 360
    }
  } else if (direction == "decrease") {
    new_lon <- lon - inc
    if (new_lon < 0) {
      new_lon <- new_lon + 360
    }
  } else {
    stop("Direction must be either 'increase' or 'decrease'")
  }
  return(new_lon)
}



# ##==============================================================================
# """
#     downscale_brick(;lon, lat, results_dir, proj_or_hind, ensemble_or_map, model_config, rcp_scenario="RCP85")
# 
# Downscale BRICK projections to a single point, using either the whole ensemble
# or only the maximum a posteriori ensemble member. Note this function assumes a
# specific folder structure and file naming within the top level results_dir.
# 
# lon = longitude (degrees East) of location for downscaling
# lat = latitude (degrees North) of location for downscaling
# results_dir = the top level directory of results ie. "my_brick_results_20M_20-02-2022"
# proj_or_hind = "proj" for projections, or "hind" for hindcast
# ensemble_or_map = "ensemble" for entire posterior ensemble, or "map" for the maximum a posteriori ensemble member (single simulation)
# model_config = "brick", "doeclimbrick", or "sneasybrick"
# rcp_scenario = "RCP26", "RCP45", "RCP60", or "RCP85" (default). Doesn't matter for hindcast.
# """



downscale_brick <- function(lon, lat, results_dir, proj_or_hind, ensemble_or_map, model_config, rcp_scenario = "RCP85") {
  
  appen <- paste0(model_config, substr(results_dir, nchar("results_") + 9, nchar(results_dir)))
  
  if (proj_or_hind == "proj") {
    slr_dir <- file.path(paste("components/bricktest/test_data/results/", results_dir, "/projections_csv/", rcp_scenario,sep=""))
    MAP <- read.csv(file.path(slr_dir, paste0("projections_MAP_", rcp_scenario, "_", appen, ".csv")))
    years <- MAP$YEAR
    if (ensemble_or_map == "ensemble") {
      AIS <- read.csv(file.path(slr_dir, paste0("projections_antarctic_", rcp_scenario, "_", appen, ".csv")))
      GIS <- read.csv(file.path(slr_dir, paste0("projections_greenland_", rcp_scenario, "_", appen, ".csv")))
      GSIC <- read.csv(file.path(slr_dir, paste0("projections_glaciers_", rcp_scenario, "_", appen, ".csv")))
      TE <- read.csv(file.path(slr_dir, paste0("projections_thermal_", rcp_scenario, "_", appen, ".csv")))
      LWS <- read.csv(file.path(slr_dir, paste0("projections_landwater_storage_sl_", rcp_scenario, "_", appen, ".csv")))
      num_ens <- ncol(AIS)
    } else if (ensemble_or_map == "map") {
      AIS <- MAP$AIS
      GIS <- MAP$GIS
      GSIC <- MAP$GLAC
      TE <- MAP$TE
      LWS <- MAP$LWS
      num_ens <- 1
    }
  } else if (proj_or_hind == "hind") {
    slr_dir <- file.path(paste("components/bricktest/test_data/results/", results_dir, "/hindcast_csv",sep=""))
    MAP <- read.csv(file.path(slr_dir, paste0("hindcast_MAP_", appen, ".csv")))
    years <- MAP$YEAR
    if (ensemble_or_map == "ensemble") {
      AIS <- read.csv(file.path(slr_dir, paste0("hindcast_antarctic_", appen, ".csv")))
      GIS <- read.csv(file.path(slr_dir, paste0("hindcast_greenland_", appen, ".csv")))
      GSIC <- read.csv(file.path(slr_dir, paste0("hindcast_glaciers_", appen, ".csv")))
      TE <- read.csv(file.path(slr_dir, paste0("hindcast_thermal_", appen, ".csv")))
      LWS <- read.csv(file.path(slr_dir, paste0("hindcast_landwater_storage_sl_", appen, ".csv")))
      num_ens <- ncol(AIS)
    } else if (ensemble_or_map == "map") {
      AIS <- MAP$AIS
      GIS <- MAP$GIS
      GSIC <- MAP$GLAC
      TE <- MAP$TE
      LWS <- MAP$LWS
      num_ens <- 1
    }
  }
  
  num_years <- length(years)
  
  fp_data <- get_fingerprints()
  fplat <- fp_data$fplat
  fplon <- fp_data$fplon
  fpAIS <- fp_data$fpAIS
  fpGSIC <- fp_data$fpGSIC
  fpGIS <- fp_data$fpGIS
  
  # Convert Longitude to degrees East
  # CIAM Lat is already in (-90,90) by default
  if (lon < 0) {
    lon <- lon + 360
  }
  
  # Find fingerprint degrees nearest to lat,lon
  ilat <- which(abs(fplat - lat) == min(abs(fplat - lat)))
  ilon <- which(abs(fplon - lon) == min(abs(fplon - lon)))
  
  if(length(ilat)>1){
    ilat <- ilat[1]
  }
  if(length(ilon)>1){
    ilon <- ilon[1]
  }
  
  # Take average of closest lat/lon values
  fpAIS_flat <- na.omit(as.vector(unlist(fpAIS[ilon, ilat])))
  fpGSIC_flat <- na.omit(as.vector(unlist(fpGSIC[ilon, ilat])))
  fpGIS_flat <- na.omit(as.vector(unlist(fpGIS[ilon, ilat])))
  
  fpAIS_loc <- mean(fpAIS_flat, na.rm = TRUE)
  fpGSIC_loc <- mean(fpGSIC_flat, na.rm = TRUE)
  fpGIS_loc <- mean(fpGIS_flat, na.rm = TRUE)
  fpTE_loc <- 1.0
  fpLWS_loc <- 1.0
  
  # Keep searching nearby lat/lon values if fingerprint value is NaN unless limit is hit
  inc <- 1
  
  while (is.nan(fpAIS_loc) || is.nan(fpGIS_loc) || is.nan(fpGSIC_loc) && inc < 5) {
    newlonStart <- next_lon(fplon[ilon], inc, "decrease")[1]
    newlatStart <- next_lat(fplat[ilat], inc, "decrease")[1]
    newlonEnd <- next_lon(fplon[ilon], inc, "increase")[1]
    newlatEnd <- next_lat(fplat[ilat], inc, "increase")[1]
    
    latInd1 <- which(abs(fplat - newlatStart) == min(abs(fplat - newlatStart)))
    latInd2 <- which(abs(fplat - newlatEnd) == min(abs(fplat - newlatEnd)))
    
    lonInd1 <- which(abs(fplon - newlonStart) == min(abs(fplon - newlonStart)))
    lonInd2 <- which(abs(fplon - newlonEnd) == min(abs(fplon - newlonEnd)))
    
    if (latInd2 < latInd1) {
      latInds <- c(latInd1, 1:latInd2)
    } else {
      latInds <- latInd1:latInd2
    }
    
    if (lonInd2 < lonInd1) {
      lonInds <- c(lonInd1, 1:lonInd2)
    } else {
      lonInds <- lonInd1:lonInd2
    }
    
    fpAIS_flat <- na.omit(as.vector(unlist(fpAIS[lonInds, latInds])))
    fpGSIC_flat <- na.omit(as.vector(unlist(fpGSIC[lonInds, latInds])))
    fpGIS_flat <- na.omit(as.vector(unlist(fpGIS[lonInds, latInds])))
    
    fpAIS_loc <- mean(fpAIS_flat, na.rm = TRUE)
    fpGSIC_loc <- mean(fpGSIC_flat, na.rm = TRUE)
    fpGIS_loc <- mean(fpGIS_flat, na.rm = TRUE)
    
    inc <- inc + 1
  }

  # If still NaN, throw an error
  if (is.nan(fpAIS_loc) || is.nan(fpGIS_loc) || is.nan(fpGSIC_loc)) {
    cat(paste0("Error: no fingerprints found for (lon,lat) = (", lon, ",", lat, ")\n"))
    return(NULL)
  }

  # Prepare outputs for CSV
  outputs <- matrix(NA, nrow = num_years, ncol = num_ens+1)
  outputs[,1] <- years

  # Multiply fingerprints by BRICK ensemble members
  if (dim(as.matrix(AIS))[2] > 1) {
    lsl_out <- matrix(NA, nrow = dim(AIS)[1], ncol = dim(AIS)[2])
    for (n in 1:dim(AIS)[2]) { # loop through ensemble members
      lsl_out[,n] <- fpGIS_loc * GIS[,n] + fpAIS_loc * AIS[,n] + fpGSIC_loc * GSIC[,n] + fpTE_loc * TE[,n] + fpLWS_loc * LWS[,n]
    }
    outputs[,2:(num_ens+1)] <- lsl_out
  } else {
    lsl_out <- fpGIS_loc * GIS + fpAIS_loc * AIS + fpGSIC_loc * GSIC + fpTE_loc * TE + fpLWS_loc * LWS
    outputs[,2] <- lsl_out
  }

  # Write to CSV
  filepath_output <- file.path(slr_dir, "localslr")
  
  if(file.exists(filepath_output) ==T){
  }else{
  dir.create(filepath_output, recursive = TRUE)
  }

  lat_rounded <- round(lat, digits = 2)
  lon_rounded <- round(lon, digits = 2)
  filename_output <- file.path(filepath_output, paste0("projections_lsl-lat", lat_rounded, "-lon", lon_rounded, "_", model_config, ".csv"))
  write.csv(data.frame(outputs), file = filename_output, row.names = FALSE)

  return(list(years, lsl_out))

}


#####test the downscale function
lon=285.99
lat=40.71
model_config = "brick"
results_dir = "my_brick_results_1K_18-05-2022"
proj_or_hind = "proj"
ensemble_or_map = "map"
model_config = "brick"
rcp_scenario = "RCP85"
downscale_brick(lon, lat, results_dir, proj_or_hind, ensemble_or_map, model_config, rcp_scenario = "RCP85")





