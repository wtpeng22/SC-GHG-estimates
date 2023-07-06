# Retrieve BRICK fingerprints from NetCDF file

get_fingerprints <- function(fp_file = file.path(dirname(rstudioapi::getSourceEditorContext()$path), "../../data/CIAM/FINGERPRINTS_SLANGEN_Bakker.nc")){
  
  fplat <- ncvar_get(fp_file, "lat")
  fplon <- ncvar_get(fp_file, "lon")
  fpAIS <- ncvar_get(fp_file, "AIS")
  fpGSIC <- ncvar_get(fp_file, "GLAC")
  fpGIS <- ncvar_get(fp_file, "GIS")
  nc_close(fp_file)
  
  return(list(fplat, fplon, fpAIS, fpGSIC, fpGIS))
}

get_segment_fingerprints <- function(fp_file = file.path(dirname(getwd()), "data/CIAM/FINGERPRINTS_SLANGEN_Bakker.nc"),
                                     segIDs_file = file.path(dirname(getwd()), "data/CIAM/diva_segment_latlon.csv"),
                                     fp_segments_file = file.path(dirname(getwd()), "data/CIAM/segment_fingerprints.csv")) {
  
  # Load fingerprint data from FINGERPRINTS_SLANGEN_Bakker
  # The fplat and fplon are -90 to 90 and 0 to 360 respectively
  library(ncdf4)
  fp_data <- nc_open(fp_file)
  fplat <- ncvar_get(fp_data, "lat")
  fplon <- ncvar_get(fp_data, "lon")
  fpAIS <- ncvar_get(fp_data, "AIS")
  fpGSIC <- ncvar_get(fp_data, "GSIC")
  fpGIS <- ncvar_get(fp_data, "GIS")
  nc_close(fp_data)
  
  # Load segment data
  library(data.table)
  ciamlonlat <- fread(segIDs_file, data.table = FALSE) %>%
    setDT() %>%
    .[order(segments)]
  ciamlonlat[longi < 0, longi := longi + 360] # Convert Longitude to degrees East, CIAM Lat is already in (-90,90) by default
  
  # Initialize output dataframe
  df <- data.frame(segments = integer(),
                   segid = integer(),
                   lon = double(),
                   lat = double(),
                   rgn = integer(),
                   fpGIS_loc = double(),
                   fpAIS_loc = double(),
                   fpGSIC_loc = double(),
                   fpTE_loc = double(),
                   fpLWS_loc = double(),
                   stringsAsFactors = FALSE)
  
  for (i in 1:nrow(ciamlonlat)) {
    lon <- ciamlonlat$longi[i]
    lat <- ciamlonlat$lati[i]
    segid <- ciamlonlat$segid[i]
    segment <- ciamlonlat$segments[i]
    rgn <- ciamlonlat$rgn[i]
    
    # Find fingerprint degrees nearest to lat,lon
    ilat <- which(abs(fplat - lat) == min(abs(fplat - lat)))
    ilon <- which(abs(fplon - lon) == min(abs(fplon - lon)))
    
    # Take average of closest lat/lon values
    fpAIS_flat <- c(fpAIS[ilon, ilat])
    fpAIS_flat <- fpAIS_flat[!is.na(fpAIS_flat)]
    fpGSIC_flat <- c(fpGSIC[ilon, ilat])
    fpGSIC_flat <- fpGSIC_flat[!is.na(fpGSIC_flat)]
    fpGIS_flat <- c(fpGIS[ilon, ilat])
    fpGIS_flat <- fpGIS_flat[!is.na(fpGIS_flat)]
    
    fpAIS_loc <- mean(fpAIS_flat)
    fpGSIC_loc <- mean(fpGSIC_flat)
    fpGIS_loc <- mean(fpGIS_flat)
    fpTE_loc <- 1
    fpLWS_loc <- 1
    
    # Keep searching nearby lat/lon values if fingerprint value is NaN unless limit is hit
    # Search for fingerprint values
    inc <- 1
    fpAIS_loc <- NaN
    fpGIS_loc <- NaN
    fpGSIC_loc <- NaN
    while (is.na(fpAIS_loc) || is.na(fpGIS_loc) || is.na(fpGSIC_loc) & inc < 5) {
      newlonStart <- next_lon(lons[i], inc, "decrease")
      newlatStart <- next_lat(lats[i], inc, "decrease")
      newlonEnd <- next_lon(lons[i], inc, "increase")
      newlatEnd <- next_lat(lats[i], inc, "increase")
      
      latInd1 <- which(abs(flat-newlatStart) == min(abs(flat-newlatStart)))
      latInd2 <- which(abs(flat-newlatEnd) == min(abs(flat-newlatEnd)))
      
      lonInd1 <- which(abs(flon-newlonStart) == min(abs(flon-newlonStart)))
      lonInd2 <- which(abs(flon-newlonEnd) == min(abs(flon-newlonEnd)))
      
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
      
      fpAIS_flat <- na.omit(as.vector(fpAIS[lonInds, latInds]))
      fpGSIC_flat <- na.omit(as.vector(fpGSIC[lonInds, latInds]))
      fpGIS_flat <- na.omit(as.vector(fpGIS[lonInds, latInds]))
      
      fpAIS_loc <- mean(fpAIS_flat)
      fpGSIC_loc <- mean(fpGSIC_flat)
      fpGIS_loc <- mean(fpGIS_flat)
      
      inc <- inc + 1
    }
    
    # If still NaN, throw an error
    if (is.na(fpAIS_loc) || is.na(fpGIS_loc) || is.na(fpGSIC_loc)) {
      stop(paste0("Error: no fingerprints found for (", lon, ",", lat, ")"))
    }
    
    #append to the DataFrame
    write.csv(df, file = fp_segments_file, row.names = FALSE)
  }
}
  
  next_lat <- function(lat, inc, direction){
    if(lat < -90 || lat > 90){
      stop("Latitude must be between -90 and 90")
    }
    
    if(direction == "increase"){
      new_lat <- lat + inc
      if(new_lat > 90){
        new_lat <- new_lat - 180 # wrap around
      }
    }else if(direction == "decrease"){
      new_lat <- lat - inc
      if(new_lat < -90){
        new_lat <- new_lat + 180
      }
    }
    
    return(new_lat)
  }
  
  #' Increment longitude by inc in either positive direction (direction="increase")
  #' or in the negative direction (direction="decrease").
  #' Assumes longitude runs from 0 to 360 (deg E).
  #' @param lon numeric value of longitude (in degrees)
  #' @param inc numeric value of increment to be applied to the longitude
  #' @param direction character value indicating the direction of increment, can be "increase" or "decrease"
  #' @return numeric value of the updated longitude
  next_lon <- function(lon, inc, direction){
    if(lon < 0 || lon > 360){
      stop("Longitude must be between 0 and 360")
    }
    
    if(direction == "increase"){
      new_lon <- lon + inc
      if(new_lon > 360){
        new_lon <- new_lon - 360
      }
    }else if(direction == "decrease"){
      new_lon <- lon - inc
      if(new_lon < 0){
        new_lon <- new_lon + 360
      }
    }
    
    return(new_lon)
  }
  
  
  
  
    
      
      