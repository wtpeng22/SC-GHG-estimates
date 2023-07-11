library(raster)
library(sf)
library(exactextractr)
library(terra)
library(openxlsx)

setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise/Module")
############For Burke et al. country level model
###baseline observed temperature data
country_shp <- st_read(dsn = "GAMD/Level0/GAMD_level0.shp",
                       quiet = TRUE, stringsAsFactors = FALSE)

# country_shp <- st_read(dsn = "GAMD/Level0/abw_admbnda_adm0_2020.shp",
#                        quiet = TRUE, stringsAsFactors = FALSE)

globe_map_data <- read.xlsx("GAMD_ISO.xlsx",sheet = "Map_region",startRow = 1)
years_tot_base = 2000:2009 #####2010-2019 average temperature

for (yy in 1:length(years_tot_base)) {
  print(yy)
  year_target=as.character(years_tot_base[yy])

  if(yy%%5==1){
    population_brick = as.data.frame(raster(paste("Population/gpw_v4_population_count_rev11_",year_target,"_15_min.tif",sep="")),xy = T)
    population_rast = rast(paste("Population/gpw_v4_population_count_rev11_",year_target,"_15_min.tif",sep=""))
    colnames(population_brick) = c("lon","lat",year_target)
  }else{
    year_target_before = years_tot_base[yy] - years_tot_base[yy]%%5
    year_target_after = years_tot_base[yy] - years_tot_base[yy]%%5 +5;
    
    population_brick_before = as.data.frame(raster(paste("Population/gpw_v4_population_count_rev11_",year_target_before,"_15_min.tif",sep="")),xy = T)
    population_rast_before = (rast(paste("Population/gpw_v4_population_count_rev11_",year_target_before,"_15_min.tif",sep="")))

    population_brick_after = as.data.frame(raster(paste("Population/gpw_v4_population_count_rev11_",year_target_after,"_15_min.tif",sep="")),xy = T)
    population_rast_after = (rast(paste("Population/gpw_v4_population_count_rev11_",year_target_after,"_15_min.tif",sep="")))
    
    population_brick = population_brick_before;
    population_brick[,3] = population_brick_before[,3]*(population_brick_after[,3] / population_brick_before[,3])^(0.2*(years_tot_base[yy]%%5))
    terra::values(population_rast) = terra::values(population_rast_before)*(terra::values(population_rast_after) / terra::values(population_rast_before))^(0.2*(years_tot_base[yy]%%5))
    colnames(population_brick) = c("lon","lat",year_target)

    names(population_rast) = year_target
  }
  
  population_raster_target <- rasterFromXYZ(population_brick[, c('lon', 'lat', year_target)])
  names(population_raster_target) = year_target
  population_crop = raster::crop(population_raster_target, country_shp)

  if(yy==1){
    population_crop_series = population_crop
    population_rast_series = population_rast
  }else{
    population_crop_series = stack(population_crop_series,population_crop)
    population_rast_series = c(population_rast_series,population_rast)
  }
}

weather_brick = brick("Temp_obs/cru_ts4.06.1901.2021.tmp.dat.nc",layer = 1452)
time_weather_brick = getZ(weather_brick)
weather_data_temp_pop_months = matrix(nrow = nrow(country_shp),ncol = 12*length(years_tot_base))
weather_data_temp_pop_yearly = matrix(nrow = nrow(country_shp),ncol = length(years_tot_base))
weather_data_temp_pop_baseline = matrix(nrow = nrow(country_shp),ncol = 2)
weather_data_temp_pop_baseline = as.data.frame(weather_data_temp_pop_baseline)
colnames(weather_data_temp_pop_baseline) = c("GID_0","Temperature");
weather_data_temp_pop_baseline$GID_0 = country_shp$GID_0

for (mm in 1:12) {
  for (yy in 1:length(years_tot_base)) {
    print(yy)
    time_target = time_weather_brick[(yy-1+years_tot_base[1] - 1901)*12+mm]
    weather_raster_target <- subset(weather_brick, which(getZ(weather_brick) == time_target))
    weather_crop = raster::crop(weather_raster_target, country_shp)
    if(yy==1){
      weather_crop_series = weather_crop
    }else{
      weather_crop_series = stack(weather_crop_series,weather_crop)
    }
  }
  #aggreagte to country weighted by population
  population_rs = raster::resample(population_crop_series, weather_crop_series)
  weather_data_temp_pop <- exact_extract(weather_crop_series, country_shp, fun = "weighted_mean", weights=population_rs,default_weight=0);
  # weather_data_temp_pop <- exact_extract(weather_crop_series, country_shp, fun = "mean",default_weight=0);
  
  weather_data_temp_pop_months[,((mm-1)*length(years_tot_base)+1):(mm*length(years_tot_base))] = as.matrix(weather_data_temp_pop)
}

for (yy in 1:length(years_tot_base)) {
  weather_data_temp_pop_yearly[,yy] = rowMeans(weather_data_temp_pop_months[,seq(yy,ncol(weather_data_temp_pop_months),by=length(years_tot_base))])
}

weather_data_temp_pop_baseline$Temperature = rowMeans(weather_data_temp_pop_yearly)

############For Burke et al. country level damage model
country_data <- read.xlsx("GAMD_ISO.xlsx",sheet = "Country",startRow = 1)
temp_pop_baseline_target = weather_data_temp_pop_baseline[weather_data_temp_pop_baseline$GID_0 %in% country_data$ISO3,]

write.xlsx(temp_pop_baseline_target, "Climate/downscale/Country_level_basetemp2000.xlsx")

