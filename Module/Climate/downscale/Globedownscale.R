library(raster)
library(sf)
library(exactextractr)
library(openxlsx)
 
setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise/Module")

# ####Country level shapefile
# setwd("/Users/tianpeng/Desktop/nonCO2-cost/NCC_revise/Module/GAMD")
# country_list <- read.xlsx("../GAMD_ISO.xlsx",sheet = "Globe",startRow = 1)
# charafr<-country_list$ISO3
# area_map = matrix(nrow = length(charafr),ncol = 1)
# ac <- list()
# for (i in 1:length(charafr)){
#   ac[[i]] <- getData('GADM', country=c(charafr[i]), level=0)
#   area_map[i] = ac[[i]]@polygons[[1]]@area
# }
# allac = do.call("bind",ac)
# write.csv(area_map,"area_map.csv",quote = F,row.names = F)
# shapefile(allac, './Level0/GAMD_level0.shp',overwrite=TRUE)


country_shp <- st_read(dsn = "GAMD/Level0/GAMD_level0.shp",
                       quiet = TRUE, stringsAsFactors = FALSE)

country_shp <- st_read(dsn = "GAMD/Level0/abw_admbnda_adm0_2020.shp",
                       quiet = TRUE, stringsAsFactors = FALSE)

globe_map_data <- read.xlsx("GAMD_ISO.xlsx",sheet = "Map_region",startRow = 1)

years_tot_base = 1900:1950 #####2010-2019 average temperature
years_tot_his = 1900:2005 #####2010-2019 average temperature
years_tot_pro = 2006:2100
years_tot_cmip5 = 1900:2100

country_data = matrix(nrow = nrow(country_shp)*length(years_tot_cmip5),ncol = 5)
country_data = as.data.frame(country_data)
colnames(country_data) = c("YEAR","GID_0","NAME_0","type_u","type_u_anomalies");
country_data$YEAR = rep(years_tot_cmip5,each=nrow(country_shp))
country_data$GID_0 = rep(country_shp$GID_0,length(years_tot_cmip5))  
country_data$NAME_0 = rep(country_shp$NAME_0,length(years_tot_cmip5))  

globe_data = matrix(nrow = length(years_tot_cmip5),ncol = 5)
globe_data = as.data.frame(globe_data)
colnames(globe_data) = c("YEAR","GID_0","NAME_0","type_u","type_u_anomalies");
globe_data$YEAR = years_tot_cmip5
globe_data$GID_0 = rep("globe",length(years_tot_cmip5))  
globe_data$NAME_0 = rep("globe",length(years_tot_cmip5))   

weather_brick_origin_rcp45 = brick("CMIP5/tas_Amon_ensemble_rcp45_200601-210012.nc",nlayers=1140)  ##download from https://climate-scenarios.canada.ca/?page=gridded-data
weather_brick1_rcp45 <- crop(weather_brick_origin_rcp45, extent(0, 180, -90, 90))
weather_brick2_rcp45 <- crop(weather_brick_origin_rcp45, extent(180, 360, -90, 90))   
extent(weather_brick2_rcp45) <- c(-180, 0, -90, 90)
weather_brick_rcp45 <- merge(weather_brick1_rcp45, weather_brick2_rcp45)
weather_raster_target_rcp45 <- weather_brick_rcp45
weather_crop_rcp45 = raster::crop(weather_raster_target_rcp45, country_shp)

weather_brick_origin_historic = brick("CMIP5/tas_Amon_ensemble_historical_190001-200512.nc",nlayers=1272)  ##download from https://climate-scenarios.canada.ca/?page=gridded-data
weather_brick1_historic <- crop(weather_brick_origin_historic, extent(0, 180, -90, 90))
weather_brick2_historic <- crop(weather_brick_origin_historic, extent(180, 360, -90, 90))   
extent(weather_brick2_historic) <- c(-180, 0, -90, 90)
weather_brick_historic <- merge(weather_brick1_historic, weather_brick2_historic)
weather_raster_target_historic <- weather_brick_historic
weather_crop_historic = raster::crop(weather_raster_target_historic, country_shp)

#aggreagte to country without weighting by population (unweighted)
weather_data_temp_area_rcp45 = exact_extract(weather_crop_rcp45, country_shp,fun =c( "weighted_mean"), weights = area(weather_crop_rcp45))
weather_data_temp_area_historic = exact_extract(weather_crop_historic, country_shp,fun =c( "weighted_mean"), weights = area(weather_crop_historic))

#aggreagte to globe without weighting by population (unweighted)
globe_poly <- sf::st_as_sfc('POLYGON ((-180 -90, -180 90, 180 90, 180 -90,-180 -90))')
weather_data_temp_globe_weighted_rcp45 = exact_extract(weather_brick_rcp45, globe_poly,fun =c( "weighted_mean"),weights = area(weather_brick_rcp45))
weather_data_temp_globe_weighted_historic = exact_extract(weather_brick_historic, globe_poly,fun =c( "weighted_mean"),weights = area(weather_brick_historic))

for (yy in 1:length(years_tot_cmip5)) {
  year_target = years_tot_cmip5[yy];
  if(yy<=length(years_tot_his)){
  country_data$type_u[((yy-1)*nrow(country_shp)+1):(yy*nrow(country_shp))] = rowMeans(weather_data_temp_area_historic[,((yy-1)*12+1):(yy*12)])-273.15
  globe_data$type_u[yy] = mean(as.numeric(weather_data_temp_globe_weighted_historic[((yy-1)*12+1):(yy*12)]))-273.15
  }else{
  country_data$type_u[((yy-1)*nrow(country_shp)+1):(yy*nrow(country_shp))] = rowMeans(weather_data_temp_area_rcp45[,((yy-length(years_tot_his)-1)*12+1):((yy-length(years_tot_his))*12)])-273.15
  globe_data$type_u[yy] = mean(as.numeric(weather_data_temp_globe_weighted_rcp45[((yy-length(years_tot_his)-1)*12+1):((yy-length(years_tot_his))*12)]))-273.15
  }
}

#####baseline 1900-1950
for (yy in 1:length(years_tot_cmip5)) {
  globe_data$type_u_anomalies[yy] = globe_data$type_u[yy] - mean(globe_data$type_u[1:51])
  for (cc in 1:nrow(country_shp)) {
    map_country_name = country_shp$GID_0[cc];
    country_data$type_u_anomalies[(yy-1)*nrow(country_shp)+cc] = country_data[country_data$GID_0==map_country_name,]$type_u[yy] - mean(country_data[country_data$GID_0==map_country_name,]$type_u[1:50]) 
  }
}
# for (yy in 1:length(years_tot_cmip5)) {
# country_data$type_u_anomalies[(yy-1)*nrow(country_shp)+cc] = country_data$type_u[yy] - mean(country_data$type_u[1:50]) 
# }
######getting country specific temperature rise coefficient
map_temp_coeff = matrix(nrow = nrow(country_shp),ncol = 2)
map_temp_coeff = as.data.frame(map_temp_coeff)
colnames(map_temp_coeff) = c("GID_0","Coeff");
map_temp_coeff$GID_0 = country_shp$GID_0

for (cc in 1:nrow(country_shp)) {
  map_country_name = country_shp$GID_0[cc];
  linear_regression = lm(country_data[country_data$GID_0==map_country_name,]$type_u_anomalies[(length(years_tot_base)+1):length(years_tot_cmip5)]  ~ globe_data$type_u_anomalies[(length(years_tot_base)+1):length(years_tot_cmip5)] + 0)
  map_temp_coeff$Coeff[cc] = as.numeric(linear_regression$coefficients)
}

# linear_regression = lm(country_data$type_u_anomalies[(length(years_tot_base)+1):length(years_tot_cmip5)]  ~ globe_data$type_u_anomalies[(length(years_tot_base)+1):length(years_tot_cmip5)] + 0)
# as.numeric(linear_regression$coefficients)

############For Burke et al. country level model
country_data <- read.xlsx("GAMD_ISO.xlsx",sheet = "Country",startRow = 1)
country_list = country_data$ISO3
country_temp_coeff = matrix(nrow = length(country_list),ncol=2)
country_temp_coeff = as.data.frame(country_temp_coeff)
colnames(country_temp_coeff) = c("GID_0","Coeff");
country_temp_coeff$GID_0 = country_list

for (rr in 1:length(country_list)) {
  country_target = country_list[rr]
  country_temp_coeff$Coeff[rr] = map_temp_coeff[map_temp_coeff$GID_0 %in% country_data[country_data$ISO3==country_target,]$ISO3,]$Coeff
}

#############For FUND model
fund_region_data <- read.xlsx("GAMD_ISO.xlsx",sheet = "FUND_region",startRow = 1)
fund_region_list = c("USA","CAN","WEU","JPK","ANZ","CEE","FSU","MDE","CAM","SAM","SAS","SEA","CHI","NAF","SSA","SIS")
fund_temp_coeff = matrix(nrow = length(fund_region_list),ncol=2)
fund_temp_coeff = as.data.frame(fund_temp_coeff)
colnames(fund_temp_coeff) = c("GID_0","Coeff");
fund_temp_coeff$GID_0 = fund_region_list

for (rr in 1:length(fund_region_list)) {
  fund_region_target = fund_region_list[rr]
  fund_temp_coeff$Coeff[rr] = sum(map_temp_coeff[map_temp_coeff$GID_0 %in% fund_region_data[fund_region_data$Region==fund_region_target,]$ISO3,]$Coeff *
  globe_map_data[globe_map_data$ISO3 %in% fund_region_data[fund_region_data$Region==fund_region_target,]$ISO3,]$Area_map)/sum(
    globe_map_data[globe_map_data$ISO3 %in% fund_region_data[fund_region_data$Region==fund_region_target,]$ISO3,]$Area_map)
}

#############For PAGE model
page_region_data <- read.xlsx("GAMD_ISO.xlsx",sheet = "PAGE_region",startRow = 1)
page_region_list = c("EU","US","OT","EE","CA","IA","AF","LA")
page_temp_coeff = matrix(nrow = length(page_region_list),ncol=2)
page_temp_coeff = as.data.frame(page_temp_coeff)
colnames(page_temp_coeff) = c("GID_0","Coeff");
page_temp_coeff$GID_0 = page_region_list

for (rr in 1:length(page_region_list)) {
  page_region_target = page_region_list[rr]
  page_temp_coeff$Coeff[rr] = sum(map_temp_coeff[map_temp_coeff$GID_0 %in% page_region_data[page_region_data$Region==page_region_target,]$ISO3,]$Coeff *
                                    globe_map_data[globe_map_data$ISO3 %in% page_region_data[page_region_data$Region==page_region_target,]$ISO3,]$Area_map)/sum(
                                      globe_map_data[globe_map_data$ISO3 %in% page_region_data[page_region_data$Region==page_region_target,]$ISO3,]$Area_map)
}

temp_coeff <- list("country_temp_coeff" = country_temp_coeff, "fund_temp_coeff" = fund_temp_coeff, "page_temp_coeff" = page_temp_coeff)
write.xlsx(temp_coeff, "Climate/downscale/Downscale_temp_coeff.xlsx")



