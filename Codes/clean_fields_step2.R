grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v1_dt.rds")

grid10_soils_dt3 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt3.rds")

grid10_soils_dt3[,apsoil := ifelse(mukey %in% unique(grid10_horizons_v1_dt$mukey), 1, 0)]

table(grid10_soils_dt3$apsoil)

grid10_soils_dt3[apsoil == 0,]

grid10_soils_dt4 <- grid10_soils_dt3[grid10_soils_dt3$apsoil == 1, ] %>% dplyr::select(-c('apsoil')) 


#--------------------------------------------------------------------------------
#Check the area by field
grid10_soils_dt4[, field_ha := sum(area_ha), by = .(id_tile, id_10, id_field)]
grid10_soils_dt4 <- grid10_soils_dt4[field_ha > 39]

grid10_fields_sf <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf.rds')
table(st_is_empty(grid10_fields_sf))

good_fields_dt <- grid10_soils_dt4[,.N, by = .(id_10, id_field)] %>% .[,-'N'] %>% .[,ok := 1]

grid10_fields_sf2 <- dplyr::left_join(grid10_fields_sf, good_fields_dt, by = c('id_10', 'id_field'))
grid10_fields_sf2 <- grid10_fields_sf2[!is.na(grid10_fields_sf2$ok),] %>% dplyr::select(-ok)
table(st_is_empty(grid10_fields_sf2))

#--------------------------------------------------------------------------------
#add coordinates
grid10_fields_sf2 <- cbind(grid10_fields_sf2, st_coordinates(st_centroid(grid10_fields_sf2))) %>% setnames(c('X', "Y"), c('long', 'lat'))
grid10_soils_dt4 <- merge(grid10_soils_dt4, data.table(grid10_fields_sf2) %>% .[,.(id_10, id_field, long, lat)], by = c('id_10', 'id_field'))

saveRDS(grid10_soils_dt4, "./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
saveRDS(grid10_fields_sf2, "./n_policy_box/Data/Grid/grid10_fields_sf2.rds")

summary(grid10_horizons_v1_dt$restriction)

#======================================================================================
# GET REGIONS - they will have the same amount of corn cells on them
# This was done later but then I realized I need it before running the simulations
if(FALSE){
  grid10_tiles_sf5 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles.sf5.rds") 
  # 
  # tm_shape(grid10_tiles_sf5) + tm_polygons("county_name")
  # 
  # grid10_tiles_sf5 <- st_transform(grid10_tiles_sf5, 4326)
  # regions_dt <- data.table(grid10_tiles_sf5[, c('id_10', 'corn5_cell')], st_coordinates(st_centroid(grid10_tiles_sf5))) %>% .[,-'geometry']
  # setnames(regions_dt, c('X', 'Y'), c('long', 'lat'))
  # lat_min <- min(regions_dt$lat)
  # lat_max <- max(regions_dt$lat)
  # regions_dt[, region := .bincode(regions_dt$lat, breaks=c(lat_min-1, quantile(rep(regions_dt$lat, regions_dt$corn5_cell), c(0.333333, 0.66666)), lat_max +1))]
  # regions_dt[,.(corn5_cell = sum(corn5_cell)), by = region]
  
  
  
  #----------
  # Fix the file by hand and load again
  # rid10_region <- grid10_tiles_sf7 %>% group_by(region) %>% summarize()
  # 
  # #install.packages('smoothr')
  # library(smoothr)
  # area_thresh <- units::set_units(10*10+10, km^2)
  # grid10_region2 <- fill_holes(grid10_region, threshold = area_thresh)
  # st_write(grid10_region, "./n_policy_box/Data/shapefiles/grid10_region.shp", delete_dsn = TRUE)
  
  grid10_region_by_hand <- sf::read_sf('./n_policy_box/Data/shapefiles/grid10_region_by_hand_mrtn.shp')
  grid10_region_by_hand <- st_transform(grid10_region_by_hand, crs = st_crs(grid10_tiles_sf5))
  # tm_shape(grid10_region_by_hand) + tm_polygons('region')
  
  # Add the region to both spatial files
  grid10_tiles_sf6 <- st_join(grid10_tiles_sf5, grid10_region_by_hand, join = st_intersects, largest = T)
  tm_shape(grid10_tiles_sf6) + tm_polygons('region')
  coordinates_df <- data.frame(st_coordinates(st_centroid(grid10_tiles_sf6)))
  class(coordinates_df)
  grid10_tiles_sf6$lat <- coordinates_df$Y
  hist(grid10_tiles_sf6[is.na(grid10_tiles_sf6$region),]$lat)
  lat_limit <- 1900000 
  grid10_tiles_sf6 <- grid10_tiles_sf6 %>% dplyr::mutate(region = ifelse(is.na(region) & lat > lat_limit, 3, region))
  grid10_tiles_sf6 <- grid10_tiles_sf6 %>% dplyr::mutate(region = ifelse(is.na(region) & lat < lat_limit, 1, region))
  tm_shape(grid10_tiles_sf6) + tm_polygons('region')
  
  saveRDS(grid10_tiles_sf6, "./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
  grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds") 
  
  #Update other files
  regions_dt <- data.table(grid10_tiles_sf6) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
  grid10_soils_dt4 <- data.table(left_join(grid10_soils_dt4[,-'region'], regions_dt[,.(id_10, region)], by = 'id_10'))
  grid10_soils_dt4 <- data.table(grid10_soils_dt4)
  table(grid10_soils_dt4$region)
  saveRDS(grid10_soils_dt4, "./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  
  grid10_fields_sf2 <- readRDS("./n_policy_box/Data/Grid/grid10_fields_sf2.rds")
  grid10_fields_sf2$region <- NULL
  grid10_fields_sf2 <- left_join(grid10_fields_sf2, regions_dt[,.(id_10, region)], by = 'id_10')
  saveRDS(grid10_fields_sf2, "./n_policy_box/Data/Grid/grid10_fields_sf2.rds")
  
  #Add hybrids
  # grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds")
  # hybrids_by_hand_sf <- sf::read_sf('./n_policy_box/Data/shapefiles/hybrids_by_hand.shp')
  # hybrids_by_hand_sf <- st_transform(hybrids_by_hand_sf, crs = st_crs(grid10_tiles_sf6))
  # setnames(hybrids_by_hand_sf, 'id', 'hybrids_rm')
  # grid10_tiles_sf6 <- st_join(grid10_tiles_sf6, hybrids_by_hand_sf, join = st_intersects, largest = T)
  # tm_shape(grid10_tiles_sf6) + tm_polygons('hybrids_rm')
  # grid10_tiles_sf6 <- grid10_tiles_sf6 %>% dplyr::mutate(hybrids_rm = ifelse(is.na(hybrids_rm) , '110', hybrids_rm))
  # saveRDS(grid10_tiles_sf6, "./n_policy_box/Data/Grid/grid10_tiles_sf6.rds")
  # st_write(grid10_tiles_sf6, "./n_policy_box/Data/shapefiles/grid10_tiles_sf6.shp", delete_dsn = TRUE)
}  