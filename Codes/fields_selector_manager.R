setwd("~/")
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
setwd('C:/Users/germa/Box Sync/My_Documents') #Dell

source('./Codes_useful/R.libraries.R')

#install.packages('fasterize')
# library('fasterize')


#Load Data
CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_small.img')

CFL_r <- raster::raster("./crop_frequency_layer/crop_frequency_corn_small.img")


# CDL09 <- raster('./Project.Grid/Grid/CDL/2009_30m_cdls/2009_30m_cdls.img')
# CDL08 <- raster('//ad.uillinois.edu/aces/CPSC/share/Bioinformatics Lab/germanm2/Grid/CDL/2008_30m_cdls/2008_30m_cdls.img')
# CDL08 <- raster('//ad.uillinois.edu/aces/CPSC/share/Bioinformatics Lab/germanm2/Grid/CDL/2008_30m_cdls/2008_30m_cdls.img')


# //aces-dfs/03.ad.uillinois.edu/cpsc-share
# file:///S:/Bioinformatics Lab/germanm2/vr_value_v2/crop_frequency_layer/crop_frequency_corn_small.img

grid10_tiles.sf <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles.sf5.rds")

#======================================================================================
# GET REGIONS - they will have the same amount of corn cells on them

tm_shape(grid10_tiles.sf) + tm_polygons("county_name")

grid10_tiles_sf <- st_transform(grid10_tiles.sf, 4326)
regions_dt <- data.table(grid10_tiles_sf[, c('id_10', 'corn5_cell')], st_coordinates(st_centroid(grid10_tiles_sf))) %>% .[,-'geometry']
setnames(regions_dt, c('X', 'Y'), c('long', 'lat'))
lat_min <- min(regions_dt$lat)
lat_max <- max(regions_dt$lat)
regions_dt[, region := .bincode(regions_dt$lat, breaks=c(lat_min-1, quantile(rep(regions_dt$lat, regions_dt$corn5_cell), c(0.333333, 0.66666)), lat_max +1))]
regions_dt[,.(corn5_cell = sum(corn5_cell)), by = region]

# Add the region to both spatial files
grid10_tiles.sf <- left_join(grid10_tiles.sf, regions_dt[,.(id_10, region)], by = 'id_10')
saveRDS(grid10_tiles.sf, "./vr_value/Data/Grid/grid10_tiles_sf6.rds") 
grid10_tiles.sf <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds") 

#======================================================================================

#Sample triple of the RS need, just to avoid looking 4 fields in many cells
# candidates_dt <- data.table(grid10_tiles.sf) %>% .[,.(id_10, region)] %>% .[,.N, by = .(id_10, region)]
# #candidates_dt2 <- candidates_dt
# stations_by_region <- 40
# candidates_dt <- candidates_dt[,.SD[sample(.N, 40*3)],by = .(region)]
# 
# grid10_tiles.sf <- dplyr::mutate(grid10_tiles.sf, RS = ifelse(id_10 %in% candidates_dt$id_10, 1, 0))

#======================================================================================

unique_id_tiles <- unique(grid10_tiles.sf$id_tile)

tm_shape(grid10_tiles.sf) + tm_polygons('id_tile')

all_tiles_list <- list()

for(tile_n in unique_id_tiles){
  # tile_n =  10
  print(paste('tile', tile_n))
  
  ids_10_seq <- grid10_tiles.sf[grid10_tiles.sf$id_tile == tile_n,]$id_10
  
  source('./vr_value_v2/Codes/fields_selector_parallel_focal_v2.R')
  all_tiles_list[[tile_n]] <- fields_sf
  
  # filename <- paste('./Project.Grid/Grid/rds.files/landuse_files/landuse_tile_', tile_n, '.rds', sep = '')  
  # saveRDS(results, filename)
  
  
}# end of tile_n loop

#-------------------------------------------------------------------------------
#Merge and fix row names
grid10_fields_sf <- do.call(what = base::rbind, args = all_tiles_list)
rownames(grid10_fields_sf) <- 1:nrow(grid10_fields_sf)
tm_shape(grid10_fields_sf) + tm_polygons()
nrow(grid10_fields_sf)

# #--------------------------------------------------------
# # Eliminate id_10 field 3 if there is not field 4
# grid10_fields_sf <- grid10_fields_sf %>% group_by(id_10) %>% dplyr::mutate(N = n()) %>% ungroup()
# 
# grid10_fields_sf2 <- dplyr::filter(grid10_fields_sf, id_field %in% c(1,2) | (id_field %in% c(3,4) & N == 4))
# nrow(grid10_fields_sf)
# nrow(grid10_fields_sf2)

#--------------------------------------------------------
# #Sample RS
# grid10_fields_sf2 <- cbind(grid10_fields_sf, st_coordinates(st_centroid(grid10_fields_sf))) %>% 
#   dplyr::rename(lat = X, long = Y) %>% dplyr::select(-sample)
# 
# stations_dt <- data.table(grid10_fields_sf2) %>% .[,.(long = mean(long),
#                                   lat = mean(lat),
#                                   N = .N,
#                                   region = mean(region)), by = .(id_10)] %>% .[N==4]
# 
# # stations_by_region <- round(nrow(stations_dt)*0.05/3,0)
# stations_by_region <- 40
# 
# # Also, only one station by id_10
# set.seed(123)
# # Equally distributed by region
# stations_dt <- stations_dt[order(region, lat)]
# 
# #Split the fields of each region into the number of stations we are getting by region, and then select one from each group
# stations_dt[,quantile := cut(lat, quantile(lat, probs = 0:stations_by_region/stations_by_region),
#                                           labels = FALSE, include.lowest = TRUE), by = region]
# 
# stations_dt2 <- stations_dt[,.SD[sample(.N, 1)],by = .(region, quantile)][,-c('long', 'quantile', 'lat')]
# 
# grid10_fields_sf2 <- dplyr::mutate(grid10_fields_sf, RS = ifelse(id_10 %in% stations_dt2$id_10,1,0))
# 
# grid10_fields_sf3 <- dplyr::filter(grid10_fields_sf2, id_field %in% c(1,2, 3) | (id_field %in% c(4) & RS == 1))
# nrow(grid10_fields_sf3)

#-------------------------------------------------------------------------------
# FIELDS NUMBER BY CELL

grid10_fields_dt <- data.table(grid10_fields_sf)
field_count_dt <-grid10_fields_dt[, .(field_count = .N), by = id_10]
grid10_fields_dt[, .(field_count = .N), by = region]
table(field_count_dt$field_count)

grid10_tiles.sf <- left_join(grid10_tiles.sf, field_count_dt, by = 'id_10')
grid10_tiles.sf$field_count[is.na(grid10_tiles.sf$field_count)] <- 0

p <- tm_shape(grid10_tiles.sf) + tm_polygons(c('corn5_cell', 'field_count')) +
  tm_layout(legend.text.size = 0.7,
            main.title = paste('Corn presence and Number of fields selected'),
            main.title.position = "center",
            main.title.size = 1)

tmap_save(p, filename = "./vr_value_v2/Data/figures/corn_vs_numberoffields.jpg", scale = 2)
#-------------------------------------------------------------------------------

# FIELDS DISPERSION
total_fields_num <- sum(field_count_dt$field_count)

grid10_tiles.sf
(p <- tm_shape(grid10_tiles.sf) + tm_borders()+ 
  tm_shape(grid10_fields_sf) + tm_dots() +
  tm_layout(title = paste(total_fields_num, "Total fields"),
            title.size = 0.7,
            title.position = c('left', 'bottom'),
            main.title = paste('GRID MAP - Res = 10 km'),
            main.title.position = "center"))

tmap_save(p, filename = "./vr_value_v2/Data/figures/grid_fields_dispersion.jpg", scale = 2)

saveRDS(grid10_fields_sf, "./vr_value_v2/Data/Grid/grid10_fields_sf.rds")
grid10_fields_sf <- readRDS("./vr_value_v2/Data/Grid/grid10_fields_sf.rds")
