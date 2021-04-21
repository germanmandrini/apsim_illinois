#OBJECTIVE: ADD THE AVERAGE AREA OF CORN TO EACH CELL. 
# THE AVG AREA IS THE TOTAL AREA OF CORN IN 11 YEARS/11

setwd("~/")
setwd('C:/Users/germanm2/Box Sync/My_Documents')

source('/home/germanm2/Codes_useful/R.libraries.R')
source('./Codes_useful/R.libraries.R')
#install.packages('fasterize')
library('fasterize')


#Keep the rasters in the P drive for now
CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value_v2/crop_frequency_layer/crop_frequency_corn_2008-2018.img')
CFL_r <- raster('~/crop_frequency_layer/crop_frequency_corn_small.img') #shortcut for the server
CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_small.img')
grid10_tiles_sf6 <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles_sf6.rds")

crs_cdl <- crs(CFL_r)
grid10_tiles_sf6 <- st_transform(grid10_tiles_sf6, crs_cdl@projargs)

#----------------------------------------------------------------------
tm_shape(grid10_tiles_sf6) + tm_polygons('id_tile')



max(grid10_tiles_sf6$id_tile)
grid10_tiles_sf6$corn_ha_cell <- NA
grid10_tiles_sf6$corn5_cell_check <- NA
grid10_tiles_sf6$corn5_tile_check <- NA

#CLEAN TILES WITHOUT CORN

for(tile_n in unique(grid10_tiles_sf6$id_tile)){
  # tile_n=10
  print(tile_n)
  one_tile.sf <- grid10_tiles_sf6[grid10_tiles_sf6$id_tile == tile_n,]
  cult.layer_one_tile.r <- raster::crop(CFL_r, one_tile.sf)
  ext <- getValues(cult.layer_one_tile.r)
  ext <- ext[!is.na(ext)]
  count_crops_tile = sum(ext > 4) #this gives how many cells have CORN for more than # years
  
  grid10_tiles_sf6[grid10_tiles_sf6$id_tile == tile_n, 'corn5_tile_check'] <- count_crops_tile
  
  if(count_crops_tile > 0){
    #CLEAN CELLS WITHOUT CROPS
    for(cell_n in unique(one_tile.sf$id_10)){
      # cell_n=711 
      #print(cell_n)
      one_cell.sf <- one_tile.sf[one_tile.sf$id_10 == cell_n,]
      cult.layer_one_cell.r <- crop(cult.layer_one_tile.r,one_cell.sf)
      ext <- getValues(cult.layer_one_cell.r)
      ext <- ext[!is.na(ext)]
      count_crops_cell = sum(ext > 4)  #this gives how many cells have CORN for more than # years
      corn_ha_cell <- sum(ext)*(30*30)/10000/11 #avg corn ha
      
      grid10_tiles_sf6[grid10_tiles_sf6$id_10 == cell_n, 'corn5_cell_check'] <- count_crops_cell
      grid10_tiles_sf6[grid10_tiles_sf6$id_10 == cell_n, 'corn_ha_cell'] <- corn_ha_cell
      
    }  
    
    # test <- grid10_tiles.sf[grid10_tiles.sf$id_tile == tile_n,] 
    # print(sum(test$corn5_cell) == count_crops_tile)
    
    
  }
  
  
}

grid10_tiles_sf6[grid10_tiles_sf6$corn5_cell != grid10_tiles_sf6$corn5_cell_check,] #beautiful
grid10_tiles_sf7 <- grid10_tiles_sf6 %>% dplyr::select(-corn5_cell_check, -corn5_tile_check)


grid10_tiles_sf7 <- st_transform(grid10_tiles_sf7, 4326)
saveRDS(grid10_tiles_sf7, "./vr_value_v2/Data/Grid/grid10_tiles_sf7.rds")

# grid10_tiles.sf <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles.sf2.rds")
# 
# #DELETE CELLS WITH LESS THAN 20 HA OF CORN
# cells_20ha <- 20*10000/(30*30)
# 
# nrow(grid10_tiles.sf)
# grid10_tiles.sf <- grid10_tiles.sf[grid10_tiles.sf$corn5_tile > 0,]
# grid10_tiles.sf <- grid10_tiles.sf[!is.na(grid10_tiles.sf$corn5_cell),]
# grid10_tiles.sf <- grid10_tiles.sf[grid10_tiles.sf$corn5_cell > cells_20ha,]
# 
# nrow(grid10_tiles.sf)
# 
# saveRDS(grid10_tiles.sf, "./vr_value_v2/Data/Grid/grid10_tiles.sf3.rds")
# grid10_tiles.sf <- readRDS("./vr_value_v2/Data/Grid/grid10_tiles.sf3.rds")
# 
# #ADD GEOGRAPHIC DATA
# 
# grid10_tiles.sf <- st_join(grid10_tiles.sf, us_states_GRS80_sf, join = st_intersects, left = TRUE, largest = TRUE)
# # check == nrow(grid10_tiles.sf)
# saveRDS(grid10_tiles.sf, "./vr_value_v2/Data/Grid/grid10_tiles.sf4.rds")
# tm_shape(grid10_tiles.sf) + tm_polygons('county_name')
# 
# #REMOVE AREAS OUTSIDE THE STATE
# grid10_tiles.sf <- grid10_tiles.sf[!is.na(grid10_tiles.sf$county_name),]
# 
# #MAKE IDS SEQUENTIAL
# grid10_tiles.sf <- grid10_tiles.sf[order(grid10_tiles.sf$id_tile, grid10_tiles.sf$id_10),]#order
# grid10_tiles.sf <- dplyr::mutate(grid10_tiles.sf, id_10 = 1:nrow(grid10_tiles.sf))
# 
# grid10_tiles.sf <- grid10_tiles.sf[order(grid10_tiles.sf$id_tile, grid10_tiles.sf$id_10),]#order again just in case
# 
# each_tile <- table(grid10_tiles.sf$id_tile)
# tiles_c <- rep(1:length(unique(grid10_tiles.sf$id_tile)), times = each_tile)
# table(tiles_c)[1:10]
# each_tile[1:10]
# 
# grid10_tiles.sf <- dplyr::mutate(grid10_tiles.sf, id_tile = tiles_c)
# table(grid10_tiles.sf$id_tile)[1:10]
# 
# #ORDER AND CLEAN ROW NAMES
# 
# grid10_tiles.sf <- grid10_tiles.sf[order(grid10_tiles.sf$id_tile, grid10_tiles.sf$id_10),]#order
# rownames(grid10_tiles.sf) <- 1:nrow(grid10_tiles.sf)
# 
# #SAVE FINAL VERSION
# saveRDS(grid10_tiles.sf, "./vr_value_v2/Data/Grid/grid10_tiles.sf5.rds")
# # st_write(grid10_tiles.sf, "./vr_value_v2/Data/Grid/grid10_tiles.shp", delete_dsn=TRUE)
