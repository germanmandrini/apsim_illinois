setwd("~/")
setwd('C:/Users/germanm2/Box Sync/My_Documents')

source('/home/germanm2/Codes_useful/R.libraries.R')
source('./Codes_useful/R.libraries.R')
#install.packages('fasterize')
library('fasterize')


#Keep the rasters in the P drive for now
CFL_r <- raster('S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_2008-2018.img')
CFL_r <- raster('~/crop_frequency_layer/crop_frequency_corn_small.img') #shortcut for the server


#ADD GEOGRAPHIC DATA
# install.packages('USAboundaries')
library(USAboundaries)
us_counties <- us_counties() 

crs_cdl <- crs(CFL_r)

us_states_GRS80_sf <- st_transform(us_counties, crs_cdl@projargs)
us_states_GRS80_sf <- us_states_GRS80_sf %>% dplyr::select(state_name, county_name = name) %>% dplyr::filter(state_name == 'Illinois')
#us_states_GRS80_sf <- us_states_GRS80_sf %>% dplyr::filter(county_name == 'Champaign')

CFL_r <- crop(CFL_r,us_states_GRS80_sf)
CFL_r$crop_frequency_corn_2008.2018@file

CFL_r[CFL_r == 0] <- NA #0 means no corn. 
CFL_r[CFL_r == 255] <- NA # 255 means background

CFL_r@data@attributes
str(CFL_r)

#SAVE A SMALLER VERSION OF THE CROP FREQUENCY RASTER, EASIER TO UPLOAD TO R
raster::writeRaster(CFL_r, 'S:/Bioinformatics Lab/germanm2/vr_value/crop_frequency_layer/crop_frequency_corn_small.img', overwrite=TRUE)

table(CFL_r[])

#----------------------------------------------------------------------
# CREATE A GOOD grid.10.sf
# with id.tiles, id.10 and NA if do not touch the us_states

grid10.r <- raster::raster(CFL_r)
res(grid10.r) <- 10000
values(grid10.r) <- 1:ncell(grid10.r)

# ### CREATE AND ADD THE TILES
tiles.r <- raster::raster(CFL_r)
res(tiles.r) <- 100000
values(tiles.r) <- 1:ncell(tiles.r)
tiles.sp <- rasterToPolygons(tiles.r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
names(tiles.sp) <- 'id_tile'
plot(tiles.sp)
tiles.sf <- st_as_sf(tiles.sp) # convert polygons to 'sf' object

grid10_tiles.r <- fasterize(tiles.sf, grid10.r, field = 'id_tile') #Transfer values associated with 'object' type spatial data (points, lines, polygons) to raster cells

#writeRaster(grid10_tiles.r, "./Project.Grid/Grid/rds.files/grid10_tiles", format = 'GTiff', overwrite = TRUE)

length(which(is.na(grid10_tiles.r[])))/ncell(grid10_tiles.r)

#The grid10.r is slightly larger than tiles.sf. We will put all the not assinged cells to another tile
grid10_tiles.r[which(is.na(grid10_tiles.r[]))] <- ncell(tiles.r)+1

tiles_key.df <- data.frame(id_10 = grid10.r[],
                           id_tile = grid10_tiles.r[])

grid10.sp <- rasterToPolygons(grid10.r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
names(grid10.sp@data) <- 'id_10'
grid10.sf <- st_as_sf(grid10.sp) # convert polygons to 'sf' object
crs_raster <- raster::crs(grid10.r)
st_crs(grid10.sf) <- crs_raster@projargs
grid10_tiles.sf <- dplyr::left_join(grid10.sf, tiles_key.df)
head(grid10_tiles.sf)
saveRDS(grid10_tiles.sf, "./vr_value/Data/Grid/grid10_tiles.sf1.rds")
grid10_tiles.sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf1.rds")

tm_shape(grid10_tiles.sf) + tm_polygons('id_tile')

max(grid10_tiles.sf$id_tile)
grid10_tiles.sf$corn5_tile <- NA
grid10_tiles.sf$corn5_cell <- NA

#CLEAN TILES WITHOUT CORN

for(tile_n in unique(grid10_tiles.sf$id_tile)){
  #tile_n=10
  print(tile_n)
  one_tile.sf <- grid10_tiles.sf[grid10_tiles.sf$id_tile == tile_n,]
  cult.layer_one_tile.r <- raster::crop(CFL_r,one_tile.sf)
  ext <- getValues(cult.layer_one_tile.r)
  ext <- ext[!is.na(ext)]
  count_crops_tile = sum(ext > 4) #this gives how many cells have CORN for more than # years
  
  grid10_tiles.sf[grid10_tiles.sf$id_tile == tile_n, 'corn5_tile'] <- count_crops_tile
  
  if(count_crops_tile > 0){
    #CLEAN CELLS WITHOUT CROPS
    for(cell_n in unique(one_tile.sf$id_10)){
      #cell_n=711 
      #print(cell_n)
      one_cell.sf <- one_tile.sf[one_tile.sf$id_10 == cell_n,]
      cult.layer_one_cell.r <- crop(cult.layer_one_tile.r,one_cell.sf)
      ext <- getValues(cult.layer_one_cell.r)
      ext <- ext[!is.na(ext)]
      count_crops_cell = sum(ext > 4)  #this gives how many cells have CORN for more than # years
      
      grid10_tiles.sf[grid10_tiles.sf$id_10 == cell_n, 'corn5_cell'] <- count_crops_cell
      
    }  
    
    # test <- grid10_tiles.sf[grid10_tiles.sf$id_tile == tile_n,] 
    # print(sum(test$corn5_cell) == count_crops_tile)
    
    
  }
  
  
}

saveRDS(grid10_tiles.sf, "./vr_value/Data/Grid/grid10_tiles.sf2.rds")
grid10_tiles.sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf2.rds")

#DELETE CELLS WITH LESS THAN 20 HA OF CORN
cells_20ha <- 20*10000/(30*30)

nrow(grid10_tiles.sf)
grid10_tiles.sf <- grid10_tiles.sf[grid10_tiles.sf$corn5_tile > 0,]
grid10_tiles.sf <- grid10_tiles.sf[!is.na(grid10_tiles.sf$corn5_cell),]
grid10_tiles.sf <- grid10_tiles.sf[grid10_tiles.sf$corn5_cell > cells_20ha,]

nrow(grid10_tiles.sf)

saveRDS(grid10_tiles.sf, "./vr_value/Data/Grid/grid10_tiles.sf3.rds")
grid10_tiles.sf <- readRDS("./vr_value/Data/Grid/grid10_tiles.sf3.rds")

#ADD GEOGRAPHIC DATA

grid10_tiles.sf <- st_join(grid10_tiles.sf, us_states_GRS80_sf, join = st_intersects, left = TRUE, largest = TRUE)
# check == nrow(grid10_tiles.sf)
saveRDS(grid10_tiles.sf, "./vr_value/Data/Grid/grid10_tiles.sf4.rds")
tm_shape(grid10_tiles.sf) + tm_polygons('county_name')

#REMOVE AREAS OUTSIDE THE STATE
grid10_tiles.sf <- grid10_tiles.sf[!is.na(grid10_tiles.sf$county_name),]

#MAKE IDS SEQUENTIAL
grid10_tiles.sf <- grid10_tiles.sf[order(grid10_tiles.sf$id_tile, grid10_tiles.sf$id_10),]#order
grid10_tiles.sf <- dplyr::mutate(grid10_tiles.sf, id_10 = 1:nrow(grid10_tiles.sf))

grid10_tiles.sf <- grid10_tiles.sf[order(grid10_tiles.sf$id_tile, grid10_tiles.sf$id_10),]#order again just in case

each_tile <- table(grid10_tiles.sf$id_tile)
tiles_c <- rep(1:length(unique(grid10_tiles.sf$id_tile)), times = each_tile)
table(tiles_c)[1:10]
each_tile[1:10]

grid10_tiles.sf <- dplyr::mutate(grid10_tiles.sf, id_tile = tiles_c)
table(grid10_tiles.sf$id_tile)[1:10]

#ORDER AND CLEAN ROW NAMES

grid10_tiles.sf <- grid10_tiles.sf[order(grid10_tiles.sf$id_tile, grid10_tiles.sf$id_10),]#order
rownames(grid10_tiles.sf) <- 1:nrow(grid10_tiles.sf)

#SAVE FINAL VERSION
saveRDS(grid10_tiles.sf, "./vr_value/Data/Grid/grid10_tiles.sf5.rds")
# st_write(grid10_tiles.sf, "./vr_value/Data/Grid/grid10_tiles.shp", delete_dsn=TRUE)
