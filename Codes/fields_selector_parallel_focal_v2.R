#===================================
# prepare clusters
#===================================



no_cores <- detectCores() * 7/8
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================

# cell_n= ids_10_seq[20]

#FUNCTIONS TO REMEMBER
# boundaries(CFL_cell, type='inner', classes=T, directions=8, asNA=FALSE)
# cell_clump <- clump(CFL_cell, directions=8)

process_cells <- function(cell_n){
  library(data.table)
  library(raster)
  library(sf)
  library(dplyr)
  
  one_cell.sf <- grid10_tiles.sf[grid10_tiles.sf$id_10 == cell_n,]
  
  #tm_shape(one_cell.sf)+tm_polygons()
  CFL_cell <- raster::crop(CFL_r,one_cell.sf)
  CFL_cell <- raster::mask(CFL_cell, one_cell.sf)
  
  #----------------------------------------------------------------------
  #Real corn count
  years_corn <- getValues(CFL_cell)
  years_corn <- years_corn[!is.na(years_corn)]
  corn_count_dt <- data.table(as.numeric(years_corn))
  corn_count_dt <- corn_count_dt[,.N, by = .(years_corn)]
  corn_count_dt[, corn_ha_11 := years_corn * N * 30 * 30 /10000]
  corn_count_dt <- corn_count_dt[,.(corn_avg_ha = sum(corn_ha_11)/11)]

    #----------------------------------------------------------------------
  # plot(CFL_cell, main = '10x10 grid - Corn Frequency')

  CFL_5yrs <- CFL_cell > 4
  
  # plot(CFL_5yrs)
  CFL_cellw <- focal(CFL_5yrs, matrix(1, nrow = 21, ncol = 21), sum)
  # plot(CFL_cellw)
  CFL_cellw[CFL_cellw < 21*21] <- NA
  # plot(CFL_cellw)
  if(all(is.na(values(CFL_cellw)))){return()}
  
  #To avoid creating overlapping fields, sample two cells far apart
  area_ha <- 40
  dist_min <- sqrt( area_ha *10000 ) * 2
  fields_num <- 4 #ifelse(one_cell.sf$RS == 0, 2, 4)
  # pnt_sf <- st_utm(pnt_sf)
  set.seed(123)
  pnt_sf <- sample_fields(CFL_cellw, dist_min, fields_num) #and transforms to utm
  fields_sf <- make_squares(pnt_sf, area_ha)
  
  if(FALSE){
    CFL_cell_selected <- CFL_cell
    CFL_cell_selected[values(CFL_cell_selected) < 5] <- NA
    png('./vr_value/Data/figures/cfl_more4.png')
    plot(CFL_cell_selected, main = '10x10 grid - Corn Frequency >= 5')
    dev.off()
    
    CFL_5yrs_sp <- rasterToPolygons(CFL_cell_selected, dissolve = T)
    CFL_5yrs_sf <- st_as_sf(CFL_5yrs_sp) %>% dplyr::rename(corn_years = crop_frequency_corn_small)
    CFL_5yrs_sf <- st_cast(CFL_5yrs_sf, 'POLYGON')
    table(CFL_5yrs_sf$corn_years)
    CFL_5yrs_sf <- st_utm(CFL_5yrs_sf)
    p <- tm_shape(CFL_5yrs_sf) + tm_polygons('corn_years') + 
      tm_shape(fields_sf) + tm_borders(col = 'red', lwd = 3)
    tmap_save(p, filename = "./vr_value/Data/figures/sample_2_fields.jpg", scale = 2)
    
    # st_write(CFL_5yrs_sf, "./vr_value/Data/Grid/CFL_5yrs_sf.shp", delete_dsn=TRUE)
    # st_write(fields_sf, "./vr_value/Data/Grid/fields_sf.shp", delete_dsn=TRUE)
  } 
  
  pnt_sf$sample
  info <- cbind(data.table(one_cell.sf) %>% .[,-'geometry'], corn_count_dt)
  # info$sample = pnt_sf$sample[1]
 
  fields_sf2 <- suppressWarnings(sf::st_bind_cols(fields_sf,info))
  
  fields_wgs_sf <- st_transform(fields_sf2, 4326)
  
  setcolorder(fields_wgs_sf, c('id_tile', 'id_10', 'id_field', 'state_name', 'county_name', 'corn_avg_ha','corn5_tile', 'corn5_cell'))
  
  return(fields_wgs_sf)
}

source('./vr_value/Codes/functions_vr.R')

keep <- c('keep', 'CFL_r', 'grid10_tiles.sf', 'tile_n','process_cells', 'st_utm', 'make_squares', 'cl','sample_fields')


clusterExport(cl, varlist = keep, envir=environment())


results_list <- parLapply(cl, ids_10_seq, function(x) process_cells(x))

# fields <- list()
# for(cell_n in ids_10_seq[0:20]){
#   print(cell_n)
#   fields[[cell_n]] <- process_cells(cell_n)
# }

results_list_clean <- results_list[vapply(results_list, Negate(is.null), NA)]

fields_sf <- do.call(what = base::rbind, args = results_list_clean)

stopCluster(cl)
