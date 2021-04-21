# Load libraries and functions #################################################
library(maps)
library(maptools)


setwd('C:/Users/germa/Box Sync/My_Documents') #dell
setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

grid10_fields_sf <- readRDS('./n_policy_box/Data/Grid/grid10_fields_sf.rds')

# source("R/moldSSURGO_gm.R")

#---------------------------------------------------------------
# Step 1 Get the soils for each field (MAKE IT BETTER, SEQUENTIAL (PARALELL DOESN'T WORK, NOT BY TILE, PUT WHILE STATEMENT AND A TRY))

all_soils <- list()

grid10_soils_list <- list()

for(tile_n in c(22,23)){
  # tile_n = 1
  print(tile_n)
  
  one_tile_sf <- grid10_fields_sf[grid10_fields_sf$id_tile == tile_n,]
  nrow(one_tile_sf)
  # if(nrow(one_tile_sf) >10){one_tile_sf <- one_tile_sf[1:10,]}
  
  source('./n_policy_box/Data/APssurgo_master/R/get_soils_parallel.R')

  grid10_soils_list[[tile_n]] <- fields_sf # rerun 5
}

length(grid10_soils_list)

sapply(grid10_soils_list, nrow)
saveRDS(grid10_soils_list, "./n_policy_box/Data/Grid/grid10_soils_list.rds")


#---------------------------------------------------------------
# Merge and re-run missing fields as many t to get all of them
grid10_soils_sf1 <- do.call(what = base::rbind, args = grid10_soils_list)

obtained_dt <- data.table(grid10_soils_sf2) %>% .[,.N, by = .(id_tile, id_10, id_field)] %>% .[,-'N'] %>% .[,ok := 1]
missing_sf <- left_join(grid10_fields_sf, obtained_dt, by = c('id_tile', 'id_10', 'id_field'))
missing_sf <- missing_sf[is.na(missing_sf$ok),] %>% dplyr::select(-ok)

source('./n_policy_box/Data/APssurgo_master/R/get_soils_seq.R')

results_list <- list()
for(field_n in 1:nrow(missing_sf)){
  # field_n = 1
  print(field_n)
  results_list[[field_n]] <- get_soils(field_n)
}
length(results_list)
results_list_clean <- results_list[vapply(results_list, Negate(is.null), NA)]
fields_sf <- do.call(what = base::rbind, args = results_list_clean)
rownames(fields_sf) <- 1:nrow(fields_sf)


nrow(grid10_soils_sf1)

grid10_soils_sf2 <- rbind(grid10_soils_sf1, fields_sf)
nrow(grid10_soils_sf2)
grid10_soils_sf2 <- unique(grid10_soils_sf2)

grid10_soils_list[[length(grid10_soils_list)+1]] <- dplyr::select(fields_sf, -ok)
  
saveRDS(grid10_soils_sf2, "./n_policy_box/Data/Grid/grid10_soils_sf2.rds")
grid10_soils_sf2 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_sf2.rds")

#---------------------------------------------------------------  
# Clean the soils (replace small areas by biggest)
source('./n_policy_git/Codes/clean_fields_step1.R')
grid10_soils_dt3 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt3.rds")
length(unique(grid10_soils_dt3$mukey)) #total soils to download

#---------------------------------------------------------------  
# Step 2 get the horizons information
source('./n_policy_box/Data/APssurgo_master/R/get_horizons_parallel.R')
grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v1_dt.rds")


# Clean the soils not available in SSURGO
source('./n_policy_git/Codes/clean_fields_step2.R')
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
grid10_fields_sf2 <- readRDS("./n_policy_box/Data/Grid/grid10_fields_sf2.rds")

#---------------------------------------------------------------
# Step 3 Add more information and save
# info <- data.table(grid10_fields_sf2, st_coordinates(st_centroid(grid10_fields_sf2))) %>% .[, .(id_tile, id_cell, id_field, X, Y)]
# info <- info[,.(id_tile = min(id_tile), X = mean(X), Y = mean(Y)), by = .(mukey)] #remove repeated mukeys in same tile

# Add the id_tile to make folders based on it. If a mukey is in more than 1 tile, it will be located in the lower id_tile
# grid10_horizons_v2_dt <- merge(info, grid10_horizons_v1_dt, by = 'mukey')
# setcolorder(grid10_horizons_v2_dt, c('id_tile','mukey', 'X', 'Y'))
# saveRDS(grid10_horizons_v2_dt, "./n_policy_box/Data/Grid/grid10_horizons_v2_dt.rds")
# grid10_horizons_v2_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v2_dt.rds")
#---------------------------------------------------------------
# Create one main soil for each region
regions_dt <- grid10_soils_dt4[,.(region, mukey)] %>%unique() %>% data.table()
grid10_horizons_v1_dt <- merge(grid10_horizons_v1_dt, regions_dt, by = 'mukey')

regions_horizons_dt <- grid10_horizons_v1_dt[, lapply(.SD, mean, na.rm=TRUE), by= .(region, top, bottom, thick, center)]
saveRDS(regions_horizons_dt, "./n_policy_box/Data/Grid/regions_horizons_dt.rds")

#---------------------------------------------------------------
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")

grid10_soils_dt4_old <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4_old.rds")
grid10_soils_dt4_old[,id10_field_mukey := paste(id_10, id_field, mukey, sep = '_')]

grid10_soils_dt4 <- grid10_soils_dt4[!id10_field_mukey %in% unique(grid10_soils_dt4_old$id10_field_mukey)]
saveRDS(grid10_soils_dt4, "./n_policy_box/Data/Grid/grid10_soils_dt4_difference.rds")
