rm(list=ls())

# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/apsim_illinois_git/Codes/parameters.R'))

#======================================================================================
library("foreach")
library("doParallel")
#Merge all files
# YC OUTPUT EVALUATION
#MERGE FILES
multiple_files <- list.files("./apsim_illinois_box/Data/yc_output_55", full.names = T)
grid10_soils_dt5 <- readRDS("./apsim_illinois_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
grid10_soils_dt5[id_field %in% c(1,3), type := 'odd']
grid10_soils_dt5[id_field %in% c(2,4), type := 'even']
mukeys_dt <- grid10_soils_dt5[,.(area_ha = sum(area_ha)), by = .(id_10, mukey, type)]

#select the mukeys that are repeated in both types of fields
mukeys_dt2a <- mukeys_dt[,.(.N, 
                            area_ha = sum(area_ha)), by = .(id_10, mukey)][N > 1][,.SD[area_ha == max(area_ha)], by = id_10][,-'N']

#for those that do not have repeated, select the larger mukey of each type
mukeys_dt2b <-mukeys_dt[!id_10 %in% mukeys_dt2a$id_10][,.SD[area_ha == max(area_ha)], by = .(id_10, type)][,-'type']

mukeys_dt3 <- rbind(mukeys_dt2a, mukeys_dt2b)

mukeys_dt3[,file_name := paste0(id_10, '_',mukey, '.rds')]

multiple_files_dt <- data.table(path = multiple_files,
                                file_name = basename(multiple_files),
                                id_10 = sapply(strsplit(as.character(basename(multiple_files)), split="_"), "[", 1))
multiple_files_dt <- multiple_files_dt[id_10 %in% c(1128, 1198)]
multiple_files <- multiple_files_dt[file_name %in% mukeys_dt3$file_name ]$path

length(multiple_files)

registerDoParallel(2) # register the cluster
# registerDoParallel(cores = 10)
output_list = foreach(file_n = multiple_files, .combine = "c", .packages = c("data.table")) %dopar% {
  # file_n <- multiple_files[1]
  tmp_dt <- readRDS(file_n)
  names(tmp_dt)
  tmp_dt <- tmp_dt[year == 2010 & month %in% 3:5,.(sim_name, id_10, mukey, z, Date, year, month, day, st_3)]
  
  tmp_dt[,N_fert := lapply(strsplit(as.character(sim_name), split="_"), "[", 5) ]
  tmp_dt <- tmp_dt[N_fert == 260]
  tmp_dt[, soilt10d := rollapplyr(st_3, 1:10, mean), by =  sim_name]
  tmp_dt[, soilt15d := rollapplyr(st_3, 1:15, mean), by =  sim_name]
  tmp_dt[,.N, .(sim_name)]
  tmp_dt <- tmp_dt[,.(st_3 = mean(st_3),
                        soilt10d = mean(soilt10d),
                      soilt15d = mean(soilt15d)), by = .(id_10, mukey, z, Date, year, month, day)] #mukey is out
  list(tmp_dt)
}#end of dopar loop

stopImplicitCluster()
plant_dates_output2 <- rbindlist(output_list)
saveRDS(plant_dates_output, "./apsim_illinois_box/Data/files_rds/plant_dates_output.rds") 
# ---------------------------------------------------------------------
plant_dates_output <- readRDS( "./apsim_illinois_box/Data/files_rds/plant_dates_output.rds") 
plant_dates_output <- plant_dates_output[!id_10 %in% c(1128, 1198)]
plant_dates_output <- rbind(plant_dates_output, plant_dates_output2)
# #Add latitude
# grid10_tiles_sf6 <- readRDS("./apsim_illinois_box/Data/Grid/grid10_tiles_sf6.rds") 
# grid10_tiles_sf6 <- st_transform(grid10_tiles_sf6, 4326)
# coord_dt <- data.table(st_coordinates(st_centroid(grid10_tiles_sf6)))
# grid10_tiles_dt <- data.table(grid10_tiles_sf6)
# grid10_tiles_dt <- cbind(grid10_tiles_dt[,-c('lat')], coord_dt[,.(lat=Y, long = X)])
# grid10_tiles_dt[,id_10 := as.character(id_10)]
# plant_dates_output2 <- merge(plant_dates_output, grid10_tiles_dt[,.(region, id_10, lat, long)])

plant_dates_output <- plant_dates_output[,.(st_3 = mean(st_3),
                                              soilt10d = mean(soilt10d),
                                              soilt15d = mean(soilt15d)), by = .(id_10, z, Date, year, month, day)] #mukey is out

# Select the first day with soil temp > thershold
# plant_dates_output2 <-plant_dates_output[order(-lat)]
plant_dates_output2 <- plant_dates_output[soilt10d > 10][,.SD[1], .(id_10,z)]
setnames(plant_dates_output2, 'day', 'pd_soil')
# plant_dates_output2[,.(pd_soil = mean(pd_soil)), region]

#======================================================================================
# Get last frost date. This would be the planting date, considering that it toletates until v5
weather_historic_dt <- readRDS('./apsim_illinois_box/Data/met_files/weather_historic_dt2019.rds')
weather_historic_dt <- weather_historic_dt[day > 10 & day < 120 & mint < 0]
weather_historic_dt2 <- weather_historic_dt[,.SD[day == max(day)], by = .(id_10, year)]
weather_historic_dt3 <- weather_historic_dt2[,.(pd_frost = round(mean(day),0),
                                                pd_frost_max = round(max(day),0)), id_10]

#------------------------------------------------------------------------------
#Beautiful map
grid10_tiles_sf6 <- readRDS("./apsim_illinois_box/Data/Grid/grid10_tiles_sf6.rds") 
planting_dates_sf <- left_join(grid10_tiles_sf6, weather_historic_dt3, by = 'id_10')

(frost_map <- tm_shape(planting_dates_sf) + tm_polygons('pd_frost', title = 'Last frost (Julian date)') +
    tm_layout(legend.text.size = 0.7,
              # main.title = 'Final fields map',
              # main.title.position = "center",
              main.title.size = 1))

tmap_save(frost_map, filename = "./apsim_illinois_box/Data/figures/frost_map.pdf", height = 8, width = 6)  

#======================================================================================
# Create a planting date going from south to north
weather_historic_dt3[,id_10 := as.character(id_10)]
plant_dates_dt <- merge(plant_dates_output2[,.(id_10, z, year, month, pd_soil)], weather_historic_dt3[,.(id_10, pd_frost)], by = 'id_10')
plant_dates_dt[pd_soil >= pd_frost, p_date_corn := pd_soil]
plant_dates_dt[pd_frost > pd_soil, p_date_corn := pd_frost]
plant_dates_dt[pd_soil >= pd_frost]

plant_dates_dt[,hybrid_rm := '110']
plant_dates_dt[p_date_corn > 105, hybrid_rm := '105']
plant_dates_dt[p_date_corn < 100,hybrid_rm := '115']
table(plant_dates_dt$hybrid_rm)

#------------------------------------------------------------------------------
#make a map of hybrid rm
grid10_tiles_sf6$id_10 <- as.character(grid10_tiles_sf6$id_10)
dates_dt <- plant_dates_dt[,.(hybrid_rm = mean(as.numeric(hybrid_rm)), 
                              p_date_corn = mean(p_date_corn)), by = id_10]
planting_dates_sf <- left_join(grid10_tiles_sf6, dates_dt, by = 'id_10')
tm_shape(planting_dates_sf)+tm_polygons(c('hybrid_rm', 'p_date_corn'))

#------------------------------------------------------------------------------
#Soybean date and gm
plant_dates_dt[, p_date_soy := p_date_corn+25]
plant_dates_dt[,soy_mg := 2]
plant_dates_dt[p_date_corn > 106,soy_mg := 1]
plant_dates_dt[p_date_corn < 101,soy_mg := 3]

#------------------------------------------------------------------------------
#make a map of soybean mg
grid10_tiles_sf6$id_10 <- as.character(grid10_tiles_sf6$id_10)
dates_dt <- plant_dates_dt[,.(soy_mg = mean(as.numeric(soy_mg)), 
                              p_date_soy = mean(p_date_soy)), by = id_10]
planting_dates_sf <- left_join(grid10_tiles_sf6, dates_dt, by = 'id_10')
tm_shape(planting_dates_sf)+tm_polygons(c('soy_mg', 'p_date_soy'))

#------------------------------------------------------------------------------

plant_dates_dt[,p_date_corn2 := as.Date(p_date_corn, origin=as.Date("2010-01-01"))]
plant_dates_dt[,p_date_corn2 := format(p_date_corn2,"%d-%b")]

plant_dates_dt[,p_date_soy2 := as.Date(p_date_soy, origin=as.Date("2010-01-01"))]
plant_dates_dt[,p_date_soy2 := format(p_date_soy2,"%d-%b")]

plant_dates_dt[pd_soil> pd_frost]

saveRDS(plant_dates_dt, "./apsim_illinois_box/Data/files_rds/plant_dates_dt.rds")

#------------------------------------------------------------------------------
#Go to the R in CPSC and save it there to avoid the error in the cluster
plant_dates_dt <- readRDS("C:/Users/germanm2/Box Sync/My_Documents/apsim_illinois_box/Data/files_rds/plant_dates_dt.rds")
saveRDS(plant_dates_dt, "C:/Users/germanm2/Box Sync/My_Documents/apsim_illinois_box/Data/files_rds/plant_dates_dt.rds")

plant_dates_dt[p_date_corn == min(p_date_corn)][1]
plant_dates_dt[p_date_corn == 100][1]
plant_dates_dt[p_date_corn == 105][1]
plant_dates_dt[p_date_corn == max(p_date_corn)][1]
