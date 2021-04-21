# library(data.table)
library(dplyr)
library(parallel)
library(XML)

grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")

# sample(grid10_soils_dt4[region == 3]$id_10,3)
grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/grid10_horizons_v1_dt.rds")
grid10_horizons_v1_dt <- grid10_horizons_v1_dt[bottom <= 200] #make soils to only 150 cm

#---------------------------------------------------------
if(FALSE){
# Explore soils
  library(ggplot2)
  regions_dt <- grid10_soils_dt4[,.(region = round(mean(region),0)), by = mukey]
  grid10_horizons_v1_dt2 <- merge(grid10_horizons_v1_dt, regions_dt, by = 'mukey')
  
  grid10_horizons_v1_dt2[,region := factor(region)]
  
  (plot_1 <- ggplot(grid10_horizons_v1_dt2[restriction<200], aes(restriction , fill = region)) + #geom_bar(aes(y = (..count..)/sum(..count..)))+
      geom_histogram()+
      facet_wrap(region~., 
                 ncol =1 ,
                 #scales="free",
                 strip.position = "left"))
  
  grid10_horizons_v1_dt2[,rest_count := ifelse(restriction < 200, 1, 0)]
  grid10_horizons_v1_dt2[, .(rest_count = sum(rest_count)/.N), by = region]
  
  # hist(grid10_horizons_v1_dt$watertable)
}

if(FALSE){ #test if regions are correct
  regions1 <- unique(grid10_soils_dt4[,.(id_10, region)])
  regions2 <- data.table(grid10_fields_sf2) %>% .[,.(id_10, region)] %>% unique()
  comp_dt <- merge(regions1, regions2, by = 'id_10')
  comp_dt[region.x != region.y] # has to be empty
  grid10_tiles_sf6 <- readRDS("./n_policy_box/Data/Grid/grid10_tiles_sf6.rds")
  tm_shape(grid10_tiles_sf6)+ tm_polygons('region')
  regions3 <- data.table(grid10_tiles_sf6) %>% .[,.(id_10, region)] %>% unique()
  comp_dt <- merge(comp_dt, regions3, by = 'id_10')
  comp_dt[region.x != region]# has to be empty
}

if(server){
  directory <- paste('/home/germanm2/apsim_temp/n_policy/batch_', batch_n, '/cell', id10_n, sep = '')
}else if(cpsc){
  directory <- paste('C:/apsim_temp/', Sys.info()["nodename"],'/n_policy/batch_', batch_n, '/cell', id10_n, sep = '')
}else if(cluster){
  directory <- paste('/projects/aces/germanm2/n_policy/batch_', batch_n, '/cell', id10_n, sep = '')
  # directory <- paste('/home/germanm2/scratch/apsim_temp/batch_', batch_n, '/cell', id10_n, sep = '')
  # directory <- paste('/projects/aces/germanm2/scratch/batch_', batch_n, '/cell', id10_n, sep = '')
}

unlink(directory ,recursive=TRUE)

one_cell_dt <- grid10_soils_dt4[id_10 == id10_n,]

#----------------------------------------------------------------------------
#OJO!!!
# Select largest mukey
if(FALSE & test_small){
  one_cell_dt <- one_cell_dt[ area_ha == max( area_ha)][1] 
}

# Select largest mukey by field
if(FALSE & server & !regional_soils){
  one_cell_dt <- one_cell_dt[,.SD[prop_area == max(prop_area)], by = id_field]
}
if(TRUE & server){
  one_cell_dt <- one_cell_dt[ area_ha == max( area_ha)][1] 
  one_cell_dt[,id_field := 1]
}

#----------------------------------------------------------------------------
grid10_fields_sf2 <- readRDS("./n_policy_box/Data/Grid/grid10_fields_sf2.rds")
cell_coords <- data.table(grid10_fields_sf2[grid10_fields_sf2$id_10 == id10_n,]) %>% .[,.(X = mean(long), Y = mean(lat))]

#----------------------------------------------------------------------------
# WEATHER FILES
source(paste0(codes_folder, '/n_policy_git/Codes/simC_make_z_and_met_files.R'))
"C:/Users/germanm2/Documents/n_policy_git/Codes/simC_make_z_and_met_files.R"
"./n_policy_git/Codes/simC_make_z_and_met_files.R"

#----------------------------------------------------------------------------
# Get the regional soils
if(server & regional_soils){
  # one_cell_dt <- one_cell_dt[c(1,1),]
  # one_cell_dt[,id_field := c(1,2)]
  one_cell_dt <- one_cell_dt[1,]
  one_cell_dt[,id_field := 1]
  one_cell_dt[,mukey := region]
  grid10_horizons_v1_dt <- readRDS("./n_policy_box/Data/Grid/average_regions_soils_dt.rds")
  grid10_horizons_v1_dt <- grid10_horizons_v1_dt[bottom <=200] #make soils to only 150 cm
}

#----------------------------------------------------------------------------
# CREATE SOIL FILES
source(paste0(codes_folder, '/n_policy_git/APssurgo_master/calc_apsim_variables_onesoil.R'))
'./n_policy_git/APssurgo_master/calc_apsim_variables_onesoil.R'
"C:/Users/germanm2/Documents/n_policy_git/APssurgo_master/calc_apsim_variables_onesoil.R"

source(paste0(codes_folder, '/n_policy_git/APssurgo_master/make_apsoils_toolbox.R'))
'./n_policy_git/APssurgo_master/make_apsoils_toolbox.R'
"C:/Users/germanm2/Documents/n_policy_git/APssurgo_master/make_apsoils_toolbox.R"
region_n = one_cell_dt$region[1]

horizons_cell_dt <- grid10_horizons_v1_dt[mukey %in% one_cell_dt$mukey,]
horizons_cell_dt[is.na(ph), ph := 6] #a few soils didn't have ph and apsim doesn't use it
horizons_cell2_dt <- calc_apsim_variables(horizons_cell_dt, region_n)
# horizons_cell2_dt[bottom >= restriction, XF_maize := 0] #limit the depth of the soil to the restriction

horizons_cell2_dt <- cbind(horizons_cell2_dt,cell_coords)
make_apsoils_toolbox(data_soils = horizons_cell2_dt, badge_name = 'soils_vr_value', path = directory, crops = tolower(c("Maize","Soybean")))

#----------------------------------------------------------------------------
# CREATE THE INSTRUCTIONS FOR THE STABILIZATION PERIOD 
is_even <- function(x) x %% 2 == 0
z_seq <- unique(weather_cell.dt$z)
z_even = z_seq[is_even(z_seq)]
z_odd = z_seq[!is_even(z_seq)]

if(any(one_cell_dt$id_field %in% c(1,3))){
  instructions1 <- data.table(id_10 = id10_n,
                              region = one_cell_dt$region[1],
                              expand.grid(z = z_odd,
                                          mukey = sort(unique(one_cell_dt[id_field %in% c(1,3)]$mukey)),
                                          stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE) 
}else{instructions1 <- data.table()}


if(any(one_cell_dt$id_field %in% c(2,4))){
  instructions2 <- data.table(id_10 = id10_n,
                              region = one_cell_dt$region[1],
                              expand.grid(z = z_even,
                                          mukey = sort(unique(one_cell_dt[id_field %in% c(2,4)]$mukey)),
                                          stringsAsFactors = FALSE),
                              stringsAsFactors = FALSE) 
}else{instructions2 <- data.table()}

instructions <- rbind(instructions1, instructions2) %>% setcolorder(c('id_10',  'mukey', 'z'))
instructions <- merge(instructions, horizons_cell2_dt[, .(watertable = mean(watertable)), by = mukey], by = 'mukey')
instructions[,batch := batch_n]
instructions[,water := water_n]

#---------------------------------------------------------------
# N by region, and same by field and z combination
set.seed(1)
z_count <- length(unique(instructions$z))
n_target_vector <- list(sample(1:60, z_count, replace = T), #South
                        sample(1:60, z_count, replace = T),  #Central
                        sample(1:60, z_count, replace = T))[[region_n]] #North

n_target_vector <- list(sample(c(1:10,50:60), z_count, replace = T), #South
                        sample(c(1:20,40:60), z_count, replace = T),  #Central
                        sample(c(1:20,50:66), z_count, replace = T))[[region_n]] #North

n_target_dt <- data.table(z = unique(instructions$z),
                          n_target= n_target_vector)
instructions <- merge(instructions, n_target_dt, by = 'z')
#---------------------------------------------------------------
# if(regional_test) {instructions <- instructions[z %in% c(2,3,6,7,13,14,15,16,24,25,28,29)]}
if(test_small) {instructions <- instructions[1,]}
if(FALSE) {instructions <- instructions[z==23,]}
print(instructions )
"C:/Users/germanm2/Documents/n_policy_git/Codes/simD_create_apsim_files.R"
"./n_policy_git/Codes/simD_create_apsim_files.R"
source(paste0(codes_folder, '/n_policy_git/Codes/simD_create_apsim_files.R'))


