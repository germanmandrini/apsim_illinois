rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
# library(scales)
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_dynamic_git/Codes/parameters.R'))
"~/n_dynamic/Codes/parameters.R"

grid10_tiles_sf6 <- readRDS("./n_dynamic_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()
yc_yearly_dt3 <- readRDS("./n_dynamic_box/Data/files_rds/yc_yearly_dt3.rds")

grid10_fields_sf2 <- readRDS('./n_dynamic_box/Data/Grid/grid10_fields_sf2.rds')

#======================================================================================
# if(FALSE){
#   # SAMPLE LOCS FOR REGIONAL TESTING
#   # MRTN had 80 locs in IL
#   # Sample 80 fields. Equal number by region. Assume that each field is heavily explored and different soils are mapped and sampled every year.
#   full_fields_dt <- grid10_soils_dt5[,.(long = mean(long),
#                                     lat = mean(lat),
#                                     region = mean(region),
#                                     field_soils_cnt = length(unique(mukey))), by = .(id_10, id_field)]
#   
#   full_fields_dt[,fields_cell := length(unique(id_field)), by = id_10]
#   table(full_fields_dt$id_field)
#   
#   
#   #Choose stations from cells with 4 fields, to avoid loosing field in cells with few fields.
#   # Choose fields with more than 2 soils to get some variability
#   stations_dt <- full_fields_dt[field_soils_cnt > 1 & fields_cell == 4 & id_field %in% c(3,4) ]
#   
#   #Only if 3 and 4 meet the requirements (one could have only 1 soil)
#   stations_dt[,N := .N, by = id_10]
#   stations_dt <- stations_dt[N==2, -'N']
#   
#   # stations_by_region <- round(nrow(stations_dt)*0.05/3,0)
#   stations_by_region <- 40
#   
#   # Also, only one station by id_10
#   set.seed(123)
#   stations_sample_cells_dt <- stations_dt[, .(lat = mean(lat),
#                                               long = mean(long)),by = .(region, id_10)]
#   
#   # Equally distributed by region
#   stations_sample_cells_dt <- stations_sample_cells_dt[order(region, lat)]
#   
#   #Split the fields of each region into the number of stations we are getting by region, and then select one from each group
#   stations_sample_cells_dt[,quantile := cut(lat, quantile(lat, probs = 0:stations_by_region/stations_by_region),
#                     labels = FALSE, include.lowest = TRUE), by = region]
# 
#   stations_sample_cells_dt <- stations_sample_cells_dt[,.SD[sample(.N, 1)],by = .(region, quantile)][,-c('long', 'quantile', 'lat')]
#   
#   #Select fields for each sampled id_10
#   stations_dt2 <- grid10_soils_dt5[id_10 %in% stations_sample_cells_dt$id_10 &
#                                      id_field %in% c(3,4), .(region, id_10, id_field, mukey, lat, long)]
#   
#   stations_dt2[,.N,.(id_10, id_field)][,.N, by = .(id_10)]
#   
#   #-------------------
#   #Remove fields that are stations
#   remove_this <- filter_dt_in_dt(grid10_soils_dt5, filter_dt = unique(stations_dt2[,.(id_10, id_field)]))
#   # remove_this <- c(remove_this, filter_dt_in_dt(fields_dt, filter_dt = data.table(id_10 = 296, id_field = 3)))
#   full_fields_dt2 <- grid10_soils_dt5[-remove_this, .(region, id_10, id_field, mukey, lat, long)]
#   length(unique(full_fields_dt2$id_10))
#   
# 
# }else{
#   reg_model_stuff <- readRDS( "./n_dynamic_box/Data/files_rds/reg_model_stuff.rds")
#   stations_dt2 <- reg_model_stuff[['stations']]
#   full_fields_dt2 <- reg_model_stuff[['full_fields']]
#   model1b_eonr <- reg_model_stuff[['model1b_eonr']]
#   model2b_eonr <- reg_model_stuff[['model2b_eonr']]
#   rm(reg_model_stuff)
# }
#======================================================================================
  if(FALSE){
    fields_cnt <- grid10_soils_dt5[,.N, by = .(id_10, id_field)] %>% .[,.(field_cnt = .N), by = id_10]
    
    grid10_soils_dt5 <- merge(grid10_soils_dt5, fields_cnt, by = 'id_10')
    candidates_dt <- grid10_soils_dt5[field_cnt == 4 & id_field %in% c(1,2)]
    
    #Method 1: stations by region
    if(FALSE){
      set.seed(256)
      stations_dt <- data.table()
      stations_by_region <- 40
      for(region_n in c(1,2,3)){
        # region_n <- 1
        candidates_region_dt <- candidates_dt[region == region_n]
        id_10_sample <- sample(unique(candidates_region_dt$id_10), stations_by_region, replace = F)
        stations_dt <- rbind(stations_dt, candidates_region_dt[id_10 %in% id_10_sample])
      }
    }
    #Method 2: random stations proportional to the number of fields
    set.seed(512)
    stations_by_region_dt <- grid10_soils_dt5[,.N, .(region, id_10, id_field)][,.N, region]
    stations_by_region_dt[,prop := N/sum(N)]
    stations_by_region_dt[,station := round(120*prop)]
    stations_by_region_dt[,sum(station)] == 120
    
    stations_dt <- data.table()
    for(region_n in c(1,2,3)){
      # region_n <- 1
      candidates_region_dt <- candidates_dt[region == region_n]
      amount_of_stations <- stations_by_region_dt[region == region_n]$station
      id_10_sample <- sample(unique(candidates_region_dt$id_10), amount_of_stations, replace = F)
      stations_dt <- rbind(stations_dt, candidates_region_dt[id_10 %in% id_10_sample])
    }
    stations_dt[,.N, region]
    length(unique(stations_dt$id_10))
    #-------------------
    #Remove fields that are stations
    remove_this <- filter_dt_in_dt(grid10_soils_dt5, filter_dt = unique(stations_dt[,.(id_10, id_field)]))
    # remove_this <- c(remove_this, filter_dt_in_dt(fields_dt, filter_dt = data.table(id_10 = 296, id_field = 3)))
    full_fields_dt <- grid10_soils_dt5[-remove_this, .(region, id_10, id_field, mukey, lat, long)]
    length(unique(full_fields_dt$id_10))
  }else{
    reg_model_stuff <- readRDS("./n_dynamic_box/Data/files_rds/reg_model_stuff.rds")
    full_fields_dt <- reg_model_stuff[['full_fields']]
    stations_dt <- reg_model_stuff[['stations']]
    
    rm(reg_model_stuff)
  }
# 
# # candidates_dt2 <- candidates_dt[id_10 %in% unique(stations_dt$id_10) & id_field %in% c(1,2)]
# # stations_dt <- candidates_dt2
# # saveRDS(stations_dt, "./n_dynamic_box/Data/files_rds/stations_lovely.rds")
# 
# # EXPLORE THE DATA-------------------
# Y_corn_explore_dt <- yc_yearly_dt3[,.(Y_corn = mean(Y_corn)), by = z]
# Y_corn_explore_dt[,z:= as.numeric(z)]
# Y_corn_explore_dt[order(z)]
# 
# eonr_explore_dt <- yc_yearly_dt3[, P := Y_corn * Pc - N_fert * Pn] %>% 
#   .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(eonr_explore_dt, 'N_fert', 'eonr')
# 
# eonr_explore_dt[,z:= as.numeric(z)]
# eonr_explore_dt[,.(eonr = mean(eonr)), by = z][order(z)]
# ggplot(eonr_explore_dt) + geom_boxplot(aes(x = factor(z), y = eonr))
# 
# # training_z <- sort(sample(1:30, 10))
# # 
# # training_z <- c(1:10)
# 
# # training_z <- sort(sample(1:30, 15))
# 
# # eonr_explore_dt[,set := ifelse(z %in% training_z, '1training', '2testing')]
# # # eonr_explore_dt[z %in% c(5,10), set := 'discard']
# # eonr_explore_dt[,.(eonr = mean(eonr)), by = set]
# 
# # grid.arrange(
# #   ggplot(eonr_explore_dt) +
# #     geom_boxplot(aes(x = set, y = eonr)),
# #   ggplot(eonr_explore_dt) +
# #     geom_boxplot(aes(x = set, y = Y_corn)),
# #   ggplot(eonr_explore_dt) +
# #     geom_boxplot(aes(x = set, y =  n_deep_v5)), ncol = 2
# # )
# 
# # ggplot(eonr_explore_dt) +
# #   geom_boxplot(aes(x = set, y = eonr))
# 
# # SPLIT THE DATA (TRAINING, VALIDATION)-------------------
# # training_z <- 1:10
# # set.seed(234)
# # training_z <- sort(sample(1:30, 10, replace = F))
# 
# # training_z <- c(1:4, 16:21)
# # training_z <- c(4, 6, 8, 11, 13, 15, 16, 17, 28, 25)
# # training_z <- c(11:20)
# 
# z_odd = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
# z_even = z_odd+1
# 
# indx <- filter_dt_in_dt(yc_yearly_dt3, filter_dt = unique(stations_dt[,.(id_10, mukey)]))
# 
# TrainSet <- yc_yearly_dt3[indx]
# # TrainSet <- TrainSet[z %in% training_z]
# 
# # z_dry <- TrainSet[,.(Y_corn = mean(Y_corn), .N), by = z][Y_corn < 5000]$z
# # TrainSet <- TrainSet[!z %in% z_dry]
# 
# # Filter the right training z, depending on odd or even fields. Remove some z that are coming from fields 1 and 2 that are not RS
# # Also, add the area to the TrainSet2 to do area weighted average of reponse later
# 
# stations_dt[,z_type := ifelse(id_field == 1, 'odd', 'even')]
# TrainSet[,z_type := ifelse(z %in% z_odd, 'odd', 'even')]
# 
# right_mukey_z_combination <- stations_dt[,.(area_ha = mean(area_ha)),.(id_10, mukey, z_type)][, right_comb := 1]
# 
# right_mukey_z_combination[,.N, by = id_10]
# 
# TrainSet2 <- merge(TrainSet, right_mukey_z_combination, all.x = T)
# TrainSet2 <- TrainSet2[!is.na(right_comb)]
# TrainSet2[ ,right_comb := NULL]
# 
# # TrainSet2 <- TrainSet
# 
# TrainSet2[,.N, by = .(id_10, mukey, z)]$N %>% table() #of rates by mukey z, has to be 33
# TrainSet2[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)]$N %>% table() #of z by mukey, has to be 5 or 10
# TrainSet2[,.N, by = id_10] %>% nrow() # of stations, has to be 120
# TrainSet2[,.N, by = .(id_10, mukey)][,.N,by = .(id_10)] # of mukey by station, just exploratory
# 
# TrainSet2_python <- copy(TrainSet2)
# TrainSet2_python[, P := Y_corn * Pc - N_fert * Pn]  #update profits
# TrainSet2_python[,P_max := max(P), by = .(id_10, mukey, z)]
# TrainSet2_python[,P_loss := abs(P - P_max)]
# TrainSet2_python[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
# TrainSet2_python[,L := L1 + L2]
# TrainSet2_python[, Yld_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]
# saveRDS(TrainSet2_python, "./n_dynamic_box/Data/files_rds/TrainSet2_python.rds") #for python
# 
# ValidationSet_python <- yc_yearly_dt3[-indx][z %in% 1:10]
# ValidationSet_python[, P := Y_corn * Pc - N_fert * Pn]  #update profits
# ValidationSet_eonr_python <- ValidationSet_python[, .SD[ P == max(P)], by = .(id_10, mukey,z)] %>% 
#                        .[, .SD[ N_fert == min( N_fert)], by = .(id_10, mukey,z)]
# setnames(ValidationSet_eonr_python, 'N_fert', 'eonr')
# ValidationSet_eonr_python[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
# ValidationSet_eonr_python[,L := L1 + L2]
# ValidationSet_eonr_python[, Yld_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]
# 
# 
# saveRDS(ValidationSet_eonr_python, "./n_dynamic_box/Data/files_rds/ValidationSet_eonr.rds") #for python
# 
# 
# # Plot a relationship
# TrainSet_eonr <- TrainSet2[, P := Y_corn * Pc - N_fert * Pn] %>% 
#   .[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5] %>% 
#   .[, .SD[ P == max( P)], by = .(id_10, mukey, z)] %>%
#   .[, .SD[ N_fert == min(N_fert)], by = .(id_10, mukey, z)]
# setnames(TrainSet_eonr, 'N_fert', 'eonr')
# 
# unique(stations_dt[,.(id_10, id_field, mukey)]) %>% .[,.N, by = .(id_10, id_field)] %>% .[,.(N = mean(N))] #mean number of soils in the stations fields
# #---------------
# #Main relationships plots
# 
# plot_1 <- ggplot(data = TrainSet_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
#   geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
# 
# plot_2 <- ggplot(data = TrainSet_eonr, aes(x = t_min_30, y = eonr)) + 
#   geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
# 
# plot_3 <- ggplot(data = TrainSet_eonr, aes(x = swdef_expan_fw, y = eonr)) + 
#   geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
# 
# plot_4 <- ggplot(data = TrainSet_eonr, aes(x = rain_90, y = eonr)) + 
#   geom_point() + geom_smooth(formula = 'y ~ x', method = lm)
# 
# grid.arrange(plot_1, plot_2, plot_3, plot_4)
# 
# ggsave(plot = grid.arrange(plot_1, plot_2, plot_3, plot_4), 
#        filename = "./n_dynamic_box/Data/figures/rf_main_predictors.jpg", width = 979/300*3, height = 1042/300*3,
#        units = 'in')
# 
# hist(TrainSet_eonr$eonr)
# 
# #---------------
# #Compare soy vs corn yield in mukeys that had the both crops the same year
# soy_dt <- TrainSet_eonr[,.(id_10, mukey, z, Y_soy)]
# soy_dt[,z := as.numeric(z)+1]
# corn_dt <- TrainSet_eonr[,.(id_10, mukey, z, Y_corn)]
# corn_dt[,z := as.numeric(z)]
# yield_test <- merge(soy_dt, corn_dt, by = c('id_10', 'mukey', 'z'))
# ggplot(yield_test, aes(x=Y_soy, y = Y_corn)) + 
#   geom_point()+
#   geom_smooth()
# 
# 
# # Make a Validation set
# # ValidSet <- yc_yearly_dt3[-indx]
# # 
# # sets_rf <- unique(ValidSet[,.(id_10, mukey)])
# # sets_rf <- sets_rf[sample(1:nrow(sets_rf), 100)]
# # ValidSet2 <- filter_dt_in_dt(ValidSet, filter_dt = sets_rf, return_table = T)
# # 
# # ValidSet2_eonr <- ValidSet2[, P := Y_corn * Pc - N_fert * Pn] %>% .[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5] %>% 
# #   .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# # setnames(ValidSet2_eonr, 'N_fert', 'eonr')
# # 
# # ggplot(data = ValidSet2_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
# #   geom_point() + geom_smooth(formula = 'y ~ x', method = lm)

# Save the objects that will be needed later
reg_model_stuff <- list()
# reg_model_stuff[['ValidSet']] <- ValidSet2
# reg_model_stuff[['TrainSet']] <- TrainSet2

#======================================================================================
# EXPLORE STATIONS MAP

full_fields_dt[,rf := 1]
stations_dt[,rs := 1]

regularfields_sf <- dplyr::left_join(grid10_fields_sf2, full_fields_dt[,.(id_10, id_field, rf)], 
                                     by = c('id_10', 'id_field')) %>% dplyr::filter(!is.na(rf)) #%>%dplyr::select(-rf)

stations_sf <- dplyr::left_join(grid10_fields_sf2, stations_dt[,.(id_10, id_field, rs)], 
                                by = c('id_10', 'id_field')) %>% dplyr::filter(!is.na(rs)) #%>% dplyr::select(-rs)

stations_sf %>% data.table() %>% .[,.N, by = .(id_10, id_field)] %>% nrow() == stations_by_region * 6

stations_single_sf <- stations_sf %>% group_by(id_tile, id_10, state_name, county_name) %>% summarise(corn_avg_ha = mean(corn_avg_ha))

# grid10_tiles_sf2 <- grid10_tiles_sf7 %>% mutate(corn_ha_cell = corn5_cell*30*30/10000/11)
# grid10_tiles_sf2$region <- as.character(grid10_tiles_sf2$region)

grid10_region <- grid10_tiles_sf6 %>% group_by(region) %>% summarise()

# #install.packages('smoothr')
library(smoothr)
area_thresh <- units::set_units(10*10+10, km^2)
grid10_region2 <- fill_holes(grid10_region, threshold = area_thresh)
grid10_region2$labels <- c('1-South', '2-Central', '3-North')
# grid10_region_by_hand <- sf::read_sf('./n_dynamic_box/Data/shapefiles/grid10_region_by_hand.shp')
# grid10_region_by_hand <- st_transform(grid10_region_by_hand, crs = st_crs(stations_sf))

(fields_map_clean <- tm_shape(grid10_tiles_sf6) + tm_polygons('region') +
    tm_shape(regularfields_sf) + tm_dots(size = 0.04, col = 'darkblue') +
    tm_shape(stations_single_sf) + tm_dots(size = 0.3, col = 'green', shape = 24) +
    tm_shape(grid10_region2) + tm_borders(lwd = 4, col = 'darkgreen') + tm_text('labels', bg.color='white')+
    tm_layout(legend.text.size = 0.7,
              # main.title = 'Final fields map',
              main.title.position = "center",
              main.title.size = 1)+
    tm_legend(show=FALSE))

tmap_save(fields_map_clean, filename = "./n_dynamic_box/Data/figures/fields_map_and_stations_ch2.pdf", height = 8, width = 6)  

st_write(stations_sf, "./n_dynamic_box/Data/shapefiles/stations_sf.shp", delete_dsn = TRUE)
st_write(regularfields_sf, "./n_dynamic_box/Data/shapefiles/regularfields_sf.shp", delete_dsn = TRUE)

#=======================================================================================
# Map for chapter 1 thesis
grid10_tiles_sf7 <- readRDS("./n_dynamic_box/Data/Grid/grid10_tiles_sf7.rds") 
grid10_region <- grid10_tiles_sf7 %>% group_by(region) %>% summarise()

# #install.packages('smoothr')
library(smoothr)
area_thresh <- units::set_units(10*10+10, km^2)
grid10_region2 <- fill_holes(grid10_region, threshold = area_thresh)

(fields_map_clean <- tm_shape(grid10_tiles_sf7) + tm_polygons('region') +
    tm_shape(grid10_fields_sf2) + tm_dots(size = 0.04, col = 'darkblue') +
    # tm_shape(regularfields_sf) + tm_dots(size = 0.04) +
    tm_shape(grid10_region2) + tm_borders(lwd = 4, col = 'darkgreen') + #tm_text('labels', bg.color='white')+
    tm_layout(legend.text.size = 0.7,
              # main.title = 'Final fields map',
              main.title.position = "center",
              main.title.size = 1))#+
    # tm_legend(show=FALSE))
tmap_save(fields_map_clean, filename = "./n_dynamic_box/Data/figures/fields_map_thesis.pdf", height = 8, width = 6) 

#======================================================================================
# SPLIT THE DATA (TRAINING, VALIDATION)
# training_z <- 1:10
# set.seed(234)
# training_z <- sort(sample(1:30, 10, replace = F))
# # training_z <- c(2,9, 8, 12, 14, 15, 17, 18, 21, 29)
# 
# 
# z_odd = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
# z_even = z_odd+1
# 
# indx <- filter_dt_in_dt(yc_yearly_dt3, filter_dt = unique(stations_dt2[,.(id_10, mukey)]))
# StationsSet <- yc_yearly_dt3[indx]
# ValidSet <- yc_yearly_dt3[-indx]
# 
# sets_rf <- unique(ValidSet[,.(id_10, mukey)])
# sets_rf <- sets_rf[sample(1:nrow(sets_rf), 100)]
# ValidSet2 <- filter_dt_in_dt(ValidSet, filter_dt = sets_rf, return_table = T)
# 
# # Filter the right training z, depending on odd or even fields. Remove some z that are coming from fields 1 and 2 that are not RS
# stations_dt2[,z_type := ifelse(id_field == 3, 'odd', 'even')]
# StationsSet[,z_type := ifelse(z %in% z_odd, 'odd', 'even')]
# StationsSet <- merge(StationsSet, stations_dt2[,.(id_10, mukey, z_type)])
# 
# ValidSet2[,.N, by = .(id_10, mukey,z)][,.N, by = z]
# 
# Y_corn_explore_dt <- StationsSet[,.(Y_corn = mean(Y_corn)), by = z]
# Y_corn_explore_dt[,z:= as.numeric(z)]
# Y_corn_explore_dt <- Y_corn_explore_dt[order(z)]
# 
# ggplot(Y_corn_explore_dt, aes(x = z, y = Y_corn))+
#   geom_bar(stat="identity")
# 
# TrainSet <- StationsSet[z %in% training_z]
# z_dry <- TrainSet[,.(Y_corn = mean(Y_corn), .N), by = z][Y_corn < 5000]$z
# TrainSet <- TrainSet[!z %in% z_dry]
# 
# ValidSet3 <- ValidSet2[!z %in% training_z]
# 
# reg_model_stuff <- list()
# reg_model_stuff[['ValidSet']] <- ValidSet3
# reg_model_stuff[['TrainSet']] <- TrainSet

# =========================================================================================================================================================
# Make a random training set
# sets_rf <- unique(yc_yearly_dt3[,.(id_10, mukey)])
# sets_rf <- sets_rf[sample(1:nrow(sets_rf), 200)]
# TrainSet <- filter_dt_in_dt(yc_yearly_dt3, filter_dt = sets_rf[1:100], return_table = T)
# ValidSet <- filter_dt_in_dt(yc_yearly_dt3, filter_dt = sets_rf[101:200], return_table = T)
# 
# TrainSet <- TrainSet[z %in% training_z]
# ValidSet3 <- ValidSet[!z %in% training_z]
# 
# TrainSet_eonr <- TrainSet[, P := Y_corn * Pc - N_fert * Pn] %>% .[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5] %>% .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(TrainSet_eonr, 'N_fert', 'eonr')
# ValidSet3_eonr <- ValidSet3[, P := Y_corn * Pc - N_fert * Pn] %>% .[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5] %>% .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(ValidSet3_eonr, 'N_fert', 'eonr')
# # =========================================================================================================================================================
# 
# yc_yearly_sample <- yc_yearly_dt3[mukey %in% sample(unique(yc_yearly_dt3$mukey), 100),]
# yc_yearly_sample[, P := Y_corn * Pc - N_fert * Pn]
# yc_yearly_sample[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
# 
# yc_yearly_sample_eonr <- yc_yearly_sample[, .SD[ P == max( P)], by = .(id_10, mukey, z)]
# setnames(yc_yearly_sample_eonr, 'N_fert', 'eonr')
# 
# ggplot(data = yc_yearly_sample_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
#   geom_point() + geom_smooth()
# 
# 
# 
# ggplot(data = TrainSet_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
#   geom_point() + geom_smooth()
# 
# ggplot(data = ValidSet3_eonr, aes(x = n_0_60cm_v5, y = eonr)) + 
#   geom_point() + geom_smooth()

# =========================================================================================================================================================
# CREATE THE REGIONAL MINIMUM MODEL

#Analysis included only responsive sites (sawyer 2006)
# TrainSet2[, Y_corn_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]
# TrainSet_RMM <- TrainSet2[Y_corn_response > 500]
# 
# #Select a few rates
# #Alll this comes from https://rcompanion.org/handbook/I_11.html
# # N_rates_trial <- c(10, 90,170,250, 330)
# N_rates_trial <- seq(10,330,10)
# 
# quadratic_dt <- TrainSet_RMM[,list(intercept=coef(lm(Y_corn~N_fert + I(N_fert^2)))[1], 
#                                 coef1=coef(lm(Y_corn ~ N_fert + I(N_fert^2)))[2],
#                                 coef2=coef(lm(Y_corn ~ N_fert + I(N_fert^2)))[3]),by=.(id_10, mukey,z, region)]
# 
# # Expand and calculate yield
# N_rates_int <- seq(min(N_rates_trial),max(N_rates_trial), by = 10)
# quadratic_dt2 <- quadratic_dt[rep(x = 1:nrow(quadratic_dt), each = length(N_rates_int))]
# 
# 
# 
# quadratic_dt2[,N_fert := rep(N_rates_int, nrow(quadratic_dt))]
# quadratic_dt2[,Y_corn := intercept + coef1 * N_fert + coef2 * (N_fert^2)]
# quadratic_dt2[,P:= Y_corn * Pc - N_fert * Pn]
# 
# #Average all curves
# quadratic_dt3 <- quadratic_dt2[,.(P_avg = mean(P)), by = .(region, N_fert)]
# ggplot(quadratic_dt3) + geom_point(aes(x = N_fert, y = P_avg, colour = interaction(region)))
# 
# #Select EONR
# model_minimum_regional <- quadratic_dt3[, .SD[ P_avg == max( P_avg)], by = .(region)][,.( region, N_fert)]
# setnames(model_minimum_regional, 'N_fert', 'eonr_pred')
# 
# 
# # reg_model_stuff <- readRDS("./n_dynamic_box/Data/files_rds/reg_model_stuff.rds")
# 
# reg_model_stuff[['model_minimum_regional']] <-  model_minimum_regional

# =========================================================================================================================================================
# CREATE THE REGIONAL MINIMUM MODEL OK

#Analysis included only responsive sites (sawyer 2006)
# TrainSet2[, P := Y_corn * Pc - N_fert * Pn] #update P
# TrainSet2[, Y_corn_response := max(Y_corn) - min(Y_corn), by = .(id_10, mukey,z)]
# TrainSet_RMM <- TrainSet2[Y_corn_response > 500]
# 
# #Select a few rates
# #Alll this comes from https://rcompanion.org/handbook/I_11.html
# 
# model_minimum_ok  <- aggregate_by_area(data_dt = TrainSet_RMM, variables = c('P'), 
#                                     weight = 'area_ha', by_c = c('region', 'N_fert')) %>% 
#   .[, .SD[ P == max( P)], by = .(region)] %>% .[,.(region, eonr_pred = N_fert)]
# 
# reg_model_stuff[['minimum_ok']] <-  model_minimum_regional

# =========================================================================================================================================================

reg_model_stuff[['full_fields']] <-  full_fields_dt[,-'rf']
reg_model_stuff[['stations']] <-  stations_dt[,-'rs']


# reg_model_stuff[['trial_rates']] <-  trial_rates

saveRDS(reg_model_stuff, "./n_dynamic_box/Data/files_rds/reg_model_stuff.rds")

