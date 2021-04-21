rm(list=ls())

# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# codes_folder <-'C:/Users/germa/Documents'#Dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_dynamic_git/Codes/parameters.R'))
# source('./Codes_useful/gm_functions.R')


#======================================================================================
#DATA CLEANING & QUALITY CONTROL
if(FALSE){
  # batch_n = 56
  batch_n = 152
  library("foreach")
  library("doParallel")
  print(batch_n)
  multiple_files <- list.files(paste0("./n_dynamic_box/Data/yc_output_summary_", batch_n), full.names = T)
  length(multiple_files)
  #--------------------------------------------------------------------------------------------
  # multiple_files_names <- list.files(paste0("./n_dynamic_box/Data/yc_output_", batch_n), full.names = T)
  # multiple_files_size <- sapply(multiple_files_names, function(file_n) file.size(file_n))
  # multiple_files_dt <- data.table(path = multiple_files_names, size = multiple_files_size)
  # multiple_files_dt[,name := basename(path)]
  # multiple_files_dt[,id_10 := sapply(strsplit(as.character(name), split="_"), "[", 1) ]
  # multiple_files_dt[,mukey := gsub(pattern = '.rds', replacement = '', x = sapply(strsplit(as.character(name), split="_"), "[", 2)) ]
  # multiple_files_dt <- multiple_files_dt[size < 5000]
  # 
  # 
  # paste(multiple_files_dt$id_10, collapse = ', ')
  # paste(multiple_files_dt$mukey, collapse = ', ')
  # 
  # hist(multiple_files_size[multiple_files_size < 5000])
  
  #--------------------------------------------------------------------------------------------
  registerDoParallel(20) # register the cluster
  # registerDoParallel(cores = 10)
  output_list = foreach(file_n = multiple_files, .combine = "c", .packages = c("data.table")) %dopar% {
    # file_n <- multiple_files[1]
    tmp_dt <- readRDS(file_n)
    list(tmp_dt)
  }#end of dopar loop
  
  stopImplicitCluster()
  yc_yearly_dt <- rbindlist(output_list)
  yc_yearly_dt[,id_10 := as.integer(id_10)]
  
  yc_yearly_dt[,.N, by = .(id_10, mukey, z)]$N %>% table() #of rates by mukey z, has to be 33
  yc_yearly_dt[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)]$N %>% table()#of z by mukey, has to be 30 or 15

  yc_yearly_dt[,L := L1 + L2]
  
  saveRDS(yc_yearly_dt, './n_dynamic_box/Data/files_rds/yc_yearly_dt.rds')
  
  yc_yearly_dt[rain_periods != rain_annual ]
}
  yc_yearly_dt <- readRDS('./n_dynamic_box/Data/files_rds/yc_yearly_dt.rds')
  
  length(unique(yc_yearly_dt$id_10))
  
  #======================================================================================
  # ADD REGIONS
  grid10_tiles_sf7 <- readRDS("./n_dynamic_box/Data/Grid/grid10_tiles_sf7.rds") 
  regions_dt <- data.table(grid10_tiles_sf7) %>% .[,.N,.(id_10, region, region_eq)] %>% .[,-'N']
  yc_yearly_dt <- merge(yc_yearly_dt[,-c('region','region_eq')], regions_dt[,.(id_10, region, region_eq)], by = 'id_10')
  setcolorder(yc_yearly_dt, c('id_10', 'region', 'region_eq', 'mukey', 'z', 'year'))
  #------------------------------------------------------------------------------------
  MAKE ESW
  #------------------------------------------------------------------------------------
  # QC
  # Remove low lai_v5 (farmers would not fertilize)
  hist(yc_yearly_dt$lai_v5)
  
  ggplot(yc_yearly_dt, aes(lai_v5 , fill = region_eq    )) + #geom_bar(aes(y = (..count..)/sum(..count..)))+
    geom_histogram(aes( y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]))+
    # labs(x= 'N fert (kg/ha)', y = "relative frequencies")+
    theme_bw()+
    theme(legend.title =  element_blank(),
          axis.text=element_text(size=14))+
    facet_wrap(region_eq~., 
               ncol = 1,
               #scales="free",
               strip.position = "left")
  
  ggplot(yc_yearly_dt[sample(1:nrow(yc_yearly_dt), 10000, replace = F)], aes(x = lai_v5, y = Y_corn)) + 
    geom_point()
  
  yc_yearly_dt2a <- yc_yearly_dt[lai_v5 >= yc_yearly_dt[Y_corn > 0, min(lai_v5)]] #lai thereshold = lowest lai where Y_corn was positive
  # Assumption: if the lai is too low, the farmer will not apply the rate and will plant soybean
  nrow(yc_yearly_dt) - nrow(yc_yearly_dt2a)
  #Dont do it
  yc_yearly_dt2a <- yc_yearly_dt
  #------------------------------------------------------------------------------------
  # Remove weird soils, with high OC or NA n_deep_v5
  ggplot(yc_yearly_dt[oc_20cm_v5 >10], aes(oc_20cm_v5 , fill = region_eq    )) + #geom_bar(aes(y = (..count..)/sum(..count..)))+
    geom_histogram(aes( y=..count../tapply(..count.., ..fill.. ,sum)[..fill..]))+
    # labs(x= 'N fert (kg/ha)', y = "relative frequencies")+
    theme_bw()+
    theme(legend.title =  element_blank(),
          axis.text=element_text(size=14))+
    facet_wrap(region_eq~., 
               ncol = 1,
               #scales="free",
               strip.position = "left")
  
  yc_yearly_dt2b <- yc_yearly_dt2a[oc_20cm_v5 < 10]
  
  summary(yc_yearly_dt2b$n_deep_v5)
  yc_yearly_dt2c <- yc_yearly_dt2b[!is.na(n_deep_v5)]
  summary(yc_yearly_dt2c$whc)
  
  #------------------------------------------------------------------------------------
  #Made it?
  yc_yearly_dt2c[stages_cnt != 12]
  #by cleaning the low yields should be fine
  
  #------------------------------------------------------------------------------------
  #Fix NA values
  # grid10_horizons_v1_dt <- readRDS("./n_dynamic_box/Data/Grid/grid10_horizons_v1_dt.rds")
  # grid10_horizons_v1_dt <- grid10_horizons_v1_dt[bottom <= 200] #make soils to only 150 cm
  
  yc_yearly_dt2c[is.na(sand_40cm)] %>% nrow() == 0
  yc_yearly_dt2c[is.na(clay_40cm)] %>% nrow() == 0
  yc_yearly_dt2c[is.na(om_40cm)] %>% nrow() == 0
  
  #------------------------------------------------------------------------------------
  #Remove low yielding mukeys (farmers will not plant here). Use the N that maximizes P (yield can be too high)
  
  yearly_eonr_dt <- copy(yc_yearly_dt2c) %>% 
    .[, P := Y_corn * Pc - N_fert * Pn] %>% 
    .[, .SD[ P == max( P)], by = .(id_10, mukey, z)]#filter the rate that maximized profits
  
  yearly_eonr_dt2 <- yearly_eonr_dt[,.(Y_corn = mean(Y_corn), 
                                       L = mean(L),
                                       N_fert = mean(N_fert),
                                       Y_soy = mean(Y_soy),
                                 n_deep_v5 = mean(n_deep_v5),
                                 n_deep_v5_max = max(n_deep_v5)), by = .(id_10, mukey)] #z is out
  
  summary(yearly_eonr_dt2$Y_corn)
  summary(yearly_eonr_dt2$L)
  summary(yearly_eonr_dt2$N_fert)
  summary(yearly_eonr_dt2$Y_soy)
  summary(yearly_eonr_dt2$n_deep_v5)
  summary(yearly_eonr_dt2$n_deep_v5_max)
  
  grid.arrange(
    ggplot(yearly_eonr_dt2, aes(Y_corn)) + stat_ecdf(geom = "point"),
    ggplot(yearly_eonr_dt2, aes(L)) + stat_ecdf(geom = "point"),
    ggplot(yearly_eonr_dt2, aes(N_fert)) + stat_ecdf(geom = "point"),
    ggplot(yearly_eonr_dt2, aes(Y_soy)) + stat_ecdf(geom = "point")
  )
  
  ggplot(yearly_eonr_dt2)+geom_point(aes(x = Y_corn, y = Y_soy))+ ggtitle("Soy and Corn correlation")
  
  #Remove mukeys that have low yield (farmers would not plant or APSIM is not simulating well)
  qc_control <- yearly_eonr_dt2[Y_corn < 6000 | L > 125 | N_fert < 60 | Y_soy < 2000]
  
  remove_this <- filter_dt_in_dt(yc_yearly_dt2c, filter_dt = qc_control[,.(id_10, mukey)])
  yc_yearly_dt2d <- yc_yearly_dt2c[-remove_this]
  length(unique(yc_yearly_dt2d$id_10))
  
  nrow(yc_yearly_dt2c) - nrow(yc_yearly_dt2d)
  
  yc_yearly_dt2d[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)]$N %>% table() #of z by mukey, has to be 5 or 10
  
  #------------------------------------------------------------------------------------
  # REMOVE MUKEYS THAT DID NOT HAVE ALL THE RUNS (ROTATIONS OR Z ARE MISSING)
  yc_yearly_dt2d[,.N, by = .(id_10, mukey, z)][N < 25]
  yc_yearly_dt2d[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)][N < 14]
  
  missing_ids_dt <- yc_yearly_dt2d[,.N, by = .(id_10, mukey, z)][N > 25] #at least x rates
  missing_ids_dt2 <- missing_ids_dt[,.N, by = .(id_10, mukey)][N >= 14][,ok := 1] #at least x z
  # missing_ids_dt3 <- missing_ids_dt2[,.N, by = .(id_10, mukey)][N ==2][,-'N'][,ok := 1] #both prev_crop
  
  length(unique(missing_ids_dt$id_10))
  
  yc_yearly_dt3 <- merge(yc_yearly_dt2d, missing_ids_dt2, by =c('id_10', 'mukey'), all.x = T) %>% 
    .[!is.na(ok)] %>% .[,-'ok']
  length(unique(yc_yearly_dt3$id_10))
  yc_yearly_dt3[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)]$N %>% table() #of z by mukey, has to be 5 or 10
  
  #------------------------------------------------------------------------------------
  # CHECK FIELDS AREAS AFTER CLEANING 
  grid10_soils_dt4 <- readRDS("./n_dynamic_box/Data/Grid/grid10_soils_dt4.rds")
  
  simulated_soils_dt <- yc_yearly_dt3[,.N, by = .(id_10, z, mukey)][,-'N']
  z_even = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)
  z_odd = z_even-1
  
  simulated_soils_field1 <- simulated_soils_dt[z %in% z_odd] %>% .[,id_field := 1 ]
  simulated_soils_field3 <- copy(simulated_soils_field1) %>% .[,id_field := 3 ]
  simulated_soils_field2 <- simulated_soils_dt[z %in% z_even] %>% .[,id_field := 2 ]
  simulated_soils_field4 <- copy(simulated_soils_field2) %>% .[,id_field := 4]
  simulated_soils_dt2 <- simulated_soils_field1 %>% 
    rbind(simulated_soils_field2) %>% 
    rbind(simulated_soils_field3) %>% 
    rbind(simulated_soils_field4) %>% .[,.N, by = .(id_10, mukey, id_field)] %>% .[,-'N'] %>%
    .[,run := 'ok']
  simulated_soils_dt2[,id_10:= as.integer(id_10)]
  
  grid10_soils_dt5 <- merge(grid10_soils_dt4, simulated_soils_dt2, by =c('id_10', 'id_field','mukey')) %>% 
    .[,-c('run', 'id10_field_mukey', 'field_area')]
  
  table(grid10_soils_dt5$id_field)
  table(grid10_soils_dt5$mukey_rank)
  
  #Recalculate areas
  grid10_soils_dt5[, field_ha := sum(area_ha), by = .(id_10, id_field)]
  grid10_soils_dt5[, prop_area := area_ha/field_ha]
  grid10_soils_dt5[, field_ha := 40]
  grid10_soils_dt5[,area_ha := field_ha * prop_area]
  
  table(grid10_soils_dt5[,.N, by = .(id_10, id_field, mukey)]$N)
  table(grid10_soils_dt5[,.N, by = .(id_10, id_field)]$N)
  saveRDS(grid10_soils_dt5, "./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds")


# saveRDS(yc_yearly_dt2d, "./n_dynamic_box/Data/files_rds/yc_yearly_dt2d.rds")

#======================================================================================
# GET REGIONS - they will have the same amount of corn cells on them
if(FALSE){
  grid10_tiles_sf7 <- readRDS("./n_dynamic_box/Data/Grid/grid10_tiles_sf7.rds")
  #
  tm_shape(grid10_tiles_sf7) + tm_polygons("county_name")

  # grid10_tiles_sf7 <- st_transform(grid10_tiles_sf7, 4326)
  regions_dt <- data.table(grid10_tiles_sf7[, c('id_10', 'corn_avg_ha')], st_coordinates(st_centroid(grid10_tiles_sf7))) %>% .[,-'geometry']
  setnames(regions_dt, c('X', 'Y'), c('long', 'lat'))
  lat_min <- min(regions_dt$lat)
  lat_max <- max(regions_dt$lat)
  regions_dt[, region_eq := .bincode(regions_dt$lat, breaks=c(lat_min-1, quantile(rep(regions_dt$lat, regions_dt$corn_avg_ha ), c(0.333333, 0.66666)), lat_max +1))]
  regions_dt[,.(corn_avg_ha  = sum(corn_avg_ha )), by = region_eq]
  regions_dt[region_eq == 1, region_eq_lab := '1-South']
  regions_dt[region_eq == 2, region_eq_lab := '2-Central']
  regions_dt[region_eq == 3, region_eq_lab := '3-North']
  regions_dt[,region_eq := NULL]
  setnames(regions_dt, 'region_eq_lab', 'region_eq')
  
  # Add the region to both spatial files
  grid10_tiles_sf7$region_eq <- NULL
  grid10_tiles_sf7 <- merge(grid10_tiles_sf7, regions_dt[,.(id_10, region_eq)], by = 'id_10')
  
  #Clean the region ya que estamos aca
  # grid10_tiles_sf7 <- mutate(grid10_tiles_sf7, region_lab = ifelse(region == 1, '1-South', NA))
  # grid10_tiles_sf7 <- mutate(grid10_tiles_sf7, region_lab = ifelse(region == 2, '2-Central', region_lab))
  # grid10_tiles_sf7 <- mutate(grid10_tiles_sf7, region_lab = ifelse(region == 3, '3-North', region_lab))
  # grid10_tiles_sf7$region <- grid10_tiles_sf7$region_lab
  # grid10_tiles_sf7$region_lab <- NULL
  
  tm_shape(grid10_tiles_sf7) + tm_polygons(c("region", "region_eq"))
  saveRDS(grid10_tiles_sf7, "./n_dynamic_box/Data/Grid/grid10_tiles_sf7.rds") 
  
  # grid10_soils_dt5 <- readRDS("./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds")
  # grid10_soils_dt5[region == 1, region_lab := '1-South']
  # grid10_soils_dt5[region == 2, region_lab := '2-Central']
  # grid10_soils_dt5[region == 3, region_lab := '3-North']
  # grid10_soils_dt5[,region := NULL]
  # setnames(grid10_soils_dt5, 'region_lab', 'region')
  # 
  # grid10_soils_dt5 <- left_join(grid10_soils_dt5[,-'region_eq'], regions_dt[,.(id_10, region_eq)], by = 'id_10')
  # saveRDS(grid10_soils_dt5, "./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds")
  
  
  grid10_soils_dt4 <- readRDS("./n_dynamic_box/Data/Grid/grid10_soils_dt4.rds")
  # grid10_soils_dt4[region == 1, region_lab := '1-South']
  # grid10_soils_dt4[region == 2, region_lab := '2-Central']
  # grid10_soils_dt4[region == 3, region_lab := '3-North']
  # grid10_soils_dt4[,region := NULL]
  # setnames(grid10_soils_dt4, 'region_lab', 'region')
  
  grid10_soils_dt4 <- left_join(grid10_soils_dt4[,-'region_eq'], regions_dt[,.(id_10, region_eq)], by = 'id_10')
  saveRDS(grid10_soils_dt4, "./n_dynamic_box/Data/Grid/grid10_soils_dt4.rds")
#   #----------
#   # Fix the file by hand and load again
  # grid10_region_df <- dplyr::select(grid10_tiles_sf7, id_10, corn_avg_ha) %>%
  #   merge(regions_dt[,.(id_10, region_us)], by = 'id_10') %>%
  #   group_by(region_us) %>% summarize()
  # 
  #     #install.packages('smoothr')
  #     library(smoothr)
  #     area_thresh <- units::set_units(10*10+10, km^2)
  #     grid10_region_df <- fill_holes(grid10_region_df, threshold = area_thresh)
  #     st_write(grid10_region_df, "./n_dynamic_box/Data/shapefiles/grid10_region_df.shp", delete_dsn = TRUE)
# 
#   grid10_region_by_hand <- sf::read_sf('./n_dynamic_box/Data/shapefiles/grid10_region_by_hand.shp')
#   grid10_region_by_hand <- st_transform(grid10_region_by_hand, crs = st_crs(grid10_tiles_sf5))
#   tm_shape(grid10_region_by_hand) + tm_polygons('region')
#   
#   # Add the region to both spatial files
#   grid10_tiles_sf6 <- st_join(grid10_tiles_sf5, grid10_region_by_hand, join = st_intersects, largest = T)
#   tm_shape(grid10_tiles_sf6) + tm_polygons('region')
#   saveRDS(grid10_tiles_sf6, "./n_dynamic_box/Data/Grid/grid10_tiles_sf6.rds") 
#   regions_dt <- data.table(grid10_tiles_sf6) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
#   grid10_soils_dt5 <- left_join(grid10_soils_dt5[,-'region'], regions_dt[,.(id_10, region)], by = 'id_10')
#   table(grid10_soils_dt5$region)
#   saveRDS(grid10_soils_dt5, "./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds")
#   
#   yc_yearly_dt3 <- merge(yc_yearly_dt3, regions_dt, by = 'id_10')
#   
}
# 
# grid10_tiles_sf6 <- readRDS("./n_dynamic_box/Data/Grid/grid10_tiles_sf6.rds") 
# grid10_soils_dt5 <- readRDS("./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds") 
# 
# tm_shape(grid10_tiles_sf6) + tm_polygons("region")

#======================================================================================
# ADD LONG TERM YIELD
long_term_Y_corn_dt <- yc_yearly_dt3[,.(Y_corn = max(Y_corn)), by = .(id_10, mukey,z)] %>%
                                 .[, .(Y_corn_lt_avg = mean(Y_corn),
                                       Y_corn_lt_min = min(Y_corn),
                                       Y_corn_lt_max = max(Y_corn)), by = .(id_10, mukey)]
hist(long_term_Y_corn_dt$Y_corn_lt_avg)
# eonr_mukey_dt2 <- merge(eonr_mukey_dt2, long_term_Y_corn_dt, by = c('id_10', 'mukey'))
# saveRDS(eonr_mukey_dt2, "./n_dynamic_box/Data/files_rds/eonr_mukey_dt2.rds")

yc_yearly_dt3 <- merge(yc_yearly_dt3, long_term_Y_corn_dt, by = c('id_10', 'mukey'))

yc_yearly_dt3 <- yc_yearly_dt3[,-c('sim_name', 'year') ]

yc_yearly_dt3[,.N, by = .(id_10, mukey)]

yc_yearly_dt3[,.N, by = .(id_10, mukey, z)]$N %>% table() #of rates by mukey z, has to be 33
yc_yearly_dt3[,.N, by = .(id_10, mukey, z)][,.N, by = .(id_10, mukey)]$N %>% table()#of z by mukey, has to be 30 or 15


saveRDS(yc_yearly_dt3, "./n_dynamic_box/Data/files_rds/yc_yearly_dt3.rds")

# region_link <- data.table(grid10_soils_sf6) %>% .[,.(id_10, region)] %>% unique()
# full_fields_dt2 <- merge(full_fields_dt2, region_link, by = 'id_10')
# saveRDS(full_fields_dt2, "./n_dynamic_box/Data/files_rds/full_fields_dt2.rds")
#======================================================================================
# MAKE THREE REGIONAL SOILS
grid10_soils_dt5 <- readRDS("./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds")
grid10_horizons_v1_dt <- readRDS("./n_dynamic_box/Data/Grid/grid10_horizons_v1_dt.rds")


regions_dt <- grid10_soils_dt5[,.N, .(mukey, region)]
regions_dt[,N := .N, .(mukey)]
regions_dt <- regions_dt[N==1][,-'N']# remove mukeys repeated in regions

average_regions_soils_dt <- merge(grid10_horizons_v1_dt, regions_dt, by = 'mukey')
average_regions_soils_dt <- average_regions_soils_dt[,names(average_regions_soils_dt):= lapply(.SD, as.numeric), .SDcols = names(average_regions_soils_dt)]
average_regions_soils_dt <- average_regions_soils_dt[, lapply(.SD, mean, na.rm = T), by = .(region, layer)]
average_regions_soils_dt[,mukey := region]
saveRDS(average_regions_soils_dt, "./n_dynamic_box/Data/Grid/average_regions_soils_dt.rds")

#======================================================================================
# Water-table by county (NOT USED)
tiles_county_dt <- read.csv('./n_dynamic_box/Data/tiledrains_county.csv', header = T) %>% data.table() %>% 
  .[,.(county_name, prop_drain = Proportion)] %>% .[order(prop_drain)]

fields_dt <- grid10_soils_dt5[,.N, by = .(county_name, id_10, id_field)][,-'N'] 

counties_list <- sort(unique(fields_dt$county_name))
fields_dt2 <- data.table()
for(county_n in counties_list){
  # county_n <- counties_list[50]
  
  fields_tmp_dt <- fields_dt[county_name == county_n]
  how_many <- round(nrow(fields_tmp_dt) * tiles_county_dt[county_name == county_n]$prop_drain)
  fields_tmp_dt[sample(1:nrow(fields_tmp_dt), how_many), drain := 1]
  fields_tmp_dt[is.na(drain), drain := 0]
  fields_dt2 <-rbind(fields_dt2, fields_tmp_dt)
  
}

grid10_soils_dt5 <- merge(grid10_soils_dt5[,-'tiles'], fields_dt2, by = c('id_10', 'id_field', 'county_name'))
table(grid10_soils_dt5$drain)

grid10_tiles_sf6 <- readRDS("./n_dynamic_box/Data/Grid/grid10_tiles_sf6.rds") 
plot_sf <- merge(grid10_tiles_sf6, tiles_county_dt, by = c('county_name'))
tm_shape(plot_sf) + tm_polygons("prop_drain", n = 10)

