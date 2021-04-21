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

library("foreach")
library("doParallel")
# library(randomForest)
# library(mlr)
# eonr_mukey_dt3 <- readRDS("./n_dynamic_box/Data/files_rds/eonr_mukey_dt3.rds")
# yc_yearly_dt <- readRDS("./n_dynamic_box/Data/files_rds/yc_yearly_dt.rds")  
yc_yearly_dt3 <- readRDS("./n_dynamic_box/Data/files_rds/yc_yearly_dt3.rds")
# grid10_tiles_sf6 <- readRDS("./n_dynamic_box/Data/Grid/grid10_tiles_sf6.rds") 
grid10_soils_dt5 <- readRDS("./n_dynamic_box/Data/Grid/grid10_soils_dt5.rds") %>% data.table()


# grid10_fields_sf2 <- readRDS('./n_dynamic_box/Data/Grid/grid10_fields_sf2.rds')
# grid10_soils_sf2 <- readRDS('./n_dynamic_box/Data/Grid/grid10_soils_sf2.rds')

names(reg_model_stuff)

#======================================================================================
# GET THE FIELDS THAT CAN BE RUN
# stations_dt2 <- reg_model_stuff2$stations
fields_list_dt <- grid10_soils_dt5[, .(region, region_eq, id_10, id_field)] %>% unique()#one row by field

# ----------------

# crop_varb <- reg_model_stuff$crop_varb

process_field <- function(j){
  # j = 31
  print(j)
  field_info <- fields_list_dt[j]
  # field_info <- data.table(id_10 = 911, id_field = 3)
  
    field_soils_dt <- grid10_soils_dt5[id_10 == field_info$id_10 &
                      id_field == field_info$id_field,]
    
    
    
    sum(field_soils_dt$area_ha)
    
    #======================================================================================
    # # ADD GEOGRAPHIC INFORMATION 
    # one_field_dt <- data.table(one_field_sf, st_coordinates(st_centroid(one_field_sf))) %>% .[,-'geometry']
    # setnames(one_field_dt, c('X', 'Y'), c('long', 'lat'))
    # one_field_dt <- one_field_dt[,.(area_ha = sum(area_ha),
    #                                        region = max(region), 
    #                                        long = mean(long), 
    #                                         lat = mean(lat)), by = .(id_tile, id_10, state_name, county_name, mukey)]
    # sum(one_field_dt$area_ha)
    #---------------------------------------------------------------------------
    #MAKE A MAP OF THE FIELD
    if(FALSE){
  
      one_field_sf <- grid10_soils_sf2[grid10_soils_sf2$id_10 == field_info$id_10 &
                                         grid10_soils_sf2$id_field == field_info$id_field,]
      one_field_sf2 <- one_field_sf[one_field_sf$mukey %in% field_soils_dt$mukey,]
      
      (field <- tm_shape(one_field_sf2) + tm_polygons("mukey") + 
         tm_layout(legend.text.size = 0.7,
                   main.title = paste('ONE FIELD MAP -', round(sum(one_field_sf$area_ha),1),' ha'),
                   main.title.position = "center",
                   main.title.size = 1))
      tmap_save(field, filename = "./n_dynamic_box/Data/figures/field.jpg", scale = 2)  
    }
    #---------------------------------------------------------------------------
    # FILTER APSIM DATA AND ADD VARIABLES NEEDED FOR PREDICTIONS
    z_odd = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29)
    z_even = z_odd+1
    z_select <- if(field_info$id_field %in% c(1,3)){z_odd} else{z_even}
    # z_select <- z_select[!z_select %in% training_z] #keep only testing z
    ic_field_dt <- yc_yearly_dt3[id_10 == field_info$id_10 & 
                                   mukey %in% unique(field_soils_dt$mukey) & z %in% z_select]
    
    # Update area and coordinates using field level information
    ic_field_dt <- merge(ic_field_dt, field_soils_dt[,.(id_10, mukey, area_ha, lat, long)], by = c('id_10', 'mukey'))
    
    ic_field_dt[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
    # ic_field_dt[,L := L1 + L2] #update leaching adding corn and soy
    
    ic_field_dt[,.N, by = .(z, mukey)]
    ic_field_dt$mukey %>% unique()
    # #===================================================================================================================
    # # CHANGE THE RATIO APPROACH
    # testing_set <- ic_field_dt %>%
    #     .[,c("mukey", "z", "id_10", "area_ha", "Y_corn", 'Y_soy', 'L1', 'L2', "L", "n_deep_v5","N_fert")]
      

    #===================================================================================================================
    # AGGREGATE THE INFORMATION AT THE FIELD LEVEL 
  
    ic_field_dt[, P := Y_corn * Pc - N_fert * Pn]
    # prediction_set <- data.table(unique(ic_field_dt[, c('mukey', 'z','area_ha', pred_vars, 'N_fert','P'), with = FALSE])) #this is unique v5 conditions, doesn't have the different N rates

    table(ic_field_dt$z)
    
    
    # We need to aggregate at the field level because is UR
    # do_aggregate = c(pred_vars, 'P')
    
    do_aggregate <- names(ic_field_dt)[!names(ic_field_dt) %in% 
                                         c('region','region_eq','id_10', 'area_ha', 'z', 'N_fert', 'mukey', 'water', 'N', 'sim_name')]

    ic_field_aggregated  <- aggregate_by_area(data_dt = ic_field_dt, variables = do_aggregate,
                                                  weight = 'area_ha', by_c = c('z', 'N_fert'))
    # %>% 
    #   .[, Yld_response := max(Y_corn) - min(Y_corn), by = .(z)] %>% #need for high information
    #   .[, .SD[ P == max( P)], by = .(z)] %>%
    #   .[, .SD[ N_fert == min(N_fert)], by = .(z)] %>% setnames('N_fert', 'eonr_12') %>% .[,-'P']
    # 
    # sort(names(prediction_set_aggregated))
    # test <- ic_field_dt[z == 11 & N_fert == 160, .(area_ha, oc_20cm_v5)]
    # test[,oc := oc_20cm_v5 * area_ha]
    # round(sum(test$oc)/sum(test$area_ha),2) == round(prediction_set_aggregated[z == 11,  ]$oc_20cm_v5, 2)
    ic_field_aggregated <- cbind(field_info[1,.(id_10, id_field, region, region_eq)], ic_field_aggregated)
    

  return(ic_field_aggregated)
}

# library('foreach')

time1 <- Sys.time()

fields_seq <- 1:nrow(fields_list_dt)

#---------------------
#Get the two sets for each field
process_field(20)

registerDoParallel(cores = detectCores()/2)
output_list = foreach(j = fields_seq, .combine = "c", .packages = c("data.table", "dplyr")) %dopar% {
  # j <- 1
  tmp_dt <- process_field(j)
  list(tmp_dt)
}#end of dopar loop

stopImplicitCluster()

yc_field_dt <- rbindlist(output_list)

yc_field_dt[, n_0_60cm_v5 := n_20cm_v5 + n_40cm_v5 + n_60cm_v5]
yc_field_dt[,L := L1 + L2]
yc_field_dt[, Yld_response := max(Y_corn) - min(Y_corn), by = .(id_10, id_field,z)]

# Add if it is a station or not
reg_model_stuff <- readRDS( "./n_dynamic_box/Data/files_rds/reg_model_stuff.rds")
stations_dt <- reg_model_stuff[['stations']] %>%
  .[,.(id_10, id_field)] %>% unique() %>% .[,station := 1]

yc_field_dt <- merge(yc_field_dt, stations_dt, by = c('id_10', 'id_field'), all.x = TRUE)
yc_field_dt[is.na(station), station := 0]
yc_field_dt[,.(area_ha = sum(area_ha)), by = .(id_10, id_field, z, N_fert)]$area_ha %>% summary()
yc_field_dt[,year := as.numeric(z)+1988]




saveRDS(yc_field_dt, "./n_dynamic_box/Data/files_rds/yc_field_dt.rds")

# yc_field_dt <- readRDS( "./n_dynamic_box/Data/files_rds/yc_field_dt.rds")
# yc_field_dt[region == 1, region_lab := '1-South']
# yc_field_dt[region == 2, region_lab := '2-Central']
# yc_field_dt[region == 3, region_lab := '3-North']
# yc_field_dt[,region := NULL]
# setnames(yc_field_dt, 'region_lab', 'region')
# 
# 
# region_dt <- unique(fields_list_dt[,.(id_10, region_eq)])
# yc_field_dt <- merge(yc_field_dt, region_dt, by = c('id_10') )
# 
# setcolorder(yc_field_dt, neworder = c('id_10', 'id_field', 'region', 'region_eq', 'z'))
