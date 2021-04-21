######################################
# Parallelized Simulations
######################################

#===================================
# prepare clusters
#===================================

no_cores <- detectCores() * 7/8

cl <- parallel::makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================
#i =   1

apsim_create_files <- function(i){
  # i = 1
  #--------------------------
  # preparation
  #--------------------------
  #--- load libraries ---#
  library(xml2)
  library(data.table)
  library(dplyr)
  
  instructions_tmp <- instructions[i]
  sim_name <- paste(unlist(instructions_tmp[,.(id_10, mukey, z, water)]), collapse = '_')
  
  #--- load the base apsim file ---# 
  base_doc <- xml2::read_xml("./n_policy_box/Data/apsim_files/vr_value_v9.apsim")
  
  
  #Clean the plots
  # for(x in xml2::xml_find_all(base_doc, '//Graph')[2:10]){xml2::xml_remove(x)}
  
  #--- edit the met directory ---#
  met_dir <- paste(directory, '/met_files/z_',instructions_tmp$z,'.met', sep = '')
  met_dir <- gsub("/", "\\", met_dir, fixed=TRUE)
  
  node <-  xml_find_all(base_doc,'//metfile/filename')
  xml_text(node) <- met_dir
  
  #--- edit modules directory ---#
  #SurgaceOM Module
  # module_dir <- paste0(getwd(), '/n_policy_box/Data/apsim_files/modules_edited/SurfaceOM_aug2020.xml')
  # module_dir <- gsub("/", "\\", module_dir, fixed=TRUE)
  # node <-  xml_find_all(base_doc,'//surfaceom/ini/filename')
  # xml_text(node) <- module_dir
  
  #Maize Module
  module_dir <- paste0(getwd(), '/n_policy_box/Data/apsim_files/modules_edited/Maize_aug2020.xml')
  module_dir <- gsub("/", "\\", module_dir, fixed=TRUE)
  node <-  xml_find_all(base_doc,'//maize/ini/filename')
  xml_text(node) <- module_dir
  
  #Soybean Module
  # module_dir <- paste0(getwd(), '/n_policy_box/Data/apsim_files/modules_edited/Soybean_v711.xml')
  # module_dir <- gsub("/", "\\", module_dir, fixed=TRUE)
  # node <-  xml_find_all(base_doc,'//soybean/ini/filename')
  # xml_text(node) <- module_dir
  
  #--------------------------
  # PLANTING DATE BY REGION
  #--------------------------
  plant_dates_dt <- readRDS("./n_policy_box/Data/files_rds/plant_dates_dt.rds") %>%
  .[id_10 == instructions_tmp$id_10] %>% .[1] 
  # plant_dates_dt <- plant_dates_dt[z == instructions_tmp$z]
  planting_start_corn <- plant_dates_dt$p_date_corn2[1]
  planting_end_corn <- plant_dates_dt$p_date_corn2[1]
  # planting_start_corn <- c('05-Apr', '05-Apr', '15-Apr')[instructions_tmp$region]
  # planting_end_corn <- c('10-Apr', '10-Apr', '20-Apr')[instructions_tmp$region]
  
  x <- xml_find_all(base_doc, ".//manager/ui/date1_corn")
  xml_text(x) <- as.character(planting_start_corn)
  
  x <- xml_find_all(base_doc, ".//manager/ui/date2_corn")
  xml_text(x) <- as.character(planting_end_corn)
  
  #Soy some days later
  # planting_start_soy <- c('15-Apr', '20-Apr', '24-Apr')[instructions_tmp$region]
  # planting_end_soy <- c('20-Apr', '24-Apr', '30-Apr')[instructions_tmp$region]
  # planting_start_soy <- c('25-Apr', '30-Apr', '5-May')[instructions_tmp$region]
  # planting_end_soy <- c('5-May', '10-May', '15-May')[instructions_tmp$region]
  
  planting_start_soy <- plant_dates_dt$p_date_soy2[1]
  planting_end_soy <- plant_dates_dt$p_date_soy2[1]
  
  x <- xml_find_all(base_doc, ".//manager/ui/date1_soy")
  xml_text(x) <- as.character(planting_start_soy)
  
  x <- xml_find_all(base_doc, ".//manager/ui/date2_soy")
  xml_text(x) <- as.character(planting_end_soy)
  
  #--------------------------
  # CULTIVAR BY REGION
  #--------------------------
  cultivar_corn <- paste0('B_', plant_dates_dt$hybrid_rm[1], '_iowa_hy')
  
  x <- xml_find_all(base_doc, ".//manager/ui/cultivar_corn")
  xml_text(x) <- as.character(cultivar_corn)
  
  # cultivar_soy <- c('MG_4', 'MG_3', 'MG_2')[instructions_tmp$region]
  cultivar_soy <- paste0('MG_', plant_dates_dt$soy_mg[1]+1)
  x <- xml_find_all(base_doc, ".//manager/ui/cultivar_soy")
  xml_text(x) <- as.character(cultivar_soy)
  
  
  #--------------------------
  # PLANT POPULATION
  #--------------------------
  # if(instructions_tmp$batch >= 54 ){
  #   plant_population <- c('7', '8.5', '9')[instructions_tmp$region]
  # }
  
  plant_population <- '8'
  
  x <- xml_find_all(base_doc, ".//manager/ui/density_corn")
  xml_text(x) <- as.character(plant_population)
  
  #--------------------------
  # CLOCK
  #--------------------------
  date_start <-  '01/01/2010'
  date_end <-'30/04/2012'
  
  node <- xml_find_all(base_doc,'//clock/start_date')
  xml_text(node) <- date_start
  
  node <-  xml_find_all(base_doc,'//clock/end_date')
  xml_text(node) <- date_end
  
  #--------------------------
  # ROTATION
  #--------------------------
  
  crop_seq <- c('maize', 'soybean', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil', 'nil',  'nil',  'nil')
  
  
  for(crop_n in 1:11){
  # crop_n = 1
    node <- xml_find_all(base_doc, paste0('//manager/ui/crop', crop_n))
    xml_text(node) <- crop_seq[crop_n]
  }
  
  #--------------------------
  # SOIL
  #--------------------------
  #--- extract soil data from the soil data base ---#
  # soils_database <- xmlParse('./Trial_crct_DIFM/Data/APssurgo_master/APSIM_soils/Soils_DIFM_bysoil.soils')
  soils_database <- xml2::read_xml(paste(directory, 'soils_vr_value.soils', sep = '/'))
  
  soil_name2 <- paste('.//Soil[@name="', tolower(instructions_tmp$mukey), '"]', sep = '')
  
  #--- replace soil ---#
  soil_temp2 <- xml_find_all(soils_database, soil_name2)
  base_doc_soil <- xml_find_all(base_doc, "//Soil")
  
  xml_replace(base_doc_soil, soil_temp2, .copy = TRUE)
  
  rm(soils_database, soil_temp2,base_doc_soil)
  
  #-----------------------------------------------------------------------------------------------
  # 1 - Insert the swim module
  if(instructions_tmp$water == 'swim'){
    swim_file <- xml2::read_xml("./n_policy_box/Data/apsim_files/LongTermAssessmentsTileDrainage_ger.apsim")
    
    node_swim <- xml_find_all(swim_file,'//Swim')
    
    node_swat <- xml_find_all(base_doc,'//SoilWater')
    xml_replace(node_swat, node_swim, .copy = TRUE)
    rm(swim_file, node_swim,node_swat)
  }#end if swim
  #-----------------------------------------------------------------------------------------------
  # 2 - Add water_table to swat
  if(instructions_tmp$water == 'swat' & !(is.na(instructions_tmp$watertable))){
    watertable_n <- round(instructions_tmp$watertable)*10
    if(watertable_n < 1500){watertable_n = 1500}
    node <- xml_find_all(base_doc,'//manager[@name="Empty manager"]/script')[2]
    empty_manager <- xml_text(node)
    empty_manager2 <- gsub(pattern = '!water_table = 1000\n\n\n\nstart_of_day', replacement = paste0('water_table = ', watertable_n), empty_manager)
    xml_text(node) <- empty_manager2
  }#end if swat
  #-----------------------------------------------------------------------------------------------
  # 3 - Update the Initial Conditions
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simE_update_ic.R"
  "./n_policy_git/Codes/simE_update_ic.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simE_update_ic.R')) #simplified version
  #The initial residue assumes an alternation. Can be improved for account for other types of rotations
  base_doc <- update_ic(base_doc, instructions_tmp)

  #--- Set the rate for the stab period ---#
  x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_stab")
  xml_text(x) <- "150"
  
  #--- Set the starter rate ---#
  x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_sow")
  xml_text(x) <- "0"
  
  #--- CREATE A FOLDER TO SAVE FILES ---#
  folder_name <- paste0(directory, '/', sim_name)
  
  if(file.exists(folder_name)){unlink(folder_name ,recursive=TRUE) }
  dir.create(folder_name, recursive = TRUE)
  
  #--- Set the N rate for the trial period ---#
  N_rates <- seq(0, 320, 10)
  if(test_small) {N_rates <- 160}
  if(regional_test){N_rates <- c(0, seq(16, 150, 28), seq(150, 200, 10), seq(212, 270, 28))}
  # if(regional_test){N_rates <- 160}
  # N_rates <- seq(0, 300, 25)
  # N_rates <- c(0,260)
  # N_rates <- c('Nminus', 'Nrich')
  # N_rates <- 150
  for(N_n in N_rates){
    # N_n = N_rates[1]
    sim_name_n <- paste(sim_name, N_n, sep = '_')
    
    #----------------------------------
    #Change the name of the simulation
    x <-  xml_find_all(base_doc, ".//simulation")
    xml2::xml_attrs(x, 'name')[[1]] <-  c(name = sim_name_n)
    
    #----------------------------------
    #Change the name of the outputfile
    x <- xml_find_all(base_doc, ".//outputfile/filename")
    xml_text(x) <- as.character(paste(sim_name_n, '.out', sep = ''))
    
    #----------------------------------
    #Apply the treatment at v5
    x <- xml_find_all(base_doc, ".//manager/ui/fert_amount_yc")
    xml_text(x) <- as.character(N_n)
    
    #--- save as a temporary xml file ---#
    folder_name_n <- paste(folder_name, N_n, sep = '/')
    dir.create(folder_name_n, recursive = TRUE)
    filename <- paste0(sim_name_n, '.apsim')
    
    xml2::write_xml(base_doc, paste(folder_name_n,'/',filename,sep=''))
  }
}


keep <- c('keep', 'apsim_create_files', 'instructions', 'directory', 'codes_folder', 'regional_test', 'test_small')
# if(unique(instructions$type) == 'YC'){ keep <- append(keep, 'initial_conditions' )}
# # #rm(list = ls()[!ls() %in% keep])

parallel::clusterExport(cl, varlist = keep, envir=environment())

results.list <- parallel::parLapply(cl, 1:nrow(instructions), function(x) apsim_create_files(x))

# instructions <- rbindlist(results.list, fill = TRUE)

stopCluster(cl)