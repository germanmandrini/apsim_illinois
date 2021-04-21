rm(list=ls())

library(stringr)
library(data.table)

#Get the computer where this is running
server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
cpsc <-ifelse(Sys.info()["nodename"] == "CPSC-P10E53323", TRUE, FALSE)
cluster <- str_detect(string = Sys.info()["nodename"], pattern = 'campuscluster')
print(Sys.info()["nodename"])

#Set the wd
if(server){
  setwd('~')
  codes_folder <- getwd()
  test_small <- F #only one soil and one z
  regional_test <- F #makes rates every 25
  regional_soils <- F#uses regional soils
}else if(cpsc){
  setwd('C:/Users/germanm2/Box Sync/My_Documents')
  codes_folder <-'C:/Users/germanm2/Documents'
  test_small <- F
  regional_test <- F #makes rates every 25
  regional_soils <- F #uses regional soils
}else{
  setwd('/projects/aces/germanm2/')
  # setwd('/home/germanm2/scratch/')
  cluster <- TRUE	
  codes_folder <- '/projects/aces/germanm2'
  # codes_folder <- '/home/germanm2/scratch'
  test_small <- TRUE   #only one soil and one z
  regional_test <- FALSE #makes rates every 25
  regional_soils <- FALSE    #uses regional soils
}

#----------------------------------------------------------------------------
# 

water_n = 'swat'

if(server|cpsc){
  id10_n = 39
  batch_n = '150'
}else{
  id10_n = as.numeric(commandArgs(trailingOnly=TRUE)[1])
  batch_n = as.numeric(commandArgs(trailingOnly=TRUE)[2])
}


# id10_seq <- c(1214, 1332, 1488, 970, 894, 663, 219, 39, 45, 43, 807, 1362)
# id10_seq <- c(39, 663, 1488)
# id10_seq <- c(1214, 1332, 970, 894, 219, 45, 43, 807, 1362)
# id10_seq <- c(219, 45, 43)
# id10_seq <- c(1500, 1245, 1156, 596, 797, 576, 286, 52, 253)
# id10_seq <- c(1500, 596,52,1069, 513, 53)
# id10_seq <- c(43, 807, 1362,1069, 513, 53)

# for(batch_n in c('89_105', '89_110', '89_115')){
# for(id10_n in id10_seq){
  # id10_n = id10_seq[1]
  print(id10_n)
  print(batch_n)
  
  # CREATE ALL FILES
  start1 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simB_create_instructions.R"
  "./n_policy_git/Codes/simB_create_instructions.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simB_create_instructions.R'))
  instructions1_rows <- nrow(instructions)
  
  #RUN ALL APSIM FILES
  start2 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simF_run_files.R"
  "./n_policy_git/Codes/simF_run_files.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simF_run_files.R'))
  
  #MERGE ALL THE OUTPUT
  start3 <- Sys.time()
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simG_merge_results.R"
  "./n_policy_git/Codes/simG_merge_results.R"
  source(paste0(codes_folder, '/n_policy_git/Codes/simG_merge_results.R'))
  
  start4 <- Sys.time()
  
  #MAKE YEARLY SUMMARY
  files_daily <- list.files(paste0('./n_policy_box/Data/yc_output_', batch_n, '_', water_n), pattern = paste0('^',id10_n, '_'), full.names = T)
  print(files_daily)
  "C:/Users/germanm2/Documents/n_policy_git/Codes/simH_daily_to_yearly.R"
  './n_policy_git/Codes/simH_daily_to_yearly.R'
  source(paste0(codes_folder, '/n_policy_git/Codes/simH_daily_to_yearly.R'))
  
  unlink(directory, recursive = TRUE)
  
  start5 <- Sys.time()
  
  time_track_tmp <- data.table(id_10 = id10_n,
                               mukey_n = length(unique(instructions$mukey)),
                               time = start1,
                               inst = instructions1_rows,
                               create = as.numeric(difftime(start2, start1, units = "mins")),
                               run = as.numeric(difftime(start3, start2, units = "mins")),
                               merge_save = as.numeric(difftime(start4, start3, units = "mins")),
                               yearly_summary = as.numeric(difftime(start5, start4, units = "mins")),
                               cell = as.numeric(difftime(start5, start1, units = "mins")))
  print(time_track_tmp)
  
  folder_name <- paste0('./n_policy_box/Data/time_track_', batch_n)
  if(!file.exists(folder_name)){dir.create(folder_name, recursive = TRUE)}
  saveRDS(time_track_tmp, paste0(folder_name,'/time_track_',id10_n,'.rds'))
  
  
# }# end id10_n loop
#}#end batch_n loop

