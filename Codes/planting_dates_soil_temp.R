rm(list=ls())

# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# codes_folder <-'C:/Users/germanm2/Documents'#CPSC

setwd('~')#Server
codes_folder <-'~' #Server


source('./Codes_useful/R.libraries.R')
source('./Codes_useful/gm_functions.R')
source(paste0(codes_folder, '/n_policy_git/Codes/parameters.R'))

#======================================================================================
#Merge all files
# YC OUTPUT EVALUATION
#MERGE FILES
multiple_files <- list.files("./n_policy_box/Data/yc_output_18", full.names = T)
# multiple_files <- multiple_files[sample(1:length(multiple_files), 200)]
# length(multiple_files)

files_list <- list()
for(file_n in multiple_files){
  # file_n <- multiple_files[1]
  tmp_dt <- readRDS(file_n)
  names(tmp_dt)
  tmp_dt2 <- tmp_dt[year == 2010,.(sim_name, id_10, mukey, z, Date, year, month, day, st_3)]
  
  tmp_dt2[,N_fert := lapply(strsplit(as.character(sim_name), split="_"), "[", 5) ]
  tmp_dt3 <- tmp_dt2[N_fert == 260]
  tmp_dt3[, soilt10d := rollapplyr(st_3, 1:10, mean), by =  sim_name]
  tmp_dt3[,.N, .(sim_name)]
  tmp_dt4 <- tmp_dt3[,.(st_3 = mean(st_3),
             soilt10d = mean(soilt10d)), by = .(id_10, mukey, z, Date, year, month, day)] #mukey is out
  
  # table(tmp_dt$z)
  # names(tmp_dt)
  # cols <- names(tmp_dt)[1:82]
  # tmp_dt <- rbind(tmp_dt[year == 2001 & day == 1 & !is.na(Y), c(cols), with = F], tmp_dt[year == 2010 & month == 12 & day == 365, c(cols), with = F])
  # tmp_dt <- tmp_dt[year == 2010 & month == 1 & day == 1 & Y == 0][, .SD[1], by = .(id_10, mukey, z)]
  files_list[[length(files_list)+1]] <- tmp_dt4
}
yc_output_dt <- rbindlist(files_list)

#Add regions
grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
regions_dt <- data.table(grid10_soils_dt4) %>% .[,.N,.(id_10, region)] %>% .[,-'N']
sapply(regions_dt, class)
regions_dt[,id_10 := as.character(id_10)]
yc_output_dt <- merge(yc_output_dt, regions_dt, by = 'id_10')
plant_dates_dt <- yc_output_dt[,.(st_3 = mean(st_3),
                                  soilt10d = mean(soilt10d)), by = .(region, z, Date, year, month, day)][month %in% c(4,5)]
plant_dates_dt2 <- plant_dates_dt[soilt10d > 10][, .SD[1], by = .(region,  z)]
plant_dates_dt2[,.(day = mean(day)), region]
plant_dates_dt2[,day := lapply(strsplit(as.character(Date), split="-"), "[", 3) ]
plant_dates_dt2[,Date := paste0(day, '-Apr')]


saveRDS(plant_dates_dt2, "./n_policy_box/Data/files_rds/plant_dates_dt.rds")



