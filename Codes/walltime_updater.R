rm(list=ls())

setwd('C:/Users/germa/Box Sync/My_Documents') #dell
codes_folder <-'C:/Users/germa/Documents'#Dell
setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
codes_folder <-'C:/Users/germanm2/Documents'#CPSC
setwd('~')#Server
codes_folder <-'~' #Server



library(data.table)
library(dplyr)
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
#setwd('/projects/aces/germanm2/')
#---------------------------------------------------------------------------


time_track_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/time_track_walltime_dt.rds")
time_track_walltime_dt <- time_track_walltime_dt[id_10 %in% unique(c(grid10_soils_dt4[!region %in% c(3)]$id_10))]
time_track_walltime_dt[,dur := 120]
time_track_walltime_dt[id_10 == 699, dur := 160]
time_track_walltime_dt <- time_track_walltime_dt[order(mukey_n)]

write.table(time_track_walltime_dt[,.(id_10, dur)], paste0(codes_folder, '/n_policy_git/time_track_walltime_dt.txt'), row.names = F, col.names = F)

#---------------------------------------------------------------------------

# files_all <- list.files('./n_policy_box/Data/yc_output_summary/', recursive = T, pattern = '.rds', full.names = F)
# files_all <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_cluster/yc_output/', recursive = T, pattern = '.rds')
# files_all <- list.files('S:/Bioinformatics Lab/germanm2/n_policy/yc_output_summary/', recursive = T, pattern = '.rds')

# id_10_runned <- unique(sapply(strsplit(as.character(files_all), split="_"), "[", 1) )

id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
# id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/time_track_walltime_dt.rds")
id_10_walltime_dt <- id_10_walltime_dt[!id_10 %in% unique(two_batches_yc_dt$id_10)]
id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_failed]
time_track_walltime_dt <-id_10_walltime_dt

id_10_walltime_dt[,dur := N*6]
id_10_walltime_dt[N>5,dur := N*5]
id_10_walltime_dt[N>7,dur := N*4]
id_10_walltime_dt[N>10,dur := N*5]
time_track_walltime_dt <- id_10_walltime_dt[order(dur)]

write.table(id_10_walltime_dt[,.(id_10, dur)], paste0(codes_folder,'/n_policy_git/id_10_walltime.txt'), row.names = F, col.names = F)
#---------------------------------------------------------------------------

if(FALSE){ #send again files while still running
  
  id_10_walltime_dt_sent <- fread('./n_policy/id_10_walltime.txt', col.names = c('id_10', 'dur'))
  id_10_walltime_dt <- id_10_walltime_dt[!id_10 %in% id_10_walltime_dt_sent$id_10]
  id_10_walltime_dt[,dur := N *  4]
  files_all <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_cluster/yc_output/', recursive = T, pattern = '.rds')
  id_10_runned <- unique(sapply(strsplit(as.character(files_all), split="_"), "[", 1) )
  id_10_walltime_dt <- id_10_walltime_dt[!id_10 %in% id_10_runned]
  id_10_walltime_dt <- id_10_walltime_dt[order(dur)]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime2.txt', row.names = F, col.names = F)
  
}


if(FALSE){ #create the walltime file
  
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  grid10_soils_dt4[, z_type := ifelse(id_field %in% c(1,3), 'odd', 'even')]
  id_10_walltime_dt <- grid10_soils_dt4[,.N, by = .(id_10, mukey, z_type)] #number of id_10 x mukey x 15z combinations
  id_10_walltime_dt2 <- id_10_walltime_dt[,.N, by = .(id_10)][order(N)] 
  id_10_walltime_dt2[,dur := N*6]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
  saveRDS(id_10_walltime_dt, "./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
}


if(FALSE){ #re run id10 that didn't pass the daily to yearly
  id_10_rerun <- unique(sapply(strsplit(as.character(basename(files_daily2)), split="_"), "[", 1) )
  id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
  id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_10_rerun]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
}

if(FALSE){ #re run id10 that didn't pass the daily to yearly
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  files_yearly <- list.files("./n_policy_box/Data/yc_output_summary", recursive = T)
  library(data.table)
  already_run_dt <- data.table(id_10 = as.integer(sapply(strsplit(as.character(files_yearly), split="_"), "[", 1)),
                            mukey = gsub(sapply(strsplit(as.character(files_yearly), split="_"), "[", 2),pattern = '.rds', replacement = '')) 
  already_run_dt[,run := 'ok']
  grid10_soils_dt4_difference <- merge(grid10_soils_dt4, already_run_dt, by = c('id_10', 'mukey'))
  id_10_walltime_dt <- grid10_soils_dt4_difference[,.N, by = .(id_10, id_field, mukey)][,-'N']
  id_10_walltime_dt[id_field == 3 ,id_field := 1 ]
  id_10_walltime_dt[id_field == 4 ,id_field := 2 ]
  id_10_walltime_dt <- unique(id_10_walltime_dt )
  id_10_walltime_dt <- id_10_walltime_dt[,.N, by =id_10][order(N)]
  id_10_walltime_dt[,dur := N *  4]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
  saveRDS(id_10_walltime_dt, "./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime.txt', row.names = F, col.names = F)
}  
  
if(FALSE){ #re run id10 that didn't run in the sensor running
  normal_files <- list.files("./n_policy_box/Data/yc_output_summary_1", full.names = F)
  length(normal_files)
  sensor_files <- list.files("./n_policy_box/Data/yc_output_summary_2", full.names = F)
  length(sensor_files)
  missing <- sensor_files[!sensor_files %in% normal_files]
  id_10_missing <- unique(sapply(strsplit(as.character(missing), split="_"), "[", 1) )
  id_10_missing <- c(5, id_10_missing)
  #Create a new walltime file
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  id_10_walltime_dt <- grid10_soils_dt4[,.N, by = .(id_10, id_field, mukey)][,-'N']
  id_10_walltime_dt[id_field == 3 ,id_field := 1 ]
  id_10_walltime_dt[id_field == 4 ,id_field := 2 ]
  id_10_walltime_dt <- unique(id_10_walltime_dt ) #each row is a mukey with a set of z. Each row is 15 runs
  id_10_walltime_dt <- id_10_walltime_dt[,.N, by =id_10][order(N)] #Cound of 15 runs by id_10 
  id_10_walltime_dt[,dur := N *  2]
  id_10_walltime_dt <- id_10_walltime_dt[id_10 %in% id_10_missing]
  id_10_walltime_dt[id_10 == 5, dur := dur + 10]
  write.table(id_10_walltime_dt[,.(id_10, dur)], './n_policy/id_10_walltime2.txt', row.names = F, col.names = F)
}

if(FALSE){ #find missing mukeys
  grid10_soils_dt4 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_dt4.rds")
  files_yearly <- list.files("./n_policy_box/Data/yc_output_summary_88_swat", recursive = T)
  length(files_yearly)
  
  already_run_dt <- data.table(id_10 = as.integer(sapply(strsplit(as.character(files_yearly), split="_"), "[", 1)),
                               mukey = gsub(sapply(strsplit(as.character(files_yearly), split="_"), "[", 2),pattern = '.rds', replacement = '')) 
  
  already_run_dt[,run := 'ok']
  
  grid10_soils_dt4 <- grid10_soils_dt4[id_10 %in% unique(already_run_dt$id_10)]
  grid10_soils_dt4_difference <- merge(grid10_soils_dt4, already_run_dt, by = c('id_10', 'mukey'), all.x = T)
  
  incomplete_id10 <- grid10_soils_dt4_difference[is.na(run)]$id_10 %>% unique()

  id_10_walltime_dt2 <- id_10_walltime_dt2[id_10 %in% incomplete_id10,]
  write.table(id_10_walltime_dt2[,.(id_10, dur)], paste0(codes_folder,'/n_policy_git/id_10_walltime.txt'), row.names = F, col.names = F)
  
}  

if(FALSE){ #Update using timetrack
  # setwd('C:/Users/germa/Box Sync/My_Documents') #dell
  setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
  # setwd("/home/germanm2")
  # setwd('~')
  rm(list=ls())
  
  source('./Codes_useful/R.libraries.R')
  
  files_time <- list.files('S:/Bioinformatics Lab/germanm2/n_policy_cameron/time_track',full.names = T, recursive = T)
  files_time <- list.files('./n_policy_box/Data/time_track_141/',full.names = T, recursive = T)
  length(files_time)
  
  results_list <- list()
  for(file_n in files_time){
    # file_n <- files_daily[2]
    # print(file_n)
    results_list[[basename(file_n)]] <- readRDS(file_n)
  }
  
  time_track_dt <- rbindlist(results_list)
  time_track_dt[mukey_n ==0]
  
  time_track_walltime_dt <- time_track_dt[,.(id_10, mukey_n, dur = cell)]
  ggplot(time_track_walltime_dt) +
    geom_point(aes(x = mukey_n, y = dur))
  
  time_track_walltime_dt[,dur_max := max(dur), by = mukey_n]
  
  time_track_walltime_dt[,dur_max := round(dur_max,0)+5]
  
  ggplot(time_track_walltime_dt) +
    geom_point(aes(x = mukey_n, y = dur_max))
  
  time_track_walltime_dt[,dur5 := round(dur,0)+5]
  
  time_track_walltime_dt[,.(dur = sum(dur),
                            dur5 = sum(dur5),
                            dur_max = sum(dur_max))]
  
  time_track_walltime_dt <- time_track_walltime_dt[,.(id_10, mukey_n, dur5)][order(dur5) ]
  
  setnames(time_track_walltime_dt, 'dur5', 'dur')
  
  write.table(time_track_walltime_dt[,.(id_10, dur)], paste0(codes_folder, '/n_policy_git/time_track_walltime_dt.txt'), row.names = F, col.names = F)
  saveRDS(time_track_walltime_dt, "./n_policy_box/Data/files_rds/time_track_walltime_dt.rds")
  # missing_dt <- time_track_walltime_dt2[!id_10 %in% time_track_walltime_dt$id_10]
  # missing_dt[,dur := dur +20]
  # time_track_walltime_dt <- rbind(time_track_walltime_dt, missing_dt)
  # time_track_walltime_dt <- time_track_walltime_dt[,.(id_10, mukey_n, dur)][order(dur) ]
  
  id_10_walltime_dt <- readRDS("./n_policy_box/Data/files_rds/id_10_walltime_dt.rds")
  comp_dt <- merge(id_10_walltime_dt, time_track_walltime_dt, by = 'id_10')
  ggplot(comp_dt) +
    geom_point(aes(x = dur.x, y = dur.y))
  
}