#===================================
# prepare clusters
#===================================
setwd("~") #server
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC

source('./Codes_useful/R.libraries.R')

no_cores <- 28#detectCores() * 7/8
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================




# install.packages('APSIM')

#install.packages("daymetr")

#setwd('C:/Users/germa/Box Sync/My_Documents') #Dell
#setwd("C:/Users/germanm2/Box Sync/My_Documents") #CPSC
#setwd("~") #server
#source('./Codes_useful/R.libraries.R')

weather_historic_dt <- readRDS('./n_policy_box/Data/met_files/weather_historic_dt2019.rds')



create_z <- function(id_10_n, weather_historic_dt){
  # id_10_n = 730
  print(id_10_n)
  
  packages_need <- c('APSIM', 'daymetr','dplyr', 'data.table', 'sf')
  lapply(packages_need, require, character.only = TRUE)
  
  
  daymet_dt <- weather_historic_dt[id_10 == id_10_n]
  
  # setcolorder(daymet.dt, c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'dayl'))
  years_seq_long
  
  #------------------------------------------------------------------
  # CREATE THE Za
  daymet_z_list <- list()
  
 
  for(z_n in 1:30){
    # z_n = 30
    
    years_seq_12 <- years_seq_long[z_n:(z_n+11)]
  
    idx_and_year <- data.table()
    year_counter = 2001 #2010 is the year of the yield curve
    for(year_n in years_seq_12){
      # year_n = years_seq_12[1]
      idx_tmp <- which(daymet_dt$year == year_n)
      idx_and_year <- rbind(idx_and_year,
                            data.table(idx  = idx_tmp, year = rep(year_counter, length(idx_tmp))))
      year_counter <- year_counter + 1
    } # end year_n loop
  
    daymet_tmp <- daymet_dt[idx_and_year$idx]
    table(daymet_tmp$year)
    daymet_tmp$year <- idx_and_year$year
    table(daymet_tmp$year)
    
    daymet_tmp[,z:=z_n]
      
    daymet_z_list[[z_n]] <- daymet_tmp
  }#end of z_n loop
  
  length(daymet_z_list)
  daymet_z_cell <- rbindlist(daymet_z_list)
  #------------------------------------------------------------------
  #CORRECT LEAP YEAR
  # The Daymet calendar is based on a standard calendar year. All Daymet years have 1 - 365 days, including leap years. 
  # For leap years, the Daymet database includes leap day. Values for December 31 are discarded from leap years to maintain a 365-day year.
  leap_years <- seq(1980,2019, by = 4)
  correction_leap.dt <- daymet_z_cell[year %in% leap_years & day %in% 59:60]
  correction_leap.dt <- correction_leap.dt[,.(day = mean(day),
                                              radn = mean(radn),
                                              maxt = mean(maxt),
                                              mint = mean(mint),
                                              rain = mean(rain),
                                              dayl = mean(dayl)), by = .(year, lat,lon, id_10, z)]
  nrow(correction_leap.dt)
  
  daymet_z_cell2 <- rbind(daymet_z_cell, correction_leap.dt) %>% .[order(year, day)]
  daymet_z_cell2[, day2 := seq_len(.N), by = .(year, lat,lon, id_10, z)]
  daymet_z_cell2[,day := NULL]
  setnames(daymet_z_cell2, 'day2', 'day')
  
  daymet_z_cell2[,.N, by = .(year, z)][,.(mean(N)), by = year]
  
  #------------------------------------------------------------------
  # hist(daymet_z_cell[,.(rain = sum(rain)), by = .(z, year)][,rain]) #check. They have to be different
  
  saveRDS(daymet_z_cell2, paste('./n_policy_box/Data/met_files/weather_z_cell', id_10_n, '_dt.rds', sep = '')) #save each id, it gets to heavy if all together
  
  return()
}

keep <- c('keep', 'create_z', 'weather_historic_dt', 'years_seq_long')
set.seed(123)




#EXPLORE ----
# years_seq_long <- readRDS("./n_policy_box/Data/files_rds/years_seq_long.rds")

years_seq_long <- c(sort(unique(weather_historic_dt$year)), 2010)
saveRDS(years_seq_long, "./n_policy_box/Data/files_rds/years_seq_long.rds")
years_seq_long_dt <- data.table(year = years_seq_long, 
                                order = 1:length(years_seq_long))

weather_explore_dt <- weather_historic_dt[,.(rain=sum(rain)), by = .(year, id_10)] %>% .[,.(rain=mean(rain)), by = .(year)]
weather_explore_dt <- merge(weather_explore_dt, years_seq_long_dt, by = 'year')
weather_explore_dt[order(order)]
ggplot(weather_explore_dt, aes(x = order, y = rain))+
    geom_bar(stat="identity")

#----


clusterExport(cl, varlist = keep, envir=environment())

grid10_fields_sf2 <- readRDS("./n_policy_box/Data/Grid/grid10_fields_sf2.rds")
id_10_seq <- sort(unique(grid10_fields_sf2$id_10))

results_list <- parLapply(cl, id_10_seq, function(x) create_z(x, weather_historic_dt))

# results_list <- list()
# for(id_10_n in ids_10_seq){
#   results_list[[length(results_list)+1]] <- create_z(id_10_n, grid10_tiles_sf)
# }

# weather_z_all_cells_dt <-  rbindlist(results_list)

# saveRDS(weather_z_all_cells_dt, './n_policy_box/Data/met_files/weather_z_all_cells_dt.rds')

stopCluster(cl)
 
