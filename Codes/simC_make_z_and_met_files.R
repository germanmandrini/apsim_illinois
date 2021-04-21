#=====================================================================================================
# STEP 1: Create the data.table with the weathers for each z
weather_historic_dt <- readRDS('./n_policy_box/Data/met_files/weather_historic_dt2019.rds')

create_z <- function(id10_n, weather_historic_dt){
  # id10_n = 730
  print(id10_n)
  years_seq_long <- c(1980:2019,2010)
  # packages_need <- c('APSIM', 'daymetr','dplyr', 'data.table', 'sf')
  packages_need <- c('dplyr', 'data.table')
  lapply(packages_need, require, character.only = TRUE)
  
  
  daymet_dt <- weather_historic_dt[id_10 == id10_n]
  
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
  
  return(daymet_z_cell2)
}


weather_cell.dt <- create_z(id10_n, weather_historic_dt)
rm(weather_historic_dt)
#--------------
# Save the weather file data.table. We need it for yearly summary
folder_name <- paste(directory, '/met_files',sep = '')
if(!file.exists(folder_name)){dir.create(folder_name, recursive = TRUE)}
saveRDS(weather_cell.dt, paste(folder_name, '/weather_z_cell', id10_n, '_dt.rds', sep = '')) #save each id, it gets to heavy if all together

#=====================================================================================================
# STEP 2: Create the met files and save them in the simulation folder

#===================================
# prepare clusters
#===================================

# make_met_files_paralell <- function(weather_cell.dt, directory){
no_cores <- detectCores() *7/8
cl <- parallel::makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================


make_met_files <- function(z_n, weather_cell.dt, directory = directory){
  # z_n = 'A1'
  packages_need <- c('APSIM','dplyr', 'data.table', 'sf')
  lapply(packages_need, require, character.only = TRUE)
  
  daymet.dt2 <- weather_cell.dt[z == z_n,]
  
  #Fix leap years #remove this in the future
  # leap <- table(daymet.dt2$year)
  # if(leap[1] == 366){ daymet.dt2 <- daymet.dt2[-366,]} 
  # if(leap[2] == 365){ daymet.dt2 <- rbind(daymet.dt2, daymet.dt2[.N][,day := 366]) } 
 
  table(daymet.dt2$year) #1999 has to be 365 and 2000 has to be 366
  #------------------------------------------------------------------
  # PREPARE THE FILE
  units_input <- c("()", "()", "(MJ/m^2/day)", "(oC)", "(oC)", "(mm)", "(hours)") 
  setcolorder(daymet.dt2, c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'dayl'))
  daymet.df <- data.frame(daymet.dt2[,-c('lat', 'lon', 'id_10', 'z', 'z1', 'z2')])
  
  met_file_tmp <- prepareMet(daymet.df, 
                             lat = mean(daymet.dt2$lat), 
                             lon = mean(daymet.dt2$lon),  
                             units = units_input)
  
  table(met_file_tmp@data$year)
  #------------------------------------------------------------------
  # CORRECT TAV AND AMP (they are switched)
  # Amp is obtained by averaging the mean daily temperature of each month over the entire data period resulting in
  # twelve mean temperatures, and then subtracting the minimum of these values from the maximum
  # Tav is obtained by averaging the twelve mean monthly temperatures.
  daymet.dt2[,date := 1:nrow(daymet.dt2)]
  daymet.dt2[,date2 := as.Date(date, origin = paste0(min(daymet.dt2$year)-1, "-12-31"))]
  daymet.dt2[,month := month(date2)]
  daymet.dt2[,meant := (mint+maxt)/2]
  monthly <- daymet.dt2[,.(tmonth = mean(meant)), by = month]
  Amp <- max(monthly$tmonth) - min(monthly$tmonth)
  Tav <- mean(monthly$tmonth)
  
  met_file_tmp@amp <- Amp
  met_file_tmp@tav <- Tav
  
  #------------------------------------------------------------------
  # SAVE THE FILE
  #--- save as a temporary xml file ---#

  # unlink(folder ,recursive=TRUE)
  folder_name <- paste(directory, '/met_files',sep = '')
  
  # if(!file.exists(folder_name)){dir.create(folder_name, recursive = TRUE)}
  
  fileName_tmp <- paste(folder_name,'/z_', z_n, '.met', sep='')
  
  writeMetFile(fileName_tmp, met_file_tmp)
  
  # return(folder_name2)
  
}#end of loc_id_n loop

keep <- c('keep', 'make_met_files','id10_n', 'weather_cell.dt', 'directory')

parallel::clusterExport(cl, varlist = keep, envir=environment())

z_seq <- unique(weather_cell.dt$z)
results.list <- parallel::parLapply(cl, z_seq, function(x) make_met_files(x, weather_cell.dt, directory))

stopCluster(cl)



