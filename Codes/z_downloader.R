#===================================
# prepare clusters
#===================================
setwd("~") #server
# setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents') #CPSC
source('./Codes_useful/R.libraries.R')

no_cores <- detectCores() * 6/8
cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================

grid10_tiles_sf <- readRDS('./n_policy_box/Data/Grid/grid10_tiles.sf5.rds')

download_weather <- function(id_10_n, grid10_tiles_sf){
  # id_10_n = sort(unique(grid10_tiles_sf$id_10))[120]
  print(id_10_n)
  
  packages_need <- c('APSIM', 'daymetr','dplyr', 'data.table', 'sf')
  lapply(packages_need, require, character.only = TRUE)
  
  one_10 <- grid10_tiles_sf[grid10_tiles_sf$id_10 == id_10_n,]
  one_10 <- st_transform(one_10, 4326)
  
  centroid <- data.table(st_coordinates(st_centroid(one_10)))
  
  daymet.daymetr <- download_daymet(site = id_10_n, 
                        lat = centroid$Y, lon = centroid$X, 
                        start = 2019, end = 2019, 
                        internal = TRUE)

  # data.table::fwrite(daymet.daymetr$data, './n_policy_box/Data/met_files/testdata.csv')
  daymet.dt <- daymet.daymetr$data %>% data.table() %>% 
    .[,.(year, day=yday, radn=srad..W.m.2., maxt=tmax..deg.c., mint=tmin..deg.c., 
         rain=prcp..mm.day., dayl = dayl..s.)]
  #------------------------------------------------------------------
  #CONVERT SOLAR RADIATION
  # Watts/m2 = MJ/m2 * 1000000 / (60 * 60 * 24) https://cliflo-niwa.niwa.co.nz/pls/niwp/wh.do_help?id=ls_rad
  # input = srad	W/m2	Incident shortwave radiation flux density in watts per square meter, 
  # taken as an average over the daylight period of the day. 
  # NOTE: Daily total radiation (MJ/m2/day) can be calculated as follows: 
  # ((srad (W/m2) * dayl (s/day)) / l,000,000) #https://daac.ornl.gov/DAYMET/guides/Daymet_V3_CFMosaics.html
  # output = (MJ/m^2/day) #prepareMet documentation
  
  daymet.dt[,radn := radn * dayl/1000000]
  daymet.dt[,dayl := dayl/3600]
  summary(daymet.dt$radn)
  # daymet.dt[,.N, by=year]
  # daymet.dt[,.(pp = sum(rain)), by=year]
  # #head(daymet_tmp.df)
  # # head(daymet.dt)
  #newNames <-c("Date", "maxt", "mint", "rain", "evaporation", "radn", "vp", "Wind", "RH", "SVP") 
 
  #------------------------------------------------------------------
  #CORRECT LEAP YEAR
  # The Daymet calendar is based on a standard calendar year. All Daymet years have 1 - 365 days, including leap years. 
  # For leap years, the Daymet database includes leap day. Values for December 31 are discarded from leap years to maintain a 365-day year.
  # correction_leap.dt <- daymet.dt[year %in% leap_years & day %in% c(59, 60)]
  # correction_leap.dt <- correction_leap.dt[,.(day = mean(day), radn = mean(radn), mint = mean(mint), maxt = mean(maxt), rain = mean(rain), dayl = mean(dayl)), by = year]
  table(daymet.dt$year)
  # daymet.dt2 <- rbind(daymet.dt, correction_leap.dt) %>% .[order(year, day)]
  
  # daymet.dt2[ ,day := NULL]
  # daymet.dt2[ ,day := 1:.N, by = year]
  setcolorder(daymet.dt, c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'dayl'))
  
  daymet.dt[,lat := centroid$Y]
  daymet.dt[,lon := centroid$X]
  daymet.dt[,id_10 := id_10_n]
  # hist(daymet_z_cell[,.(rain = sum(rain)), by = .(z, year)][,rain]) #check. They have to be different
  
  return(daymet.dt)
}

keep <- c('keep', 'download_weather', 'grid10_tiles_sf')

#ids_10_seq <- sort(unique(grid10_tiles_sf$id_10))


# grid10_soils_sf2 <- readRDS("./n_policy_box/Data/Grid/grid10_soils_sf2.rds")  #to avoid downloading cells w/o fields
# 
# id_10_seq <- sort(unique(grid10_soils_sf2$id_10))

id_10_seq <- sort(unique(grid10_tiles_sf$id_10))

clusterExport(cl, varlist = keep, envir=environment())

results_list <- parLapply(cl, id_10_seq, function(x) download_weather(x, grid10_tiles_sf))

# results_list <- list()
# for(id_10_n in ids_10_seq[1:10]){
#   results_list[[length(results_list)+1]] <- download_weather(id_10_n, grid10_tiles_sf)
# }

weather_historic_dt2019 <-  rbindlist(results_list)

weather_historic_dt <- readRDS('./n_policy_box/Data/met_files/weather_historic_dt.rds')
weather_historic_dt[,.N, year]
weather_historic_dt2019 <- rbind(weather_historic_dt, weather_historic_dt2019)
weather_historic_dt2019[,.N, year]
saveRDS(weather_historic_dt2019, './n_policy_box/Data/met_files/weather_historic_dt2019.rds')

stopCluster(cl)


