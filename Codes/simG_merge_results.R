#COLLECT AND MERGE THE DATA
library(data.table)
library(stringr)
library(tools)
# library(dplyr)

# setwd('/projects/aces/germanm2/')
# if(cpsc){setwd('C:/Users/germanm2/Box Sync/My_Documents')}#CPSC

apsim_merge_data <- function(out_file_n){
  # directory_output= paste0('./vr_value/Data/initial_conditions/cell_', id10_n)
  # directory_output= paste0('./vr_value/Data/yc_output/cell_', id10_n)
  # out_file_n = out_files_tmp[1]
  
  # results_collection_ls <- list()
  res <- try(fread(out_file_n, header = T), TRUE)
  
  res <- res[-1, ]
  
  if ("Date" %in% colnames(res)) {
    res$Date <- as.Date(res$Date, format = c("%d/%m/%Y"))
  }
  
  # save_SP <- str_detect(out_file_n, pattern = '_0.out') #save all the SP for the one that has 0 N (just to pick one)
  
  # if(!save_SP){
  #   res <- res[year > 2009]
  # }
  
  exclude <- c('Date', 'stage', 'stage_name')
  res_col_names <- names(res)[!names(res) %in% exclude]
  
  suppressWarnings( res[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names])
  
  names(res) <- gsub('(\\()([0-9]+)(\\))$', '_\\2', names(res))
  names(res) <- gsub('\\()', '', names(res))
  
  name_sim <- basename(file_path_sans_ext(out_file_n))
  info <- strsplit(name_sim, split = '_')[[1]]
  res <- cbind(data.table(sim_name = name_sim,
                          id_10 = info[1],
                          mukey = info[2],
                          z = info[3], water = info[5]),    res)
  
  
}# end of out_file loop
#COLLECT AND MERGE THE DATA
# library(lubridate)


# id10_n = as.numeric(commandArgs(trailingOnly=TRUE)[1])
# if(cpsc) id10_n = 561


# directory_cell <- paste('./n_policy/apsim_temp/cell', id10_n, sep = '')
# if(cpsc){directory_cell <- paste('C:/apsim_temp/CPSC-P10E53323/n_policy/cell', id10_n, sep = '')}
# directory_cell <- paste('./cell', id10_n, sep = '')

out_files_dt <- data.table(path = list.files(directory, pattern = '.out', full.names = T, recursive = T)) #all out files in one folder
out_files_dt[,basename_f := file_path_sans_ext(basename(path))]

out_files_dt[,mukey := sapply(strsplit(basename_f, split="_"), "[", 2) ]
out_files_dt[,z := as.integer(sapply(strsplit(basename_f, split="_"), "[", 3)) ]
# out_files_dt[,mukey_z := paste(mukey, z, sep = '_')]

out_files_dt2 <- data.table()
mukey_seq <- unlist(unique(out_files_dt$mukey))

for(mukey_n in mukey_seq){
  # mukey_n <- mukey_seq[1]
  out_files_tmp <- out_files_dt[mukey == mukey_n]$path
  
  results_collection_ls <- lapply(out_files_tmp, function(out_file_n) apsim_merge_data(out_file_n))
  
  #SAVE THE OUTPUT
  file_output_name <- paste('./n_policy_box/Data/','yc_output','_',batch_n,'_', water_n, '/', id10_n,"_",mukey_n, '.rds', sep = '')
  # if(cpsc){file_output_name <- paste('S:/Bioinformatics Lab/germanm2/n_policy/',stab_or_yc, id10_n,"_",mukey_n, '.rds', sep = '')}
  
  if(!file.exists(dirname(file_output_name))){ dir.create(dirname(file_output_name), recursive = TRUE) }
  
  saveRDS(rbindlist(results_collection_ls, fill = TRUE), file_output_name)
  
  out_files_dt2 <- rbind(out_files_dt2, 
                         data.table(out_files_dt[path %in% out_files_tmp][,-'path'], 
                                    rows = sapply(results_collection_ls, nrow)))
  
  
}#end of mukey_n loop

out_files_dt2[,mukey := as.character(mukey)]
instructions[,mukey := as.character(mukey)]
instructions_stab <- merge(instructions, out_files_dt2[,.(mukey,  z, rows)], by = c('mukey', 'z'), suffixes = c(".x", ""))

print(instructions_stab[order(rows)])

if(FALSE){
  #COMPARE THE CONTINUOUS SIMULATION VS THE SEQUENTIAL
  # COMPARE THE YIELD
  ic_files <- list.files(paste0('./n_policy_box/Data/initial_conditions'), full.names = TRUE)
  ic_files <- ic_files[str_detect(basename(ic_files), pattern = paste0(id10_n, '_'))]
  ic_dt <- data.table()
  for(file_n in ic_files){
    ic_dt <- rbind(ic_dt, readRDS(file_n))}
  
  yc_files <- list.files(paste0('./n_policy_box/Data/yc_output'), full.names = TRUE)
  yc_files <- yc_files[str_detect(basename(yc_files), pattern = paste0(id10_n, '_'))]
  yc_dt <- data.table()
  for(file_n in yc_files){
    yc_dt <- rbind(yc_dt, readRDS(file_n))}
  
  sim_name_n <- unique(yc_dt$sim_name)[str_detect(unique(yc_dt$sim_name), pattern = '_150')]
  ic_dt2 <- ic_dt[year %in% c(2001:2010), .(z, mukey, year, day, sim_name, fertiliser, Y_ic = Y)]
  yc_dt2 <- yc_dt[year %in% c(2009, 2010) & sim_name %in% sim_name_n,.(z, mukey, year, day, sim_name, fertiliser, Y_yc = Y)]
  
  ic_dt2 <- ic_dt2[,.(Y_ic = max(Y_ic, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, sim_name)]
  yc_dt2 <- yc_dt2[,.(Y_yc = max(Y_yc, na.rm = T), fertiliser = sum(fertiliser)), by = .(z, mukey, year, sim_name)]
  
  paired <- merge(ic_dt2[,-'sim_name'], yc_dt2[,-'sim_name'], by = c('z', 'mukey', 'year', 'fertiliser'), al.x = T)
  res_col_names <- c('Y_yc', 'Y_ic')
  paired[, (res_col_names) := lapply(.SD, as.numeric), .SDcols = res_col_names]
  library(ggplot2)
  
  ggplot(data=paired, aes(x = Y_ic, y = Y_yc)) +
    geom_point()+ theme(aspect.ratio=1) + coord_fixed() + geom_abline() + ylim(8000, 12000)+ xlim(8000, 12000) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    ggtitle('Sequential vs Continuous') +
    theme_bw()
  
  paired[,Y_diff := abs(Y_ic - Y_yc)][order(-Y_diff)]
  # CHECK THE IC
  fc <- initial_conditions[year == 2008 & day == 366][1]
  ic <- yc_results[year == 2009 & day == 1][2]
  
  rbind(fc, initial_conditions_tmp)
  
  # SHOW YIELD CURVES
  yc <- yc2[year == 2010]
  yc[,mukey_rotation := paste(mukey, rotation, sep = '_')]
  
  ggplot(data=yc, aes(x = fertiliser, y = Y_yc, color = mukey_rotation)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    ggtitle(paste('Summarized Response by mukey'))
}#end of the graph part