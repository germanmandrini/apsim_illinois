
if(server){

  if(FALSE){
    # -----------------------------------------------------------------------
    #APSIM compiled on server
    # apsim_exe <- '/opt/apsim/Model/ApsimRun.sh'
    # apsim_exe <- '/opt/apsim_dev/trunk/Model/ApsimRun.sh' #Nov2019
    # apsim_exe <- '/usr/bin/mono /home/germanm2/apsim710_r4207/APSIMClassic/Model/Apsim.exe' #German compiled version in Aug2020
    apsim_exe <- '/usr/bin/mono /opt/APSIMClassic/Model/Apsim.exe' #Rodrigo compiled version in Aug2020
    
    
    
    flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
    
    if(test_small) {
      flist <- flist[str_detect(string = flist, pattern = '_swim_160|_swat_160|_swat_212')] #OJO!!!!
    }
    
    # print(flist)
    apsim_file = file.path(directory, 'apsim.txt')
    write.table(flist, apsim_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
    
    # start <- Sys.time()
    tst = system(paste('parallel -j 28 --eta', apsim_exe, '{} <', apsim_file), ignore.stdout = T, ignore.stderr = T)
    
  
    # apsim_exe <- '/usr/bin/mono /opt/APSIMClassic/Model/Apsim.exe' 
    # apsim_file <- '/opt/APSIMClassic/test/765_2_18_stab_swat/765_2_18_stab_swat.apsim'
    # res <- suppressWarnings(system(paste(apsim_exe, apsim_file, sep = " "), show.output.on.console = FALSE))
    # apsim_exe <- '/usr/bin/mono /usr/local/APSIMClassic/Model/Apsim.exe'
  
    # print(Sys.time() - start)
  }
  # -----------------------------------------------------------------------
  #Singularity on sever
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  
  if(test_small) {
    flist <- flist[str_detect(string = flist, pattern = '_swim_160|_swat_160|_swat_212')] #OJO!!!!
  }
  apsim_file = file.path(directory, 'apsim.txt')
  write.table(flist, apsim_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  tst = system(paste('parallel -j 28 --eta', 'singularity exec /opt/apsim_nov16.simg /usr/bin/mono /usr/local/APSIMClassic/Model/Apsim.exe ', '{} <', apsim_file), ignore.stdout = T, ignore.stderr = T)
  
# -----------------------------------------------------------------------
# OLD COMMANDS
# apsim_exe = '/opt/apsim_dev/trunk/Model/ApsimModel.sh '
# apsimr::apsim
# 
# apsim_exe <- '/opt/apsim/Model/ApsimRun.sh'
# 
# apsim_exe = '/opt/apsim_dev/trunk/Model/ApsimModel.sh '
# 
# file <- "/home/germanm2/apsim_temp/vr_value/cell1094/z100_173430/temp.apsim"
# 
# res <- suppressWarnings(system(paste(apsim_exe, file, sep = " "), show.output.on.console = FALSE))

# -----------------------------------------------------------------------
}else if(cpsc){
  # WINDOWS
  library(doParallel)  
  library(stringr)
  no_cores <- detectCores() * 6/8
  registerDoParallel(cores=no_cores)  
  cl <- makeCluster(no_cores) 
  
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  
  if(test_small) {
    flist <- flist[str_detect(string = flist, pattern = '_swim_160|_swat_160|_swat_212')] #OJO!!!!
  }
  
  # system2( 'C:/Program Files (x86)/APSIM710-r4207/Model/Apsim.exe',  flist[1] )
  # result <- parLapply(cl, flist, function(x) system2( 'C:/Program Files (x86)/APSIM710-r4158/Model/Apsim.exe',  x ))  
  result <- parLapply(cl, flist, function(x) system2( 'C:/Program Files (x86)/APSIM710-r4207/Model/Apsim.exe',  x ))  
  stopCluster(cl) 

  # -----------------------------------------------------------------------
}else if(cluster){
  #CLUSTER
  # system2(paste0('singularity exec /projects/aces/germanm2/apsim_nov16.simg /usr/bin/mono /usr/local/APSIMClassic/Model/Apsim.exe ', directory, '/**/*.apsim'))
  
  # apsim_exe <- '/usr/bin/mono /usr/local/APSIMClassic/Model/ApsimRun.sh'
  apsim_exe <- '/usr/bin/mono /usr/local/APSIMClassic/Model/Apsim.exe'

  
  flist = list.files(directory, full.names = TRUE, recursive = TRUE, pattern = '.apsim')
  
  if(test_small) {
    flist <- flist[str_detect(string = flist, pattern = '_swim_160|_swat_160|_swat_212')] #OJO!!!!
  }
  
  print(flist)
  file_name = file.path(directory, 'files_to_run.txt')
  write.table(flist, file_name, row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  tst = system(paste('parallel -j 28 --eta', apsim_exe, '{} <', file_name), ignore.stdout = T, ignore.stderr = T)

  
}

