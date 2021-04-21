setwd('C:/Users/germa/Box Sync/My_Documents') #dell
# setwd('C:/Users/germanm2/Box Sync/My_Documents')#CPSC
# setwd("/home/germanm2")
# setwd('~')

source('./Codes_useful/R.libraries.R')
setwd("~/apsim_illinois_git/Codes")


details_df <- file.info(list.files(pattern="*.R"))

codes_index_dt <- data.table(files = rownames(details_df),
                             mtime = details_df$mtime)

codes_index_dt <- codes_index_dt[with(codes_index_dt, order(-as.POSIXct(mtime))), ]
codes_index_dt[,mtime := as.Date(as.character(codes_index_dt$mtime))]

fwrite(codes_index_dt, "codes_index.csv")
