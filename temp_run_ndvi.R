#RUn change analysis
library('data.table')
library('parallel')
library('raster')
library('prophet')
library('data.table')
library('tools')
library('prophet')
sg = readRDS('/media/dan/summary_grid.rds')

ras2run = sg[time == "" & funk == "" & variables %in% c("NDVI"),] #'LST_Night_1km', 'EVI', 'avg_rad', 'LST_Day_1km',
#ras2run = sg[time == "" & funk == "" & variables %in% c('NDVI'),]
source('~/Documents/code/trends_harmonic_patterns/prophet.R')
setorder(ras2run, -city)
for(sss in 1:nrow(ras2run)){
#blah = lapply(10:13, function(sss) {
#blah = mclapply(10:nrow(ras2run), function(sss) {
  print(paste(Sys.time(), sss))
  fp = ras2run[sss, raspath]
  metadata = ras2run[sss,]
  #ARGS 1
  in_dir <- dirname(fp)
  #ARGS 2
  out_dir <- "/media/dan/processed/trends/"
  #ARGS 3
  inr <- basename(fp)
  #ARGS 4
  start_date <- "2004-01-01"
  #start_date <- "2012-11-01"  #new data starts in November 2012 (DCC: not really sure what this means)
  #ARGS 5
  end_date <- NULL
  #ARGS 6
  create_out_dir_param=TRUE #create a new ouput dir if TRUE
  #ARGS 7
  out_suffix <- tools::file_path_sans_ext(basename(fp)) #output suffix for the files and ouptut folder #param 12
  #ARGS 8
  num_cores <- 1 # number of cores
  #ARGS 9
  file_format <- ".tif"
  #ARGS 10
  #range_window <- c("2012-01-01","2017-01-01")
  #ARGS 11
  out_prefix <- NULL
  
  window_val_select <- 23 #windowing-- usually refers to the number of layers in a year
  
  #bbb = do_ras(sss, ras2run)
  source('~/Documents/code/trends_harmonic_patterns/change_analysis_regression.R')
  source('~/Documents/code/trends_harmonic_patterns/mankendall.R')
  print(paste(Sys.time(), sss))
  
}  
#}, mc.cores = 4, mc.preschedule = FALSE)