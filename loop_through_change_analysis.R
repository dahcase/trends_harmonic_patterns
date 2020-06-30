#RUn change analysis
library('data.table')

sg = readRDS('/media/dan/summary_grid.rds')

ras2run = sg[time == "" & funk == "" & variables == 'LST_Night_1km',]

for(sss in 1:nrow(ras2run)){
  print(paste(Sys.time(), sss))
  fp = ras2run[sss, raspath]
  
  #ARGS 1
  in_dir <- dirname(fp)
  #ARGS 2
  out_dir <- "/media/dan/processed/trends/"
  #ARGS 3
  infile_name_raster <- basename(fp)
  #ARGS 4
  #start_date <- "2004-01-01"
  start_date <- "2012-11-01"  #new data starts in November 2012 (DCC: not really sure what this means)
  #ARGS 5
  end_date <- NULL
  #ARGS 6
  create_out_dir_param=TRUE #create a new ouput dir if TRUE
  #ARGS 7
  out_suffix <- tools::file_path_sans_ext(basename(fp)) #output suffix for the files and ouptut folder #param 12
  #ARGS 8
  num_cores <- 6 # number of cores
  #ARGS 9
  file_format <- ".tif"
  #ARGS 10
  #range_window <- c("2012-01-01","2017-01-01")
  #ARGS 11
  out_prefix <- NULL
  
  window_val_select <- 46 #windowing-- usually refers to the number of layers in a year
  
  source('~/Documents/code/trends_harmonic_patterns/change_analysis_regression.R')
  
  print(paste(Sys.time(), sss))
  
  
}