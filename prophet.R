# library('raster')
# library('prophet')
# library('data.table')
# library('tools')
# library('prophet')

#laod the raster
do_ras = function(sss, ras2run){
  fp = ras2run[sss, raspath]
  metadata = ras2run[sss,]
  #ARGS 1
  in_dir <- dirname(fp)
  #ARGS 2
  out_dir <- "/media/dan/processed/trends/"
  #ARGS 3
  infile_name_raster <- basename(fp)
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
  num_cores <- 6 # number of cores
  #ARGS 9
  file_format <- ".tif"
  #ARGS 10
  #range_window <- c("2012-01-01","2017-01-01")
  #ARGS 11
  out_prefix <- NULL
  
  window_val_select <- 23 #windowing-- usually refers to the number of layers in a year
r = readAll(brick(file.path(in_dir,infile_name_raster)))

#pull the dates
layerfolder = '/media/dan/earth_engine/'
namepath = file.path(layerfolder, paste0('MODIS',ifelse(nchar(metadata[,version])>0, paste0('_',metadata[,version],'_'), "_"), metadata[,product],'.txt'))
nnn = read.delim(namepath, header = F, stringsAsFactors = F)[,1]
if(!all(grepl('_', nnn, fixed = T))){
  nnn = paste(substr(nnn, 1,4), substr(nnn, 5,6), substr(nnn, 7,8), sep = '_')
}
#convert to date paths
nnn = as.Date(nnn, '%Y_%m_%d')


pixel_fit = function(x, dates){
  dat = data.frame(ds = dates, y = x)
  a <- try(p <- prophet(dat, weekly.seasonality = FALSE, daily.seasonality = FALSE))
  if(!inherits(a, 'try-error')){
    res <- predict(p, dat)$yhat
  }else{
    res <- rep(NA, nrow(dat))
  }
  return(res)
}


preds = calc(r, fun = function(x) pixel_fit(x, nnn))
resids = r - preds

bn =  file_path_sans_ext(basename(infile_name_raster))
writeRaster(preds, filename = file.path(out_dir,paste0('output_',bn),paste0(bn, '_prophet_preds.tif')), overwrite = TRUE)
writeRaster(resids, filename = file.path(out_dir,paste0('output_',bn),paste0(bn, '_prophet_resids.tif')), overwrite = TRUE)
return(bn)
}

