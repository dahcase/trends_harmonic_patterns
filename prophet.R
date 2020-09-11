# library('raster')
# library('prophet')
# library('data.table')
# library('tools')
# library('prophet')

#laod the raster
do_ras = function(sss, ras2run, ui = .8, sps = 10, nmcmc = 0){
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
  out_suffix <- file.path(tools::file_path_sans_ext(basename(fp))) #output suffix for the files and ouptut folder #param 12
  #ARGS 8
  num_cores <- 6 # number of cores
  #ARGS 9
  file_format <- ".tif"
  #ARGS 10
  #range_window <- c("2012-01-01","2017-01-01")
  #ARGS 11
  out_prefix <- NULL
  subfol = paste('a', ui, sps, nmcmc, sep = "_")
  
  window_val_select <- 23 #windowing-- usually refers to the number of layers in a year
  r = readAll(brick(file.path(in_dir,infile_name_raster)))
  
  #hold = r
  template = r * NA
  #pull the dates
  layerfolder = '/media/dan/earth_engine/'
  namepath = file.path(layerfolder, paste0('MODIS',ifelse(nchar(metadata[,version])>0, paste0('_',metadata[,version],'_'), "_"), metadata[,product],'.txt'))
  nnn = read.delim(namepath, header = F, stringsAsFactors = F)[,1]
  if(!all(grepl('_', nnn, fixed = T))){
    nnn = paste(substr(nnn, 1,4), substr(nnn, 5,6), substr(nnn, 7,8), sep = '_')
  }
  #convert to date paths
  nnn = as.Date(nnn, '%Y_%m_%d')
  n = data.table(ds = nnn)
  n[, timestep := .I]
  
  r = as.data.table(as.data.frame(r))[, id := .I]
  r = melt(r, id.vars = c('id'), variable.factor = FALSE)
  r[, timestep := as.numeric(substr(variable, regexpr('.', variable, fixed = TRUE) +1, nchar(variable)))]
  r = merge(r, n, by = 'timestep', all.x = T)
  weights = r[, sum(!is.na(value))/.N, by = id]
  
  pixel_fit = function(x, dates, sps = 10, ui = .8, nmcmc = 0){
    dat = data.frame(ds = dates, y = x)
    a <- try(p <- prophet(dat, weekly.seasonality = FALSE,
                          daily.seasonality = FALSE,
                          seasonality.prior.scale = sps,
                          interval.width = ui,
                          mcmc.samples = nmcmc))
    return(a)
  }
  
  
  get_preds = function(px, oex, sps = 10, ui = .8, nmcmc = 0){
    mods = lapply(px, function(x) pixel_fit(oex[id %in% x, value], oex[id %in% x, ds], sps = sps, ui = ui, nmcmc = nmcmc))
    stopifnot(all(vapply(mods, function(x) inherits(x, 'prophet'), TRUE)))
    preds = lapply(px, function(x) setDT(predict(mods[[which(px %in% x)]], oex[id %in% x]))[,id:=x])
    preds = rbindlist(preds)
    preds[, ds := as.Date(ds)]
    oex[, type := 'original']
    #oex = oex[id %in% px]
    oex = merge(oex, preds, all.x = T, by = c('id','ds'))
    #res = rbind(oex[id %in% px], preds, fill = T)
    
    return(oex)
  }
  
  ppp = get_preds(px = weights[V1>.1][,id], r, sps = sps, ui = ui, nmcmc = nmcmc)
  bn =  file_path_sans_ext(basename(infile_name_raster))
  ppp = setorder(ppp, timestep, id)
  ppp[, resid := value - yhat]
  ppp[, resid_trend := value - trend]
  ppp[, outofui := !between(value, yhat_lower, yhat_upper)]
  
  dir.create(file.path(out_dir,paste0('output_',bn),subfol), recursive = TRUE)
  
  saveRDS(ppp, file =  file.path(out_dir,paste0('output_',bn),subfol,paste0(bn, '_prophet_preds.rds')))
  
  for(yyy in c('yhat', 'yhat_lower', 'yhat_upper', 'resid', 'resid_trend')){
    res = setValues(template, ppp[,get(yyy)])
    writeRaster(res, filename = file.path(out_dir,paste0('output_',bn),subfol,paste0(bn, '_', yyy, '.tif')), overwrite = TRUE)
  }

  #writeRaster(preds, filename = file.path(out_dir,paste0('output_',bn),paste0(bn, '_prophet_preds.tif')), overwrite = TRUE)
  #writeRaster(resids, filename = file.path(out_dir,paste0('output_',bn),paste0(bn, '_prophet_resids.tif')), overwrite = TRUE)
  return(file.path(out_dir,paste0('output_',bn),subfol))
}


