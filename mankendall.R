library('raster')
library('Kendall')
library('data.table')
library('tools')
#laod the raster
infile_name_raster <- file.path(in_dir,infile_name_raster)

r = readAll(brick(infile_name_raster))

mkcalc = function(x){
  if(all(is.na(x))) return(NA)
  
  hold = capture.output(a <- MannKendall(x))
  
  if(length(hold)>0 && grepl('WARNING', hold)) return(NA)
  
  return(a[[2]][1])
  
}

#make things data.tabley
blah =setDT(as.data.frame(r))
blah[, id:= .I]
blah = melt(blah, id.vars = 'id', variable.factor = F)
res = blah[, as.integer(mkcalc(value)<.05), by = 'id']


rr = r[[1]]
rr[] = res[, V1]

#save the raster
bn =  file_path_sans_ext(basename(infile_name_raster))
writeRaster(rr, filename = file.path(out_dir,paste0('output_',bn),paste0(bn, '_mksig.tif')), overwrite = TRUE)


