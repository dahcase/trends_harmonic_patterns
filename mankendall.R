library('raster')
library('Kendall')
library('data.table')
library('tools')
#laod the raster
infile_name_raster <- file.path(in_dir,inr)

r = readAll(brick(infile_name_raster))

mkcalc = function(x){
  if(all(is.na(x))) return(as.numeric(NA))
  
  hold = capture.output(a <- MannKendall(x))
  
  if(length(hold)>0 && grepl('WARNING', hold)) return(as.numeric(NA))
  
  return(as.numeric(a[[2]][1]))
  
}

#make things data.tabley
blah =setDT(as.data.frame(r))
blah[, id:= .I]
blah = melt(blah, id.vars = 'id', variable.factor = F)
#res = blah[, as.integer(mkcalc(value)<.05), by = 'id']
res = blah[, mkcalc(value), by = 'id']

# ddd = lapply(unique(blah$id), function(x){
#   print(x)
#   mkcalc(blah[id == x, value])
# })

rr = r[[1]]
rr[] = res[, V1]

#save the raster
bn =  file_path_sans_ext(basename(infile_name_raster))
writeRaster(rr, filename = file.path(out_dir,paste0('output_',bn),paste0(bn, '_mksig.tif')), overwrite = TRUE)


