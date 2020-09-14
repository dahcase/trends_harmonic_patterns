library('dplyr')
library('data.table')
library('raster')
library('prophet')
library('sf')
library('ggplot2')
library('ggspatial')
library('ggthemes')
library('GADMTools')
#sen_ad3 = readRDS('/media/dan/sen_ad3.rds')
tanz = gadm_sf_loadCountries("TZA", level=3, basefile=tempdir())
tanz = tanz$sf %>% filter(NAME_1 == 'Dar es Salaam')
selpx = st_read("/media/dan/select_locs.gpkg")
selpx_dt = as.data.table(selpx)[, id:=.I]
sg = readRDS('/media/dan/summary_grid.rds')
ras2run = sg[time == "" & funk == "" & variables %in% c("NDVI") & city %in% c('DarEsSalaam'),]
in_dir <- "/media/dan/processed/trends/"

melt_ras = function(ras, value_name){
  ras = as.data.table(as.data.frame(ras))[,id := .I]
  ras = melt(ras, id.vars = 'id', value.name = value_name)
  ras[, timestep := tools::file_ext(variable)]
  
  return(ras[, .SD, .SDcols = c('id', 'timestep', value_name)])
}

ras_gg = function(ras, thetitle, limits){
  g = ggplot() + layer_spatial(ras, aes(fill = stat(band1))) +
    scale_fill_viridis_c(na.value = NA) + theme_dark() +
    ggtitle(thetitle)
  
  return(g)
}

lu_ids = data.table(id = c(1,11,2,222,22,3,5), name = c('Planned high density', 
                                                        'Planned low density',
                                                        'Informal high density',
                                                        'Informal medium density',
                                                        'Informal low density',
                                                        'Commercial, Industrial, Adminstrative',
                                                        'Green space, water, open space, marshes, agriculture'))

sss =1

layerfolder = '/media/dan/earth_engine/'
namepath = file.path(layerfolder, paste0('MODIS',ifelse(nchar(ras2run[sss,version])>0, paste0('_',ras2run[sss,version],'_'), "_"), ras2run[sss,product],'.txt'))
nnn = read.delim(namepath, header = F, stringsAsFactors = F)[,1]
if(!all(grepl('_', nnn, fixed = T))){
  nnn = paste(substr(nnn, 1,4), substr(nnn, 5,6), substr(nnn, 7,8), sep = '_')
}
nnn = as.Date(nnn, '%Y_%m_%d')
n = data.table(ds = nnn)
n[, timestep := as.character(.I)]

select_dates = seq(1:length(nnn))

# pdf(paste0('/media/dan/prophet_pixel_daressalaam', 'multiple_thresh','_2009.pdf'), width = 10, height = 9)
# 
# for(ui in c(c(.7,.8,.9,.95,.99))){
# 
#   subfol = paste('a', ui, 10, 0, sep = "_")
#   
# # make_diagnostics = function(sss){
#   bn = file_path_sans_ext(basename(ras2run[sss, raspath]))
#   base = subset(readAll(brick(ras2run[sss, raspath])), select_dates)
#   resid = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'resid', '.tif')))), select_dates)
#   upper = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat_upper', '.tif')))), select_dates)
#   lower = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat_lower', '.tif')))), select_dates)
#   xtreme = !(base<upper & base>lower)
#   
#   
#   all_na = sum(is.na(base)) == nlayers(base)
#   all_na[all_na == 1] <- NA
#   all_na[all_na == 0] <- 1
#   
#   #at least one extreme
#   al1_xtreme = xtreme >0
#   
#   #sum exteme
#   sum_xtreme = sum(xtreme, na.rm = T) * all_na
#   
#   #sum residuals
#   sum_resid = sum(resid,na.rm = T) * all_na
#   
#   #sum abs resid
#   sum_abs_resid = sum(abs(resid), na.rm = T) * all_na
#   
#   #perc obs mean
#   mean_xtreme = round(mean(xtreme, na.rm = T) * 100) * all_na
#   
#   #plot(ras_gg(sum_xtreme, paste('Sum of Extreme Values', subfol)))
#   #plot(ras_gg(sum_resid, paste('Sum of Residuals', subfol)))
#   #plot(ras_gg(sum_abs_resid, paste('Sum of Abs(Residuals)', subfol)))
#   plot(ras_gg(mean_xtreme, paste('# extreme pixels/# !na pixels', subfol)))
#   plot(ras_gg(sum_xtreme, paste('# extreme pixels',subfol)))
# 
# }
# dev.off()

#Pixel plots
for(ui in c(c(.7,.8,.9,.95,.99))){

  subfol = paste('a', ui, 10, 0, sep = "_")

# make_diagnostics = function(sss){
  bn = file_path_sans_ext(basename(ras2run[sss, raspath]))
  base = subset(readAll(brick(ras2run[sss, raspath])), select_dates)
  resid = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'resid', '.tif')))), select_dates)
  upper = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat_upper', '.tif')))), select_dates)
  lower = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat_lower', '.tif')))), select_dates)
  yhat = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat', '.tif')))), select_dates)
  xtreme = !(base<upper & base>lower)
  
  base_ex = melt_ras(raster::extract(base, selpx), 'value')[,var := 'NDVI']
  yhat_ex = melt_ras(raster::extract(yhat, selpx), 'value')[, var := 'Pred']
  xtr_ex = melt_ras(raster::extract(xtreme, selpx), 'value')[, var := 'Extreme']
  
  base_ex=  merge(base_ex, xtr_ex[, .(id, timestep, xtr = value)], all.x = T, by=  c('id', 'timestep'))
  base_ex = merge(base_ex, n, by = 'timestep', all.x = T)
  yhat_ex = merge(yhat_ex, n, by = 'timestep', all.x = T)
  
  names(xtreme) = nnn
  pdf(paste0('/media/dan/prophet_pixel_daressalaam_pixel_plots_',ui,'.pdf'), width = 10, height = 9)
  for(lll in unique(selpx$Final_Labe)){
    draw_base = base_ex[id %in% which(selpx$Final_Labe==lll)]
    draw_yhat = yhat_ex[id %in% which(selpx$Final_Labe==lll)]
    
    
    g = ggplot() + geom_point(data = draw_base[!is.na(value)], aes(x = ds, y=  value, color = xtr), size = .5) +
      theme_dark() + geom_line(data = draw_yhat, aes(x = ds, y = value)) + facet_wrap(~id) + coord_cartesian(ylim = c(0,1)) + 
      ggtitle(paste('Dar Es Salaam:', 'ui = ', ui, '|','lu = ', lu_ids[id == lll, name]))
    plot(g)
  }
  dev.off()

  
  pdf(paste0('/media/dan/prophet_pixel_daressalaam_xtr_plots_',ui,'.pdf'), width = 10.3, height = 10)
  for(yyy in unique(year(nnn[select_dates]))){
    subby = (subset(xtreme, which(year(nnn) == yyy)))

    print(spplot(subby))
  }
  dev.off()
  
  # for(ward in tanz$NAME_3){
  #   xtr_sub = crop(xtreme, filter(tanz, NAME_3 == {{ward}}))
  #   names(xtr_sub) = nnn
  #   pdf(paste0('/media/dan/prophet_pixel_daressalaam_ward_plots_',ward,'_',ui,'.pdf'), width = 10.3, height = 10)  
  #   for(yyy in unique(year(nnn[select_dates]))){
  #     subby = (subset(xtr_sub, which(year(nnn) == yyy)))
  #     
  #     spplot(subby)
  #   }
  #   dev.off()
  #   
  # }
  

}
