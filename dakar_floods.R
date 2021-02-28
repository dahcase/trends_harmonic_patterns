library('dplyr')
library('data.table')
library('raster')
library('prophet')
library('sf')
library('ggplot2')
library('ggspatial')
library('ggthemes')
library('GADMTools')
library('latticeExtra')
#sen_ad3 = readRDS('/media/dan/sen_ad3.rds')

floods = st_read('/media/dan/Flood_Areas/Flooded_areas_2005to12_latlong.shp')
floods = floods %>% summarize()
sen = gadm_sf_loadCountries("SEN", level=4, basefile=tempdir())
sen = sen$sf %>% filter(NAME_1 == 'Dakar') %>% filter(NAME_4 == "Yeumbeul Nord")
sg = readRDS('/media/dan/summary_grid.rds')
ras2run = sg[time == "" & funk == "" & variables %in% c("NDVI") & city %in% c('Dakar'),]
in_dir <- "/media/dan/processed/trends/"
thresh = 1.65

melt_ras = function(ras, value_name){
  ras = as.data.table(as.data.frame(ras))[,id := .I]
  ras = melt(ras, id.vars = 'id', value.name = value_name)
  ras[, timestep := tools::file_ext(variable)]
  
  return(ras[, .SD, .SDcols = c('id', 'timestep', value_name)])
}

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

#Pixel plots
ui = .8

subfol = paste('a', ui, 10, 0, sep = "_")

#Load rasters
bn = file_path_sans_ext(basename(ras2run[sss, raspath]))
base = subset(readAll(brick(ras2run[sss, raspath])), select_dates)
resid = -1 * subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'resid', '.tif')))), select_dates)
upper = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat_upper', '.tif')))), select_dates)
lower = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat_lower', '.tif')))), select_dates)
yhat = subset(readAll(brick(file.path(in_dir,paste0('output_',bn),subfol,paste0(bn, '_', 'yhat', '.tif')))), select_dates)
xtreme = !(base<upper & base>lower)


#vectorize the grid
gridras = base[[1]]
gridras[] <- 1:ncell(gridras)
grid = st_as_sf(rasterToPolygons(gridras))
names(grid) <- c('id', 'geometry')
f_cells = st_intersects(grid, floods, sparse = F)
f_grid = grid[f_cells,]
f_grid$start = as.numeric(st_area(f_grid))
f_grid = st_intersection(f_grid, floods)
f_grid$end = as.numeric(st_area(f_grid))
f_grid = f_grid %>% mutate(frac = end/start)

f_cells_id = f_grid %>% filter(frac>.2) %>% pull('id')


#extract by the subnational unit
fbox =  st_sf(id = 1, st_as_sfc(st_bbox(floods)))
base_ex = melt_ras(raster::extract(base, fbox), 'value')[,var := 'NDVI']
yhat_ex = melt_ras(raster::extract(yhat, fbox), 'value')[, var := 'Pred']
xtr_ex = melt_ras(raster::extract(xtreme, fbox), 'value')[, var := 'Extreme']
gras_ex = melt_ras(raster::extract(gridras, fbox), 'value')[,var := 'id']
resid_ex = melt_ras(raster::extract(resid, fbox), 'value')[,var := 'resid']

#add values
base_ex=  merge(base_ex, xtr_ex[, .(id, timestep, xtr = value)], all.x = T, by=  c('id', 'timestep'))
base_ex = merge(base_ex, n, by = 'timestep', all.x = T)
yhat_ex = merge(yhat_ex, n, by = 'timestep', all.x = T)

base_ex[, floody := id %in% gras_ex[value %in% f_cells_id, id]]
base_ex[, `Flood Prone` := factor(floody, c(T,F), c("Flood Recorded", "Flood Not Recorded"))]

base_ex = merge(base_ex, yhat_ex[, .(id, timestep, Pred = value)], all.x = T, by = c('id', 'timestep'))
pdffp =paste0('/media/dan/dakar_summary_flood_1.6thresh_xtra_3.pdf')
pdf(pdffp, width = 8.5, height = 9)

#Map of NDVI over time, colored by whether a pixel is flooded
f_px = sample(unique(base_ex[floody == TRUE, id]), 5)
nf_px = sample(unique(base_ex[floody == FALSE, id]), 5)
base_ex[, Year := year(ds)]
g1 = ggplot(data = base_ex[id %in% c(f_px, nf_px) & Year %in% c(2005,2009,2012)], aes(x = ds, y = value, group = id, color = as.factor(id))) + 
  geom_line() +
  theme_dark() +
  facet_wrap(~`Flood Prone` + Year, scale = 'free_x') +
  theme(legend.position = 'none') +
  xlab('Date') +
  ylab('NDVI') +
  ggtitle('Flood vs. Not Flooded, Observed NDVI') +
  geom_vline(xintercept = as.Date(c('2005-08-20', '2005-09-10', '2009-08-09','2009-09-20','2012-08-15','2012-08-31')))

plot(g1)

gdat = base_ex[id %in% c(f_px) & Year %in% c(2005,2009,2012), .(id, ds, `Flood Prone`, Year, Predicted = Pred, Observed = value)]
gdat = melt(gdat, id.vars = c('id','ds', 'Year', 'Flood Prone'))
g2 = ggplot(data = gdat, aes(x = ds, color = variable, y = value)) + 
  theme_dark() +
  geom_line() + 
  facet_wrap(~Year + id, scale = 'free_x') +
  xlab('Date') +
  ylab('NDVI') +
  geom_vline(xintercept = as.Date(c('2005-08-20', '2005-09-10', '2009-08-09','2009-09-20','2012-08-15','2012-08-31')))

plot(g2)


gdat = base_ex[id %in% c(f_px) & Year %in% c(2005,2009,2012), .(id, ds, `Flood Prone`, Year, Predicted = Pred, Observed = value)]
gdat[, Residual := Predicted - Observed]
ressd = base_ex[id %in% c(f_px), sd(value-Pred, na.rm = T), by = id]
ressd = rbind(ressd[, .(V1 = V1 * thresh, id)], ressd[, .(V1 = V1 * -1 * thresh, id)])
ressd = rbindlist(lapply(c(2005,2009,2012), function(x) copy(ressd)[, Year := x]))
g3 = ggplot(data = gdat, aes(x = ds, y = Residual)) + 
  theme_dark() +
  geom_line() + 
  facet_wrap(~Year + id, scale = 'free_x') +
  xlab('Date') +
  ylab('NDVI') +
  geom_hline(data = ressd, aes(yintercept = V1), color = 'blue') + 
  geom_vline(xintercept = as.Date(c('2005-08-20', '2005-09-10', '2009-08-09','2009-09-20','2012-08-15','2012-08-31')))

plot(g3)

#2005 Flood
dates05 = which(year(nnn) == 2005 & month(nnn) %in% 7:11)
flood05 = subset(crop(base, fbox), dates05)
flood05_resid = subset(crop(resid, fbox), dates05)
names(flood05) <- nnn[dates05]
names(flood05_resid) <- nnn[dates05]

#2009 Flood
dates09 = which(year(nnn) == 2009 & month(nnn) %in% 7:11)
flood09 = subset(crop(base, fbox), dates09)
flood09_resid = subset(crop(resid, fbox), dates09)
names(flood09) <- nnn[dates09]
names(flood09_resid) <- nnn[dates09]

#2012 Flood
dates12 = which(year(nnn) == 2012 & month(nnn) %in% 7:11)
flood12 = subset(crop(base, fbox), dates12)
flood12_resid = subset(crop(resid, fbox), dates12)
names(flood12) <- nnn[dates12]
names(flood12_resid) <- nnn[dates12]

sd_resid = crop(calc(resid, fun= sd, na.rm = T), fbox)

spplot(flood05_resid, main=list(label="Residuals, NDVI, 2005")) + latticeExtra::layer(sp.polygons(as(floods, 'Spatial')))
spplot(flood05_resid>(thresh*sd_resid) | flood05_resid<(-1 * thresh *sd_resid), main=list(label="Anomaly, NDVI, 2005"), col.regions = c('Gray50', 'Yellow') , colorkey = FALSE) + latticeExtra::layer(sp.polygons(as(floods, 'Spatial')))
spplot(flood09_resid, main=list(label="Residuals, NDVI, 2009")) + latticeExtra::layer(sp.polygons(as(floods, 'Spatial')))
spplot(flood09_resid>(thresh*sd_resid) | flood09_resid<(-1 * thresh *sd_resid), main=list(label="Anomaly, NDVI, 2009"), col.regions = c('Gray50', 'Yellow') , colorkey = FALSE ) + latticeExtra::layer(sp.polygons(as(floods, 'Spatial')))
spplot(flood12_resid, main=list(label="Residuals, NDVI, 2012")) + latticeExtra::layer(sp.polygons(as(floods, 'Spatial')))
spplot(flood12_resid>(thresh*sd_resid) | flood12_resid<(-1 * thresh *sd_resid), main=list(label="Anomaly, NDVI, 2012"), col.regions = c('Gray50', 'Yellow') , colorkey = FALSE ) + latticeExtra::layer(sp.polygons(as(floods, 'Spatial')))


#compute moving window
sd_resid = calc(resid, fun= sd, na.rm = T)
anomaly = resid>(thresh*sd_resid) | resid<(-1 * thresh *sd_resid)
twostep = brick(lapply(1:nlayers(anomaly), function(x){
  
  if((x+1)<=nlayers(anomaly)){
    return(anomaly[[x]] & anomaly[[x+1]])
  }else{
    return(anomaly[[x]])
  }
}))

consect_anom = sum(twostep, na.rm = T)
anomaly_na = reclassify(anomaly, matrix(c(0, NA, 1,1,NA,NA), ncol = 2, byrow = T))
clumps = brick(lapply(1:nlayers(anomaly_na), function(x){
  cl = clump(anomaly_na[[x]])
  
  cl_n = setDT(as.data.frame(cl))
  clump1 = cl_n[, .N, by = clumps][N<=1, clumps]
  cl[cl%in% clump1] <- NA
  
  cl>0
}))

clumps[is.na(clumps) & !is.na(anomaly)] <- 0 

n_clumps = sum(clumps, na.rm = T)

spplot(consect_anom, main = list(label = '# of consecutive anomalies'))
spplot(n_clumps, main = list(label = '# of clump appearances'))

plot_ts = function(ras, dates, title = 'A map'){
  dat = subset(crop(ras, fbox), dates)
  
  d = df_spatial(dat)
  setDT(d)
  setnames(d, c("x","y", as.character(nnn[dates])))
  d = melt(d, id.vars = c('x','y'))
  d[, value := factor(value, c(0,1), c('No', 'Yes'))]
  
  g = ggplot() + 
    facet_wrap(~variable) + geom_raster(data = d, aes(x = x, y = y, fill = value)) +
    scale_fill_manual(values = c('No' = 'Gray50', 'Yes' = 'Yellow'), drop = FALSE, name = 'Presence') +
    theme_minimal() + 
    geom_sf(data = floods, fill = NA, color = 'black') + 
    theme(axis.ticks = element_blank(), axis.title = element_blank(), legend.position = 'bottom') +
    coord_sf(datum = NA) +
    ggtitle(title)
  
  plot(g)
  
}

plot_ts(twostep, dates05, title = 'Consecutive Anomlies, 2005')
plot_ts(twostep, dates09, title = 'Consecutive Anomlies, 2009')
plot_ts(twostep, dates12, title = 'Consecutive Anomlies, 2012')

plot_ts(clumps, dates05, title = 'Clump Presence, 2005')
plot_ts(clumps, dates09, title = 'Clump Presence, 2009')
plot_ts(clumps, dates12, title = 'Clump Presence, 2012')


# herp = crop(resid, sd_resid)
# blah = (herp > (thresh * sd_resid)) | (herp < (-thresh * sd_resid))
# sdr = calc(resid, sd, na.rm = T)
# blah2 = (resid > (thresh * sdr)) | (resid < (-thresh * sdr))
# a = sum(blah2, na.rm = T)
# b = sum(!is.na(resid))

#NDVI for flooded pixels
#Map of NDVI over time, colored by whether a pixel is flooded
gdat = copy(base_ex[Year %in% c(2005,2009,2012)])[, .(value = mean(value, na.rm= T), Pred = mean(Pred)), by = c('timestep','ds','Flood Prone', 'Year')]
gdat = melt(gdat, id.vars =c('timestep', 'ds', 'Flood Prone', "Year"))
gdat[, variable := factor(variable, c('value', 'Pred'), c('Observed', 'Predicted'))]
g4 = ggplot(data = gdat, aes(x = ds, y = value, color = variable)) + 
  geom_line() +
  theme_dark() +
  facet_wrap(~`Flood Prone` + Year, scale = 'free_x') +
  scale_color_manual(values = c('purple', 'orange')) + 
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  xlab('Date') +
  ylab('NDVI') +
  ggtitle('Avg. NDVI by flood status') +
  geom_vline(xintercept = as.Date(c('2005-08-20', '2005-09-10', '2009-08-09','2009-09-20','2012-08-15','2012-08-31')))

plot(g4)

#NDVI for xtreme pixels
gdat = copy(base_ex[Year %in% c(2005,2009,2012) ])
ressd = base_ex[, sd(Pred - value, na.rm = T), by = id]
gdat = merge(gdat, ressd, all.x = T, by = 'id')
gdat[, Residual := Pred - value]
gdat[, Extreme := Residual > thresh * V1 | Residual < -1 * thresh * V1 ]
xtr = gdat[ds %in% nnn[c(dates05,dates09,dates12)] & Extreme == TRUE, .(id = unique(id)), by = Year]
gdat = merge(gdat, xtr, by = c('id', 'Year'))

gdat = gdat[, .(Observed = mean(value,na.rm = T), Predicted = mean(Pred, na.rm = T)), by = .(ds, `Flood Prone`, Year)]
gdat = melt(gdat, id.vars =c('ds', 'Flood Prone', "Year"))
g5 = ggplot(data = gdat, aes(x = ds, y = value, color = variable)) + 
  geom_line() +
  theme_dark() +
  facet_wrap(~`Flood Prone` + Year, scale = 'free_x') +
  scale_color_manual(values = c('purple', 'orange')) + 
  theme(legend.position = 'bottom', legend.title = element_blank()) +
  xlab('Date') +
  ylab('NDVI') +
  ggtitle('Avg. NDVI among "extreme" pixels by flood status') +
  geom_vline(xintercept = as.Date(c('2005-08-20', '2005-09-10', '2009-08-09','2009-09-20','2012-08-15','2012-08-31')))

plot(g5)

dev.off()

pdftools::pdf_convert(pdffp, dpi = 300)

# names(xtreme) = nnn
# pdf(paste0('/media/dan/prophet_pixel_daressalaam_pixel_plots_',ui,'.pdf'), width = 10, height = 9)
# for(lll in unique(selpx$Final_Labe)){
#   draw_base = base_ex[id %in% which(selpx$Final_Labe==lll)]
#   draw_yhat = yhat_ex[id %in% which(selpx$Final_Labe==lll)]
#   
#   
#   g = ggplot() + geom_point(data = draw_base[!is.na(value)], aes(x = ds, y=  value, color = xtr), size = .5) +
#     theme_dark() + geom_line(data = draw_yhat, aes(x = ds, y = value)) + facet_wrap(~id) + coord_cartesian(ylim = c(0,1)) + 
#     ggtitle(paste('Dar Es Salaam:', 'ui = ', ui, '|','lu = ', lu_ids[id == lll, name]))
#   plot(g)
# }
# dev.off()
