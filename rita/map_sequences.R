library('terra')
library('glue')
library('rasterVis')
library('ggplot2')
library('sf')
library('data.table')
source('/home/dan/Documents/code/trends_harmonic_patterns/rita/mannKendall.R')
ooot = '/media/dan/rita/outputs/output_rita_ndvi2/' 
yyy = 1:16
a1 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A_1.tif'))
a2 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A_2.tif'))
a0_1 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A0_1.tif'))
a0_2 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A0_2.tif'))
p1 = file.path(ooot, glue('rita_ndvi_year_{yyy}_phase_1.tif'))
p2 = file.path(ooot, glue('rita_ndvi_year_{yyy}_phase_2.tif'))
theilsen = list.files(ooot, pattern = 'theil_sen.tif', full.names = T)
ndvi = "/media/dan/rita/VI_16Days_1Km_v6/rita_ndvi.tif"
vars = list(A1 = a1, A2 = a2, A0_1 =a0_1, A0_2 = a0_2, `Phase 1` = p1, `Phase 2` = p2, NDVI = ndvi)
area = "/media/dan/rita/studyarea/new_strata_rita_10282017.shp"
area = st_read(area, quiet = T)
area = st_transform(area, crs(rast(ndvi)))
make_map = function(x, title = '', years = 2001:2016){
  
  if(!inherits(x, 'SpatRaster')){
    stopifnot(all(file.exists(x)))
    r = rast(x)
  } else{
    r = x
  }
  
  if(!is.null(years)){
    stopifnot(length(years) == length(names(r)))
    names(r) = years
  }
  
  
  #set anything over the 99th pctile to NA for graphing
  r[r>quantile(r[], .99, na.rm = T)] = NA
  r[r<quantile(r[], .01, na.rm = T)] = NA
  
  b = gplot(r) + geom_tile(aes(fill = value)) +
    facet_wrap(~ variable) +
    scale_fill_gradient(low = 'white', high = 'blue') +
    coord_equal() +
    theme_minimal() + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(), 
          axis.title = element_blank()) +
    ggtitle(title)
  b
  
}
mapz = lapply(seq_along(vars)[-length(vars)], function(x) make_map(vars[[x]], title = names(vars)[x]))
pdf(file.path('/media/dan/rita/outputs/viz/rita_sta_out.pdf'), height =6, width = 11)
mapz
dev.off()

r = rast(ndvi)
dts = substr(names(r),nchar(names(r))-7, nchar(names(r)))
dts = as.Date(dts, '%Y_%j')

#graph over time by region
timexregion = function(ras, title, area, layer_dates = 2001:2016, date_min = -Inf, date_max = Inf){
  r = rast(ras)
  ex = terra::extract(r,as(area, 'SpatVector'), fun = mean, na.rm = T)
  
  setnames(ex, c('ID', as.character(layer_dates)))
  
  ex$ID = factor(ex$ID, 1:2, c('Not Flooded', 'Flooded'))
  setDT(ex)
  
  ex = melt(ex, id.vars = 'ID')
  ex[, variable := as.Date(variable)]
  ex = ex[variable >= date_min & variable <= date_max]
  g = ggplot(ex, aes(x = variable, y = value, group = ID)) + geom_line(size = 1.5) +
    facet_wrap(~ID) + geom_smooth(method = lm) + ggtitle(title) + xlab('Date') +
    geom_vline(xintercept = as.Date('2005-09-22'), color = 'red') + theme_bw()
  
  g
  
}

graphs = lapply(seq_along(vars), function(x){
  
  if(names(vars)[x] == 'NDVI'){
    mn = as.Date('2004-01-01')
    mx = as.Date('2006-12-31')
  }else{
    mx = Inf
    mn = -Inf
  }
  timexregion(vars[[x]], title = names(vars)[x], layer_dates = switch(names(vars)[x], NDVI = dts, paste0(2001:2016, '-07-02')), area = area, date_min = mn, date_max = mx)
})
pdf(file.path('/media/dan/rita/outputs/viz/rita_region_plots.pdf'), height =8, width = 10)
graphs
dev.off()

#thiel sen maps
ts_titles = gsub('rita_ndvi_trend_ts_', "", basename(theilsen), fixed = T)
ts_titles = gsub('_slope_theil_sen.tif', '', ts_titles, fixed = T)
tsmaps = lapply(seq_along(theilsen), function(x) make_map(theilsen[x], NULL, paste0('Theil sen for: ',ts_titles[x])))
pdf(file.path('/media/dan/rita/outputs/viz/rita_theilsen_maps.pdf'), height =4, width = 8)
tsmaps
dev.off()

#theil sen boxplots
tsen = rast(theilsen)
ts_titles[1] <- 'Overall NDVI'
names(tsen) <- ts_titles
tsex = extract(tsen, as(area, 'SpatVector'))
tsex = setDT(tsex)
tsex = melt(tsex, id.vars = 'ID')
tsex[, ID := factor(ID, 1:2, c('Not Flooded', 'Flooded'))]
g = ggplot(tsex, aes(variable, value)) + geom_boxplot() + ylim(c(-.25, .25)) + theme_bw() + ggtitle('Boxplot of Theil Sen') + facet_wrap(~ID)
pdf(file.path('/media/dan/rita/outputs/viz/rita_theilsen_boxplot.pdf'), height =5, width = 8)
g
dev.off()

#run the kendallers
#for each var?
mk = lapply(vars, function(z) runmk(rast(z)))
mk = lapply(seq_along(vars), function(z){
  r = mk[[z]]
  names(r) <- names(vars)[z]
  r
})
names(mk) = names(vars)
#make mann kendall maps
mkm = lapply(seq_along(vars), function(z) make_map(mk[[z]], paste0('Mann Kendall for ', names(mk)[z]), NULL))
pdf(file.path('/media/dan/rita/outputs/viz/rita_mk_maps.pdf'), height =5, width = 8)
mkm
dev.off()
