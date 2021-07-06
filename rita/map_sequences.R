library('terra')
library('glue')
library('rasterVis')
library('ggplot2')
library('sf')
library('data.table')

ooot = '/media/dan/rita/outputs/output_rita_ndvi/' 
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

make_map = function(x, title = '', years = 2001:2016){
  stopifnot(all(file.exists(x)))
  
  r = rast(x)
  
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

dts = substr(names(r),nchar(names(r))-7, nchar(names(r)))
dts = as.Date(dts, '%Y_%j')

#graph over time by region
timexregion = function(x, title, area, layer_dates = 2001:2016, date_min = -Inf, date_max = Inf){
  r = rast(x)
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
  timexregion(vars[[x]], title = names(vars)[x], layer_dates = switch(names(vars)[x], NDVI = dts, paste0(2001:2016, '-07-02')), area = area)
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
