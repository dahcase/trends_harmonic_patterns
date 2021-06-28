library('terra')
library('glue')
library('rasterVis')
library('ggplot2')
ooot = '/media/dan/rita/outputs/output_rita_ndvi/' 
yyy = 1:16
a1 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A_1.tif'))
a2 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A_2.tif'))
a0_1 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A0_1.tif'))
a0_2 = file.path(ooot, glue('rita_ndvi_amplitude_year_{yyy}_A0_2.tif'))
p1 = file.path(ooot, glue('rita_ndvi_year_{yyy}_phase_1.tif'))
p2 = file.path(ooot, glue('rita_ndvi_year_{yyy}_phase_2.tif'))

vars = list(A1 = a1, A2 = a2, A0_1 =a0_1, A0_2 = a0_2, `Phase 1` = p1, `Phase 2` = p2)

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

mapz = lapply(seq_along(vars), function(x) make_map(vars[[x]], title = names(vars)[x]))

pdf(file.path('/media/dan/rita/outputs/viz/rita_sta_out.pdf'), height =6, width = 11)
mapz
dev.off()

  