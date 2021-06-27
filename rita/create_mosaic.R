library('terra')
library('sf')
area = "C:/Users/Me/Documents/react/new_strata_rita_10282017.shp"

raspath = 'C:/Users/Me/Documents/react/rita/VI_16Days_1Km_v6/NDVI'
r = rast(list.files(raspath, 'tif', full.names = T))

ashp = st_read(area)
r2 = terra::project(r, 'epsg:4269')
r2 = terra::crop(r2, ashp)
r2[r2<-2000] <- NA

#scale it
r2 = r2*0.0001

raster::writeRaster(r2, file.path(dirname(raspath), 'rita_ndvi.tif'), overwrite = T)
