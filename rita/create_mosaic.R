library('terra')
library('sf')
area = "/media/dan/rita/studyarea/new_strata_rita_10282017.shp"

raspath = '/media/dan/rita/VI_16Days_1Km_v6/NDVI'
r = rast(list.files(raspath, 'tif', full.names = T))
rtar = terra::rast('/media/dan/rita_man/revised_area_Rita/r_ref_Houston_RITA.tif')
proj_str <- "+proj=lcc +lat_1=27.41666666666667 +lat_2=34.91666666666666 +lat_0=31.16666666666667 +lon_0=-100 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
ashp = st_read(area)
ashp = st_transform(ashp, proj_str)

crs(rtar) <- proj_str

r[r < -2000] <- NA

#scale it
r = r*0.0001

r2 = terra::project(r, rtar)
r2 = terra::crop(r2, ashp)

raster::writeRaster(r2, file.path(dirname(raspath), 'rita_ndvi.tif'), overwrite = T)

