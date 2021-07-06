library('raster')
library('terra')
library('gdalUtils')
ashp = st_read("/media/dan/rita/studyarea/new_strata_rita_10282017.shp")

#read from the raw raster directly with terra 
a2 = rast("/media/dan/rita/MOD13A2.A2016353.h10v06.006.2017010091128.hdf")
blah = a2[[1]]
blah2 = terra::project(blah, "+proj=longlat +datum=NAD83 +no_defs") #crs(as(ashp, 'SpatVector'), proj = T))

print(res(blah2))

#read with raster
metadat = gdalUtils::get_subdatasets("/media/dan/rita/MOD13A2.A2016353.h10v06.006.2017010091128.hdf")
blah3 = raster(metadat[1])
blah3 = projectRaster(blah3,crs=("+proj=longlat +datum=NAD83 +no_defs"))

print(res(blah3))