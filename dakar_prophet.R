#RUn change analysis
library('data.table')
library('parallel')
library('raster')
library('prophet')
library('data.table')
library('tools')
library('prophet')
source('/home/dan/Documents/code/trends_harmonic_patterns/prophet.R')
sg = readRDS('/media/dan/summary_grid.rds')
ras2run = sg[time == "" & funk == "" & variables %in% c("NDVI") & city %in% 'Dakar',] 

lapply(c(.7,.8,.9,.95,.99), function(x){
  print(x)
  do_ras(1, ras2run, x)
} )
