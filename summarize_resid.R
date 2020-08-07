library('raster')
library('data.table')
library('ggplot2')
library('viridis')

nnn = read.delim('/media/dan/earth_engine/MODIS_006_MOD13A1.txt', header = F, stringsAsFactors = F)[,1]
if(!all(grepl('_', nnn, fixed = T))){
  nnn = paste(substr(nnn, 1,4), substr(nnn, 5,6), substr(nnn, 7,8), sep = '_')
}
#convert to date paths
nnn = as.Date(nnn, '%Y_%m_%d')
n = data.table(ds = nnn)
n[, timestep := as.character(.I)]

o = readAll(brick("/media/dan/processed//MOD13A1/latlong/Dakar_MOD13A1_006_EVI_2001_2016.tif"))

#randomly sample 25 pixels

oex = as.data.table(as.data.frame(o))[, id := .I][, type := 'og']
oex = melt(oex, id.vars = c('id', 'type'), variable.factor = FALSE)
oex[, timestep := substr(variable, regexpr('.', variable, fixed = TRUE) +1, nchar(variable))]
oex = merge(oex, n, by = 'timestep', all.x = T)
weights = oex[, sum(!is.na(value))/.N, by = id][V1>0]
px = sample(weights[, id], size = 25, prob = weights[, V1])

px = c(5580L, 5440L, 2043L, 6574L, 7805L, 9083L, 9335L, 5665L, 5095L, 
  4856L, 3168L, 5302L, 5372L, 6500L, 639L, 5724L, 2552L, 1902L, 
  5714L, 4471L, 2166L, 4591L, 5552L, 3166L, 6058L)

oex = oex[id %in% px]

pixel_fit = function(x, dates, sps = 10, ui = .8, nmcmc = 0){
  dat = data.frame(ds = dates, y = x)
  a <- try(p <- prophet(dat, weekly.seasonality = FALSE,
                        daily.seasonality = FALSE,
                        seasonality.prior.scale = sps,
                        interval.width = ui,
                        mcmc.samples = nmcmc))
  return(a)
}


get_preds = function(px, oex, sps = 10, ui = .8, nmcmc = 0){
  mods = lapply(px, function(x) pixel_fit(oex[id %in% x, value], oex[id %in% x, ds], sps = sps, ui = ui, nmcmc = nmcmc))
  stopifnot(all(vapply(mods, function(x) inherits(x, 'prophet'), TRUE)))
  preds = lapply(px, function(x) setDT(predict(mods[[which(px %in% x)]], oex[id %in% x]))[,id:=x])
  preds = rbindlist(preds)
  preds[, ds := as.Date(ds)]
  oex[, type := 'original']
  oex = oex[id %in% px]
  oex = merge(oex, preds, all.x = T, by = c('id','ds'))
  #res = rbind(oex[id %in% px], preds, fill = T)
  
  return(oex)
}


a1 = get_preds(px, oex, sps = 10, ui = .8, nmcmc = 0)
a2 = get_preds(px, oex, sps = 10, ui = .8, nmcmc = 1000) #mcmc sa
a3 = get_preds(px, oex, sps = 20, ui = .8, nmcmc = 0)
#a4 = get_preds(px, oex, sps = 20, ui = .8, nmcmc = 1000)

saveRDS(list(a1,a2,a3), '/media/dan/a1-3_prophet.rds')

#compare MCMC w/ MAP
# mcmcmap = merge(a1[type == 'pred', .(id, ds, mval = value, mlow = yhat_lower, mupp = yhat_upper)],
#                 a2[type == 'pred', .(id, ds, mcval = value, mclow = yhat_lower, mcupp = yhat_upper)],
#                 by = c('id','ds'), all.x = T)

pdf('/media/dan/prophet_pixel_cd_test.pdf', width = 10, height = 9)

# g1 = ggplot(mcmcmap, aes(x = mval, y =  mcval)) + theme_dark() + geom_point() + 
#   facet_wrap(~id, scales = 'free') + geom_abline(slope = 1, intercept = 0) +
#   ggtitle('MCMC vs. Map, yhat')
# 
# plot(g1)
# 
# g2 = ggplot(mcmcmap, aes(x = mlow, y =  mclow)) + theme_dark() + geom_point() + 
#   facet_wrap(~id, scales = 'free') + geom_abline(slope = 1, intercept = 0) +
#   ggtitle('MCMC vs. Map, lower')
# 
# plot(g2)
# 
# g3 = ggplot(mcmcmap, aes(x = mupp, y =  mcupp)) + theme_dark() + geom_point() + 
#   facet_wrap(~id, scales = 'free') + geom_abline(slope = 1, intercept = 0) +
#   ggtitle('MCMC vs. Map, upper')
# 
# plot(g3)

#Defaults: MAP, with .8 interval, sps of 10
a1[type == 'original' & !between(value, yhat_lower, yhat_upper), type := 'original, extreme']
g = ggplot() +
  geom_point(data = a1[id != 5665], aes(x = ds, y = value, group = type, color = type)) +
  geom_line(data = a1[id != 5665], aes(x = ds, y = yhat), color = 'black') +
  facet_wrap(~id) + theme_dark() +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + ggtitle('MAP; .8; 10')
plot(g)

g = ggplot(data = a1[type != 'pred'], aes(x = value - yhat, group = type, fill = type)) +
  geom_histogram(bins = 100) + facet_wrap(~id) + theme_dark() +
  scale_fill_brewer(type = 'qual') + ggtitle('MAP; .8; 10')
plot(g)  

#MCMC, with .8 interval, sps of 10
a2[type == 'original' & !between(value, yhat_lower, yhat_upper), type := 'original, extreme']
g = ggplot() +
  geom_point(data = a2[id != 5665], aes(x = ds, y = value, group = type, color = type)) +
  geom_line(data = a2[id != 5665], aes(x = ds, y = yhat), color = 'black') +
  facet_wrap(~id) + theme_dark() +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + ggtitle('MCMC; .8; 10')
plot(g)

g = ggplot(data = a2[type != 'pred'], aes(x = value - yhat, group = type, fill = type)) +
  geom_histogram(bins = 100) + facet_wrap(~id) + theme_dark() +
  scale_fill_brewer(type = 'qual') + ggtitle('MCMC; .8; 10')
plot(g)  

#MAP, .8 interval, sps of 20
a3[type == 'original' & !between(value, yhat_lower, yhat_upper), type := 'original, extreme']
g = ggplot() +
  geom_point(data = a3[id != 5665], aes(x = ds, y = value, group = type, color = type)) +
  geom_line(data = a3[id != 5665], aes(x = ds, y = yhat), color = 'black') +
  facet_wrap(~id) + theme_dark() +
  scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') + ggtitle('MAP; .8; 20')
plot(g)

g = ggplot(data = a3[type != 'pred'], aes(x = value - yhat, group = type, fill = type)) +
  geom_histogram(bins = 100) + facet_wrap(~id) + theme_dark() +
  scale_fill_brewer(type = 'qual') + ggtitle('MAP; .8; 20')
plot(g)  

dev.off()
# #More wiggle room
# r3[type == 'original' & !between(value, yhat_lower, yhat_upper), type := 'original, extreme']
# g = ggplot() +
#   geom_point(data = r3[type != 'pred'], aes(x = ds, y = value, group = type, color = type)) +
#   geom_line(data = r3[type == 'pred'], aes(x = ds, y = value, group = type, color = type)) + 
#   facet_wrap(~id) + theme_dark() +
#   scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual') 
# plot(g)
# 
# 
# r1 = get_preds(px, oex, 10)
# r2 = get_preds(px, oex, 15)
# r3 = get_preds(px, oex, 20)
# 
# r1[, sps := 10]
# r2[, sps := 15]
# r3[, sps := 20]
# 
# sps_test = rbind(r1,r2,r3)[type %in% 'pred']
# sps_test[, sps := as.character(sps)]
# 
# g = ggplot(sps_test, aes(x = ds, y = value, group = sps, color = sps)) + 
#   geom_line() +
#   facet_wrap(~id) + theme_bw() +
#   scale_color_brewer(type = 'qual')
# plot(g)



# g = ggplot(r1, aes(x = ds, y = value, group = type, color = type, fill = type)) + 
#   geom_line() +
#   facet_wrap(~id) + theme_bw() +
#   scale_color_brewer(type = 'qual') + scale_fill_brewer(type = 'qual')
# plot(g)


# 
# rex = as.data.table(as.data.frame(r))[, id := .I][, type := 'resid']
# pex = as.data.table(as.data.frame(p))[, id := .I][, type := 'preds']
# rex = melt(rex, id.vars = c('id', 'type'), variable.factor = FALSE)
# pex = melt(pex, id.vars = c('id', 'type'), variable.factor = FALSE)
# 
# rex[, timestep := substr(variable, regexpr('.', variable, fixed = TRUE) +1, nchar(variable))]
# pex[, timestep := substr(variable, regexpr('.', variable, fixed = TRUE) +1, nchar(variable))]
# 
# ex = rbind(rex, pex, oex)
# 
# weights = ex[, sum(!is.na(value))/.N, by = id][V1>0]
# 
# px = sample(weights[, id], size = 25, prob = weights[, V1])
