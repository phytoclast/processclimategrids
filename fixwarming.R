library(terra)

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
#load basic files ----
for (i in 1:12){
  assign(paste0('p',month[i]), rast(paste0('data/p',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('tl',month[i]), rast(paste0('data/tn',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('th',month[i]), rast(paste0('data/tx',month[i],'.tif')))
}


for (i in 1:12){
  assign(paste0('wp',month[i]), rast(paste0('warming/p',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('wtl',month[i]), rast(paste0('warming/tn',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('wth',month[i]), rast(paste0('warming/tx',month[i],'.tif')))
}

for (i in 1:12){
  assign(paste0('fp',month[i]), rast(paste0('flawedoriginal/p',month[i],'.tif')))
}
for (i in 7:8){
  assign(paste0('ftl',month[i]), rast(paste0('flawedoriginal/tn',month[i],'.tif')))
}
for (i in 7:8){
  assign(paste0('fth',month[i]), rast(paste0('flawedoriginal/tx',month[i],'.tif')))
}

outer <- ext(-130, -65, 25, 50)
inner <- ext(-107, -90, 39, 48)

for (i in 1:12){
p <- crop(get(paste0('p',month[i])), inner)
wp <- crop(get(paste0('wp',month[i])), inner)
fp <- crop(get(paste0('fp',month[i])), inner)
correct <- log(p+1) - log(fp+1)
correct[correct < 0.0001 & correct > -0.0001] <- 0
cp <- exp(log(wp+1)+correct)-1

wpp <- get(paste0('wp',month[i]))
cpp <- merge(wpp, cp) 
writeRaster(cpp, paste0('fixed/p',month[i],'.tif'))
}


outer <- ext(-114, -96, 39, 55)
inner <- ext(-109, -101, 42, 52)

layer <- c('th07', 'th08', 'tl07', 'tl08')
for (i in 1:4){
  t <- crop(get(layer[i]), inner)
  wt <- crop(get(paste0('w',layer[i])), inner)
  ft <- crop(get(paste0('f',layer[i])), inner)
  correct <- t - ft
  ct <- wt+correct
  plot(correct)
  wtt <- get(paste0('w',layer[i]))
  ctt <- merge(wtt, ct) 
  writeRaster(ctt, paste0('fixed/',layer[i],'.tif'))
}
plot(crop((ctt-tl08), outer))