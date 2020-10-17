library(terra)
library(sf)
library(ranger)
library(rgdal)


wrong <- st_read('data/wrong.shp')
wrong$v2 <- 1
wrong <- rbind(wrong, wrong)
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

#bring elevation and training layers ----
Elev <- rast('C:/a/geo/climate/worldclim2.1/wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif' )
chelsa.p <- rast('C:/a/geo/climate/chelsa/output/p.tif' )
terra.p <- rast('C:/a/geo/climate/terraclimate/output/p.tif' )
t07 <- rast('C:/a/geo/climate/terraclimate/output/T07.tif' )
t01 <- rast('C:/a/geo/climate/terraclimate/output/T01.tif' )
crs(chelsa.p)<- crs(terra.p)

#fix alaska precipitation ----
outer <-  ext(-165, -125, 53, 71)
inner <-  ext(-151, -140, 63, 69)

c.outer <- crop(chelsa.p, outer)
c.inner <- crop(chelsa.p, inner)
c.outer[is.na(c.outer)]=0
c.inner[is.na(c.inner)]=0
c.outer[c.outer >=0 |c.outer< 0] <- 1
c.inner[c.inner >=0 |c.inner< 0] <- 0
c.outer <- merge(c.outer, c.inner)
blend <- c.outer
c.outer[c.outer == 0] <- NA

blend <- aggregate(blend, fact=10, fun='mean')
blend <- focal(blend, w=3, fun='mean')
blend <- resample(blend, c.outer, method='bilinear')
blend <- focal(blend, w=3, fun='mean')
z <- crop(Elev, outer)
c <- crop(chelsa.p, outer)
t1 <- crop(t01, outer)
t2 <- crop(t07, outer)
names(z)<- 'z'
names(c)<- 'c'
names(t1)<- 't1'
names(t2)<- 't2'

for (i in 1:12){
  #get new month
  p0 <- get(paste0('p',month[7]))
  #crop to area of interest ----
  p0 <- crop(p0, outer)
  pp <- p0*c.outer
  names(pp)<-'pp'
  brk <- c(pp,c,z,t1,t2)
  df <- as.data.frame(brk, xy=T)
  df.s <- subset(df, !is.na(pp) & !is.na(z) & !is.na(c))
  df.s <- df[sample(rownames(df.s), 100000, replace = T) ,] 
  df.rast <- rast(cbind(x=df.s$x,y=df.s$y,z=df.s$c), type="xyz", crs=crs(w))
  plot(df.rast)
  rf <- ranger(pp ~ c+ z+x+y+t1+t2,
               data=df.s, num.trees=500, max.depth = 5, importance = 'impurity', write.forest = TRUE)
  df <- subset(df, !is.na(z)&!is.na(c))
  df$pred <- predictions(predict(rf, data=df))
  df.rast <- rast(cbind(x=df$x,y=df$y,z=df$pred), type="xyz", crs=crs(blend))
  crs(df.rast) <- crs(blend)
  importance(rf)
  ncol(df.rast) == ncol(blend)
  fixed <- blend*p0 + (blend*-1+1)*df.rast
  plot(df.rast-p0)
  plot(fixed-p0)
  plot(fixed)
  plot(log(p0), maxcell=1000000)
  p <- merge(pp, fixed)
  writeRaster(p, paste0('fixed/p',month[i],'.tif'), overwrite=T)
}














#fix precipitation ----
clim <- crop(chelsa.p, ext(-107, -90, 39, 48))
for (i in 1:12){
#get new month
pp <- get(paste0('p',month[i]))
#crop to area of interest ----
clim1 <- crop(pp, ext(clim))

#make holes ----
wrong.rast <- rasterize(vect(wrong), clim, field = 'v2')
wrong.d <- distance(wrong.rast)
blend <- wrong.d
w <- wrong.d
w[w < 45000] <- NA
w[!is.na(w)] <- 1
w <- w * clim1

#make blend ----
blend[blend < 70000] <- 0
blend[blend > 0] <- 1
blend <- blend*-1+1
blend <- aggregate(blend, fact=4, fun='mean')
blend <- focal(blend, w=3, fun='mean')
blend <- resample(blend, w, method='bilinear')
blend <- focal(blend, w=3, fun='mean')


names(clim)<- 'clim'
names(w) <- 'w'
z <- crop(Elev, ext(clim))
names(z) <- 'z'
brk <- c(z,w,clim)
df <- as.data.frame(brk, xy=T)
df.s <- subset(df, !is.na(w) & !is.na(z) & !is.na(clim))
df.s <- df[sample(rownames(df.s), 50000, replace = T) ,] 
df.rast <- rast(cbind(x=df.s$x,y=df.s$y,z=df.s$w), type="xyz", crs=crs(w))
plot(df.rast)
rf <- ranger(w ~ clim + x+ y+ z,
             data=df.s, num.trees=100, max.depth = 1000, importance = 'impurity', write.forest = TRUE)
df <- subset(df, !is.na(z))
df$pred <- predictions(predict(rf, data=df))
df.rast <- rast(cbind(x=df$x,y=df$y,z=df$pred), type="xyz", crs=crs(w))
crs(df.rast) <- crs(w)
plot(df.rast)
importance(rf)
res(df.rast) ==res(blend)
fixed <- blend*df.rast + (blend*-1+1)*clim1

p <- merge(pp, fixed)
writeRaster(p, paste0('fixed/p',month[i],'.tif'), overwrite=T)
}


#fix temperatures ---- 
#make hole
outer <- ext(-109, -101, 42, 52)
inner <- ext(-106, -103.75, 45.5, 50)
c.outer <- crop(th07, outer)
c.inner <- crop(th07, inner)
c.outer[c.outer >=0 |c.outer< 0] <- 1
c.inner[c.inner >=0 |c.inner< 0] <- 0
c.outer <- merge(c.outer, c.inner)
blend <- c.outer
c.outer[c.outer == 0] <- NA

blend <- aggregate(blend, fact=5, fun='mean')
blend <- focal(blend, w=3, fun='mean')
blend <- resample(blend, c.outer, method='bilinear')
blend <- focal(blend, w=3, fun='mean')

t1 <- crop(th06, outer)
names(t1)<- 't1'
t2 <- crop(th09, outer)
names(t2)<- 't2'
t3 <- crop(tl06, outer)
names(t3)<- 't3'
t4 <- crop(tl09, outer)
names(t4)<- 't4'
z <- crop(Elev, outer)
names(z) <- 'z'

layer <- c('th07', 'th08', 'tl07', 'tl08')
for (i in 1:4){
  #get new month
  t <- get(layer[i])
  tt <- crop(t, outer)
  ttt <- tt * c.outer
  names(ttt)<- 'ttt'
  brk <- c(ttt,t1,t2,t3,t4,z)
  df <- as.data.frame(brk, xy=T)
  df.s <- subset(df, !is.na(ttt) & !is.na(z) & !is.na(t1))
  #df.s <- df[sample(rownames(df.s), 50000, replace = F) ,] 
  #df.rast <- rast(cbind(x=df.s$x,y=df.s$y,z=df.s$ttt), type="xyz", crs=crs(ttt))
  #plot(df.rast)
  rf <- ranger(ttt ~ t1+t2+t3+t4 +  z,
               data=df.s, num.trees=200, max.depth = 1000, importance = 'impurity', write.forest = TRUE)
  df <- subset(df, !is.na(z))
  df$pred <- predictions(predict(rf, data=df))
  df.rast <- rast(cbind(x=df$x,y=df$y,z=df$pred), type="xyz", crs=crs(blend))
  crs(df.rast) <- crs(blend)
  plot(df.rast)
  importance(rf)

  fixed <- blend*tt + (blend*-1+1)*df.rast
  plot(fixed)
  tttt <- merge(t, fixed)
  #plot(crop(tttt, ext(-120, -90, 30, 55)))
  #plot(c.outer, add = T)
  writeRaster(tttt, paste0('fixed/',layer[i],'.tif'))
}


