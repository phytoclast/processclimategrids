library(terra)
e.wtlow <-  function(el,mid){
  x = 0.01^(el/mid-1)
  elwt = x/(x+1)
  return(elwt)
}
e.wthigh <-  function(el,mid){
  x = 0.01^(1-el/mid)
  elwt = x/(x+1)
  return(elwt)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ----

Elev5km = rast('output/Elev5km.tif')
Elev1km = rast('output/Elev1km.tif')
ElevMax <- aggregate(Elev1km, fact=5, fun='max',na.rm=TRUE)
ElevMin <- aggregate(Elev1km, fact=5, fun='min',na.rm=TRUE)

ElevMax5 <- aggregate(Elev5km, fact=5, fun='max',na.rm=TRUE)
ElevMin5 <- aggregate(Elev5km, fact=5, fun='min',na.rm=TRUE)
ElevMean5 <- aggregate(Elev5km, fact=5, fun='mean',na.rm=TRUE)

ElevMax5 <- resample(ElevMax5, Elev5km)
ElevMin5 <- resample(ElevMin5, Elev5km)
ElevMean5 <- resample(ElevMean5, Elev5km)

# ElevMax5 <- focal(ElevMax5, w=3, fun="mean" ,na.rm=TRUE)
# ElevMin5 <- focal(ElevMin5, w=3, fun="mean" ,na.rm=TRUE)
# ElevMean5 <- focal(ElevMean5, w=3, fun="mean" ,na.rm=TRUE)


ElevBin <- Elev5km >= ElevMean5

ElevRel1 <- (Elev5km - ElevMean5)/(ElevMax5 - ElevMean5 + 0.1)
ElevRel2 <- (ElevMean5 - Elev5km)/(ElevMean5 - ElevMin5 + 0.1)

ElevAmp1 <- (ElevMax * (ElevRel1) + Elev5km * (ElevRel1-1)*-1)
ElevAmp2 <- (Elev5km * (ElevRel2) + ElevMin * (ElevRel2-1)*-1)

ElevAmp <- ElevAmp1 * ElevBin + ElevAmp2 * (ElevBin-1)*-1
writeRaster(ElevAmp, 'output/ElevAmp.tif', overwrite =T)
#writeRaster(ElevBin, 'output/ElevBin.tif')
#Temperatures 

library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ----

Elev5km = rast('wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')

#Egrid <- aggregate(Elev5km, fact=5, fun='mean',na.rm=TRUE)
Egrid <- aggregate(Elev5km, fact=3, fun='mean',na.rm=TRUE)
Egrid <- as.data.frame(Egrid, xy=T); colnames(Egrid) <- c('x','y','z'); Egrid <- subset(Egrid, !is.nan(z))
xy = as.matrix(as.data.frame(list(x=Egrid$x, y=Egrid$y)))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")
xy2 <- st_as_sf(Egrid, coords = c("x", "y"), crs = crs(Elev5km))
xy2<- as_Spatial(xy2)
writeOGR(xy2, 'output','xy', driver = 'ESRI Shapefile', overwrite_layer = T)



xy.Elev = extract(Elev5km, xy);  xy.Elev <- subset(xy.Elev, select= -ID);colnames(xy.Elev) <- c('Elev')
Egrid <- cbind(Egrid, xy.Elev); 
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')#i=7
if(T){for (i in 1:12){
  t = extract(rast(paste0('output/t',month[i],'.tif')), xy); t <- subset(t, select= -ID); colnames(t) <- paste0('t',month[i])
  Egrid <- cbind(Egrid, t)
}
  # for (i in 1:12){
  #   th = extract(rast(paste0('data/tx',month[i],'.tif')), xy); th <- subset(th, select= -ID); colnames(th) <- paste0('th',month[i])
  #   Egrid <- cbind(Egrid, th)
  # }
  # for (i in 1:12){
  #   tl = extract(rast(paste0('data/tn',month[i],'.tif')), xy); tl <- subset(tl, select= -ID); colnames(tl) <- paste0('tl',month[i])
  #   Egrid <- cbind(Egrid, tl)
  # }
  Egrid <- subset(Egrid, !is.nan(Elev))
  t.colrange = grep("^t01$", colnames(Egrid)):grep("^t12$", colnames(Egrid))
  # th.colrange = grep("^th01$", colnames(Egrid)):grep("^th12$", colnames(Egrid))
  # tl.colrange = grep("^tl01$", colnames(Egrid)):grep("^tl12$", colnames(Egrid))
# 
# for (i in 1:12){
#   Egrid$var <- round((Egrid[,th.colrange[i]]+Egrid[,tl.colrange[i]])/2, 2)
#   colnames(Egrid)[colnames(Egrid) == 'var'] <- paste0("t", month[i])
# }

Egrid$zone <- floor((Egrid$x+180)/2)*1000 + floor(Egrid$y+90)
Egrid <- subset(Egrid, !is.na(t07) & !is.na(t01))
saveRDS(Egrid, 'output/Egrid.RDS')
}
Egrid <- readRDS('output/Egrid.RDS')
Egrid.agg <- aggregate(Egrid$zone, by=list(Egrid$zone), 'length')
colnames(Egrid.agg) <- c('zone','count')
t.colrange = grep("^t01$", colnames(Egrid)):grep("^t12$", colnames(Egrid))
k=5000
Egrid.s <- subset(Egrid.agg, count >= 50) 
#define fields to rotate through ----
Egrid.s$r1t01 <- NA
Egrid.s$r1t02 <- NA
Egrid.s$r1t03 <- NA
Egrid.s$r1t04 <- NA
Egrid.s$r1t05 <- NA
Egrid.s$r1t06 <- NA
Egrid.s$r1t07 <- NA
Egrid.s$r1t08 <- NA
Egrid.s$r1t09 <- NA
Egrid.s$r1t10 <- NA
Egrid.s$r1t11 <- NA
Egrid.s$r1t12 <- NA

Egrid.s$r2t01 <- NA
Egrid.s$r2t02 <- NA
Egrid.s$r2t03 <- NA
Egrid.s$r2t04 <- NA
Egrid.s$r2t05 <- NA
Egrid.s$r2t06 <- NA
Egrid.s$r2t07 <- NA
Egrid.s$r2t08 <- NA
Egrid.s$r2t09 <- NA
Egrid.s$r2t10 <- NA
Egrid.s$r2t11 <- NA
Egrid.s$r2t12 <- NA

r1t.cols <- grep("^r1t01$", colnames(Egrid.s)):grep("^r1t12$", colnames(Egrid.s))
r2t.cols <- grep("^r2t01$", colnames(Egrid.s)):grep("^r2t12$", colnames(Egrid.s))
#need to create a weight for tile and for distance from tile... then wt for high and low elevation...
# clim.tab$dist <- (((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5
# clim.tab$wt <- (localzone/(clim.tab$dist+localzone))^shape*100
Egrid$wt.low <- e.wtlow(Egrid$Elev, 2000)
Egrid$wt.high <- e.wthigh(Egrid$Elev, 1000)
chk = 14153
i=1
Egridk <- Egrid[Egrid$zone %in% chk,]
model.t1 <-lm(Egridk[,t.colrange[i]] ~ Elev + y+ x, data = Egridk, weights = Egridk$wt.low) ;summary(model.t1)
model.t2 <-lm(Egridk[,t.colrange[i]] ~ Elev + y+ x, data = Egridk, weights = Egridk$wt.high) ;summary(model.t2)
Egrid.s[k,tr.cols[i]] = model.t$coefficients[2]

if(T){

for (k in 1:nrow(Egrid.s)){
#for (k in 1:10){
Egridk <- Egrid[Egrid$zone %in% Egrid.s[k,1],]
for(i in 1:12){#i=7
  model.t <-lm(Egridk[,t.colrange[i]] ~ Elev + y+ x, data = Egridk) ;summary(model.t)
  Egrid.s[k,tr.cols[i]] = model.t$coefficients[2]
    
}}; saveRDS(Egrid.s, 'output/Egrid.s.RDS'); saveRDS(Egrid, 'output/Egrid.RDS')
}

# ----
Egrid.s <- readRDS('output/Egrid.s.RDS')
Egrid <- readRDS('output/Egrid.RDS')
Egrid.agg <- aggregate(list(x=Egrid$x,y=Egrid$y), by=list(zone=Egrid$zone), 'mean')
Egrid.agg <- merge(Egrid.agg,Egrid.s, by='zone' )
Egrid.agg <- subset(Egrid.agg, !is.na(rt01))
xy = as.matrix(as.data.frame(list(x=Egrid.agg$x, y=Egrid.agg$y)))
xy <- vect(xy,type="points", atts=Egrid.agg, crs="+proj=longlat +datum=WGS84")
xy.sfc <- st_as_sf(Egrid.agg, coords = c('x', 'y'), crs = "+proj=longlat +datum=WGS84")
template <- aggregate(Elev5km, fact=10, fun='mean',na.rm=TRUE)
saveRDS(Egrid.agg, 'output/elevrts.RDS')
library(gstat)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(raster)

Elev5km = rast('output/Elev5km.tif')

d <- Egrid.agg[Egrid.agg$rt07 > -.01 & Egrid.agg$rt07 < 0.005,c('x','y','rt07')]
d<- subset(d, !is.na(x) &!is.na(y) &!is.na(rt07))
d2 <- st_as_sf(d, coords = c("x", "y"), crs = crs(Elev5km))
d2<- as_Spatial(d2)
writeOGR(d2, 'output','d', driver = 'ESRI Shapefile', overwrite_layer = T)

r <- raster(template)
r[r>=0] <- NA; r[r<0] <- NA
gs <- gstat(id = 'rt07', formula=rt07~1, locations=~x+y, data=d)
idw <- interpolate(r, gs, debug.level=0)
writeRaster(idw,'output/idw.tif', overwrite=T)
idw <- rast('output/idw.tif')
nelev <-  Elev5km
nelev <- aggregate(nelev, fact=10, fun='mean', na.rm=T)
nelev[nelev>=0] <- 1; nelev[nelev<0] <- 1
idw2 <-  focal(idw, w=3, na.rm=TRUE, fun=mean)
idw2 <-  focal(idw2, w=3, na.rm=TRUE, fun=mean)

idw3 <- nelev*idw2
idw2 <-  resample(idw2, Elev5km)

ElevAmp <-  rast('output/ElevAmp.tif')
Elev5km = rast('output/Elev5km.tif')
# Elev1km = rast('output/Elev1km.tif')
# emax <- Elev1km@ptr@.xData$range_max; emin <- Elev1km@ptr@.xData$range_min
# 
#ElevAmp[ElevAmp > emax]<- emax; ElevAmp[ElevAmp < emin]<- emin
rt07 <- (ElevAmp - Elev5km) * idw2

writeRaster(rt07, 'output/rt07.tif', overwrite=T)
t07 <- rast('output/t07.tif')
crs(rt07) <- crs(t07)
t07a <- t07 + rt07
writeRaster(t07a, 'output/t07a.tif', overwrite=T)
plot(ElevAmp)
plot(rt07)

######################################
#generate amplified temperature grids ----
library(gstat)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(terra)
library(raster)

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')

ElevAmp <-  rast('output/ElevAmp.tif')
Elev5km = rast('output/Elev5km.tif')
elevrts <- readRDS('output/elevrts.RDS')
edif <- (ElevAmp - Elev5km)
rt.cols <- grep("^rt01$", colnames(elevrts)):grep("^rt12$", colnames(elevrts))
nelev <- aggregate(Elev5km, fact=10, fun='mean', na.rm=T)
nelev[nelev>=0] <- 1; nelev[nelev<0] <- 1
r <- raster(nelev)
r[r>=0] <- NA; r[r<0] <- NA
for (i in 1:12){#i=1
d <- elevrts[elevrts[,rt.cols[i]] > -.012 & elevrts[,rt.cols[i]] < 0.006, c('x','y',paste0('rt',month[i]))]
d$z <- d[,3]
d<- subset(d, !is.na(x) &!is.na(y) &&!is.na(z))
d2 <- st_as_sf(d, coords = c("x", "y"), crs = crs(Elev5km))
d2<- as_Spatial(d2)

#writeOGR(d2, 'output','d', driver = 'ESRI Shapefile', overwrite_layer = T)

gs <- gstat(id = 'z', formula=z~1, locations=~x+y, data=d)
idw <- interpolate(r, gs, debug.level=0)
writeRaster(idw,'output/idw.tif', overwrite=TRUE)
idw <- rast('output/idw.tif')
idw2 <-  focal(idw, w=5, na.rm=TRUE, fun=mean)
idw2 <- nelev*idw2
idw2 <-  resample(idw2, Elev5km)
#writeRaster(idw2,'output/idw2.tif', overwrite=TRUE)
th0 = rast(paste0('data/tx',month[i],'.tif'))
tl0 = rast(paste0('data/tn',month[i],'.tif'))
t0 = rast(paste0('output/denalifix/t',month[i],'.tif'))
tmax <- t0@ptr$range_max
tmin <- t0@ptr$range_min
tdif <- edif * idw2; crs(tdif) <- crs(t0)
t  <- tdif + t0; names(t) <- paste0('t',month[i]); t[t > tmax+3] <- tmax+3; t[t < tmin-3] <- tmin-3
th <- (th0 - tl0)/2 + t; names(th) <- paste0('th',month[i])
tl <- (tl0 - th0)/2 + t; names(tl) <- paste0('tl',month[i])
writeRaster(t, paste0('output/amplified/t',month[i],'.tif'), overwrite=T)
writeRaster(th, paste0('output/amplified/th',month[i],'.tif'), overwrite=T)
writeRaster(tl, paste0('output/amplified/tl',month[i],'.tif'), overwrite=T)
}

