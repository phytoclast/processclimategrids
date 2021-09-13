library(terra)

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

Egrid <- aggregate(Elev5km, fact=5, fun='mean',na.rm=TRUE)
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
  th = extract(rast(paste0('data/tx',month[i],'.tif')), xy); th <- subset(th, select= -ID); colnames(th) <- paste0('th',month[i])
  Egrid <- cbind(Egrid, th)
}
for (i in 1:12){
  tl = extract(rast(paste0('data/tn',month[i],'.tif')), xy); tl <- subset(tl, select= -ID); colnames(tl) <- paste0('tl',month[i])
  Egrid <- cbind(Egrid, tl)
}
Egrid <- subset(Egrid, !is.nan(Elev))
th.colrange = grep("^th01$", colnames(Egrid)):grep("^th12$", colnames(Egrid))
tl.colrange = grep("^tl01$", colnames(Egrid)):grep("^tl12$", colnames(Egrid))

for (i in 1:12){
  Egrid$var <- round((Egrid[,th.colrange[i]]+Egrid[,tl.colrange[i]])/2, 2)
  colnames(Egrid)[colnames(Egrid) == 'var'] <- paste0("t", month[i])
}

Egrid$zone <- floor((Egrid$x+180)/2)*1000 + floor(Egrid$y+90)
saveRDS(Egrid, 'output/Egrid.RDS')}
Egrid <- readRDS('output/Egrid.RDS')
Egrid.agg <- aggregate(Egrid$zone, by=list(Egrid$zone), 'length')
colnames(Egrid.agg) <- c('zone','count')
t.colrange = grep("^t01$", colnames(Egrid)):grep("^t12$", colnames(Egrid))
k=5000
Egrid.s <- subset(Egrid.agg, count >= 25)
#define fields to rotate through ----
Egrid.s$rt01 <- NA
Egrid.s$rt02 <- NA
Egrid.s$rt03 <- NA
Egrid.s$rt04 <- NA
Egrid.s$rt05 <- NA
Egrid.s$rt06 <- NA
Egrid.s$rt07 <- NA
Egrid.s$rt08 <- NA
Egrid.s$rt09 <- NA
Egrid.s$rt10 <- NA
Egrid.s$rt11 <- NA
Egrid.s$rt12 <- NA
tr.cols <- grep("^rt01$", colnames(Egrid.s)):grep("^rt12$", colnames(Egrid.s))



if(T){

for (k in 1:nrow(Egrid.s)){
#for (k in 1:10){
Egridk <- Egrid[Egrid$zone %in% Egrid.s[k,1],]
for(i in 1:12){#i=7
  model.t <-lm(Egridk[,t.colrange[i]] ~ Elev + y+ x, data = Egridk) ;summary(model.t)
  Egrid.s[k,tr.cols[i]] = model.t$coefficients[2]
    
}}; saveRDS(Egrid.s, 'output/Egrid.s.RDS'); saveRDS(Egrid, 'output/Egrid.RDS')
}
#-----
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
