library(gstat)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(plyr)
library(terra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Egrid <- readRDS('output/Egrid.RDS')

t11 <- rast('output/t11.tif')
t12 <- rast('output/t12.tif')
t01 <- rast('output/t01.tif')
t02 <- rast('output/t02.tif')
t03 <- rast('output/t03.tif')
Elev5km = rast('output/Elev5km.tif')
crs(Elev5km) <-  crs(t01)
reduced <- aggregate(t01, fact=5,  na.rm=TRUE)
xyz <- as.data.frame(reduced, xy=TRUE)
Lat <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$y), type="xyz", crs=crs(t01))
crs(Lat) <- crs(t01)

egridsealevel <- subset(Egrid, Elev < 200 & Elev > -200 & t01 > -5)
egridsealevel$yy <- floor((egridsealevel$y+90)/5)
egridsealevel$xx <- floor((egridsealevel$x+180)/5)
#egridsealevel <- ddply(egridsealevel, .variables = c('xx','yy'), .fun = summarise,
egridsealevel <- ddply(egridsealevel, .variables = c('yy'), .fun = summarise,
                       z = quantile(t01, .99),
                       #x = mean(x),
                       y = mean(y)
                       )

plot(z~y, egridsealevel)


# 
# #https://michaelminn.net/tutorials/r-spatial-interpolation/index.html
# library(fields)
# spline1 = Tps(d[,c('x','y')],d$z, lon.lat = TRUE, miles = FALSE);splined1 = interpolate(r, spline1)
# spline2 = fastTps(d[,c('x','y')],d$z, theta = 50, lambda=2);splined2 = interpolate(r, spline2)
# spline3 = spatialProcess(d[,c('x','y')],d$z);splined3 = interpolate(r, spline3)
# 
# plot(splined3)



# library(interp) 
# library(dplyr)
# d <- egridsealevel
# nelev <- aggregate(Elev5km, fact=20, fun='mean', na.rm=T)
# nelev[nelev>=0] <- 1; nelev[nelev<0] <- 1
# r <- raster(nelev)
# r[r>=0] <- NA; r[r<0] <- NA
# 
# d<- subset(d, !is.na(x) &!is.na(y) &!is.na(z))
# 
# x <-  list(x=c(0:70)*5-175)
# d <- merge(d,x)
# d2 <- st_as_sf(d, coords = c("x", "y"), crs = crs(Elev5km))
# d2<- as_Spatial(d2)
# d3 <- vect(d2)
# nelev <-  Elev5km
# nelev <- aggregate(nelev, fact=10, fun='mean', na.rm=T)
# nelev[nelev>=0] <- 1; nelev[nelev<0] <- 1
# 
# 
# d.rast <-  rasterize(d3, nelev, field = 'z')
# d.rast.df <- as.data.frame(d.rast, xy=T, na.omit=F)
# d.points <-subset(d.rast.df, !is.na(z))
# TIN <- interp(x=d.points$x, y=d.points$y, z=d.points$z,
#               xo = d.rast.df$x, yo = d.rast.df$y,
#               output = "points") %>% bind_cols()
# TIN_intr <- rasterFromXYZ(TIN, crs="+proj=longlat +datum=WGS84")
# plot(TIN_intr, main="Triangular Irregular Network")
# writeRaster(TIN_intr,'output/TIN.tif', overwrite=T)
# t.sl <- rast('output/TIN.tif')
# t.sl <- focal(t.sl, na.rm=T, na.only=T, fun='median')
# 
# t.r <- (-30-t.sl)/9000
# plot(t.r)
# t.r <- resample(t.r, t01)
# t.sl <- resample(t.sl, t01)
# 
# mint <- Elev5km*t.r + t.sl

mintbin <-  (Elev5km > 2000)*(t01 > 0)
mintbin <- aggregate(mintbin, fact=20, fun='max', na.rm=TRUE)
Elev5km = rast('output/Elev5km.tif')
crs(Elev5km) <-  crs(t01)
reduced <- aggregate(t01, fact=20,  na.rm=TRUE)
xyz <- as.data.frame(reduced, xy=TRUE)
Lat <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$y), type="xyz", crs=crs(t01))
crs(Lat) <-  crs(t01)

denali <- (mintbin * (Lat > 55)) ; denali <- focal(denali, m=5, fun='max', na.rm=TRUE)
denali[denali == 0] <- NA
denali.dist <- distance(denali)
denali.curve <- 0.5^((denali.dist/1000000)^4)
t.sl <- (denali.curve*-1+1)*45+5
plot(t.sl)
t.hl <-  0.5^((Lat/45)^4)
t.hl <- -25*t.hl + -40*(t.hl*-1+1)
t.r <- (t.hl-t.sl)/9000

t.r <- resample(t.r, t01)
t.sl <- resample(t.sl, t01)

mint <- Elev5km*t.r + t.sl
mintbin <- mint > t01
plot(mintbin)
writeRaster(mint, 'output/mint.tif', overwrite=T)
writeRaster(mintbin , 'output/mintbin.tif', overwrite=T)


xx = c(0:90)*2-90
yy = 0.5^((xx/45)^4)
plot(yy~xx)
