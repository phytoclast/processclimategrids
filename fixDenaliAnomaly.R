library(gstat)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(plyr)
library(Rsagacmd)
library(terra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Egrid <- readRDS('output/Egrid.RDS')
#reasonable months for Denali
t10 <- rast('output/t10.tif')
t04 <- rast('output/t04.tif')

#bad values for Denali
t11 <- rast('output/t11.tif')
t12 <- rast('output/t12.tif')
t01 <- rast('output/t01.tif')
t02 <- rast('output/t02.tif')
t03 <- rast('output/t03.tif')
Elev5km = rast('output/Elev5km.tif')
crs(Elev5km) <-  crs(t01)
# reduced <- aggregate(t01, fact=5,  na.rm=TRUE)
# xyz <- as.data.frame(reduced, xy=TRUE)
# Lat <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$y), type="xyz", crs=crs(t01))
# crs(Lat) <- crs(t01)
# 
# egridsealevel <- subset(Egrid, Elev < 200 & Elev > -200 & t01 > -5)
# egridsealevel$yy <- floor((egridsealevel$y+90)/5)
# egridsealevel$xx <- floor((egridsealevel$x+180)/5)
# #egridsealevel <- ddply(egridsealevel, .variables = c('xx','yy'), .fun = summarise,
# egridsealevel <- ddply(egridsealevel, .variables = c('yy'), .fun = summarise,
#                        z = quantile(t01, .99),
#                        #x = mean(x),
#                        y = mean(y)
#                        )
# 
# plot(z~y, egridsealevel)
# 
# 
# # 
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

# mintbin <-  (Elev5km > 2000)*(t01 > 0)
# mintbin <- aggregate(mintbin, fact=20, fun='max', na.rm=TRUE)
# Elev5km = rast('output/Elev5km.tif')
# crs(Elev5km) <-  crs(t01)
# reduced <- aggregate(t01, fact=20,  na.rm=TRUE)
# xyz <- as.data.frame(reduced, xy=TRUE)
# Lat <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$y), type="xyz", crs=crs(t01))
# crs(Lat) <-  crs(t01)
# 
# denali <- (mintbin * (Lat > 55)) ; denali <- focal(denali, m=5, fun='max', na.rm=TRUE)
# denali[denali == 0] <- NA
# denali.dist <- distance(denali)
# denali.curve <- 0.5^((denali.dist/1000000)^4)
# t.sl <- (denali.curve*-1+1)*45+5
# plot(t.sl)
# t.hl <-  0.5^((Lat/45)^4)
# t.hl <- -25*t.hl + -40*(t.hl*-1+1)
# t.r <- (t.hl-t.sl)/9000
# 
# t.r <- resample(t.r, t01)
# t.sl <- resample(t.sl, t01)
# 
# mint <- Elev5km*t.r + t.sl
# mintbin <- mint > t01
# plot(mintbin)
# writeRaster(mint, 'output/mint.tif', overwrite=T)
# writeRaster(mintbin , 'output/mintbin.tif', overwrite=T)
# 
# 
# xx = c(0:90)*2-90
# yy = 0.5^((xx/45)^4)
# plot(yy~xx)
# 
# 
# 
# sealevelt <- c(-10.000000,
# -8.650000,
# -5.650000,
# -0.54999995,
# 6.0000000,
# 11.3500004,
# 13.600000,
# 12.200000,
# 7.50000000,
# -0.10000014,
# -7.450000,
# -9.400000)
# 
Elev5km = rast('output/Elev5km.tif')
crs(Elev5km) <-  crs(t01)
mintbin <-  (Elev5km > 2000)*(t01 > 0)

fake <- rast(xmin=-155, ymin=58, xmax=-135, ymax=65, crs=crs(t01), res=res(mintbin))
minttrim <- crop(mintbin, fake)
minttrim[minttrim < 1] <- NA
dist.trm <-  distance(minttrim)
plot(dist.trm)

elv.trm  <- crop(Elev5km, fake)
t10.trm <- crop(t10, fake)
t11.trm <- crop(t11, fake)
t12.trm <- crop(t12, fake)
t01.trm <- crop(t01, fake)
t02.trm <- crop(t02, fake)
t03.trm <- crop(t03, fake)
t04.trm <- crop(t04, fake)

brk = c(dist.trm,
          elv.trm,
          t10.trm,
          t11.trm,
          t12.trm,
          t01.trm,
          t02.trm,
          t03.trm,
          t04.trm)
df <- as.data.frame(brk, xy=T)
colnames(df) <- c('x', 'y', 'd', 'e', 't10', 't11', 't12', 't01', 't02', 't03', 't04')


#Rain Shadows ----
library(gstat)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(plyr)
library(Rsagacmd)
library(terra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
saga <- saga_gis()

Elev5km = rast('output/Elev5km.tif')
Elev5km2 = raster('output/Elev5km.tif')
Elev10km <- aggregate(Elev5km, fact=2)

radii = c(150,100,75,50,25,10)
dtn = c(0:7)*45
dtnV = dtn[c(5,6,7,8,1,2,3,4)]
dtnL = dtn[c(7,8,1,2,3,4,5,6)]
dtnR = dtn[c(3,4,5,6,7,8,1,2)]
for (k in 1:length(dtn)){#k=6
  for (i in 1:length(radii)){#i=1;k=1
    sgridN <- Elev10km %>% saga$statistics_grid$focal_statistics(max='temp/tempN.sgrd',.all_outputs = FALSE,
                                                                 kernel_radius=radii[i],
                                                                 kernel_type = 3,
                                                                 kernel_direction = dtn[k],
                                                                 kernel_tolerance = 0)
    
    
    if(i==1){sgrid <- sgridN}else{sgrid <- (sgridN + sgrid*(2-1))/2}
  }
  sgridV <- sgrid %>% saga$statistics_grid$focal_statistics(mean='temp/sgridV.sgrd',.all_outputs = FALSE,
                                                            kernel_radius=10,
                                                            kernel_type = 3,
                                                            kernel_direction = dtnV[k],
                                                            kernel_tolerance = 0)
  sgridL <- sgridV %>% saga$statistics_grid$focal_statistics(mean='temp/tempL.sgrd',.all_outputs = FALSE,
                                                             kernel_radius=2,
                                                             kernel_type = 3,
                                                             kernel_direction = dtnL[k],
                                                             kernel_tolerance = 0)
  sgridR <- sgridV %>% saga$statistics_grid$focal_statistics(mean='temp/tempR.sgrd',.all_outputs = FALSE,
                                                             kernel_radius=2,
                                                             kernel_type = 3,
                                                             kernel_direction = dtnR[k],
                                                             kernel_tolerance = 0)
  sgrid1 <- min((sgridL+sgridR)/2, sgrid)
  sgrid2 <- resample(sgrid1, Elev5km2)
  #sgrid2 <- Elev5km2-sgrid2
  #writeRaster(sgrid2, 'output/sgrid.tif', overwrite=T)
  
  names(sgrid2) <- paste0('shadw',dtn[k])
  writeRaster(sgrid2, paste0('output/shadw',dtn[k],'.tif'), overwrite=T)
}

#Water influence ----
library(gstat)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(plyr)
library(Rsagacmd)
library(terra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
saga <- saga_gis()

Elev5km = rast('output/Elev5km.tif')
Elev5km2 = raster('output/Elev5km.tif')
lakes <- vect('data/wlakes.shp')
lake.rast <- terra::rasterize(lakes, Elev5km)
lake.rast[lake.rast >= 0] <- 1
lake.rast[is.na(lake.rast)] <- 0
Ocean <- Elev5km; Ocean[Ocean >= 0] <- 0; Ocean[Ocean <= 0] <- 0; Ocean[is.na(Ocean)]<-1
Water <- max(Ocean, lake.rast)
plot(Water)
Water10km <- aggregate(Water, fact=2)
Water100km <- aggregate(Water10km, fact=10)
Water200km <- aggregate(Water100km, fact=2)

radii = c(10,5,2,1)
plot(Water100km)
water2000 <- Water200km %>% saga$statistics_grid$focal_statistics(mean='temp/temp.sgrd',.all_outputs = FALSE,
                                                                  kernel_radius=10,
                                                                  kernel_type = 2)
water1000 <- Water100km %>% saga$statistics_grid$focal_statistics(mean='temp/temp7.sgrd',.all_outputs = FALSE,
                                                                  kernel_radius=10,
                                                                  kernel_type = 2)
water500 <- Water100km %>% saga$statistics_grid$focal_statistics(mean='temp/temp6.sgrd',.all_outputs = FALSE,
                                                                 kernel_radius=5,
                                                                 kernel_type = 2)
water200 <- Water100km %>% saga$statistics_grid$focal_statistics(mean='temp/temp5.sgrd',.all_outputs = FALSE,
                                                                 kernel_radius=2,
                                                                 kernel_type = 2)
water100 <- Water10km %>% saga$statistics_grid$focal_statistics(mean='temp/temp4.sgrd',.all_outputs = FALSE,
                                                                kernel_radius=10,
                                                                kernel_type = 2)
water50 <- Water10km %>% saga$statistics_grid$focal_statistics(mean='temp/temp3.sgrd',.all_outputs = FALSE,
                                                               kernel_radius=5,
                                                               kernel_type = 2)
water20 <- Water10km %>% saga$statistics_grid$focal_statistics(mean='temp/temp2.sgrd',.all_outputs = FALSE,
                                                               kernel_radius=2,
                                                               kernel_type = 2)
water10 <- Water %>% saga$statistics_grid$focal_statistics(mean='temp/temp1.sgrd',.all_outputs = FALSE,
                                                           kernel_radius=2,
                                                           kernel_type = 2)
water2000 <- raster::resample(water2000, Elev5km2)
water1000 <- raster::resample(water1000, Elev5km2)
water500 <- raster::resample(water500, Elev5km2)
water200 <- raster::resample(water200, Elev5km2)
water100 <- raster::resample(water100, Elev5km2)
water50 <- raster::resample(water50, Elev5km2)
water20 <- raster::resample(water20, Elev5km2)
water10 <- raster::resample(water10, Elev5km2)
raster::writeRaster(water2000, 'output/water2000.tif', overwrite=T)
raster::writeRaster(water1000, 'output/water1000.tif', overwrite=T)
raster::writeRaster(water500, 'output/water500.tif', overwrite=T)
raster::writeRaster(water200, 'output/water200.tif', overwrite=T)
raster::writeRaster(water100, 'output/water100.tif', overwrite=T)
raster::writeRaster(water50, 'output/water50.tif', overwrite=T)
raster::writeRaster(water20, 'output/water20.tif', overwrite=T)
raster::writeRaster(water10, 'output/water10.tif', overwrite=T)

water2000<-rast('output/water2000.tif')
water1000<-rast('output/water1000.tif')
water500<-rast('output/water500.tif')
water200<-rast('output/water200.tif')
water100<-rast('output/water100.tif')
water50<-rast('output/water50.tif')
water20<-rast('output/water20.tif')
water10<-rast('output/water10.tif')


oceanic <- (water2000*8+water1000*7+water500*6+water200*5+
                water100*4+water50*3+water20*2+water10)/(8+7+6+5+4+3+2+1)

writeRaster(oceanic, 'output/oceanic.tif', overwrite=T)
oceanic <- rast('output/oceanic.tif')
plot(oceanic)

oceanic.aspect <-  terra::terrain((oceanic*-10000),  v="aspect")
plot(oceanic.aspect)

compwt <- function(A){
  max(cos((A)*3.141592*2/360),0)
}
cwt1 <- compwt(oceanic.aspect - 0) 
cwt2 <- compwt(oceanic.aspect - 45)
cwt3 <- compwt(oceanic.aspect - 90)
cwt4 <- compwt(oceanic.aspect - 135)
cwt5 <- compwt(oceanic.aspect - 180)
cwt6 <- compwt(oceanic.aspect - 225)
cwt7 <- compwt(oceanic.aspect - 270)
cwt8 <- compwt(oceanic.aspect - 315)

oceanicwtshad <- (
  cwt1 * sd1 +
  cwt2 * sd2 +
  cwt3 * sd3 +
  cwt4 * sd4 +
  cwt5 * sd5 +
  cwt6 * sd6 +
  cwt7 * sd7 +
  cwt8 * sd8)/
  (cwt1+cwt2+cwt3+cwt4+cwt5+cwt6+cwt7+cwt8)
crs(oceanicwtshad) <- crs(Elev5km)
oceanicwtshad <- oceanicwtshad-Elev5km
writeRaster(oceanicwtshad, 'output/oceanicwtshad.tif', overwrite=T)
crs(oceanic) <- crs(Elev5km)



####################################
#Model Temperature ----
library(gstat)
library(sf)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(plyr)
library(Rsagacmd)
library(terra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Egrid <- readRDS('output/Egrid.RDS')
#reasonable months for Denali
t10 <- rast('output/t10.tif')
t04 <- rast('output/t04.tif')

#bad values for Denali
t11 <- rast('output/t11.tif')
t12 <- rast('output/t12.tif')
t01 <- rast('output/t01.tif')
t02 <- rast('output/t02.tif')
t03 <- rast('output/t03.tif')
Elev5km = rast('output/Elev5km.tif')
terra::crs(Elev5km) <-  terra::crs(t01)


sd1 <- rast('output/shadw0.tif')
sd2 <- rast('output/shadw45.tif')
sd3 <- rast('output/shadw90.tif')
sd4 <- rast('output/shadw135.tif')
sd5 <- rast('output/shadw180.tif')
sd6 <- rast('output/shadw225.tif')
sd7 <- rast('output/shadw270.tif')
sd8 <- rast('output/shadw315.tif')
crs(sd1) <- crs(t01)
crs(sd2) <- crs(t01)
crs(sd3) <- crs(t01)
crs(sd4) <- crs(t01)
crs(sd5) <- crs(t01)
crs(sd6) <- crs(t01)
crs(sd7) <- crs(t01)
crs(sd8) <- crs(t01)

water2000<-rast('output/water2000.tif')
water1000<-rast('output/water1000.tif')
water500<-rast('output/water500.tif')
water200<-rast('output/water200.tif')
water100<-rast('output/water100.tif')
water50<-rast('output/water50.tif')
water20<-rast('output/water20.tif')
water10<-rast('output/water10.tif')
oceanic<-rast('output/oceanic.tif')

crs(oceanic) <- crs(t01)
crs(Elev5km) <- crs(t01)

# invzone <- (oceanic * (500) + (oceanic*-1+1) * (2000)) * (Elev5km>0)
# plot(invzone)

Elev5km = rast('output/Elev5km.tif')
crs(Elev5km) <-  crs(t01)
mintbin <-  (Elev5km > 2000)*(t01 > 0)

fake <- rast(xmin=-158, ymin=55, xmax=-132, ymax=68, crs=crs(t01), res=res(mintbin))
#fake <- rast(xmin=-160, ymin=18, xmax=-154, ymax=23, crs=crs(t01), res=res(mintbin))
minttrim <- crop(mintbin, fake)
minttrim[minttrim < 1] <- NA
dist.trm <-  raster::distance(raster(minttrim))
dist.trm <- rast(dist.trm)
plot(dist.trm)

elv.trm  <- crop(Elev5km, fake)
t10.trm <- crop(t10, fake)
t11.trm <- crop(t11, fake)
t12.trm <- crop(t12, fake)
t01.trm <- crop(t01, fake)
t02.trm <- crop(t02, fake)
t03.trm <- crop(t03, fake)
t04.trm <- crop(t04, fake)
w2000.trm <- crop(water2000, fake)
w1000.trm <- crop(water1000, fake)
w500.trm <- crop(water500, fake)
w200.trm <- crop(water200, fake)
w100.trm <- crop(water100, fake)
w50.trm <- crop(water50, fake)
w20.trm <- crop(water20, fake)
w10.trm <- crop(water10, fake)
ocean.trm <- crop(oceanic, fake)

sd1.trm <- crop(sd1, fake)
sd2.trm <- crop(sd2, fake)
sd3.trm <- crop(sd3, fake)
sd4.trm <- crop(sd4, fake)
sd5.trm <- crop(sd5, fake)
sd6.trm <- crop(sd6, fake)
sd7.trm <- crop(sd7, fake)
sd8.trm <- crop(sd8, fake)

# sd1.trm <- sd1.trm - elv.trm
# sd2.trm <- sd2.trm - elv.trm
# sd3.trm <- sd3.trm - elv.trm
# sd4.trm <- sd4.trm - elv.trm
# sd5.trm <- sd5.trm - elv.trm
# sd6.trm <- sd6.trm - elv.trm
# sd7.trm <- sd7.trm - elv.trm
# sd8.trm <- sd8.trm - elv.trm

crs(w2000.trm) <-  crs(t01)
crs(w1000.trm) <-  crs(t01)
crs(w500.trm) <-  crs(t01)
crs(w200.trm) <-  crs(t01)
crs(w100.trm) <-  crs(t01)
crs(ocean.trm) <-  crs(t01)

invzone <- (ocean.trm * (1500) + (ocean.trm*-1+1) * (1500))
invA <- (elv.trm - invzone)*((elv.trm - invzone) >=0)
invB <- (elv.trm - invzone)*((elv.trm - invzone) < 0)
sdmin <- min(sd1.trm,sd2.trm,sd3.trm,sd4.trm,sd4.trm,sd5.trm,sd6.trm,sd7.trm,sd5.trm,sd8.trm)
sdmax <- max(sd1.trm,sd2.trm,sd3.trm,sd4.trm,sd4.trm,sd5.trm,sd6.trm,sd7.trm,sd5.trm,sd8.trm)


brk = c(dist.trm,
        elv.trm,
        t10.trm,
        t11.trm,
        t12.trm,
        t01.trm,
        t02.trm,
        t03.trm,
        t04.trm,
        w2000.trm,
        w1000.trm,
        w500.trm,
        w200.trm,
        w100.trm,
        w50.trm,
        w20.trm,
        w10.trm,
        sd1.trm,
        sd2.trm,
        sd3.trm,
        sd4.trm,
        sd5.trm,
        sd6.trm,
        sd7.trm,
        sd8.trm,
        ocean.trm,
        sdmin,
        sdmax,
        invA,
        invB)
df <- as.data.frame(brk, xy=T)
colnames(df) <- c('x', 'y', 'd', 'e', 't10', 't11', 't12', 't01', 't02', 't03', 't04',
                  'w2000',
                  'w1000',
                  'w500',
                  'w200',
                  'w100',
                  'w50',
                  'w20',
                  'w10',
                  'sd000',
                  'sd045',
                  'sd090',
                  'sd135',
                  'sd180',
                  'sd225',
                  'sd270',
                  'sd315',
                  'ocean',
                  'sdmin',
                  'sdmax',
                  'invA',
                  'invB')
library(ranger)
df.s <- subset(df, !is.na(e)&!is.na(t10)&!is.na(t01))
df.s$south <- apply(df.s[,c('sd135', 'sd180', 'sd225', 'sd270')], MARGIN = 1, FUN = 'mean')
df.s$north <- apply(df.s[,c('sd000', 'sd045','sd270', 'sd315')], MARGIN = 1, FUN = 'mean')
df.s$eocean <- df.s$ocean*(df.s$e*-1+1) 
df.train <- subset(df.s, d > 0000) 

df.train$invzone <- (df.train$ocean * (1500) + (df.train$ocean*-1+1) * (1500))
df.train2 <- df.train
df.train2$e <- 9000

df.train2$invA <- (9000 - df.train$invzone)*((9000 - df.train$invzone) >=0)
df.train2$invB <- (9000 - df.train$invzone)*((9000 - df.train$invzone) < 0)
df.train2$eocean <- df.train2$ocean*(df.train2$e*-1+1)
df.train2$t01 <- -50
df.train <- rbind(df.train, df.train2)

#plot((elv.trm > 3000) * (dist.trm > 0000))
# x+y+d+e+t10+t11+t12+t01+t02+t03+t04+w2000+w1000+w500+w200+w100+w50+w20+w10+sd000+sd045+sd090+sd135+sd180+sd225+sd270+sd315

#random forest
# rf <- ranger(t01 ~ x+y+w2000+w1000+w500+w200+w100+w50+w20+w10+invA+invB+sd000+sd045+sd090+sd135+sd180+sd225+sd270+sd315,
#              data=df.train, num.trees=200, max.depth = 7, importance = 'impurity', write.forest = TRUE)
# df.s$pred <-    predictions(predict(rf, data=df.s))
# df.rast <- rast(cbind(x=df.s$x,y=df.s$y,z=df.s$pred), type="xyz", crs=crs(t01))
# crs(df.rast) <- crs(t01)
# plot(df.rast)

#linear model
# lmod <- lm(t01 ~ x+y+e+w2000+w1000+w500+w200+w100+w50+w20+w10+sd000+sd045+sd090+sd135+sd180+sd225+sd270+sd315,
#            data=df.train)
# summary(lmod)
# library(olsrr)
# ols_step_backward_p(lmod, details = T)
# cor(df.train[,c('t01', 'x', 'y', 'w2000', 'w1000', 'w500', 'w200', 'w100', 'w50', 'w20',
#                 'w10', 'sd000', 'sd045', 'sd090', 'sd135', 'sd180', 'sd225', 'sd270', 'sd315', 'invA', 'invB')])


lmod <- lm(t01 ~ x+y+invA+invB+sdmin+sdmax+eocean+w2000+w1000+w500+w200+w100+w50+w20+w10+sd000+sd045+sd090+sd135+sd180+sd225+sd270+sd315,
           data=df.train)
summary(lmod)

df.s$pred <-   predict.lm(lmod, df.s)

df.rast <- rast(cbind(x=df.s$x,y=df.s$y,z=df.s$pred), type="xyz", crs=crs(t01))
crs(df.rast) <- crs(t01)
plot(df.rast)

writeRaster(df.rast,'output/df.rast.tif', overwrite=T)


