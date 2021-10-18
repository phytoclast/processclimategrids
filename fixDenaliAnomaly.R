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
library(ranger)
df.s <- subset(df, !is.na(e)&!is.na(t10)&!is.na(t01))
df.train <- subset(df.s, d > -50000)
df.train2 <- df.train
df.train2$e <- 9000
df.train2$t01 <- -50
df.train <- rbind(df.train, df.train2)
#plot((elv.trm > 3000) * (dist.trm > 0000))
# x+y+d+e+t10+t11+t12+t01+t02+t03+t04
# rf <- ranger(t01 ~ x+y+e+t10+t04,
#              data=df.train, num.trees=200, max.depth = 20, importance = 'impurity', write.forest = TRUE)
lmod <- lm(t01 ~ x+y+e+t04+t10+t02,
             data=df.train)
summary(lmod)
# df.s$pred <- predictions(predict(rf, data=df.s))
df.s$pred <-   predict.lm(lmod, df.s)

df.rast <- rast(cbind(x=df.s$x,y=df.s$y,z=df.s$pred), type="xyz", crs=crs(t01))
crs(df.rast) <- crs(t01)
#plot(t01.trm)
#plot(df.rast * (dist.trm < 50000))
Elev10km <- aggregate(Elev5km, fact=2)
saga_search()
Elev5km2 <- raster(Elev5km)
saga <- saga_gis()
#print(saga$statistics_grid$focal_statistics)
radii = c(150,100,75,50,25,10)
dtn = c(0:7)*45
dtnL = c(345,30,75,120,165,210,255,300)
dtnR = c(15,60,105,150,195,240,285,330)
for (k in 1:length(dtn)){#k=1
  for (i in 1:length(radii)){#i=4;k=1
    sgridN <- Elev10km %>% saga$statistics_grid$focal_statistics(max='temp/tempN.sgrd',.all_outputs = FALSE,
                                                                 kernel_radius=radii[i],
                                                                 kernel_type = 3,
                                                                 kernel_direction = dtn[k],
                                                                 kernel_tolerance = 0)
    sgridL <- Elev10km %>% saga$statistics_grid$focal_statistics(max='temp/tempL.sgrd',.all_outputs = FALSE,
                                                                 kernel_radius=radii[i],
                                                                 kernel_type = 3,
                                                                 kernel_direction = dtnL[k],
                                                                 kernel_tolerance = 0)
    sgridR <- Elev10km %>% saga$statistics_grid$focal_statistics(max='temp/tempR.sgrd',.all_outputs = FALSE,
                                                                 kernel_radius=radii[i],
                                                                 kernel_type = 3,
                                                                 kernel_direction = dtnR[k],
                                                                 kernel_tolerance = 0)
    sgrid0 <- (sgridN + sgridL + sgridR)/3
    
    if(i==1){sgrid <- sgrid0}else{sgrid <- (sgrid0 + sgrid1)/2}
   
     sgrid1 <- sgrid %>% saga$statistics_grid$focal_statistics(mean='temp/temp.sgrd',.all_outputs = FALSE,
                                                             kernel_radius=1,
                                                             kernel_type = 2)
     sgrid2 <- resample(sgrid1, Elev5km2)
    }
  
  names(sgrid2) <- paste0('shad',dtn[k])
  writeRaster(sgrid2, paste0('output/shad',dtn[k],'.tif'), overwrite=T)
}
dxn <- rast('output/shad225.tif')
crs(dxn) <- crs(Elev5km)
dxnshd <-  dxn - Elev5km
writeRaster(dxnshd, 'output/dxnshd.tif', overwrite=T)
dxs <- rast('output/shad45.tif')
crs(dxs) <- crs(Elev5km)
dxsshd <-  dxs - Elev5km
writeRaster(dxsshd, 'output/dxsshd.tif', overwrite=T)
sd1 <- rast('output/shad0.tif')
sd2 <- rast('output/shad45.tif')
sd3 <- rast('output/shad90.tif')
sd4 <- rast('output/shad135.tif')
sd5 <- rast('output/shad180.tif')
sd6 <- rast('output/shad225.tif')
sd7 <- rast('output/shad270.tif')
sd8 <- rast('output/shad315.tif')
shdmin <- min(sd1,sd2,sd3,sd4,sd5,sd6,sd7,sd8);crs(shdmin) <- crs(Elev5km)
shdmax <- max(sd1,sd2,sd3,sd4,sd5,sd6,sd7,sd8);crs(shdmax) <- crs(Elev5km)
shdmind <- shdmin - Elev5km; writeRaster(shdmind, 'output/shdmind.tif', overwrite=T)
shdmaxd <- shdmax - Elev5km; writeRaster(shdmaxd, 'output/shdmaxd.tif', overwrite=T)
shdmean <- mean(sd1,sd2,sd3,sd4,sd5,sd6,sd7,sd8);crs(shdmean) <- crs(Elev5km)
shdmeand <- shdmean - Elev5km; writeRaster(shdmeand, 'output/shdmeand.tif', overwrite=T)
#Water influence
lakes <- vect('data/wlakes.shp')
lake.rast <- rasterize(lakes, Elev5km)
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
water2000 <- resample(water2000, Elev5km2)
water1000 <- resample(water1000, Elev5km2)
water500 <- resample(water500, Elev5km2)
water200 <- resample(water200, Elev5km2)
water100 <- resample(water100, Elev5km2)
water50 <- resample(water50, Elev5km2)
water20 <- resample(water20, Elev5km2)
water10 <- resample(water10, Elev5km2)

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


  