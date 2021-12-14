library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
#load basic files ----
for (i in 1:12){
  assign(paste0('p',month[i]), rast(paste0('data/p',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('tl',month[i]), rast(paste0('output/amplified/tl',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('th',month[i]), rast(paste0('output/amplified/th',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('t',month[i]), rast(paste0('output/amplified/t',month[i],'.tif')))
}

#calculate extreme winter low ----
reduced <- aggregate(tl01, fact=5,  na.rm=TRUE)
xyz <- as.data.frame(reduced, xy=TRUE)
Lat <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$y), type="xyz", crs=crs(tl01))
crs(Lat) <- crs(tl01)
Lon <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$x), type="xyz", crs=crs(tl01))
crs(Lon) <- crs(tl01)
Lat1 <- resample(Lat, tl01, method="bilinear")
Elev <- rast('output/ElevAmp.tif' )
crs(Elev)=crs(tl01)

Tcl <- min(tl01,tl02,tl03,tl04,tl05,tl06,tl07,tl08,tl09,tl10,tl11,tl12)
writeRaster(Tcl, paste0('output/Tcl.tif'), overwrite=T)
pacificsouth <- 1/((((Lat - -22.7)/13)^2 + ((Lon - -82.3)/14)^2)^2+1)
amazon2 <- 1/((((Lat - -10.2)/5)^2 + ((Lon - -59.9)/10)^2)^2+1)
amazon1 <- 1/((((Lat - -2.8)/14)^2 + ((Lon - -61.3)/19)^2)^2+1)
pacificcent <- 1/((((Lat - 4.1)/21)^2 + ((Lon - -122.4)/41)^2)^2+1)
mexico <- 1/((((Lat - 26)/6)^2 + ((Lon - -98.4)/12)^2)^2+1)
florida <- 1/((((Lat - 27.5)/4)^2 + ((Lon - -81.1)/8)^2)^2+1)
pacificnorth <- 1/((((Lat - 32.9)/26)^2 + ((Lon - -145)/27)^2)^2+1)
oklahoma <- 1/((((Lat - 33.6)/4)^2 + ((Lon - -98.4)/8)^2)^2+1)
arizona <- 1/((((Lat - 34)/12)^2 + ((Lon - -113.1)/8)^2)^2+1)
atlantic <- 1/((((Lat - 34)/15)^2 + ((Lon - -60.7)/19)^2)^2+1)
himalayas <- 1/((((Lat - 35.3)/6)^2 + ((Lon - 91.3)/13)^2)^2+1)
kentucky <- 1/((((Lat - 38.5)/3)^2 + ((Lon - -87.6)/9)^2)^2+1)
ontario <- 1/((((Lat - 44.6)/2)^2 + ((Lon - -79.2)/6)^2)^2+1)
montana <- 1/((((Lat - 45.4)/5)^2 + ((Lon - -111.8)/10)^2)^2+1)
minn <- 1/((((Lat - 47.6)/6)^2 + ((Lon - -92.6)/12)^2)^2+1)
hudson <- 1/((((Lat - 60)/7)^2 + ((Lon - -87)/34)^2)^2+1)
siberia <- 1/((((Lat - 61.2)/20)^2 + ((Lon - 105.7)/39)^2)^2+1)
california <- 1/((((Lat - 34.8)/9)^2 + ((Lon - -128.2)/9)^2)^2+1)
washington <- 1/((((Lat - 46)/5)^2 + ((Lon - -126.6)/5)^2)^2+1)
colorado <- 1/((((Lat - 38.3)/2)^2 + ((Lon - -108.8)/3)^2)^2+1)
hawaii <- 1/((((Lat - 21.3)/7)^2 + ((Lon - -157.5)/11)^2)^2+1)
chess <- 1/((((Lat - 37)/3)^2 + ((Lon - -74)/3)^2)^2+1)

Tclx1 <-
  Lat *	-0.04149	+
  pacificsouth *	-1.792	+
  amazon2 *	2.573	+
  amazon1 *	-1.014	+
  pacificcent *	-0.749	+
  mexico *	-0.8227	+
  florida *	-3.557	+
  pacificnorth *	-1.246	+
  oklahoma *	0.1758	+
  arizona *	2.605	+
  chess *	0.8347 +
  atlantic *	0.2967	+
  himalayas *	-1.814	+
  kentucky *	-2.644 +
  ontario *	-2.314	+
  montana *	-4.415	+
  minn *	1.136 +
  hudson *	-5.154	+
  siberia *	-3.797	+
  california *	4.48 +
  washington *	3.597	+
  colorado *	1.458	+
  hawaii *	6.673	

Tclx1 <- resample(Tclx1, tl01, method="bilinear")
Tclx<-	-9.171	+
  Tcl *	1.202	+
  Elev *	0.0008691	+
  Lat1 * Elev *	-0.00002455 +
  Tclx1
rm(pacificsouth, amazon2, amazon1, pacificcent,mexico ,florida,pacificnorth,oklahoma,arizona,chess,
   atlantic,himalayas,kentucky,ontario,montana,minn,hudson,siberia,california,washington,colorado,hawaii,Tclx1)
writeRaster(Tclx, paste0('output/Tclx.tif'), overwrite=T)
plot((Tclx >= -15)+(Tclx >= 0), maxcell = 1000000)

# PET ----
Days <- c(31.00, 28.25, 31.00, 30.00, 31.00, 30.00, 31.00, 31.00, 30.00, 31.00, 30.00, 31.00)
DayNumber <- c(16.000,45.625,75.250,106.125,136.250,166.750,197.250,228.250,258.750,289.250,319.750,350.250)
dcl <- 0.409*sin(2*3.141592*DayNumber/365-1.39)

GetSolarRad <- function(i){
  declination <- dcl[i]
  hs <- acos(min(max(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
  Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                   cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
  Ra <- resample(Ra, Lat1, method="bilinear")
  names(Ra) <- paste0('Ra',month[i])
  writeRaster(Ra, paste0('output/Ra',month[i],'.tif'), overwrite=T)}
for (i in 1:12){#i=1
  GetSolarRad(i)
}
for (i in 1:12){
  assign(paste0('Ra',month[i]), rast(paste0('output/Ra',month[i],'.tif')))
}

GetVpmax <- function(i){  
  th <- get(paste0('th',month[i]))
  Vpmax = 0.6108*exp(17.27*th/(th+237.3)) #saturation vapor pressure kPa
  names(Vpmax) <- paste0('Vpmax',month[i])
  writeRaster(Vpmax, paste0('output/Vpmax',month[i],'.tif'), overwrite=T)}
for (i in 1:12){#i=1
GetVpmax(i)
}
for (i in 1:12){
  assign(paste0('Vpmax',month[i]), rast(paste0('output/Vpmax',month[i],'.tif')))
}

GetVpmin <- function(i){  
  tl <- get(paste0('tl',month[i]))
  Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
  names(Vpmin) <- paste0('Vpmin',month[i])
  writeRaster(Vpmin, paste0('output/Vpmin',month[i],'.tif'), overwrite=T)}
for (i in 1:12){#i=1
  GetVpmin(i)
}
for (i in 1:12){
  assign(paste0('Vpmin',month[i]), rast(paste0('output/Vpmin',month[i],'.tif')))
}

GetPET <- function(i){
  
  Ra <- get(paste0('Ra',month[i]))
  Vpmax <- get(paste0('Vpmax',month[i]))
  Vpmin <- get(paste0('Vpmin',month[i]))
  p <- get(paste0('p',month[i]))

  logp <- log(p+1)
  e <- Ra*0.0508780  +
    Vpmax*0.7893714  +
    Vpmin*-0.5589255  +
    logp*-0.1309403  +
    Ra*Vpmax*0.0049383
  e[e<0] <- 0
  return(e)}

GetTransGrow <- function(i) {#Adjust to reduction in transpiration due to cold, with evaporation only outside growing season
  ts = 0.8 #assumed T/ET ratio during growing season
  tw = 0.0 #assumed T/ET ratio during freezing season
  th <- get(paste0('th',month[i]))
  tl <- get(paste0('tl',month[i]))
  t <- (th+tl)/2
  G0 <- (t-0)/10 
  G0[G0>1] = 1
  G0[G0<0] = 0 #proportion of monthly temperature above freezing considering daily temperature range
  evmin = (tw)+(1-ts)
  G = G0*(1-evmin)+evmin
  return(G)}

for (i in 1:12){#i=1
  assign(paste0('e',month[i]), 0.85*GetTransGrow(i)*GetPET(i)*Days[i]); writeRaster(get(paste0('e',month[i])), paste0('output/e',month[i],'.tif'), overwrite=T)
}
plot(e01)

Tc <- min(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12)
writeRaster(Tc, paste0('output/Tc.tif'), overwrite=T)
Tw <- max(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12)
writeRaster(Tw, paste0('output/Tw.tif'), overwrite=T)
Twh <- max(th01,th02,th03,th04,th05,th06,th07,th08,th09,th10,th11,th12)
writeRaster(Twh, paste0('output/Twh.tif'), overwrite=T)
#Biotemperature ----
for (i in 1:12){
  assign(paste0('b',month[i]), (get(paste0('t',month[i]))>0)*(get(paste0('t',month[i]))) )
}
Bts <- max(mean(b01, b02, b03, b04, b11, b12), mean(b05, b06, b07, b08, b09, b10))
writeRaster(Bts, paste0('output/Bts.tif'), overwrite=T)
# Deficit/Surplus/pAET ----
for (i in 1:12){
  assign(paste0('b',month[i]), (get(paste0('t',month[i]))>0)*(get(paste0('t',month[i]))) )
}
for (i in 1:12){
  assign(paste0('e',month[i]), rast(paste0('output/e',month[i],'.tif')))
}

for (i in 1:12){
  e = get(paste0('e',month[i]))
  p = get(paste0('p',month[i]))
  d <- max(e-p, 0)
  s <- max(p-e, 0)
  a <- min(p,e)
  if(i == 1){
    deficit <- d
    surplus <- s
    pAET <- a
  }else{
    deficit <- deficit + d
    surplus <- surplus + s
    pAET <- max(pAET, a)
  }}
writeRaster(deficit, paste0('output/deficit.tif'), overwrite=T)
writeRaster(surplus, paste0('output/surplus.tif'), overwrite=T)
writeRaster(pAET, paste0('output/pAET.tif'), overwrite=T)

qmon <- c('11','12','01','02','03','04','05','06','07','08','09','10','11','12')
for (i in 1:12){
    e0 = get(paste0('e',qmon[i+0]));e1 = get(paste0('e',qmon[i+1]));e2 = get(paste0('e',qmon[i+2]))
    p0 = get(paste0('p',qmon[i+0]));p1 = get(paste0('p',qmon[i+1]));p2 = get(paste0('p',qmon[i+2]))
    a <- (min(e0,p0)+min(e1,p1)+min(e2,p2))
    if(i == 1){
       p3AET <- a
    }else{
       p3AET <- max(p3AET, a)
    }}
writeRaster(p3AET, paste0('output/p3AET.tif'), overwrite=T)


#Moisture Index ----
e <- sum(e01,e02,e03,e04,e05,e06,e07,e08,e09,e10,e11,e12)
ept <- sum(ept01,ept02,ept03,ept04,ept05,ept06,ept07,ept08,ept09,ept10,ept11,ept12)
p <- sum(p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12)
m <- p/(e+0.0001)
writeRaster(e, paste0('output/e.tif'), overwrite=T)
writeRaster(p, paste0('output/p.tif'), overwrite=T)
writeRaster(m, paste0('output/m.tif'), overwrite=T)

# Load again ----
Tg <- rast(paste0('output/Bts.tif'))
Tc <- rast(paste0('output/Tc.tif'))
Tcl <- rast(paste0('output/Tcl.tif'))
Tw <- rast(paste0('output/Tw.tif')) 
Twh <- rast(paste0('output/Twh.tif'))
Tclx <- rast(paste0('output/Tclx.tif'))
deficit <- rast(paste0('output/deficit.tif'))
surplus <- rast(paste0('output/surplus.tif'))
pAET <- rast(paste0('output/pAET.tif'))
p3AET <- rast(paste0('output/p3AET.tif'))
m <- rast(paste0('output/m.tif'))
e <- rast(paste0('output/e.tif'))
p <- rast(paste0('output/p.tif'))
for (i in 1:12){
  assign(paste0('t',month[i]), rast(paste0('output/amplified/t',month[i],'.tif')))
}
#Map Classification ----

Cindex <- min(Tc, Tclx+15)

isopluv <- (m >= 1) * (deficit < 150)
isoxeric <- (m < 0.5) * (surplus < 25)

mreg1 <- (isoxeric*-1+1)+isopluv
pluv1 <- (p3AET>=120)*(isopluv*-1+1)
pluv2 <- (p3AET>=180)*(isopluv*-1+1)
pluv3 <- (p3AET>=180)*(isopluv*-1+1)
Mreg2 <- mreg1*10+(pluv1 + pluv2)+ (m>=1)*10
Mreg3 <- mreg1*10+(pluv3)+ (m>=1)*10
plot(Mreg2, maxcell = 1000000)
writeRaster(Mreg2, paste0('output/Mreg2.tif'), overwrite=TRUE)
writeRaster(Mreg3, paste0('output/Mreg3.tif'), overwrite=TRUE)
TRegime <- (Tg >= 18)*(Cindex >= 15)*7 + 
  (Tg >= 18)*(Cindex >= 0)*(Cindex < 15)*6+
  (Tg >= 6)*(Tg < 18)*(Cindex >= 0)*5+
  (Tg < 6)*(Cindex >= 0)*4+
  (Tg >= 12)*(Cindex < 0)*3 + 
  (Tg >= 6)*(Tg < 12)*(Cindex < 0)*2 + 
  (Tg < 6)*(Cindex < 0)*1
plot(TRegime, maxcell = 1000000)
writeRaster(TRegime, paste0('output/TRegime.tif'), overwrite=TRUE)
#combine mregime and tregime ----
ClimateRegime <-  TRegime*100+Mreg2

writeRaster(ClimateRegime, paste0('output/ClimateRegime.tif'), overwrite=TRUE)

tempclim <- (ClimateRegime == 730)*(Tg >= 24)#*(Cindex >= 20)
ClimateRegime1 <- tempclim*731 +
  (tempclim * -1+1) * ClimateRegime

tempclim <- (ClimateRegime1 == 630)*(Cindex >= 5)#*(Tg >= 24)
ClimateRegime1 <- tempclim*631 +
  (tempclim * -1+1) * ClimateRegime1

tempclim <- (ClimateRegime1 == 530)*(Tg >= 12)
ClimateRegime1 <- tempclim*531 +
  (tempclim * -1+1) * ClimateRegime1

tempclim <- (ClimateRegime1 == 330)*(Tg >= 18)*(Cindex >= -10)
ClimateRegime1 <- tempclim*332 +
  (tempclim * -1+1) * ClimateRegime1

tempclim <- (ClimateRegime1 == 330)*(Tg >= 15)*(Cindex >= -25)
ClimateRegime1 <- tempclim*331 +
  (tempclim * -1+1) * ClimateRegime1

writeRaster(ClimateRegime1, paste0('output/ClimateRegime5alt.tif'), overwrite=TRUE)


#reproject rasters for modeling ----
newTclx <- rast('C:/workspace2/bonapmexico/nam5k/Tclx.tif')

Tg.n <- project(Tg, newTclx, method='bilinear', filename = 'nam5k/Tg.tif', overwrite=T)
Tc.n <- project(Tc, newTclx, method='bilinear', filename = 'nam5k/Tc.tif', overwrite=T)
Tcl.n <- project(Tcl, newTclx, method='bilinear', filename = 'nam5k/Tcl.tif', overwrite=T)
Tw.n <- project(Tw, newTclx, method='bilinear', filename = 'nam5k/Tw.tif', overwrite=T)
Twh.n <- project(Twh, newTclx, method='bilinear', filename = 'nam5k/Twh.tif', overwrite=T)
Tclx.n <- project(Tclx, newTclx, method='bilinear', filename = 'nam5k/Tclx.tif', overwrite=T)
deficit.n <- project(deficit, newTclx, method='bilinear', filename = 'nam5k/d.tif', overwrite=T)
surplus.n <- project(surplus, newTclx, method='bilinear', filename = 'nam5k/s.tif', overwrite=T)
pAET.n <- project(pAET, newTclx, method='bilinear', filename = 'nam5k/pAET.tif', overwrite=T)
m.n <- project(m, newTclx, method='bilinear', filename = 'nam5k/m.tif', overwrite=T)
e.n <- project(e, newTclx, method='bilinear', filename = 'nam5k/e.tif', overwrite=T)
p.n <- project(p, newTclx, method='bilinear', filename = 'nam5k/p.tif', overwrite=T)
a <- e.n - deficit.n
writeRaster(a, filename = 'nam5k/a.tif', overwrite=T)


plot(a, maxcell=1000000)

#name rasters
library(stringr);library(terra);library(raster)
files <- list.files('nam5k')

for (i in 1:length(files)){
  nm <- str_split_fixed(files[i], '\\.',3)[,1]
  assign(nm, rast(paste0('nam5k/',files[i])))
  x <- get(nm)
  names(x) <- nm
  writeRaster(x, paste0('nam5k2/',files[i]), overwrite=T, compress='LZW')
}
for (i in 1:length(files)){i=13
  nm <- str_split_fixed(files[i], '\\.',3)[,1]
  assign(nm, raster(paste0('nam5k/',files[i])))
  x <- get(nm)
  names(x) <- nm
  writeRaster(x, paste0('nam5k2/',files[i]), overwrite=T, compress='LZW')
}

e.tab <- as.data.frame(e, xy=TRUE)
median(e.tab$tx01)
ept.tab <- as.data.frame(ept, xy=TRUE)
median(ept.tab$tx01)
usa.e <- subset(e.tab, x > -126 & x < -64 & y > 24 & y < 49)
usa.ept <- subset(ept.tab, x > -126 & x < -64 & y > 24 & y < 49)
median(usa.e$tx01)/median(usa.ept$tx01)*1.26
