library(terra)


month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
#load basic files ----
for (i in 1:12){
  assign(paste0('p',month[i]), rast(paste0('warming/p',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('tl',month[i]), rast(paste0('warming/tn',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('th',month[i]), rast(paste0('warming/tx',month[i],'.tif')))
}
if (T){
for (i in 1:12){
  assign(paste0('t',month[i]), (get(paste0('th',month[i])) + get(paste0('tl',month[i])) )/2 )
  writeRaster(get(paste0('t',month[i])), paste0('woutput/t',month[i],'.tif'))
}

#calculate extreme winter low ----
reduced <- aggregate(tl01, fact=5)

xyz <- as.data.frame(reduced, xy=TRUE)
Lat <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$y), type="xyz", crs=crs(tl01))
crs(Lat) <- crs(tl01)
Lon <- rast(cbind(x=xyz$x,y=xyz$y,z=xyz$x), type="xyz", crs=crs(tl01))
crs(Lon) <- crs(tl01)

Elev <- rast('C:/a/geo/climate/worldclim2.1/wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif' )
crs(Elev)=crs(tl01)
Tcl <- min(tl01,tl02,tl03,tl04,tl05,tl06,tl07,tl08,tl09,tl10,tl11,tl12)
writeRaster(Tcl, paste0('woutput/Tcl.tif'))
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

Lat <- resample(Lat, tl01, method="bilinear")
Tclx1 <- resample(Tclx1, tl01, method="bilinear")

Tclx<-	-9.171	+
  Tcl *	1.202	+
  Elev *	0.0008691	+
  Lat * Elev *	-0.00002455 +
  Tclx1
rm(pacificsouth, amazon2, amazon1, pacificcent,mexico ,florida,pacificnorth,oklahoma,arizona,chess,
   atlantic,himalayas,kentucky,ontario,montana,minn,hudson,siberia,california,washington,colorado,hawaii,Tclx1)
writeRaster(Tclx, paste0('woutput/Tclx.tif'))
plot((Tclx >= -15)+(Tclx >= 0), maxcell = 1000000)
# PET ----
DaysMonth <- readRDS('data/DaysMonth.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
PET <- function(i){
declination <- DaysMonth[i,]$declination
Days <- DaysMonth[i,]$Days
t <- get(paste0('t',month[i]))
th <- get(paste0('th',month[i]))
tl <- get(paste0('tl',month[i]))
hs <- acos(min(max(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
             cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
e <- 0.008404*216.7*exp(17.26939*t/
                          (t+237.3))/(t+273.3)*(Ra)*Days*abs((th - tl))^0.5 + 0.001
writeRaster(e, paste0('woutput/e',month[i],'.tif'))
return(e)}
for (i in 1:12){
  assign(paste0('e',month[i]), PET(i))
}
Tc <- min(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12)
writeRaster(Tc, paste0('woutput/Tc.tif'))
#Biotemperature ----
for (i in 1:12){
  assign(paste0('b',month[i]), (get(paste0('t',month[i]))>0)*(get(paste0('t',month[i]))) )
  #writeRaster(get(paste0('b',month[i])), paste0('woutput/b',month[i],'.tif'))
}
Bts <- max(mean(b01, b02, b03, b04, b11, b12), mean(b05, b06, b07, b08, b09, b10))
writeRaster(Bts, paste0('woutput/Bts.tif'), overwrite=T)
# Deficit/Surplus/pAET ----
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
writeRaster(deficit, paste0('woutput/deficit.tif'), overwrite=T)
writeRaster(surplus, paste0('woutput/surplus.tif'), overwrite=T)
writeRaster(pAET, paste0('woutput/pAET.tif'), overwrite=T)
#Moisture Index ----
e <- sum(e01,e02,e03,e04,e05,e06,e07,e08,e09,e10,e11,e12)
p <- sum(p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12)
m <- p/(e+0.0001)
writeRaster(e, paste0('woutput/e.tif'), overwrite=T)
writeRaster(p, paste0('woutput/p.tif'), overwrite=T)
writeRaster(m, paste0('woutput/m.tif'), overwrite=T)
}
# Load again ----
Tg <- rast(paste0('woutput/Bts.tif'))
Tc <- rast(paste0('woutput/Tc.tif'))
Tcl <- rast(paste0('woutput/Tcl.tif'))
Tclx <- rast(paste0('woutput/Tclx.tif'))
deficit <- rast(paste0('woutput/deficit.tif'))
surplus <- rast(paste0('woutput/surplus.tif'))
pAET <- rast(paste0('woutput/pAET.tif'))
m <- rast(paste0('woutput/m.tif'))
e <- rast(paste0('woutput/e.tif'))
p <- rast(paste0('woutput/p.tif'))
for (i in 1:12){
  assign(paste0('t',month[i]), rast(paste0('woutput/t',month[i],'.tif')))
}
for (i in 1:12){
  assign(paste0('e',month[i]), rast(paste0('woutput/e',month[i],'.tif')))
}
#Map Classification ----

Cindex <- min(Tc, Tclx+15)

isopluv <- (m >= 1) * (deficit < 150)
isoxeric <- (m < 0.5) * (surplus < 25)

mreg1 <- (isoxeric*-1+1)+isopluv
pluv1 <- (pAET >= 50)*(isopluv*-1+1)
pluv2 <- (pAET>=75)*(isopluv*-1+1)
Mreg2 <- mreg1*10+(pluv1 + pluv2)+ (m>=1)*10
plot(Mreg2, maxcell = 1000000)
writeRaster(Mreg2, paste0('woutput/Mreg2.tif'),options=c("COMPRESS=LZW"), overwrite=TRUE)
TRegime <- (Tg >= 18)*(Cindex >= 15)*7 + 
  (Tg >= 18)*(Cindex >= 0)*(Cindex < 15)*6+
  (Tg >= 6)*(Tg < 18)*(Cindex >= 0)*5+
  (Tg < 6)*(Cindex >= 0)*4+
  (Tg >= 12)*(Cindex < 0)*3 + 
  (Tg >= 6)*(Tg < 12)*(Cindex < 0)*2 + 
  (Tg < 6)*(Cindex < 0)*1
plot(TRegime, maxcell = 1000000)
writeRaster(TRegime, paste0('woutput/TRegime.tif'),options=c("COMPRESS=LZW"), overwrite=TRUE)
#combine mregime and tregime ----
ClimateRegime <-  TRegime*100+Mreg2

writeRaster(ClimateRegime, paste0('woutput/ClimateRegime.tif'),options=c("COMPRESS=LZW"), overwrite=TRUE)

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

writeRaster(ClimateRegime1, paste0('woutput/ClimateRegime5alt.tif'),options=c("COMPRESS=LZW"), overwrite=TRUE)
