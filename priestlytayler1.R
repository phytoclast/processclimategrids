library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DaysMonth <- readRDS('data/DaysMonth.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
monind <- c(12,1:12,1)
#acquire monthly data from grids ---- 
Lat = 39.768611; Lon = -86.158056 # Indianapolis
Lat = 43.074722; Lon = -89.384167 # Madison
Lat =34.05; Lon = -118.25 #Los Angeles
Lat = 42.961111; Lon =  -85.655556 #Grand Rapids
#Lat = 67; Lon =  -100 #47.925278, -97.0325

xy = as.matrix(as.data.frame(list(x=Lon, y=Lat)))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")
Elev = extract(rast(paste0('wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')), xy)[1,2]; names(Elev) <- NULL
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
climtab <- NA
for (i in 1:12){
  mon = month[i]
  th = extract(rast(paste0('data/tx',month[i],'.tif')), xy)[2] ; names(th) <- 'th'
  tl = extract(rast(paste0('data/tn',month[i],'.tif')), xy)[2] ; names(tl) <- 'tl'
  p = extract(rast(paste0('data/p',month[i],'.tif')), xy)[2] ; names(p) <- 'p'
  climtab0 <- as.data.frame(cbind(mon, th, tl, p))
  rownames(climtab0) <- i
  climtab0$th <- as.numeric(climtab0$th);  climtab0$tl <- as.numeric(climtab0$tl);  climtab0$p <- as.numeric(climtab0$p)
  if(is.na(climtab)){climtab=climtab0}else{climtab <- rbind(climtab, climtab0)}
}
#climtab <- read.csv('data/fernandez.csv')

climtab0 <- NULL
#Humidity ----
climtab$t <- (climtab$th+climtab$tl)/2
climtab$Vpmax = 0.6108*exp(17.27*climtab$th/(climtab$th+237.3)) #saturation vapor pressure kPa
climtab$Vpmin = 0.6108*exp(17.27*climtab$tl/(climtab$tl+237.3)) #saturation vapor pressure kPa
climtab$Vp = (climtab$Vpmax+climtab$Vpmin)/2
climtab$RH = climtab$Vpmin/climtab$Vp*100



#calculate radiation ----
climtab$declination <- NA
climtab$Days <- NA
for(i in 1:12){
  climtab[i,]$declination <- DaysMonth[i,]$declination
  climtab[i,]$Days <- DaysMonth[i,]$Days
}

climtab$hs <- acos(pmin(pmax(-tan(Lat/360*2*3.141592) * tan(climtab$declination),-1),1))
climtab$Ra <- 117.5 * (climtab$hs*sin(Lat/360*2*3.141592)*sin(climtab$declination) +
                 cos(Lat/360*2*3.141592)*cos(climtab$declination)*sin(climtab$hs)) / 3.141592
climtab$Dl <- ifelse(Lat + climtab$declination*360/2/3.141592 > 89.16924, 24, ifelse(Lat - climtab$declination*360/2/3.141592 >= 90, 0, (atan(-((sin(-0.83/360*2*3.141592)-sin(climtab$declination)*sin(Lat/360*2*3.141592))/(cos(climtab$declination)*cos(Lat/360*2*3.141592)))/(-((sin(-0.83/360*2*3.141592)-sin(climtab$declination)*sin(Lat/360*2*3.141592))/(cos(climtab$declination)*cos(Lat/360*2*3.141592)))*((sin(-0.83/360*2*3.141592)-sin(climtab$declination)*sin(Lat/360*2*3.141592))/(cos(climtab$declination)*cos(Lat/360*2*3.141592)))+1)^0.5)+2*atan(1))/3.141592*24))
#climtab$hs <- NULL ; climtab$declination <- NULL
climtab$Rso <- (0.75+2*10^-5*Elev)*climtab$Ra 
climtab$Rs <- pmin(climtab$Rso,pmax(0.3*climtab$Rso, 0.14*(climtab$th-climtab$tl)^0.5*climtab$Ra)) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1 and using formula for Hargreaves with average constant of 0.175 for 0.16 inland and 0.19 for coastal, but reduced to 0.14 because of bias suggests it is 0.8 of the actual values at a few selected stations
climtab$Rnl <- 4.901*10^-9 * (1.35*climtab$Rs/(climtab$Rso+0.000001)-0.35) * (0.34 - 0.14 * climtab$Vpmin^0.5) * ((climtab$th+273.16)^4 + (climtab$tl+273.16)^4)/2
climtab$Rns <- (1-0.23)*climtab$Rs
climtab$Rn <- pmax(0,climtab$Rns - climtab$Rnl)
climtab$Gi = 0.07*(climtab[monind[as.numeric(rownames(climtab))+2],]$t - climtab[monind[as.numeric(rownames(climtab))],]$t)

climtab$delta <- 2503*exp(17.27*climtab$t/(climtab$t+237.3))/(climtab$t+237.3)^2


climtab$lambda <- 2.501 - (2.361*10^-3)*climtab$t
Ps <- 101.3*((293-0.0065*Elev)/293)^5.26 #kPa
gamma = 0.000665*Ps

#calculate radiation on slope ----
if(F){
hs <- climtab$hs[7]
declination <- climtab$declination[7]
aspect = 180*2*3.141592/360
slope = 30*2*3.141592/360
lat = Lat*2*3.141592/360

o1x=asin(sin(lat)*cos(slope) - cos(lat)*sin(slope)*cos(aspect))
d=1/2*(h1-h0); e=1/2*(h1-h0)
g = asin(sin(B)*sin(aspect)*1/cos(o1x))
wx = acos(-tan(o1x)*tan(o6))



Rslope = (sin(o1x)/sin(o1)*(d - sin(d)*cos(e)*cos(g)/cos(wx)))/(hs-tan(hs))
}


# ----
climtab$I = (pmax(0,climtab$t)/5)^1.514#Thornthwaite
I <- sum(climtab$I); climtab$I <- NULL#Thornthwaite
a = 0.49239+1792*10^-5*I-771*10^-7*I^2+675*10^-9*I^3#Thornthwaite
cf <- 0.92/1.26 #Correction factor to make for forest and mixed landuse vegetation instead of short grass, based on alpha of Priestly-Taylor equation

climtab$e.tw = 16*(10*pmax(climtab$t,0)/I)^a*(climtab$Dl/12)*(climtab$Days/30)#Thornthwaite

climtab$e.ho <- 58.93/365*pmax(0, climtab$t)*climtab$Days#Holdridge

climtab$e.gs <- 0.008404*216.7*exp(17.26939*climtab$t/
                          (climtab$t+237.3))/(climtab$t+273.3)*(climtab$Ra)*climtab$Days*abs((climtab$th - climtab$tl))^0.5 + 0.001#Schmidt

climtab$e.pt <- cf* 1.26 * (climtab$delta / (climtab$delta + gamma))*pmax(0,(climtab$Rn-climtab$Gi))/climtab$lambda*climtab$Days #Priestley-Taylor

climtab$e.pm <- cf* (0.408*climtab$delta*pmax(0,(climtab$Rn-climtab$Gi))+gamma*900/(climtab$t+273)*2*(climtab$Vp-climtab$Vpmin))/(climtab$delta+gamma*(1+0.34*2))*climtab$Days #Penman-Monteith

climtab$e.hs <- cf* 0.408*0.0023*(climtab$t+17.78)*(climtab$th-climtab$tl)^0.5*climtab$Ra*climtab$Days#Hargreaves Samani 

climtab$e.tc <- cf* 0.01333 *((23.9001*climtab$Rs)+50)*pmax(climtab$t,0)/(pmax(climtab$t,0)+15)*(1+(50-pmin(50,climtab$RH))/70)*climtab$Days#Turc

climtab$e.mh <- cf* 0.7 * (climtab$delta / (climtab$delta + gamma))*climtab$Rs/climtab$lambda*climtab$Days#Makkink-Hansen

climtab$e.hm = 0.1651 * climtab$Dl * (216.7 * (6.108 * exp(17.26939*pmax(climtab$t,0) / (pmax(climtab$t,0) + 237.3))) / (pmax(climtab$t,0) + 273.3)) * 2.376169#Hamon (last factor is correlation coefficient 1.2)

#Remove excess columns
climtab <- subset(climtab, select= -c(Vp, Vpmax, Vpmin, delta, lambda, Rns, Rnl, Rso)) 
write.csv(climtab,'output/climtab.csv')
#Evaluate totals for bias correction using literature values
mean(climtab$Rs)# MJ/m2/d
mean(climtab$Rs)/24/3600*1000000# W/m2
mean(climtab$RH)
(173.2/204.8751+ # Correction of Rs for Charleston, MO; Lat= 36.921389; Lon =  -89.346389
    164/220.5132+ # Correction of Rs for Aberdeen, Idaho; Lat= 42.943333; Lon =  -112.839444
    233.6/258.7754+ #Oasis, CA; Lat= 33.5275; Lon =  -116.126111
    159.6/196.1291+ # Correction of Rs for Spring Green, WI; Lat= 43.183889; Lon = -90.103333
    158.4/227.455+ # Correction of Rs for Fort Collins, CO; Lat =40.559167; Lon =  -105.078056
    207.7/229.5699+ # Correction of Rs for Dixon, CA; Lat = 38.449167; Lon =  -121.826944
    145.7/182.1581)/7 # Correction of Rs for Grand Forks, ND; Lat = 47.925278; Lon =  -97.0325

#Thornthwaite
sum(climtab$e.tw) 
#Holdridge
sum(climtab$e.ho) 
#Schmidt
sum(climtab$e.gs)
#Priestley-Taylor
sum(climtab$e.pt)
#Penman- Monteith
sum(climtab$e.pm)
#Hargreaves Samani
sum(climtab$e.hs)
#Turc
sum(climtab$e.tc)
#Hamon
sum(climtab$e.hm)

totalp <- sum(climtab$p)
#Thornthwaite
totalp/sum(climtab$e.tw) 
#Holdridge
totalp/sum(climtab$e.ho) 
#Schmidt
totalp/sum(climtab$e.gs)
#Priestley-Taylor
totalp/sum(climtab$e.pt)
#Penman- Monteith
totalp/sum(climtab$e.pm)
#Hargreaves Samani
totalp/sum(climtab$e.hs)
#Turc
totalp/sum(climtab$e.tc)
#Hamon
totalp/sum(climtab$e.hm)
sum(climtab$e.tw)/sum(climtab$e.hm)*1.2

#single month calculation
i=7
Days <- DaysMonth[i,]$Days
t = climtab[i,]$t #Mean temperature C
th = climtab[i,]$th #daily high temperature
tl = climtab[i,]$tl #daily low temperature
declination = DaysMonth[i,]$declination
hs <- acos(min(max(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                 cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592 #MJ/m2/d
e.gs <- 0.008404*216.7*exp(17.26939*t/
                          (t+237.3))/(t+273.3)*(Ra)*Days*abs((th - tl))^0.5 + 0.001 #mm PET


# ------
Vpmax = 0.6108*exp(17.27*th/(th+237.3)) #saturation vapor pressure kPa
Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
Vp = (Vpmax+Vpmin)/2
RH = Vpmin/mean(Vpmax,Vpmin)*100  #Daily Relative humidity % assuming low temperature is at the dew point
lambda = 2.501 - 0.002361*t #MJ/kg latent heat of vaporization
#delta = 0.200*(0.00738*t + 0.8072)^7 - 0.000116
delta = 2503*exp(17.27*t/(t+237.3))/(t+237.3)^2 #kPa/C slope of vaporization
#Ps = 101.3 - 0.01055*Elev
Ps <- 101.3*((293-0.0065*Elev)/293)^5.26 #kPa Cp=1.013 KJ/kg/C
#gamma.pt = 1.013*Ps/(0.622*lambda) #psychrometric constant kPa/C
gamma = 0.000665*Ps #psychrometric constant kPa/C
Rso = (0.75 + 2*10^-5*Elev)*Ra #clear sky radiation
#Rs = max(0.3*Rso,0.7*Ra - 4) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1
Rs = min(Rso,max(0.3*Rso, 0.175*(th-tl)^0.5*Ra)) # Estimate of measured solar radiation. The ratio of Rs/Rso is limited to 0.3-1 and using formula for Hargreaves averaging a constant of 0.16 inland and 0.19 for coastal. 
Gi = 0.07*(t-t)#heat flux monthly periods previous and following months t[i+1] - t[i-1] 

fcd = (1.35*Rs/Rso-0.35)
Rns =  (1 - 0.23)*Rs #albedo of 0.23
Rnl = 4.901*10^-9 * fcd * (0.34 - 0.14 * Vpmin^0.5) * ((th+273.16)^4 + (tl+273.16)^4)/2
Rn = Rns - Rnl #MJ/m2/d
Cn = 900 #constant for 12 cm grass; 0.5 m vegetation supposedly =1600
Cd = 0.34 #constant for 12 cm grass; 0.5 m vegetation supposedly =0.38
u = 2 #windspeed m/s
alpha = 1.26 #alpha for the Priestley-Taylor equation, for short grass and shallow lakes. Alternative values as high as 1.57 for stongly advective (dry windy) conditions; 0.92 (0.73-1.18) for douglas-fir forest and other conifer forests; 1.06 for corn; 1.05 for tundra; 0.6-1 mixed land use Oklahoma
0.92/1.26
x=c(.73, .8, .84, 1.05, 1.18); mean(x)
x=c(1.07, .83,   1,  .95,.81, 1.1, 1.46, 1.08, 1.2, 1); mean(x)
e.mh <- 0.7 * (delta / (delta + gamma))*Rs/lambda*Days#Makkink-Hansen
e.tc <- 0.01333 *((23.9001*Rs)+50)*t/(t+15)*(1+(50-min(50,RH))/70)*Days #Turc PET
e.hs <- 0.0023*(T+17.78)*(th-tl)^0.5*Ra*Days #Hargreaves Samani PET
e.pt <- alpha * (delta / (delta + gamma))*(Rn-Gi)/lambda*Days # mm, Priestley-Taylor PET;  note that some versions use a different gamma with the lambda term there instead of main equation.

e.pm <- (0.408*delta*(Rn-Gi)+gamma*Cn/(t+273)*u*(Vp-Vpmin))/(delta+gamma*(1+Cd*u))*Days #mm, Penman- Monteith PET



#Set up grid extraction of multiple points ----
norms2010 <- readRDS('data/norms2010.RDS')
norm.stat <- unique(subset(norms2010, select=c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation" )))
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
stat.climtab = NULL

for (j in 1:10){#j=2
norm.stat[j,]$Station_Name
Lat0 = norm.stat[j,]$Latitude; Lon0 =  norm.stat[j,]$Longitude

xy = as.matrix(as.data.frame(list(x=Lon, y=Lat)))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")
Elev0 = extract(rast(paste0('wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')), xy)[1,2]; names(Elev0) <- NULL

climtab <- NULL
for (i in 1:12){#i=1
  StationID = norm.stat[j,]$Station_ID
  Station_Name = norm.stat[j,]$Station_Name
  Lat = Lat0
  Lon = Lon0
  Elev = Elev0
  Mon = i
  th = extract(rast(paste0('data/tx',month[i],'.tif')), xy)[2] ; names(th) <- 'th'
  tl = extract(rast(paste0('data/tn',month[i],'.tif')), xy)[2] ; names(tl) <- 'tl'
  p = extract(rast(paste0('data/p',month[i],'.tif')), xy)[2] ; names(p) <- 'p'
  climtab0 <- as.data.frame(cbind(StationID, Station_Name, Lat,Lon,Elev, Mon, th, tl, p))
   if(is.null(climtab)){climtab=climtab0}else{climtab <- rbind(climtab, climtab0)}
}
if(is.null(stat.climtab)){stat.climtab=climtab}else{stat.climtab <- rbind(stat.climtab, climtab)}}