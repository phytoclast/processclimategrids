setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DaysMonth <- readRDS('data/DaysMonth.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)

Lat = 43.074722
xy = as.data.frame(list(x=-89.384167, y=Lat))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
climtab <- NA
for (i in 1:12){
  mon = month[i]
  th = extract(rast(paste0('data/tx',month[i],'.tif')), xy)[2]
  tl = extract(rast(paste0('data/tn',month[i],'.tif')), xy)[2]
  p = extract(rast(paste0('data/p',month[i],'.tif')), xy)[2]
  climtab0 <- cbind(mon, th, tl, p)
  if(is.na(climtab)){climtab=climtab0}else{climtab <- rbind(climtab, climtab0)}
}


i=7
declination <- DaysMonth[i,]$declination
Days <- DaysMonth[i,]$Days
t <- 22.9 
tr <- 25
th <- t+tr/2
tl <- t-tr/2
Lat <- 34
hs <- acos(min(max(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                 cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
e <- 0.008404*216.7*exp(17.26939*t/
                          (t+237.3))/(t+273.3)*(Ra)*Days*abs((th - tl))^0.5 + 0.001
# ------
dr <- 1+0.033*cos(2*3.141592/365*DaysMonth[i,]$Day_)
ws <- acos(-tan(Lat*2*3.141592/360)*tan(DaysMonth[i,]$declination))
Rsa <- (24*60/3.141592)*0.08202*dr*(
  cos(Lat*2*3.141592/360) * cos(DaysMonth[i,]$declination) * sin(ws) + ws * sin(Lat*2*3.141592/360)*sin(DaysMonth[i,]$declination)
)
# ------
Elev <- 0 #m
Ps <- 101.3*((293-0.0065*Elev)/293)^5.26 #kPa
Vs <- 0.611*exp(17.27*t/
                  (t+237.3))
Vd <- 0.611*exp(17.27*tl/
                  (t+237.3))
Rns <- (1-0.23)*Ra
sigma <- 4.903*10^-9
Tkmax <- th+273.16
Tkmin <- tl+273.16
Rso <- (0.75+2*10^-5*Elev)*Ra
Rs <- Ra
Rnl <- sigma*(Tkmax^4 + Tkmin^4)/2*(0.34-0.14*Vd^0.5)*(1.35*min(Rs/Rso, 1) - 0.35)
Rn <- Rns - Rnl
delta <- 4098*Vs*t/(t+237.3)
lambda <- 2.501 - (2.361*10^-3)*t
gamma <- 1.013*Ps/(0.622*lambda)
alpha <- 1.26
e.pt <- alpha * delta / (delta + gamma) * Rn *Days
#Turc ----
alpha = 0.01333
e.tc <- alpha *((23.9001*Rs)+50)*t/(t+15)*(1+(50-min(50,RH))/70)*Days

#Thornthwaite ----
I = 3*(22/5)^1.514 + 6*((22+-5)/2/5)^1.514 + 3*(0/5)^1.514
a = 0.49239+1792*10^-5*I-771*10^-7*I^2+675*10^-9*I^3
Ta = (22+-5)/2
et0 = 16*(10*Ta/I)^a
dm=31
N=15
e.th <- et0*(N/12)*(dm/30)

#Hargreaves-Samani ----
#Kt = 0.17
#e.hs <- 0.0135*Kt*(T+17.78)*(th-tl)^0.5*Ra*Days
e.hs <- 0.0023*(T+17.78)*(th-tl)^0.5*Ra*Days

#Reduced PM ----
Rs = max(0,0.7*Ra - 4) # Estimate of normally measured solar radiation
Ps <- 101.3*((293-0.0065*Elev)/293)^5.26 #kPa
Vp = 0.6108*exp(17.27*t/(t+237.3)) #saturation vapor pressure kPa
Vpmax = 0.6108*exp(17.27*th/(th+237.3)) #saturation vapor pressure kPa
Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
Vp = (Vpmax+Vpmin)/2
RH = Vpmin/mean(Vpmax,Vpmin)*100  #Daily Relative humidity % assuming low temperature is saturated
Gi = 0.07*(t-t) #ignore for now




# PT ----
alpha = 1.26
lambda = 2.501 - 0.002361*t #MJ/kg latent heat of vaporization
#delta = 0.200*(0.00738*t + 0.8072)^7 - 0.000116
delta = 2503*exp(17.27*t/(t+237.3))/(t+237.3)^2 #kPa/C slope of vaporization
#Ps = 101.3 - 0.01055*Elev
Ps <- 101.3*((293-0.0065*Elev)/293)^5.26 #kPa Cp=1.013 KJ/kg/C
#gamma.pt = 1.013*Ps/(0.622*lambda) #psychrometric constant kPa/C
gamma = 0.000665*Ps #psychrometric constant kPa/C
Rso = (0.75 + 2*10^-5*Elev)*Ra #clear sky radiation
#Rs = max(0.3*Rso,0.7*Ra - 4) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1
Rs = min(max(Rso,0.3*Rso, 0.16*(th-tl)^0.5*Ra)) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1 and using formula for Hargreaves with constant of 0.16 inland and 0.19 for coastal
Gi = 0.07*(t-t)#heat flux monthly periods previous and following months t[i+1] - t[i-1] make index to capture months in a cycle 1:14 for c(12,1:12,1); 1=m+1; m=months 1:12

fcd = (1.35*Rs/Rso-0.35)
Rns =  (1 - 0.23)*Rs #albedo of 0.23
Rnl = 4.901*10^-9 * fcd * (0.34 - 0.14 * Vpmin^0.5) * ((th+273.16)^4 + (tl+273.16)^4)/2
Rn = Rns - Rnl #MJ/m2/d
Rn.pt = Rn*23.9006 #cal/m2/day
#f = (1.2*Rs/Ra+0.1)#Rn = 0.77*Rs - 2.45*10^-9*f*(0.261*exp(-7.7710*10^-4*t^2) - 0.02)*((th+273.16)^4 + (tl+273.16)^4)+ 0.83
#e.pt <- alpha * (delta / (delta + gamma.pt))*(Rn.pt-Gi)*Days * 10 #original in cm *10 for mm
e.pt <- alpha * (delta / (delta + gamma))*(Rn-Gi)/lambda*Days #

e.pm <- (0.408*delta*(Rn-Gi)+gamma*900/(t+273)*2*(Vp-Vpmin))/(delta+gamma*(1+0.34*2))*Days #mm asuming windspeed or 2 m/s
