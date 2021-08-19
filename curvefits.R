

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#functions ---- 
e.trans <-  function(e){
  e1 = 0.5^((e/500-1)^2)
  return(e1)
}
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

p.trans <-  function(p){
  p1 = log2(pmax(0,p+0.0001)+100)
  return(p1)
}

p.vert <- function(p1){
  p = pmax(0,(2^(p1)-100)-0.0001)
  return(p)}

p.vert(p.trans(1000))
r.trans <-  function(r){#r=1
  r= pmin(1,(pmax(0, r)))
  r1 = log2((r*0.899999+0.1)/(1 - (r*0.899999+0.1)))
  return(r1)
}
r.vert <-  function(r1){
  r = ((2^r1/(2^r1+1))-0.1)/0.899999
  r = pmin(1,(pmax(0, r)))
  return(r)
}
t.trans <-  function(tr){
  tr1 = log2(tr+0.0001)
  return(tr1)
}
t.vert <-  function(tr1){
  tr = 2^(tr1)-0.0001
  return(tr)
}

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
pre.tab <- read.csv('output/clim.tab.fill.csv')
cols.th <- colnames(pre.tab[,grep("^th01$", colnames(pre.tab)):grep("^th12$", colnames(pre.tab))])
cols.tl <- colnames(pre.tab[,grep("^tl01$", colnames(pre.tab)):grep("^tl12$", colnames(pre.tab))])
cols.p <- colnames(pre.tab[,grep("^p01$", colnames(pre.tab)):grep("^p12$", colnames(pre.tab))])
cols.th.2080 <- colnames(pre.tab[,grep("^th.2080.01$", colnames(pre.tab)):grep("^th.2080.12$", colnames(pre.tab))])
cols.tl.2080 <- colnames(pre.tab[,grep("^tl.2080.01$", colnames(pre.tab)):grep("^tl.2080.12$", colnames(pre.tab))])
cols.p.2080 <- colnames(pre.tab[,grep("^p.2080.01$", colnames(pre.tab)):grep("^p.2080.12$", colnames(pre.tab))])


#Choose Warming or not (T/F) ----
warming = F



clim.tab.fill <- pre.tab
if(warming == T){
  clim.tab.fill <- clim.tab.fill[, !colnames(clim.tab.fill) %in% c(cols.th, cols.tl, cols.p)]
  colnames(clim.tab.fill)[colnames(clim.tab.fill) %in% cols.th.2080] <- cols.th
  colnames(clim.tab.fill)[colnames(clim.tab.fill) %in% cols.tl.2080] <- cols.tl
  colnames(clim.tab.fill)[colnames(clim.tab.fill) %in% cols.p.2080] <- cols.p
}else{
  clim.tab.fill <- clim.tab.fill[, !colnames(clim.tab.fill) %in% c(cols.th.2080, cols.tl.2080, cols.p.2080)]
}

if(is.null(clim.tab.fill$t01)){for (i in 1:12){
  clim.tab.fill$x <- (clim.tab.fill[,paste0('th',month[i])] + clim.tab.fill[,paste0('tl',month[i])]) /2
  colnames(clim.tab.fill)[colnames(clim.tab.fill) == 'x'] <- paste0("t", month[i])
}
}
colrange = grep("^t01$", colnames(clim.tab.fill)):grep("^t12$", colnames(clim.tab.fill))
clim.tab.fill$t.mean <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='mean')
clim.tab.fill$t.min <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='min')
clim.tab.fill$t.max <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='max')
colrange = grep("^th01$", colnames(clim.tab.fill)):grep("^th12$", colnames(clim.tab.fill))
clim.tab.fill$th.mean <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='mean')
colrange = grep("^tl01$", colnames(clim.tab.fill)):grep("^tl12$", colnames(clim.tab.fill))
clim.tab.fill$tl.mean <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='mean')
clim.tab.fill$tm.range <- t.trans(clim.tab.fill$t.max - clim.tab.fill$t.min)
clim.tab.fill$td.range <- t.trans(clim.tab.fill$th.mean - clim.tab.fill$tl.mean)
colrange = grep("^p01$", colnames(clim.tab.fill)):grep("^p12$", colnames(clim.tab.fill))
clim.tab.fill$p.sum <- p.trans(apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='sum'))
colrange = grep("^p01$", colnames(clim.tab.fill)):grep("^p12$", colnames(clim.tab.fill))
clim.tab.fill$p.max <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='max')
clim.tab.fill$p.min <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='min')
clim.tab.fill$p.ratio <- r.trans(clim.tab.fill$p.min/(clim.tab.fill$p.max+0.000001))
clim.tab.fill$Elev2 <- e.trans(clim.tab.fill$Elev)
#----

clim.tab <- subset(clim.tab.fill, !is.na(p.sum), select=c("Station_ID","Station_Name","State","Lat","Lon","Elev","Elev2",
                                                          "t.mean","t.max", "t.min","tm.range","th.mean","td.range","p.sum","p.ratio"))

#Choose Station Name ----
st.name = 'GRAND RAPIDS'

station <- subset(clim.tab.fill, grepl(st.name,Station_Name) & !is.na(p.sum)) [1,]
sLat =   station$Lat[1]  
sLon =   station$Lon[1]  
sElev =   station$Elev[1]  
shape=2
localzone = 50
cutoff = 500
clim.tab$altdifwt <- (clim.tab$Elev - sElev)^2/((clim.tab$Elev - sElev)^2 + 500^2)

clim.tab$dist <- (((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5
clim.tab$wt <- (localzone/(clim.tab$dist+localzone))^shape*100
clim.tab$cutoff <- cutoff + cutoff*clim.tab$altdifwt/2
midElev <- (quantile(clim.tab[clim.tab$dist < clim.tab$cutoff, ]$Elev,.99) + quantile(clim.tab[clim.tab$dist < clim.tab$cutoff, ]$Elev,.01))/2

clim.tab[clim.tab$dist > clim.tab$cutoff,]$wt <- 0
clim.tab$wt.low <- clim.tab$wt * e.wtlow(clim.tab$Elev, midElev)
clim.tab$wt.high <- clim.tab$wt * e.wthigh(clim.tab$Elev, midElev)


summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.low))
summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.high))
summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt))

model.1A <- summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.low))
model.1B <- summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.high))
f.t.meanA = model.1A$coefficients[2]
f.t.meanB = model.1B$coefficients[2]


model.2.1A <- lm(t.max ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.low)
f.t.maxA = model.2.1A$coefficients[2]
model.2.1B <- lm(t.max ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.high)
f.t.maxB = model.2.1B$coefficients[2]

model.2.2A <- lm(t.min ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.low)
f.t.minA = model.2.2A$coefficients[2]
model.2.2B <- lm(t.min ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.high)
f.t.minB = model.2.2B$coefficients[2]



model.3.1A <- lm(th.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.low)
f.th.meanA = model.3.1A$coefficients[2]
model.3.1B <- lm(th.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.high)
f.th.meanB = model.3.1B$coefficients[2]

model.4A <- lm(p.sum ~ Elev + Lat + Lon, data = clim.tab, weights = clim.tab$wt.low)
summary(model.4A)
f.p.sumA = model.4A$coefficients[2]
model.4B <- lm(p.sum ~ Elev + Lat + Lon, data = clim.tab, weights = clim.tab$wt.high)
summary(model.4B)
f.p.sumB = model.4B$coefficients[2]


model.5A <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.low)
f.p.ratioA = model.5A$coefficients[2]
model.5B <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.high)
f.p.ratioB = model.5B$coefficients[2]

#Choose Elevation ----
Elev1 = 2000

station$t.mean1 <- f.t.meanA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.t.meanB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$t.mean 
station$t.mean
station$t.mean1

station$t.max1 <- f.t.maxA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.t.maxB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$t.max 
station$t.max
station$t.max1
station$t.min1 <- f.t.minA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.t.minB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev))  + station$t.min 
station$t.min
station$t.min1
station$t.rangeA <- station$t.max - station$t.mean
station$t.rangeB <- station$t.mean - station$t.min
station$t.rangeA1 <- station$t.max1 - station$t.mean1
station$t.rangeB1 <- station$t.mean1 - station$t.min1
station$th.mean1 <- f.th.meanA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.th.meanB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$th.mean

station$td.rangeA1 <- (station$th.mean1 - station$t.mean1)*2

station$p.sum1 <- f.p.sumA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.p.sumB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$p.sum 
p.vert(station$p.sum)
p.vert(station$p.sum1)
station$p.ratio1 <- f.p.ratioA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.p.ratioA * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$p.ratio 
r.vert(station$p.ratio)
r.vert(station$p.ratio1)



t.colrange = grep("^t01$", colnames(station)):grep("^t12$", colnames(station))
th.colrange = grep("^th01$", colnames(station)):grep("^th12$", colnames(station))
tl.colrange = grep("^tl01$", colnames(station)):grep("^tl12$", colnames(station))
p.colrange = grep("^p01$", colnames(station)):grep("^p12$", colnames(station))

pfactor <-   apply(1-((1-station[,p.colrange]/station$p.max)), MARGIN = 1, FUN='sum')/apply(1-((1-station[,colrange]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))), MARGIN = 1, FUN='sum')*p.vert(station$p.sum1)/ p.vert(station$p.sum)


#New Table ----
clim.tab2 <- NULL

for(i in 1:12){#i=1
  Mon = i
  Lat=station$Lat
  Lon=station$Lon
  sElev=sElev
  Elev1=Elev1
  p <- (1-((1-station[,p.colrange[i]]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))))*station$p.max*pfactor[1]
  t <- ifelse(station[,t.colrange[i]]> station$t.mean, 
              (station[,t.colrange[i]]-station$t.mean)/station$t.rangeA*station$t.rangeA1+station$t.mean + (station$t.mean1 - station$t.mean),(station[,t.colrange[i]]-station$t.mean)/station$t.rangeB*station$t.rangeB1+station$t.mean + (station$t.mean1 - station$t.mean))[1]
  th <- t + (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
  tl <- t - (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
  p.o <- station[,p.colrange[i]]
  t.o <- station[,t.colrange[i]]
  th.o <- station[,th.colrange[i]]
  tl.o <- station[,tl.colrange[i]]
  
  
  clim.tab0 <- data.frame(cbind(Mon,Lat,Lon,sElev,Elev1,p.o,p,t.o,t,th.o,tl.o,th,tl))
  if(is.null(clim.tab2)){clim.tab2 <- clim.tab0}else{clim.tab2 <- rbind(clim.tab2,clim.tab0)}
}
rownames(clim.tab2)<- clim.tab2$Mon;clim.tab0<- NULL

#PET ----
DaysMonth <- readRDS('data/DaysMonth.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
monind <- c(12,1:12,1)
Elev <- Elev1
climtab <- subset(clim.tab2, select=c(p,t,th,tl))
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
climtab$hs <- NULL ; climtab$declination <- NULL
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
