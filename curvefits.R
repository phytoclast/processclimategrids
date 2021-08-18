library(terra)
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
clim.tab.fill <- read.csv('output/clim.tab.fill.csv')
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


clim.tab <- subset(clim.tab.fill, !is.na(p.sum), select=c("Station_ID","Station_Name","State","Lat","Lon","Elev","Elev2",
                                                          "t.mean","t.max", "t.min","tm.range","th.mean","td.range","p.sum","p.ratio"))
station <- subset(clim.tab.fill, grepl('DEATH',Station_Name) & !is.na(p.sum)) [1,]
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
#Columns same table ----
if(F){
  for(i in 1:12){
    station$p.x <- (1-((1-station[,p.colrange[i]]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))))*station$p.max*pfactor
    station[,paste0('p.', month[i])] <- NULL
    colnames(station)[colnames(station) == 'p.x'] <- paste0('p.', month[i])
  }
  for(i in 1:12){
    station$t.x <- (station[,t.colrange[i]]-station$t.mean)/t.vert(station$tm.range)*t.vert(station$tm.range1)+station$t.mean + (station$t.mean1 - station$t.mean)
    station[,paste0('t.', month[i])] <- NULL
    colnames(station)[colnames(station) == 't.x'] <- paste0('t.', month[i])
  }
  
  for(i in 1:12){#i=1
    t..colrange = grep("^t\\.01$", colnames(station)):grep("^t\\.12$", colnames(station))
    
    station$th.x <- station[,t..colrange[i]] + (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*t.vert(station$td.range1)/2
    
    station[,paste0('th.', month[i])] <- NULL
    colnames(station)[colnames(station) == 'th.x'] <- paste0('th.', month[i])
  }
  for(i in 1:12){
    t..colrange = grep("^t\\.01$", colnames(station)):grep("^t\\.12$", colnames(station))
    
    station$tl.x <- station[,t..colrange[i]] - (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*t.vert(station$td.range1)/2
    station[,paste0('tl.', month[i])] <- NULL
    colnames(station)[colnames(station) == 'tl.x'] <- paste0('tl.', month[i])
  }
  
  
  
  t..colrange = grep("^t\\.01$", colnames(station)):grep("^t\\.12$", colnames(station))
  th..colrange = grep("^th\\.01$", colnames(station)):grep("^th\\.12$", colnames(station))
  tl..colrange = grep("^tl\\.01$", colnames(station)):grep("^tl\\.12$", colnames(station))
  p..colrange = grep("^p\\.01$", colnames(station)):grep("^p\\.12$", colnames(station))
  
  station[,t.colrange]
  station[,t..colrange]
  
  station[,th.colrange]
  station[,th..colrange]
  
  station[,tl.colrange]
  station[,tl..colrange]
  
  station[,p.colrange]
  station[,p..colrange]
}

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
