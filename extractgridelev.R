library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ----

norms2010 <- readRDS('data/norms2010.RDS')
norm.stat <- unique(subset(norms2010, select=c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation" )))
xy = as.matrix(as.data.frame(list(x=norm.stat$Longitude, y=norm.stat$Latitude)))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")

Elev = rast('wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')
Elev.10 <- aggregate(Elev, fact=50)

xy.rast <- terra::rasterize(xy, Elev.10)
plot(xy.rast)
xy.dist <- distance(xy.rast)

xy.500 <- xy.dist <500000
xy.500[xy.500 == 0] <- NA
xy.500.vect <- terra::as.polygons(xy.500)
xy.sample <- terra::spatSample(xy.500.vect, 20000)
plot(xy.sample)
xy.500.rs <- resample(xy.500, Elev)
Elev.500 <- Elev * xy.500.rs
Elev.tab <- as.data.frame(Elev.500, xy=TRUE, cells=FALSE, na.rm=TRUE)


Elev.max <- aggregate(Elev, fact=22, fun='max', na.rm=TRUE);res(Elev.max)
Elev.min <- aggregate(Elev, fact=22, fun='min', na.rm=TRUE)
Elev.mean <- aggregate(Elev, fact=22, fun='mean', na.rm=TRUE)
Elev.mean.tab <- as.data.frame(Elev.mean, xy=TRUE, cells=TRUE, na.rm=TRUE)
Elev.mean.vect <- vect(Elev.mean.tab, geom=c("x", "y"), crs=crs(Elev.mean))
Elev.cell <- terra::rasterize(Elev.mean.vect, Elev.mean, field='cell')


Elev.max <- resample(Elev.max, Elev, method="near") * xy.500.rs
Elev.min <- resample(Elev.min, Elev, method="near") * xy.500.rs
Elev.cell <- resample(Elev.cell, Elev, method="near") * xy.500.rs
Elev.500 <- Elev * xy.500.rs
Elev.brick <- c(Elev.500, Elev.max, Elev.min, Elev.cell)
Elev.tab <- as.data.frame(Elev.brick, xy=TRUE, cells=FALSE, na.rm=TRUE); colnames(Elev.tab) <- c('Lon','Lat','Elev','E.max','E.min', 'E.cell')
Elev.tab.sub <- subset(Elev.tab, Elev == E.max | Elev == E.min)
Elev.tab.agg <- aggregate(list(meanLon = Elev.tab.sub$Lon, meanLat = Elev.tab.sub$Lat), by=list(Elev = Elev.tab.sub$Elev, E.cell = Elev.tab.sub$E.cell), FUN='mean')
Elev.tab.merge <- merge(Elev.tab.sub, Elev.tab.agg, by=c('Elev','E.cell'))
Elev.tab.merge$dist <- (((Elev.tab.merge$Lon - Elev.tab.merge$meanLon)*.75)^2+((Elev.tab.merge$Lat - Elev.tab.merge$meanLat))^2)^0.5
Elev.tab.mindist <- aggregate(list(mindist = Elev.tab.merge$dist), by=list(Elev = Elev.tab.merge$Elev, E.cell = Elev.tab.merge$E.cell), FUN='min')
Elev.tab.merge <- merge(Elev.tab.merge, Elev.tab.mindist, by=c('Elev','E.cell'))
Elev.tab.merge <- subset(Elev.tab.merge, dist == mindist )
Elev.tab.agg <- aggregate(list(Lon = Elev.tab.merge$Lon, Lat = Elev.tab.merge$Lat), by=list(Elev = Elev.tab.merge$Elev, E.cell = Elev.tab.merge$E.cell), FUN='mean')

saveRDS(Elev.tab.agg,'output/Elev.tab.RDS')

#Load points then extract ----
#
#
#
Elev.tab <- readRDS('output/Elev.tab.RDS')
norms2010 <- readRDS('data/norms2010.RDS')

norm.stat <- unique(subset(norms2010, select=c("Station_ID","Station_Name","State","Latitude","Longitude","Elevation" )))
norm.stat$E.cell <- NA
colnames(norm.stat) <- c("Station_ID","Station_Name","State","Lat","Lon","Elev", "E.cell")
Elev.tab$Station_ID <- NA; Elev.tab$Station_Name <- NA; Elev.tab$State <- NA
Elev.tab <- subset(Elev.tab, select=c("Station_ID","Station_Name","State","Lat","Lon","Elev", "E.cell"))
Elev.tab <- rbind(Elev.tab, norm.stat)
colnames(Elev.tab) <- c("Station_ID","Station_Name","State", "Lat","Lon","Elevation","E.cell")  

xy = as.matrix(as.data.frame(list(x=Elev.tab$Lon, y=Elev.tab$Lat)))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")

xy.Elev = extract(rast(paste0('wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')), xy);  xy.Elev <- subset(xy.Elev, select= -ID);colnames(xy.Elev) <- c('Elev')
clim.tab.fill <- cbind(Elev.tab, xy.Elev)
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')#i=7
for (i in 1:12){
  th = extract(rast(paste0('data/tx',month[i],'.tif')), xy); th <- subset(th, select= -ID); colnames(th) <- paste0('th',month[i])
  clim.tab.fill <- cbind(clim.tab.fill, th)
}
for (i in 1:12){
  tl = extract(rast(paste0('data/tn',month[i],'.tif')), xy); tl <- subset(tl, select= -ID); colnames(tl) <- paste0('tl',month[i])
  clim.tab.fill <- cbind(clim.tab.fill, tl)
}
for (i in 1:12){
  p = extract(rast(paste0('data/p',month[i],'.tif')), xy); p <- subset(p, select= -ID); colnames(p) <- paste0('p',month[i])
  clim.tab.fill <- cbind(clim.tab.fill, p)
}

# warming ---- 
for (i in 1:12){
  th = pmax(extract(rast(paste0('warming/tx',month[i],'.tif')), xy), extract(rast(paste0('warming/tn',month[i],'.tif')), xy)); th <- subset(th, select= -ID); colnames(th) <- paste0('th.2080.',month[i])
  clim.tab.fill <- cbind(clim.tab.fill, th)
}
for (i in 1:12){
  tl = pmin(extract(rast(paste0('warming/tx',month[i],'.tif')), xy), extract(rast(paste0('warming/tn',month[i],'.tif')), xy)); tl <- subset(tl, select= -ID); colnames(tl) <- paste0('tl.2080.',month[i])
  clim.tab.fill <- cbind(clim.tab.fill, tl)
}

for (i in 1:12){
  p = extract(rast(paste0('warming/p',month[i],'.tif')), xy); p <- subset(p, select= -ID); colnames(p) <- paste0('p.2080.',month[i])
  clim.tab.fill <- cbind(clim.tab.fill, p)
}


write.csv(clim.tab.fill, 'output/clim.tab.fill.csv', na='', row.names = F)

#Load again ====
library(terra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#functions ---- 
e.trans <-  function(e){
  e1 = 0.5^((e/500-1)^2)
  return(e1)
}
e.wtlow <-  function(e){
  e1 = 0.01^(e/500-1)
  e1 = e1/(e1+1)
  return(e1)
}
e.wthigh <-  function(e){
  e1 = 0.01^(1-e/500)
  e1 = e1/(e1+1)
  return(e1)
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
station <- subset(clim.tab.fill, grepl('GRAND RAPIDS',Station_Name) & !is.na(p.sum))
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
clim.tab[clim.tab$dist > clim.tab$cutoff,]$wt <- 0
clim.tab$wt.low <- clim.tab$wt * e.wtlow(clim.tab$Elev)
clim.tab$wt.high <- clim.tab$wt * e.wthigh(clim.tab$Elev)


summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.low))
summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt.high))
summary(lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt))

model.1 <- lm(t.mean ~ Elev + Elev2 + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
summary(model.1)
f.t.mean = model.1$coefficients[2]
f.t.mean2 = ifelse(is.na(model.1$coefficients[3]),0,model.1$coefficients[3])
model.2 <- lm(tm.range ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.tm.range = model.2$coefficients[2]
model.2.1 <- lm(t.max ~ Elev + Elev2 + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.t.max = model.2.1$coefficients[2]
f.t.max2 = ifelse(is.na(model.2.1$coefficients[3]),0,model.2.1$coefficients[3])
model.2.2 <- lm(t.min ~ Elev + Elev2 + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.t.min = model.2.2$coefficients[2]
f.t.min2 = ifelse(is.na(model.2.2$coefficients[3]),0,model.2.2$coefficients[3])
model.3 <- lm(td.range ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.td.range = model.3$coefficients[2]
model.3.1 <- lm(th.mean ~ Elev + Elev2 + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.th.mean = model.3.1$coefficients[2]
f.th.mean2 = ifelse(is.na(model.3.1$coefficients[3]),0,model.3.1$coefficients[3])
model.4 <- lm(p.sum ~ Elev + Elev2 + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
summary(model.4)
f.p.sum = model.4$coefficients[2]
f.p.sum2 = model.4$coefficients[3]
model.5 <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.p.ratio = model.5$coefficients[2]

Elev1 = 4000

station$t.mean1 <- f.t.mean * (Elev1 - station$Elev) + f.t.mean2 * (e.trans(Elev1) - e.trans(station$Elev)) + station$t.mean 
station$t.mean
station$t.mean1
station$tm.range1 <- f.tm.range * (Elev1 - station$Elev) + station$tm.range 
t.vert(station$tm.range)
t.vert(station$tm.range1)
station$t.max1 <- f.t.max * (Elev1 - station$Elev) + f.t.max2 * (e.trans(Elev1) - e.trans(station$Elev))  + station$t.max 
station$t.max
station$t.max1
station$t.min1 <- f.t.min * (Elev1 - station$Elev) + f.t.min2 * (e.trans(Elev1) - e.trans(station$Elev))  + station$t.min 
station$t.min
station$t.min1
station$t.rangeA <- station$t.max - station$t.mean
station$t.rangeB <- station$t.mean - station$t.min
station$t.rangeA1 <- station$t.max1 - station$t.mean1
station$t.rangeB1 <- station$t.mean1 - station$t.min1
station$td.range1 <- f.td.range * (Elev1 - station$Elev) + station$td.range
t.vert(station$td.range)
t.vert(station$td.range1)
station$td.rangeA1 <- ((f.th.mean * (Elev1 - station$Elev) + f.th.mean2 * (e.trans(Elev1) - e.trans(station$Elev))  + station$th.mean) - 
                         (f.t.mean * (Elev1 - station$Elev) + f.t.mean2 * (e.trans(Elev1) - e.trans(station$Elev))  + station$t.mean ) )*2
station$td.rangeA1
station$p.sum1 <- f.p.sum * (Elev1 - station$Elev) + f.p.sum * (e.trans(Elev1) - e.trans(station$Elev)) + station$p.sum 
p.vert(station$p.sum)
p.vert(station$p.sum1)
station$p.ratio1 <- f.p.ratio * (Elev1 - station$Elev) + station$p.ratio 
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
   Elev=Elev1
   p <- (1-((1-station[,p.colrange[i]]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))))*station$p.max*pfactor[1]
   # t <- (station[,t.colrange[i]]-station$t.mean)/t.vert(station$tm.range)*t.vert(station$tm.range1)+station$t.mean + (station$t.mean1 - station$t.mean)[1]
      t <- ifelse(station[,t.colrange[i]]> station$t.mean, 
               (station[,t.colrange[i]]-station$t.mean)/station$t.rangeA*station$t.rangeA1+station$t.mean + (station$t.mean1 - station$t.mean),(station[,t.colrange[i]]-station$t.mean)/station$t.rangeB*station$t.rangeB1+station$t.mean + (station$t.mean1 - station$t.mean))[1]
   
   # th <- t + (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*t.vert(station$td.range1)/2
   # tl <- t - (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*t.vert(station$td.range1)/2   
   
   th <- t + (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
   tl <- t - (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
  p.o <- station[,p.colrange[i]]
   t.o <- station[,t.colrange[i]]
   th.o <- station[,th.colrange[i]]
   tl.o <- station[,tl.colrange[i]]
   
   
   clim.tab0 <- data.frame(cbind(Mon,Lat,Lon,Elev,p.o,p,t.o,t,th.o,tl.o,th,tl))
if(is.null(clim.tab2)){clim.tab2 <- clim.tab0}else{clim.tab2 <- rbind(clim.tab2,clim.tab0)}
 }
 rownames(clim.tab2)<- clim.tab2$Mon;clim.tab0<- NULL
 

# midslope=100
# limit=10000
# bottom = 0.00001
# ex =  -log(0.5)/(log(limit) - log(midslope))
# p=2000
# ptrans0 <- (p+bottom)^ex
# ptransmax <- (limit+bottom*2)^ex
# ptrans <- log10(ptrans0/ptransmax/(1-ptrans0/ptransmax))
# ptrans

# ptrans <-  function(p){
#   sc = 10^6
#   tf = max(0,p)+0.0001 - sc/(max(0,p)+0.0001)
#   return(tf)
# }
# 
# prevers <- function(tf){
#   sc = 10^6
#   p1 = max(0,(tf + (tf^2 - -4*sc)^0.5)/2-0.0001)
#   return(p1)}


# tf - p + sc/p = 0 
# tf*p - p^2 + sc = 0
# tf*p - p^2 + sc = 0
# p^2 - tf*p - sc = 0
