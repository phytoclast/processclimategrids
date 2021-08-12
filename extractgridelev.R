library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
for (i in 1:12){
  clim.tab.fill$x <- (clim.tab.fill[,paste0('th',month[i])] + clim.tab.fill[,paste0('tl',month[i])]) /2
  colnames(clim.tab.fill)[colnames(clim.tab.fill) == 'x'] <- paste0("t", month[i])
}
write.csv(clim.tab.fill, 'output/clim.tab.fill.csv', na='', row.names = F)

#Load again ====
library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
clim.tab.fill$tm.range <- clim.tab.fill$t.max - clim.tab.fill$t.min
clim.tab.fill$td.range <- clim.tab.fill$th.mean - clim.tab.fill$tl.mean
colrange = grep("^p01$", colnames(clim.tab.fill)):grep("^p12$", colnames(clim.tab.fill))
clim.tab.fill$p.sum <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='sum')
colrange = grep("^p01$", colnames(clim.tab.fill)):grep("^p12$", colnames(clim.tab.fill))
clim.tab.fill$p.max <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='max')
clim.tab.fill$p.min <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='min')
clim.tab.fill$p.ratio <- clim.tab.fill$p.min/(clim.tab.fill$p.max+0.000001)


clim.tab <- subset(clim.tab.fill, !is.na(p.sum), select=c("Station_ID","Station_Name","State","Lat","Lon","Elev",
                                                          "t.mean","tm.range","td.range","p.sum","p.ratio"))
station <- subset(clim.tab.fill, grepl('RAINIER PARADISE',Station_Name) & !is.na(p.sum), select=c("Station_ID","Station_Name","State","Lat","Lon","Elev",
                                                      "t.mean","tm.range","td.range","p.sum","p.ratio"))
sLat =   station$Lat[1]  
sLon =   station$Lon[1]  
shape=2
localzone = 50
cutoff = 500
clim.tab$wt <- (localzone/((((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5+localzone))^shape*100
clim.tab[clim.tab$wt < (localzone/(cutoff+localzone))^shape*100,]$wt <- 0
model.1 <- lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.t.mean = model.1$coefficients[2]
model.2 <- lm(tm.range ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.t.mean = model.2$coefficients[2]
model.3 <- lm(td.range ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.t.mean = model.3$coefficients[2]
model.4 <- lm(p.sum ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.t.mean = model.4$coefficients[2]
model.5 <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
f.t.mean = model.5$coefficients[2]
summary(model)

max(clim.tab$p.sum)
p.rast <- rast('output/p.tif')


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

ptrans <-  function(p){
  tf = log2(pmax(0,p+0.0001)+100)
  return(tf)
}

prevers <- function(tf){
  p1 = pmax(0,(2^(tf)-100)-0.0001)
  return(p1)}
prevers(ptrans(1000))

rtrans <-  function(r){
  tf = log2((r+0.0001)/(1 - (r+0.0001)))
  return(tf)
}
rvert <-  function(tf){
  r1 = 2^(tf/(tf+1))+0.0001
  return(r1)
}



clim.tab$transformed <- log((clim.tab$p.ratio+0.0001)/(1 - (clim.tab$p.ratio+0.0001)))
model.4 <- lm(transformed ~ Elev + Lat+ Lon, data = clim.tab, weights = clim.tab$wt)
summary(model.4)

clim.tab$transformed <- log2(clim.tab$p.sum+10)
hist(clim.tab[clim.tab$wt >0,]$p.ratio, nclass=50)
hist(clim.tab[clim.tab$wt >0,]$transformed, nclass=50)

prevers(ptrans(10000))

# tf - p + sc/p = 0 
# tf*p - p^2 + sc = 0
# tf*p - p^2 + sc = 0
# p^2 - tf*p - sc = 0
