setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ----
# 
ftoc <- function(f){
  c = round((as.numeric(as.character(f))/10-32)/1.8,2)
  return(c)  }

inchtomm <- function(i){
  m = round((as.numeric(as.character(i))/100)*25.4,1)
  return(m)  }
p.trans <-  function(p){
  p1 = log2(pmax(0,p+0.0001)+100)
  return(p1)}

p.vert <- function(p1){
  p = pmax(0,(2^(p1)-100)-0.0001)
  return(p)}


#1990
pnorm <- read.table('globalnorm/MEANPRCP.NML', colClasses = "character")
colnames(pnorm) <- c('preID', 'p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12','p0112')
pnorm[,2:14]  <- lapply(pnorm[,2:14], FUN = 'inchtomm')
pnorm$ID <-  substring(pnorm$preID, 1,6)
pnorm$preID <- NULL#substring(pnorm$preID, 7,9)

tnorm <- read.table('globalnorm/MEANTEMP.NML', colClasses = "character")
colnames(tnorm) <- c('preID', 't01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12','t0112')
tnorm[,2:14]  <- lapply(tnorm[,2:14], FUN = 'ftoc')
tnorm$ID <-  substring(tnorm$preID, 1,6)
tnorm$preID <- NULL#substring(tnorm$preID, 7,9)

thnorm <- read.table('globalnorm/MAXTEMP.NML', colClasses = "character")
colnames(thnorm) <- c('preID', 'th01','th02','th03','th04','th05','th06','th07','th08','th09','th10','th11','th12','th0112')
thnorm[,2:14]  <- lapply(thnorm[,2:14], FUN = 'ftoc')
thnorm$ID <-  substring(thnorm$preID, 1,6)
thnorm$preID <- NULL#substring(tnorm$preID, 7,9)
tlnorm <- read.table('globalnorm/MINTEMP.NML', colClasses = "character")
colnames(tlnorm) <- c('preID', 'tl01','tl02','tl03','tl04','tl05','tl06','tl07','tl08','tl09','tl10','tl11','tl12','tl0112')
tlnorm[,2:14]  <- lapply(tlnorm[,2:14], FUN = 'ftoc')
tlnorm$ID <-  substring(tlnorm$preID, 1,6)
tlnorm$preID <- NULL#substring(tnorm$preID, 7,9)


stn <- read.delim('globalnorm/STATION.LST.txt', header = F)
stn$ID <- substring(stn$V1, 1,6)
stn$NAME <- trimws(substring(stn$V1, 9,32))
stn$LATD <- substring(stn$V1, 33,34)
stn$LATM <- substring(stn$V1, 35,36)
stn$LATA <- substring(stn$V1, 37,37)
stn$LOND <- substring(stn$V1, 41,43)
stn$LONM <- substring(stn$V1, 44,45)
stn$LONA <- substring(stn$V1, 46,46)
stn$ELEVFT <- as.numeric(substring(stn$V1, 50,54))
stn$Elev <- round(stn$ELEVFT*.3048,1)
stn$Lat <- (as.numeric(stn$LATD)+as.numeric(stn$LATM)/60) * ifelse(stn$LATA %in% 'N',1,-1)
stn$Lon <- (as.numeric(stn$LOND)+as.numeric(stn$LONM)/60) * ifelse(stn$LONA %in% 'E',1,-1)

stn <- stn[,c('ID', 'NAME', 'Lat', 'Lon', 'Elev')]

stn.t <- merge(stn, tnorm, by='ID')
stn.th <- merge(stn, thnorm, by='ID')
stn.tl <- merge(stn, tlnorm, by='ID')
stn.p <- merge(stn, pnorm, by='ID')
stn.clim <- merge(stn.th, tlnorm, by='ID')
stn.clim <- merge(stn.clim, pnorm, by='ID')
stn.clim$Period <- '1990'
stn.clim$th0112 <- NULL;stn.clim$tl0112 <- NULL;stn.clim$p0112 <- NULL;
tab.1990 <- stn.clim[, c('ID','NAME','Lat','Lon','Elev','Period',
                         'th01','th02','th03','th04','th05','th06','th07','th08','th09','th10','th11','th12',
                         'tl01','tl02','tl03','tl04','tl05','tl06','tl07','tl08','tl09','tl10','tl11','tl12',
                         'p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12')]

#2010
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
Norms2010 <- readRDS(file='data/Norms2010.RDS')
cols.t <- colnames(Norms2010[,grep("^t01$", colnames(Norms2010)):grep("^t12$", colnames(Norms2010))])
cols.tl <- colnames(Norms2010[,grep("^tl01$", colnames(Norms2010)):grep("^tl12$", colnames(Norms2010))])
cols.p <- colnames(Norms2010[,grep("^pp01$", colnames(Norms2010)):grep("^pp12$", colnames(Norms2010))])
Norms2010.agg <- aggregate(Norms2010[,c(cols.t,cols.tl,cols.p)], by=list(ID=Norms2010$Station_ID,
                                                                         NAME=Norms2010$Station_Name,
                                                                         Lat=Norms2010$Latitude,
                                                                         Lon=Norms2010$Longitude,
                                                                         Elev=Norms2010$Elevation
                                                                         ), FUN='mean')
cols.t <- colnames(Norms2010.agg[,grep("^t01$", colnames(Norms2010.agg)):grep("^t12$", colnames(Norms2010.agg))])
cols.tl <- colnames(Norms2010.agg[,grep("^tl01$", colnames(Norms2010.agg)):grep("^tl12$", colnames(Norms2010.agg))])
if(is.null(Norms2010.agg$th01)){for (i in 1:12){
  Norms2010.agg$x <- (Norms2010.agg[,paste0('t',month[i])]*2 - Norms2010.agg[,paste0('tl',month[i])])
  colnames(Norms2010.agg)[colnames(Norms2010.agg) == 'x'] <- paste0("th", month[i])
}}
for (i in 1:12){
  colnames(Norms2010.agg)[colnames(Norms2010.agg) == paste0("pp", month[i])] <- paste0("p", month[i])
}
Norms2010.agg$Period <- '2010'

tab.2010 <- Norms2010.agg[, c('ID','NAME','Lat','Lon','Elev','Period',
                         'th01','th02','th03','th04','th05','th06','th07','th08','th09','th10','th11','th12',
                         'tl01','tl02','tl03','tl04','tl05','tl06','tl07','tl08','tl09','tl10','tl11','tl12',
                         'p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12')]


#Gridded
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
pre.tab <- readRDS('data/pre.tab.RDS')
pre.tab$NAME <- paste(pre.tab$Station_Name, pre.tab$State)
pre.tab$ID <- pre.tab$E.cell
pre.tab$Period <- 'Grid1990'
tab.g1990 <- pre.tab[, c('ID','NAME','Lat','Lon','Elev','Period',
                                   'th01','th02','th03','th04','th05','th06','th07','th08','th09','th10','th11','th12',
                                   'tl01','tl02','tl03','tl04','tl05','tl06','tl07','tl08','tl09','tl10','tl11','tl12',
                                   'p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12')]
pre.tab$Period <- 'Grid2080'
tab.g2080 <- pre.tab[, c('ID','NAME','Lat','Lon','Elev','Period',
                         'th.2080.01','th.2080.02','th.2080.03','th.2080.04','th.2080.05','th.2080.06',
                         'th.2080.07','th.2080.08','th.2080.09','th.2080.10','th.2080.11','th.2080.12',
                         'tl.2080.01','tl.2080.02','tl.2080.03','tl.2080.04','tl.2080.05','tl.2080.06',
                         'tl.2080.07','tl.2080.08','tl.2080.09','tl.2080.10','tl.2080.11','tl.2080.12',
                         'p.2080.01','p.2080.02','p.2080.03','p.2080.04','p.2080.05','p.2080.06',
                         'p.2080.07','p.2080.08','p.2080.09','p10','p.2080.11','p.2080.12')]
colnames(tab.g2080) <- c('ID','NAME','Lat','Lon','Elev','Period',
                         'th01','th02','th03','th04','th05','th06','th07','th08','th09','th10','th11','th12',
                         'tl01','tl02','tl03','tl04','tl05','tl06','tl07','tl08','tl09','tl10','tl11','tl12',
                         'p01','p02','p03','p04','p05','p06','p07','p08','p09','p10','p11','p12')

common.tab <- rbind(tab.1990, tab.2010, tab.g1990, tab.g2080)

clim.tab <- common.tab
col.p = grep("^p01$", colnames(clim.tab)):grep("^p12$", colnames(clim.tab))
clim.tab[,col.p] <- p.trans(clim.tab[,col.p])#transform precipitation

clim.tab$P1990 <- ifelse(clim.tab$Period %in% '1990',1,0)
clim.tab$G1990 <- ifelse(clim.tab$Period %in% 'Grid1990',1,0)
clim.tab$G2080 <- ifelse(clim.tab$Period %in% 'Grid2080',1,0)
clim.tab$P2010 <- ifelse(clim.tab$Period %in% '2010',1,0)
station <- subset(common.tab, NAME %in% 'MT WASHINGTON')
stationlist <- unique(subset(clim.tab, Period %in% c('2010', 'Grid1990')))

#set priority for stations paired with different normal periods----
list.1990 <- unique(subset(clim.tab, Period %in% '1990', select = c(Lat, Lon)))
list.2010 <- unique(subset(clim.tab, Period %in% '2010', select = c(Lat, Lon)))
list.gridded <- unique(subset(clim.tab, Period %in% c('Grid1990', 'Grid2080'), select = c(Lat, Lon)))

clim.tab$adj <- NA

for (i in 1:nrow(clim.tab)){#i=1
  sLat = clim.tab$Lat[i]
  sLon = clim.tab$Lon[i]
  if(clim.tab[i,]$Period %in% '1990'){
    clim.tab[i,]$adj <- min((((list.2010$Lat - sLat)*10000/90)^2 + ((list.2010$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5)
}else{
  if(clim.tab$Period[i] %in% '2010'){
    clim.tab[i,]$adj <- min((((list.1990$Lat - sLat)*10000/90)^2 + ((list.1990$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5)
}else{
  clim.tab[i,]$adj <- mean(min((((list.2010$Lat - sLat)*10000/90)^2 + ((list.2010$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5), min((((list.1990$Lat - sLat)*10000/90)^2 + ((list.1990$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5))
}}}




for(k in 1:nrow(stationlist)){#k=1
station <- stationlist[k,]
sLat =   station$Lat[1]  
sLon =   station$Lon[1]  
sElev =   station$Elev[1]  
shape=2
localzone = 10
cutoff = 500
clim.tab$altdifwt <- (clim.tab$Elev - sElev)^2/((clim.tab$Elev - sElev)^2 + 500^2)
clim.tab$dist <- (((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5+(clim.tab$adj)
clim.tab$cutoff <- cutoff + cutoff*clim.tab$altdifwt/200

colrange = grep("^th01$", colnames(clim.tab)):grep("^th12$", colnames(clim.tab))
clim.tab$th.mean <- apply(clim.tab[,colrange], MARGIN = 1, FUN='mean')
colrange = grep("^tl01$", colnames(clim.tab)):grep("^tl12$", colnames(clim.tab))
clim.tab$tl.mean <- apply(clim.tab[,colrange], MARGIN = 1, FUN='mean')
clim.tab$t.mean <- (clim.tab$th.mean+clim.tab$tl.mean)/2
clim.tab.s <- subset(clim.tab, Period %in% c('1990', 'Grid1990', '2010','Grid2080'))
clim.tab.s$rank <- rank(clim.tab.s$dist)
clim.tab.s$srank <- NA
clim.tab.s[clim.tab.s$Period %in% '1990',]$srank <- rank(clim.tab.s[clim.tab.s$Period %in% '1990',]$dist)
clim.tab.s[clim.tab.s$Period %in% '2010',]$srank <- rank(clim.tab.s[clim.tab.s$Period %in% '2010',]$dist)
clim.tab.s[clim.tab.s$Period %in% 'Grid1990',]$srank <- rank(clim.tab.s[clim.tab.s$Period %in% 'Grid1990',]$dist)
clim.tab.s[clim.tab.s$Period %in% 'Grid2080',]$srank <- rank(clim.tab.s[clim.tab.s$Period %in% 'Grid2080',]$dist)
clim.tab.s$wt <- (localzone/(clim.tab.s$dist+localzone))^shape*100
clim.tab.s <- subset(clim.tab.s, rank <= 16 | srank <= 3)

col.th = grep("^th01$", colnames(clim.tab.s)):grep("^th12$", colnames(clim.tab.s))
col.tl = grep("^tl01$", colnames(clim.tab.s)):grep("^tl12$", colnames(clim.tab.s))
col.p = grep("^p01$", colnames(clim.tab.s)):grep("^p12$", colnames(clim.tab.s))
col.th.s = grep("^th01$", colnames(station)):grep("^th12$", colnames(station))
col.tl.s = grep("^tl01$", colnames(station)):grep("^tl12$", colnames(station))
col.p.s = grep("^p01$", colnames(station)):grep("^p12$", colnames(station))

for(i in 1:12){#i=1
model.th <-lm(clim.tab.s[,col.th[i]] ~ Elev + Lat+ Lon + G1990 + G2080 + P2010, data = clim.tab.s, weights = clim.tab.s$wt)
model.tl <-lm(clim.tab.s[,col.tl[i]] ~ Elev + Lat+ Lon + G1990 + G2080 + P2010, data = clim.tab.s, weights = clim.tab.s$wt)
model.p <-lm(clim.tab.s[,col.p[i]] ~ Elev + Lat+ Lon + G1990 + G2080 + P2010, data = clim.tab.s, weights = clim.tab.s$wt)

G1990.th = model.th$coefficients[5]
G2080.th = model.th$coefficients[6]
P2010.th = model.th$coefficients[7]
G1990.tl = model.tl$coefficients[5]
G2080.tl = model.tl$coefficients[6]
P2010.tl = model.tl$coefficients[7]
G1990.p = model.p$coefficients[5]
G2080.p = model.p$coefficients[6]
P2010.p = model.p$coefficients[7]
#New Normals
th = station[,col.th.s[i]]
th.1990 = station$G1990*-G1990.th + 
  station$P2010*-P2010.th +th
th.2010 = station$G1990*-G1990.th + 
  station$P2010*-P2010.th +th+P2010.th
th.2080 = station$G1990*-G1990.th + 
  station$P2010*-P2010.th +th+G2080.th-G1990.th

tl = station[,col.tl.s[i]]
tl.1990 = station$G1990*-G1990.tl + 
  station$P2010*-P2010.tl +tl
tl.2010 = station$G1990*-G1990.tl + 
  station$P2010*-P2010.tl +tl+P2010.tl
tl.2080 = station$G1990*-G1990.tl + 
  station$P2010*-P2010.tl +tl+G2080.tl-G1990.tl

p = station[,col.p.s[i]]
p.1990 = p.vert(station$G1990*-G1990.p + 
  station$P2010*-P2010.p +p)
p.2010 = p.vert(station$G1990*-G1990.p + 
  station$P2010*-P2010.p +p+P2010.p)
p.2080 = p.vert(station$G1990*-G1990.p + 
  station$P2010*-P2010.p +p+G2080.p-G1990.p)

row.th0 <- data.frame(rbind(th.1990, th.2010, th.2080))
colnames(row.th0) <- paste0('th', month[i])
if(i==1){
  row.th <- cbind(list(Period = c('1990','2010','2080')),row.th0)}else{
    row.th <- cbind(row.th,row.th0)
  }
row.tl0 <- data.frame(rbind(tl.1990, tl.2010, tl.2080))
colnames(row.tl0) <- paste0('tl', month[i])
if(i==1){
  row.tl <- row.tl0}else{
    row.tl <- cbind(row.tl,row.tl0)
  }
row.p0 <- data.frame(rbind(p.1990, p.2010, p.2080))
colnames(row.p0) <- paste0('p', month[i])
if(i==1){
  row.p <- row.p0}else{
    row.p <- cbind(row.p,row.p0)
  }
}
rows <- cbind(row.th, row.tl, row.p)
rows$NAME <- station$NAME; rows$Lat <-  station$Lat; rows$Lon <- station$Lon; rows$Elev <- station$Elev
rows <- rows[,c("NAME", "Lat",  "Lon",  "Elev", "Period", "th01", "th02", "th03", "th04", "th05", "th06", "th07", "th08", "th09", "th10", "th11", "th12", 
                    "tl01", "tl02","tl03", "tl04", "tl05", "tl06", "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                    "p01",  "p02",  "p03",  "p04",  "p05", "p06",  "p07",  "p08",  "p09",  "p10",  "p11",  "p12")]
if(k==1){
  harmonized <- rows }else{
    harmonized <- rbind(harmonized,rows)
    }
}
saveRDS(harmonized, 'output/harmonized.RDS')