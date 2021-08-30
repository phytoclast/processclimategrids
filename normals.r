setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ----
# 
ftoc <- function(f){
  c = round((as.numeric(as.character(f))/10-32)/1.8,2)
  return(c)  }

inchtomm <- function(i){
  m = round((as.numeric(as.character(i))/100)*25.4,1)
  return(m)  }

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
clim.tab$P1990 <- ifelse(clim.tab$Period %in% '1990',1,0)
clim.tab$G1990 <- ifelse(clim.tab$Period %in% 'Grid1990',1,0)
clim.tab$G2080 <- ifelse(clim.tab$Period %in% 'Grid2080',1,0)
clim.tab$P2010 <- ifelse(clim.tab$Period %in% '2010',1,0)
station <- subset(common.tab, NAME %in% 'MOUNT WASHINGTON')

sLat =   station$Lat[1]  
sLon =   station$Lon[1]  
sElev =   station$Elev[1]  
shape=2
localzone = 20
cutoff = 25
clim.tab$altdifwt <- (clim.tab$Elev - sElev)^2/((clim.tab$Elev - sElev)^2 + 500^2)
clim.tab$dist <- (((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5
clim.tab$dist <- (((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5
clim.tab$wt <- (localzone/(clim.tab$dist+localzone))^shape*100
clim.tab$cutoff <- cutoff + cutoff*clim.tab$altdifwt/200
clim.tab$wt <- ifelse(clim.tab$dist > clim.tab$cutoff, 0, clim.tab$wt)
colrange = grep("^th01$", colnames(clim.tab)):grep("^th12$", colnames(clim.tab))
clim.tab$th.mean <- apply(clim.tab[,colrange], MARGIN = 1, FUN='mean')
colrange = grep("^tl01$", colnames(clim.tab)):grep("^tl12$", colnames(clim.tab))
clim.tab$tl.mean <- apply(clim.tab[,colrange], MARGIN = 1, FUN='mean')
clim.tab$t.mean <- (clim.tab$th.mean+clim.tab$tl.mean)/2
clim.tab.s <- subset(clim.tab, Period %in% c('1990', 'Grid1990', '2010'))
summary(lm(t.mean ~ Elev + Lat+ Lon + G1990 + P2010, data = clim.tab.s, weights = clim.tab.s$wt))