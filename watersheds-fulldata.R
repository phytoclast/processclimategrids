library(terra)
library(sf)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

wtrshd <- read_sf("C:/a/geo/NA_Watersheds")
rivers <- subset(wtrshd, NAW4_EN %in% 'English')

e <- rast(paste0('output/e.tif'))
p <- rast(paste0('output/p.tif'))
d <- rast(paste0('output/deficit.tif'))
s <- rast(paste0('output/surplus.tif'))
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

st_crs(wtrshd)
naproj = as.character('PROJCS["NA Lambert Azimuthal Equal Area",
                GEOGCS["GCS_WGS_1984",
                       DATUM["D_WGS_1984",
                             SPHEROID["WGS_1984",6378137.0,298.257223563]],
                       PRIMEM["Greenwich",0.0],
                       UNIT["degree",0.0174532925199433]],
                PROJECTION["Lambert_Azimuthal_Equal_Area"],
                PARAMETER["false_easting",0.0],
                PARAMETER["false_northing",0.0],
                PARAMETER["longitude_of_center",-100.0],
                PARAMETER["latitude_of_center",45.0],
                UNIT["meter",1.0]]')
wtrshd.repr <-  sf::st_transform(wtrshd, crs=st_crs(e))

Ohio <- wtrshd[wtrshd$NAW3_EN %in% 'Ohio River',]$NAW4_EN;# 490600;8000
Mackenzie <- wtrshd[wtrshd$NAW2_EN %in% 'Mackenzie River',]$NAW4_EN ;# 	1783912; 10338 
Yukon <- wtrshd[wtrshd$NAW2_EN %in% 'Yukon River',]$NAW4_EN;#	833232; 6576 
rivername <- c('Ohio','Mackenzie','Yukon',
                  'Saginaw', 
                  'Fox', 
                  'Cheyenne', 
                  'Pecos',
                  'Little Colorado', 
                  'Klamath', 
                  'Willamette', 
                  'Salmon', 
                  'Humboldt', 
                  'Big Horn', 
                  'Des Moines', 
                  'Iowa', 
                  'Minnesota', 
                  'Kentucky', 
                  'Osage', 
                  'Kaskaskia', 
                  'Savannah', 
                  'Muskingum', 
                  'Great Miami', 
                  'Allegheny', 
                  'Roanoke', 
                  'Kanawha', 
                  'Fond-du-Lac', 
                  'Saskatchewan', 
                  'Coppermine', 
                  'La Grande Rivière', 
                  'Cumberland',
                  'Apalachicola', 
                  'Penobscot', 
                  'Connecticut', 
                  'Wisconsin', 
                  'Wabash',
                  'Colville River',
                  'Fort Nelson')

riverlist <- list(Ohio,Mackenzie,Yukon,
                  'Saginaw', 
                  'Fox', 
                  'Cheyenne', 
                  c('Lower Pecos','Upper Pecos'), 
                  'Little Colorado', 
                  'Klamath', 
                  'Willamette', 
                  'Salmon', 
                  'Humboldt', 
                  'Big Horn', 
                  'Des Moines', 
                  'Iowa', 
                  'Minnesota', 
                  'Kentucky', 
                  'Osage', 
                  'Kaskaskia', 
                  'Savannah', 
                  'Muskingum', 
                  'Great Miami', 
                  'Allegheny', 
                  'Roanoke', 
                  'Kanawha', 
                  'Fond-du-Lac', 
                  'Saskatchewan', 
                  'Coppermine', 
                  'La Grande Rivière', 
                  c('Lower Cumberland','Upper Cumberland') , 
                  'Apalachicola', 
                  'Penobscot', 
                  c('Lower Connecticut','Upper Connecticut') , 
                  'Wisconsin', 
                  'Wabash',
                  'Colville River',
                  'Fort Nelson')


riverarea <- c(490600,1783912,833232,
               22260, 
                  16650, 
                  62800, 
                  115000, 
                  69000, 
                  40630, 
                  29730, 
                  36000, 
                  43200, 
                  59270, 
                  38340,
                  32711865446/1000^2, 
                  44000, 
                  18140, 
                  40000, 
                  14880, 
                  25500, 
                  20850, 
                  13920, 
                  30000, 
                  25100, 
                  31690 , 
                  66800, 
                  335900, 
                  50700, 
                  97600, 
                  45920, 
                  50505, 
                  22300, 
                  29200, 
                  31800, 
                  86000,
                  53000,
                  55900)
riverdischarge <- c(8000,10338,6576,
                    136.7, 
                       117.0, 
                       24.7, 
                       7.5, 
                       11.5, 
                       475, 
                       935, 
                       310, 
                       11, 
                       112.0, 
                       13223*0.3048^3, 
                       14109*0.3048^3, 
                       236.6, 
                       252.7, 
                       308.1, 
                       420.0, 
                       332, 
                       267.863, 
                       152.0, 
                       559, 
                       220.9, 
                       432, 
                       300, 
                       634, 
                       337.69, 
                       3400, 
                       1055, 
                       555.1, 
                       342, 
                       520, 
                       340, 
                       1001,
                       288.6,
                       331.9)

for(i in 1:length(riverlist)){#i=19

selectedriver = riverlist[[i]]
rivers0 <- subset(wtrshd.repr, NAW4_EN %in% riverlist[[i]])
newext0 <- c(st_bbox(rivers0)[1],st_bbox(rivers0)[3],st_bbox(rivers0)[2],st_bbox(rivers0)[4]) 
lat <- mean(newext0[3:4])
lon <- mean(newext0[1:2])
rast.p0 <- crop(p, newext0)
rast.e0 <- crop(e, newext0)
rast.d0 <- crop(d, newext0)
rast.s0 <- crop(s, newext0)
rivers1 <- subset(wtrshd, NAW4_EN %in% riverlist[[i]])
rivers1 <- st_transform(rivers1, crs=st_crs(naproj))
newext1 <- c(st_bbox(rivers1)[1],st_bbox(rivers1)[3],st_bbox(rivers1)[2],st_bbox(rivers1)[4]) 
fakerast <- rast(xmin=st_bbox(rivers1)[1], xmax=st_bbox(rivers1)[3], ymin=st_bbox(rivers1)[2], ymax=st_bbox(rivers1)[4],
resolution = c(4000,4000)) ; crs(fakerast) <- ((naproj))
rast.p1 <- terra::project(rast.p0,fakerast)
rast.e1 <- terra::project(rast.e0,fakerast)
rast.d1 <- terra::project(rast.d0,fakerast)
rast.s1 <- terra::project(rast.s0,fakerast)
ext(fakerast)[3]
rivers2 <- vect(rivers1)
river.cnt <- sum(extract(rast.p1, rivers2, fun='length')[,2])
river.sum <- sum(extract(rast.p1, rivers2, fun='sum',na.rm=TRUE)[,2])
ppt <- river.sum/river.cnt
river.cnt <- sum(extract(rast.e1, rivers2, fun='length')[,2])
river.sum <- sum(extract(rast.e1, rivers2, fun='sum',na.rm=TRUE)[,2])
pet <- river.sum/river.cnt
river.cnt <- sum(extract(rast.d1, rivers2, fun='length')[,2])
river.sum <- sum(extract(rast.d1, rivers2, fun='sum',na.rm=TRUE)[,2])
deficit <- river.sum/river.cnt
river.cnt <- sum(extract(rast.s1, rivers2, fun='length')[,2])
river.sum <- sum(extract(rast.s1, rivers2, fun='sum',na.rm=TRUE)[,2])
surplus <- river.sum/river.cnt

riv0 <- data.frame(lat= lat, lon= lon, river = rivername[i], ppt,pet,deficit,surplus, discharge = riverdischarge[i], basinarea = riverarea[i])
if(i == 1){riv <- riv0}else{riv <- rbind(riv,riv0)}
}

riv$runoff <- riv$discharge*3600*24*365/(riv$basinarea*1000^2)*1000
riv$aet <- riv$ppt-riv$runoff
riv$cropcoef <- riv$aet/riv$pet
riv$MI.aet <- riv$ppt/riv$aet
riv$MI.pet <- riv$ppt/riv$pet
write.csv(riv,'output/riv.csv')

riv <- read.csv('output/riv.csv')



riv<-subset(riv, MI.aet > 0 & !river %in% 'Kaskaskia')
plot(cropcoef~ppt, data=riv)
colnames(riv[,c(2:3,5:15)])
cortab <- as.data.frame(cor(riv[,c(2:3,5:15)]))

model<- lm(runoff~0+ lat+ppt+deficit+surplus, riv)
summary(model)

plot(rast.p0)
plot(st_geometry(rivers0), add=T)

plot(rast.e1)
plot(st_geometry(rivers1), add=T)



##########BigBrick ----


newTclx <- rast('C:/workspace2/bonapmexico/nam5k/Tclx.tif')
wtrshd.repr<- sf::st_transform(wtrshd, crs=st_crs(newTclx))

lat <- aggregate(t01, fact = 4)
lat.df <- as.data.frame(lat, xy=TRUE)
lat <- rast(cbind(x=lat.df$x, y=lat.df$y, z=lat.df$y), type="xyz", crs=crs(lat))
lon <- rast(cbind(x=lat.df$x, y=lat.df$y, z=lat.df$x), type="xyz", crs=crs(lat))
names(lat)<- 'lat'
names(lon)<- 'lon'

newTclx <- aggregate(newTclx, fact = 4)
Elev <- rast('output/ElevAmp.tif' )
names(Elev) <- 'Elev'
for (i in 1:12){#i=2
assign(paste0('th',month[i],'n'), project(get(paste0('th',month[i])), newTclx, method='bilinear'))
assign(paste0('tl',month[i],'n'), project(get(paste0('tl',month[i])), newTclx, method='bilinear'))
assign(paste0('p',month[i],'n'), project(get(paste0('p',month[i])), newTclx, method='bilinear'))
}
lat <- project(lat, newTclx, method='bilinear')
lon <- project(lon, newTclx, method='bilinear')

Elev <- project(Elev, newTclx, method='bilinear')
wtrshd.rast <- rasterize(vect(wtrshd.repr), Elev, field = 'OBJECTID')
names(wtrshd.rast) <- 'basin'
brick <- c(wtrshd.rast,lat,lon, Elev, 
           th01n,
           th02n,
           th03n,
           th04n,
           th05n,
           th06n,
           th07n,
           th08n,
           th09n,
           th10n,
           th11n,
           th12n,
           tl01n,
           tl02n,
           tl03n,
           tl04n,
           tl05n,
           tl06n,
           tl07n,
           tl08n,
           tl09n,
           tl10n,
           tl11n,
           tl12n,
           p01n,
           p02n,
           p03n,
           p04n,
           p05n,
           p06n,
           p07n,
           p08n,
           p09n,
           p10n,
           p11n,
           p12n
           )
bigbrick <- as.data.frame(brick, xy=T)
saveRDS(bigbrick,'output/bigbrick.RDS')






#############reload ----
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
bigbrick <- readRDS('output/bigbrick.RDS')
th. = grep("^th01$", colnames(bigbrick)):grep("^th12$", colnames(bigbrick))
tl. = grep("^tl01$", colnames(bigbrick)):grep("^tl12$", colnames(bigbrick))
p. = grep("^p01$", colnames(bigbrick)):grep("^p12$", colnames(bigbrick))
maketmean <- function(th,tl) {
  (th+tl)/2
}
for(i in 1:12){#i=1
bigbrick$t <- (bigbrick[,th.[i]]+bigbrick[,tl.[i]])/2; names(bigbrick)[names(bigbrick) == 't'] <- paste0('t',month[i])
}
t. = grep("^t01$", colnames(bigbrick)):grep("^t12$", colnames(bigbrick))

Days <- c(31.00, 28.25, 31.00, 30.00, 31.00, 30.00, 31.00, 31.00, 30.00, 31.00, 30.00, 31.00)
DayNumber <- c(16.000,45.625,75.250,106.125,136.250,166.750,197.250,228.250,258.750,289.250,319.750,350.250)
dcl <- 0.409*sin(2*3.141592*DayNumber/365-1.39)

GetSolarRad <- function(Month, Lat){
  declination <- dcl[Month]
  
  hs <- acos(pmin(pmax(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
  Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                   cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
  return(Ra)
}

GetDayLength<- function(Month, Lat){
  declination <- dcl[Month]
  
  Dl <- ifelse(Lat + declination*360/2/3.141592 > 89.16924, 24, ifelse(Lat - declination*360/2/3.141592 >= 90, 0, (atan(-((sin(-0.83/360*2*3.141592)-sin(declination)*sin(Lat/360*2*3.141592))/(cos(declination)*cos(Lat/360*2*3.141592)))/(-((sin(-0.83/360*2*3.141592)-sin(declination)*sin(Lat/360*2*3.141592))/(cos(declination)*cos(Lat/360*2*3.141592)))*((sin(-0.83/360*2*3.141592)-sin(declination)*sin(Lat/360*2*3.141592))/(cos(declination)*cos(Lat/360*2*3.141592)))+1)^0.5)+2*atan(1))/3.141592*24))
  return(Dl)}


GetVp  <- function(p,th,tl) {#Based on linear regression using 10 minute WorldClim 2.0 data with vapor pressure estimates
  Vpmax = 0.6108*exp(17.27*th/(th+237.3)) #saturation vapor pressure kPa
  Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
  Vp0 <- (Vpmin*7.976e-01+
            Vpmin*log(p+1)*9.499e-02+
            Vpmin*Vpmax*-6.599e-02)
  Vp <- pmax(0,pmin(Vpmin,Vp0))
  return(Vp)}

GetSolar <- function(Ra, Elev, th, tl, p) {#Based on linear regression using 10 minute WorldClim 2.0 data with solar radiation estimates
  Rso <- (0.75+2*10^-5*Elev)*Ra
  Rs0 <- (Rso*9.521e-01+
            Rso*log(p+1)*-9.087e-02+
            Rso*tl*-3.644e-03+
            Rso*log(p+1)*th*1.335e-03)
  Rs <- pmax(0.3*Rso,pmin(Rso,Rs0))
  return(Rs)}

GetPET <- function(Ra, th, tl, p){
  Vpmax = 0.6108*exp(17.27*th/(th+237.3)) #saturation vapor pressure kPa
  Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
  logp <- log(p+1)
  e0 <- Ra*0.0508780  +
    Vpmax*0.7893714  +
    Vpmin*-0.5589255  +
    logp*-0.1309403  +
    Ra*Vpmax*0.0049383
  e <- pmax(0,e0)
  return(e)}



GetNetSolar <- function(Ra, Elev, th, tl, p){
  Vp = GetVp(p,th,tl)
  Rso <- (0.75+2*10^-5*Elev)*Ra
  Rs <- GetSolar(Ra, Elev, th, tl, p)
  Rnl <- 4.901*10^-9 * (1.35*Rs/(Rso+0.000001)-0.35) * (0.34 - 0.14 * Vp^0.5) * ((th+273.16)^4 + (tl+273.16)^4)/2
  Rns <- (1-0.23)*Rs
  Rn <- pmax(0,Rns - Rnl)
  return(Rn)}

GetTransGrow <- function(th, tl) {#Adjust to reduction in transpiration due to cold, with evaporation only outside growing season
  ts = 0.8 #assumed T/ET ratio during growing season
  tw = 0 #assumed T/ET ratio during freezing season
  t <- (th+tl)/2
  tr <- 10 #generally as mean temperatures get below 10 transpiration shuts down, regardless of warm daytime temperatures
  G0 <- (t-0)/(tr) 
  G1 <- pmin(1,pmax(0,G0)) #generally as mean temperatures get below 5 transpiration shuts down, regardless of warm daytime temperatures
  evmin = (tw)+(1-ts)
  G = G1*(1-evmin)+evmin
  return(G)}

for(i in 1:12){
bigbrick$Ra <-  GetSolarRad(i,bigbrick$lat); names(bigbrick)[names(bigbrick) == 'Ra'] <- paste0('Ra',month[i])

}
Ra. = grep("^Ra01$", colnames(bigbrick)):grep("^Ra12$", colnames(bigbrick))

for(i in 1:12){
  bigbrick$Vp <-  GetVp(bigbrick[,p.[i]],bigbrick[,th.[i]],bigbrick[,tl.[i]]); names(bigbrick)[names(bigbrick) == 'Vp'] <- paste0('Vp',month[i])
}
Vp. = grep("^Vp01$", colnames(bigbrick)):grep("^Vp12$", colnames(bigbrick))
for (i in 1:12){#i=1
  bigbrick$e <- 0.85*GetTransGrow(bigbrick[,th.[i]], bigbrick[,tl.[i]])*
    GetPET(GetSolarRad(i,bigbrick$lat), bigbrick[,th.[i]], bigbrick[,tl.[i]], bigbrick[,p.[i]])*
    Days[i]
  names(bigbrick)[names(bigbrick) == 'e'] <- paste0('e',month[i])
}

e. = grep("^e01$", colnames(bigbrick)):grep("^e12$", colnames(bigbrick))
#holdridge ----
for (i in 1:12){#i=1
  bigbrick[,e.[i]] <- 58.93/365*pmax(0, bigbrick[,t.[i]])*Days[i]
}
#schimidt2 ---- 
for (i in 1:12){#i=1
  bigbrick[,e.[i]] <- 0.85*GetTransGrow(bigbrick[,th.[i]], bigbrick[,tl.[i]])*
    GetPET(GetSolarRad(i,bigbrick$lat), bigbrick[,th.[i]], bigbrick[,tl.[i]], bigbrick[,p.[i]])*
    Days[i]
}
for (i in 1:12){#i=1
  bigbrick[,e.[i]] <- 0.008404*216.7*exp(17.26939*bigbrick[,t.[i]]/
                                     (bigbrick[,t.[i]]+237.3))/(bigbrick[,t.[i]]+273.3)*(bigbrick[,Ra.[i]])*Days[i]*
    abs((bigbrick[,th.[i]] - bigbrick[,tl.[i]]))^0.5 + 0.001#Schmidt.2018
}




bigbrick$p <- apply(bigbrick[,p.[1:12]], MARGIN = 1, FUN = 'sum')
bigbrick$e <- apply(bigbrick[,e.[1:12]], MARGIN = 1, FUN = 'sum')
bigbrick$s <- 0
for(i in 1:12){
  bigbrick$var <- pmax(0,bigbrick[,p.[i]]-bigbrick[,e.[i]])
  bigbrick$s <- bigbrick$s+bigbrick$var
};bigbrick$var = NULL

bigbrick$d <- 0
for(i in 1:12){
  bigbrick$var <- pmax(0,bigbrick[,e.[i]]-bigbrick[,p.[i]])
  bigbrick$d <- bigbrick$d+bigbrick$var
};bigbrick$var = NULL

for(i in 1:length(rivername)){
riverpoints <- wtrshd[wtrshd$NAW4_EN %in% riverlist[[i]],]$OBJECTID
sb <- subset(bigbrick, basin %in% riverpoints)
riv0 <- data.frame(lat= mean(sb$lat), lon= mean(sb$lon), river = rivername[i], p=mean(sb$p) ,e=mean(sb$e),s=mean(sb$s),d=mean(sb$d), discharge = riverdischarge[i], basinarea = riverarea[i])
if(i == 1){riv <- riv0}else{riv <- rbind(riv,riv0)}
}
riv$r <- riv$discharge*3600*24*365/(riv$basinarea*1000^2)*1000
riv$a <- riv$p-riv$r
riv$cropcoef <- riv$a/riv$e
riv$mi <- riv$p/riv$a
riv$di <- riv$d/riv$p

riv<-subset(riv, cropcoef > 0 & !river %in% 'Kaskaskia')
model<- lm(r~0+ s+p+d, riv)
summary(model)


#humidstations <- riv[riv$di < 0.1,]$river
humidstations <- c("Ohio", "Mackenzie", "Yukon", "Saginaw", "Fox", "Willamette", "Des Moines", "Iowa", 
                   "Minnesota", "Kentucky", "Osage", "Savannah", "Muskingum", "Great Miami", "Allegheny", "Roanoke", 
                   "Kanawha", "Fond-du-Lac", "Saskatchewan", "Cumberland", "Apalachicola", "Penobscot", "Connecticut",
                   "Wisconsin", "Wabash", "Fort Nelson")
riv<-subset(riv, river %in% humidstations)

model<- lm(a~0+ e, riv)
summary(model)

plot(r~s, riv)
