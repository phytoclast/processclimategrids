library(terra)
library(sf)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

wtrshd <- read_sf("C:/a/geo/NA_Watersheds")
rivers <- subset(wtrshd, NAW4_EN %in% 'English')

e <- rast(paste0('output/e.tif'))
p <- rast(paste0('output/p.tif'))
d <- rast(paste0('output/deficit.tif'))
s <- rast(paste0('output/surplus.tif'))

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
