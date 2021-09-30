harmonized <-  readRDS('output/harmonized.RDS'); rownames(harmonized) <- NULL
retro <-  subset(harmonized, Period %in% '1990')
xy = as.matrix(as.data.frame(list(x=retro$Lon, y=retro$Lat)))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")

xy.Elev = extract(rast(paste0('wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')), xy);  xy.Elev <- subset(xy.Elev, select= -ID);
colnames(xy.Elev) <- c('xElev')
retro <- cbind(retro, xy.Elev)
retro <- subset(retro, Elev == xElev)
xy = as.matrix(as.data.frame(list(x=retro$Lon, y=retro$Lat)))
xy <- vect(xy, crs="+proj=longlat +datum=WGS84")

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')#i=7
for (i in 1:12){
  t = extract(rast(paste0('output/t',month[i],'.tif')), xy);
  t <- subset(t, select= -ID); colnames(t) <- paste0('xt',month[i])
  retro <- cbind(retro, t)
}

th.colrange = grep("^th01$", colnames(retro)):grep("^th12$", colnames(retro))
tl.colrange = grep("^tl01$", colnames(retro)):grep("^tl12$", colnames(retro))
xt.colrange = grep("^xt01$", colnames(retro)):grep("^xt12$", colnames(retro))
p.colrange = grep("^p01$", colnames(retro)):grep("^p12$", colnames(retro))

for(i in 1:12){
  retro$t <- (retro[, th.colrange[i]]+retro[, tl.colrange[i]])/2  -  retro[, xt.colrange[i]]
  colnames(retro)[colnames(retro) == 't'] <- paste0("dt", month[i])
}

retro <- retro[,-c(th.colrange,tl.colrange, xt.colrange, p.colrange)]
saveRDS(retro, 'output/retro.RDS') 
#make idw grids ... probably forego a whole set of 1990 corrections
retro <- readRDS('output/retro.RDS')
retro <- subset(retro, nchar(NAME) >1)
Elev5km = rast('output/Elev5km.tif')

d<- subset(retro, !is.na(Lon) &!is.na(Lat) &!is.na(dt07))
d <- subset(d, dt07 < 3 & dt07 > -3)
d2 <- st_as_sf(d, coords = c("Lon", "Lat"), crs = crs(Elev5km))
d2<- as_Spatial(d2)
writeOGR(d2, 'output','d2', driver = 'ESRI Shapefile', overwrite_layer = T)
template <-  aggregate(Elev5km, fact=10)
r <- raster(template)
r[r>=0] <- NA; r[r<0] <- NA
gs <- gstat(id = 'dt07', formula=dt07~1,  data=d2)
idw <- interpolate(r, gs, debug.level=0)
writeRaster(idw,'output/idw2.tif', overwrite=T)
