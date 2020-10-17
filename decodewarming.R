library(raster) # package for raster manipulation
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
tmax <- brick("ncfiles/TerraClimate2C_tmax.nc", varname="tmax")  
tmin <- brick("ncfiles/TerraClimate2C_tmin.nc", varname="tmin")    
ppt <- brick("ncfiles/TerraClimate2C_ppt.nc", varname="ppt")    

for (i in 1:12){
  writeRaster(tmax[[i]], paste0('warming/tx',month[i],'.tif'))
}
for (i in 1:12){
  writeRaster(tmin[[i]], paste0('warming/tn',month[i],'.tif'))
}
for (i in 1:12){
  writeRaster(ppt[[i]], paste0('warming/p',month[i],'.tif'))
}


