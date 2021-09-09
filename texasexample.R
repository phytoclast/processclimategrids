library(rgdal)
library(tmap)

# Load precipitation data
z <- gzcon(url("http://github.com/mgimond/Spatial_pre2021/raw/master/Data/precip.rds"))
P <- readRDS(z)

# Load Texas boudary map
z <- gzcon(url("http://github.com/mgimond/Spatial_pre2021/raw/master/Data/texas.rds"))
W <- readRDS(z)

# Replace point boundary extent with that of Texas
P@bbox <- W@bbox

tm_shape(W) + tm_polygons() +
  tm_shape(P) +
  tm_dots(col="Precip_in", palette = "RdBu", auto.palette.mapping = FALSE,
          title="Sampled precipitation \n(in inches)", size=0.7) +
  tm_text("Precip_in", just="left", xmod=.5, size = 0.7) +
  tm_legend(legend.outside=TRUE)


library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(raster)
# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
proj4string(grd) <- proj4string(P)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(Precip_in ~ 1, P, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster(P.idw)
r.m     <- mask(r, W)

# Plot
tm_shape(r.m) + 
  tm_raster(n=10,palette = "RdBu", auto.palette.mapping = FALSE,
            title="Predicted precipitation \n(in inches)") + 
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# ---- 
# Define the 1st order polynomial equation
f.1 <- as.formula(Precip_in ~ X + Y) 

# Add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

# Run the regression model
lm.1 <- lm( f.1, data=P)

# Use the regression model output to interpolate the surface
dat.1st <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.1, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.1st)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# Define the 2nd order polynomial equation
f.2 <- as.formula(Precip_in ~ X + Y + I(X*X)+I(Y*Y) + I(X*Y))

# Add X and Y to P
P$X <- coordinates(P)[,1]
P$Y <- coordinates(P)[,2]

# Run the regression model
lm.2 <- lm( f.2, data=P)

# Use the regression model output to interpolate the surface
dat.2nd <- SpatialGridDataFrame(grd, data.frame(var1.pred = predict(lm.2, newdata=grd))) 

# Clip the interpolated raster to Texas
r   <- raster(dat.2nd)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE,
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

# Define the 1st order polynomial equation
f.1 <- as.formula(Precip_in ~ X + Y) 

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the 
# variogram on the de-trended data.
var.smpl <- variogram(f.1, P, cloud = FALSE, cutoff=1000000, width=89900)

# Compute the variogram model by passing the nugget, sill and range values
# to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000000))

# Define the trend model
f.1 <- as.formula(Precip_in ~ X + Y) 

# Perform the krige interpolation (note the use of the variogram model
# created in the earlier step)
dat.krg <- krige( f.1, P, grd, dat.fit)

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
r.m <- mask(r, W)

# Plot the map
tm_shape(r.m) + 
  tm_raster(n=10, palette="RdBu", auto.palette.mapping=FALSE, 
            title="Predicted precipitation \n(in inches)") +
  tm_shape(P) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)

