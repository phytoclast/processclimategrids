library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Days <- c(31.00, 28.25, 31.00, 30.00, 31.00, 30.00, 31.00, 31.00, 30.00, 31.00, 30.00, 31.00)
DayNumber <- c(16.000,45.625,75.250,106.125,136.250,166.750,197.250,228.250,258.750,289.250,319.750,350.250)
dcl <- 0.409*sin(2*3.141592*DayNumber/365-1.39)

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
#load basic files ----
# for (i in 1:12){
#   assign(paste0('p',month[i]), rast(paste0('10min/wc2.1_10m_prec/wc2.1_10m_prec_',month[i],'.tif')))
# }
# for (i in 1:12){
#   assign(paste0('tl',month[i]), rast(paste0('10min/wc2.1_10m_tmin/wc2.1_10m_tmin_',month[i],'.tif')))
# }
# for (i in 1:12){
#   assign(paste0('th',month[i]), rast(paste0('10min/wc2.1_10m_tmax/wc2.1_10m_tmax_',month[i],'.tif')))
# }
# for (i in 1:12){
#   assign(paste0('t',month[i]), rast(paste0('10min/wc2.1_10m_tavg/wc2.1_10m_tavg_',month[i],'.tif')))
# }
# for (i in 1:12){
#   assign(paste0('Vp',month[i]), rast(paste0('10min/wc2.1_10m_vapr/wc2.1_10m_vapr_',month[i],'.tif')))
# }
# for (i in 1:12){
#   assign(paste0('Rs',month[i]), rast(paste0('10min/wc2.1_10m_srad/wc2.1_10m_srad_',month[i],'.tif')))
# }
# for (i in 1:12){
#   assign(paste0('U',month[i]), rast(paste0('10min/wc2.1_10m_wind/wc2.1_10m_wind_',month[i],'.tif')))
# }
Elev <- rast('10min/wc2.1_10m_elev/wc2.1_10m_elev.tif'); names(Elev) <- 'Elev'
for (i in 1:12){#i=1
p <- assign(paste0('p',month[i]), rast(paste0('10min/wc2.1_10m_prec/wc2.1_10m_prec_',month[i],'.tif'))); names(p) <- 'p'
t <- assign(paste0('t',month[i]), rast(paste0('10min/wc2.1_10m_tavg/wc2.1_10m_tavg_',month[i],'.tif'))); names(t) <- 't'
th <- assign(paste0('th',month[i]), rast(paste0('10min/wc2.1_10m_tmax/wc2.1_10m_tmax_',month[i],'.tif'))); names(th) <- 'th'
tl <- assign(paste0('tl',month[i]), rast(paste0('10min/wc2.1_10m_tmin/wc2.1_10m_tmin_',month[i],'.tif'))); names(tl) <- 'tl'
Vp <- assign(paste0('Vp',month[i]), rast(paste0('10min/wc2.1_10m_vapr/wc2.1_10m_vapr_',month[i],'.tif'))); names(Vp) <- 'Vp'
Rs <- assign(paste0('Rs',month[i]), rast(paste0('10min/wc2.1_10m_srad/wc2.1_10m_srad_',month[i],'.tif'))); names(Rs) <- 'Rs'
U <- assign(paste0('U',month[i]), rast(paste0('10min/wc2.1_10m_wind/wc2.1_10m_wind_',month[i],'.tif'))); names(U) <- 'U'

c.brick <- c(Elev,t,th,tl,p,Vp,Rs,U)
c.tab0 <- as.data.frame(c.brick, xy=TRUE)
c.tab0$mon <- i
if(i==1){c.tab <- c.tab0}else{c.tab <- rbind(c.tab,c.tab0)}
};saveRDS(c.tab, '10min/c.tab.RDS')
rm(list = ls())
#begin again ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Days <- c(31.00, 28.25, 31.00, 30.00, 31.00, 30.00, 31.00, 31.00, 30.00, 31.00, 30.00, 31.00)
DayNumber <- c(16.000,45.625,75.250,106.125,136.250,166.750,197.250,228.250,258.750,289.250,319.750,350.250)
dcl <- 0.409*sin(2*3.141592*DayNumber/365-1.39)

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')


c.tab <- readRDS('10min/c.tab.RDS')
c.tab$Rs <- c.tab$Rs/1000

GetSolarRad <- function(Month, Lat){
  declination <- dcl[Month]
  
  hs <- acos(pmin(pmax(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
  Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                   cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
  return(Ra)
}

c.tab$Ra <- GetSolarRad(c.tab$mon, c.tab$y)
c.tab$Rso <- (0.75+2*10^-5*c.tab$Elev)*c.tab$Ra 
c.tab$Ps <- 101.3*((293-0.0065*c.tab$Elev)/293)^5.26
c.tab$Vpmax = 0.6108*exp(17.27*c.tab$th/(c.tab$th+237.3))
c.tab$Vpmin = 0.6108*exp(17.27*c.tab$tl/(c.tab$tl+237.3))
c.tab$Vpdif <- c.tab$Vpmax - c.tab$Vpmin
c.tab$RH <- c.tab$Vpmin/c.tab$Vpmax
c.tab$Vpdif2 <- c.tab$Vpmax - c.tab$Vp
c.tab$RH2 <- c.tab$Vp/c.tab$Vpmax
c.tab$tr <- c.tab$th - c.tab$tl
c.tab$RsRa <- c.tab$Rs/(c.tab$Ra+0.0001)
c.tab <-  subset(c.tab, !RsRa > 1 & Rs > 0 & Ra > 0)

c.tab$logp <- log(c.tab$p+1)
# model <- glm(RsRa ~ 0 
#              +p
#              +t:p
#              +tr
#              +Ps
#              ,data = c.tab, family = binomial)
# summary(model)
model <- lm(Rs ~ 0 
            +Rso
            +Rso:logp
            +Rso:logp:th
            +Rso:tl
            ,data = c.tab)
summary(model)

model <- lm(U ~ 
             poly(y,2)
            +Elev
            +Ra
            +th
            +tl
            +p
            ,data = c.tab)
summary(model)
mean(c.tab$U)

findRs <- function(Rso,p,th,tl) {
  Rs0 <- (Rso*9.521e-01+
            Rso*log(p+1)*-9.087e-02+
            Rso*tl*-3.644e-03+
            Rso*log(p+1)*th*1.335e-03)
  Rs <- pmax(0.3*Rso,pmin(Rso,Rs0))
  return(Rs)}


model <- lm(Vp ~ 0 
            +Vpmin:logp
            +Vpmin:Vpmax
            +Vpmin
            ,data = c.tab)
summary(model)
findVp  <- function(p,Vpmax,Vpmin) {
  Vp0 <- (Vpmin*7.976e-01+
            Vpmin*log(p+1)*9.499e-02+
            Vpmin*Vpmax*-6.599e-02)
  Vp <- pmax(0,pmin(Vpmin,Vp0))
  return(Vp)}
c.tab$Vp.new <- findVp(c.tab$p,c.tab$Vpmax,c.tab$Vpmin)


