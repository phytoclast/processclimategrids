setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ----
# 
ftoc <- function(f){
  c = round((as.numeric(as.character(f))/10-32)/1.8,2)
  return(c)  }

inchtomm <- function(i){
  m = round((as.numeric(as.character(i))/100)*25.4,1)
  return(m)  }

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
stn$Lat <- (as.numeric(stn$LATD)+as.numeric(stn$LATD)/60) * ifelse(stn$LATA %in% 'N',1,-1)
stn$Lon <- (as.numeric(stn$LOND)+as.numeric(stn$LONM)/60) * ifelse(stn$LONA %in% 'E',1,-1)

stn <- stn[,c('ID', 'NAME', 'Lat', 'Lon', 'Elev')]

stn.t <- merge(stn, tnorm, by='ID')
stn.p <- merge(stn, pnorm, by='ID')