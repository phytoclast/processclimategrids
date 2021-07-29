DaysMonth <- readRDS('data/DaysMonth.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
i=7
  declination <- DaysMonth[i,]$declination
  Days <- DaysMonth[i,]$Days
  t <- 20
  tr <- 10
  th <- t+tr/2
  tl <- t-tr/2
  Lat <- 43
  hs <- acos(min(max(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
  Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                   cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
  e <- 0.008404*216.7*exp(17.26939*t/
                            (t+237.3))/(t+273.3)*(Ra)*Days*abs((th - tl))^0.5 + 0.001
  
  Elev <- 0
  Ps <- 101.3*((293-0.0065*Elev)/293)^5.26
  Vs <- 0.611*exp(17.27*t/
                    (t+237.3))/(t+273.3)
  Vd <- 0.611*exp(17.27*tl/
                    (t+237.3))/(t+273.3)
  Rns <- (1-0.23)*Ra
  sigma <- 4.903*10^-9
  Tkmax <- th+273.16
  Tkmin <- tl+273.16
  Rso <- (0.75+2*10^-5*Elev)*Ra
  Rs <- Ra
  Rnl <- sigma*(Tkmax^4 + Tkmin^4)/2*(0.34-0.14*Vd^0.5)*(1.35*min(Rs/Rso, 1) - 0.35)
  Rn <- Rns - Rnl
  delta <- 4098*Vs*t/(t+237.3)
  lambda <- 2.501 - (2.361*10^-3)*t
  gamma <- 1.013*Ps/(0.622*lambda)
  alpha <- 1.26
  e <- alpha * delta / (delta + gamma) * Rn/lambda *Days
  