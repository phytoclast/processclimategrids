setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DaysMonth <- readRDS('data/DaysMonth.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)
i=12
  declination <- DaysMonth[i,]$declination
  Days <- DaysMonth[i,]$Days
  t <- 20
  tr <- 8
  th <- t+tr/2
  tl <- t-tr/2
  Lat <- 60
  hs <- acos(min(max(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
  Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                   cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
  e <- 0.008404*216.7*exp(17.26939*t/
                            (t+237.3))/(t+273.3)*(Ra)*Days*abs((th - tl))^0.5 + 0.001
  # ------
  dr <- 1+0.033*cos(2*3.141592/365*DaysMonth[i,]$Day_)
  ws <- acos(-tan(Lat*2*3.141592/360)*tan(DaysMonth[i,]$declination))
  Rsa <- (24*60/3.141592)*0.08202*dr*(
    cos(Lat*2*3.141592/360) * cos(DaysMonth[i,]$declination) * sin(ws) + ws * sin(Lat*2*3.141592/360)*sin(DaysMonth[i,]$declination)
    )
  # ------
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
  e.pt <- alpha * delta / (delta + gamma) * Rn/lambda *Days
  #Hargreaves-Samani ----
  
  Kt = 0.17
  e.hs <- 0.0135*Kt*(T+17.78)*(th-tl)^0.5*Ra*Days
  #Reduced PM ----
  Rs = max(0,0.7*Ra - 4) # Estimate of normally measured solar radition
  Vp = 0.611*exp(17.27*t/(t+237.3))
  Vpmax = 0.611*exp(17.27*th/(th+237.3))
  Vpmin = 0.611*exp(17.27*tl/(tl+237.3))
  RH = Vpmin/Vp*100
   
  Gi = 0.07*(20-18) 
  #Turc ----
  alpha = 0.01333
  
  

  e.tc <- alpha *((23.9001*Rs)+50)*t/(t+15)*(1+(50-min(50,RH))/70)*Days
  # PT ----
  alpha = 1.26
  lambda = 2.501 - 0.0002361*t
  delta = 0.200*(0.00738*t + 0.8072)^7 - 0.000116
  Ps = 101.3 - 0.01055*Elev
  gamma = 1.013*Ps/(0.622*lambda)
  Rso = (0.75 + 2*10^-5*Elev)*Ra #clear sky radiation
  Rs = max(0.3*Rso,0.7*Ra - 4) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1
  

  Gi = 0.07*(t-t)#heat flux monthly periods previous and following months t[i+1] - t[i-1] make index to capture months in a cycle 1:14 for c(12,1:12,1); 1=m+1; m=months 1:12
  f=(1.2*Rs/Ra+0.1)
  fcd=(1.35*Rs/Rso-0.35)
  Rns =  (1 - 0.23)*Rs
 Rnl = 4.901*10^-9 * fcd * (0.34 - 0.14 * Vpmin^0.5) * ((th+273.16)^4 + (tl+273.16)^4)/2
 Rn = Rns - Rnl
  Rn = 0.77*Rs - 2.45*10^-9*f*(0.261*exp(-7.7710*10^-4*t^2) - 0.02)*((th+273.16)^4 + (tl+273.16)^4)+ 0.83
  e.pt <- (alpha * delta / (delta + gamma)*(Rn-Gi))*Days