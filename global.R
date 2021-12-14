library(shiny)
library(sf)
library(plyr)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#options(shiny.sanitize.errors = T)
Norms2010 <- readRDS(file='data/Norms2010.RDS')
monind <- c(12,1:12,1)
savedselect <- readRDS(file='data/savedselect.RDS')

lakes <- readRDS(file='data/lakes.RDS')
states <- readRDS(file='data/states.RDS')

geomaxmin <- readRDS(file='data/geomaxmin.rds')
geomaxmin <- geomaxmin[c(1,248,264:320,330:335,344,349,353,360,370),]

NormCoordTrans <- as.data.frame(readRDS(file='data/NormCoordTrans.RDS'))
colnames(NormCoordTrans)[2:3] <- c('x','y')

Norms2010<- Norms2010[,c('Station_ID','Station_Name', 'State', 'Latitude', 'Longitude', 'Elevation','Year_',
                         "t01", "t02", "t03", "t04", "t05", "t06",
                         "t07", "t08", "t09", "t10", "t11", "t12",
                         "tl01", "tl02", "tl03", "tl04", "tl05", "tl06",
                         "tl07", "tl08", "tl09", "tl10", "tl11", "tl12",
                         "pp01", "pp02", "pp03", "pp04", "pp05", "pp06",
                         "pp07", "pp08", "pp09", "pp10", "pp11","pp12")]


#functions ---- 
mmtoin <- function(p){
  round(p/25.4,2)
}
CtoF <- function(t){
  round(t*1.8+32.4,1)
}
e.trans <-  function(e){
  e1 = 0.5^((e/500-1)^2)
  return(e1)
}
e.wtlow <-  function(el,mid){
  x = 0.01^(el/mid-1)
  elwt = x/(x+1)
  return(elwt)
}
e.wthigh <-  function(el,mid){
  x = 0.01^(1-el/mid)
  elwt = x/(x+1)
  return(elwt)
}

p.trans <-  function(p){
  p1 = log2(pmax(0,p+0.0001)+100)
  return(p1)
}

p.vert <- function(p1){
  p = pmax(0,(2^(p1)-100)-0.0001)
  return(p)}

p.vert(p.trans(1000))
r.trans <-  function(r){#r=1
  r= pmin(1,(pmax(0, r)))
  r1 = log2((r*0.899999+0.1)/(1 - (r*0.899999+0.1)))
  return(r1)
}
r.vert <-  function(r1){
  r = ((2^r1/(2^r1+1))-0.1)/0.899999
  r = pmin(1,(pmax(0, r)))
  return(r)
}
t.trans <-  function(tr){
  tr1 = log2(tr+0.0001)
  return(tr1)
}
t.vert <-  function(tr1){
  tr = 2^(tr1)-0.0001
  return(tr)
}
XtremLow <- function(Tcl, Lat, Lon, Elev){
  pacificsouth <- 1/((((Lat - -22.7)/13)^2 + ((Lon - -82.3)/14)^2)^2+1)
  amazon2 <- 1/((((Lat - -10.2)/5)^2 + ((Lon - -59.9)/10)^2)^2+1)
  amazon1 <- 1/((((Lat - -2.8)/14)^2 + ((Lon - -61.3)/19)^2)^2+1)
  pacificcent <- 1/((((Lat - 4.1)/21)^2 + ((Lon - -122.4)/41)^2)^2+1)
  mexico <- 1/((((Lat - 26)/6)^2 + ((Lon - -98.4)/12)^2)^2+1)
  florida <- 1/((((Lat - 27.5)/4)^2 + ((Lon - -81.1)/8)^2)^2+1)
  pacificnorth <- 1/((((Lat - 32.9)/26)^2 + ((Lon - -145)/27)^2)^2+1)
  oklahoma <- 1/((((Lat - 33.6)/4)^2 + ((Lon - -98.4)/8)^2)^2+1)
  arizona <- 1/((((Lat - 34)/12)^2 + ((Lon - -113.1)/8)^2)^2+1)
  atlantic <- 1/((((Lat - 34)/15)^2 + ((Lon - -60.7)/19)^2)^2+1)
  himalayas <- 1/((((Lat - 35.3)/6)^2 + ((Lon - 91.3)/13)^2)^2+1)
  kentucky <- 1/((((Lat - 38.5)/3)^2 + ((Lon - -87.6)/9)^2)^2+1)
  detroit <- 1/((((Lat - 41.8)/3)^2 + ((Lon - -82.6)/4)^2)^2+1)
  ontario <- 1/((((Lat - 44.6)/2)^2 + ((Lon - -79.2)/6)^2)^2+1)
  montana <- 1/((((Lat - 45.4)/5)^2 + ((Lon - -111.8)/10)^2)^2+1)
  minn <- 1/((((Lat - 47.6)/6)^2 + ((Lon - -92.6)/12)^2)^2+1)
  hudson <- 1/((((Lat - 60)/7)^2 + ((Lon - -87)/34)^2)^2+1)
  siberia <- 1/((((Lat - 61.2)/20)^2 + ((Lon - 105.7)/39)^2)^2+1)
  california <- 1/((((Lat - 34.8)/9)^2 + ((Lon - -128.2)/9)^2)^2+1)
  washington <- 1/((((Lat - 46)/5)^2 + ((Lon - -126.6)/5)^2)^2+1)
  colorado <- 1/((((Lat - 38.3)/2)^2 + ((Lon - -108.8)/3)^2)^2+1)
  hawaii <- 1/((((Lat - 21.3)/7)^2 + ((Lon - -157.5)/11)^2)^2+1)
  chess <- 1/((((Lat - 37)/3)^2 + ((Lon - -74)/3)^2)^2+1)
  
  Tclx<-	-9.171	+
    Tcl *	1.202	+
    Lat *	-0.04149	+
    Elev *	0.0008691	+
    Lat * Elev *	-0.00002455	+
    pacificsouth *	-1.792	+
    amazon2 *	2.573	+
    amazon1 *	-1.014	+
    pacificcent *	-0.749	+
    mexico *	-0.8227	+
    florida *	-3.557	+
    pacificnorth *	-1.246	+
    oklahoma *	0.1758	+
    arizona *	2.605	+
    chess *	0.8347	+
    atlantic *	0.2967	+
    himalayas *	-1.814	+
    kentucky *	-2.644	+
    detroit *	0	+
    ontario *	-2.314	+
    montana *	-4.415	+
    minn *	1.136	+
    hudson *	-5.154	+
    siberia *	-3.797	+
    california *	4.48	+
    washington *	3.597	+
    colorado *	1.458	+
    hawaii *	6.673	
  return(Tclx)}

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

# GetSolar <- function(Ra, Elev, th, tl){
#   Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
#   Rso <- (0.75+2*10^-5*Elev)*Ra
#   Rs <- pmin(Rso,pmax(0.3*Rso, 0.14*(th-tl)^0.5*Ra)) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1 and using formula for Hargreaves with average constant of 0.175 for 0.16 inland and 0.19 for coastal, but reduced to 0.14 because of bias suggests it is 0.8 of the actual values at a few selected stations
#     return(Rs)}

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


# GetNetSolar <- function(Ra, Elev, th, tl){
#   Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
#   Rso <- (0.75+2*10^-5*Elev)*Ra
#   Rs <- pmin(Rso,pmax(0.3*Rso, 0.14*(th-tl)^0.5*Ra)) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1 and using formula for Hargreaves with average constant of 0.175 for 0.16 inland and 0.19 for coastal, but reduced to 0.14 because of bias suggests it is 0.8 of the actual values at a few selected stations
#   Rnl <- 4.901*10^-9 * (1.35*Rs/(Rso+0.000001)-0.35) * (0.34 - 0.14 * Vpmin^0.5) * ((th+273.16)^4 + (tl+273.16)^4)/2
#   Rns <- (1-0.23)*Rs
#   Rn <- pmax(0,Rns - Rnl)
#   return(Rn)}

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


month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
pre.tab <- readRDS('data/harmonized.RDS'); rownames(pre.tab) <- NULL
listofstations <-readRDS('data/listofstations.RDS')
if(is.null(listofstations$t01)){for (i in 1:12){
  listofstations$x <- (listofstations[,paste0('th',month[i])] + listofstations[,paste0('tl',month[i])]) /2
  colnames(listofstations)[colnames(listofstations) == 'x'] <- paste0("t", month[i])
}}

clim.tab.fill <- pre.tab

if(is.null(clim.tab.fill$t01)){for (i in 1:12){
  clim.tab.fill$x <- (clim.tab.fill[,paste0('th',month[i])] + clim.tab.fill[,paste0('tl',month[i])]) /2
  colnames(clim.tab.fill)[colnames(clim.tab.fill) == 'x'] <- paste0("t", month[i])
}}

clim.tab.fill <- subset(clim.tab.fill, !(tl07 > th07|tl01 > th01|tl02 > th02|tl03 > th03|tl04 > th04|tl05 > th05|
                                           tl06 > th06|tl08 > th08|tl09 > th09|tl10 > th10|tl11 > th11|tl12 > th12))

Q2 <- readRDS('data/Norms2010.Q2.RDS')
Q8 <- readRDS('data/Norms2010.Q8.RDS')
periods <- data.frame(cbind(period=c('1961-1990','1981-2010','+2C future'), speriod=c('1990','2010','2080')))

#summary stats for model building table
colrange = grep("^t01$", colnames(clim.tab.fill)):grep("^t12$", colnames(clim.tab.fill))
clim.tab.fill$t.mean <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='mean')
clim.tab.fill$t.min <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='min')
clim.tab.fill$t.max <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='max')
colrange = grep("^th01$", colnames(clim.tab.fill)):grep("^th12$", colnames(clim.tab.fill))
clim.tab.fill$th.mean <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='mean')
colrange = grep("^tl01$", colnames(clim.tab.fill)):grep("^tl12$", colnames(clim.tab.fill))
clim.tab.fill$tl.mean <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='mean')
clim.tab.fill$tm.range <- t.trans(clim.tab.fill$t.max - clim.tab.fill$t.min)
clim.tab.fill$td.range <- t.trans(clim.tab.fill$th.mean - clim.tab.fill$tl.mean)
colrange = grep("^p01$", colnames(clim.tab.fill)):grep("^p12$", colnames(clim.tab.fill))
clim.tab.fill$p.sum <- p.trans(apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='sum'))
colrange = grep("^p01$", colnames(clim.tab.fill)):grep("^p12$", colnames(clim.tab.fill))
clim.tab.fill$p.max <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='max')
clim.tab.fill$p.min <- apply(clim.tab.fill[,colrange], MARGIN = 1, FUN='min')
clim.tab.fill$p.ratio <- r.trans(clim.tab.fill$p.min/(clim.tab.fill$p.max+0.000001))
#summary stats for station table
colrange = grep("^t01$", colnames(listofstations)):grep("^t12$", colnames(listofstations))
listofstations$t.mean <- apply(listofstations[,colrange], MARGIN = 1, FUN='mean')
listofstations$t.min <- apply(listofstations[,colrange], MARGIN = 1, FUN='min')
listofstations$t.max <- apply(listofstations[,colrange], MARGIN = 1, FUN='max')
colrange = grep("^th01$", colnames(listofstations)):grep("^th12$", colnames(listofstations))
listofstations$th.mean <- apply(listofstations[,colrange], MARGIN = 1, FUN='mean')
colrange = grep("^tl01$", colnames(listofstations)):grep("^tl12$", colnames(listofstations))
listofstations$tl.mean <- apply(listofstations[,colrange], MARGIN = 1, FUN='mean')
listofstations$tm.range <- t.trans(listofstations$t.max - listofstations$t.min)
listofstations$td.range <- t.trans(listofstations$th.mean - listofstations$tl.mean)
colrange = grep("^p01$", colnames(listofstations)):grep("^p12$", colnames(listofstations))
listofstations$p.sum <- p.trans(apply(listofstations[,colrange], MARGIN = 1, FUN='sum'))
colrange = grep("^p01$", colnames(listofstations)):grep("^p12$", colnames(listofstations))
listofstations$p.max <- apply(listofstations[,colrange], MARGIN = 1, FUN='max')
listofstations$p.min <- apply(listofstations[,colrange], MARGIN = 1, FUN='min')
listofstations$p.ratio <- r.trans(listofstations$p.min/(listofstations$p.max+0.000001))


