library(sf)
library(ggplot2)
library(plyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Global -----
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
  dcl <- c(-0.36716856, -0.23248978, -0.03864644,  0.17304543,  0.33397508,  0.40733259,  0.37096065,  0.23176540,  0.03163222, -0.17702222, -0.33798947, -0.40790729)
  declination <- dcl[Month]
  
  hs <- acos(pmin(pmax(-tan(Lat/360*2*3.141592) * tan(declination),-1),1))
  Ra <- 117.5 * (hs*sin(Lat/360*2*3.141592)*sin(declination) +
                   cos(Lat/360*2*3.141592)*cos(declination)*sin(hs)) / 3.141592
  return(Ra)
}

GetDayLength<- function(Month, Lat){
  dcl <- c(-0.36716856, -0.23248978, -0.03864644,  0.17304543,  0.33397508,  0.40733259,  0.37096065,  0.23176540,  0.03163222, -0.17702222, -0.33798947, -0.40790729)
  declination <- dcl[Month]
  
  Dl <- ifelse(Lat + declination*360/2/3.141592 > 89.16924, 24, ifelse(Lat - declination*360/2/3.141592 >= 90, 0, (atan(-((sin(-0.83/360*2*3.141592)-sin(declination)*sin(Lat/360*2*3.141592))/(cos(declination)*cos(Lat/360*2*3.141592)))/(-((sin(-0.83/360*2*3.141592)-sin(declination)*sin(Lat/360*2*3.141592))/(cos(declination)*cos(Lat/360*2*3.141592)))*((sin(-0.83/360*2*3.141592)-sin(declination)*sin(Lat/360*2*3.141592))/(cos(declination)*cos(Lat/360*2*3.141592)))+1)^0.5)+2*atan(1))/3.141592*24))
  return(Dl)}

GetSolar <- function(Ra, Elev, th, tl){
  Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
  Rso <- (0.75+2*10^-5*Elev)*Ra
  Rs <- pmin(Rso,pmax(0.3*Rso, 0.14*(th-tl)^0.5*Ra)) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1 and using formula for Hargreaves with average constant of 0.175 for 0.16 inland and 0.19 for coastal, but reduced to 0.14 because of bias suggests it is 0.8 of the actual values at a few selected stations
  return(Rs)}

GetNetSolar <- function(Ra, Elev, th, tl){
  Vpmin = 0.6108*exp(17.27*tl/(tl+237.3)) #saturation vapor pressure kPa
  Rso <- (0.75+2*10^-5*Elev)*Ra
  Rs <- pmin(Rso,pmax(0.3*Rso, 0.14*(th-tl)^0.5*Ra)) # Estimate of normally measured solar radiation Rs/Rso is limited to 0.3-1 and using formula for Hargreaves with average constant of 0.175 for 0.16 inland and 0.19 for coastal, but reduced to 0.14 because of bias suggests it is 0.8 of the actual values at a few selected stations
  Rnl <- 4.901*10^-9 * (1.35*Rs/(Rso+0.000001)-0.35) * (0.34 - 0.14 * Vpmin^0.5) * ((th+273.16)^4 + (tl+273.16)^4)/2
  Rns <- (1-0.23)*Rs
  Rn <- pmax(0,Rns - Rnl)
  return(Rn)}


month <- c('01','02','03','04','05','06','07','08','09','10','11','12')

pre.tab <- readRDS('data/harmonized.RDS'); rownames(pre.tab) <- NULL
listofstations <-readRDS('data/listofstations.RDS')
if(is.null(listofstations$t01)){for (i in 1:12){
  listofstations$x <- (listofstations[,paste0('th',month[i])] + listofstations[,paste0('tl',month[i])]) /2
  colnames(listofstations)[colnames(listofstations) == 'x'] <- paste0("t", month[i])
}}
clim.tab.fill <- pre.tab

Q2 <- readRDS('data/Norms2010.Q2.RDS')
Q8 <- readRDS('data/Norms2010.Q8.RDS')
periods <- data.frame(cbind(period=c('1961-1990','1981-2010','+2C future'), speriod=c('1990','2010','2080')))


if(is.null(clim.tab.fill$t01)){for (i in 1:12){
  clim.tab.fill$x <- (clim.tab.fill[,paste0('th',month[i])] + clim.tab.fill[,paste0('tl',month[i])]) /2
  colnames(clim.tab.fill)[colnames(clim.tab.fill) == 'x'] <- paste0("t", month[i])
}}

clim.tab.fill <- subset(clim.tab.fill, !(tl07 > th07|tl01 > th01|tl02 > th02|tl03 > th03|tl04 > th04|tl05 > th05|
                     tl06 > th06|tl08 > th08|tl09 > th09|tl10 > th10|tl11 > th11|tl12 > th12))
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

#----
#### Server ---- 
timeperiod = '1990'
clim.tab <- subset(clim.tab.fill, !is.na(p.sum) & Period %in% '1990', select=c("NAME","Lat","Lon","Elev",
                                                                                   "t.mean","t.max", "t.min","tm.range","th.mean","td.range","p.sum","p.ratio"))


station0 <- subset(listofstations,
                  Station_Name %in% 'MT WASHINGTON NH') [1,]

station <- subset(clim.tab.fill, Lat==station0$Lat & Lon==station0$Lon & Elev==station0$Elevation & Period %in% timeperiod)
station.Q <- subset(clim.tab.fill, Lat==station0$Lat & Lon==station0$Lon & Elev==station0$Elevation & Period %in% '2010')[1,]
station.Q2 <- subset(Q2, Latitude==station0$Lat & Longitude==station0$Lon & Elevation==station0$Elevation)[1,]
station.Q8 <- subset(Q8, Latitude==station0$Lat & Longitude==station0$Lon & Elevation==station0$Elevation)[1,]

sLat =   station$Lat[1]  
sLon =   station$Lon[1]  
sElev =   station$Elev[1]  
shape=2
localzone = 50
cutoff = 500
clim.tab$altdifwt <- (clim.tab$Elev - sElev)^2/((clim.tab$Elev - sElev)^2 + 500^2)

clim.tab$dist <- (((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5
clim.tab$wt <- (localzone/(clim.tab$dist+localzone))^shape*100
clim.tab$cutoff <- cutoff + cutoff*clim.tab$altdifwt/1
midElev <- (quantile(clim.tab[clim.tab$dist < clim.tab$cutoff, ]$Elev,.99, na.rm = T) + quantile(clim.tab[clim.tab$dist < clim.tab$cutoff, ]$Elev,.01, na.rm = T))/2

clim.tab$wt <- ifelse(clim.tab$dist > clim.tab$cutoff, 0, clim.tab$wt)
clim.tab$wt.low <- clim.tab$wt * e.wtlow(clim.tab$Elev, midElev)
clim.tab$wt.high <- clim.tab$wt * e.wthigh(clim.tab$Elev, midElev)

model.1A <- lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low)
model.1B <- lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
f.t.meanA = model.1A$coefficients[2]
f.t.meanB = model.1B$coefficients[2]
midElev
summary(model.1A)
summary(model.1B)
model.2.1A <- lm(t.max ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low, na.action=na.exclude)
f.t.maxA = model.2.1A$coefficients[2]
model.2.1B <- lm(t.max ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
f.t.maxB = model.2.1B$coefficients[2]

summary(model.2.1A)
summary(model.2.1B)

model.2.2A <- lm(t.min ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low)
f.t.minA = model.2.2A$coefficients[2]
model.2.2B <- lm(t.min ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
f.t.minB = model.2.2B$coefficients[2]



model.3.1A <- lm(th.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low)
f.th.meanA = model.3.1A$coefficients[2]
model.3.1B <- lm(th.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
f.th.meanB = model.3.1B$coefficients[2]

model.4A <- lm(p.sum ~ Elev + Lat + Lon, data = clim.tab, weights = wt.low)
summary(model.4A)
f.p.sumA = model.4A$coefficients[2]
model.4B <- lm(p.sum ~ Elev + Lat + Lon, data = clim.tab, weights = wt.high)
summary(model.4B)
f.p.sumB = model.4B$coefficients[2]


# model.5A <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low)
# f.p.ratioA = model.5A$coefficients[2]
# model.5B <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
# f.p.ratioB = model.5B$coefficients[2]

#Choose Elevation ----
Elev1 = 2000

station$t.mean1 <- f.t.meanA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.t.meanB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$t.mean
station$t.mean
station$t.mean1

station$t.max1 <- f.t.maxA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.t.maxB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$t.max
station$t.max
station$t.max1
station$t.min1 <- f.t.minA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.t.minB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev))  + station$t.min
station$t.min
station$t.min1
station$t.rangeA <- station$t.max - station$t.mean
station$t.rangeB <- station$t.mean - station$t.min
station$t.rangeA1 <- station$t.max1 - station$t.mean1
station$t.rangeB1 <- station$t.mean1 - station$t.min1
station$th.mean1 <- f.th.meanA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.th.meanB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$th.mean

station$td.rangeA1 <- (station$th.mean1 - station$t.mean1)*2

station$p.sum1 <- f.p.sumA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.p.sumB * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$p.sum
p.vert(station$p.sum)
p.vert(station$p.sum1)
# station$p.ratio1 <- f.p.ratioA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.p.ratioA * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$p.ratio
# r.vert(station$p.ratio)
# r.vert(station$p.ratio1)



t.colrange = grep("^t01$", colnames(station)):grep("^t12$", colnames(station))
th.colrange = grep("^th01$", colnames(station)):grep("^th12$", colnames(station))
tl.colrange = grep("^tl01$", colnames(station)):grep("^tl12$", colnames(station))
p.colrange = grep("^p01$", colnames(station)):grep("^p12$", colnames(station))
tQ.colrange = grep("^t01$", colnames(station.Q2)):grep("^t12$", colnames(station.Q2))
pQ.colrange = grep("^p01$", colnames(station.Q2)):grep("^p12$", colnames(station.Q2))

# pfactor <-   apply(1-((1-station[,p.colrange]/station$p.max)), MARGIN = 1, FUN='sum')/apply(1-((1-station[,colrange]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))), MARGIN = 1, FUN='sum')*p.vert(station$p.sum1)/ p.vert(station$p.sum)


#New Table ----
clim.tab2 <- NULL

for(i in 1:12){#i=1
  Mon = i
  Lat=station$Lat
  Lon=station$Lon
  sElev=sElev
  Elev1=Elev1
  p <- station[,p.colrange[i]]* p.vert(station$p.sum1)/p.vert(station$p.sum)
  pQ2 <- p*station.Q2[,pQ.colrange[i]]/(station.Q[,p.colrange[i]] + 1)
  pQ8 <- p*station.Q8[,pQ.colrange[i]]/(station.Q[,p.colrange[i]] + 1)
  #p <- (1-((1-station[,p.colrange[i]]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))))*station$p.max*pfactor[1]
  t <- ifelse(station[,t.colrange[i]]> station$t.mean,
              (station[,t.colrange[i]]-station$t.mean)/station$t.rangeA*station$t.rangeA1+station$t.mean + (station$t.mean1 - station$t.mean),(station[,t.colrange[i]]-station$t.mean)/station$t.rangeB*station$t.rangeB1+station$t.mean + (station$t.mean1 - station$t.mean))[1]
  th <- t + (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
  tl <- t - (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
  tQ2 <- t+station.Q2[,tQ.colrange[i]]-(station.Q[,t.colrange[i]])
  tQ8 <- t+station.Q8[,tQ.colrange[i]]-(station.Q[,t.colrange[i]])
  
  
  p.o <- station[,p.colrange[i]]
  t.o <- station[,t.colrange[i]]
  th.o <- station[,th.colrange[i]]
  tl.o <- station[,tl.colrange[i]]
  
  
  clim.tab0 <- data.frame(cbind(Mon,Lat,Lon,sElev,Elev1,p.o,p,t.o,t,th.o,tl.o,th,tl,tQ2, tQ8, pQ2, pQ8))
  if(is.null(clim.tab2)){clim.tab2 <- clim.tab0}else{clim.tab2 <- rbind(clim.tab2,clim.tab0)}
}
rownames(clim.tab2)<- clim.tab2$Mon;clim.tab0<- NULL


#PET ----

Elev <- Elev1
climtab <- subset(clim.tab2, select=c(Mon,p,t,th,tl,tQ2,tQ8,pQ2,pQ8))
#Humidity ----
climtab$t <- (climtab$th+climtab$tl)/2
climtab$Vpmax = 0.6108*exp(17.27*climtab$th/(climtab$th+237.3)) #saturation vapor pressure kPa
climtab$Vpmin = 0.6108*exp(17.27*climtab$tl/(climtab$tl+237.3)) #saturation vapor pressure kPa
climtab$Vp = (climtab$Vpmax+climtab$Vpmin)/2
climtab$RH = climtab$Vpmin/climtab$Vp*100
climtab$b <- ifelse(climtab$t >0, climtab$t,0)
Tg <- pmax(mean(climtab[c(5:10),]$b),mean(climtab[c(1:4,11:12),]$b))
Tc <- min(climtab$t)
Tcl <-  min(climtab$tl)
Tw <-  max(climtab$t)
Twh <-  max(climtab$th)
Tclx <- XtremLow(Tcl,Lat,Lon,Elev)

#calculate radiation ----
climtab$Ra <- GetSolarRad(climtab$Mon, Lat)
climtab$Rs <- GetSolar(climtab$Ra, Elev, climtab$th, climtab$tl)
climtab$Rn <- GetNetSolar(climtab$Ra, Elev, climtab$th, climtab$tl)
climtab$Gi = 0.07*(climtab[monind[as.numeric(rownames(climtab))+2],]$t - climtab[monind[as.numeric(rownames(climtab))],]$t)

climtab$delta <- 2503*exp(17.27*climtab$t/(climtab$t+237.3))/(climtab$t+237.3)^2


climtab$lambda <- 2.501 - (2.361*10^-3)*climtab$t
Ps <- 101.3*((293-0.0065*Elev)/293)^5.26 #kPa
gamma = 0.000665*Ps

climtab$Dl <- GetDayLength(climtab$Mon, Lat)
climtab$I = (pmax(0,climtab$t)/5)^1.514#Thornthwaite
I <- sum(climtab$I); climtab$I <- NULL#Thornthwaite
a = 0.49239+1792*10^-5*I-771*10^-7*I^2+675*10^-9*I^3#Thornthwaite
cf <- 0.92/1.26 #Correction factor to make for forest and mixed landuse vegetation instead of short grass, based on alpha of Priestly-Taylor equation

climtab$e.tw = 16*(10*pmax(climtab$t,0)/I)^a*(climtab$Dl/12)*(Days[climtab$Mon]/30)#Thornthwaite

climtab$e.ho <- 58.93/365*pmax(0, climtab$t)*Days[climtab$Mon]#Holdridge

climtab$e.gs <- 0.008404*216.7*exp(17.26939*climtab$t/
                                     (climtab$t+237.3))/(climtab$t+273.3)*(climtab$Ra)*Days[climtab$Mon]*abs((climtab$th - climtab$tl))^0.5 + 0.001#Schmidt

climtab$e.pt <- cf* 1.26 * (climtab$delta / (climtab$delta + gamma))*pmax(0,(climtab$Rn-climtab$Gi))/climtab$lambda*Days[climtab$Mon] #Priestley-Taylor

climtab$e.pm <- cf* (0.408*climtab$delta*pmax(0,(climtab$Rn-climtab$Gi))+gamma*900/(climtab$t+273)*2*(climtab$Vp-climtab$Vpmin))/(climtab$delta+gamma*(1+0.34*2))*Days[climtab$Mon] #Penman-Monteith

climtab$e.hs <- cf* 0.408*0.0023*(climtab$t+17.78)*(climtab$th-climtab$tl)^0.5*climtab$Ra*Days[climtab$Mon]#Hargreaves Samani

climtab$e.tc <- cf* 0.01333 *((23.9001*climtab$Rs)+50)*pmax(climtab$t,0)/(pmax(climtab$t,0)+15)*(1+(50-pmin(50,climtab$RH))/70)*Days[climtab$Mon]#Turc

climtab$e.mh <- cf* 0.7 * (climtab$delta / (climtab$delta + gamma))*climtab$Rs/climtab$lambda*Days[climtab$Mon]#Makkink-Hansen

climtab$e.hm = 0.1651 * climtab$Dl * (216.7 * (6.108 * exp(17.26939*pmax(climtab$t,0) / (pmax(climtab$t,0) + 237.3))) / (pmax(climtab$t,0) + 273.3)) * 2.376169#Hamon (last factor is correlation coefficient 1.2)

#Remove excess columns
climtab <- subset(climtab, select= -c(Vp, Vpmax, Vpmin, delta, lambda))
climtab$e <- climtab[,paste0('e.', 'gs')]
climtab$a <- pmin(climtab$e, climtab$p)


pAET <- max(climtab$a)
PET <- sum(climtab$e)
MAP <- sum(climtab$p)
AET <- sum(climtab$a)
MAAT <- mean(climtab$t)
Deficit <- max(PET - AET, 0)
Surplus <- max(MAP - AET, 0)
PPETRatio <- MAP/(PET +0.0001)
Mindex <- PPETRatio/(PPETRatio+1)
SLabel <- paste0(station0$Station_Name, " @ ",Elev, ' m (', periods[periods$speriod %in% timeperiod,]$period,')')

# Additional indices


StationMeans <- as.data.frame(cbind(Lat, Lon, Elev, Tg, Tc, Tcl,Tw, Twh, Tclx, pAET, PET, MAP, AET, MAAT, Deficit, Surplus, PPETRatio, Mindex))
StationMeans <- cbind(SLabel,StationMeans)
StationMeans$SP1 <- round(ifelse(StationMeans$PPETRatio < 0.5 & StationMeans$Surplus < 25, pmax(StationMeans$Surplus/25)  ,1),15)
StationMeans$SP2 <- round(ifelse(StationMeans$SP1 >= 1, ifelse(StationMeans$pAET < 75 & (StationMeans$Deficit >= 150 | StationMeans$PPETRatio < 1), pmax(StationMeans$pAET/75, 150/(StationMeans$Deficit+150)),1),0),15)
StationMeans$SP3 <- round(ifelse(StationMeans$SP2 >= 1, ifelse(StationMeans$Deficit >= 150 | StationMeans$PPETRatio < 1, pmax(150/(StationMeans$Deficit+150)),1),0),15)
StationMeans$SP4 <- round(ifelse(StationMeans$SP3 >= 1, pmin(1-StationMeans$Deficit/150),0),15)
StationMeans$SPindex <- StationMeans$SP1 + StationMeans$SP2 + StationMeans$SP3 + StationMeans$SP4 + 1 #Seasonal precipitation index
StationMeans$Cindex <- pmin(StationMeans$Tclx+15, StationMeans$Tc) #Cold index
StationMeans$Dindex <- StationMeans$Deficit/(StationMeans$Deficit + 100)
StationMeans$Sindex <- StationMeans$Surplus/(StationMeans$Surplus + 100)
StationMeans$Aindex <- StationMeans$pAET/(StationMeans$pAET + 100)

#Swap out the external data to a separate data frame and retain internal data for all but elevation graph.
if(T)
{savedselect <- StationMeans}#pass into storage.
savedselect <- savedselect#retrieve from storage.
currentMLR <- as.character(StationMeans$SLabel[1])#for labeling comparison graphs
savedMLRA <- as.character(savedselect$SLabel[1])#for labeling comparison graphs


Seasonalilty <- ifelse(Deficit < 150 & PPETRatio>=1, "Isopluvial",
                       ifelse(Surplus < 25 & PPETRatio < 0.5, ifelse(peakAET < 75, "Isoxeric","Pluvioxeric"),
                              ifelse(peakAET < 75,"Xerothermic","Pluviothermic")))







MRegime <- ifelse(PPETRatio>=2,"Perhumid",
                  ifelse(PPETRatio>=1.414,"Moist-Humid",
                         ifelse(PPETRatio>=1,"Dry-Humid",
                                ifelse(PPETRatio>=0.707,"Moist-Subhumid",
                                       ifelse(PPETRatio>=0.5,"Dry-Subhumid",
                                              ifelse(PPETRatio>=0.25,"Semiarid",
                                                     ifelse(PPETRatio>=0.125,"Arid","Perarid"
                                                     )))))))


BioTemperatureC <- 
  ifelse(Tc >= 20 & Tclx >=5,"Meso-Tropical",
         ifelse(Tc >= 15 & Tclx >=0,"Cryo-Tropical",
                ifelse(Tc >= 10 & Tclx >=-5,"Thermo-Sutropical",
                       ifelse(Tc >= 5 & Tclx >=-10,"Meso-Subtropical",
                              ifelse(Tc >= 0 & Tclx >=-15,"Cryo-Subtropical",
                                     ifelse(Tc >= -5 & Tclx >=-20,"Thermo-Temperate",
                                            ifelse(Tc >= -10 & Tclx >=-25,"Meso-Temperate",
                                                   ifelse(Tc >= -25 & Tclx >=-40,"Cryo-Temperate","Polar"
                                                   ))))))))

BioTemperatureW <- ifelse(Tg >= 24,"Hot (Lowland)",
                          ifelse(Tg >= 18,"Warm (Premontane)",
                                 ifelse(Tg >= 15,"Warm-Mild (Lower-Montane)",
                                        ifelse(Tg >= 12,"Cool-Mild (Upper-Montane)",
                                               ifelse(Tg >= 6,"Cool (Subalpine)","Cold (Alpine)"
                                               )))))
Climatetext<-paste(BioTemperatureW," ",BioTemperatureC,", ",MRegime," ",Seasonalilty, sep="" )

climplot <- ggplot(climtab, aes(x=Mon)) + 
  geom_bar(stat="identity",aes(fill="Precipitation", y=p/5), alpha = 0.85,  color="blue") +
  geom_bar(stat="identity", aes(fill='PET', y=e/5), alpha = 0.60,  color="red" ) +
  geom_line(stat="identity",  aes(color= "Temperature", y=t), alpha = 1) +
  geom_point(aes(shape='Mean', y=t), color="red") +
  geom_point(aes(shape='Low', y=tl), color="red") +
  geom_point(aes(shape='High', y=th), color="red") +
  geom_errorbar(aes(ymin=pQ2/5, ymax=pQ8/5), width=.2,position=position_dodge(-0.9), color="blue") +
  geom_errorbar(aes(ymin=tQ2, ymax=tQ8), width=.2,position=position_dodge(0.9), color="red") +
  
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c('01','02','03','04','05','06','07','08','09','10','11','12'))+
  scale_y_continuous(name= "Temperature",
                     breaks=c(-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45), labels=c('-20 (-4)', '-15 (  5)', '-10 (14)', '-5 (23)', '0 (32)', '5 (41)', '10 (50)', '15 (59)', '20 (68)', '25 (77)', '30 (86)', '35 (95)', '40 (104)', '°C (°F)'),
                     sec.axis = sec_axis(trans = ~.*1,
                                         name = "Precipitation",
                                         breaks=c(0,5,10,15,20,25,30,35,40,45),
                                         labels = c('0', '25   (1)', '50   (2)', '75   (3)', '100 (4)', '125 (5)', '150 (6)', '175 (7)', '200 (8)', 'mm (in)')))+
    theme(legend.position="bottom") +
  scale_fill_manual("Legend", values = c("Precipitation" = "cyan", "PET" = "yellow"))+
  scale_color_manual("",values = c("Temperature" = "red", "Mean" = "red", "Low" = "red", "High"="red","Growth"="darkgreen"))+
  scale_shape_manual("",values = c("Mean" = 19, "Low" = 6, "High"=2))+
  coord_fixed(ratio = 1/9,xlim = c(1,12), ylim = c(-20, 43))+
  labs(title = paste0("Climate of ",station0$Station_Name, ": est. @ ",Elev, ' m (', periods[periods$speriod %in% timeperiod,]$period,')'))# ,  subtitle = my_text1)


a1=data.frame(x=c(-50,-50,0,0), y=c(0,6,6,0))
a2=data.frame(x=c(-50,-50,0,0), y=c(6,12,12,6))
a3=data.frame(x=c(-50,-50,0,0), y=c(12,36,36,12))
a4=data.frame(x=c(0,0,6,0), y=c(0,6,6,0))
a5=data.frame(x=c(0,0,18,6), y=c(6,18,18,6))
a6=data.frame(x=c(0,0,15,15), y=c(18,36,36,18))
a7=data.frame(x=c(15,15,36,18), y=c(18,36,36,18))

ll1 <- data.frame(x=c(-50,6), y=c(6,6))
ll2 <- data.frame(x=c(0,0), y=c(0,36))
ll3 <- data.frame(x=c(15,15), y=c(18,36))
l1 <- data.frame(x=c(-50,12), y=c(12,12))
l2 <- data.frame(x=c(-25,0), y=c(15,15))
l3 <- data.frame(x=c(-10,18), y=c(18,18))
l4 <- data.frame(x=c(15,24), y=c(24,24))
l5 <- data.frame(x=c(-10,-10), y=c(18,36))
l6 <- data.frame(x=c(-25,-25), y=c(15,36))
l7 <- data.frame(x=c(5,5), y=c(18,36))
l8 <- data.frame(x=c(15,15), y=c(24,36))
if(T) #Decide whether to plot comparison graph.
{
  climplot2 <-  ggplot() +
    geom_polygon(data=a1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.5)+
    geom_polygon(data=a2, mapping=aes(x=x, y=y, fill='boreal'),alpha = 0.5)+
    geom_polygon(data=a3, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.5)+
    geom_polygon(data=a4, mapping=aes(x=x, y=y, fill='andean'),alpha = 0.5)+
    geom_polygon(data=a5, mapping=aes(x=x, y=y, fill='oceanic'),alpha = 0.5)+
    geom_polygon(data=a6, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.5)+
    geom_polygon(data=a7, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.5)+
    
    geom_line(data=ll1, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=ll2, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=ll3, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l1, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l2, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l3, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l4, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l5, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l6, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l7, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l8, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_point(data=StationMeans, mapping=aes(x=Cindex, y=Tg, color = "currentMLR"), size=0.5)+
    #geom_density2d(data=climtab, mapping=aes(x=Cindex, y=Tg), color = 'black',alpha = 0.25)+
    geom_point(data=savedselect, mapping=aes(x=Cindex, y=Tg, color = "savedMLRA"), size=0.5)+
    #geom_density2d(data=savedselect, mapping=aes(x=Cindex, y=Tg),color = 'red',alpha = 0.25)+
    scale_fill_manual("Thermozone", values = c("alpine" = "pink",
                                               "boreal" = "darkgreen",
                                               "temperate" = "greenyellow",
                                               "andean" = "lightblue",
                                               "oceanic" = "darkcyan",
                                               "subtropical" = "orange",
                                               "tropical" = "darkred"
                                               
    ))+
    scale_color_manual(values=c("black", "red"),
                       name="MLRA",
                       breaks=c("currentMLR", "savedMLRA"),
                       labels=c(currentMLR, savedMLRA))+
    
    scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)",
                       breaks=c(-45,-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                       labels=c('-45 (-60)','-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                                '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
    scale_y_continuous(name= "Growing Season", breaks=c(0,6,12,18,24,30))+
    coord_fixed(ratio = 1/1,xlim = c(-45,30), ylim = c(0, 33))+
    labs(title = paste("Climate of ",StationMeans[1,]$LRU, ": ", sep=""))+
    theme_bw()+
    theme(legend.position='right',axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
          panel.grid.major = element_line(), panel.grid.minor = element_blank())
}else{
  climplot2 <-  ggplot() +
    geom_polygon(data=a1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.5)+
    geom_polygon(data=a2, mapping=aes(x=x, y=y, fill='boreal'),alpha = 0.5)+
    geom_polygon(data=a3, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.5)+
    geom_polygon(data=a4, mapping=aes(x=x, y=y, fill='andean'),alpha = 0.5)+
    geom_polygon(data=a5, mapping=aes(x=x, y=y, fill='oceanic'),alpha = 0.5)+
    geom_polygon(data=a6, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.5)+
    geom_polygon(data=a7, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.5)+
    
    geom_line(data=ll1, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=ll2, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=ll3, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l1, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l2, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l3, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l4, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l5, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l6, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l7, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_line(data=l8, mapping=aes(x=x, y=y),alpha = 0.2, color='black', linetype='solid')+
    geom_point(data=StationMeans, mapping=aes(x=Cindex, y=Tg), color = 'black', size=0.5)+
    #geom_density2d(data=StationMeans, mapping=aes(x=Cindex, y=Tg),color = 'black',alpha = 0.25)+
    scale_fill_manual("Legend", values = c("alpine" = "pink",
                                           "boreal" = "darkgreen",
                                           "temperate" = "greenyellow",
                                           "andean" = "lightblue",
                                           "oceanic" = "darkcyan",
                                           "subtropical" = "orange",
                                           "tropical" = "darkred"
                                           
    ))+
    
    scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)",
                       breaks=c(-45,-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                       labels=c('-45 (-60)','-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                                '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
    scale_y_continuous(name= "Growing Season", breaks=c(0,6,12,18,24,30))+
    coord_fixed(ratio = 1/1,xlim = c(-45,30), ylim = c(0, 33))+
    labs(title = paste("Climate of ",StationMeans[1,]$LRU, ": ", sep=""))+
    theme_bw()+
    theme(legend.position='right',axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
          panel.grid.major = element_line(), panel.grid.minor = element_blank())
}

bs1=data.frame(y=c(1,1,2,2), x=c(0,0.3333,0.3333,0))
bs2=data.frame(y=c(2,2,3,3), x=c(0,1,1,0))
bs3=data.frame(y=c(3,3,4,4), x=c(0,1,1,0))
bs4=data.frame(y=c(4,4,5,5), x=c(0.5,1,1,0.5))

bm1=data.frame(y=c(1,1,4,4), x=c(0,0.1111,0.1111,0))
bm2=data.frame(y=c(1,1,4,4), x=c(0.1111,0.2,0.2,0.1111))
bm3=data.frame(y=c(1,1,4,4), x=c(0.2,0.3333,0.333,0.2))
bm4=data.frame(y=c(2,2,4,4), x=c(0.3333,0.5,0.5,0.3333))
bm5=data.frame(y=c(2,2,5,5), x=c(0.5,0.6667,0.6667,0.5))
bm6=data.frame(y=c(2,2,5,5), x=c(0.6667,1,1,0.6667))


climplot3 <- ggplot() +
  geom_polygon(data=bs1, mapping=aes(x=x, y=y, fill='isoxeric'),alpha = 0.2)+
  geom_polygon(data=bs2, mapping=aes(x=x, y=y, fill='xerothermic'),alpha = 0.2)+
  geom_polygon(data=bs3, mapping=aes(x=x, y=y, fill='pluviothermic'),alpha = 0.2)+
  geom_polygon(data=bs4, mapping=aes(x=x, y=y, fill='isopluvial'),alpha = 0.2)+
  geom_polygon(data=bm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.2)+
  geom_polygon(data=bm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.2)+
  geom_polygon(data=bm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.2)+
  geom_polygon(data=bm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.2)+
  geom_polygon(data=bm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.2)+
  geom_polygon(data=bm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.2)+
  geom_point(data=StationMeans, mapping=aes(y=SPindex, x=Mindex, color = "currentMLR"), size=0.5)+
  #geom_density2d(data=StationMeans, mapping=aes(y=SPindex, x=Mindex), color = 'black',alpha = 0.25)+
  geom_point(data=savedselect, mapping=aes(y=SPindex, x=Mindex, color = "savedMLRA"), size=0.5)+
  #geom_density2d(data=savedselect, mapping=aes(y=SPindex, x=Mindex),color = 'red',alpha = 0.25)+
  scale_fill_manual("Legend", values = c("isoxeric" = "red",
                                         "xerothermic" = "blue",
                                         "pluviothermic" = "yellow",
                                         "isopluvial" = "green",
                                         "perarid" = "red",
                                         "arid" = "orange",
                                         "semiarid" = "yellow",
                                         "subhumid" = "green",
                                         "humid" = "cyan",
                                         "perhumid" = "blue"
  ),guide = 'none')+
  scale_color_manual(values=c("black", "red"), 
                     name="MLRA",
                     breaks=c("currentMLR", "savedMLRA"),
                     labels=c(currentMLR, savedMLRA))+
  
  scale_y_continuous(name= "Seasonality", breaks=c(1, 2,3,4),
                     labels=c('Isoxeric', 'Xerothermic', 'Pluviothermic','Isopluvial'))+
  scale_x_continuous(name= "P/PET Ratio", breaks=c(0, 0.1111, 0.2,0.3333,0.5,0.6667),
                     labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
  coord_fixed(ratio = 1/9, ylim = c(1,5), xlim = c(0, 1))+
  labs(title = paste("Climate of ",StationMeans[1,]$SLabel, ": ", sep=""))+
  theme_bw()+
  theme(legend.position='right', axis.text.x = element_text(angle = 0, vjust = 0, hjust = -0.5), axis.text.y = element_text(vjust = -2), 
        panel.grid.major = element_line(), panel.grid.minor = element_blank()) 

climplot3