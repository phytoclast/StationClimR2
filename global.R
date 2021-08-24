library(shiny)
library(sf)
library(plyr)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Norms2010 <- readRDS(file='data/Norms2010.RDS')
DaysMonth <- readRDS(file='data/DaysMonth.RDS')
savedselect <- readRDS(file='data/savedselect.RDS')
DaysMonth$declination <- 0.409*sin(2*3.141592*DaysMonth$Day_/365-1.39)

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

month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
pre.tab <- readRDS('data/pre.tab.RDS')
pre.tab$Station_Name <- paste(pre.tab$Station_Name, pre.tab$State)
cols.th <- colnames(pre.tab[,grep("^th01$", colnames(pre.tab)):grep("^th12$", colnames(pre.tab))])
cols.tl <- colnames(pre.tab[,grep("^tl01$", colnames(pre.tab)):grep("^tl12$", colnames(pre.tab))])
cols.p <- colnames(pre.tab[,grep("^p01$", colnames(pre.tab)):grep("^p12$", colnames(pre.tab))])
cols.th.2080 <- colnames(pre.tab[,grep("^th.2080.01$", colnames(pre.tab)):grep("^th.2080.12$", colnames(pre.tab))])
cols.tl.2080 <- colnames(pre.tab[,grep("^tl.2080.01$", colnames(pre.tab)):grep("^tl.2080.12$", colnames(pre.tab))])
cols.p.2080 <- colnames(pre.tab[,grep("^p.2080.01$", colnames(pre.tab)):grep("^p.2080.12$", colnames(pre.tab))])


#Choose Warming or not (T/F) ----
warming = F



clim.tab.fill <- pre.tab
if(warming == T){
  clim.tab.fill <- clim.tab.fill[, !colnames(clim.tab.fill) %in% c(cols.th, cols.tl, cols.p)]
  colnames(clim.tab.fill)[colnames(clim.tab.fill) %in% cols.th.2080] <- cols.th
  colnames(clim.tab.fill)[colnames(clim.tab.fill) %in% cols.tl.2080] <- cols.tl
  colnames(clim.tab.fill)[colnames(clim.tab.fill) %in% cols.p.2080] <- cols.p
}else{
  clim.tab.fill <- clim.tab.fill[, !colnames(clim.tab.fill) %in% c(cols.th.2080, cols.tl.2080, cols.p.2080)]
}

if(is.null(clim.tab.fill$t01)){for (i in 1:12){
  clim.tab.fill$x <- (clim.tab.fill[,paste0('th',month[i])] + clim.tab.fill[,paste0('tl',month[i])]) /2
  colnames(clim.tab.fill)[colnames(clim.tab.fill) == 'x'] <- paste0("t", month[i])
}
}
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

clim.tab <- subset(clim.tab.fill, !is.na(p.sum), select=c("Station_ID","Station_Name","State","Lat","Lon","Elev",
                                                          "t.mean","t.max", "t.min","tm.range","th.mean","td.range","p.sum","p.ratio"))
