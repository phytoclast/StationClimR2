#
#
library(shiny)
library(ggplot2)
library(plyr)


######
# Define server logic
shinyServer(function(input, output, session) {
  
  output$stationselect = renderUI({
    
    liststations1 <- subset(clim.tab.fill,
                        Lat >= input$lat[1] &
                          Lat <= input$lat[2] &
                          Lon >= input$lon[1] &
                          Lon <= input$lon[2] 
    )
    liststations <- sort(unique(liststations1[, c('Station_Name')]))
    
    selectInput(inputId = "stationselect", #name of input
                label = "Select Station Name:", #label displayed in ui
                choices = unique(liststations), #calls list of available counties
                selected = 'GRAND RAPIDS MI')
  })

  output$country = renderUI({
    
    
    
    selectInput(inputId = "country", #name of input
                label = "Filter Lat-Lon by Admin Area:", #label displayed in ui
                choices = (unique(geomaxmin[,1])), #calls list of available counties
                selected = 'NORTHERN AMERICA')
  })
  
  output$elev = renderUI({
    
    sliderInput(inputId = 'elev',
                label = 'Elevation',
                min= -1000, max= 9000,
                value= c(0), step = 50,
                dragRange = TRUE)
    
  })
  output$lat = renderUI({
    latmax <- geomaxmin[geomaxmin$name %in% input$country, 'latmax']
    latmin <- geomaxmin[geomaxmin$name %in% input$country, 'latmin']
    sliderInput(inputId = 'lat',
                label = 'Latitude range',
                min= -90, max= 90,
                value= c(latmin-0.5, latmax+0.5), step = 1,
                dragRange = TRUE)
    
  })
  output$lon = renderUI({
    lonmax <- geomaxmin[geomaxmin$name %in% input$country, 'lonmax']
    lonmin <- geomaxmin[geomaxmin$name %in% input$country, 'lonmin']
    sliderInput(inputId = 'lon',
                label = 'Longitude range',
                min= -180, max= 180,
                value= c(lonmin-1.25, lonmax+1.25), step = 2.5,
                dragRange = TRUE)
    
  })
  
  output$climplot <- renderPlot({ 
    
    #parameters
    selected <- input$stationselect
    
    
    station <- subset(clim.tab.fill,
                           clim.tab.fill$Station_Name %in% selected) [1,]
    clim.tab <- subset(clim.tab.fill, !is.na(p.sum), select=c("Station_ID","Station_Name","State","Lat","Lon","Elev",
                                                              "t.mean","t.max", "t.min","tm.range","th.mean","td.range","p.sum","p.ratio"))
    

    sLat =   station$Lat[1]  
    sLon =   station$Lon[1]  
    sElev =   station$Elev[1]  
    shape=2
    localzone = 50
    cutoff = 500
    clim.tab$altdifwt <- (clim.tab$Elev - sElev)^2/((clim.tab$Elev - sElev)^2 + 500^2)

    clim.tab$dist <- (((clim.tab$Lat - sLat)*10000/90)^2 + ((clim.tab$Lon - sLon)*cos(sLat*2*3.141592/360)*10000/90)^2)^0.5
    clim.tab$wt <- (localzone/(clim.tab$dist+localzone))^shape*100
    clim.tab$cutoff <- cutoff + cutoff*clim.tab$altdifwt/2
    midElev <- (quantile(clim.tab[clim.tab$dist < clim.tab$cutoff, ]$Elev,.99, na.rm = T) + quantile(clim.tab[clim.tab$dist < clim.tab$cutoff, ]$Elev,.01, na.rm = T))/2

    clim.tab$wt <- ifelse(clim.tab$dist > clim.tab$cutoff, 0, clim.tab$wt)
    clim.tab$wt.low <- clim.tab$wt * e.wtlow(clim.tab$Elev, midElev)
    clim.tab$wt.high <- clim.tab$wt * e.wthigh(clim.tab$Elev, midElev)
   
    model.1A <- lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low)
    model.1B <- lm(t.mean ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
    f.t.meanA = model.1A$coefficients[2]
    f.t.meanB = model.1B$coefficients[2]


    model.2.1A <- lm(t.max ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low, na.action=na.exclude)
    f.t.maxA = model.2.1A$coefficients[2]
    model.2.1B <- lm(t.max ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
    f.t.maxB = model.2.1B$coefficients[2]

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


    model.5A <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low)
    f.p.ratioA = model.5A$coefficients[2]
    model.5B <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
    f.p.ratioB = model.5B$coefficients[2]

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
    station$p.ratio1 <- f.p.ratioA * (pmin(midElev,Elev1) - pmin(midElev,station$Elev)) + f.p.ratioA * (pmax(midElev,Elev1) - pmax(midElev,station$Elev)) + station$p.ratio
    r.vert(station$p.ratio)
    r.vert(station$p.ratio1)



    t.colrange = grep("^t01$", colnames(station)):grep("^t12$", colnames(station))
    th.colrange = grep("^th01$", colnames(station)):grep("^th12$", colnames(station))
    tl.colrange = grep("^tl01$", colnames(station)):grep("^tl12$", colnames(station))
    p.colrange = grep("^p01$", colnames(station)):grep("^p12$", colnames(station))

    pfactor <-   apply(1-((1-station[,p.colrange]/station$p.max)), MARGIN = 1, FUN='sum')/apply(1-((1-station[,colrange]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))), MARGIN = 1, FUN='sum')*p.vert(station$p.sum1)/ p.vert(station$p.sum)


    #New Table ----
    clim.tab2 <- NULL

    for(i in 1:12){#i=1
      Mon = i
      Lat=station$Lat
      Lon=station$Lon
      sElev=sElev
      Elev1=Elev1
      p <- (1-((1-station[,p.colrange[i]]/station$p.max)/(1-r.vert(station$p.ratio))*(1-r.vert(station$p.ratio1))))*station$p.max*pfactor[1]
      t <- ifelse(station[,t.colrange[i]]> station$t.mean,
                  (station[,t.colrange[i]]-station$t.mean)/station$t.rangeA*station$t.rangeA1+station$t.mean + (station$t.mean1 - station$t.mean),(station[,t.colrange[i]]-station$t.mean)/station$t.rangeB*station$t.rangeB1+station$t.mean + (station$t.mean1 - station$t.mean))[1]
      th <- t + (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
      tl <- t - (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
      p.o <- station[,p.colrange[i]]
      t.o <- station[,t.colrange[i]]
      th.o <- station[,th.colrange[i]]
      tl.o <- station[,tl.colrange[i]]


      clim.tab0 <- data.frame(cbind(Mon,Lat,Lon,sElev,Elev1,p.o,p,t.o,t,th.o,tl.o,th,tl))
      if(is.null(clim.tab2)){clim.tab2 <- clim.tab0}else{clim.tab2 <- rbind(clim.tab2,clim.tab0)}
    }
    rownames(clim.tab2)<- clim.tab2$Mon;clim.tab0<- NULL

    
    #PET ----

    Elev <- Elev1
    climtab <- subset(clim.tab2, select=c(Mon,p,t,th,tl))
    #Humidity ----
    climtab$t <- (climtab$th+climtab$tl)/2
    climtab$Vpmax = 0.6108*exp(17.27*climtab$th/(climtab$th+237.3)) #saturation vapor pressure kPa
    climtab$Vpmin = 0.6108*exp(17.27*climtab$tl/(climtab$tl+237.3)) #saturation vapor pressure kPa
    climtab$Vp = (climtab$Vpmax+climtab$Vpmin)/2
    climtab$RH = climtab$Vpmin/climtab$Vp*100



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

 
    
    
  })
  })
