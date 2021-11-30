#
#
library(shiny)
library(ggplot2)
library(plyr)
library(xtable)

######
# Define server logic
shinyServer(function(input, output, session) {
  rv <- reactiveValues(my_text = "") 
  output$stationselect = renderUI({
    
    liststations1 <- subset(listofstations,
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
  
  output$setelev = renderUI({
    selected <- input$stationselect
    station <- subset(listofstations,
                      Station_Name %in% selected) [1,]
    elevset <- ifelse(is.null(station$Elevation[1]), 237.1, ifelse(is.na(station$Elevation[1]), 237.1, station$Elevation[1]))

    sliderInput(inputId = 'setelev',
                 label = 'Elevation',
                 min= -1000, max= 9000,
                 value= elevset, step = 50,
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
    timeperiod <- input$timeperiod
    
    
    station0 <- subset(listofstations,
                           Station_Name %in% selected) [1,]

    #----
    

    
    clim.tab <- subset(clim.tab.fill, !is.na(p.sum) & Period %in% timeperiod, select=c("NAME","Lat","Lon","Elev",
                                                                                       "t.mean","t.max", "t.min","tm.range","th.mean","td.range","p.sum","p.ratio"))
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


    # model.5A <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.low)
    # f.p.ratioA = model.5A$coefficients[2]
    # model.5B <- lm(p.ratio ~ Elev + Lat+ Lon, data = clim.tab, weights = wt.high)
    # f.p.ratioB = model.5B$coefficients[2]

    #Choose Elevation ----
    Elev1 = input$setelev

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
      t <- ifelse(station[,t.colrange[i]]> station$t.mean,
                  (station[,t.colrange[i]]-station$t.mean)/station$t.rangeA*station$t.rangeA1+station$t.mean + (station$t.mean1 - station$t.mean),(station[,t.colrange[i]]-station$t.mean)/station$t.rangeB*station$t.rangeB1+station$t.mean + (station$t.mean1 - station$t.mean))[1]
      th <- t + (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
      tl <- t - (station[,th.colrange[i]] - station[,tl.colrange[i]])/t.vert(station$td.range)*(station$td.rangeA1)/2
      tQ2 <- t+station.Q2[,tQ.colrange[i]]-(station.Q[,t.colrange[i]])
      tQ8 <- t+station.Q8[,tQ.colrange[i]]-(station.Q[,t.colrange[i]])
      th <- pmin(pmax(th,t+0.5),t+20)
      tl <- pmax(pmin(tl,t-0.5),t-20)
      
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
    climtab$t = (climtab$th+climtab$tl)/2
    climtab$Vpmax = 0.6108*exp(17.27*climtab$th/(climtab$th+237.3)) #saturation vapor pressure kPa
    climtab$Vpmin = 0.6108*exp(17.27*climtab$tl/(climtab$tl+237.3)) #saturation vapor pressure kPa
    climtab$Vpmean = 0.6108*exp(17.27*climtab$t/(climtab$t+237.3))
    climtab$Vp = GetVp(climtab$p,climtab$th,climtab$tl)#actual vapor pressure kPa
    climtab$RH = climtab$Vp*100/climtab$Vpmean
    climtab$b = ifelse(climtab$t >0, climtab$t,0)
    Tg = pmax(mean(climtab[c(5:10),]$b),mean(climtab[c(1:4,11:12),]$b))
    Tc = min(climtab$t)
    Tcl =  min(climtab$tl)
    Tw =  max(climtab$t)
    Twh =  max(climtab$th)
    Tclx = XtremLow(Tcl,Lat,Lon,Elev)




    #calculate radiation ----
    climtab$Ra <- GetSolarRad(climtab$Mon, Lat)
    climtab$Rs <- GetSolar(climtab$Ra, Elev, climtab$th, climtab$tl, climtab$p)
    climtab$Rn <- GetNetSolar(climtab$Ra, Elev, climtab$th, climtab$tl, climtab$p)
    climtab$Gi = 0.07*(climtab[monind[as.numeric(rownames(climtab))+2],]$t - climtab[monind[as.numeric(rownames(climtab))],]$t)

    climtab$delta <- 2503*exp(17.27*climtab$t/(climtab$t+237.3))/(climtab$t+237.3)^2


    climtab$lambda <- 2.501 - (2.361*10^-3)*climtab$t
    Ps <- 101.3*((293-0.0065*Elev)/293)^5.26 #kPa
    gamma = 0.000665*Ps

    climtab$Dl <- GetDayLength(climtab$Mon, Lat)
    climtab$I = (pmax(0,climtab$t)/5)^1.514#Thornthwaite
    I <- sum(climtab$I); climtab$I <- NULL#Thornthwaite
    a = 0.49239+1792*10^-5*I-771*10^-7*I^2+675*10^-9*I^3#Thornthwaite
    cf <- 1#0.92/1.26 #Correction factor to make for forest and mixed landuse vegetation instead of short grass, based on alpha of Priestly-Taylor equation

    climtab$e.tw = 16*(10*pmax(climtab$t,0)/I)^a*(climtab$Dl/12)*(Days[climtab$Mon]/30)#Thornthwaite

    climtab$e.ho <- 58.93/365*pmax(0, climtab$t)*Days[climtab$Mon]#Holdridge

    climtab$e.gs <- 0.008404*216.7*exp(17.26939*climtab$t/
                                         (climtab$t+237.3))/(climtab$t+273.3)*(climtab$Ra)*Days[climtab$Mon]*abs((climtab$th - climtab$tl))^0.5 + 0.001#Schmidt.2018
    # var.a = 0.0000641357278041525; var.c = 0.0226701035899885
    # climtab$e.gs <- climtab$Ra*var.a/(1-var.c*climtab$t)*Days[climtab$Mon]*1000
    climtab$e.gs2 <- GetTransGrow(climtab$th, climtab$tl)*GetPET(climtab$Ra, climtab$th, climtab$tl, climtab$p)*Days[climtab$Mon] #Schmidt.2021
    
    climtab$e.pt <- cf* 1.26 * (climtab$delta / (climtab$delta + gamma))*pmax(0,(climtab$Rn-climtab$Gi))/climtab$lambda*Days[climtab$Mon] #Priestley-Taylor

    climtab$e.pm <- cf* (0.408*climtab$delta*pmax(0,(climtab$Rn-climtab$Gi))+gamma*900/(climtab$t+273)*2*(climtab$Vpmean-climtab$Vp))/(climtab$delta+gamma*(1+0.34*2))*Days[climtab$Mon] #Penman-Monteith

    climtab$e.hs <- pmax(0, cf* 0.408*0.0023*(climtab$t+17.78)*(climtab$th-climtab$tl)^0.5*climtab$Ra*Days[climtab$Mon])#Hargreaves Samani

    climtab$e.tc <- cf* 0.01333 *((23.9001*climtab$Rs)+50)*pmax(climtab$t,0)/(pmax(climtab$t,0)+15)*(1+(50-pmin(50,climtab$RH))/70)*Days[climtab$Mon]#Turc

    climtab$e.mh <- cf* 0.7 * (climtab$delta / (climtab$delta + gamma))*climtab$Rs/climtab$lambda*Days[climtab$Mon]#Makkink-Hansen

    climtab$e.hm = 0.1651 * climtab$Dl * (216.7 * (6.108 * exp(17.26939*pmax(climtab$t,0) / (pmax(climtab$t,0) + 237.3))) / (pmax(climtab$t,0) + 273.3)) * 2.376169#Hamon (last factor is correlation coefficient 1.2)

    #Set PET method
    climtab$e <- climtab[,paste0('e.', input$RadioPET)]
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

    #Swap out the external data to a separate data frame and retain internal data for graphs.
    if(input$saveselect == FALSE)
    {rv$savedselect <- StationMeans}#pass into storage.
    savedselect <- rv$savedselect#retrieve from storage.
    currentMLR <- as.character(StationMeans$SLabel[1])#for labeling comparison graphs
    savedMLRA <- as.character(savedselect$SLabel[1])#for labeling comparison graphs

    
    
    #classify ----
    Seasonalilty <- ifelse(Deficit < 150 & PPETRatio>=1, "Isopluvial",
                           ifelse(Surplus < 25 & PPETRatio < 0.5, ifelse(pAET < 75, "Isoxeric","Pluvioxeric"),
                                  ifelse(pAET < 75,"Xerothermic","Pluviothermic")))







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

    #assemble supplemental summary

    my_text1 <- paste("Lat: ",round(Lat,digits=2),"  Lon:", round(Lon,digits=2),"  Elev: ",round(Elev,digits=0)," m")
    metric <- paste("Lat: ",round(Lat,digits=2),"°;  Lon: ", round(Lon,digits=2),"°;  Elev: ",round(Elev,digits=0)," m","\n",
                    "MAAT: ",round(MAAT,digits=1),"°C;  ","MAP: ", round(MAP,0)," mm  ","\n",
                    "Warm Month: ", round(Tw,1),"°C; High: ",round(Twh,1),"°C; ", "Cold Month: ", round(Tc,1),"°C; Low: ",round(Tcl,1),"°C","\n",
                    "Growing Season Temperature: ",round(Tg,digits=1),"°C; Annual Extreme Low: ", round(Tclx,1),"°C","\n",
                    "P/PET: ", round(PPETRatio,2),"; Surplus: ", round(Surplus,0)," mm; Deficit: ", round(Deficit,0)," mm; Peak AET: ", round(pAET,0), " mm","\n", Climatetext,sep="")
    #, "SPindex: ",round(SPindex,2),"; Cindex: ",round(Cindex,2),"\n"
    retro <- paste("Lat: ",round(Lat,digits=2),"°;  Lon: ", round(Lon,digits=2),"°;  Elev: ",round(Elev/0.3048,digits=0)," ft","\n",
                   "Annual Temperature: ",round(MAAT*1.8+32,digits=0),"°F;  ","Annual Precipitation: ", round(MAP/25.4,0)," in  ","\n",
                   "Warm Month: ", round(Tw*1.8+32,0),"°F; High: ",round(Twh*1.8+32,0),"°F; ", "Cold Month: ", round(Tc*1.8+32,0),"°F; Low: ",round(Tcl*1.8+32,0),"°F","\n",
                   "Growing Season Temperature: ",round(Tg*1.8+32,digits=0),"°F; Annual Extreme Low: ", round(Tclx*1.8+32,0),"°F","\n",
                   "P/PET: ", round(PPETRatio,2),"; Surplus: ", round(Surplus/25.4,0)," in; Deficit: ", round(Deficit/25.4,0)," in; Peak AET: ", round(pAET/25.4,0), " in","\n", Climatetext,sep="")
    my_text2 <- if(input$RadioUnits == 'USC'){retro} else {metric}
    rv$my_text2 <- my_text2
    mtable <- climtab[,c('Mon', 't', 'th', 'tl', 'p', 'e', 'Dl', 'Ra', 'Rn','RH','Vp')]
    ftable <- mtable
    ftable[,c('t','th','tl')] <- CtoF(ftable[,c('t','th','tl')])
    ftable[,c('p','e')] <- mmtoin(ftable[,c('p','e')])
    rv$my_table <- if(input$RadioUnits == 'USC'){xtable(ftable)} else {xtable(mtable)}
    
#climplot ----
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
    
#climplot2 ----
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
    if(input$saveselect == TRUE) #Decide whether to plot comparison graph.
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
        labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
        theme_bw()+
        theme(legend.position='right',axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
              panel.grid.major = element_line(), panel.grid.minor = element_blank())
    }
    else{
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
        labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
        theme_bw()+
        theme(legend.position='right',axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0),
              panel.grid.major = element_line(), panel.grid.minor = element_blank())
    }
#climplot3 ----
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
    if(input$saveselect == TRUE) #Decide whether to plot comparison graph.
    { climplot3 <- ggplot() +
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
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='right', axis.text.x = element_text(angle = 0, vjust = 0, hjust = -0.5), axis.text.y = element_text(vjust = -2), 
            panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    
    }else{climplot3 <- ggplot() +
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
      geom_point(data=StationMeans, mapping=aes(y=SPindex, x=Mindex), color = 'black', size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(y=SPindex, x=Mindex),color = 'black',alpha = 0.25)+
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
      ))+
      scale_y_continuous(name= "Seasonality", breaks=c(1, 2,3,4),
                         labels=c('Isoxeric', 'Xerothermic', 'Pluviothermic','Isopluvial'))+
      scale_x_continuous(name= "P/PET Ratio", breaks=c(0, 0.1111, 0.2,0.3333,0.5,0.6667),
                         labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
      coord_fixed(ratio = 1/9, ylim = c(1,5), xlim = c(0, 1))+
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='none', axis.text.x = element_text(angle = 0, vjust = 0, hjust = -0.5), axis.text.y = element_text(vjust = -2), 
            panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    }
#climplot4 ----
    #moisture x temperature
    bw1=data.frame(y=c(0,0,6,6), x=c(0,1,1,0))
    bw2=data.frame(y=c(6,6,12,12), x=c(0,1,1,0))
    bw3=data.frame(y=c(12,12,18,18), x=c(0,1,1,0))
    bw4=data.frame(y=c(18,18,24,24), x=c(0,1,1,0))
    bw5=data.frame(y=c(24,24,30,30), x=c(0,1,1,0))
    
    bmm1=data.frame(y=c(0,0,30,30), x=c(0,0.1111,0.1111,0))
    bmm2=data.frame(y=c(0,0,30,30), x=c(0.1111,0.2,0.2,0.1111))
    bmm3=data.frame(y=c(0,0,30,30), x=c(0.2,0.3333,0.333,0.2))
    bmm4=data.frame(y=c(0,0,30,30), x=c(0.3333,0.5,0.5,0.3333))
    bmm5=data.frame(y=c(0,0,30,30), x=c(0.5,0.6667,0.6667,0.5))
    bmm6=data.frame(y=c(0,0,30,30), x=c(0.6667,1,1,0.6667))
    
    if(input$saveselect == TRUE) #Decide whether to plot comparison graph.
    {climplot4 <- ggplot() +
      geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.2)+
      geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='cool'),alpha = 0.2)+
      geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='mild'),alpha = 0.2)+
      geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='warm'),alpha = 0.2)+
      geom_polygon(data=bw5, mapping=aes(x=x, y=y, fill='hot'),alpha = 0.2)+
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.1)+
      geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.1)+
      geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.1)+
      geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.1)+
      geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.1)+
      geom_point(data=StationMeans, mapping=aes(x=Mindex, y=Tg, color = "currentMLR"), size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Mindex, y=Tg), color = 'black',alpha = 0.25)+
      geom_point(data=savedselect, mapping=aes(x=Mindex, y=Tg, color = "savedMLRA"), size=0.5)+
      #geom_density2d(data=savedselect, mapping=aes(x=Mindex, y=Tg),color = 'red',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("alpine" = "cyan",
                                             "cool" = "green",
                                             "mild" = "yellow",
                                             "warm" = "orange",
                                             "hot" = "red",
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
      scale_y_continuous(name= "Growing Season", breaks=c(6,12,18,24,30),
                      labels=c('alpine/arctic 6', 'cool 12', 'mild 18','warm 24','hot 30'))+
      scale_x_continuous(name= "P/PET Ratio", breaks=c(0, .1111, .2,0.3333,0.5,0.6667),
                         labels=c('perarid', 'arid 0.125', 'semiarid 0.25','subhumid 0.5','humid 1','perhumid 2'))+
      coord_fixed(ratio = 1/30,ylim = c(0,30), xlim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='right', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1), 
            axis.text.y = element_text(vjust = 0), 
            panel.grid.major = element_line(), panel.grid.minor = element_blank()) }
    else
    {climplot4 <- ggplot() +
      geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.2)+
      geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='cool'),alpha = 0.2)+
      geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='mild'),alpha = 0.2)+
      geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='warm'),alpha = 0.2)+
      geom_polygon(data=bw5, mapping=aes(x=x, y=y, fill='hot'),alpha = 0.2)+
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.1)+
      geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.1)+
      geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.1)+
      geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.1)+
      geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.1)+
      geom_point(data=StationMeans, mapping=aes(x=Mindex, y=Tg), color = 'black', size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Mindex, y=Tg),color = 'black',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("alpine" = "cyan",
                                             "cool" = "green",
                                             "mild" = "yellow",
                                             "warm" = "orange",
                                             "hot" = "red",
                                             "perarid" = "red",
                                             "arid" = "orange",
                                             "semiarid" = "yellow",
                                             "subhumid" = "green",
                                             "humid" = "cyan",
                                             "perhumid" = "blue"
      ))+
      scale_y_continuous(name= "Growing Season", breaks=c(6,12,18,24,30),
                      labels=c('alpine/arctic 6', 'cool 12', 'mild 18','warm 24','hot 30'))+
      scale_x_continuous(name= "P/PET Ratio", breaks=c(0, .1111, .2,0.3333,0.5,0.6667),
                         labels=c('perarid', 'arid 0.125', 'semiarid 0.25','subhumid 0.5','humid 1','perhumid 2'))+
      coord_fixed(ratio = 1/30,ylim = c(0,30), xlim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1), 
            axis.text.y = element_text(vjust = 0), 
            panel.grid.major = element_line(), panel.grid.minor = element_blank()) }
    
#climplot5 ----
    #surplus x deficit
    b1=data.frame(y=c(0,0,0.2,0.2), x=c(0,0.6,0.6,0))
    b2=data.frame(y=c(0,0,0.2,0.2), x=c(0.6,1,1,0.6))
    
    humidline =data.frame(y=c(0,1), x=c(0,1))
    b3=data.frame(y=c(0.2,0.2,1,1), x=c(0,0.6,0.6,0))
    b4=data.frame(y=c(0.2,0.2,1,1), x=c(0.6,1,1,0.6))
    if(input$saveselect == TRUE) #Decide whether to plot comparison graph.
    {climplot5 <- ggplot() +
      geom_polygon(data=b1, mapping=aes(x=x, y=y, fill='b'),alpha = 0.2)+
      geom_polygon(data=b2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.2)+
      geom_polygon(data=b3, mapping=aes(x=x, y=y, fill='d'),alpha = 0.2)+
      geom_polygon(data=b4, mapping=aes(x=x, y=y, fill='c'),alpha = 0.2)+
      geom_line(data=humidline, mapping=aes(x=x, y=y, fill='c'),color = 'black',alpha = 0.2)+
      geom_point(data=StationMeans, mapping=aes(x=Dindex, y=Sindex, color = "currentMLR"), size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Dindex, y=Sindex), color = 'black',alpha = 0.25)+
      geom_point(data=savedselect, mapping=aes(x=Dindex, y=Sindex, color = "savedMLRA"), size=0.5)+
      #geom_density2d(data=savedselect, mapping=aes(x=Dindex, y=Sindex),color = 'red',alpha = 0.25)+
      scale_fill_manual("Legend", values = c(
        "a" = "red",
        "b" = "yellow",
        "c" = "green",
        "d" = "blue"
        
      ),guide = 'none')+
      scale_color_manual(values=c("black", "red"), 
                         name="MLRA",
                         breaks=c("currentMLR", "savedMLRA"),
                         labels=c(currentMLR, savedMLRA))+
      scale_y_continuous(name= "Surplus", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75,0.8571),
                         labels=c('0', '25', '50','75','100','150','300','600'))+
      scale_x_continuous(name= "Deficit", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75,0.8571),
                         labels=c('0', '25', '50','75','100','150','300','600'))+
      coord_fixed(ratio = 1/1, ylim = c(0, 1), xlim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='right', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    }else 
    {climplot5 <- ggplot() +
      geom_polygon(data=b1, mapping=aes(x=x, y=y, fill='b'),alpha = 0.2)+
      geom_polygon(data=b2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.2)+
      geom_polygon(data=b3, mapping=aes(x=x, y=y, fill='d'),alpha = 0.2)+
      geom_polygon(data=b4, mapping=aes(x=x, y=y, fill='c'),alpha = 0.2)+
      geom_line(data=humidline, mapping=aes(x=x, y=y, fill='c'),color = 'black',alpha = 0.2)+
      geom_point(data=StationMeans, mapping=aes(x=Dindex, y=Sindex), color = 'black', size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Dindex, y=Sindex),color = 'black',alpha = 0.25)+
      scale_fill_manual("Legend", values = c(
        "a" = "red",
        "b" = "yellow",
        "c" = "green",
        "d" = "blue"
        
      ))+
      scale_y_continuous(name= "Surplus", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75,0.8571),
                         labels=c('0', '25', '50','75','100','150','300','600'))+
      scale_x_continuous(name= "Deficit", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75,0.8571),
                         labels=c('0', '25', '50','75','100','150','300','600'))+
      coord_fixed(ratio = 1/1, ylim = c(0, 1), xlim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    }
#climplot6 ----
    #growingseason x pAET
    bw1=data.frame(y=c(0,0,6,6), x=c(0,1,1,0))
    bw2=data.frame(y=c(6,6,12,12), x=c(0,1,1,0))
    bw3=data.frame(y=c(12,12,18,18), x=c(0,1,1,0))
    bw4=data.frame(y=c(18,18,24,24), x=c(0,1,1,0))
    bw5=data.frame(y=c(24,24,30,30), x=c(0,1,1,0))
    
    bmm1=data.frame(y=c(0,0,30,30), x=c(0,0.2,0.2,0))
    bmm2=data.frame(y=c(0,0,30,30), x=c(0.2,0.3333,0.3333,0.2))
    bmm3=data.frame(y=c(0,0,30,30), x=c(0.3333,0.4286,0.4286,0.333))
    bmm4=data.frame(y=c(0,0,30,30), x=c(0.4286,0.5,0.5,0.4286))
    bmm5=data.frame(y=c(0,0,30,30), x=c(0.5,0.6,0.6,0.5))
    bmm6=data.frame(y=c(0,0,30,30), x=c(0.6,0.75,0.75,0.6))
    if(input$saveselect == TRUE) #Decide whether to plot comparison graph.
    { climplot6 <- ggplot() +
      geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.2)+
      geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='cool'),alpha = 0.2)+
      geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='mild'),alpha = 0.2)+
      geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='warm'),alpha = 0.2)+
      geom_polygon(data=bw5, mapping=aes(x=x, y=y, fill='hot'),alpha = 0.2)+
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_point(data=StationMeans, mapping=aes(x=Aindex, y=Tg, color = "currentMLR"), size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Aindex, y=Tg), color = 'black',alpha = 0.25)+
      geom_point(data=savedselect, mapping=aes(x=Aindex, y=Tg, color = "savedMLRA"), size=0.5)+
      #geom_density2d(data=savedselect, mapping=aes(x=Aindex, y=Tg),color = 'red',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("alpine" = "cyan",
                                             "cool" = "green",
                                             "mild" = "yellow",
                                             "warm" = "orange",
                                             "hot" = "red",
                                             "a" = "blue",
                                             "b" = "yellow"
      ),guide = 'none')+
      scale_color_manual(values=c("black", "red"), 
                         name="MLRA",
                         breaks=c("currentMLR", "savedMLRA"),
                         labels=c(currentMLR, savedMLRA))+
      scale_y_continuous(name= "Growing Season", breaks=c(6,12,18,24,30),
                      labels=c('alpine/arctic 6', 'cool 12', 'mild 18','warm 24','hot 30'))+
      scale_x_continuous(name= "Peak Monthly Actual Evapotranspiration", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                         labels=c('0', '25', '50','75','100','150','300'))+
      coord_fixed(ratio = 1/30,ylim = c(0,30), xlim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='right', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    }else
    { climplot6 <- ggplot() +
      geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='alpine'),alpha = 0.2)+
      geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='cool'),alpha = 0.2)+
      geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='mild'),alpha = 0.2)+
      geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='warm'),alpha = 0.2)+
      geom_polygon(data=bw5, mapping=aes(x=x, y=y, fill='hot'),alpha = 0.2)+
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_point(data=StationMeans, mapping=aes(x=Aindex, y=Tg), color = 'black', size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Aindex, y=Tg),color = 'black',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("alpine" = "cyan",
                                             "cool" = "green",
                                             "mild" = "yellow",
                                             "warm" = "orange",
                                             "hot" = "red",
                                             "a" = "blue",
                                             "b" = "yellow"
      ))+
      scale_y_continuous(name= "Growing Season", breaks=c(6,12,18,24,30),
                      labels=c('alpine/arctic 6', 'cool 12', 'mild 18','warm 24','hot 30'))+
      scale_x_continuous(name= "Peak Monthly Actual Evapotranspiration", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                         labels=c('0', '25', '50','75','100','150','300'))+
      coord_fixed(ratio = 1/30,ylim = c(0,30), xlim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    }
    
#climplot7 ----
    #winter x pAET
    bw1=data.frame(x=c(-100,-100,-25,-25), y=c(0,1,1,0))
    bw2=data.frame(x=c(-25,-25,0,0), y=c(0,1,1,0))
    bw3=data.frame(x=c(0,0,15,15), y=c(0,1,1,0))
    bw4=data.frame(x=c(15,15,100,100), y=c(0,1,1,0))
    
    bmm1=data.frame(x=c(-100,-100,100,100), y=c(0,0.2,0.2,0))
    bmm2=data.frame(x=c(-100,-100,100,100), y=c(0.2,0.3333,0.3333,0.2))
    bmm3=data.frame(x=c(-100,-100,100,100), y=c(0.3333,0.4286,0.4286,0.333))
    bmm4=data.frame(x=c(-100,-100,100,100), y=c(0.4286,0.5,0.5,0.4286))
    bmm5=data.frame(x=c(-100,-100,100,100), y=c(0.5,0.6,0.6,0.5))
    bmm6=data.frame(x=c(-100,-100,100,100), y=c(0.6,1,1,0.6))
    if(input$saveselect == TRUE) #Decide whether to plot comparison graph.
    { climplot7 <- ggplot() +
      geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='polar'),alpha = 0.2)+
      geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.2)+
      geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.2)+
      geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.2)+
      
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_point(data=StationMeans, mapping=aes(x=Cindex, y=Aindex, color = "currentMLR"), size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Cindex, y=Aindex), color = 'black',alpha = 0.25)+
      geom_point(data=savedselect, mapping=aes(x=Cindex, y=Aindex, color = "savedMLRA"), size=0.5)+
      #geom_density2d(data=savedselect, mapping=aes(x=Cindex, y=Aindex),color = 'red',alpha = 0.25)+
      
      scale_fill_manual("Legend", values = c("polar" = "orange",
                                             "temperate" = "yellow",
                                             "mild" = "yellowgreen",
                                             "tropical" = "darkgreen",
                                             
                                             "a" = "blue",
                                             "b" = "yellow"
      ),guide = 'none')+
      scale_color_manual(values=c("black", "red"), 
                         name="MLRA",
                         breaks=c("currentMLR", "savedMLRA"),
                         labels=c(currentMLR, savedMLRA))+
      
      scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)", breaks=c(-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                         labels=c('-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                                  '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
      scale_y_continuous(name= "Peak Monthly Actual Evapotranspiration", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                         labels=c('0', '25', '50','75','100','150','300'))+
      coord_fixed(ratio = 30/1,xlim = c(-40,30), ylim = c(0,1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='right', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    } else
    { climplot7 <- ggplot() +
      geom_polygon(data=bw1, mapping=aes(x=x, y=y, fill='polar'),alpha = 0.2)+
      geom_polygon(data=bw2, mapping=aes(x=x, y=y, fill='temperate'),alpha = 0.2)+
      geom_polygon(data=bw3, mapping=aes(x=x, y=y, fill='subtropical'),alpha = 0.2)+
      geom_polygon(data=bw4, mapping=aes(x=x, y=y, fill='tropical'),alpha = 0.2)+
      
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm3, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm4, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm5, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_polygon(data=bmm6, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_point(data=StationMeans, mapping=aes(x=Cindex, y=Aindex), color = 'black', size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Cindex, y=Aindex),color = 'black',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("polar" = "orange",
                                             "temperate" = "yellow",
                                             "mild" = "yellowgreen",
                                             "tropical" = "darkgreen",
                                             
                                             "a" = "blue",
                                             "b" = "yellow"
      ))+
      scale_x_continuous(name= "Coldest Month (Annual Extreme Minimum)", breaks=c(-40, -35, -30, -25, -20,-15, -10,-5, 0,5, 10,15, 20,25,30),
                         labels=c('-40 (-55)', '-35 (-50)','-30 (-45)', '-25 (-40)','-20 (-35)','-15 (-30)','-10 (-25)',
                                  '-5 (-20)','0 (-15)','5 (-10)','10 (-5)','15 (0)','20 (5)','25 (10)','30 (15)'))+
      scale_y_continuous(name= "Peak Monthly Actual Evapotranspiration", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                         labels=c('0', '25', '50','75','100','150','300'))+
      coord_fixed(ratio = 30/1,xlim = c(-40,30), ylim = c(0,1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    }
#climplot8 ----
    #Moisture x pAET
    bmm1=data.frame(x=c(0,0,1,1), y=c(0,0.6,0.6,0))
    bmm2=data.frame(x=c(0,0,1,1), y=c(0.6,1,1,0.6))
    
    bm1=data.frame(y=c(0,0,1,1), x=c(0,0.1111,0.1111,0))
    bm2=data.frame(y=c(0,0,1,1), x=c(0.1111,0.2,0.2,0.1111))
    bm3=data.frame(y=c(0,0,1,1), x=c(0.2,0.3333,0.333,0.2))
    bm4=data.frame(y=c(0,0,1,1), x=c(0.3333,0.5,0.5,0.3333))
    bm5=data.frame(y=c(0,0,1,1), x=c(0.5,0.6667,0.6667,0.5))
    bm6=data.frame(y=c(0,0,1,1), x=c(0.6667,1,1,0.6667))
    if(input$saveselect == TRUE) #Decide whether to plot comparison graph.
    {climplot8 <- ggplot() +
      geom_polygon(data=bm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.2)+
      geom_polygon(data=bm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.2)+
      geom_polygon(data=bm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.2)+
      geom_polygon(data=bm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.2)+
      geom_polygon(data=bm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.2)+
      geom_polygon(data=bm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.2)+
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      geom_point(data=StationMeans, mapping=aes(x=Mindex, y=Dindex, color = "currentMLR"), size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Mindex, y=Dindex), color = 'black',alpha = 0.25)+
      geom_point(data=savedselect, mapping=aes(x=Mindex, y=Dindex, color = "savedMLRA"), size=0.5)+
      #geom_density2d(data=savedselect, mapping=aes(x=Mindex, y=Dindex),color = 'red',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("perarid" = "red",
                                             "arid" = "orange",
                                             "semiarid" = "yellow",
                                             "subhumid" = "green",
                                             "humid" = "cyan",
                                             "perhumid" = "blue",
                                             
                                             "a" = "yellow",
                                             "b" = "cyan"
      ),guide = 'none')+
      scale_color_manual(values=c("black", "red"), 
                         name="MLRA",
                         breaks=c("currentMLR", "savedMLRA"),
                         labels=c(currentMLR, savedMLRA))+
      
      scale_x_continuous(name= "P/PET Ratio", breaks=c(0, .1111, .2,0.3333,0.5,0.6667),
                         labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
      scale_y_continuous(name= "Deficit", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                         labels=c('0', '25', '50','75','100','150','300'))+
      coord_fixed(ratio = 1/1,xlim = c(0,1), ylim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='right', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    } else
    {climplot8 <- ggplot() +
      geom_polygon(data=bm1, mapping=aes(x=x, y=y, fill='perarid'),alpha = 0.2)+
      geom_polygon(data=bm2, mapping=aes(x=x, y=y, fill='arid'),alpha = 0.2)+
      geom_polygon(data=bm3, mapping=aes(x=x, y=y, fill='semiarid'),alpha = 0.2)+
      geom_polygon(data=bm4, mapping=aes(x=x, y=y, fill='subhumid'),alpha = 0.2)+
      geom_polygon(data=bm5, mapping=aes(x=x, y=y, fill='humid'),alpha = 0.2)+
      geom_polygon(data=bm6, mapping=aes(x=x, y=y, fill='perhumid'),alpha = 0.2)+
      geom_polygon(data=bmm1, mapping=aes(x=x, y=y, fill='a'),alpha = 0.1)+
      geom_polygon(data=bmm2, mapping=aes(x=x, y=y, fill='b'),alpha = 0.1)+
      
      geom_point(data=StationMeans, mapping=aes(x=Mindex, y=Dindex), color = 'black', size=0.5)+
      #geom_density2d(data=StationMeans, mapping=aes(x=Mindex, y=Dindex),color = 'black',alpha = 0.25)+
      scale_fill_manual("Legend", values = c("perarid" = "red",
                                             "arid" = "orange",
                                             "semiarid" = "yellow",
                                             "subhumid" = "green",
                                             "humid" = "cyan",
                                             "perhumid" = "blue",
                                             
                                             "a" = "yellow",
                                             "b" = "cyan"
      ))+
      scale_x_continuous(name= "P/PET Ratio", breaks=c(0, .1111, .2,0.3333,0.5,0.6667),
                         labels=c('perarid', 'arid', 'semiarid','subhumid','humid','perhumid'))+
      scale_y_continuous(name= "Deficit", breaks=c(0, 0.2, 0.3333,0.4286,0.5,0.6,0.75),
                         labels=c('0', '25', '50','75','100','150','300'))+
      coord_fixed(ratio = 1/1,xlim = c(0,1), ylim = c(0, 1))+
      
      labs(title = paste("Climate of ",StationMeans[1,]$SLabel, "", sep=""))+
      theme_bw()+
      theme(legend.position='none', axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0), 
            axis.text.y = element_text(vjust = 0),panel.grid.major = element_line(), panel.grid.minor = element_blank()) 
    } 
#climmap ----
    graphymax = input$lat[2]#max(selectClim$Latitude)+10# 
    graphymin = input$lat[1]#min(selectClim$Latitude)-10# 
    graphxmax = input$lon[2]#max(selectClim$Longitude)+10# 
    graphxmin = input$lon[1]#min(selectClim$Longitude)-10# 
    ocean = data.frame(y=c(-90,-90,90,90), x=c(-180,180,180,-180))
    
    climmap <-  ggplot() +
      geom_polygon(data = ocean, 
                   aes(x = x, y = y),
                   color = 'darkgray', fill = 'lightcyan', size = .2)+
      geom_sf(data = states,
              color = 'darkgray', fill = 'lightyellow', size = .2)+
      geom_sf(data = lakes,
              color = 'darkgray', fill = 'lightcyan', size = .2)+
      geom_point(data=climtab, mapping=aes(x=Lon, y=Lat), color = 'red', size=1)+
      
      coord_sf(xlim = c(graphxmin,graphxmax), ylim = c(graphymin,graphymax)) + theme_void() 
#endplots ----
    if(input$RadioGraphtype == 1){climplot} 
    else if(input$RadioGraphtype == 2) {climplot2}
    else if(input$RadioGraphtype == 3) {climplot3}
    else if(input$RadioGraphtype == 4) {climplot4}
    else if(input$RadioGraphtype == 5) {climplot5}
    else if(input$RadioGraphtype == 6) {climplot6}
    else if(input$RadioGraphtype == 7) {climplot7}
    else if(input$RadioGraphtype == 8) {climplot8}
    else if(input$RadioGraphtype == 9) {climmap}
    else{plot(climelev)}
    

  
    
      # plot(t.mean~t.max, clim.tab, main=paste0('Lat:',station$NAME))
    
  })
  output$Climtext = renderText({ 
    rv$my_text2
  })
  
  output$Climtab = renderTable({ 
    rv$my_table
  })
})
