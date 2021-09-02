#
#
library(shiny)
library(ggplot2)
library(plyr)


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
    cf <- 1#0.92/1.26 #Correction factor to make for forest and mixed landuse vegetation instead of short grass, based on alpha of Priestly-Taylor equation

    climtab$e.tw = 16*(10*pmax(climtab$t,0)/I)^a*(climtab$Dl/12)*(Days[climtab$Mon]/30)#Thornthwaite

    climtab$e.ho <- 58.93/365*pmax(0, climtab$t)*Days[climtab$Mon]#Holdridge

    climtab$e.gs <- 0.008404*216.7*exp(17.26939*climtab$t/
                                         (climtab$t+237.3))/(climtab$t+273.3)*(climtab$Ra)*Days[climtab$Mon]*abs((climtab$th - climtab$tl))^0.5 + 0.001#Schmidt

    climtab$e.pt <- cf* 1.26 * (climtab$delta / (climtab$delta + gamma))*pmax(0,(climtab$Rn-climtab$Gi))/climtab$lambda*Days[climtab$Mon] #Priestley-Taylor

    climtab$e.pm <- cf* (0.408*climtab$delta*pmax(0,(climtab$Rn-climtab$Gi))+gamma*900/(climtab$t+273)*2*(climtab$Vp-climtab$Vpmin))/(climtab$delta+gamma*(1+0.34*2))*Days[climtab$Mon] #Penman-Monteith

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


    climplot <- ggplot(climtab, aes(x=Mon)) +
      geom_bar(stat="identity",aes(fill="Precipitation", y=p/5), alpha = 0.85,  color="blue") +
      geom_bar(stat="identity", aes(fill='PET', y=e/5), alpha = 0.60,  color="red" ) +
      geom_line(stat="identity",  aes(color= "Temperature", y=t), alpha = 1) +
      geom_point(aes(shape='Mean', y=t), color="red") +
      geom_point(aes(shape='Low', y=tl), color="red") +
      geom_point(aes(shape='High', y=th), color="red") +
      #geom_errorbar(aes(ymin=p25/5, ymax=p75/5), width=.2,position=position_dodge(-0.9), color="blue") +
      #geom_errorbar(aes(ymin=t25, ymax=t75), width=.2,position=position_dodge(0.9), color="red") +

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
      labs(title = paste("Climate of ",station$Station_Name, ": ", sep=""))# ,  subtitle = my_text1)

    climplot
      # plot(t.mean~t.max, clim.tab, main=paste0('Lat:',station$NAME))
    
  })
  output$Climtext = renderText({ 
    rv$my_text2
  })
})
