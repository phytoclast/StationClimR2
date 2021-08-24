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
    
    selectInput(inputId = "station", #name of input
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
    station
  })
  

  
})
