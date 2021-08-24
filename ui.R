#
library(shiny)
library(ggplot2)
library(plyr)

fluidPage(
  
  headerPanel('Station Climate Browser'),
  sidebarPanel(htmlOutput("stationselect"),
    htmlOutput("country"),
    htmlOutput("elev"),
    htmlOutput("lat"),
    htmlOutput("lon")


  ),
  mainPanel(
    plotOutput("climplot"),
    verbatimTextOutput('Climtext'),
    fluidRow(
  
      column(width = 2,
             radioButtons("RadioUnits", label = ("Select Units"),
                          choices = list('Metric System' = 'm', 
                                         'Medieval Units' = 'USC'), 
                          selected = 'm')
      ),
      column(width = 2,
             checkboxInput("saveselect", label = "Compare Station", value = FALSE)
      ),
    column(width = 5,
           radioButtons("RadioGraphtype",inline = T,  label = ("Select Graph"),
                        choiceNames = list(HTML("<font size=-2>Monthly"), 
                                           HTML("Summer × Winter"),
                                           HTML("Summer × Moisture"), 
                                           HTML("Surplus × Deficit"),
                                           HTML("Summer × pAET"),  
                                           HTML("Winter × pAET"),
                                           HTML("Moisture × Deficit"),
                                           HTML("Moisture × Seasonality"),
                                           HTML("Map"),
                                           HTML("Temperature × Elevation</font>")),
                        
                        choiceValues = list(1,2,4,5,6,7,8,3,9,10),
                        selected = 1),
           
    HTML("</font>")
    )),
  fluidRow(
      HTML("<font size=-2>Based on 1981-2010 Climatic Normals. Error bars on temperature and precipitation are the 20th and 80th percentiles for the 30 years of data for a single station. Other graphs show each year as individual points. Click here for more information about the "),
                  tags$a(href="https://phytoclast.github.io/ClimateClassification/", "climate classification"),
                  HTML(" used above.</font>")
           )

  )
)
