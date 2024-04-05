#
library(shiny)
library(ggplot2)
library(dplyr)

fluidPage(
  
  headerPanel('Station Climate Browser'),
  sidebarPanel(htmlOutput("stationselect"),
    htmlOutput("country"),
    htmlOutput("setelev"),
    htmlOutput("lat"),
    htmlOutput("lon"),
    checkboxInput("saveselect", label = "Compare Station", value = FALSE),
    radioButtons("timeperiod", label = ("Time Period"),
                 choices = list('1961-1990 Normals' = '1990', 
                                '1981-2010 Normals' = '2010', 
                 '+2C Projections' = '2080'), 
                 selected = '1990')
    


  ),
  mainPanel(
    fluidRow(
      tabsetPanel(
      tabPanel('Plot',plotOutput("climplot")),
      # tabPanel('Text',verbatimTextOutput('Climtext')),
      tabPanel('Table',tableOutput('Climtab'))
      )),
    fluidRow(
      verbatimTextOutput('Climtext')),
      fluidRow(
      column(width = 2,
             radioButtons("RadioUnits", label = ("Select Units"),
                          choices = list('Metric System' = 'm', 
                                         'Medieval Units' = 'USC'), 
                          selected = 'm'),
      
      
    ),
      
    column(width = 5,
           radioButtons("RadioPET", label = ("PET Method"),
                        choiceNames = list(HTML("<font size=-3>Default"), 
                                           
                                           HTML('Thornthwaite'),
                                           HTML('Holdridge'),
                                           HTML('Priestley-Taylor'),
                                           HTML('Penman-Monteith'),
                                           HTML('Hargreaves Samani'),
                                           HTML('Turc'),
                                           HTML('Hamon')
                        ), 
                        choiceValues = list('gs2','tw','ho','pt','pm','hs','tc','hm'),
                        selected = 'gs2'),
           HTML("</font>")
               ),
    column(width = 5,
           radioButtons("RadioGraphtype",inline = T,  label = ("Select Graph"),
                        choiceNames = list(HTML("<font size=-3>Monthly"), 
                                           HTML("Summer × Winter"),
                                           HTML("Summer × Moisture"), 
                                           # HTML("Surplus × Deficit"),
                                           # HTML("Summer × pAET"),  
                                           # HTML("Winter × pAET"),
                                           # HTML("Moisture × Deficit"),
                                           # HTML("Moisture × Seasonality"),
                                           HTML("Map</font>")),
                        
                        # choiceValues = list(1,2,4,5,6,7,8,3,9),
                        choiceValues = list(1,2,4,9),
                        selected = 1),
           
           HTML("</font>")
    )),
    fluidRow(
      HTML("<font size=-2>Based on Gridded 1961-1990 Climatic Normals and estimated 2080 projections (WorldClim2). Error bars on temperature and precipitation are the 20th and 80th percentiles relative to annual variability in 1981-2010 data station records. Other graphs show each year as individual points. Click here for more information about the "),
                  tags$a(href="https://phytoclast.github.io/ClimateClassification/", "climate classification"),
                  HTML(" used above. t = mean temperature (C or F); th = daily high temperature; tl = daily low temperature; p = monthly precipitation (mm or inch); e = potential evapotranspiration; Dl = daylength (hours); Ra = solar radiation at top of atmosphere (MJ/m2/hr); Rn = net radiation reaching earth surface.</font>")
           )

  )
)
