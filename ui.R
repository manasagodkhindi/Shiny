library(shinydashboard)

shinyUI(
         dashboardPage(
                     dashboardHeader(title = "WildFire Analysis: California"),
                      dashboardSidebar(
                      sidebarMenu(
                      menuItem("Dashboard", tabName = "overviewmap", icon = icon("map")),  
                      menuItem("Map", tabName = "map", icon = icon("map")),
                      menuItem("Spread", tabName = "Area", icon = icon("line-chart")),
                      menuItem("Wildfires & Climate", tabName = "factors", icon = icon                           ('lines'),
                               menuItem('Climate',
                                        tabName = 'clmt',
                                        icon = icon("line-chart"),
                                        menuSubItem('Precipitation',
                                                    tabName = 'ppt',
                                                    icon = icon("line-chart")),
                                        menuSubItem('Temperature & VPD',
                                                    tabName = 'temp',
                                                    icon = icon("line-chart"))        
                                        )),
                           
                             
                      
                      selectInput(inputId="county",
                                  label="County", 
                                  choices=unique(california_fires$FIPS_NAME)),
                      sliderInput("year", label =("Year Range"), 
                                  min = lubridate::year("2005-01-01"),
                                  max = lubridate::year("2015-01-01"), 
                                  value = c(year("2011-01-01"),year("2015-01-01")),
                                  sep = '',ticks=FALSE
            ))
    
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "overviewmap",
              
              fluidRow(box(leafletOutput("MapOverview"), height = 500),
                       box(plotlyOutput("Acerage_Burnt_Static"), height = 500)
                      )),
      tabItem(tabName = "map",
              fluidRow(box(leafletOutput("RegionalMap"), height = 420),
                       box(plotOutput("IncidentsByYear"),height = 420)),
              fluidRow(box(plotOutput("Factors"), height = 420),
                       box(dygraphOutput("TimeSeries"),height = 420))),
      tabItem(tabName="ppt",
              fluidRow(box(plotlyOutput("Areappt"),height = 400),
                       box(htmlOutput("Countppt"), height = 400))),
      tabItem(tabName="temp",  
               fluidRow(box(dygraphOutput("AreaVPD"), height = 400),
                       box(dygraphOutput("AreaTemp"), height = 400))),
      tabItem(tabName="Area",
              fluidRow(box(plotlyOutput("CauseProportion"), height = 400),
                       box(plotlyOutput("Acerage_burnt"), height = 400),
                       box(plotOutput("ts"),height = 420)))
                       
             
     )
   )
  )
)



