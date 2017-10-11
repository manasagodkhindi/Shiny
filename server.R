library(leaflet)
library(plotly)
library(dygraphs)
library(googleVis)
library(RColorBrewer)
library(ggthemes)


function(input,output,session){
  #connect=dbConnector(session,dbname)
  
  fires_overview= reactive({
    if (!is.null(input$year))
      california_fires %>% 
      filter(
             FIRE_YEAR>=input$year[1]&
               FIRE_YEAR<=input$year[2])
  })
  
  
 
  fires_subset= reactive({
                         if (!is.null(input$county)& !is.null(input$year))
                           california_fires %>% 
                           filter(FIPS_NAME==input$county &
                                  FIRE_YEAR>=input$year[1]&
                                   FIRE_YEAR<=input$year[2])
                          })
  
  Acres_burned_byyear=reactive ({fires_subset() %>% group_by(FIRE_YEAR) %>% 
    summarise(Area_burnt=sum(FIRE_SIZE), 
              ppt_year=mean(ppt,na.rm=TRUE),
              FireCount=n(),
              Temp_year=mean(tmean,na.rm=TRUE),
              vpd_year=mean(vpdmean,na.rm=TRUE))
  })
 # observe({print (fires_subset())})
  
  
  output$MapOverview= renderLeaflet(
    {
      leaflet(fires_overview())%>% 
        addTiles() %>% 
        addCircles(weight=1,radius=~fires_subset()$FIRE_SIZE *0.15, color="red") %>%
        #addMarkers(icon=FireIcon) %>%
        addPolygons(data=mapStates,
                    fillColor = heat.colors(3, alpha = NULL),
                    stroke = FALSE)
        
        #addProviderTiles("Esri.WorldStreetMap")
      
      
    })
  
  
  
  
  
  output$RegionalMap= renderLeaflet(
  {
     leaflet(fires_subset())%>% 
     addTiles() %>% 
     #addMarkers(icon=FireIcon) %>%
     addCircles(weight=1,radius=~fires_subset()$FIRE_SIZE *0.15, color="red") %>% 
     addProviderTiles("Esri.WorldStreetMap")
   
  
  })
 
    
  
  output$IncidentsByYear=renderPlot(
          {
            by_year=fires_subset() %>% group_by(FIRE_YEAR,Month_Name) %>% 
                    summarize(Incident_Count=n())
            
            ggplot(by_year,aes(x=Month_Name,y=Incident_Count))+
            geom_col(fill='darkred')+ theme_classic()+ggtitle("Wildfire Incidents By Month")+
            xlab("Year")+
            ylab("Number Of WildFires")+
            theme(plot.title = element_text(hjust = 0.5))
          }
        )
  
       output$Factors=renderPlot({
             by_factor= fires_subset()%>% group_by(STAT_CAUSE_DESCR) %>% 
               summarize(count_cause=n())
             
             ggplot(by_factor,aes(x=STAT_CAUSE_DESCR,y=count_cause))+
                     geom_col(aes(fill=STAT_CAUSE_DESCR))+
                     coord_polar()+theme_calc()+
                     scale_fill_brewer(palette="Set3")+
                     ggtitle("WildFire Number By Cause")+
                     xlab("")+
                     ylab("")+
                     theme(plot.title = element_text(hjust = 0.5,
                                                     size=14,family='serif'),
                           axis.text.y = element_blank())+
                     labs(fill="Causes")
           }
          )

       output$TimeSeries=renderDygraph({
              by_year=fires_subset() %>% group_by(FIRE_YEAR) %>% 
                summarize(Incident_Count=n())      
               dygraph(by_year,
                       main="Number of Wildfire By Year",
                       ylab="Number Of Wildfires",
                       xlab="Year")
            
       }
       )
       output$Acerage_burnt=renderPlotly({
             #fires_iso=isolate({()})
         
           Acerage_burnt= california_fires %>% group_by(FIRE_YEAR) %>% 
           summarise(Percentage=sum(FIRE_SIZE)/Total_FireArea*100)
              
              plot_ly(Acerage_burnt, x = ~FIRE_YEAR, 
                      y = ~Percentage, text = ~paste(Percentage, ' Area %'),
                      type ='scatter', mode = 'markers', 
                      color = ~Percentage, colors = 'Reds',height=375,
                      marker = list(size = ~Percentage*2, opacity = 0.5)) %>%
                      layout(title = 'Acerage Burnt Over the Years',
                      xaxis = list(showgrid = TRUE, title='Year'),
                      yaxis = list(showgrid = TRUE, title='Area Spread (percent)'))
              
       })
       output$Areappt=renderGvis({
         gvisLineChart(Acres_burned_byyear(), "FIRE_YEAR", c("ppt_year","Area_burnt"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                         {targetAxisIndex:1}]",
              vAxes="[{title:'Precipitation(inches)'},{title:'Area Affected(Acres)'}]",
              hAxes = "[{format:'####'}]",
              title="Influence of Precipitation on Area of WildFire Spread",
              height =375
                       ))
         
       }
       )
       
       output$Areappt=renderPlotly({
         
         plot_ly(Acres_burned_byyear()) %>%
           add_trace(x = ~FIRE_YEAR, y = ~Area_burnt, type = 'bar', name = 'Area',
                     marker = list(color = '#C9EFF9'),
                     hoverinfo = "text",
                     text = ~paste(Area_burnt, ' Acres')) %>%
           add_trace(x = ~FIRE_YEAR, y = ~ppt_year, type = 'scatter', mode = 'lines',                       name = 'Precipitation', yaxis = 'y2',
                     line = list(color = '#45171D'),
                     hoverinfo = "text",
                     text = ~paste(ppt_year, ' inches')) %>%
           layout(title = 'Influence of Precipittion on Spread of Wildfire',
                  xaxis = list(title = ""),
                  yaxis = list(side = 'left', title = 'Area (Acres)', showgrid = FALSE,                    zeroline = FALSE),
                  yaxis2 = list(side = 'right', overlaying = "y", title = 'Precipitation                  (inches)', showgrid = FALSE, zeroline = FALSE
                  ),height=375)
         
         
       }
      )
       output$AreaVPD=renderDygraph({
         dygraph(Acres_burned_byyear()[,c(1,2,6)], 
                  main = "Influence of VPD in Spread Of WildFires")%>% 
                  dyAxis("x",drawGrid=FALSE) %>%
                  dyAxis("y", label = "Area (Acres)")%>%
                  dyAxis("y2", label = "VPD ") %>%
                  dySeries("vpd_year", axis=('y2')) 
                       
       })
       
       output$AreaTemp=renderDygraph({
         dygraph(Acres_burned_byyear()[,c(1,2,5)], 
            main = "Influence of Temperature in spread Of Wildfires")%>%
           dyAxis("x",drawGrid=FALSE)%>%
           dyAxis("y", label = "Area (Acres)")%>%
           dyAxis("y2", label = "Temperature (F) ") %>%
           dySeries("Temp_year", axis=('y2')) %>%
           dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
       })
         
       output$Countppt=renderGvis({
             gvisLineChart(
             Acres_burned_byyear(), "FIRE_YEAR", c("FireCount","ppt_year"),
            options=list(
             series="[{color:'red' ,labelInLegend:'wildfires',targetAxisIndex: 0},
             {color: 'green',labelInLegend:'Area',targetAxisIndex:1}]",
             vAxes="[{title:'Wildfire Number'}, {title:'Precipitation'}]",
             title="Influence Of Precipitation in Number of Wildfires",
             height=375
           ))
       })
       
       output$ts=renderPlot({
            plot(Fires_ts,main=" Overview of Spread Influencing Factors",xlab="Year")
       })
       
       output$CauseProportion=renderPlotly({
         Size_byCause=fires_subset() %>% group_by(STAT_CAUSE_DESCR) %>% summarise                           (Percent=sum(FIRE_SIZE)/Total_FireArea*100)
         
         
         plot_ly( Size_byCause,labels =~STAT_CAUSE_DESCR , values = ~Percent ,
                  marker = list(colors=brewer.pal(12,"Pastel2"))) %>%
                  add_pie(hole = 0.6) %>%
                  layout(title = "Area Affected And Cause",  showlegend = T,          
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels =                       FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels =                        FALSE),
                  height=375)
       })
       output$Acerage_Burnt_Static=renderPlotly({
         #fires_iso=isolate({()})
         
         plot_ly(Acerage_burnt, x = ~FIRE_YEAR, 
                 y = ~Percentage, text = ~paste(Percentage, ' Area %'),
                 type ='scatter', mode = 'markers', 
                 color = ~Percentage, colors = 'Reds',height=375,
                 marker = list(size = ~Percentage*2, opacity = 0.5)) %>%
           layout(title = 'Acerage Burnt Over the Years',
                  xaxis = list(showgrid = TRUE, title='Year'),
                  yaxis = list(showgrid = TRUE, title='Area Spread (percent)'))
         
       })
       
       output$CauseProportion_Static=renderPlotly({
         plot_ly( Size_byCause,labels =~STAT_CAUSE_DESCR , values = ~Percent ,
                  marker = list(colors=brewer.pal(12,"Pastel2"))) %>%
           add_pie(hole = 0.6) %>%
           layout(title = "Area Affected And Cause",  showlegend = T,          
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels =                       FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels =                        FALSE),
                  height=375)
       })
         
  
}
