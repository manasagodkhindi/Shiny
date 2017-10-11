library(shiny)
library(RSQLite)
library(dygraphs)
library(leaflet)
library(plotly)
library(data.table)
library(dplyr)
library(lubridate)
library(maps)

source("./helper.R")

dbname="FPA_FOD_20170508.sqlite"
tablename="California_WildFires"



connect=dbConnector(dbname)

california_fires=dbGetTable(conn=connect,
                            tblname=tablename
                            )
mapStates = map("state", fill = TRUE,
                 plot = FALSE,
                 region = c('california'))



california_fires= california_fires %>% 
                  select(-c(MTBS_ID,MTBS_FIRE_NAME,COMPLEX_NAME,FOD_ID,FPA_ID,Shape))

Total_FireArea= sum(california_fires$FIRE_SIZE)

remove_NA=function(df, column){
  return (df[complete.cases(df[,column]),])
}
#remove_NA(california_fires,c("FIPS_NAME","DISCOVERY_TIME"))

california_fires=california_fires[-which(is.na(california_fires$DISCOVERY_TIME)), ]
california_fires=california_fires[-which(is.na(california_fires$FIPS_NAME)), ]

california_fires$Discovery_Date_Conv=as.POSIXct((california_fires$DISCOVERY_DATE-2440587.5)*86400,origin="1970-01-01",tz="GMT")

california_fires$Contained_Date_Conv=as.POSIXct((california_fires$CONT_DATE-2440587.5)*86400,origin="1970-01-01",tz="GMT")

california_fires= california_fires%>% 
                   mutate(Discovery_Month=lubridate::month(Discovery_Date_Conv)
                   ,Contained_Month=month(Contained_Date_Conv)
                   ,Month_Name=lubridate::month(Discovery_Date_Conv,label=TRUE))

Size_byCause= california_fires %>% group_by(STAT_CAUSE_DESCR) %>% summarise(Percent=sum               (FIRE_SIZE)/Total_FireArea*100)

Acerage_burnt=california_fires %>% group_by(FIRE_YEAR) %>% 
  summarise(Percentage=sum(FIRE_SIZE)/Total_FireArea*100)

climate=read.csv("California_weather.csv",
                            stringsAsFactors = FALSE,
                            header=TRUE,sep=",")

climate$Date=(as.POSIXct(paste0(climate$Date, "-01")))

climate= climate %>% mutate(vpdmean=(vpdmin+vpdmax)/2, 
                           Month=month(Date),
                           Year=year(Date))

california_fires=california_fires %>% left_join(climate,by=c("FIPS_NAME"="County","Discovery_Month"="Month","FIRE_YEAR"="Year"))



Area_burned_byMonth= california_fires %>% group_by(Discovery_Month,FIRE_YEAR) %>% 
                      summarise(Area=sum(FIRE_SIZE), 
                                precipitation=mean(ppt,na.rm=TRUE),
                                WildFires=n(),
                                Temperature=mean(tmean,na.rm=TRUE),
                                vpd=mean(vpdmean,na.rm=TRUE))


Fires_ts=Area_burned_byMonth[,-c(1,2)]
Fires_ts=ts( Fires_ts,start=c(2005,1),frequency=12)




