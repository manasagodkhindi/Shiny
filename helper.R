library(RSQLite)
library(dplyr)
library(data.table)

dbConnector <- function( dbname) {
  require(RSQLite)
  ## setup connection to database
  conn <- dbConnect(drv = SQLite(), 
                    dbname = dbname)
  ## disconnect database when session ends
  #session$onSessionEnded(function() {
  #  dbDisconnect(conn)
  #})
  ## return connection
  conn
}

dbGetData <- function(conn, tblname,county,min,max) {
  query <- paste0("SELECT * FROM ",
                 tblname,
                 " WHERE FIRE_YEAR BETWEEN ",
                 as.character(min),
                 " and ", as.character(max),
                " and FIPS_NAME ='",
                 as.character(county),
                 "'"
                 )
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}

dbGetTable <- function(conn, tblname,county,min,max) {
  query <- paste0("SELECT * FROM ",
                  tblname,
                  " WHERE FIRE_YEAR BETWEEN 2006 ",
                  " and  2016"
                  
  )
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}


FireIcon = makeIcon("Fire.png", "Fire@2x.png", 18, 18)

