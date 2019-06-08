library(magrittr)
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(evaluate)
library(ggmap)
library(rgdal)
library(tmap)
library(tmaptools)
library(sf)
library(geojsonio)
library(sqldf)
library(DBI)
library(gsubfn)
library(RH2)
library(RSQLite)
library(rJava)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- dashboardPage(
   # Application title
   dashboardHeader(title="Vic Car Accidents"),
   dashboardSidebar(
     sliderInput("range", "Year:",
                 min = 2013, max = 2018,
                 value = c(2013,2018)),
   actionButton("change", "Change")),
   
   dashboardBody(
            box(title = "Choropleth Map",width = 12, status = "primary",leafletOutput("young_driver",width = "100%", height = 400)
                ),
            box(tableOutput("values"))
   ))


# Define server logic required to draw a histogram
options(scipen = 999)
#read LGA geojson file from local file
LGA<-st_read("Data/LGA.geojson",stringsAsFactors = FALSE)
#read car crashes data from local file
carCrashes<- read_csv("Data/Car.csv")

server <- shinyServer(function(input, output, session) {
  sliderValues <- reactive({
    data.frame(name = "range", value= (paste(input$range[1],input$range[2])))})
  output$values <- renderTable({
    sliderValues()
  })
  
  cmap <- observeEvent(input$change,{
    range_1 <- as.numberic(input$range[1])
    range_2 <- as.numeric(input$range[2])
    temp<- sqldf(
    "Data/Car.csv",
    sql = paste("select distinct LGA_NAME, count(LGA_NAME) as 'number of young driver' 
    from file
    where driver_type = 'young driver' and
    year_period between range_1 and range_2
    group by LGA_NAME; "))
    
    sub_and_car <- left_join(LGA,temp,by = c("VIC_LGA__3" = "LGA_NAME"))
    output$young_driver <- renderLeaflet({
      tm <- tm_shape(sub_and_car)+tm_polygons(col="number of young driver", border.col="grey")
      tmap_leaflet(tm)
    })
    })
   })


# Run the application 
shinyApp(ui = ui, server = server)

