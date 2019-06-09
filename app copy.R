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
library(shinycssloaders)
library(shinyWidgets)

# Define server logic required to draw a histogram
options(scipen = 999)
#read LGA geojson file from local file
LGA<-st_read("Data/LGA.geojson",stringsAsFactors = FALSE)
#read car crashes data from local file
carCrashes<- read_csv("Data/Car.csv")

LGA<- select(LGA,-c(LG_PLY_PID,DT_CREATE,LGA_PID,VIC_LGA_sh,VIC_LGA__2,VIC_LGA__5,rmapshaperid))

carCrashes<- select(carCrashes,-c(accident_date,RMA,region_name,old_driver,YOUNG_DRIVER))

df <- left_join(LGA,carCrashes,by = c("VIC_LGA__3" = "LGA_NAME"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  # Application title
  dashboardHeader(title="Vic Car Accidents"),
  dashboardSidebar(
    sliderInput("range", "Year:",
                min = 2013, max = 2018,
                value = c(2013,2018)),
    pickerInput("type","Driver Types:", 
                choices = unique(df$driver_type), 
                selected = unique(df$driver_type), 
                options = list(`actions-box` = TRUE),
                multiple = T)
    ),
  
  dashboardBody(
    box(title = "Choropleth Map",
        width = 12, 
        status = "primary",
        withSpinner(leafletOutput("map",width = "100%", height = 400))
    ),
    box(dataTableOutput("table")
  )))




server <- shinyServer(function(input, output, session) {
  sliderValues <- reactive({
    data.frame(name = "range", value= (paste(input$range[1],input$range[2])))})
  output$values <- renderTable({
    sliderValues()
  })

  
  cmap <- reactive({
    range_1 <- as.numeric(input$range[1])
    range_2 <- as.numeric(input$range[2])
    type <-data.frame(value = as.character(paste(input$type)))
    
    temp <- df%>%
      filter(driver_type %in% type & 
               year_period >= range_1 & year_period <= range_2) %>%
      group_by(VIC_LGA__3) %>%
      mutate(Number_of_Drivers = n())
    
    temp<- select (temp, c(VIC_LGA__3,geometry,Number_of_Drivers))
    
    temp<- temp[!duplicated(temp$VIC_LGA__3), ]
    
  })
  
  output$map <- renderLeaflet({
    tmap_mode("view")
    tm <- tm_shape(cmap())+
      tm_polygons(col="Number_of_Drivers", 
                  border.col="grey",
                  convert2density = TRUE,
                  breaks=c(0.01,0.02,0.05, 0.1,1,2,5,10,15,Inf),
                  title = "Number of accidents per KM2")+
      tm_layout(frame = F, title = "Car Accident Density", 
                title.size = 2, 
                #title.position = c(0.55, "top"), 
                legend.hist.size = 0.5, 
                legend.outside = T) 
    tmap_leaflet(tm)
  })
  

    output$table = renderDataTable({
      cmap()
    }, options = list(aLengthMenu = c(12, 20, 40), iDisplayLength = 6))
  })



# Run the application 
shinyApp(ui = ui, server = server)