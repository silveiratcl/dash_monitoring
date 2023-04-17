library(shiny) 
library(leaflet) 
library(dplyr) 
library(rgdal) 
library(lubridate) 
library(sf)


# Load the shapefiles 
data1 <- st_read(dsn = "shp/dafor.shp") 
data2 <- st_read(dsn = "shp/geomorfologia.shp") 

ui <- fluidPage( 
  titlePanel("PACS MONITORING"), 
  sidebarLayout( 
    sidebarPanel( 
      checkboxGroupInput("shapes", "Select Shapes", choices = c("Shapefile1", "Shapefile2")), 
      dateRangeInput("data", "Select Dates"), 
      width = 3), 
    mainPanel( 
      leafletOutput("map", width = "100%"), #leafletOutput("map", width = "100%", height = "100%")
      width = 9) 
  ) 
) 

# Filter data by date range 
observe({ 
  if(!is.null(input$data)){ 
    data1 <- data1 %>% filter(as.Date(data,format="%Y-%m-%d")>=input$data[1] & as.Date(data,format="%Y-%m-%d")<=input$data[2]) 
    data2 <- data2 %>% filter(as.Date(data,format="%Y-%m-%d")>=input$data[1] & as.Date(data,format="%Y-%m-%d")<=input$data[2]) 
  } 
}) 

# Add the shapefiles to the map 
observe({ 
  if("Shapefile1" %in% input$shapes){ 
    m<-m %>% addPolygons(data=data1) 
  } 
  if("Shapefile2" %in% input$shapes){ 
    m<-m %>% addPolygons(data=data2) 
  } 
}) 

server <- function(input,output){ 
  # Create the Leaflet Map 
  output$map<- renderLeaflet({ 
    m<-leaflet()%>%addTiles()%>% setView(-48.38, -27.28, zoom = 10) 
    m 
  }) 
} 

shinyApp(ui = ui, server = server)
