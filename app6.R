library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(maptools)
library(htmlwidgets)
library(sf)
library(sp)
library(tmap)
library(dplyr)

shp1 <- st_read("shp/dafor.shp") %>%
  mutate(data = as.Date(data))

shp2 <- st_read("shp/geomorfologia.shp") %>%
  mutate(data = as.Date(data))


#Create the UI
ui <- dashboardPage(
  dashboardHeader(title = "Sun Coral Monitoring"),
  dashboardSidebar(
    dateRangeInput("data", "Selecione intervalo de data: ", 
                   format = "yyyy-mm-dd",
                   start = "2022-06-15",
                   end =  "2023-12-31",
                   separator = " até "),
    
    
    
    
    checkboxGroupInput("layer", label = h4("Layer"), choices = c("Shapefile 1", "Shapefile 2"),  selected = "Shapefile 1" )
  ),
  dashboardBody(
    fluidRow(
      leafletOutput("map", width = "100%", height = "100%")
    )
  )
)

#Create the Server
server <- function(input, output) {
  
  shp1_filtered <- reactive({
    shp1 %>%
      filter(data == as.Date(input$data, "%Y-%m-%d"))
      
  })
  
  shp2_filtered <- reactive({
    shp2 %>%
      filter(data == as.Date(input$data, "%Y-%m-%d" ))
     
  })
  
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      setView(-48.38, -27.28, zoom = 10) %>% 
      addProviderTiles("CartoDB.Positron")
    
    if ("Shapefile 1" %in% input$layer) {
      map_shp1 <- tm_shape(shp1_filtered()) +
        tm_borders(col = "red", lwd = 0.5) 
     
      m <- addPolylines(m, data = shp1_filtered())
     
    }
    
    if ("Shapefile 2" %in% input$layer) {
      map_shp2 <- tm_shape(shp2_filtered()) +
        tm_borders(col = "blue", lwd = 0.5) 
     
      m <- addPolylines(m, data = shp2_filtered())

      
     
    }
  })
}

#Run the shiny app
shinyApp(ui, server)