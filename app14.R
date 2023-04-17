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


# Import shapefiles data
dafor_shp <- st_read("shp/dafor.shp")
geo_shp <-st_read("shp/geomorfologia.shp")

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







#https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/


# Define Server
server <- function(input, output) {
  
  
  
  
  # Define reactive data
  reactiveData <- eventReactive(input$data, {
    # Filter shapefiles data based on selected date range
    filtered_dafor <-  dafor_shp[dafor_shp$data >= input$data[1] & dafor_shp$data <= input$data[2], ]
    filtered_geo <- geo_shp[geo_shp$data >= input$data[1] & geo_shp$data <= input$data[2], ]
    
    
    
    # Return filtered shapefiles data
    list(filtered_dafor = filtered_dafor, 
         filtered_geo = filtered_geo) ### needs work
    
    
    
    
  })  
  
  #https://shiny.rstudio.com/articles/basics.html
  
  
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-48.38, -27.28, zoom = 10) %>% 
      addPolylines(data =reactiveData()$filtered_dafor,
                   fillColor = "#FF0000",
                   fillOpacity = 0.5,
                   color = "#FF0000",
                   weight = 8,
                   popup = ~paste0("<strong>Localidade: </strong> ", localidade, "<br>",
                                   "<strong>Data:</strong> ", data, "<br>",
                                   "<strong>N. Mergulhadores: </strong> ", n_divers, "<br>", 
                                   "<strong>N. Transectos presente: </strong> " , n_tr_pr,"<br>",
                                   "<strong>Distancia(m): </strong> ", comp_m),
                   labelOptions = labelOptions(noHide = F, direction = "rigth")) %>%
      
      addPolylines(data = reactiveData()$filtered_geo,
                   fillColor = "#0000FF",
                   fillOpacity = 0.5,
                   color = "#0000FF",
                   weight = 8,
                   popup = ~paste0("<strong>Localidade:</strong> ", localidade, "<br>",
                                   "<strong>Data:</strong> ", data, "<br>",
                                   "<strong>IAH:</strong> ", iah_seg, "<br>",
                                   "<strong>Distancia(m): </strong> ", comp_m),
                   labelOptions = labelOptions(noHide = F, direction = "right"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)






