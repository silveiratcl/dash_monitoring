library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(sf)



# Define UI
ui <- bootstrapPage(
  
  dateRangeInput("dateRange", label = "Select date range:",
                 format = "yyyy-mm-dd",
                 start = "2022-06-15",
                 end =  "2023-12-31",
                 separator = " até "),
  checkboxGroupInput("shapefiles", label = "Select shapefile:", choices = c("Shapefile 1", "Shapefile 2")),
  leafletOutput("map", width = "100%", height = "100%")
)


# Define Server
server <- function(input, output) {
  
  # Import shapefiles data
  shapefile1 <- st_read("c//../shp/dafor.shp")
  shapefile2 <- st_read("c//../shp/geomorfologia.shp")
  
  
  # Define reactive data
  reactiveData <- eventReactive(input$dateRange, {
    # Filter shapefiles data based on selected date range and shapefiles
    if ("Shapefile 1" %in% input$shapefiles) {
      filteredShapefile1 <- shapefile1[shapefile1$data >= input$dateRange[1] & shapefile1$data <= input$dateRange[2], ]
    } else {
      filteredShapefile1 <- NULL
    }
    if ("Shapefile 2" %in% input$shapefiles) {
      filteredShapefile2 <- shapefile2[shapefile2$data >= input$dateRange[1] & shapefile2$data <= input$dateRange[2], ]
    } else {
      filteredShapefile2 <- NULL
    }
    
    # Return filtered shapefiles data
    list(filteredShapefile1 = filteredShapefile1,
         filteredShapefile2 = filteredShapefile2)
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(-48.38, -27.28, zoom = 10) %>%
      addPolylines( data = reactiveData()$filteredShapefile1,
                  fillColor = "#FF0000",
                  fillOpacity = 0.5,
                  color = "#000000",
                  weight = 1,
                  popup = ~paste0("<strong>Localidade: </strong> ", localidade, "<br>",
                                  "<strong>Data:</strong> ", data, "<br>",
                                  "<strong>N. Mergulhadores: </strong> ", n_divers, "<br>", 
                                  "<strong>N. Transectos presente: </strong> " , n_tr_pr,"<br>",
                                  "<strong>Distancia(m): </strong> ", comp_m),
                  labelOptions = labelOptions(noHide = F, direction = "rigth")) %>%
      
      addPolylines( data = reactiveData()$filteredShapefile2,
                  fillColor = "#0000FF",
                  fillOpacity = 0.5,
                  color = "#000000",
                  weight = 1, 
                  popup = ~paste0("<strong>Localidade: </strong> ", localidade, "<br>",
                                  "<strong>Data:</strong> ", data, "<br>",
                                  "<strong>N. Mergulhadores: </strong> ", n_divers, "<br>", 
                                  "<strong>N. Transectos presente: </strong> " , n_tr_pr,"<br>",
                                  "<strong>Distancia(m): </strong> ", comp_m),
                  labelOptions = labelOptions(noHide = F, direction = "rigth"))
  
    })
}


      addPolylines(data =reactiveData()$filtered_dafor,
                   fillColor = "#FF0000",
                   fillOpacity = 0.5,
                   color = "#FF0000",
                   weight = 6,
                   popup = ~paste0("<strong>Localidade: </strong> ", localidade, "<br>",
                                   "<strong>Data:</strong> ", data, "<br>",
                                   "<strong>N. Mergulhadores: </strong> ", n_divers, "<br>", 
                                   "<strong>N. Transectos presente: </strong> " , n_tr_pr,"<br>",
                                   "<strong>Distancia(m): </strong> ", comp_m),
                   labelOptions = labelOptions(noHide = F, direction = "rigth")) %>%







# Run the app
shinyApp(ui = ui, server = server)