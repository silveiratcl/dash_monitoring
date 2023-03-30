library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(sf)


# Import shapefiles data
dafor_shp <- st_read("c//../shp/dafor.shp")
geo_shp <-st_read("c//../shp/geomorfologia.shp")


# Define UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                dateRangeInput("data", "Selecione intervalo de data: ", 
                               format = "yyyy-mm-dd",
                               start = "2022-06-15",
                               end =  "2023-12-31",
                               separator = " até "),
                
                checkboxInput(inputId = "data", #needs work
                              label = strong("DAFOR - Coral Sol"),
                              value = FALSE),
                
                checkboxInput(inputId = "data", #needs work
                              label = strong("Geomorfologia"),
                              value = FALSE)
                
                
                
  )
)

#https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/


# Define Server
server <- function(input, output, session) {
  
  
  
  
  # Define reactive data
  reactiveData <- eventReactive(input$dateRange, {
    # Filter shapefiles data based on selected date range and shapefiles
    if ("Dafor" %in% input$shapefiles) {
      filtered_dafor <- shapefile1[dafor_shp$data >= input$dateRange[1] & dafor_shp$data <= input$dateRange[2], ]
    } else {
      filtered_dafor <- NULL
    }
    if ("Geomorfologia" %in% input$shapefiles) {
      filtered_geo <- geo_shp[geo_shp$data >= input$dateRange[1] & geo_shp$data <= input$dateRange[2], ]
    } else {
      filtered_geo <- NULL
    }
    
    # Return filtered shapefiles data
    list(filtered_dafor = filtered_dafor,
         filtered_geo = filtered_geo)
  })
  
  
  #https://shiny.rstudio.com/articles/basics.html
  
  
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-48.38, -27.28, zoom = 10) %>% 
      addPolylines(data =reactiveData()$shapefiles$filtered_dafor,
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
      
      addPolylines(data = reactiveData()$shapefiles$filtered_geo,
                   fillColor = "#0000FF",
                   fillOpacity = 0.5,
                   color = "#0000FF",
                   weight = 6,
                   popup = ~paste0("<strong>Localidade:</strong> ", localidade, "<br>",
                                   "<strong>Data:</strong> ", data, "<br>",
                                   "<strong>IAH:</strong> ", iah_seg, "<br>",
                                   "<strong>Distancia(m): </strong> ", comp_m),
                   labelOptions = labelOptions(noHide = F, direction = "right"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)






