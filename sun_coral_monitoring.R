library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(sp)


dafor_data <- st_read("c//../shp/dafor.shp")


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                dateRangeInput("data", "Data: ", 
                               format = "yyyy-mm-dd",
                               start = "2022-06-15",
                               end =  "2023-12-31",
                               separator = " até ")
  )
)





server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    dafor_data[dafor_data$data >= input$range[1] & dafor_data$data <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(dafor_data) %>% 
      addTiles() %>%
      addPolylines(data = dafor_data,
                   popup = ~paste0("<strong>Localidade: </strong> ", localidade, "<br>",
                                   "<strong>Data:</strong> ", data, "<br>",
                                   "<strong>N. Mergulhadores: </strong> ", n_divers, "<br>", 
                                   "<strong>N. Transectos presente: </strong> " , n_tr_pr,
                                   "<strong>Distancia(m): </strong> ", comp_m),
                   labelOptions = labelOptions(noHide = F, direction = "rigth"))  # insert distance
      
        
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
observe({
    leafletProxy("map", data = filteredData()) %>%
      clearShapes()
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = dafor_data)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.

    
   })
  }
 


shinyApp(ui, server)








