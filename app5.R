# Define UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                dateRangeInput("date_range", "Selecione intervalo de data: ", 
                               format = "yyyy-mm-dd",
                               start = "2022-06-15",
                               end =  "2023-12-31",
                               separator = " até "),
                
                checkboxInput(inputId = "dafor_box",
                              label = strong("DAFOR - Coral Sol"),
                              value = FALSE),
                
                checkboxInput(inputId = "geo_box",
                              label = strong("Geomorfologia"),
                              value = FALSE)
                
                
                
  )
)




server <- function(input, output, session) {
  
  
  reactiveData <- eventReactive(input$date_range, {
    # Filter shapefiles data based on selected date range
    filtered_dafor <-  dafor_shp[dafor_shp$data >= input$date_range[1] & dafor_shp$data <= input$date_range[2], ]
    filtered_geo <- geo_shp[geo_shp$data >= input$date_range[1] & geo_shp$data <= input$date_range[2], ]
    
    
    # Return filtered shapefiles data
    list(filtered_dafor = filtered_dafor, 
         filtered_geo = filtered_geo) ### needs work
  })  
  
  
  
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-48.38, -27.28, zoom = 10) %>% 
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
      
      addPolylines(data = reactiveData()$filtered_geo,
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


shinyApp(ui = ui, server = server)
