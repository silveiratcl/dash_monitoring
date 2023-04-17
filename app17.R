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


# Define UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                dateRangeInput("daterange", "Selecione intervalo de data: ", 
                              format = "yyyy-mm-dd",
                               start = "2022-06-15",
                               end =  "2023-12-31",
                               separator = " até "),
                
                
                checkboxGroupInput("layers", 
                                   label = "Select shapefile:", 
                                   choices = c("Dafor", "Geomorfologia"),
                                   selected = "Dafor"
                )      
                
                
  )
)



# Define Server
server <- function(input, output) {
  
  # Define reactive data
  reactiveData <- eventReactive(input$daterange, {
    
    # Filter shapefiles data based on selected date range
    filtered_dafor <-  dafor_shp[dafor_shp$data >= input$daterange[1] & dafor_shp$data <= input$daterange[2], ]
    filtered_geo <- geo_shp[geo_shp$data >= input$daterange[1] & geo_shp$data <= input$daterange[2], ]
    
    
    
    # Extract variables column and add to filtered_dafor
    n_divers <- dafor_shp$n_divers
    n_tr_pr <- dafor_shp$n_tr_pr
    comp_m_dafor <- dafor_shp$comp_m
    
    
    
    # Extract variables column and add to filtered_geo
    iah_seg<- geo_shp$iah_seg
    comp_m_geo <- geo_shp$comp_m
    
    
    
    # Return filtered shapefiles data
    list(filtered_dafor = filtered_dafor, 
         filtered_geo = filtered_geo,
         n_divers = n_divers,
         n_tr_pr = n_tr_pr,
         iah_seg = iah_seg,
         comp_m_dafor = comp_m_dafor,
         comp_m_geo = comp_m_geo)
  })  
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-48.38, -27.28, zoom = 10) #%>% 
      #addProviderTiles("CartoDB.Positron")
  })
  
  observe({
    # Show/hide layers based on check box input
    if ("Dafor" %in% input$layers) {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>%
        addPolylines(fillColor = "#FF0000",
                     fillOpacity = 0.5,
                     color = "#FF0000",
                     weight = 8,
                      popup = ~paste0("<strong>Localidade: </strong> ", reactiveData()$localidade, "<br>",
                                     "<strong>Data:</strong> ", data, "<br>",
                                    "<strong>N. Mergulhadores: </strong> ", reactiveData()$n_divers, "<br>", 
                                   "<strong>N. Transectos presente: </strong> " , reactiveData()$n_tr_pr,"<br>",
                                  "<strong>Distancia(m): </strong> ", reactiveData()$comp_m_dafor),
                     labelOptions = labelOptions(noHide = F, direction = "rigth")
        )
    } else {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>%
        clearShapes()
    }
    
    
    if("Geomorfologia" %in% input$layers) {
      leafletProxy("map", data = reactiveData()$filtered_geo) %>%
        addPolylines(fillColor = "#120001",
                     fillOpacity = 0.5,
                     color = "#120001",
                     weight = 8,
                      popup = ~paste0("<strong>Localidade: </strong> ", reactiveData()$localidade, "<br>",
                                     "<strong>Data:</strong> ", reactiveData()$data, "<br>",
                                    "<strong>N. Mergulhadores: </strong> ", reactiveData()$n_divers, "<br>", 
                                   "<strong>N. Transectos presente: </strong> " , reactiveData()$n_tr_pr,"<br>",
                                  "<strong>Distancia(m): </strong> ", reactiveData()$comp_m_geo),
                     labelOptions = labelOptions(noHide = F, direction = "rigth")
        )
    } else {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>% 
        clearShapes()
    }
    
  }
  )}

# Run the app
shinyApp(ui = ui, server = server)


