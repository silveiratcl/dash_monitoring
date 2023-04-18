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
                dateRangeInput("daterange", "Select date range: ", 
                               format = "yyyy-mm-dd",
                               start = "2022-06-15",
                               end =  "2023-12-31",
                               separator = " to "),
                
                
                checkboxGroupInput("layers", 
                                   label = "Select shapefile:", 
                                   choices = c("Dafor", "Geomorphology"),
                                   selected = c("Dafor","Geomorphology")
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
    localidade_dafor <- dafor_shp$localidade
    n_divers <- dafor_shp$n_divers
    n_tr_pr <- dafor_shp$n_tr_pr
    vis_horiz <- dafor_shp$vis_horiz 
    comp_m_dafor <- dafor_shp$comp_m
    
    
    
    # Extract variables column and add to filtered_geo
    localidade_geo <- geo_shp$localidade
    iah_seg<- geo_shp$iah_seg
    comp_m_geo <- geo_shp$comp_m
    
    
    
    # Return filtered shapefiles data
    list(filtered_dafor = filtered_dafor, 
         filtered_geo = filtered_geo,
         localidade_dafor = localidade_dafor,
         localidade_geo = localidade_geo,
         n_divers = n_divers,
         n_tr_pr = n_tr_pr,
         vis_horiz = vis_horiz,
         iah_seg = iah_seg,
         comp_m_dafor = comp_m_dafor,
         comp_m_geo = comp_m_geo)
  })  
  
  
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-48.38, -27.28, zoom = 10)%>%
      addLegend(
        position = "bottomright",
        colors = c("red", "blue"),
        labels = c("Dafor", "Geomorphology"),
        title = "Legend"
      )
    #addProviderTiles("CartoDB.Positron")
  })
  
  observe({
    # Show/hide layers based on check box input
    if ("Dafor" %in% input$layers) {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>%
        addPolylines(fillColor = "red",
                     fillOpacity = 0.5,
                     color = "red",
                     weight = 8,
                     popup = ~paste0("<strong>Locality: </strong> ", reactiveData()$localidade_dafor, "<br>",
                                     "<strong>Date:</strong> ", data, "<br>",
                                     "<strong>N. Divers: </strong> ", reactiveData()$n_divers, "<br>", 
                                     "<strong>Horizontal Visibility(m): </strong> ", reactiveData()$vis_horiz, "<br>",
                                     "<strong>N. Transects present: </strong> " , reactiveData()$n_tr_pr,"<br>",
                                     "<strong>Distance(m): </strong> ", reactiveData()$comp_m_dafor),
                     labelOptions = labelOptions(noHide = F, direction = "rigth")
        )
    } else {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>%
        clearShapes()
    }
    
    
    if("Geomorphology" %in% input$layers) {
      leafletProxy("map", data = reactiveData()$filtered_geo) %>%
        addPolylines(fillColor = "blue",
                     fillOpacity = 0.5,
                     color = "blue",
                     weight = 8,
                     popup = ~paste0("<strong>Locality: </strong> ", reactiveData()$localidade_geo, "<br>",
                                     "<strong>Date:</strong> ", data, "<br>",
                                     "<strong>Segment IAH: </strong> ", reactiveData()$iah_seg, "<br>", 
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


