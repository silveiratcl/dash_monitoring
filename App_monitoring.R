library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(sf)


# Import shapefiles data
dafor_shp <- st_read("shp/dafor.shp", crs = 3857)
dafor_shp <- st_transform(dafor_shp, crs = 3857)

geo_shp <- st_read("shp/geomorfologia.shp", crs = 3857)
geo_shp <- st_transform(geo_shp, crs = 3857)


#st_crs(geo_shp)


# leafleat EPSG:3857


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
server <- function(input, output, session) {
  
  # Define reactive data
  reactiveData <- eventReactive(input$daterange, {
    
    # Filter shapefiles data based on selected date range
    filtered_dafor <-  dafor_shp[dafor_shp$data >= input$daterange[1] & dafor_shp$data <= input$daterange[2], ]
    filtered_geo <- geo_shp[geo_shp$data >= input$daterange[1] & geo_shp$data <= input$daterange[2], ]
    
    
    # Return filtered shapefiles data
    list(filtered_dafor = filtered_dafor, 
         filtered_geo = filtered_geo)
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
  })
  
  observe({
    # Show/hide layers based on check box input
    if ("Dafor" %in% input$layers && nrow(reactiveData()$filtered_dafor) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>%
        addPolylines(fillColor = "red",
                     fillOpacity = 0.5,
                     color = "red",
                     weight = 8,
                     popup = ~paste0("<strong>Locality: </strong> ", reactiveData()$filtered_dafor$localidade, "<br>",
                                     "<strong>Date:</strong> ", data, "<br>",
                                     "<strong>N. Divers: </strong> ", reactiveData()$filtered_dafor$n_divers, "<br>", 
                                     "<strong>Horizontal Visibility(m): </strong> ", reactiveData()$filtered_dafor$vis_horiz, "<br>",
                                     "<strong>N. Transects present: </strong> " , reactiveData()$filtered_dafor$n_tr_pr,"<br>",
                                     "<strong>Distance(m): </strong> ", reactiveData()$filtered_dafor$comp_m),
                     labelOptions = labelOptions(noHide = F, direction = "rigth")
        )
    } else {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>%
        clearShapes()
    }
    
    
    if("Geomorphology" %in% input$layers && nrow(reactiveData()$filtered_geo) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_geo) %>%
        addPolylines(fillColor = "blue",
                     fillOpacity = 0.5,
                     color = "blue",
                     weight = 8,
                     popup = ~paste0("<strong>Locality: </strong> ", reactiveData()$filtered_geo$localidade, "<br>",
                                     "<strong>Date:</strong> ", reactiveData()$filtered_geo$data, "<br>",
                                     "<strong>Segment IAH: </strong> ", reactiveData()$filtered_geo$iah_seg, "<br>", 
                                     "<strong>Distancia(m): </strong> ", reactiveData()$filtered_geo$comp_m),
                     labelOptions = labelOptions(noHide = F, direction = "rigth")
        )
    } else {
      leafletProxy("map", data = reactiveData()$filtered_geo) %>% 
        clearShapes()
    }
    
  }
  )}

# Run the app
shinyApp(ui = ui, server = server)


