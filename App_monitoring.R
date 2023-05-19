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
dafor_shp <- st_read("shp/dafor.shp")
st_crs(dafor_shp) <- 3857

geo_shp <- st_read("shp/geomorfologia.shp")
st_crs(geo_shp) <- 3857
#development

pacs_shp <- st_read("shp/pontos_pacs.shp")
st_crs(pacs_shp) <- 3857

# Define UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 10, right = 10,
    dateRangeInput(
      "daterange", "Select date range: ",
      format = "yyyy-mm-dd",
      start = "2022-06-15",
      end = "2023-12-31",
      separator = " to "
    ),
    checkboxGroupInput(
      "layers",
      label = "Select layer:",
      choices = c("Dafor", "Geomorphology", "Target Locations"),
      selected = c("Dafor", "Geomorphology")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Define reactive data
  reactiveData <- reactive({
    # Filter shapefiles data based on selected date range
    filtered_dafor <- dafor_shp[dafor_shp$data >= input$daterange[1] & dafor_shp$data <= input$daterange[2], ]
    filtered_geo <- geo_shp[geo_shp$data >= input$daterange[1] & geo_shp$data <= input$daterange[2], ]
    filtered_pacs <- pacs_shp
    # Return filtered shapefiles data
    list(
      filtered_dafor = filtered_dafor,
      filtered_geo = filtered_geo,
      filtered_pacs = filtered_pacs
    )
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-48.38, -27.28, zoom = 10) %>%
      addLegend(
        position = "bottomright",
        colors = c("red", "blue", "orange"),
        labels = c("Dafor", "Geomorphology", "Target Locations"),
        title = "Legend"
      )
  })
  
  observe({
    # Clear the map
    leafletProxy("map") %>%
      clearShapes()
    
    # Show/hide layers based on checkbox input
    if ("Dafor" %in% input$layers && nrow(reactiveData()$filtered_dafor) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_dafor) %>%
        addPolylines(
          fillColor = "red",
          fillOpacity = 0.5,
          color = "red",
          weight = 8,
          popup = ~paste0(
            "<strong>Locality: </strong> ", localidade, "<br>",
            "<strong>Date:</strong> ", data, "<br>",
            "<strong>N. Divers: </strong> ", n_divers, "<br>",
            "<strong>Horizontal Visibility(m): </strong> ", vis_horiz, "<br>",
            "<strong>N. Transects present: </strong> ", n_tr_pr, "<br>",
            "<strong>Distance(m): </strong> ", comp_m
          ),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )
    }
    
    if ("Geomorphology" %in% input$layers && nrow(reactiveData()$filtered_geo) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_geo) %>%
        addPolylines(
          fillColor = "blue",
          fillOpacity = 0.5,
          color = "blue",
          weight = 8,
          popup = ~paste0(
            "<strong>Locality: </strong> ", localidade, "<br>",
            "<strong>Date:</strong> ", data, "<br>",
            "<strong>Segment IAH: </strong> ", iah_seg, "<br>",
            "<strong>Distancia(m): </strong> ", comp_m
          ),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )
    }
    
    if ("Target Locations" %in% input$layers && nrow(reactiveData()$filtered_pacs) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_pacs) %>%
        addCircles(
          fillColor = "orange",
          fillOpacity = 0.5,
          color = "orange",
          weight = 8,
          popup = ~paste0("<strong>Locality: </strong> ", locality),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)


