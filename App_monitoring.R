library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(leaflet)
library(rgdal)
library(sp)
library(sf)
library(dplyr)


# Import shapefiles data
dafor_shp <- st_read("shp/dafor.shp", crs = 3857)
dafor_shp <- st_transform(dafor_shp, crs = 3857)

geo_shp <- st_read("shp/geomorfologia.shp", crs = 3857)
geo_shp <- st_transform(geo_shp, crs = 3857)


#st_crs(geo_shp)


# leafleat EPSG:3857
dafor_shp <- st_read("shp/dafor.shp")
#dafor_crs <- st_crs("EPSG:3857")
#dafor_shp <- st_transform(dafor_shp, dafor_crs)
print(dafor_shp)

geo_shp <- st_read("shp/geomorfologia.shp")
#geo_crs <- st_crs("EPSG:3857")
#geo_shp <- st_transform(geo_shp, geo_crs)
#print(geo_shp)


pacs_shp <- st_read("shp/pts_pacs.shp")
#pacs_crs <- st_crs("EPSG:3857")
#pacs_shp <- st_transform(pacs_shp, pacs_crs)
#print(pacs_shp)

local_shp <- st_read("shp/localidade.shp")
#local_crs <- st_crs("EPSG:3857")
#local_shp <- st_transform(local_shp, local_crs)
#print(local_shp)



ui <- dashboardPage(
  
  skin = "yellow",
  
  dashboardHeader(title = "Sun Coral Monitoring"),
  
  dashboardSidebar(
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
      choices = c("Dafor", "Geomorphology", "Target Locations", "Locality"),
      selected = c("Dafor")
    )),
  
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    
    fluidRow(
      infoBoxOutput("locations_Box", width = 3),
      infoBoxOutput("segments_Box", width = 3),
      infoBoxOutput("sun_coral_Box", width = 3),
      infoBoxOutput("dive_time_box", width = 3),
      height = "300px"
    ),
    
    fluidRow(
      box(leafletOutput("map"),
          width = "100%",
          height = "100%"
      )
    )
  )
)

server <- function(input, output, session) { 
  
  # Define reactive data
  reactiveData <- reactive({
    # Filter shapefiles data based on selected date range
    filtered_dafor <- dafor_shp[dafor_shp$data >= input$daterange[1] & dafor_shp$data <= input$daterange[2], ]
    filtered_geo <- geo_shp[geo_shp$data >= input$daterange[1] & geo_shp$data <= input$daterange[2], ]
    filtered_pacs <- pacs_shp
    filtered_local <- local_shp
    
    #boxesdata
    
    #location number
    n_location <- dafor_shp %>% 
      count(localidade) %>% 
      nrow() 
    
    #segments number
    n_segments <- dafor_shp %>% 
      count(dafor_id) %>% 
      nrow() 
    
    #sum coral prevalence
    sum_cs_present <- dafor_shp %>% 
      summarise(sum(n_tr_pr)) 
    
    n_cs_present <- sum_cs_present$`sum(n_tr_pr)` 
    
    #dive time pais
    dive_time <- dafor_shp %>% 
      summarise(sum(n_trans_vi)/60) 
    
    dive_time_pair <- round(dive_time$`sum(n_trans_vi)/60`, digits = 0)
    
    
    
    # Return filtered shapefiles data
    list(
      filtered_dafor = filtered_dafor,
      filtered_geo = filtered_geo,
      filtered_pacs = filtered_pacs,
      filtered_local = filtered_local,
      n_location = n_location,
      n_segments = n_segments,
      n_cs_present = n_cs_present,
      dive_time_pair = dive_time_pair
      
    )
  })
  
  #infoboxes
  
  output$locations_Box <- renderInfoBox({
    infoBox(
      "Monitored Locations", paste0(reactiveData()$n_location), icon = icon("location-dot"),
      color = "green"
    )
  })
  
  output$segments_Box <- renderInfoBox({
    infoBox(
      "Number of Segments", paste0(reactiveData()$n_segments), icon = icon("bacon"),
      color = "orange"
    )
  })
  
  output$sun_coral_Box <- renderInfoBox({
    infoBox(
      "Transects with Sun Coral", paste0(reactiveData()$n_cs_present), icon = icon("circle-exclamation"),
      color = "red"
    )
  })
  
  output$dive_time_box <- renderInfoBox({
    infoBox(
      "Dive time (h/pair of divers)", paste0(reactiveData()$dive_time_pair), icon = icon("clock"),
      color = "blue"
    )
  })
  
  
  #mapoutput
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>% 
      #addTiles() %>%
      setView(-48.38, -27.28, zoom = 10) %>%
      addLegend(
        position = "bottomleft",
        colors = c("red", "blue", "orange", "green"),
        labels = c("Dafor", "Geomorphology", "Target Locations", "Locality"),
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
    
    if ("Locality" %in% input$layers && nrow(reactiveData()$filtered_local) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_local) %>%
        addPolylines(
          fillColor = "green",
          fillOpacity = 0.5,
          color = "green",
          weight = 8,
          popup = ~paste0("<strong>Locality: </strong> ", localidade),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )
    }
    
  }
  )
  
  
  
}

shinyApp(ui, server)