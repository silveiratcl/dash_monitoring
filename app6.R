library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(sf)

# Import shapefile data
shapefile1 <- st_read("shp/dafor.shp")
shapefile2 <- st_read("shp/geomorfologia.shp")

# Define UI
ui <- fluidPage(
  dateRangeInput("dateRange", label = "Select date range:"),
  checkboxGroupInput("shapefiles", label = "Select shapefiles:",
                     choices = c("Shapefile 1", "Shapefile 2"),
                     selected = c("shapefile1", "shapefile2")),
  leafletOutput("map")
)

# Define Server
server <- function(input, output) {
  
  # Define reactive data
  reactiveData <- eventReactive(input$dateRange, {
    data_filt <- NULL
    # Filter data based on selected date range and shapefiles
    if ("Shapefile 1" %in% input$shapefiles) {
      shapefile1_filt <- shapefile1[shapefile1$data >= input$dateRange[1] &
                                      shapefile1$data <= input$dateRange[2], ]
      data_filt <- shapefile1_filt
    }
    
    if ("Shapefile 2" %in% input$shapefiles) {
      shapefile2_filt <- shapefile2[shapefile2$data >= input$dateRange[1] &
                                      shapefile2$data <= input$dateRange[2], ]
      if (is.null(data_filt)) {
        data_filt <- shapefile2_filt
      } else {
        data_filt <- rbind(data_filt, shapefile2_filt)
      }
    }
    
    # If no shapefiles selected, return NULL
    if (is.null(data_filt)) {
      return(NULL)
    }
    
    # Cast data into a "MULTILINESTRING" class
    data_filt <- st_cast(data_filt, "MULTILINESTRING", group_or_split = TRUE)
    
    # Return filtered and cast data
    return(data_filt)
    
  
  # Update map based on selected data and shapefile layers
    # If no data to display, return empty map
    if (is.null(data)) {
      return(leaflet() %>% addTiles())
    }
    
    # Otherwise, display data on map
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = data,
                   fillColor = "#FF0000",
                   fillOpacity = 0.5,
                   color = "#000000",
                   weight = 1)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
