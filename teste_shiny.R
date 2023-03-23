
install.packages('fastmap')

install.packages("htmltools")

install.packages(c('leaflet', 'shiny', 'shinydashboard', 'RJSONIO'))

library('fastmap')
library('htmltools')
library('leaflet')
library('shiny')
library('shinydashboard')
library('RJSONIO')
library('rgdal')

shapefile <- readOGR(dsn = "C://../shp/dafor.shp", layer = "dafor")


ui <- dashboardPage(
  dashboardHeader(title = "Shapefile Monitoring Dashboard"),
  dashboardSidebar(
    selectInput("variable", "Select a variable:",
                choices = c("data", "faixa_bat", "iar_med"))
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Map",
        status = "primary",
        leafletOutput("map", width = "100%", height = "500px")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = shapefile)
  })
}


# Run the application
shinyApp(ui = ui, server = server)




server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = shapefile)
  })
  
  # Refresh data every 30 seconds
  autoInvalidate <- reactiveTimer(30000)
  observe({
    autoInvalidate()
    shapefile <<- readOGR(dsn = "path/to/shapefile", layer = "layer_name")
  })
}

server










