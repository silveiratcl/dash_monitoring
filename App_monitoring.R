library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(leaflet)
library(rgdal)
library(sp)
library(sf)
library(dplyr)
library(markdown)
library(leaflet.extras)
library(mapview)


# Import shapefiles data

dafor_shp <- st_read("shp/dafor.shp")

geo_shp <- st_read("shp/geomorfologia.shp")

pacs_shp <- st_read("shp/pts_pacs.shp")

local_shp <- st_read("shp/localidade.shp")

rebio_shp <- st_read("shp/limite_rebio.shp")

occ_shp <- st_read("shp/manchas_cs.shp")

# Create indicators layers

# N transects with sun coral

dafor_mrg_local_shp <- dafor_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  group_by(localidade, data) %>% 
  mutate(n_tr_pr_sum = sum(n_tr_pr)) %>% 
  select(-c(geometry.x)) %>% 
  st_as_sf(sf_column_name = "geometry.y")


# IAH localities

geo_mrg_local_shp <- geo_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  group_by(localidade, data) %>% 
  mutate(iah_seg_avg = mean(iah_seg)) %>% 
  select(-c(geometry.x)) %>% 
  st_as_sf(sf_column_name = "geometry.y")

# Monitoring Effort transects - positives by 1000 meters 

effort_mrg_local_shp <- dafor_shp %>% 
  data.frame() %>% 
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  group_by(localidade, data) %>% 
  mutate(n_tr_pr_1000 = round(sum(n_tr_pr)/(sum(comp_m)/1000),3)) %>% 
  select(-c(geometry.x)) %>% 
  st_as_sf(sf_column_name = "geometry.y")

# Number of transects by locality

ntrans_mrg_local_shp <- dafor_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  group_by(localidade, data) %>% 
  mutate(n_trans = sum(n_trans_vi)) %>% 
  select(-c(geometry.x)) %>% 
  st_as_sf(sf_column_name = "geometry.y")

# Days Elapsed since last management

# Mass managed



# Sidebar Menu

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Monitoring Map", tabname = "map", icon = icon("map"),
             dateRangeInput(
               "daterange", "Select date range: ",
               format = "yyyy-mm-dd",
               start = "2012-01-1",
               separator = " to "
             ),
             
             checkboxGroupInput(
               "indicators",
               label = "Indicators:",
               choices = c("Transects with Sun Coral(TWSC)", 
                           "Habitat Suitability Index(HSI)",
                           "TWSC/1000m",
                           "N. of Transects by Locality(NTL)"),
               selected = c("Transects with Sun Coral(TWSC)")
             ),
             
             checkboxGroupInput(
               "layers",
               label = "Data:",
               choices = c("Occurrence", "Dafor", "Geomorphology", "Target Locations", "Locality", "REBIO Limits" ),
               selected = c("Occurrence")
             )),
    
    menuItem("Documentation", 
             tabName = "documentation", 
             icon = icon("file-text"),
             menuSubItem(text = "About the Dashboard", 
                         href = "https://dent-packet-5b9.notion.site/PACS-Monitoring-Dashboard-09d8969b1ff14e3ab8bd1f73de6a0906?pvs=4",
                         newtab = T)),
    
    menuItem(paste0("Last update",": ",
                    format(file.info("App_monitoring.R")$mtime, "%d-%m-%Y")))
    )
  )
  



body <- dashboardBody(
  tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
  
  
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


# Put them together into a dashboardPage

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Sun Coral Monitoring"),
  sidebar,
  body
)




server <- function(input, output, session) { 
  
  # Define reactive data
  reactiveData <- reactive({
    # Filter shapefiles data based on selected date range
    filtered_dafor <- dafor_shp[dafor_shp$data >= input$daterange[1] & dafor_shp$data <= input$daterange[2], ]
    
    filtered_geo <- geo_shp[geo_shp$data >= input$daterange[1] & geo_shp$data <= input$daterange[2], ]
    
    filtered_occ <- occ_shp[occ_shp$data >= input$daterange[1] & occ_shp$data <= input$daterange[2], ]
    
    filtered_dafor_mrg_local <- dafor_mrg_local_shp[dafor_mrg_local_shp$data >= input$daterange[1] & dafor_mrg_local_shp$data <= input$daterange[2], ]
    
    filtered_geo_mrg_local <- geo_mrg_local_shp[geo_mrg_local_shp$data >= input$daterange[1] & geo_mrg_local_shp$data <= input$daterange[2], ]
    
    filtered_effort_mrg_local <- effort_mrg_local_shp[effort_mrg_local_shp$data >= input$daterange[1] & effort_mrg_local_shp$data <= input$daterange[2], ]
    
    filtered_ntrans_mrg_local <- ntrans_mrg_local_shp[ntrans_mrg_local_shp$data >= input$daterange[1] & ntrans_mrg_local_shp$data <= input$daterange[2], ]
    
    filtered_pacs <- pacs_shp
    
    filtered_local <- local_shp
    
    filtered_rebio <- rebio_shp
    
    
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
    
    #dive time pairs
    dive_time <- dafor_shp %>% 
      summarise(sum(n_trans_vi)/60) 
    
    dive_time_pair <- round(dive_time$`sum(n_trans_vi)/60`, digits = 0)
    
    
    
    # Return filtered shapefiles data
    list(
      filtered_dafor = filtered_dafor,
      filtered_geo = filtered_geo,
      filtered_pacs = filtered_pacs,
      filtered_local = filtered_local,
      filtered_rebio = filtered_rebio,
      filtered_occ = filtered_occ,
      filtered_dafor_mrg_local = filtered_dafor_mrg_local,
      filtered_geo_mrg_local = filtered_geo_mrg_local,
      filtered_effort_mrg_local = filtered_effort_mrg_local,
      filtered_ntrans_mrg_local = filtered_ntrans_mrg_local,
      
      # Return boxes data
      n_location = n_location, #MAKE IT REACTIVE TO DATE
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
      "Segments monitored ", paste0(reactiveData()$n_segments), icon = icon("bacon"),
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
      "Dive time (hours)", paste0(reactiveData()$dive_time_pair), icon = icon("clock"),
      color = "blue"
    )
  })
  
  
  
  #map output
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>% 
      setView(-48.38, -27.28, zoom = 12) 
      
  })
  
  observe({
    # Clear the map when "on' "off" layers from checkbox input
    leafletProxy("map") %>%
      clearShapes() %>% 
      clearControls() 
    
    
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
      
      colors <- c("red", "blue", "green", "orange", "purple")
      
      leafletProxy("map", data = reactiveData()$filtered_local) %>%
        addPolylines(
          stroke = T,
          fillColor = colors,
          fillOpacity = 0.2,
          color = colors,
          weight = 8,
          popup = ~paste0("<strong>Locality: </strong> ", localidade),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )
    }
    
    if ("Occurrence" %in% input$layers && nrow(reactiveData()$filtered_occ) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_occ) %>%
        addCircles(
          fillColor = "red",
          fillOpacity = 0.5,
          color = "red",
          weight = 8,
          popup = ~paste0("<strong>Locality: </strong> ", localidade, "<br>",
                          "<strong>Date found: </strong> ", data),
          labelOptions = labelOptions(noHide = FALSE, direction = "right") #add pulse marker see chatgpt
        )
    }
    
    if ("REBIO Limits" %in% input$layers && nrow(reactiveData()$filtered_rebio) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_rebio) %>%
        addPolylines(
          fillColor = "red",
          fillOpacity = 0.5,
          color = "red",
          weight = 4,
          popup = ~paste0( local ),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )
    }
    
    if ("Transects with Sun Coral(TWSC)" %in% input$indicators && nrow(reactiveData()$filtered_dafor_mrg_local) > 0) {
      
      pal_twsc <- colorNumeric(
        palette = rev(c("#BE2A3E","#CD4242","#E76B49","#F7B059","#F5CA63","#EACF65","#D6CA64","#5DA25F","#449559","#22763F")),
        domain = reactiveData()$filtered_dafor_mrg_local$n_tr_pr_sum
      )
      
      
      leafletProxy("map", data = reactiveData()$filtered_dafor_mrg_local) %>%
        addPolylines(
          fillColor = ~pal_twsc(n_tr_pr_sum),
          color = ~pal_twsc(n_tr_pr_sum),
          weight = 10,
          popup = ~paste0("<strong>Locality: </strong> ", localidade, "<br>",
                          "<strong>Transects with sun coral: </strong> ", n_tr_pr_sum),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )%>%
        addLegend(
          pal = pal_twsc,
          values = reactiveData()$filtered_dafor_mrg_local$n_tr_pr_sum,
          position = "bottomright",
          title = ~paste0("TWSC"),
        )
    }
    
    
    if ("Habitat Suitability Index(HSI)" %in% input$indicators && nrow(reactiveData()$filtered_geo_mrg_local) > 0) {
      
      pal_iah <- colorNumeric(
        palette = "Blues",
        domain = reactiveData()$filtered_geo_mrg_local$iah_seg_avg
      )
      
      
      leafletProxy("map", data = reactiveData()$filtered_geo_mrg_local) %>%
        addPolylines(
          fillColor = ~pal_iah(iah_seg_avg),
          color = ~pal_iah(iah_seg_avg),
          weight = 10,
          popup = ~paste0("<strong>HSI: </strong> ", iah_seg_avg) ,
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )%>%
        addLegend(
          pal = pal_iah,
          values = reactiveData()$filtered_geo_mrg_local$iah_seg_avg,
          position = "bottomright",
          title = ~paste0("HSI")
        )
    }
    
    
    if ("TWSC/1000m" %in% input$indicators && nrow(reactiveData()$filtered_effort_mrg_local) > 0) {
      
      pal_effort <- colorNumeric(
        palette = "Oranges",
        domain = reactiveData()$filtered_effort_mrg_local$n_tr_pr_1000
      )
      
      
      leafletProxy("map", data = reactiveData()$filtered_effort_mrg_local) %>%
        addPolylines(
          fillColor = ~pal_effort(n_tr_pr_1000),
          color = ~pal_effort(n_tr_pr_1000),
          weight = 10,
          popup = ~paste0("<strong>TWSC/1000: </strong> ", n_tr_pr_1000) ,
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )%>%
        addLegend(
          pal = pal_effort,
          values = reactiveData()$filtered_effort_mrg_local$n_tr_pr_1000,
          position = "bottomright",
          title = ~paste0("TWSC/1000m")
        )
    }
    
    
    if ("N. of Transects by Locality(NTL)" %in% input$indicators && nrow(reactiveData()$filtered_ntrans_mrg_local) > 0) {
      
      pal_ntrans <- colorNumeric(
        palette = "PuBu",
        domain = reactiveData()$filtered_ntrans_mrg_local$n_trans
      )
      
      
      leafletProxy("map", data = reactiveData()$filtered_ntrans_mrg_local) %>%
        addPolylines(
          fillColor = ~pal_ntrans(n_trans),
          color = ~pal_ntrans(n_trans),
          weight = 10,
          popup = ~paste0("<strong>N. of Transects: </strong> ", n_trans) ,
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )%>%
        addLegend(
          pal = pal_ntrans,
          values = reactiveData()$filtered_ntrans_mrg_local$n_trans,
          position = "bottomright",
          title = ~paste0("NTL")
        )
    }
    
  })
}

shinyApp(ui, server)