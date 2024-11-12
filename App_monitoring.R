library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(leaflet)
library(sp)
library(sf)
library(dplyr)
library(markdown)
library(leaflet.extras)
library(mapview)
library(lubridate)
library(git2r)

# Import shapefiles data

dafor_shp <- st_read("shp/dafor.shp")

geo_shp <- st_read("shp/geomorfologia.shp")

pacs_shp <- st_read("shp/pts_pacs.shp")

local_shp <- st_read("shp/localidade.shp")

rebio_shp <- st_read("shp/limite_rebio.shp")

occ_shp <- st_read("shp/manchas_cs.shp") #### take off

manejo_shp <- st_read("shp/manejo_cs.shp")

invasion_pts_shp <- st_read("shp/pts_invasao_nova.shp")

####

# #locality inspection
#dafor <-sort(unique(dafor_shp$localidade))
#local <-sort(unique(local_shp$localidade))
#setdiff(dafor, local)
# 
# 
#dafor <-sort(unique(dafor_mrg_local_shp$localidade))
#local <-sort(unique(local_shp$localidade))
#setdiff(dafor, local)
#


## check comp_m

#print(dafor_shp, n=123)
#print(geo_shp, n=73)
#print(local_shp, n=42)

#dafor_shp %>% 
 # filter(comp_m == NA)

#dafor_shp$comp_m

#local_shp$comp_m



####



# Repository version

#source("version_info.R")

# Create indicators layers

## N transects with sun coral

dafor_mrg_local_shp <- dafor_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  group_by(localidade, data) %>% 
  mutate(n_tr_pr_sum = sum(n_tr_pr)) %>% 
  select(-c(geometry.x)) %>% 
  st_as_sf(sf_column_name = "geometry.y")


## IAH localities

geo_mrg_local_shp <- geo_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  group_by(localidade, data) %>% 
  mutate(iah_seg_avg = mean(iah_seg)) %>% 
  select(-c(geometry.x)) %>% 
  st_as_sf(sf_column_name = "geometry.y")

## Monitoring Effort transects - positives by 1000 meters 

#effort_mrg_local_shp <- dafor_shp %>% 
 # data.frame() %>% 
  #merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  #filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  #group_by(localidade, data) %>% 
  #mutate(n_tr_pr_1000 = round(sum(n_tr_pr)/(sum(comp_m)/1000),3)) %>% 
  #select(-c(geometry.x)) %>% 
  #st_as_sf(sf_column_name = "geometry.y")

## Number of transects by locality

ntrans_mrg_local_shp <- dafor_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>% #Temporary - need create localities to all monitored site
  group_by(localidade, data) %>% 
  mutate(n_trans = sum(n_trans_vi)) %>% 
  select(-c(geometry.x)) %>% 
  st_as_sf(sf_column_name = "geometry.y")


ntrans_mrg_local_shp[ ntrans_mrg_local_shp$localidade == "saco_do_batismo", ]


## Days since last management

today<-Sys.Date()

### Calculate the maximum date for each locality in manejo_shp
max_dates <- manejo_shp %>%
  group_by(localidade) %>%
  summarize(max_data = max(data))

### Merge manejo_shp with local_shp based on the "localidade" field
merged_data <- manejo_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>%
  select(localidade, data, n_colonias, geometry.x, geometry.y)  ### add more variables

### Join merged_data with max_dates to get the maximum date for each locality
result_data <- merged_data %>%
  left_join(max_dates, by = "localidade") %>%
  mutate(days_since_last_record = as.numeric(today - max_data))

# Filter only records where the date matches the maximum date for each locality
# result_data <- result_data %>%
# filter(data == max_data)

### Convert the result_data to an sf object
days_after_mng_mrg_local <- st_as_sf(result_data, sf_column_name = "geometry.y")

## Days since last check

### Calculate the maximum date for each locality in dafor_shp
max_dates <- dafor_shp %>%
  group_by(localidade) %>%
  summarize(max_data = max(data))

### Merge manejo_shp with local_shp based on the "localidade" field
merged_data <- dafor_shp %>%
  data.frame() %>%
  merge(., local_shp, by = "localidade", all.x = TRUE) %>%
  filter(!st_is_empty(geometry.y)) %>%
  select(localidade, data, geometry.x, geometry.y)  ### add more variables

### Join merged_data with max_dates to get the maximum date for each locality
result_data <- merged_data %>%
  left_join(max_dates, by = "localidade") %>%
  mutate(days_since_last_check = as.numeric(today - max_data))

# Filter only records where the date matches the maximum date for each locality
# result_data <- result_data %>%
# filter(data == max_data)

### Convert the result_data to an sf object
days_since_check_mrg_local <- st_as_sf(result_data, sf_column_name = "geometry.y")



# Sidebar Menu

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    HTML(paste0(
      "<br>",
      "<a><img style = 'display: block; margin-left: auto; margin-right: auto;' src='img/jpg/logo.png' width = '186'></a>",
      "<br>"
    )),
    
    menuItem("Indicators", tabname = "map", icon = icon("triangle-exclamation"),
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

                          # "TWSC/1000m",

                           "N. of Transects by Locality(NTL)",
                           "Days since last management(DSLManag)",
                           "Days since last check(DSLCheck)"),
               selected = c("Transects with Sun Coral(TWSC)")
             )),
    
    
    menuItem("Layers", tabname = "map", icon = icon("map"),
             
             
             checkboxGroupInput(
               "layers",
               label = "Layers:",
               choices = c("Occurrence", 
                           "Dafor", "Geomorphology", 
                           "Target Locations", 
                           "Locality", 
                           "REBIO Limits" ),
               selected = c("Occurrence")
             )),
    
    
    
    
    menuItem("Documentation", 
             tabName = "documentation", 
             icon = icon("file-text"),
             menuSubItem(text = "About the Dashboard", 
                         href = "about_dash/about_dash.html",
                         newtab = T)),
    
    
    div(
      class = "sidebar-footer",
      p(
        style = "font-size: 12px; text-align: left; margin-left: 15px ",
        HTML(paste0(
          "Developed by ", "<a href='https://silveiratcl.github.io/site/' target = '_blank'>Thiago Silveira</a>", "<br>",
          "last update: ", "2024-11-06", "<br>",
          "commit: [", "6c11896c", "]" ))
      )
    )
  )
)   



body <- dashboardBody(
  
  
  fluidRow(
    infoBoxOutput("locations_Box", width = 3),
    infoBoxOutput("segments_Box", width = 3),
    infoBoxOutput("sun_coral_Box", width = 3),
    infoBoxOutput("dive_time_box", width = 3),
    height = "300px"
  ),
  
  fluidRow(
    includeCSS("css/styles.css"),
    box(leafletOutput("map"),
        width = "100%",
        height = "100vh"  # Set the height to 100vh
        
    )
  )
)


# Put them together into a dashboardPage

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Sun Coral Check"),
  sidebar,
  body
)




server <- function(input, output, session) { 
  
  # Define reactive data
  reactiveData <- reactive({
    # Filter shapefiles data based on selected date range
    
    filtered_dafor <- dafor_shp[dafor_shp$data >= input$daterange[1] & dafor_shp$data <= input$daterange[2], ]
    
    filtered_geo <- geo_shp[geo_shp$data >= input$daterange[1] & geo_shp$data <= input$daterange[2], ]
    
    filtered_occ <- invasion_pts_shp[invasion_pts_shp$data >= input$daterange[1] & invasion_pts_shp$data <= input$daterange[2], ]
    
    filtered_dafor_mrg_local <- dafor_mrg_local_shp[dafor_mrg_local_shp$data >= input$daterange[1] & dafor_mrg_local_shp$data <= input$daterange[2], ]%>%
      group_by(localidade) %>% 
      mutate(n_tr_pr_sum = sum(n_tr_pr)) #fix
    
    
    filtered_geo_mrg_local <- geo_mrg_local_shp[geo_mrg_local_shp$data >= input$daterange[1] & geo_mrg_local_shp$data <= input$daterange[2], ]
    
    
    #filtered_effort_mrg_local <- effort_mrg_local_shp[effort_mrg_local_shp$data >= input$daterange[1] & effort_mrg_local_shp$data <= input$daterange[2], ] %>% 
     # group_by(localidade) %>% 
      #mutate(n_tr_pr_1000 = round(sum(n_tr_pr)/(sum(comp_m)/1000),3))
    
    
    filtered_ntrans_mrg_local <- ntrans_mrg_local_shp[ntrans_mrg_local_shp$data >= input$daterange[1] & ntrans_mrg_local_shp$data <= input$daterange[2], ] %>% 
      group_by(localidade) %>% 
      mutate(n_trans = sum(max(n_trans)))
    
    
    filtered_days_after_mng_mrg_local <- days_after_mng_mrg_local[days_after_mng_mrg_local$data >= input$daterange[1] & days_after_mng_mrg_local$data <= input$daterange[2], ]
    
    
    filtered_days_since_check_mrg_local <- days_since_check_mrg_local[days_since_check_mrg_local$data >= input$daterange[1] & days_since_check_mrg_local$data <= input$daterange[2], ]
    
    
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
      #filtered_effort_mrg_local = filtered_effort_mrg_local,
      filtered_ntrans_mrg_local = filtered_ntrans_mrg_local,
      filtered_days_after_mng_mrg_local = filtered_days_after_mng_mrg_local,
      filtered_days_since_check_mrg_local = filtered_days_since_check_mrg_local,
      
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
      "Locations Monitored ", paste0(reactiveData()$n_location), icon = icon("location-dot"),
      color = "green"
    )
  })
  
  output$segments_Box <- renderInfoBox({
    infoBox(
      "Segments Monitored ", paste0(reactiveData()$n_segments), icon = icon("bacon"),
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
    
    #first version
    # if ("Occurrence" %in% input$layers && nrow(reactiveData()$filtered_occ) > 0) {
    #   leafletProxy("map", data = reactiveData()$filtered_occ) %>%
    #     addCircles(
    #       fillColor = "red",
    #       fillOpacity = 0.5,
    #       color = "red",
    #       weight = 8,
    #       popup =  ~paste0(
    #           "<strong>Locality: </strong> ", localidade, "<br>",
    #           "<strong>Date found: </strong> ", data, "<br>",
    #           "<div><a href='img/pts_invasao/", jpg_1, "' target='_blank'><img style='display: block; margin-left: auto; margin-right; auto;' src='img/pts_invasao/", jpg_1, "' width='100';></a></div>","<br",
    #           "<div><a href='img/pts_invasao/", jpg_2, "' target='_blank'><img style='display: block; margin-left: auto; margin-right: auto;' src='img/pts_invasao/", jpg_2, "' width='100';></a></div>"
    #           )
    #     )
    #       labelOptions = labelOptions(noHide = FALSE, direction = "right")
    # 
    # }
    
    #### teste 2
    # if ("Occurrence" %in% input$layers && nrow(reactiveData()$filtered_occ) > 0) {
    #   leafletProxy("map", data = reactiveData()$filtered_occ) %>%
    #     addCircles(
    #       fillColor = "red",
    #       fillOpacity = 0.5,
    #       color = "red",
    #       weight = 8,
    #       popup = ~{
    #         popupContent <- paste0(
    #           "<strong>Locality: </strong> ", localidade, "<br>",
    #           "<strong>Date found: </strong> ", data, "<br>"
    #         )
    # 
    #         if (grepl("\\.(jpg|jpeg)$", jpg_1, ignore.case = TRUE) && !is.na(jpg_1)) {
    #           popupContent <- lapply(jpg_1, function(jpg) {
    #             paste0(
    #               popupContent,
    #               "<div><a href='img/pts_invasao/", jpg, "' target='_blank'><img style='display: block; margin-left: auto; margin-right: auto;' src='img/pts_invasao/", jpg, "' width='100';></a></div><br>"
    #             )
    #           })
    #         }
    # 
    #         if (grepl("\\.(jpg|jpeg)$", jpg_2, ignore.case = TRUE) && !is.na(jpg_2)) {
    #           popupContent <- lapply(jpg_2, function(jpg) {
    #             paste0(
    #               popupContent,
    #               "<div><a href='img/pts_invasao/", jpg, "' target='_blank'><img style='display: block; margin-left: auto; margin-right: auto;' src='img/pts_invasao/", jpg, "' width='100';></a></div><br>"
    #             )
    #           })
    #         }
    # 
    #         return(popupContent)
    #       },
    #       labelOptions = labelOptions(noHide = FALSE, direction = "right")
    #     )
    # }
    # 
    
    
    ### code works but paste place holder
    
    if ("Occurrence" %in% input$layers && nrow(reactiveData()$filtered_occ) > 0) {
      leafletProxy("map", data = reactiveData()$filtered_occ) %>%
        #addCircles(
        addMarkers(
          #clusterOptions = markerClusterOptions(),
         # fillColor = "red",
          #fillOpacity = 0.5,
          #color = "red",
          #weight = 8,
          popup = ~{
            popupContent <- paste0(
              "<strong>Locality: </strong> ", localidade, "<br>",
              "<strong>Date found: </strong> ", data, "<br>",
              "<strong>Depth: </strong> " , prof_m, "<br>",
              "<strong>Accessibility: </strong> " , acesso, "<br>",
              "<strong>Lng/Lat: </strong> " , geometry, "<br>"
              )
            
          
            
            # Check if any element in jpg_1 contains ".jpg" or ".jpeg" and, if so, add it to the popup
            if (any(sapply(jpg_1, function(x) grepl("\\.(jpg|jpeg)$", x, ignore.case = TRUE)))) {
              popupContent <- paste0(popupContent,
                                     "<div><a href='img/pts_invasao/", jpg_1, "' target='_blank'><img style='display: block; margin-left: auto; margin-right: auto;' src='img/pts_invasao/", jpg_1, "' width='100'></a></div><br>"
              )
            }
            
            
            
            # Check if any element in jpg_2 contains ".jpg" or ".jpeg" and, if so, add it to the popup
            #if (any(sapply(jpg_2, function(x) grepl("\\.(jpg|jpeg)$", x, ignore.case = TRUE)))) {
            # popupContent <- paste0(popupContent,
            #                       "<div><a href='img/pts_invasao/", jpg_2, "' target='_blank'><img style='display: block; margin-left: auto; margin-right: auto;' src='img/pts_invasao/", jpg_2, "' width='100'></a></div><br>"
            #)
            #}
            
            return(popupContent)
          },
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
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
          popup = ~paste0("<strong>Locality: </strong> ", localidade, "<br>",
                          "<strong>HSI: </strong> ", iah_seg_avg) ,
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )%>%
        addLegend(
          pal = pal_iah,
          values = reactiveData()$filtered_geo_mrg_local$iah_seg_avg,
          position = "bottomright",
          title = ~paste0("HSI")
        )
    }
    
    
    #if ("TWSC/1000m" %in% input$indicators && nrow(reactiveData()$filtered_effort_mrg_local) > 0) {
      
     # pal_effort <- colorNumeric(
      #  palette = "Oranges",

      #  domain = reactiveData()$filtered_effort_mrg_local$n_tr_pr_1000

      #)
      
      
      #leafletProxy("map", data = reactiveData()$filtered_effort_mrg_local) %>%
       # addPolylines(
        #  fillColor = ~pal_effort(n_tr_pr_1000),
         # color = ~pal_effort(n_tr_pr_1000),

          #weight = 10,
          #popup = ~paste0("<strong>Locality: </strong> ", localidade, "<br>",
           #               "<strong>TWSC/1000: </strong> ", n_tr_pr_1000) ,
          #labelOptions = labelOptions(noHide = FALSE, direction = "right")
        #)%>%
        #addLegend(
         # pal = pal_effort,
          #values = reactiveData()$filtered_effort_mrg_local$n_tr_pr_1000,
          #position = "bottomright",
          #title = ~paste0("TWSC/1000m")
        #)
    #}

    
    
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
          popup = ~paste0("<strong>Locality: </strong> ", localidade, "<br>",
                          "<strong>N. of Transects: </strong> ", n_trans) ,
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )%>%
        addLegend(
          pal = pal_ntrans,
          values = reactiveData()$filtered_ntrans_mrg_local$n_trans,
          position = "bottomright",
          title = ~paste0("NTL")
        )
    }
    
    # if ("Days since last management(DSLManag)" %in% input$indicators && nrow(reactiveData()$filtered_days_after_mng_mrg_local) > 0) {
    #   
    #   pal_last_mng <- colorNumeric(
    #     palette = rev(c("#BE2A3E","#CD4242","#E76B49","#F7B059","#F5CA63","#EACF65","#D6CA64","#5DA25F","#449559","#22763F")),
    #     domain = reactiveData()$filtered_days_after_mng_mrg_local$days_since_last_record
    #   )
    #   
    #   leafletProxy("map", data = reactiveData()$filtered_days_after_mng_mrg_local) %>%
    #     addPolylines(
    #       fillColor = ~pal_last_mng(days_since_last_record),
    #       color = ~pal_last_mng(days_since_last_record),
    #       weight = 10,
    #       popup = ~paste0("<strong>Localidade:  <strong>", localidade, "<br>",
    #                       "<strong>Days since last management:  </strong> ", days_since_last_record, "<br>",
    #                       "<strong>Date of last management:  </strong> ", max_data),
    #       labelOptions = labelOptions(noHide = FALSE, direction = "right")
    #     )%>%
    #     addLegend(
    #       pal = pal_last_mng,
    #       values = reactiveData()$filtered_days_after_mng_mrg_loca$days_since_last_record,
    #       position = "bottomright",
    #       title = ~paste0("DSLManag")
    #     )
    # }
    
    if ("Days since last management(DSLManag)" %in% input$indicators && nrow(reactiveData()$filtered_days_after_mng_mrg_local) > 0) {
      
      pal_last_mng <- colorNumeric(
        palette = rev(c("#BE2A3E", "#CD4242", "#E76B49", "#F7B059", "#F5CA63", "#EACF65", "#D6CA64", "#5DA25F", "#449559", "#22763F")),
        domain = pmax(0, pmin(reactiveData()$filtered_days_after_mng_mrg_local$days_since_last_record, 365)),
        na.color = "#BE2A3E"
      )
      
      leafletProxy("map", data = reactiveData()$filtered_days_after_mng_mrg_local) %>%
        addPolylines(
          fillColor = ~pal_last_mng(days_since_last_record),
          color = ~pal_last_mng(days_since_last_record),
          weight = 10,
          popup = ~paste0("<strong>Localidade:  <strong>", localidade, "<br>",
                          "<strong>Days since last management:  </strong> ", days_since_last_record, "<br>",
                          "<strong>Date of last management:  </strong> ", max_data),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        ) %>%
        addLegend(
          pal = pal_last_mng,
          values = pmax(0, pmin(reactiveData()$filtered_days_after_mng_mrg_local$days_since_last_record, 365)),
          position = "bottomright",
          title = ~paste0("DSLManag"),
          bins = c(0, 50, 100, 150, 200, 250, 300, 350)#,
          #labFormat = labelFormat(prefix = c("","","","","","","",">"))
        )
    }
    
    
    
    if ("Days since last check(DSLCheck)" %in% input$indicators && nrow(reactiveData()$ filtered_days_since_check_mrg_local) > 0) {
      
      pal_last_check <- colorNumeric(
        palette = rev(c("#BE2A3E","#CD4242","#E76B49","#F7B059","#F5CA63","#EACF65","#D6CA64","#5DA25F","#449559","#22763F")),
        domain = pmax(0, pmin(reactiveData()$ filtered_days_since_check_mrg_local$days_since_last_check, 365)),
        na.color = "#BE2A3E"
      )
      
      leafletProxy("map", data = reactiveData()$ filtered_days_since_check_mrg_local) %>%
        addPolylines(
          fillColor = ~pal_last_check(days_since_last_check),
          color = ~pal_last_check(days_since_last_check),
          weight = 10,
          popup = ~paste0("<strong>Localidade:  <strong>", localidade, "<br>",
                          "<strong>Days since last check:  </strong> ", days_since_last_check, "<br>",
                          "<strong>Date of last check:  <strong>", max_data),
          labelOptions = labelOptions(noHide = FALSE, direction = "right")
        )%>%
        addLegend(
          pal = pal_last_check,
          values = pmax(0,  pmin(reactiveData()$filtered_days_after_mng_mrg_local$days_since_last_record, 365)),
          position = "bottomright",
          title = ~paste0("DSLCheck"), 
          bins = c(0, 50, 100, 150, 200, 250, 300, 350)#,
          #labFormat = labelFormat(prefix = c("","","","","","",">"))
        )
    }
    
    
    
    
  })
}

shinyApp(ui, server)