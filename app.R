library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(viridis)

# 1. Load Data
# We need the shapes (geometry) and the model features (data)
nyc_shapes <- readRDS("data/nyc_census_tracts.rds")
model_data <- readRDS("data/model_input.rds")

# 2. Join them together for mapping
# We use inner_join to only show tracts that have trip data
map_data <- nyc_shapes %>%
  inner_join(model_data %>% select(-any_of(colnames(nyc_shapes)[colnames(nyc_shapes) != "GEOID"])), 
             by = "GEOID") %>%
  st_transform(4326) # Leaflet requires WGS84 coordinates

# 3. Define UI
ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10, style = "z-index:500; background: rgba(255,255,255,0.8); padding: 10px; border-radius: 5px;",
                h4("NYC Last-Mile Lab"),
                selectInput("var", "View Layer:",
                            choices = c("Bike Preference Index" = "bike_index",
                                        "Total Citi Bike Stations" = "total_stations", # <- 1. ADDED HERE
                                        "Median Income" = "med_income",
                                        "Distance to Subway" = "dist_subway_miles",
                                        "% No Car" = "pct_no_car")),
                
                # <- 2. REPLACED STATIC TEXT WITH DYNAMIC UI
                uiOutput("dynamic_help"), 
                hr(),
                
                # <- 3. REPLACED textOutput WITH uiOutput FOR BOLD FORMATTING
                uiOutput("tract_info") 
  )
)

# 4. Define Server
server <- function(input, output, session) {
  
  # Reactive color palette that updates based on selection
  colorpal <- reactive({
    # Added na.color to prevent crashes if a tract has missing data
    colorNumeric("viridis", map_data[[input$var]], na.color = "#808080")
  })
  
  # <- 4. ADDED DYNAMIC HELP TEXT LOGIC
  output$dynamic_help <- renderUI({
    desc <- switch(input$var,
                   "bike_index" = "1.0 (Yellow) = 100% Bikes<br>0.0 (Purple) = 100% Ubers",
                   "total_stations" = "Yellow = High number of Citi Bike stations",
                   "med_income" = "Yellow = Higher median household income",
                   "dist_subway_miles" = "Yellow = Further distance from subway (Transit Desert)",
                   "pct_no_car" = "Yellow = Higher % of households without a car")
    HTML(paste0("<small>", desc, "</small>"))
  })
  
  output$map <- renderLeaflet({
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.97, lat = 40.75, zoom = 11)
  })
  
  # Observer to update the map polygons without re-rendering the whole map
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = map_data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(get(input$var)),
        weight = 0.5,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
        label = ~paste0(zone, ": ", round(get(input$var), 2)),
        layerId = ~GEOID
      ) %>%
      clearControls() %>%
      addLegend(pal = pal, values = ~get(input$var), opacity = 0.7, title = input$var, position = "bottomright")
  })
  
  # <- 5. ADDED MAP CLICK EVENT TO PRINT TRACT INFO
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    
    # Filter the map data to exactly the polygon clicked
    selected_tract <- map_data[map_data$GEOID == click$id, ]
    
    output$tract_info <- renderUI({
      # Check if total_stations exists in the data to prevent crashes
      stations <- ifelse("total_stations" %in% names(selected_tract), 
                         selected_tract$total_stations, 
                         "Data not loaded")
      
      HTML(paste0(
        "<b>Zone:</b> ", selected_tract$zone, "<br>",
        "<b>Current Layer Value:</b> ", round(selected_tract[[input$var]], 2), "<br>",
        "<b>Total Bike Stations:</b> ", stations
      ))
    })
  })
}

shinyApp(ui, server)