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
                                        "Median Income" = "med_income",
                                        "Distance to Subway" = "dist_subway_miles",
                                        "% No Car" = "pct_no_car")),
                helpText("1.0 (Yellow) = 100% Bikes"),
                helpText("0.0 (Purple) = 100% Ubers"),
                hr(),
                textOutput("tract_info")
  )
)

# 4. Define Server
server <- function(input, output, session) {
  
  # Reactive color palette that updates based on selection
  colorpal <- reactive({
    colorNumeric("viridis", map_data[[input$var]])
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
}

shinyApp(ui, server)