library(shiny)
library(tidyverse)
library(sf)
library(leaflet)

# 1. Load Data
nyc_shapes <- readRDS("data/nyc_census_tracts.rds")
model_data <- readRDS("data/model_input.rds")

# 2. Join them together for mapping
map_data <- nyc_shapes %>%
  inner_join(model_data %>% select(-any_of(colnames(nyc_shapes)[colnames(nyc_shapes) != "GEOID"])), 
             by = "GEOID") %>%
  st_transform(4326) 

# 3. Define UI
ui <- fillPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10, style = "z-index:500; background: rgba(255,255,255,0.9); padding: 15px; border-radius: 8px; width: 300px;",
                h3("NYC Last-Mile Lab"),
                
                selectInput("var", "Select Feature Layer:",
                            choices = c("Bike Preference Index" = "bike_index",
                                        "Median Income ($)" = "med_income",
                                        "Distance to Subway (Miles)" = "dist_subway_miles",
                                        "% Households without Car" = "pct_no_car",
                                        "% Commuting 60+ Mins" = "pct_commute_60p",
                                        "% Taking Transit" = "pct_transit",
                                        "Pop Density (Per Sq Mi)" = "pop_density")),
                
                # Replaced static text with dynamic UI
                uiOutput("dynamic_help"),
                hr(),
                
                # Upgraded text output for clicks
                h5("Tract Details (Click Map):"),
                htmlOutput("tract_info")
  )
)

# 4. Define Server
server <- function(input, output, session) {
  
  # Reactive color palette 
  colorpal <- reactive({
    # Use viridis, but reverse it for distance so 'closer' is brighter if needed
    colorNumeric("viridis", map_data[[input$var]], na.color = "transparent")
  })
  
  # Dynamic Help Text based on selection
  output$dynamic_help <- renderUI({
    desc <- switch(input$var,
                   "bike_index" = "Yellow indicates a high preference for Citi Bikes. Purple indicates a heavy reliance on Ubers/Taxis.",
                   "med_income" = "Yellow indicates higher median household income.",
                   "dist_subway_miles" = "Yellow indicates tracts that are further away from a subway entrance (Transit Deserts).",
                   "pct_no_car" = "Yellow indicates a high percentage of households without access to a personal vehicle.",
                   "pct_commute_60p" = "Yellow indicates high 'Transit Fatigue' (commutes taking longer than an hour).",
                   "pct_transit" = "Yellow indicates high baseline usage of public transit.",
                   "pop_density" = "Yellow indicates highly dense neighborhoods.")
    helpText(desc)
  })
  
  # Base Map
  output$map <- renderLeaflet({
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.97, lat = 40.75, zoom = 11)
  })
  
  # Update map polygons when variable changes
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
        highlightOptions = highlightOptions(weight = 3, color = "#222", fillOpacity = 0.9, bringToFront = TRUE),
        # Cleaner hover labels
        label = ~paste0(zone, " (", round(get(input$var), 2), ")"),
        layerId = ~GEOID # Crucial for the click event below
      ) %>%
      clearControls() %>%
      addLegend(pal = pal, values = ~get(input$var), opacity = 0.7, title = input$var, position = "bottomright")
  })
  
  # Map Click Event
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    
    # Find the data for the clicked tract
    selected_tract <- map_data %>% filter(GEOID == click$id)
    
    output$tract_info <- renderUI({
      HTML(paste0(
        "<b>Zone:</b> ", selected_tract$zone, "<br>",
        "<b>Value:</b> ", round(selected_tract[[input$var]], 2), "<br>",
        "<b>Total Pop:</b> ", format(selected_tract$total_pop, big.mark=","), "<br>",
        "<b>Median Income:</b> $", format(selected_tract$med_income, big.mark=",")
      ))
    })
  })
}

shinyApp(ui, server)