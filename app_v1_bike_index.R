library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# ==============================================================================
# 1. DATA LOADING & PREPARATION
# ==============================================================================
# We load the pristine geometry from the census file, and the math from the model file
geo_data <- readRDS("data/nyc_census_tracts.rds") %>% select(GEOID, geometry)
ml_data <- readRDS("data/model_input.rds")

# Join them together so our predictions have shapes!
map_data <- geo_data %>%
  inner_join(ml_data, by = "GEOID") %>%
  st_transform(4326) # Leaflet strictly requires standard GPS projection (EPSG:4326)

# ==============================================================================
# 2. USER INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel("NYC Micromobility & IBX Sensitivity Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Map Controls"),
      p("Select a layer below to explore current demographics, or view the predictive impact of building the Interborough Express (IBX)."),
      
      # Grouped dropdown for a super clean UI
      selectInput("map_var", "Select Map Layer:",
                  choices = list(
                    "🤖 IBX Sensitivity Analysis (Model Predictions)" = c(
                      "Predicted Biking (Current Baseline)" = "predicted_bike_baseline",
                      "Predicted Biking (With IBX Built)"   = "predicted_bike_ibx",
                      "The IBX Surge (Net Adoption Delta)"  = "ibx_adoption_surge"
                    ),
                    "🚲 Target & Infrastructure" = c(
                      "Actual Bike Preference Index"   = "bike_index",
                      "Distance to Subway (Miles)"     = "dist_subway_miles",
                      "IBX Station Locations (Added)"  = "is_ibx_location",
                      "Stations per Sq Mile (Density)" = "stations_per_sq_mile",
                      "Stations per 1000 Residents"    = "stations_per_1000_pop"
                    ),
                    "🏙️ Economics & Housing" = c(
                      "Median Income"        = "med_income",
                      "% Below Poverty Line" = "pct_poverty",
                      "% Renter Occupied"    = "pct_renter",
                      "% Vacant Housing"     = "pct_vacant"
                    ),
                    "🚶 Demographics & Commute" = c(
                      "% No Car"                 = "pct_no_car",
                      "% Transit Users"          = "pct_transit",
                      "% Extreme Commute (60m+)" = "pct_commute_60p",
                      "% Age 18-24"              = "pct_age_18_24",
                      "% Age 25-34"              = "pct_age_25_34",
                      "% Age 65+"                = "pct_age_65_plus"
                    )
                  )),
      
      hr(),
      helpText("Data Sources: 2022 ACS (5-Year), TLC Trip Records (Oct 2023), Citi Bike Trip Data (Oct 2023).")
    ),
    
    mainPanel(
      # The interactive map fills the main panel
      leafletOutput("nyc_map", height = "800px")
    )
  )
)
# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # A simple dictionary to translate variable names to clean Legend Titles
  legend_titles <- c(
    "predicted_bike_baseline" = "Predicted Biking (Baseline)",
    "predicted_bike_ibx"      = "Predicted Biking (With IBX)",
    "ibx_adoption_surge"      = "IBX Surge (Net Delta)",
    "is_ibx_location"         = "IBX Station Walksheds (0.5mi)",
    "bike_index"              = "Bike Preference Index",
    "dist_subway_miles"       = "Distance to Subway (Miles)",
    "stations_per_sq_mile"    = "Stations per Sq Mile",
    "stations_per_1000_pop"   = "Stations per 1000 Residents",
    "med_income"              = "Median Income ($)",
    "pct_poverty"             = "% Below Poverty",
    "pct_renter"              = "% Renter Occupied",
    "pct_vacant"              = "% Vacant Housing",
    "pct_no_car"              = "% No Car",
    "pct_transit"             = "% Transit Users",
    "pct_commute_60p"         = "% 60m+ Commute",
    "pct_age_18_24"           = "% Age 18-24",
    "pct_age_25_34"           = "% Age 25-34",
    "pct_age_65_plus"         = "% Age 65+"
  )
  
  # Initialize the Base Map (This only draws once so it stays fast)
  output$nyc_map <- renderLeaflet({
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.94, lat = 40.70, zoom = 11) 
  })
  
  # Reactive Observer: Updates the polygons and legend whenever the dropdown changes
  observe({
    selected_var <- input$map_var
    var_data <- map_data[[selected_var]]
    
    # 1. FORMATTING FIX: Convert decimals to percentages for the bike metrics
    if (selected_var %in% c("bike_index", "predicted_bike_baseline", "predicted_bike_ibx", "ibx_adoption_surge")) {
      var_data <- var_data * 100
      suffix_symbol <- "%"
    } else {
      suffix_symbol <- ""
    }
    
    # 2. Dynamic color palette
    if (selected_var == "is_ibx_location") {
      # Use a distinct color (like bright Blue) for the stations vs grey for everywhere else
      pal <- colorFactor(c("#e0e0e0", "#0072B2"), domain = c(0, 1))
    } else if (selected_var == "dist_subway_miles") {
      pal <- colorNumeric("magma", domain = var_data, reverse = TRUE)
    } else if (selected_var == "ibx_adoption_surge") {
      pal <- colorNumeric("YlGn", domain = var_data)
    } else {
      pal <- colorNumeric("magma", domain = var_data)
    }
    
    # 3. Update the map
    leafletProxy("nyc_map", data = map_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(var_data),
        fillOpacity = 0.7,
        color = "white",
        weight = 0.5,
        smoothFactor = 0.2,
        # Hover tooltip now automatically includes the % sign if needed!
        label = ~paste0(legend_titles[[selected_var]], ": ", round(var_data, 1), suffix_symbol), 
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = var_data,
        title = legend_titles[[selected_var]], 
        opacity = 0.7,
        # The transform function completely bypasses Leaflet's stubborn rounding bug
        labFormat = labelFormat(suffix = suffix_symbol, transform = function(x) round(x, 1))
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)