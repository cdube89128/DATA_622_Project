library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# ==============================================================================
# 1. DATA LOADING
# ==============================================================================
# Load shapes and the model output table
geo_data <- readRDS("data/nyc_census_tracts.rds") %>% select(GEOID, geometry)
ml_data <- readRDS("data/model_input.rds")

# Combine them and project to standard GPS for Leaflet
map_data <- geo_data %>%
  inner_join(ml_data, by = "GEOID") %>%
  st_transform(4326) 

# ==============================================================================
# 2. USER INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  titlePanel("The 60-Minute Wall: NYC Commute Equity & IBX Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Analysis Controls"),
      p("Explore how the Interborough Express (IBX) could break the '60-minute wall' for New York's most transit-isolated neighborhoods."),
      
      selectInput("map_var", "Select Data Layer:",
                  choices = list(
                    "⭐ BEST: IBX Simulation (RF + Spatial Lags)" = c(
                      "Predicted 60m+ Commutes (Current)"  = "predicted_commute_baseline_rf_spatial",
                      "Predicted 60m+ Commutes (With IBX)" = "predicted_commute_ibx_rf_spatial",
                      "Drop in Long-Commute Share (pp)"    = "ibx_commute_relief_rf_spatial"
                    ),
                    "📈 IBX Predictive Simulation (Linear)" = c(
                      "Predicted 60m+ Commutes (Current)" = "predicted_commute_baseline",
                      "Predicted 60m+ Commutes (With IBX)" = "predicted_commute_ibx",
                      "Drop in Long-Commute Share (pp)"  = "ibx_commute_relief"
                    ),
                    "〰️ IBX Predictive Simulation (Spline)" = c(
                      "Spline: Predicted 60m+ Commutes (Current)"  = "predicted_commute_baseline_spline",
                      "Spline: Predicted 60m+ Commutes (With IBX)" = "predicted_commute_ibx_spline",
                      "Spline: Drop in Long-Commute Share (pp)"  = "ibx_commute_relief_spline"
                    ),
                    "🌲 IBX Predictive Simulation (Random Forest)" = c(
                      "RF: Predicted 60m+ Commutes (Current)"  = "predicted_commute_baseline_rf",
                      "RF: Predicted 60m+ Commutes (With IBX)" = "predicted_commute_ibx_rf",
                      "RF: Drop in Long-Commute Share (pp)"  = "ibx_commute_relief_rf",
                      "RF: Residuals (Actual minus Predicted)"  = "rf_residual"
                    ),
                    "🌐 IBX with Spatial Lags (Linear)" = c(
                      "Predicted 60m+ Commutes (Current)"  = "predicted_commute_baseline_lm_spatial",
                      "Predicted 60m+ Commutes (With IBX)" = "predicted_commute_ibx_lm_spatial",
                      "Drop in Long-Commute Share (pp)"    = "ibx_commute_relief_lm_spatial"
                    ),
                    "🗺️ IBX with Spatial Lags (Spline)" = c(
                      "Predicted 60m+ Commutes (Current)"  = "predicted_commute_baseline_spline_spatial",
                      "Predicted 60m+ Commutes (With IBX)" = "predicted_commute_ibx_spline_spatial",
                      "Drop in Long-Commute Share (pp)"    = "ibx_commute_relief_spline_spatial"
                    ),
                    "⏱️ Existing Commute Burden" = c(
                      "Actual % of 60m+ Commutes"     = "pct_commute_60p",
                      "IBX 10-Minute Walk Radius"     = "is_ibx_location",
                      "Distance to Subway (Miles)"    = "dist_subway_miles"
                    ),
                    "🚲 Infrastructure & Mobility" = c(
                      "Uber/Taxi Trips per Capita"  = "uber_trips_per_capita",
                      "Citi Bike Trips per Capita"  = "bike_trips_per_capita",
                      "Citi Bike Station Density"   = "stations_per_sq_mile",
                      "Bike Access per 1k Residents" = "stations_per_1000_pop"
                    ),
                    "💰 Socioeconomic Indicators" = c(
                      "Median Household Income"     = "med_income",
                      "Poverty Rate (%)"            = "pct_poverty",
                      "Renter-Occupied Rate (%)"    = "pct_renter",
                      "Housing Vacancy Rate (%)"    = "pct_vacant"
                    ),
                    "👥 Neighborhood Composition" = c(
                      "Car-Free Households (%)"     = "pct_no_car",
                      "Public Transit Dependency (%)" = "pct_transit",
                      "Age Group: 18-24 (%)"        = "pct_age_18_24",
                      "Age Group: 25-34 (%)"        = "pct_age_25_34",
                      "Age Group: 65+ (%)"          = "pct_age_65_plus"
                    )
                  )),
      
      hr(),
      helpText("Sources: 2022 ACS 5-Year Estimates, TLC Oct 2023, Citi Bike Oct 2023, MTA IBX Planning Docs.")
    ),
    
    mainPanel(
      leafletOutput("nyc_map", height = "800px")
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # Legend and Hover Dictionary
  legend_titles <- c(
    "predicted_commute_baseline" = "Predicted 60m+ Commute Rate (Linear)",
    "predicted_commute_ibx"      = "Simulated 60m+ Commute (Post-IBX, Linear)",
    "ibx_commute_relief"         = "Drop in 60+ Min Commute Rate, Linear (pp)",
    "predicted_commute_baseline_spline" = "Predicted 60m+ Commute Rate (Spline)",
    "predicted_commute_ibx_spline"      = "Simulated 60m+ Commute (Post-IBX, Spline)",
    "ibx_commute_relief_spline"         = "Drop in 60+ Min Commute Rate, Spline (pp)",
    "predicted_commute_baseline_rf" = "Predicted 60m+ Commute Rate (RF)",
    "predicted_commute_ibx_rf"      = "Simulated 60m+ Commute (Post-IBX, RF)",
    "ibx_commute_relief_rf"         = "Drop in 60+ Min Commute Rate, RF (pp)",
    "predicted_commute_baseline_lm_spatial"     = "Linear+Lags: Predicted 60m+ Commute Rate",
    "predicted_commute_ibx_lm_spatial"          = "Linear+Lags: Simulated 60m+ Commute (Post-IBX)",
    "ibx_commute_relief_lm_spatial"             = "Linear+Lags: Drop in 60+ Min Commute Rate (pp)",
    "predicted_commute_baseline_spline_spatial" = "Spline+Lags: Predicted 60m+ Commute Rate",
    "predicted_commute_ibx_spline_spatial"      = "Spline+Lags: Simulated 60m+ Commute (Post-IBX)",
    "ibx_commute_relief_spline_spatial"         = "Spline+Lags: Drop in 60+ Min Commute Rate (pp)",
    "predicted_commute_baseline_rf_spatial"     = "RF+Lags: Predicted 60m+ Commute Rate",
    "predicted_commute_ibx_rf_spatial"          = "RF+Lags: Simulated 60m+ Commute (Post-IBX)",
    "ibx_commute_relief_rf_spatial"             = "RF+Lags: Drop in 60+ Min Commute Rate (pp)",
    "rf_residual"                   = "RF Residuals (Actual − Predicted)",
    "pct_commute_60p"            = "Actual 60m+ Commute Rate",
    "is_ibx_location"            = "IBX Station Area (0.5mi)",
    "dist_subway_miles"          = "Distance to Subway (Miles)",
    "uber_trips_per_capita"      = "Uber/Taxi per Capita",
    "bike_trips_per_capita"      = "Bike Trips per Capita",
    "stations_per_sq_mile"       = "Bike Stations / Sq Mi",
    "stations_per_1000_pop"      = "Bike Access / 1k Residents",
    "med_income"                 = "Median Income",
    "pct_poverty"                = "Poverty Rate",
    "pct_renter"                 = "Renter-Occupied Rate",
    "pct_vacant"                 = "Housing Vacancy Rate",
    "pct_no_car"                 = "Car-Free Households",
    "pct_transit"                = "Transit Usage Rate",
    "pct_age_18_24"              = "Pop. Age 18-24",
    "pct_age_25_34"              = "Pop. Age 25-34",
    "pct_age_65_plus"            = "Pop. Age 65+"
  )
  
  # Base Map
  output$nyc_map <- renderLeaflet({
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -73.94, lat = 40.70, zoom = 11) 
  })
  
  observe({
    # SAFETY GATE 1: Wait for input
    req(input$map_var)
    
    selected_var <- input$map_var
    var_data <- map_data[[selected_var]]
    
    # SAFETY GATE 2: Handle NAs and Non-numeric columns
    if (!is.numeric(var_data) && selected_var != "is_ibx_location") return(NULL)
    
    # Formatting symbols
    suffix_symbol <- ifelse(grepl("ibx_commute_relief", selected_var), " pp",
                       ifelse(grepl("pct_|commute", selected_var), "%", ""))
    prefix_symbol <- ifelse(selected_var == "med_income", "$", "")
    
    # Color Logic
    if (selected_var == "is_ibx_location") {
      pal <- colorFactor(c("#e0e0e0", "#d73027"), domain = c(0, 1))
    } else if (selected_var %in% c("predicted_commute_baseline_rf", "predicted_commute_ibx_rf",
                                     "predicted_commute_baseline_rf_spatial", "predicted_commute_ibx_rf_spatial")) {
      pal <- colorNumeric("Blues", domain = var_data, na.color = "transparent")
    } else if (selected_var %in% c("ibx_commute_relief_rf", "ibx_commute_relief_rf_spatial")) {
      pal <- colorNumeric("viridis", domain = var_data, reverse = TRUE, na.color = "transparent")
    } else if (selected_var == "rf_residual") {
      pal <- colorNumeric("RdBu", domain = var_data, na.color = "transparent")
    } else if (selected_var %in% c("ibx_commute_relief", "ibx_commute_relief_lm_spatial")) {
      pal <- colorNumeric("YlGn", domain = var_data, na.color = "transparent")
    } else if (selected_var %in% c("predicted_commute_baseline_spline", "predicted_commute_ibx_spline",
                                     "predicted_commute_baseline_spline_spatial", "predicted_commute_ibx_spline_spatial")) {
      pal <- colorNumeric("Greens", domain = var_data, na.color = "transparent")
    } else if (selected_var %in% c("ibx_commute_relief_spline", "ibx_commute_relief_spline_spatial")) {
      pal <- colorNumeric("Oranges", domain = var_data, na.color = "transparent")
    } else if (selected_var == "dist_subway_miles") {
      pal <- colorNumeric("viridis", domain = var_data, reverse = TRUE, na.color = "transparent")
    } else {
      pal <- colorNumeric("magma", domain = var_data, na.color = "transparent")
    }
    
    # SAFETY GATE 3: Final check before rounding/drawing
    # Rounding only happens inside the labels to prevent data corruption
    leafletProxy("nyc_map", data = map_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(var_data),
        fillOpacity = 0.7,
        color = "white",
        weight = 0.5,
        label = ~paste0(legend_titles[[selected_var]], ": ", prefix_symbol, round(as.numeric(var_data), 1), suffix_symbol), 
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = var_data,
        title = legend_titles[[selected_var]], 
        opacity = 0.7,
        labFormat = labelFormat(
          prefix = prefix_symbol, 
          suffix = suffix_symbol, 
          transform = function(x) round(as.numeric(x), 1)
        )
      )
  })
}

shinyApp(ui = ui, server = server)