library(shiny)
library(httr)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(leaflet)
library(geosphere)

# UI definition with enhanced features
ui <- fluidPage(
  titlePanel("🌍 Smart Carbon Journey Planner"),
  
  tags$head(
    tags$style(HTML("
      .navbar { background-color: #2c3e50; }
      .eco-card { 
        background: linear-gradient(135deg, #28a745, #20c997); 
        color: white; 
        border-radius: 10px; 
        padding: 15px; 
        margin: 10px 0; 
      }
      .warning-card { 
        background: linear-gradient(135deg, #ffc107, #fd7e14); 
        color: white; 
        border-radius: 10px; 
        padding: 15px; 
        margin: 10px 0; 
      }
      .info-card { 
        background: linear-gradient(135deg, #17a2b8, #007bff); 
        color: white; 
        border-radius: 10px; 
        padding: 15px; 
        margin: 10px 0; 
      }
      .carbon-score { 
        font-size: 2em; 
        font-weight: bold; 
        text-align: center; 
      }
      .savings-highlight { 
        background-color: #d4edda; 
        border: 1px solid #c3e6cb; 
        border-radius: 5px; 
        padding: 10px; 
        margin: 10px 0; 
      }
      .metric-box {
        background-color: #f8f9fa;
        border: 2px solid #dee2e6;
        border-radius: 8px;
        padding: 20px;
        margin: 10px 0;
        text-align: center;
      }
      .metric-value {
        font-size: 2em;
        font-weight: bold;
        color: #007bff;
      }
      .btn-custom {
        background: linear-gradient(135deg, #007bff, #0056b3);
        border: none;
        color: white;
        padding: 12px 24px;
        border-radius: 6px;
        font-weight: bold;
      }
    "))
  ),
  
  navbarPage("Smart Carbon Planner",
    
    # Journey Planner Tab
    tabPanel("🚗 Journey Planner",
      fluidRow(
        column(4,
          wellPanel(
            h3("📍 Journey Details"),
            textInput("from", "From:", placeholder = "Bratislava, London, New York...", value = ""),
            textInput("to", "To:", placeholder = "Prague, Paris, Tokyo...", value = ""),
            p("💡 Try: Bratislava, Prague, Vienna, London, Paris, New York, Tokyo...", 
              style = "font-size: 11px; color: #6c757d; margin-top: -5px;"),
            br(),
            dateInput("travel_date", "Travel Date:", value = Sys.Date()),
            textInput("travel_time", "Departure Time:", value = "08:00", placeholder = "HH:MM"),
            br(),
            sliderInput("passengers", "Number of Passengers:", 
                       min = 1, max = 8, value = 1, step = 1),
            checkboxInput("return_trip", "Round Trip", value = FALSE),
            br(),
            h4("🚊 Transport Options"),
            checkboxGroupInput("transport_types", "",
              choices = list(
                "🚗 Car (Petrol)" = "car_petrol",
                "🚗 Car (Diesel)" = "car_diesel", 
                "⚡ Car (Electric)" = "car_electric",
                "🚌 Bus" = "bus",
                "🚂 Train" = "train",
                "✈️ Plane" = "plane",
                "🏍️ Motorcycle" = "motorcycle",
                "🚲 E-Bike" = "ebike",
                "🛴 E-Scooter" = "escooter"
              ),
              selected = c("car_petrol", "bus", "train", "plane")
            ),
            br(),
            actionButton("calculate_advanced", "🔍 Analyze Journey", 
                        class = "btn-custom", style = "width: 100%;")
          )
        ),
        
        column(8,
          wellPanel(
            h3("📊 Smart Results"),
            conditionalPanel(
              condition = "output.results_available",
              plotOutput("smart_emission_plot", height = "400px"),
              br(),
              div(class = "savings-highlight",
                h4("💡 Smart Recommendations"),
                htmlOutput("smart_recommendations")
              )
            ),
            conditionalPanel(
              condition = "!output.results_available",
              div(class = "info-card",
                h4("👋 Welcome!"),
                p("Enter your journey details and click 'Analyze Journey' to get started."),
                p("🌟 Get personalized recommendations for eco-friendly travel!")
              )
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = "output.results_available",
        fluidRow(
          column(6,
            wellPanel(
              h4("🌍 Environmental Impact"),
              tableOutput("environmental_table")
            )
          ),
          column(6,
            wellPanel(
              h4("💰 Cost Analysis"),
              tableOutput("cost_table")
            )
          )
        ),
        
        fluidRow(
          column(6,
            wellPanel(
              h4("🗺️ Interactive Route Map"),
              conditionalPanel(
                condition = "output.map_available",
                leafletOutput("advanced_map", height = "400px")
              ),
              conditionalPanel(
                condition = "!output.map_available",
                div(class = "info-card",
                  h4("📍 Route Information"),
                  htmlOutput("route_info"),
                  br(),
                  p("🗺️ Map will appear after location lookup completes.")
                )
              )
            )
          ),
          column(6,
            wellPanel(
              h4("📊 Journey Summary"),
              conditionalPanel(
                condition = "output.results_available",
                div(class = "metric-box",
                  div(class = "metric-value", textOutput("distance_display", inline = TRUE)),
                  h5("Total Distance"),
                  p("Direct line between locations")
                ),
                br(),
                div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                  h5("🌍 Location Details"),
                  htmlOutput("location_details")
                )
              )
            )
          )
        )
      )
    ),
    
    # Dashboard Tab
    tabPanel("📊 Dashboard",
      fluidRow(
        column(3,
          div(class = "metric-box",
            div(class = "metric-value", "2.1t"),
            h5("Annual CO2"),
            p("🌱 25% below EU average")
          )
        ),
        column(3,
          div(class = "metric-box",
            div(class = "metric-value", "47"),
            h5("Trips This Year"),
            p("📈 12% increase from last year")
          )
        ),
        column(3,
          div(class = "metric-box",
            div(class = "metric-value", "380kg"),
            h5("CO2 Saved"),
            p("🏆 Equivalent to 19 trees")
          )
        ),
        column(3,
          div(class = "metric-box",
            div(class = "metric-value", "🥉"),
            h5("Eco Level"),
            p("Bronze - 500pts to Silver")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        column(8,
          wellPanel(
            h4("📈 Monthly Carbon Trends"),
            plotOutput("carbon_trends", height = "300px")
          )
        ),
        column(4,
          wellPanel(
            h4("🚊 Transport Mix"),
            plotOutput("transport_donut", height = "300px")
          )
        )
      ),
      
      fluidRow(
        column(12,
          div(class = "eco-card",
            h4("🌍 Your Impact"),
            fluidRow(
              column(4,
                h5("🎯 vs EU Average"),
                p("You: 2.1t CO2/year"),
                p("EU Avg: 2.8t CO2/year"),
                p("You're 25% better! 🌟")
              ),
              column(4,
                h5("🌱 Carbon Offset"),
                p("Trees equivalent: 105 trees"),
                p("Solar panels: 0.8m² needed"),
                p("Wind power: 1.2 MWh/year")
              ),
              column(4,
                h5("📊 This Month"),
                p("Trips: 8 journeys"),
                p("Best choice: 🚂 Train (60%)"),
                p("Savings: 45kg CO2 vs car")
              )
            )
          )
        )
      )
    ),
    
    # Carbon Offset Tab
    tabPanel("🌱 Carbon Offset",
      fluidRow(
        column(6,
          wellPanel(
            h3("🌱 Offset Calculator"),
            p("Offset your travel emissions and support environmental projects!"),
            br(),
            numericInput("offset_amount", "CO2 to Offset (kg):", 
                        value = 50, min = 1, max = 10000, step = 1),
            br(),
            radioButtons("offset_project", "Choose Project:",
              choices = list(
                "🌳 Forest Restoration (€15/ton)" = "forest",
                "💨 Wind Energy (€12/ton)" = "wind", 
                "☀️ Solar Power (€18/ton)" = "solar",
                "🌊 Ocean Conservation (€25/ton)" = "ocean"
              ),
              selected = "forest"
            ),
            br(),
            div(style = "text-align: center;",
              h3("Total Cost: €", textOutput("offset_cost", inline = TRUE), 
                 style = "color: #28a745;"),
              br(),
              actionButton("purchase_offset", "🛒 Calculate Impact", 
                          class = "btn-custom")
            )
          )
        ),
        
        column(6,
          wellPanel(
            h3("🎯 Project Impact"),
            conditionalPanel(
              condition = "input.offset_project == 'forest'",
              div(class = "info-card",
                h4("🌳 Forest Restoration"),
                p("📍 Location: Amazon Rainforest, Brazil"),
                p("🎯 Impact: Plants 50 trees per ton CO2"),
                p("⏱️ Duration: 25 years carbon storage"),
                p("🏆 Certification: Gold Standard")
              )
            ),
            conditionalPanel(
              condition = "input.offset_project == 'wind'",
              div(class = "info-card",
                h4("💨 Wind Energy"),
                p("📍 Location: North Sea Wind Farm"),
                p("🎯 Impact: Clean energy generation"),
                p("⏱️ Duration: 20 years operation"),
                p("🏆 Certification: VCS Verified")
              )
            ),
            conditionalPanel(
              condition = "input.offset_project == 'solar'",
              div(class = "info-card",
                h4("☀️ Solar Power"),
                p("📍 Location: Sahara Solar Project"),
                p("🎯 Impact: Renewable electricity"),
                p("⏱️ Duration: 25 years generation"),
                p("🏆 Certification: CDM Certified")
              )
            ),
            conditionalPanel(
              condition = "input.offset_project == 'ocean'",
              div(class = "info-card",
                h4("🌊 Ocean Conservation"),
                p("📍 Location: Great Barrier Reef"),
                p("🎯 Impact: Marine ecosystem protection"),
                p("⏱️ Duration: Permanent conservation"),
                p("🏆 Certification: Blue Carbon Standard")
              )
            ),
            br(),
            h4("Your Contribution:"),
            htmlOutput("offset_impact")
          )
        )
      ),
      
      fluidRow(
        column(12,
          wellPanel(
            h3("💡 Alternative Green Actions"),
            p("Can't offset right now? Here are other ways to reduce your carbon footprint:"),
            fluidRow(
              column(3,
                div(class = "warning-card",
                  h5("🏠 Home Energy"),
                  p("Switch to LED bulbs"),
                  p("💾 Save: 50kg CO2/year")
                )
              ),
              column(3,
                div(class = "eco-card",
                  h5("🥗 Diet Change"),
                  p("Meat-free Mondays"),
                  p("💾 Save: 150kg CO2/year")
                )
              ),
              column(3,
                div(class = "info-card",
                  h5("🚲 Cycling"),
                  p("Bike to work 2x/week"),
                  p("💾 Save: 300kg CO2/year")
                )
              ),
              column(3,
                div(class = "warning-card", style = "background: linear-gradient(135deg, #dc3545, #e74c3c);",
                  h5("✈️ Flight Reduction"),
                  p("One less flight/year"),
                  p("💾 Save: 500kg CO2/year")
                )
              )
            )
          )
        )
      )
    ),
    
    # Eco Challenges Tab
    tabPanel("🏆 Eco Challenges",
      fluidRow(
        column(4,
          div(class = "eco-card",
            h3("🏆 Current Challenge"),
            h4("Green Week Challenge"),
            p("🎯 Goal: Use only public transport for 7 days"),
            p("📅 Progress: 3/7 days completed"),
            div(style = "background-color: rgba(255,255,255,0.3); border-radius: 10px; height: 20px; position: relative;",
              div(style = "background-color: white; height: 100%; width: 43%; border-radius: 10px;")
            ),
            br(),
            p("🏆 Reward: 500 EcoPoints + Tree Badge")
          )
        ),
        
        column(4,
          div(class = "info-card",
            h3("🌟 Achievements"),
            h5("🚲 Bike Warrior ✅"),
            p("Complete 50 bike trips"),
            p("Unlocked: 2024-08-15", style = "font-size: 12px; opacity: 0.8;"),
            hr(style = "border-color: rgba(255,255,255,0.3);"),
            h5("🌱 Carbon Neutral 🔄"),
            p("Offset 1 ton of CO2"),
            p("Progress: 780/1000 kg", style = "font-size: 12px; opacity: 0.8;"),
            hr(style = "border-color: rgba(255,255,255,0.3);"),
            h5("🚂 Train Master 🔒"),
            p("Take 100 train journeys"),
            p("Progress: 23/100", style = "font-size: 12px; opacity: 0.8;")
          )
        ),
        
        column(4,
          div(class = "warning-card",
            h3("📈 EcoPoints"),
            div(class = "carbon-score", "2,450"),
            p("🥉 Bronze Level"),
            p("Next Level: 500 points to go"),
            hr(style = "border-color: rgba(255,255,255,0.3);"),
            h5("Recent Points:"),
            p("✅ Train trip: +50 pts"),
            p("🚲 Bike commute: +25 pts"),
            p("🌱 Carbon offset: +100 pts")
          )
        )
      ),
      
      br(),
      
      fluidRow(
        column(12,
          wellPanel(
            h3("🎯 Available Challenges"),
            fluidRow(
              column(4,
                div(style = "border: 2px solid #28a745; border-radius: 8px; padding: 15px;",
                  h4("🚲 Bike Week"),
                  p("Cycle for all short trips (< 5km) for one week"),
                  p("🏆 Reward: 300 EcoPoints"),
                  actionButton("start_bike_challenge", "Start Challenge", class = "btn-custom")
                )
              ),
              column(4,
                div(style = "border: 2px solid #007bff; border-radius: 8px; padding: 15px;",
                  h4("🚂 Train Explorer"),
                  p("Take 5 train journeys this month"),
                  p("🏆 Reward: 200 EcoPoints + Badge"),
                  actionButton("start_train_challenge", "Start Challenge", class = "btn-custom")
                )
              ),
              column(4,
                div(style = "border: 2px solid #ffc107; border-radius: 8px; padding: 15px;",
                  h4("🌱 Offset Hero"),
                  p("Offset 100kg of CO2 this month"),
                  p("🏆 Reward: 500 EcoPoints + Tree Badge"),
                  actionButton("start_offset_challenge", "Start Challenge", class = "btn-custom")
                )
              )
            )
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Geocoding function with fallback to major cities
  geocode_address <- function(address) {
    if (address == "" || is.null(address)) return(NULL)
    
    # Fallback coordinates for major cities
    city_coords <- list(
      # Europe
      "bratislava" = list(lat = 48.1486, lng = 17.1077, name = "Bratislava, Slovakia"),
      "prague" = list(lat = 50.0755, lng = 14.4378, name = "Prague, Czech Republic"),
      "vienna" = list(lat = 48.2082, lng = 16.3738, name = "Vienna, Austria"),
      "budapest" = list(lat = 47.4979, lng = 19.0402, name = "Budapest, Hungary"),
      "warsaw" = list(lat = 52.2297, lng = 21.0122, name = "Warsaw, Poland"),
      "berlin" = list(lat = 52.5200, lng = 13.4050, name = "Berlin, Germany"),
      "paris" = list(lat = 48.8566, lng = 2.3522, name = "Paris, France"),
      "london" = list(lat = 51.5074, lng = -0.1278, name = "London, UK"),
      "rome" = list(lat = 41.9028, lng = 12.4964, name = "Rome, Italy"),
      "madrid" = list(lat = 40.4168, lng = -3.7038, name = "Madrid, Spain"),
      "amsterdam" = list(lat = 52.3676, lng = 4.9041, name = "Amsterdam, Netherlands"),
      "zurich" = list(lat = 47.3769, lng = 8.5417, name = "Zurich, Switzerland"),
      # USA
      "new york" = list(lat = 40.7128, lng = -74.0060, name = "New York, NY, USA"),
      "los angeles" = list(lat = 34.0522, lng = -118.2437, name = "Los Angeles, CA, USA"),
      "chicago" = list(lat = 41.8781, lng = -87.6298, name = "Chicago, IL, USA"),
      "san francisco" = list(lat = 37.7749, lng = -122.4194, name = "San Francisco, CA, USA"),
      # Asia
      "tokyo" = list(lat = 35.6762, lng = 139.6503, name = "Tokyo, Japan"),
      "seoul" = list(lat = 37.5665, lng = 126.9780, name = "Seoul, South Korea"),
      "beijing" = list(lat = 39.9042, lng = 116.4074, name = "Beijing, China"),
      "singapore" = list(lat = 1.3521, lng = 103.8198, name = "Singapore"),
      # Others
      "sydney" = list(lat = -33.8688, lng = 151.2093, name = "Sydney, Australia"),
      "toronto" = list(lat = 43.6511, lng = -79.3470, name = "Toronto, Canada")
    )
    
    # Check if it's a known city
    address_lower <- tolower(trimws(address))
    for (city in names(city_coords)) {
      if (grepl(city, address_lower)) {
        return(city_coords[[city]])
      }
    }
    
    # Try geocoding API
    tryCatch({
      base_url <- "https://nominatim.openstreetmap.org/search"
      response <- GET(
        base_url,
        query = list(
          q = address,
          format = "json",
          limit = 1
        ),
        user_agent("CarbonFootprintApp/1.0")
      )
      
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text"))
        if (length(data) > 0) {
          return(list(
            lat = as.numeric(data$lat[1]),
            lng = as.numeric(data$lon[1]),
            name = data$display_name[1]
          ))
        }
      }
      return(NULL)
    }, error = function(e) {
      # If API fails, try partial match with city list
      for (city in names(city_coords)) {
        if (grepl(city, address_lower, fixed = TRUE)) {
          return(city_coords[[city]])
        }
      }
      return(NULL)
    })
  }
  emission_factors <- reactive({
    list(
      car_petrol = list(co2 = 0.192, cost_per_km = 0.12, nox = 0.025, pm = 0.003),
      car_diesel = list(co2 = 0.171, cost_per_km = 0.10, nox = 0.045, pm = 0.008),
      car_electric = list(co2 = 0.053, cost_per_km = 0.08, nox = 0.001, pm = 0.001),
      bus = list(co2 = 0.105, cost_per_km = 0.05, nox = 0.015, pm = 0.004),
      train = list(co2 = 0.041, cost_per_km = 0.07, nox = 0.002, pm = 0.001),
      plane = list(co2 = 0.255, cost_per_km = 0.15, nox = 0.035, pm = 0.002),
      motorcycle = list(co2 = 0.113, cost_per_km = 0.08, nox = 0.020, pm = 0.003),
      ebike = list(co2 = 0.022, cost_per_km = 0.02, nox = 0.001, pm = 0.000),
      escooter = list(co2 = 0.031, cost_per_km = 0.03, nox = 0.001, pm = 0.000)
    )
  })
  
  # Reactive values for calculations
  values <- reactiveValues(
    results = NULL,
    distance = NULL,
    smart_recommendations = "",
    results_calculated = FALSE,
    from_coords = NULL,
    to_coords = NULL
  )
  
  # Check if map is available
  output$map_available <- reactive({
    return(!is.null(values$from_coords) && !is.null(values$to_coords))
  })
  outputOptions(output, "map_available", suspendWhenHidden = FALSE)
  
  # Display distance
  output$distance_display <- renderText({
    if (!is.null(values$distance)) {
      paste(values$distance, "km")
    } else {
      "- km"
    }
  })
  
  # Route info for when map is not available
  output$route_info <- renderText({
    if (!is.null(values$from_coords) && !is.null(values$to_coords)) {
      paste0(
        "<p><strong>From:</strong> ", input$from, "</p>",
        "<p><strong>To:</strong> ", input$to, "</p>",
        "<p><strong>Distance:</strong> ", values$distance, " km</p>"
      )
    } else {
      "<p>Enter locations and click 'Analyze Journey' to see route details.</p>"
    }
  })
  
  # Location details
  output$location_details <- renderText({
    if (!is.null(values$from_coords) && !is.null(values$to_coords)) {
      paste0(
        "<p><strong>📍 From:</strong><br>", 
        "<small>", values$from_coords$name, "</small></p>",
        "<p><strong>🎯 To:</strong><br>", 
        "<small>", values$to_coords$name, "</small></p>",
        "<p><strong>⏱️ Travel Date:</strong> ", input$travel_date, "</p>",
        "<p><strong>👥 Passengers:</strong> ", input$passengers, "</p>"
      )
    } else {
      "<p>Location details will appear here after analysis.</p>"
    }
  })
  output$results_available <- reactive({
    return(values$results_calculated)
  })
  outputOptions(output, "results_available", suspendWhenHidden = FALSE)
  
  # Enhanced calculation for main planner
  observeEvent(input$calculate_advanced, {
    req(input$from, input$to)
    
    showNotification("🔍 Looking up locations...", type = "message", duration = 2)
    
    # Geocode addresses
    values$from_coords <- geocode_address(input$from)
    values$to_coords <- geocode_address(input$to)
    
    if (is.null(values$from_coords)) {
      showNotification(paste("❌ Could not find location:", input$from), type = "error", duration = 5)
      return()
    }
    
    if (is.null(values$to_coords)) {
      showNotification(paste("❌ Could not find location:", input$to), type = "error", duration = 5)
      return()
    }
    
    # Calculate distance using geosphere
    values$distance <- round(distHaversine(
      c(values$from_coords$lng, values$from_coords$lat),
      c(values$to_coords$lng, values$to_coords$lat)
    ) / 1000, 1)  # Convert to km
    
    showNotification(paste("📏 Distance calculated:", values$distance, "km"), type = "message", duration = 3)
    
    # Calculate results
    results <- data.frame()
    factors <- emission_factors()
    
    for (transport in input$transport_types) {
      if (transport %in% names(factors)) {
        factor_data <- factors[[transport]]
        distance_adj <- values$distance * ifelse(input$return_trip, 2, 1)
        
        co2_total <- distance_adj * factor_data$co2 / input$passengers
        cost_total <- distance_adj * factor_data$cost_per_km
        
        results <- rbind(results, data.frame(
          Transport = transport,
          CO2_kg = round(co2_total, 2),
          Cost = round(cost_total, 2),
          Distance = distance_adj,
          NOx = round(distance_adj * factor_data$nox, 3),
          PM = round(distance_adj * factor_data$pm, 3)
        ))
      }
    }
    
    values$results <- results
    values$results_calculated <- TRUE
    
    # Generate smart recommendations
    if (nrow(results) > 1) {
      best_co2 <- results[which.min(results$CO2_kg), ]
      best_cost <- results[which.min(results$Cost), ]
      worst_co2 <- results[which.max(results$CO2_kg), ]
      
      co2_savings <- round(worst_co2$CO2_kg - best_co2$CO2_kg, 1)
      cost_savings <- round(best_cost$Cost, 2)
      
      values$smart_recommendations <- paste0(
        "<div style='padding: 10px;'>",
        "🌟 <strong>Eco Champion:</strong> ", 
        c("🚗 Car (Petrol)" = "car_petrol", "🚗 Car (Diesel)" = "car_diesel",
          "⚡ Car (Electric)" = "car_electric", "🚌 Bus" = "bus",
          "🚂 Train" = "train", "✈️ Plane" = "plane",
          "🏍️ Motorcycle" = "motorcycle", "🚲 E-Bike" = "ebike",
          "🛴 E-Scooter" = "escooter")[best_co2$Transport], 
        " saves <strong>", co2_savings, " kg CO2</strong><br>",
        "💰 <strong>Budget Winner:</strong> Only €", cost_savings, " total cost<br>",
        "👥 <strong>Group Travel:</strong> ", input$passengers, " passenger(s) = lower per-person impact<br>",
        "🏆 <strong>Pro Tip:</strong> Choose train for the best balance of cost, comfort, and environment!",
        "</div>"
      )
    }
    
    showNotification("✅ Analysis complete! Check your results below.", type = "message", duration = 3)
  })
  
  # Smart emission plot
  output$smart_emission_plot <- renderPlot({
    req(values$results)
    
    transport_labels <- c(
      "car_petrol" = "🚗 Car (Petrol)", "car_diesel" = "🚗 Car (Diesel)",
      "car_electric" = "⚡ Car (Electric)", "bus" = "🚌 Bus",
      "train" = "🚂 Train", "plane" = "✈️ Plane",
      "motorcycle" = "🏍️ Motorcycle", "ebike" = "🚲 E-Bike",
      "escooter" = "🛴 E-Scooter"
    )
    
    plot_data <- values$results %>%
      mutate(
        Transport_Label = transport_labels[Transport],
        Color = case_when(
          CO2_kg < 10 ~ "#28a745",   # Green
          CO2_kg < 50 ~ "#ffc107",   # Yellow  
          CO2_kg < 100 ~ "#fd7e14",  # Orange
          TRUE ~ "#dc3545"           # Red
        )
      )
    
    ggplot(plot_data, aes(x = reorder(Transport_Label, CO2_kg), y = CO2_kg, fill = Color)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste(CO2_kg, "kg")), hjust = -0.1, size = 4, fontface = "bold") +
      scale_fill_identity() +
      coord_flip() +
      labs(
        title = paste("🌍 Environmental Impact Comparison -", values$distance, "km journey"),
        x = "", 
        y = "CO2 Emissions (kg)",
        caption = paste("Passengers:", input$passengers, "| Return trip:", ifelse(input$return_trip, "Yes", "No"))
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        plot.caption = element_text(size = 10, color = "gray60"),
        panel.grid.minor = element_blank()
      ) +
      expand_limits(y = max(plot_data$CO2_kg) * 1.2)
  })
  
  # Smart recommendations output
  output$smart_recommendations <- renderText({
    values$smart_recommendations
  })
  
  # Environmental impact table
  output$environmental_table <- renderTable({
    req(values$results)
    
    transport_labels <- c(
      "car_petrol" = "🚗 Car (Petrol)", "car_diesel" = "🚗 Car (Diesel)",
      "car_electric" = "⚡ Car (Electric)", "bus" = "🚌 Bus",
      "train" = "🚂 Train", "plane" = "✈️ Plane",
      "motorcycle" = "🏍️ Motorcycle", "ebike" = "🚲 E-Bike",
      "escooter" = "🛴 E-Scooter"
    )
    
    values$results %>%
      mutate(
        Transport = transport_labels[Transport],
        `CO2 (kg)` = CO2_kg,
        `NOx (g)` = round(NOx * 1000, 1),
        `PM (g)` = round(PM * 1000, 2)
      ) %>%
      select(Transport, `CO2 (kg)`, `NOx (g)`, `PM (g)`)
  }, digits = 2)
  
  # Cost analysis table
  output$cost_table <- renderTable({
    req(values$results)
    
    transport_labels <- c(
      "car_petrol" = "🚗 Car (Petrol)", "car_diesel" = "🚗 Car (Diesel)",
      "car_electric" = "⚡ Car (Electric)", "bus" = "🚌 Bus",
      "train" = "🚂 Train", "plane" = "✈️ Plane",
      "motorcycle" = "🏍️ Motorcycle", "ebike" = "🚲 E-Bike",
      "escooter" = "🛴 E-Scooter"
    )
    
    values$results %>%
      mutate(
        Transport = transport_labels[Transport],
        `Total Cost (€)` = Cost,
        `Cost per km (€)` = round(Cost / Distance, 3),
        `Cost per person (€)` = round(Cost / input$passengers, 2)
      ) %>%
      select(Transport, `Total Cost (€)`, `Cost per km (€)`, `Cost per person (€)`)
  }, digits = 2)
  
  # Enhanced map with real coordinates
  output$advanced_map <- renderLeaflet({
    req(values$from_coords, values$to_coords)
    
    # Create map with actual coordinates
    map <- leaflet() %>%
      addTiles() %>%
      addMarkers(
        lng = values$from_coords$lng, 
        lat = values$from_coords$lat, 
        popup = paste("📍 From:", values$from_coords$name),
        icon = makeIcon(
          iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
          iconWidth = 25, iconHeight = 41
        )
      ) %>%
      addMarkers(
        lng = values$to_coords$lng, 
        lat = values$to_coords$lat, 
        popup = paste("🎯 To:", values$to_coords$name),
        icon = makeIcon(
          iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
          iconWidth = 25, iconHeight = 41
        )
      ) %>%
      addPolylines(
        lng = c(values$from_coords$lng, values$to_coords$lng), 
        lat = c(values$from_coords$lat, values$to_coords$lat),
        color = "#007bff", 
        weight = 4, 
        opacity = 0.8,
        popup = paste("✈️ Direct distance:", values$distance, "km")
      ) %>%
      fitBounds(
        lng1 = min(values$from_coords$lng, values$to_coords$lng) - 0.5, 
        lat1 = min(values$from_coords$lat, values$to_coords$lat) - 0.5,
        lng2 = max(values$from_coords$lng, values$to_coords$lng) + 0.5, 
        lat2 = max(values$from_coords$lat, values$to_coords$lat) + 0.5
      )
    
    # Add distance info
    map %>% 
      addControl(
        html = paste0(
          "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.2);'>",
          "<h5 style='margin: 0 0 5px 0; color: #007bff;'>📏 Journey Details</h5>",
          "<p style='margin: 0;'><strong>Distance:</strong> ", values$distance, " km</p>",
          "<p style='margin: 0;'><strong>From:</strong> ", input$from, "</p>",
          "<p style='margin: 0;'><strong>To:</strong> ", input$to, "</p>",
          "</div>"
        ),
        position = "topright"
      )
  })
  
  # Carbon trends plot for dashboard
  output$carbon_trends <- renderPlot({
    months <- month.abb[1:8]
    carbon_data <- data.frame(
      Month = factor(months, levels = months),
      Your_CO2 = c(180, 220, 190, 160, 140, 200, 250, 180),
      EU_Average = rep(230, 8),
      Target = rep(190, 8)
    )
    
    ggplot(carbon_data, aes(x = Month)) +
      geom_line(aes(y = Your_CO2, group = 1, color = "Your CO2"), size = 1.5) +
      geom_line(aes(y = EU_Average, group = 1, color = "EU Average"), size = 1, linetype = "dashed") +
      geom_line(aes(y = Target, group = 1, color = "Paris Target"), size = 1, linetype = "dotted") +
      geom_point(aes(y = Your_CO2), size = 3, color = "#007bff") +
      scale_color_manual(values = c("Your CO2" = "#007bff", "EU Average" = "#dc3545", "Paris Target" = "#28a745")) +
      labs(title = "Monthly Carbon Footprint Trends", y = "CO2 (kg)", color = "Legend") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Transport mix pie chart
  output$transport_donut <- renderPlot({
    transport_data <- data.frame(
      Transport = c("🚗 Car", "🚂 Train", "🚌 Bus", "🚲 Bike", "✈️ Plane"),
      Percentage = c(40, 25, 15, 15, 5),
      Colors = c("#ff6b6b", "#4ecdc4", "#45b7d1", "#96ceb4", "#ffeaa7")
    )
    
    ggplot(transport_data, aes(x = "", y = Percentage, fill = Transport)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = transport_data$Colors) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = "Your Transport Usage")
  })
  
  # Carbon offset calculations
  output$offset_cost <- renderText({
    cost_per_ton <- switch(input$offset_project,
      "forest" = 15, "wind" = 12, "solar" = 18, "ocean" = 25
    )
    total_cost <- round(input$offset_amount / 1000 * cost_per_ton, 2)
    paste(total_cost)
  })
  
  output$offset_impact <- renderText({
    impact_data <- switch(input$offset_project,
      "forest" = list(trees = 50, unit = "trees planted", extra = "🐝 Supports local biodiversity"),
      "wind" = list(trees = 0, unit = "MWh clean energy", extra = "⚡ Powers 12 homes for 1 month"),
      "solar" = list(trees = 0, unit = "kWh solar energy", extra = "☀️ Prevents coal burning"),
      "ocean" = list(trees = 0, unit = "m² ocean protected", extra = "🐠 Saves marine ecosystems")
    )
    
    if (input$offset_project == "forest") {
      trees <- round(input$offset_amount / 1000 * 50)
      paste0("🌳 <strong>Trees Planted:</strong> ", trees, " trees<br>",
             "📅 <strong>CO2 Storage:</strong> 25 years<br>",
             "🌍 <strong>Extra Benefit:</strong> ", impact_data$extra)
    } else {
      amount <- round(input$offset_amount / 1000 * 100, 1)
      paste0("⚡ <strong>Clean Energy:</strong> ", amount, " ", impact_data$unit, "<br>",
             "📅 <strong>Duration:</strong> 20+ years<br>",
             "🌍 <strong>Extra Benefit:</strong> ", impact_data$extra)
    }
  })
  
  # Challenge button handlers
  observeEvent(input$start_bike_challenge, {
    showNotification("🚲 Bike Week Challenge started! Good luck!", type = "message", duration = 5)
  })
  
  observeEvent(input$start_train_challenge, {
    showNotification("🚂 Train Explorer Challenge started! All aboard!", type = "message", duration = 5)
  })
  
  observeEvent(input$start_offset_challenge, {
    showNotification("🌱 Offset Hero Challenge started! Save the planet!", type = "message", duration = 5)
  })
  
  observeEvent(input$purchase_offset, {
    cost_per_ton <- switch(input$offset_project,
      "forest" = 15, "wind" = 12, "solar" = 18, "ocean" = 25
    )
    total_cost <- round(input$offset_amount / 1000 * cost_per_ton, 2)
    showNotification(paste("🌱 Great choice! Your", input$offset_amount, "kg CO2 offset would cost €", total_cost), 
                    type = "message", duration = 8)
  })
}

# Run the application
shinyApp(ui = ui, server = server)