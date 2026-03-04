# ============================================================
# tab_map.R — Person 2's File
# The Great Decoupling: MAP TAB
# ============================================================
#
# YOUR JOB:
# Build the Map page of the app.
# This page shows a choropleth (colored) map of the U.S.
# where each state is shaded by the productivity-income gap.
#
# WHAT TO DO:
# 1. Edit the ui section below to design your tab layout
# 2. Edit the server section below to create your map
# 3. When you are done, the "integrator" will copy your
#    finished code into the main app.R
#
# HOW TO TEST:
# You can run this file by itself! It is a mini Shiny app.
# Just click "Run App" in RStudio.
# ============================================================


# ---- Load Libraries ----
library(shiny)
library(tidyverse)
library(leaflet)


# ---- Placeholder Data ----
# Replace this with real data later.
# For now, fake data for a few states with lat/lon centers.

map_data <- tibble(
  state_name = c("Virginia", "California", "Texas",
                 "New York", "Florida", "Ohio"),
  lat        = c(37.4, 36.8, 31.0, 42.2, 27.8, 40.4),
  lon        = c(-79.5, -119.4, -99.7, -74.9, -81.8, -82.9),
  gap        = c(5, 18, 12, 20, 8, 10)
)


# ---- UI for Map Tab ----
# This is what the user SEES on the Map page.

tab_map_ui <- fluidPage(

  # Page title
  h2("Productivity-Income Gap by State"),

  p(
    "This map shows the gap between productivity growth",
    "and income growth for each state. Darker colors",
    "mean a bigger gap."
  ),

  hr(),

  # ---- YOUR WORK GOES HERE ----
  # Add inputs (year slider, dropdown) and map output
  # below this line.

  # A slider to pick which year to display
  sliderInput(
    inputId = "map_year",
    label   = "Select Year:",
    min     = 1997,
    max     = 2023,
    value   = 2023,
    step    = 1,
    sep     = ""
  ),

  # The map output
  leafletOutput(outputId = "state_map", height = "500px")

  # NEXT STEPS for Person 2:
  # - Load real GeoJSON state boundaries
  # - Color states by gap value (choropleth)
  # - Add popups showing state name and gap value
  # - Make the slider filter data by year
)


# ---- Server for Map Tab ----
# This is what the computer DOES for the Map page.

tab_map_server <- function(input, output) {

  # ---- Leaflet Map ----
  output$state_map <- renderLeaflet({

    # For now, just show circle markers on a US map
    # Later, replace with real choropleth polygons
    leaflet(map_data) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addTiles() %>%
      addCircleMarkers(
        lng     = ~lon,
        lat     = ~lat,
        radius  = ~gap,
        color   = "steelblue",
        fillOpacity = 0.7,
        popup   = ~paste(state_name, "<br>",
                         "Gap:", gap)
      )
  })
}


# ---- Test This Tab By Itself ----
# This mini app lets you run JUST your tab to test it.

ui <- navbarPage(
  title = "Test: Map Tab",
  tabPanel("Map", tab_map_ui)
)

server <- function(input, output) {
  tab_map_server(input, output)
}

shinyApp(ui = ui, server = server)
