# ============================================================
# app.R — The Great Decoupling:
#          Productivity vs. Income Across America
# ============================================================
#
# This is a Shiny web app skeleton.
# Right now it only shows placeholder text on each tab.
# We will add real data and real outputs later.
#
# Structure of every Shiny app (3 parts):
#   1. ui     — what the user SEES (layout, buttons, text)
#   2. server — what the computer DOES (calculations, plots)
#   3. shinyApp() — connects ui and server together
#
# ============================================================


# ---- Load Libraries ----
# We load each library we need at the very top.
# This way, if something is missing, we find out right away.

library(shiny)       # the core package for building web apps
library(tidyverse)   # includes dplyr, ggplot2, tibble, etc.
library(leaflet)     # for interactive maps (Map tab later)
library(plotly)      # for interactive charts (hover, zoom)


# ---- Create Placeholder Data ----
# This is FAKE data so we can test the app skeleton.
# We will replace this with real FRED data later.
#
# tibble() is the tidyverse version of data.frame().
# Each column is a variable, each row is an observation.

placeholder_data <- tibble(
  state_name      = c("Virginia", "California", "Texas"),
  year            = c(2000, 2000, 2000),
  productivity    = c(100, 110, 105),
  income          = c(100, 95, 90),
  gap             = c(0, 15, 15)
)

# Take a quick look at the placeholder data
# (This prints to the R console, not the app)
print(placeholder_data)


# ---- Define the UI (User Interface) ----
# The ui controls the LAYOUT of the app.
# navbarPage() gives us a top navigation bar with tabs.
# Each tabPanel() is one page/tab in the app.

ui <- navbarPage(

  # ---- App Title (appears in the navbar) ----
  title = "The Great Decoupling",

  # ---- Tab 1: Home ----
  # This is the landing page. It introduces the project.
  tabPanel(
    title = "Home",

    # fluidPage makes the content resize with the browser
    fluidPage(

      # A big heading for the page
      h2("Productivity vs. Income Across America"),

      # A smaller heading
      h4("1997 - 2023"),

      # Some introductory text (placeholder for now)
      p(
        "Welcome! This app explores the 'Great Decoupling'",
        "— the growing gap between how much workers produce",
        "(productivity) and how much they earn (income)."
      ),

      p(
        "Use the tabs above to explore a national overview,",
        "an interactive map, and state-by-state comparisons."
      ),

      # hr() draws a horizontal line (visual separator)
      hr(),

      # Placeholder for a future plot output
      # We give it an ID so the server can send a plot here
      h4("National Trend (placeholder)"),
      plotOutput(outputId = "home_plot")
    )
  ),

  # ---- Tab 2: Map ----
  # This tab will eventually show a choropleth map.
  tabPanel(
    title = "Map",

    fluidPage(

      h2("Productivity–Income Gap by State"),

      p(
        "This page will show an interactive map.",
        "States will be colored by the size of the gap",
        "between productivity growth and income growth."
      ),

      hr(),

      # Placeholder for a future leaflet map output
      h4("Interactive Map (coming soon)"),
      leafletOutput(outputId = "map_output")
    )
  ),

  # ---- Tab 3: State Comparison ----
  # This tab will let users pick states and compare them.
  tabPanel(
    title = "State Comparison",

    fluidPage(

      h2("Compare States Side by Side"),

      p(
        "This page will let you select two or more states",
        "and compare their productivity and income trends",
        "over time."
      ),

      hr(),

      # Placeholder for a future plotly chart
      h4("State Comparison Chart (coming soon)"),
      plotlyOutput(outputId = "comparison_plot")
    )
  ),

  # ---- Tab 4: About ----
  # This tab explains the project, data sources, and team.
  tabPanel(
    title = "About",

    fluidPage(

      h2("About This Project"),

      h4("What is the Great Decoupling?"),
      p(
        "Since the late 1970s, productivity in the U.S.",
        "has grown much faster than typical worker income.",
        "This app lets you explore that gap state by state."
      ),

      h4("Data Sources"),
      p(
        "All data comes from the Federal Reserve Economic",
        "Data (FRED) API. We use GDP, employment, and wage",
        "data for all 50 states from 1997 to 2023."
      ),

      h4("Our Team"),
      p(
        "This project was created for BIO 185:",
        "Visualizing Big Data at Washington & Lee University."
      ),

      h4("Methods"),
      p(
        "Productivity is measured as real GDP per worker.",
        "Income is measured using average wages.",
        "Both are indexed to a base year for comparison."
      )
    )
  )
)


# ---- Define the Server ----
# The server function tells Shiny what to calculate
# and what to send back to the ui for display.
#
# Right now we only have ONE simple placeholder output.
# We will add map, comparison, and other outputs later.
#
# input  = values coming FROM the user (buttons, sliders)
# output = values going TO the user (plots, tables, text)

server <- function(input, output) {

  # ---- Home Tab: Simple Placeholder Plot ----
  # renderPlot() tells Shiny "this output is a plot"
  # Inside the curly braces { } we write normal ggplot code
  output$home_plot <- renderPlot({

    # Use our placeholder data to make a simple bar chart
    ggplot(placeholder_data, aes(x = state_name,
                                  y = gap)) +
      geom_col(fill = "steelblue") +
      labs(
        title = "Placeholder: Gap by State (fake data)",
        x     = "State",
        y     = "Productivity - Income Gap"
      ) +
      theme_minimal()
  })

  # ---- Map Tab: Empty Leaflet Map ----
  # renderLeaflet() tells Shiny "this output is a map"
  # For now it just shows a blank US map
  output$map_output <- renderLeaflet({

    leaflet() %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addTiles()
  })

  # ---- Comparison Tab: Empty Plotly Chart ----
  # renderPlotly() tells Shiny "this output is interactive"
  # For now it just shows the placeholder data as a chart
  output$comparison_plot <- renderPlotly({

    p <- ggplot(placeholder_data, aes(x = state_name,
                                       y = gap)) +
      geom_col(fill = "coral") +
      labs(
        title = "Placeholder: State Comparison (fake data)",
        x     = "State",
        y     = "Gap"
      ) +
      theme_minimal()

    # ggplotly() converts a ggplot into an interactive chart
    ggplotly(p)
  })
}


# ---- Launch the App ----
# This line connects the ui and server and starts the app.
# Always the LAST line in app.R.

shinyApp(ui = ui, server = server)
