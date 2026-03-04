# ============================================================
# tab_home.R — Person 1's File
# The Great Decoupling: HOME TAB
# ============================================================
#
# YOUR JOB:
# Build the Home / landing page of the app.
# This page introduces the project and shows a national
# trend line of productivity vs. income over time.
#
# WHAT TO DO:
# 1. Edit the ui section below to design your tab layout
# 2. Edit the server section below to create your outputs
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
library(plotly)


# ---- Placeholder Data ----
# Replace this with real data later.
# For now, this fake data lets you build and test your tab.

home_data <- tibble(
  year         = 1997:2023,
  productivity = 100 + cumsum(rnorm(27, mean = 2, sd = 1)),
  income       = 100 + cumsum(rnorm(27, mean = 1, sd = 1))
)


# ---- UI for Home Tab ----
# This is what the user SEES on the Home page.
# Wrap everything inside fluidPage().

tab_home_ui <- fluidPage(

  # Page title
  h2("Productivity vs. Income Across America"),
  h4("1997 - 2023"),

  # Introduction paragraph
  p(
    "Welcome! This app explores the 'Great Decoupling'",
    "- the growing gap between how much workers produce",
    "(productivity) and how much they earn (income)."
  ),

  p(
    "Use the tabs above to explore a national overview,",
    "an interactive map, and state-by-state comparisons."
  ),

  hr(),

  # ---- YOUR WORK GOES HERE ----
  # Add inputs (dropdowns, sliders) and outputs (plots)
  # below this line.

  h4("National Trend"),
  plotlyOutput(outputId = "home_trend_plot")

  # NEXT STEPS for Person 1:
  # - Make the placeholder chart look nicer
  # - Add a second plot or summary statistics
  # - Replace fake data with real national-level FRED data
)


# ---- Server for Home Tab ----
# This is what the computer DOES for the Home page.

tab_home_server <- function(input, output) {

  # ---- National Trend Line Chart ----
  output$home_trend_plot <- renderPlotly({

    # Reshape data from wide to long for ggplot
    # (one row per year per variable)
    plot_data <- home_data %>%
      pivot_longer(
        cols      = c(productivity, income),
        names_to  = "measure",
        values_to = "index_value"
      )

    # Build the ggplot
    p <- ggplot(plot_data, aes(x     = year,
                                y     = index_value,
                                color = measure)) +
      geom_line(linewidth = 1) +
      labs(
        title = "National Productivity vs. Income (fake data)",
        x     = "Year",
        y     = "Index (base year = 100)",
        color = "Measure"
      ) +
      theme_minimal()

    # Convert to interactive plotly
    ggplotly(p)
  })
}


# ---- Test This Tab By Itself ----
# This mini app lets you run JUST your tab to test it.
# The real app.R will NOT use this part.

ui <- navbarPage(
  title = "Test: Home Tab",
  tabPanel("Home", tab_home_ui)
)

server <- function(input, output) {
  tab_home_server(input, output)
}

shinyApp(ui = ui, server = server)
