# ============================================================
# tab_comparison.R — Person 3's File
# The Great Decoupling: STATE COMPARISON TAB
# ============================================================
#
# YOUR JOB:
# Build the State Comparison page of the app.
# This page lets users pick 2+ states and compare their
# productivity and income trends side by side.
#
# WHAT TO DO:
# 1. Edit the ui section below to design your tab layout
# 2. Edit the server section below to create your chart
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
# Fake data for 3 states across several years.

comparison_data <- tibble(
  state_name   = rep(c("Virginia", "California", "Texas"),
                     each = 5),
  year         = rep(c(2000, 2005, 2010, 2015, 2020), 3),
  productivity = c(100, 108, 115, 125, 140,
                   100, 112, 128, 145, 165,
                   100, 106, 112, 120, 132),
  income       = c(100, 104, 107, 110, 115,
                   100, 103, 105, 108, 112,
                   100, 105, 108, 112, 118)
)

# Calculate the gap for each row
comparison_data <- comparison_data %>%
  mutate(gap = productivity - income)


# ---- UI for Comparison Tab ----
# This is what the user SEES on the Comparison page.

tab_comparison_ui <- fluidPage(

  # Page title
  h2("Compare States Side by Side"),

  p(
    "Select two or more states to compare how their",
    "productivity and income trends differ over time."
  ),

  hr(),

  # ---- YOUR WORK GOES HERE ----

  # Layout: sidebar on left, chart on right
  sidebarLayout(

    # Left side: user inputs
    sidebarPanel(
      width = 3,

      # Dropdown to pick states (multiple allowed)
      selectInput(
        inputId  = "selected_states",
        label    = "Choose States:",
        choices  = unique(comparison_data$state_name),
        selected = c("Virginia", "California"),
        multiple = TRUE
      ),

      # Radio buttons to pick what to plot
      radioButtons(
        inputId  = "plot_variable",
        label    = "What to show:",
        choices  = c("Productivity" = "productivity",
                     "Income"       = "income",
                     "Gap"          = "gap"),
        selected = "gap"
      )
    ),

    # Right side: the chart
    mainPanel(
      width = 9,
      plotlyOutput(outputId = "comparison_chart")
    )
  )

  # NEXT STEPS for Person 3:
  # - Replace fake data with real FRED data
  # - Add all 50 states to the dropdown
  # - Maybe add a data table below the chart
  # - Make the chart colors and labels look polished
)


# ---- Server for Comparison Tab ----
# This is what the computer DOES for the Comparison page.

tab_comparison_server <- function(input, output) {

  # ---- Comparison Line Chart ----
  output$comparison_chart <- renderPlotly({

    # Filter data to only the states the user picked
    filtered_data <- comparison_data %>%
      filter(state_name %in% input$selected_states)

    # Build the ggplot using the variable the user chose
    p <- ggplot(filtered_data,
                aes(x     = year,
                    y     = .data[[input$plot_variable]],
                    color = state_name)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = paste(input$plot_variable,
                      "by State (fake data)"),
        x     = "Year",
        y     = "Index Value",
        color = "State"
      ) +
      theme_minimal()

    # Convert to interactive plotly
    ggplotly(p)
  })
}


# ---- Test This Tab By Itself ----
# This mini app lets you run JUST your tab to test it.

ui <- navbarPage(
  title = "Test: Comparison Tab",
  tabPanel("State Comparison", tab_comparison_ui)
)

server <- function(input, output) {
  tab_comparison_server(input, output)
}

shinyApp(ui = ui, server = server)
