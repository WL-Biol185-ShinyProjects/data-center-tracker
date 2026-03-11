# ============================================================
# app.R - Main Coordinator File
# ============================================================
# This file wires everything together. It rarely needs editing.
#
# TEAM FILE OWNERSHIP:
#   Person 1 (Home/Design): R/ui_home.R
#   Person 2 (Maps):        R/ui_map.R, R/ui_levels.R
#   Person 3 (Comparison):  R/ui_comparison.R
#   Person 4 (Rankings):    R/ui_rankings.R
#   Data Person:            R/helpers.R
#   Shared server logic:    R/server_growth_tabs.R
#                           R/server_levels_tab.R
#   Static text:            R/ui_about.R
#
# RULE: Only edit the file(s) you own. This app.R file
# and the server files should only change when the team
# agrees to add/remove a tab.
# ============================================================


# ---- Load Packages ----
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


# ---- Source All Files ----
source("R/helpers.R")
source("R/ui_home.R")
source("R/ui_map.R")
source("R/ui_comparison.R")
source("R/ui_rankings.R")
source("R/ui_levels.R")
source("R/ui_about.R")
source("R/server_growth_tabs.R")
source("R/server_levels_tab.R")


# ---- Custom CSS (Women's Health-style design) ----
app_css <- tags$head(
  tags$style(HTML("

    /* Serif font for headings - matches Women's Health screenshot */
    .main-title {
      font-family: 'Georgia', 'Times New Roman', serif;
      font-size: 2.5em;
      font-weight: bold;
      text-align: center;
      margin-top: 30px;
      margin-bottom: 10px;
      letter-spacing: 2px;
    }

    .main-subtitle {
      font-family: 'Georgia', 'Times New Roman', serif;
      font-size: 1.1em;
      text-align: center;
      color: #6c757d;
      margin-bottom: 30px;
      padding-bottom: 20px;                    
      border-bottom: 2px solid #2c3e50;        
    }

    /* Feature cards on home page */
    .feature-card {
      border: 1px solid #dee2e6;
      border-radius: 8px;
      padding: 20px;
      margin: 10px 0;
      background: white;
      min-height: 180px;
    }

    .feature-card h4 {
      font-family: 'Georgia', 'Times New Roman', serif;
    }

    /* Light background band for sections */
    .section-light {
      background-color: #f0f4f8;
      padding: 40px 20px;
      border-radius: 8px;
      margin: 20px 0;
    }

    /* Tab titles in serif */
    h3 {
      font-family: 'Georgia', 'Times New Roman', serif;
    }

  "))
)


# ---- UI: navbarPage Layout ----
ui <- navbarPage(

  title  = "Productivity vs Wages Atlas",
  id     = "main_tab",
  header = app_css,

  home_tab,
  map_tab,
  comparison_tab,
  rankings_tab,
  levels_tab,
  about_tab
)


# ---- Server ----
server <- function(input, output, session) {

  # Load data once as reactiveVals (same as your original)
  panel_data  <- reactiveVal(load_panel_data())
  levels_data <- reactiveVal(load_levels_data())

  # Growth tabs server (map, comparison, rankings)
  growth_tabs_server(input, output, session,
                     panel_data, levels_data)

  # Levels tab server
  levels_tab_server(input, output, session,
                    levels_data)
}


# ---- Run ----
shinyApp(ui = ui, server = server)
