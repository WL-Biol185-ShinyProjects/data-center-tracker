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
source("R/ui_heatmap.R")
source("R/server_heatmap_tab.R")
source("R/ui_outsideUS.R")


# ---- Custom CSS ----
app_css <- tags$head(
  tags$style(HTML("

    /* Main page title - large bold serif at top of home page */
    .main-title {
      font-family: 'Georgia', 'Times New Roman', serif;
      font-size: 2.5em;
      font-weight: bold;
      text-align: center;
      color: #2c3e50;
      letter-spacing: 2px;
      margin-top: 30px;
      margin-bottom: 10px;
    }

    /* Subtitle below the main title */
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
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      transition: box-shadow 0.2s ease;
    }

    /* Card lifts slightly when hovered */
    .feature-card:hover {
      box-shadow: 0 6px 20px rgba(0,0,0,0.15);
    }

    /* Card headings - bold serif to match page title */
    .feature-card h4 {
      font-family: 'Georgia', 'Times New Roman', serif;
      font-weight: bold;
      color: #2c3e50;
    }

    /* Light background band used in the About section */
    .section-light {
      background-color: #f0f4f8;
      padding: 40px 20px;
      border-radius: 8px;
      margin: 20px 0;
      border-left: 4px solid #2c3e50;
    }

    /* All h3 headings use serif font to stay consistent */
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
  heatmap_tab,
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
  # Heat map tab server
  heatmap_tab_server(input, output, session,
                     panel_data)
}


# ---- Run ----
shinyApp(ui = ui, server = server)