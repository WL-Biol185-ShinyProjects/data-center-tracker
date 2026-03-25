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
source("R/ui_outsideUS.R") #EU Data
source("R/server_heatmap_tab.R")
source("R/ui_research.R")


# ---- Custom CSS ----
app_css <- tags$head(
  tags$style(HTML("

/* Navbar - navy blue background with white text */
    .navbar {
      background-color: #1a2744 !important;
      border-bottom: 3px solid #c8a96e;
    }

    .navbar-brand, .navbar-nav > li > a {
      color: #ffffff !important;
      font-family: 'Georgia', 'Times New Roman', serif;
    }

    .navbar-nav > li > a:hover {
      color: #c8a96e !important;
      background-color: transparent !important;
    }

    .navbar-nav > li.active > a {
      color: #c8a96e !important;
      background-color: rgba(255,255,255,0.1) !important;
    }

/* Page background - warm cream, easy on the eyes */
    body {
      background-color: #f5f0e8;
    }
    
    /* Section background updated to complement cream */
    .section-light {
      background-color: #eae4d8;
      padding: 40px 20px;
      border-radius: 8px;
      margin: 20px 0;
      border-left: 4px solid #1a2744;
    }

    /* Cards updated to stay white against cream background */
    .feature-card {
      border: 1px solid #d4c9b0;
      border-radius: 8px;
      padding: 20px;
      margin: 10px 0;
      background: white;
      min-height: 180px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      transition: box-shadow 0.2s ease;
    }

    .feature-card:hover {
      box-shadow: 0 6px 20px rgba(0,0,0,0.15);
      border-color: #1a2744;
    }
    
    /* Title color updated to navy */
    .main-title {
      font-family: 'Georgia', 'Times New Roman', serif;
      font-size: 2.5em;
      font-weight: bold;
      text-align: center;
      color: #1a2744;
      letter-spacing: 2px;
      margin-top: 30px;
      margin-bottom: 10px;
    }

    .main-subtitle {
      font-family: 'Georgia', 'Times New Roman', serif;
      font-size: 1.1em;
      text-align: center;
      color: #6c757d;
      margin-bottom: 30px;
      padding-bottom: 20px;
      border-bottom: 2px solid #1a2744;
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
  outside_us_tab,
  comparison_tab,
  rankings_tab,
  levels_tab,
  heatmap_tab,
  research_tab,
  about_tab
)


# ---- Server ----
server <- function(input, output, session) {
  
  panel_data      <- reactiveVal(load_panel_data())
  levels_data     <- reactiveVal(load_levels_data())
  outside_us_data <- reactiveVal(load_outside_us_data())
  
  growth_tabs_server(input, output, session,
                     panel_data, levels_data)
  levels_tab_server(input, output, session,
                    levels_data)
  heatmap_tab_server(input, output, session,
                     panel_data)
  outside_us_tab_server(input, output, session,
                        outside_us_data)
}



# ---- Run ----
shinyApp(ui = ui, server = server)