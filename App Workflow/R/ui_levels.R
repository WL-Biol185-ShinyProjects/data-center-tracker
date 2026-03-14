# ============================================================
# ui_levels.R - Levels Comparison Tab (UI only)
# ============================================================
# Owner: [PERSON 2 or PERSON 4 - pick one]
# Cross-sectional levels view: GDP per job vs wages.
# Server logic is in server_levels_tab.R.
# ============================================================


levels_tab <- tabPanel(
  "Levels Comparison",
  
  h3("Levels Comparison: GDP per Job vs. Wages",
     style = "font-family: Georgia, serif;"),
  tags$hr(),
  
  fluidRow(
    
    # ---- Sidebar Controls ----
    column(
      width = 3,
      wellPanel(
        
        p("BEA GDP/job + QCEW wages (RPP-adjusted for real metrics).",
          style = "font-size: 0.9em;"),
        
        # Metric picker dropdown
        selectInput(
          inputId  = "levels_metric",
          label    = "Map Metric:",
          choices  = levels_metric_choices
        ),
        
        tags$small(
          class = "text-muted",
          "Uses the year selected on the Map View tab.
           Real metrics depend on RPP availability."
        )
      )
    ),
    
    # ---- Main Content ----
    column(
      width = 9,
      
      # Warning if data not loaded
      uiOutput("levels_unavailable_note"),
      
      # Levels choropleth map
      plotOutput("levels_map", height = "420px"),
      
      tags$br(),
      
      # Scatter plot - GDP per job vs weekly wage
      plotOutput("levels_scatter_static", height = "420px"),
      
      tags$br(),
      
      # Levels ranking table
      tableOutput("levels_rank_table"),
      
      tags$br(),
      downloadButton("download_levels", "Download Levels Data (CSV)")
    )
  )
)

