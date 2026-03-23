# ============================================================
# ui_outsideUS.R - Outside US Comparison Tab (UI only)
# ============================================================
# Owner: [PERSON - assign name here]
# Compares EU productivity vs US compensation over time.
# Data sources: World Bank GDP, ECB Labor Productivity, FRED.
# Server logic goes in server_outsideUS_tab.R
# ============================================================

read.csv("EUdata.csv")

outside_us_tab <- tabPanel(
  "Outside US",
  
  # ---- Tab heading ----
  h3("Global Comparison: EU Productivity vs. US Compensation",
     style = "font-family: Georgia, serif;"),
  tags$hr(),
  
  fluidRow(
    
    # ---- Sidebar Controls ----
    column(
      width = 3,
      wellPanel(
        
        # -- Year range selector --
        sliderInput(
          inputId = "outside_year_range",
          label   = "Select Year Range:",
          min     = 2000,
          max     = 2023,
          value   = c(2000, 2023),
          step    = 1,
          sep     = ""
        ),
        
        tags$hr(),
        
        # -- Series selector: what to show on the chart --
        checkboxGroupInput(
          inputId  = "outside_series",
          label    = "Show Series:",
          choices  = c(
            "EU GDP per Capita"        = "avr_gdp",
            "EU Labor Productivity"    = "quarter_avr",
            "US Compensation"          = "compensation",
            "US Labor Productivity"    = "productivity"
          ),
          selected = c("quarter_avr", "compensation")
        ),
        
        tags$hr(),
        
        tags$small(
          class = "text-muted",
          "EU data: World Bank GDP + ECB Labor Productivity.
           US data: FRED compensation and productivity indices."
        )
      )
    ),
    
    # ---- Main Content ----
    column(
      width = 9,
      
      # -- Main line chart --
      plotOutput("outside_us_plot", height = "420px"),
      
      tags$br(),
      
      # -- Summary table --
      tableOutput("outside_us_table"),
      
      tags$br(),
      
      # -- Download button --
      downloadButton("download_outside_us", "Download Data (CSV)")
    )
  )
)
