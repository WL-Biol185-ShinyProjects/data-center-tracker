# ============================================================
# ui_outsideUS.R - Outside US Comparison Tab (UI only)
# ============================================================
# Owner: [PERSON - assign name here]
# Compares EU and U.S. productivity/pay context over time.
# Data sources: World Bank GDP, ECB Labor Productivity, FRED.
# Server logic goes in server_outsideUS_tab.R
# ============================================================

outside_us_tab <- tabPanel(
  "Outside US",
  
  # ---- Tab heading ----
  h3("Global Comparison: EU Productivity vs. U.S. Compensation",
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
          "This tab gives international context. It does not compare perfectly
           equivalent measures, but it helps show whether the U.S. decoupling story
           looks unique or part of a broader pattern."
        )
      )
    ),
    
    # ---- Main Content ----
    column(
      width = 9,
      
      # -- Main line chart --
      plotOutput("outside_us_plot", height = "420px"),

      tags$p(
        class = "text-muted",
        "EU lines summarize broader European output and productivity context,
         while the U.S. lines track compensation and productivity indices.
         Read this as a framing comparison, not a causal apples-to-apples test.",
        style = "margin-top: 10px;"
      ),
      
      tags$br(),
      
      # -- Summary table --
      tableOutput("outside_us_table"),
      
      tags$br(),
      
      # -- Download button --
      downloadButton("download_outside_us", "Download Data (CSV)")
    )
  )
)
