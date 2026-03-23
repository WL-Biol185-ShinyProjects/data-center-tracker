# ============================================================
# ui_comparison.R - State Comparison Tab (UI only)
# ============================================================
# UPDATED: Added a second state dropdown and a Compare button
# so users can overlay two states on the same chart.
# Server logic is in server_growth_tabs.R.
# ============================================================

comparison_tab <- tabPanel(
  "State Comparison",
  h3("State Trend Comparison",
     style = "font-family: Georgia, serif;"),
  tags$hr(),
  fluidRow(
    
    # ---- Sidebar Controls ----
    column(
      width = 3,
      wellPanel(
        
        # First state dropdown (filled by server) ----
        selectInput(
          inputId  = "state",
          label    = "State 1:",
          choices  = NULL
        ),
        
        # Second state dropdown (filled by server) ----
        selectInput(
          inputId  = "state_2",
          label    = "State 2 (optional):",
          choices  = NULL
        ),
        
        # Compare button ----
        actionButton(
          inputId = "compare_go",
          label   = "Show Comparison",
          class   = "btn-primary"
        ),
        
        tags$br(), tags$br(),
        
        tags$small(
          class = "text-muted",
          "Uses the wage basis selected on the Map View tab.
           Leave State 2 blank to show one state only."
        )
      )
    ),
    
    # ---- Line Chart ----
    column(
      width = 9,
      plotOutput("state_trends", height = "420px"),
      tags$br(),
      downloadButton("download_state_trends", "Download State Data (CSV)")
    )
  )
)