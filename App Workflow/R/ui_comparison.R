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
        p(
          strong("Question answered:"),
          " did productivity and compensation separate gradually or sharply in the states you care about?"
        ),
        p(
          strong("What this chart shows:"),
          " two indexed time series for each selected state, so you can see whether the gap widens, narrows, or reverses over time."
        ),
        
        # First state dropdown (filled by server) ----
        selectInput(
          inputId  = "state",
          label    = "State 1:",
          choices  = NULL
        ),
        
        # Second state dropdown (filled by server) ----
        selectInput(
          inputId  = "state_2",
          label    = "State 2:",
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
          "The compare button overlays one or two states using the same wage basis selected on the Map View tab."
        )
      )
    ),
    
    # ---- Line Chart ----
    column(
      width = 9,
      plotOutput("state_trends", height = "420px"),
      tags$p(
        class = "text-muted",
        "This is the best tab for showing how the visual evidence connects back to the main question over time, not just in one snapshot year.",
        style = "margin-top: 10px;"
      ),
      tags$br(),
      downloadButton("download_state_trends", "Download State Data (CSV)")
    )
  )
)
