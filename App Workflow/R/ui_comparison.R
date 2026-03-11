# ============================================================
# ui_comparison.R - State Comparison Tab (UI only)
# ============================================================
# Owner: [PERSON 3]
# Line chart of productivity vs compensation for one state.
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

        # State dropdown (filled by server)
        selectInput(
          inputId  = "state",
          label    = "State:",
          choices  = NULL
        ),

        tags$small(
          class = "text-muted",
          "Uses the wage basis selected on the Map View tab."
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
