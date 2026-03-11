# ============================================================
# ui_rankings.R - Gap Rankings Tab (UI only)
# ============================================================
# Owner: [PERSON 4]
# Table of all 50 states ranked by gap.
# Server logic is in server_growth_tabs.R.
# ============================================================


rankings_tab <- tabPanel(
  "Gap Rankings",

  h3("Gap Rankings by State",
     style = "font-family: Georgia, serif;"),
  tags$hr(),

  fluidRow(

    # ---- Sidebar Note ----
    column(
      width = 3,
      wellPanel(
        tags$small(
          class = "text-muted",
          "Uses the year and wage basis selected on the Map View tab.
           States ranked from largest gap to smallest."
        )
      )
    ),

    # ---- Rankings Table ----
    column(
      width = 9,
      tableOutput("rank_table"),
      tags$br(),
      downloadButton("download_rankings", "Download Rankings (CSV)")
    )
  )
)
