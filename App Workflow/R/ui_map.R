# ============================================================
# ui_map.R - Map View Tab (UI only)
# ============================================================
# Owner: [PERSON 2]
# Choropleth map of the productivity-wage gap by state.
# The server logic for this tab is in server_growth_tabs.R.
# ============================================================

# ---- Load leaflet for map output ----
library(leaflet)

map_tab <- tabPanel(
  "Map View",

  h3("Productivity-Compensation Gap by State",
     style = "font-family: Georgia, serif;"),
  tags$hr(),

  fluidRow(

    # ---- Sidebar Controls ----
    column(
      width = 3,
      wellPanel(

        # Year dropdown (filled by server)
        selectInput(
          inputId  = "year",
          label    = "Year:",
          choices  = NULL
        ),

        # Real vs nominal toggle
        selectInput(
          inputId  = "growth_wage_basis",
          label    = "Wage Basis:",
          choices  = c(
            "Real (GDP deflator adjusted)" = "real",
            "Nominal (not inflation-adjusted)" = "nominal"
          ),
          selected = "real"
        ),

        tags$small(
          class = "text-muted",
          "Growth tabs use indices rebased to 2007 = 100."
        )
      )
    ),

    # ---- Map Plot ----
    column(
      width = 9,
      plotOutput("gap_map", height = "550px"),
      tags$br(),
      downloadButton("download_gap_map", "Download Map Data (CSV)")
    )
  )
)
