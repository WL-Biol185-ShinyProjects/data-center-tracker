# ============================================================
# ui_map.R - Map View Tab (UI only)
# ============================================================
# Owner: [PERSON 2]
# Choropleth map of the productivity-wage gap by state.
# The server logic for this tab is in server_growth_tabs.R.
# ============================================================

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
        p(
          strong("Question answered:"),
          " Which states have the largest productivity-compensation gap in a selected year?"
        ),
        p(
          strong("What this map shows:"),
          " the selected year's gap, defined as the productivity index minus the compensation index after both are rebased to 2007 = 100."
        ),

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
          "Blue states have productivity outpacing compensation by more; orange states have smaller or negative gaps."
        )
      )
    ),

    # ---- Map Plot ----
    column(
      width = 9,
      plotOutput("gap_map", height = "550px"),
      tags$p(
        class = "text-muted",
        "Use this view to answer the broadest version of the research question: where is decoupling most visible right now?",
        style = "margin-top: 10px;"
      ),
      tags$br(),
      downloadButton("download_gap_map", "Download Map Data (CSV)")
    )
  )
)
