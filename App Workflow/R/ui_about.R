# ============================================================
# ui_about.R - About Tab (UI only)
# ============================================================
# Owner: Anyone (static text)
# Methods, data sources, and interpretation notes.
# ============================================================


about_tab <- tabPanel(
  "About",

  fluidRow(
    column(
      width = 8, offset = 2,

      h3("Methods", style = "font-family: Georgia, serif;"),
      p("This app compares labor productivity and hourly compensation
        across states."),
      p("Growth trends are rebased to 2007 = 100 for readability."),
      p("Growth tabs default to real hourly compensation using a
        national GDP deflator. Use the sidebar toggle to switch
        to nominal."),
      p("Gap metric = labor productivity index minus hourly
        compensation index (real by default)."),
      p("Levels ratio metric = annual private-sector real pay
        (QCEW + RPP) divided by real GDP per job (BEA)."),
      p("Interpretation note: these are descriptive comparisons,
        not causal claims."),

      tags$hr(),

      h4("Important Comparability Notes",
         style = "font-family: Georgia, serif;"),
      tags$ul(
        tags$li("Levels panel combines BEA total-economy GDP/jobs
                 with QCEW private-sector wages."),
        tags$li("Real wages use BEA RPP (spatial cost adjustment),
                 while GDP per job is in chained 2017 dollars."),
        tags$li("Treat levels comparisons as descriptive cross-state
                 signals, not a structural labor-share estimate.")
      ),

      tags$hr(),

      h4("Data Sources", style = "font-family: Georgia, serif;"),
      tags$ul(
        tags$li(tags$a(
          href   = "https://www.bls.gov/lpc/state-productivity.htm",
          target = "_blank", "BLS Productivity by State"
        )),
        tags$li(tags$a(
          href   = "https://www.bls.gov/cew/",
          target = "_blank", "BLS QCEW"
        )),
        tags$li(tags$a(
          href   = "https://www.bea.gov/data/gdp/gdp-state",
          target = "_blank", "BEA GDP by State"
        )),
        tags$li(tags$a(
          href   = "https://www.bea.gov/data/employment/employment-state",
          target = "_blank", "BEA Employment by State"
        )),
        tags$li(tags$a(
          href = "https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area",
          target = "_blank", "BEA Regional Price Parities"
        ))
      ),

      tags$hr(),
      div(
        style = "text-align: center; color: #6c757d; padding: 20px;",
        p("BIO 185: Visualizing Big Data"),
        p("Washington & Lee University, Spring 2026")
      )
    )
  )
)
