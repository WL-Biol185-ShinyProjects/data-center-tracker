# ============================================================
# ui_heatmap.R - Heat Map Tab UI
# ============================================================
# Owner: [YOUR NAME]
# This file defines the layout for the Heat Map tab.
# It shows states (rows) x years (columns), filled by gap.
# ============================================================

# ---- Heat Map Tab ----
heatmap_tab <- tabPanel(
  title = "Heat Map",

  # ---- Tab heading ----
  h3("Productivity-Wage Gap Heat Map"),
  p("Each tile shows the gap between productivity growth
     and wage growth for a state in a given year.
     Darker red = larger gap. Values are indexed to 2007 = 100."),

  # ---- Sidebar layout: controls on left, plot on right ----
  sidebarLayout(

    # ---- Sidebar: user controls ----
    sidebarPanel(
      width = 3,

      # -- Year range slider --
      sliderInput(
        inputId = "heatmap_year_range",
        label   = "Select Year Range:",
        min     = 2007,
        max     = 2023,
        value   = c(2007, 2023),
        step    = 1,
        sep     = ""
      ),

      # -- Sort order for states --
      selectInput(
        inputId  = "heatmap_sort",
        label    = "Sort States By:",
        choices  = c(
          "Alphabetical"          = "alpha",
          "Largest Gap (latest)"  = "gap_desc",
          "Smallest Gap (latest)" = "gap_asc"
        ),
        selected = "gap_desc"
      )

    ),
    # ---- end sidebarPanel ----

    # ---- Main panel: the heat map plot ----
    mainPanel(
      width = 9,
      plotOutput(
        outputId = "heatmap_plot",
        height   = "800px"
      )
    )
    # ---- end mainPanel ----

  )
  # ---- end sidebarLayout ----

)
# ---- end tabPanel ----
