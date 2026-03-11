# ============================================================
# ui_home.R - Home / Landing Page (UI only)
# ============================================================
# Owner: [PERSON 1]
# This is a static landing page. No server logic needed.
# ============================================================



home_tab <- tabPanel(
  "Home",
  
  # ---- Big Serif Title ----
  div(
    class = "main-title",
    "The Great Decoupling"
  ),
  
  # ---- Subtitle ----
  div(
    class = "main-subtitle",
    "Productivity vs. Wages"
  ),
  
  tags$hr(),
  
  # ---- About Section (light background) ----
  div(
    class = "section-light",
    fluidRow(
      column(
        width = 8, offset = 2,
        h3("About This Project",
           style = "font-family: Georgia, serif; font-weight: bold;"),
        p("Since the 1970s, American workers have become dramatically
           more productive, but wages have not kept pace. This dashboard
           explores that gap across all 50 states using publicly
           available data from the Bureau of Labor Statistics and the
           Bureau of Economic Analysis."),
        p("Use the tabs above to explore interactive maps, compare
           individual states over time, and see which states have the
           largest productivity-wage gaps.")
      )
    )
  ),
  
  tags$br(),
  
  # ---- Feature Cards ----
  h3("Key Features",
     style = "text-align: center; font-family: Georgia, serif;"),
  tags$br(),
  
  fluidRow(
    
    # Card 1: Map View
    column(
      width = 3,
      div(
        class = "feature-card",
        h4("Map View"),
        p("See the productivity-compensation gap across
           all 50 states, color-coded by gap size.")
      )
    ),
    
    # Card 2: State Comparison
    column(
      width = 3,
      div(
        class = "feature-card",
        h4("State Comparison"),
        p("Pick a state and see how productivity and
           compensation have diverged over time.")
      )
    ),
    
    # Card 3: Gap Rankings
    column(
      width = 3,
      div(
        class = "feature-card",
        h4("Gap Rankings"),
        p("All 50 states ranked by the size of their
           productivity-wage gap.")
      )
    ),
    
    # Card 4: Levels Comparison
    column(
      width = 3,
      div(
        class = "feature-card",
        h4("Levels Comparison"),
        p("Cross-sectional view: real GDP per job
           vs. real weekly wages by state.")
      )
    )
  ),
  
  # ---- Footer ----
  tags$br(),
  tags$hr(),
  div(
    style = "text-align: center; color: #6c757d; padding: 20px;",
    p("Data: BLS State Productivity | BEA GDP by State | BLS QCEW"),
    p("BIO 185: Visualizing Big Data | Washington & Lee University")
  )
)





