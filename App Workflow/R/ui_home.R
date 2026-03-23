# ============================================================
# ui_home.R - Home / Landing Page (UI only)
# ============================================================
# Owner: [PERSON 1]
# This is a static landing page. No server logic needed.
# ============================================================

home_tab <- tabPanel(
  "Home",
  
  # ---- Hero Banner (colored background with title) ----
  div(
    style = "background-color: #2c3e50;
             color: #FFFFFF;
             text-align: center;
             padding: 60px 20px 40px 20px;
             margin-bottom: 30px;",
    h1("The Great Decoupling",
       style = "font-family: Georgia, serif;
                font-size: 42px;
                margin-bottom: 5px;"),
    h4("Productivity vs. Wages",
       style = "color: #d4af37;
                font-family: Georgia, serif;
                margin-top: 0px;"),
    p("An interactive dashboard exploring the growing gap between",
      "worker productivity and compensation across all 50 U.S. states",
      style = "max-width: 700px;
               margin: 15px auto 0 auto;
               font-size: 16px;
               color: #cccccc;")
  ),
  
  # ---- Summary Stat Boxes ----
  fluidRow(
    # Box 1: States covered
    column(
      width = 3,
      div(
        style = "background-color: #2c3e50;
                 color: white;
                 text-align: center;
                 padding: 20px;
                 border-radius: 8px;
                 margin-bottom: 20px;",
        h2("50", style = "margin: 0; font-size: 36px;
                          color: #d4af37;"),
        p("States Covered", style = "margin: 5px 0 0 0;")
      )
    ),
    
    # Box 2: Years of data
    column(
      width = 3,
      div(
        style = "background-color: #2c3e50;
                 color: white;
                 text-align: center;
                 padding: 20px;
                 border-radius: 8px;
                 margin-bottom: 20px;",
        h2("17", style = "margin: 0; font-size: 36px;
                          color: #d4af37;"),
        p("Years of Data", style = "margin: 5px 0 0 0;")
      )
    ),
    
    # Box 3: Data starts
    column(
      width = 3,
      div(
        style = "background-color: #2c3e50;
                 color: white;
                 text-align: center;
                 padding: 20px;
                 border-radius: 8px;
                 margin-bottom: 20px;",
        h2("2007", style = "margin: 0; font-size: 36px;
                            color: #d4af37;"),
        p("Data Starts", style = "margin: 5px 0 0 0;")
      )
    ),
    
    # Box 4: Latest data
    column(
      width = 3,
      div(
        style = "background-color: #2c3e50;
                 color: white;
                 text-align: center;
                 padding: 20px;
                 border-radius: 8px;
                 margin-bottom: 20px;",
        h2("2023", style = "margin: 0; font-size: 36px;
                            color: #d4af37;"),
        p("Latest Data", style = "margin: 5px 0 0 0;")
      )
    )
  ),
  
  # ---- About Section ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #f5f0e1;
                 padding: 30px;
                 border-radius: 8px;
                 border-left: 5px solid #2c3e50;
                 margin-bottom: 30px;",
        h3("About This Project",
           style = "font-family: Georgia, serif;
                    font-weight: bold;
                    margin-top: 0;"),
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
  
  # ---- How to Use This Dashboard ----
  div(
    style = "background-color: #2c3e50;
             color: white;
             padding: 30px 20px;
             margin-bottom: 30px;
             border-radius: 8px;",
    h3("How to Use This Dashboard",
       style = "text-align: center;
                font-family: Georgia, serif;
                margin-top: 0;"),
    fluidRow(
      # Step 1
      column(
        width = 4,
        div(
          style = "text-align: center; padding: 10px;",
          div(
            style = "background-color: #d4af37;
                     color: #2c3e50;
                     width: 50px; height: 50px;
                     border-radius: 50%;
                     line-height: 50px;
                     font-size: 24px;
                     font-weight: bold;
                     margin: 0 auto 10px auto;",
            "1"
          ),
          h4("Pick a Tab", style = "margin: 5px 0;"),
          p("Navigate using the tabs above to explore
             different views of the data.",
            style = "font-size: 14px; color: #cccccc;")
        )
      ),
      
      # Step 2
      column(
        width = 4,
        div(
          style = "text-align: center; padding: 10px;",
          div(
            style = "background-color: #d4af37;
                     color: #2c3e50;
                     width: 50px; height: 50px;
                     border-radius: 50%;
                     line-height: 50px;
                     font-size: 24px;
                     font-weight: bold;
                     margin: 0 auto 10px auto;",
            "2"
          ),
          h4("Select States or Years", style = "margin: 5px 0;"),
          p("Use dropdown menus and sliders to focus
             on specific states or time periods.",
            style = "font-size: 14px; color: #cccccc;")
        )
      ),
      
      # Step 3
      column(
        width = 4,
        div(
          style = "text-align: center; padding: 10px;",
          div(
            style = "background-color: #d4af37;
                     color: #2c3e50;
                     width: 50px; height: 50px;
                     border-radius: 50%;
                     line-height: 50px;
                     font-size: 24px;
                     font-weight: bold;
                     margin: 0 auto 10px auto;",
            "3"
          ),
          h4("Explore the Gap", style = "margin: 5px 0;"),
          p("Interact with visualizations to discover
             patterns and disparities.",
            style = "font-size: 14px; color: #cccccc;")
        )
      )
    )
  ),
  
  # ---- Feature Cards ----
  h3("Key Features",
     style = "text-align: center;
              font-family: Georgia, serif;"),
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
  
  # ---- Data Sources Section ----
  tags$br(),
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #f5f0e1;
                 padding: 25px;
                 border-radius: 8px;
                 text-align: center;
                 margin-bottom: 30px;",
        h3("Data Sources",
           style = "font-family: Georgia, serif;
                    color: #2c3e50;
                    margin-top: 0;"),
        p("All data is publicly available from federal agencies:"),
        fluidRow(
          column(
            width = 4,
            div(
              style = "padding: 10px;",
              h5("BLS", style = "color: #d4af37;
                                 font-weight: bold;"),
              p("State Productivity &",
                tags$br(),
                "QCEW Wage Data",
                style = "font-size: 13px;")
            )
          ),
          column(
            width = 4,
            div(
              style = "padding: 10px;",
              h5("BEA", style = "color: #d4af37;
                                 font-weight: bold;"),
              p("GDP by State",
                tags$br(),
                "Real Output Data",
                style = "font-size: 13px;")
            )
          ),
          column(
            width = 4,
            div(
              style = "padding: 10px;",
              h5("CPI", style = "color: #d4af37;
                                 font-weight: bold;"),
              p("Consumer Price Index",
                tags$br(),
                "Inflation Adjustment",
                style = "font-size: 13px;")
            )
          )
        )
      )
    )
  ),
  
  # ---- Footer ----
  tags$hr(),
  div(
    style = "text-align: center;
             color: #6c757d;
             padding: 20px;",
    p("BIO 185: Visualizing Big Data | Washington & Lee University")
  )
)