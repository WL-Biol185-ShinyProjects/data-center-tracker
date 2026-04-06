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
    p("An interactive atlas of state-level productivity, compensation, and wage levels",
      "that asks where pay has kept up with output and where it has not.",
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
        h2("51", style = "margin: 0; font-size: 36px;
                          color: #d4af37;"),
        p("States + DC", style = "margin: 5px 0 0 0;")
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
        h2("2", style = "margin: 0; font-size: 36px;
                          color: #d4af37;"),
        p("Analysis Frames", style = "margin: 5px 0 0 0;")
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
        h2("2007-2024", style = "margin: 0; font-size: 36px;
                            color: #d4af37;"),
        p("Growth Frame", style = "margin: 5px 0 0 0;")
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
        h2("2008-2023", style = "margin: 0; font-size: 36px;
                            color: #d4af37;"),
        p("Complete Levels Frame", style = "margin: 5px 0 0 0;")
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
                 margin-bottom: 30px;
                 text-align: center;",
        h3("About This Project",
           style = "font-family: Georgia, serif;
                    font-weight: bold;
                    margin-top: 0;"),
        p("Our central research question is whether wages have decoupled
           from productivity across U.S. states and, if so, where the gap
           is largest and how it changes over time."),
        p("To answer that, the app combines a growth frame
           (productivity and compensation indices rebased to 2007 = 100)
           with a levels frame (GDP per job, weekly wages, and real pay to
           productivity ratios)."),
        tags$br(),
        p("Use the tabs above to move from broad pattern-finding
           to specific explanation: the map shows where the gap is largest,
           comparison lines show whether it widened or narrowed over time,
           rankings show who is furthest apart in a given year, and
           levels charts show whether high-output states also deliver high pay.")
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
          h4("Start With a Question", style = "margin: 5px 0;"),
          p("Choose the tab that matches what you want to know:
             where, when, or for which states the gap is largest.",
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
          h4("Read the Metric", style = "margin: 5px 0;"),
          p("Use the controls to pick a year or state, then read the note on each tab
             to see exactly what the chart is measuring.",
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
          h4("Compare and Interpret", style = "margin: 5px 0;"),
          p("Compare states, inspect rankings, and download the filtered data
             if you want evidence for a specific claim.",
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
        p("Answers: where is the productivity-pay gap largest in a selected year?")
      )
    ),
    
    # Card 2: State Comparison
    column(
      width = 3,
      div(
        class = "feature-card",
        h4("State Comparison"),
        p("Answers: did wages and productivity move together or apart over time in one or two states?")
      )
    ),
    
    # Card 3: Gap Rankings
    column(
      width = 3,
      div(
        class = "feature-card",
        h4("Gap Rankings"),
        p("Answers: which states are most decoupled once we turn the map into an ordered table?")
      )
    ),
    
    # Card 4: Levels Comparison
    column(
      width = 3,
      div(
        class = "feature-card",
        h4("Levels Comparison"),
        p("Answers: do high-output states also pay high wages, or are those relationships misaligned?")
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
        p("All charts are built from public data, but different tabs use different concepts.
           Growth tabs use indexed productivity and compensation series; levels tabs use GDP per job,
           weekly wages, and regional price parity adjustments."),
        fluidRow(
          column(
            width = 3,
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
            width = 3,
            div(
              style = "padding: 10px;",
              h5("BEA", style = "color: #d4af37;
                                 font-weight: bold;"),
              p("GDP by State",
                tags$br(),
                "and RPP Data",
                style = "font-size: 13px;")
            )
          ),
          column(
            width = 3,
            div(
              style = "padding: 10px;",
              h5("FRED", style = "color: #d4af37;
                                 font-weight: bold;"),
              p("National compensation",
                tags$br(),
                "and productivity series",
                style = "font-size: 13px;")
            )
          ),
          column(
            width = 3,
            div(
              style = "padding: 10px;",
              h5("ECB / World Bank", style = "color: #d4af37;
                                 font-weight: bold;"),
              p("EU context for the",
                tags$br(),
                "Outside U.S. tab",
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
