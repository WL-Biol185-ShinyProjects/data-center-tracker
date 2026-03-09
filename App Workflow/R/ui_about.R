# ============================================================
# ui_about.R - Enhanced About Tab
# ============================================================
# Project: Labor Productivity vs. Hourly Compensation
# Course: BIOL 185 - Visualizing Big Data
# ============================================================

about_tab <- tabPanel(
  "About",
  
  fluidPage(
    column(
      width = 10, offset = 1,
      
      # --- Project Header ---
      div(
        style = "text-align: center; padding-bottom: 30px;",
        h1("The Productivity-Pay Gap Explorer", style = "font-family: Georgia, serif; font-weight: bold;"),
        h4("Analyzing the Divergence of Labor Productivity and Compensation Across U.S. States", 
           style = "font-family: Georgia, serif; color: #555;")
      ),
      
      hr(),
      
      # --- Section 1: Project Description & Motivation ---
      fluidRow(
        column(
          width = 8,
          h3("Project Motivation", style = "font-family: Georgia, serif;"),
          p("For decades, the relationship between how much workers produce and how much they are paid 
             has been a central question in American economics. This dashboard was developed to 
             visualize these trends at the state level, allowing users to investigate whether the 
             'productivity-pay gap' is a uniform national phenomenon or if it varies significantly 
             by regional geography and local industry composition."),
          p("By comparing Bureau of Labor Statistics (BLS) productivity indices with real hourly 
             compensation, this tool provides a descriptive look at labor's share of economic growth 
             since 2007.")
        ),
        column(
          width = 4,
          wellPanel(
            h4("Key Metrics", style = "font-family: Georgia, serif;"),
            tags$ul(
              tags$li(strong("Gap Metric:"), "Productivity Index - Compensation Index."),
              tags$li(strong("Levels Ratio:"), "Real pay (QCEW) / Real GDP per job (BEA)."),
              tags$li(strong("Baseline:"), "All trends rebased to 2007 = 100.")
            )
          )
        )
      ),
      
      hr(),
      
      # --- Section 2: Meet the Creators ---
      h3("About the Creators", style = "font-family: Georgia, serif;"),
      p("We are students at Washington and Lee University exploring the intersection of 
         economics, data science, and visualization through the BIOL 185 curriculum."),
      
      fluidRow(
        # Creator 1 (Thomas)
        column(
          width = 6,
          div(
            style = "display: flex; align-items: start; margin-bottom: 20px;",
            # Uncomment the img tag below once you add a photo to your /www folder
            # img(src = "thomas_denton.jpg", width = "120px", style = "border-radius: 50%; margin-right: 20px;"),
            div(
              h4("Thomas K. Denton"),
              p("Thomas is a student at Washington and Lee University pursuing a career in finance. 
                 He is a Level I CFA candidate with a strong interest in how macroeconomic trends 
                 influence regional labor markets and investment strategies.")
            )
          )
        ),
        # Creator 2 (Placeholder - e.g., Dylan)
        column(
          width = 6,
          div(
            style = "display: flex; align-items: start; margin-bottom: 20px;",
            # img(src = "dylan_reher.jpg", width = "120px", style = "border-radius: 50%; margin-right: 20px;"),
            div(
              h4("Dylan Reher"),
              p("Dylan is a student in BIOL 185 at Washington and Lee University. He contributed 
                 to the data wrangling and visualization architecture of this dashboard, focusing 
                 on the relationship between regional price parities and real wage growth.")
            )
          )
        )
      ),
      
      hr(),
      
      # --- Section 3: Data Sources & Methodology ---
      fluidRow(
        column(
          width = 6,
          h4("Data Sources", style = "font-family: Georgia, serif;"),
          tags$ul(
            tags$li(tags$a(href = "https://www.bls.gov/lpc/state-productivity.htm", "BLS State Productivity"), 
                    ": State-level output and hours worked."),
            tags$li(tags$a(href = "https://www.bls.gov/cew/", "BLS QCEW"), 
                    ": Quarterly Census of Employment and Wages for private-sector pay."),
            tags$li(tags$a(href = "https://www.bea.gov/data/gdp/gdp-state", "BEA GDP by State"), 
                    ": Real GDP data in chained 2017 dollars."),
            tags$li(tags$a(href = "https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area", "BEA RPP"), 
                    ": Regional Price Parities used for spatial cost-of-living adjustments.")
          )
        ),
        column(
          width = 6,
          h4("Methods & Limitations", style = "font-family: Georgia, serif;"),
          tags$small(
            p("Real wages are calculated using the BEA Regional Price Parity (RPP) to account for 
               state-level inflation and cost-of-living differences. GDP per job is derived from 
               BEA total-economy data, while wages are sourced from QCEW private-sector records."),
            p("Note: These visualizations represent descriptive cross-state signals and should 
               not be interpreted as causal or structural labor-share estimates.")
          )
        )
      ),
      
      hr(),
      
      # --- Footer & Disclaimer ---
      div(
        style = "text-align: center; color: #6c757d; padding: 30px; font-size: 0.9em;",
        p("Developed for BIOL 185: Visualizing Big Data | Washington & Lee University | Spring 2026"),
        p(strong("AI-Assisted Development:"), "This application was developed with assistance from 
           large language models for code optimization and documentation polishing."),
        p(strong("Disclaimer:"), "This tool is for educational purposes only and does not 
           constitute professional economic or financial advice.")
      )
    )
  )
)

