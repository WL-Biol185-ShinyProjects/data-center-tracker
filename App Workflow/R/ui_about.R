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
      
      tags$br(),
      
      # Creator 1: Cesar Cachay
      fluidRow(
        column(
          width = 10, offset = 1,
          div(
            style = "display: flex; align-items: start; margin-bottom: 30px; border-bottom: 1px solid #eee; padding-bottom: 20px;",
            # img(src = "cesar_cachay.jpg", width = "130px", style = "border-radius: 5px; margin-right: 25px;"),
            div(
              h4("Cesar Cachay", style = "font-weight: bold;"),
              p("Cesar is an Engineering student at Washington and Lee University interested in applying 
                 data science and analytics to solve complex problems. He is focused on 
                 leveraging quantitative tools to drive data-driven decision making."),
              tags$a(href = "https://www.linkedin.com/in/cesar-cachay/", target = "_blank", "LinkedIn Profile")
            )
          )
        )
      ), # <--- Ensure these commas exist!
      
      # Creator 2: Thomas K. Denton
      fluidRow(
        column(
          width = 10, offset = 1,
          div(
            style = "display: flex; align-items: start; margin-bottom: 30px; border-bottom: 1px solid #eee; padding-bottom: 20px;",
            # img(src = "thomas_denton.jpg", width = "130px", style = "border-radius: 5px; margin-right: 25px;"),
            div(
              h4("Thomas K. Denton", style = "font-weight: bold;"),
              p("Thomas is a student at Washington and Lee University pursuing a career in finance. 
                 A Level I CFA candidate, he focuses on how macroeconomic trends influence 
                 regional labor markets and investment strategies."),
              tags$a(href = "https://www.linkedin.com/in/thomaskdenton/", target = "_blank", "LinkedIn Profile")
            )
          )
        )
      ),
      
      # Creator 3: Jackson Maroon
      fluidRow(
        column(
          width = 10, offset = 1,
          div(
            style = "display: flex; align-items: start; margin-bottom: 30px; border-bottom: 1px solid #eee; padding-bottom: 20px;",
            # img(src = "jackson_maroon.jpg", width = "130px", style = "border-radius: 5px; margin-right: 25px;"),
            div(
              h4("Jackson Maroon", style = "font-weight: bold;"),
              p("Jackson is a senior at Washington and Lee University studying Business 
                 Administration with a minor in Data Science. The Head AI Fellow at the 
                 W&L AI Lab, he will join Kearney in NYC as a Business Analyst after graduation."),
              tags$a(href = "https://www.linkedin.com/in/jacksonmaroon/", target = "_blank", "LinkedIn Profile")
            )
          )
        )
      ),
      
      # Creator 4: Nicolas Toland
      fluidRow(
        column(
          width = 10, offset = 1,
          div(
            style = "display: flex; align-items: start; margin-bottom: 30px;",
            # img(src = "nicolas_toland.jpg", width = "130px", style = "border-radius: 5px; margin-right: 25px;"),
            div(
              h4("Nicolas Toland", style = "font-weight: bold;"),
              p("Nicolas is a student at Washington and Lee University. He is exploring the 
                 intersection of data science and visualization through the BIOL 185 curriculum, 
                 focusing on the communication of high-dimensional economic data.")
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
            tags$li(tags$a(href = "https://www.bls.gov/lpc/state-productivity.htm", "BLS State Productivity")),
            tags$li(tags$a(href = "https://www.bls.gov/cew/", "BLS QCEW")),
            tags$li(tags$a(href = "https://www.bea.gov/data/gdp/gdp-state", "BEA GDP by State")),
            tags$li(tags$a(href = "https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area", "BEA Regional Price Parities"))
          )
        ),
        column(
          width = 6,
          h4("Methods & Limitations", style = "font-family: Georgia, serif;"),
          tags$small(
            p("Real wages are calculated using the BEA Regional Price Parity (RPP) to account for 
               state-level inflation. GDP per job is derived from BEA total-economy data."),
            p("Note: These are descriptive cross-state signals, not causal claims.")
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