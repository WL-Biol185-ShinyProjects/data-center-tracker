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
          p("For decades, economists have asked whether workers are being paid in line with what they produce.
             This project brings that national debate down to the state level."),
          p("The app uses two complementary data frames: a growth frame that compares indexed productivity
             and compensation over time, and a levels frame that compares GDP per job to wages across states.
             Together, those views let us ask not only whether the gap exists, but where it is largest,
             how it changes, and whether high-output states also deliver high pay.")
        ),
        column(
          width = 4,
          wellPanel(
            h4("How the Visuals Answer the Question", style = "font-family: Georgia, serif;"),
            tags$ul(
              tags$li(strong("Map View:"), "shows where the gap is largest in a selected year."),
              tags$li(strong("State Comparison:"), "shows whether the gap widens or narrows over time."),
              tags$li(strong("Gap Rankings:"), "turns the map into an ordered comparison table."),
              tags$li(strong("Levels Comparison:"), "tests whether high-productivity states also show high pay.")
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
                 focusing on the communication of high-dimensional data for practical viewership.") ,
              tags$a(href = "www.linkedin.com/in/nicolas-toland-42457626b", target = "_blank", "LinkedIn Profile")
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
            tags$li(tags$a(href = "https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area", "BEA Regional Price Parities (Accessed March 18)")),
            tags$li(tags$a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD", "EU GDP Data (Accessed March 18" )),
            tags$li(tags$a(href = "https://data.ecb.europa.eu/data/datasets/MNA/MNA.Q.Y.I9.W0.S1.S1._Z.LPR_HW._Z.OTQ._Z.IX.LR.N", "EU Worker Productivity (Accessed March 18)")),
            tags$li(tags$a(href = "https://fred.stlouisfed.org/graph/?g=11CrY&utm_source=direct&utm_medium=exported-chart&utm_campaign=myfred_referrer","Data for US Worker Productivity and Indexed Compensation (Accessed March 18)")),
          )
        ),
        column(
          width = 6,
          h4("Methods & Limitations", style = "font-family: Georgia, serif;"),
          tags$small(
            p("Growth tabs use indices rebased to 2007 = 100, so they emphasize divergence over time rather than absolute dollar levels."),
            p("Levels tabs use BEA GDP per job and QCEW wages, with real wage measures adjusted by BEA Regional Price Parities (RPP)."),
            p("These charts are descriptive rather than causal. They show where pay and output move together or apart, but they do not prove why.")
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
