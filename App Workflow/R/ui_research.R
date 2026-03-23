# ============================================================
# ui_research.R - Research & Theory Tab (UI only)
# ============================================================
# Owner: [PERSON 1]
# This is a static content page. No server logic needed.
# Provides theoretical background on the productivity-wage gap.
# ============================================================

research_tab <- tabPanel(
  "Research",
  
  # ---- Page Title ----
  div(
    style = "background-color: #2c3e50;
             color: #FFFFFF;
             text-align: center;
             padding: 40px 20px;
             margin-bottom: 30px;",
    h2("Research & Theory",
       style = "font-family: Georgia, serif;
                margin-bottom: 5px;"),
    p("The economics behind the productivity-wage gap",
      style = "color: #d4af37;
               font-size: 16px;
               margin-top: 0;")
  ),
  
  # ---- Section 1: Marginal Productivity Theory ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #f5f0e1;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #d4af37;
                 margin-bottom: 25px;",
        h3("Marginal Productivity Theory",
           style = "font-family: Georgia, serif;
                    color: #2c3e50;
                    margin-top: 0;"),
        p("At the core of this dashboard is a fundamental
           question in economics: should workers be paid
           according to what they produce?"),
        p("Marginal Productivity Theory, developed by
           John Bates Clark in the 1890s, argues that
           in a competitive market, each worker is paid
           a wage equal to the value of the additional
           output they produce. If this theory holds,
           productivity and compensation should rise
           together over time."),
        p("From the 1940s through the mid-1970s, this
           is exactly what happened in the U.S. economy.
           But since then, the two measures have
           diverged, raising questions about whether
           the theory still describes reality."),
        p(
          tags$em("Source: "),
          tags$a(
            href = "https://www.britannica.com/money/marginal-productivity-theory",
            target = "_blank",
            "Britannica - Marginal Productivity Theory"
          ),
          style = "font-size: 12px; color: #6c757d;"
        )
      )
    )
  ),
  
  # ---- Section 2: The EPI Argument ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #FFFFFF;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #2c3e50;
                 margin-bottom: 25px;
                 border: 1px solid #dee2e6;",
        h3("The EPI Argument: Policy Drove the Gap",
           style = "font-family: Georgia, serif;
                    color: #2c3e50;
                    margin-top: 0;"),
        p("The Economic Policy Institute (EPI) has
           produced the most widely cited research on
           this topic. Their central finding: since 1979,
           productivity has grown roughly 3.5 times
           faster than typical worker pay."),
        p("EPI attributes the gap to intentional policy
           decisions that eroded worker bargaining power,
           including: declining unionization, stagnant
           minimum wages, deregulation, and corporate-led
           globalization. They argue that from 1948-1979,
           policies ensured growth was broadly shared,
           and the reversal of those policies caused
           the decoupling."),
        p("Their decomposition identifies three wedges:
           (1) growing inequality of compensation,
           (2) the erosion of labor's share of income,
           and (3) the divergence between consumer
           prices and output prices. Inequality-related
           factors account for over 80% of the gap
           since 2000."),
        p(
          tags$em("Source: "),
          tags$a(
            href = "https://www.epi.org/productivity-pay-gap/",
            target = "_blank",
            "EPI - The Productivity-Pay Gap"
          ),
          style = "font-size: 12px; color: #6c757d;"
        )
      )
    )
  ),
  
  # ---- Section 3: BLS Industry-Level Findings ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #f5f0e1;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #d4af37;
                 margin-bottom: 25px;",
        h3("BLS Industry-Level Evidence",
           style = "font-family: Georgia, serif;
                    color: #2c3e50;
                    margin-top: 0;"),
        p("A 2017 Bureau of Labor Statistics study
           examined the gap across 183 detailed
           industries from 1987-2015, finding that
           productivity outpaced compensation in
           83% of industries studied."),
        p("The BLS identified two key components
           of the gap: (1) the difference between
           consumer price and output price deflators,
           and (2) a declining labor share of income.
           Industries with the largest productivity
           gains, especially IT-related fields,
           experienced the widest gaps."),
        p("Importantly, when compensation was adjusted
           using output price deflators instead of the
           CPI, the gaps shrank considerably, with
           87% of previously-gapped industries showing
           smaller divergence."),
        p(
          tags$em("Source: "),
          tags$a(
            href = "https://www.bls.gov/opub/btn/volume-6/understanding-the-labor-productivity-and-compensation-gap.htm",
            target = "_blank",
            "BLS - Understanding the Labor Productivity
             and Compensation Gap (2017)"
          ),
          style = "font-size: 12px; color: #6c757d;"
        )
      )
    )
  ),
  
  # ---- Section 4: The Measurement Debate ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #FFFFFF;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #2c3e50;
                 margin-bottom: 25px;
                 border: 1px solid #dee2e6;",
        h3("The Measurement Debate",
           style = "font-family: Georgia, serif;
                    color: #2c3e50;
                    margin-top: 0;"),
        p("Not all economists agree the gap is as
           large as EPI claims. Critics raise several
           methodological concerns:"),
        p(tags$strong("Different workers: "),
          "EPI measures productivity for all workers
           but compares it to pay for only
           production/nonsupervisory workers. When
           economy-wide pay is compared to
           economy-wide productivity, the gap
           narrows significantly."),
        p(tags$strong("Different deflators: "),
          "Productivity is deflated using output
           prices, while wages are deflated using
           the CPI. These two price indexes have
           diverged since the 1970s. Using the same
           deflator for both closes much of the gap."),
        p(tags$strong("Median vs. average: "),
          "The gap is between productivity and
           median pay, not average pay. Rising
           inequality within the wage distribution
           (high earners pulling away) explains much
           of the wedge, which even EPI acknowledges."),
        p("When all three adjustments are made,
           some researchers find that total
           compensation has tracked productivity
           closely throughout the entire post-war
           period."),
        p(
          tags$em("Sources: "),
          tags$a(
            href = "https://cepr.net/publications/the-productivity-pay-gap-and-phony-debates/",
            target = "_blank",
            "CEPR - Dean Baker (2024)"
          ),
          " | ",
          tags$a(
            href = "https://medium.com/@donaldfschneider_56329/pay-productivity-and-the-labor-share-8a241ba539a0",
            target = "_blank",
            "Schneider - Pay, Productivity, and
             the Labor Share (2023)"
          ),
          style = "font-size: 12px; color: #6c757d;"
        )
      )
    )
  ),
  
  # ---- Section 5: What Our Dashboard Shows ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #2c3e50;
                 color: #FFFFFF;
                 padding: 25px;
                 border-radius: 8px;
                 margin-bottom: 25px;",
        h3("What Our Dashboard Explores",
           style = "font-family: Georgia, serif;
                    color: #d4af37;
                    margin-top: 0;"),
        p("This dashboard brings the national debate
           down to the state level. Using BLS and BEA
           data, we visualize whether the
           productivity-compensation gap varies across
           states, how it has changed over time, and
           which states show the largest divergence."),
        p("Navigate to the other tabs to explore
           the data yourself and form your own
           conclusions about whether workers are
           being compensated in line with their
           productivity.")
      )
    )
  ),
  
  # ---- References List ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "padding: 20px;
                 margin-bottom: 20px;",
        h4("References",
           style = "font-family: Georgia, serif;
                    color: #2c3e50;"),
        p("Brill, M., Holman, C., Morris, C.,
           Raichoudhary, R., & Yosif, N. (2017).",
          tags$em("Understanding the labor productivity
                   and compensation gap."),
          "Beyond the Numbers, 6(6).
           U.S. Bureau of Labor Statistics.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Economic Policy Institute. (2024).",
          tags$em("The Productivity-Pay Gap."),
          "Retrieved from epi.org.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Baker, D. (2024).",
          tags$em("The Productivity-Pay Gap and
                   Phony Debates."),
          "Center for Economic and Policy Research.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Schneider, D. (2023).",
          tags$em("Pay, Productivity, and the
                   Labor Share."),
          "Medium.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Clark, J.B. (1899).",
          tags$em("The Distribution of Wealth."),
          "Macmillan.",
          style = "font-size: 13px;
                   margin-bottom: 8px;")
      )
    )
  ),
  
  # ---- Footer ----
  tags$hr(),
  div(
    style = "text-align: center;
             color: #6c757d;
             padding: 20px;",
    p("BIO 185: Visualizing Big Data |
       Washington & Lee University")
  )
)