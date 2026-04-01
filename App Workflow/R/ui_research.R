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
    style = "background-color: #1a2744;
             color: #FFFFFF;
             text-align: center;
             padding: 40px 20px;
             margin-bottom: 30px;",
    h2("Research & Theory",
       style = "font-family: Georgia, serif;
                margin-bottom: 5px;"),
    p("The economics behind the productivity-wage gap",
      style = "color: #c8a96e;
               font-size: 16px;
               margin-top: 0;")
  ),
  
  # ---- Section 1: Marginal Productivity Theory ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #eae4d8;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #c8a96e;
                 margin-bottom: 25px;",
        h3("Marginal Productivity Theory",
           style = "font-family: Georgia, serif;
                    color: #1a2744;
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
                 border-left: 5px solid #1a2744;
                 margin-bottom: 25px;
                 border: 1px solid #d4c9b0;",
        h3("The EPI Argument: Policy Drove the Gap",
           style = "font-family: Georgia, serif;
                    color: #1a2744;
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
        style = "background-color: #eae4d8;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #c8a96e;
                 margin-bottom: 25px;",
        h3("BLS Industry-Level Evidence",
           style = "font-family: Georgia, serif;
                    color: #1a2744;
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
                 border-left: 5px solid #1a2744;
                 margin-bottom: 25px;
                 border: 1px solid #d4c9b0;",
        h3("The Measurement Debate",
           style = "font-family: Georgia, serif;
                    color: #1a2744;
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
  
  # ---- Section 5: W&L Faculty Research ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #eae4d8;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #c8a96e;
                 margin-bottom: 25px;",
        h3("W&L Faculty Research: The 4th Industrial Revolution",
           style = "font-family: Georgia, serif;
                    color: #1a2744;
                    margin-top: 0;"),
        p("Washington & Lee economists Art Goldsmith
           and Jim Casey have published research
           directly connecting technological change
           to the productivity-wage gap. Their 2024
           presidential address to the Southern
           Economic Association, published in the",
          tags$em("Southern Economic Journal,"),
          "argues that the 4th Industrial Revolution
           is fundamentally different from prior
           industrial revolutions."),
        p("Previous waves of technological change
           were skill-neutral: they improved
           productivity and wages for workers across
           the education spectrum. The technologies
           of the 4th IR, including AI, machine
           learning, and industrial robotics, are
           skill-biased. They augment the productivity
           of highly educated workers performing
           non-routine cognitive tasks while displacing
           those in middle-skill routine jobs."),
        p("This has produced job polarization: a
           hollowing out of middle-class employment.
           Goldsmith and Casey find that long-term
           unemployment (27+ weeks) rose from 13% of
           total unemployment before the 4th IR to
           nearly 30% by 2010-2023, which they view
           as indirect evidence of growing
           technological unemployment."),
        p("They propose policy solutions including
           tax incentives for firms that reinstate
           displaced workers and government-subsidized
           certificate programs as alternatives to
           traditional college degrees for reskilling."),
        p(
          tags$em("Sources: "),
          tags$a(
            href = "https://doi.org/10.1002/soej.12714",
            target = "_blank",
            "Goldsmith & Casey (2024),
             Southern Economic Journal"
          ),
          " | ",
          tags$a(
            href = "https://columns.wlu.edu/wl-professor-to-deliver-lecture-at-the-federal-reserve-bank-of-richmond/",
            target = "_blank",
            "W&L Columns: Goldsmith at the
             Richmond Fed"
          ),
          style = "font-size: 12px; color: #6c757d;"
        )
      )
    )
  ),
  
  # ---- Section 6: What Our Dashboard Shows ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #1a2744;
                 color: #FFFFFF;
                 padding: 25px;
                 border-radius: 8px;
                 margin-bottom: 25px;",
        h3("What Our Dashboard Explores",
           style = "font-family: Georgia, serif;
                    color: #c8a96e;
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
  
  # ---- Section 7: A Global Perspective: EU vs. U.S. ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #eae4d8;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #c8a96e;
                 margin-bottom: 25px;",
        h3("A Global Perspective: EU vs. U.S.",
           style = "font-family: Georgia, serif;
                    color: #1a2744;
                    margin-top: 0;"),
        p(tags$em("Backs: Outside US"),
          style = "color: #6c757d;
                   font-size: 13px;
                   margin-bottom: 15px;"),
        p("Is the productivity-wage decoupling
           uniquely American? The Outside US tab
           compares four series from 2000 to 2023:
           EU GDP per capita and EU labor
           productivity alongside U.S. compensation
           and U.S. labor productivity."),
        p("U.S. labor productivity rose from an
           index of about 177 in 2000 to 274 by
           2023. U.S. compensation grew much more
           slowly, from roughly 128 to 154. That
           widening gap is the core pattern this
           dashboard documents."),
        p("EU labor productivity stayed remarkably
           flat near an index of 100 over the same
           period. EU GDP per capita nearly tripled
           in nominal dollar terms, but that
           reflects price changes and economic
           expansion rather than a per-worker
           productivity surge like in the U.S."),
        p("The tab uses faceted panels with
           independent y-axes because the series
           operate on very different scales (raw
           dollars in the tens of thousands vs.
           index values near 100-274). This
           prevents one series from visually
           overwhelming the others."),
        p(
          tags$em("Data sources: "),
          tags$a(
            href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=EU",
            target = "_blank",
            "World Bank - EU GDP per Capita"
          ),
          " | ",
          tags$a(
            href = "https://data.ecb.europa.eu/data/data-categories/prices-macroeconomic-and-sectoral-statistics/labour-market/labour-productivity-and-unit-labour-costs/labour-productivity",
            target = "_blank",
            "ECB - Labour Productivity"
          ),
          " | ",
          tags$a(
            href = "https://fred.stlouisfed.org/series/COMPRNFB",
            target = "_blank",
            "FRED - Compensation"
          ),
          " | ",
          tags$a(
            href = "https://fred.stlouisfed.org/series/OPHNFB",
            target = "_blank",
            "FRED - Productivity"
          ),
          style = "font-size: 12px; color: #6c757d;"
        )
      )
    )
  ),
  
  # ---- Section 8: AI and the Future of the Gap ----
  fluidRow(
    column(
      width = 8, offset = 2,
      div(
        style = "background-color: #FFFFFF;
                 padding: 25px;
                 border-radius: 8px;
                 border-left: 5px solid #1a2744;
                 margin-bottom: 25px;
                 border: 1px solid #d4c9b0;",
        h3("Looking Ahead: AI and the Productivity-Pay Gap",
           style = "font-family: Georgia, serif;
                    color: #1a2744;
                    margin-top: 0;"),
        p("Artificial intelligence adds a new dimension
           to the productivity-wage debate. Previous
           waves of automation primarily displaced
           middle-skill, routine jobs like clerical work
           and factory assembly. AI is different: it
           increasingly affects higher-paying roles
           involving information processing and
           analysis."),
        p("A 2025 NBER study using 58 million LinkedIn
           profiles found that when AI can perform most
           tasks in a given role, employment in that
           role falls by about 14%. However, firms that
           adopted AI grew faster in revenue and
           employment, which offset many of these
           losses. Through 2023, AI-exposed roles had
           not experienced net job losses relative to
           other roles."),
        p("Whether AI will widen or narrow the
           productivity-pay gap remains an open
           question. Optimists argue that AI-driven
           productivity gains could lift wages if
           workers are equipped to use these tools.
           Skeptics warn that without policy
           intervention, the gains will flow
           disproportionately to capital owners and
           top earners, deepening the same patterns
           observed since the 1970s."),
        p("Recent productivity data has also drawn
           scrutiny. A Yale Budget Lab analysis
           cautions that apparent productivity gains
           in 2025 may reflect noisy measurement and
           compositional effects rather than a true
           AI-driven boom."),
        p("A World Economic Forum survey of chief
           economists found that while nearly half
           view worker augmentation as one of AI's
           biggest benefits for growth, increasing
           inequality ranked among their top
           assessed risks. Three out of four said
           government spending on upskilling and
           redeployment should be a priority."),
        p(
          tags$em("Sources: "),
          tags$a(
            href = "https://mitsloan.mit.edu/ideas-made-to-matter/how-artificial-intelligence-impacts-us-labor-market",
            target = "_blank",
            "MIT Sloan / NBER (2025)"
          ),
          " | ",
          tags$a(
            href = "https://budgetlab.yale.edu/research/ai-productivity-boom-dont-count-your-productivity-data-chickens",
            target = "_blank",
            "Yale Budget Lab (2026)"
          ),
          " | ",
          tags$a(
            href = "https://www.weforum.org/stories/2025/05/productivity-pay-artificial-intelligence/",
            target = "_blank",
            "World Economic Forum (2025)"
          ),
          style = "font-size: 12px; color: #6c757d;"
        )
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
                    color: #1a2744;"),
        p("Baker, D. (2024).",
          tags$em("The Productivity-Pay Gap and
                   Phony Debates."),
          "Center for Economic and Policy Research.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Brill, M., Holman, C., Morris, C.,
           Raichoudhary, R., & Yosif, N. (2017).",
          tags$em("Understanding the labor productivity
                   and compensation gap."),
          "Beyond the Numbers, 6(6).
           U.S. Bureau of Labor Statistics.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Clark, J.B. (1899).",
          tags$em("The Distribution of Wealth."),
          "Macmillan.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Economic Policy Institute. (2024).",
          tags$em("The Productivity-Pay Gap."),
          "Retrieved from epi.org.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("European Central Bank. (2025).",
          tags$em("Labour Productivity Data Portal."),
          "ECB Statistical Data Warehouse.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Gimbel, M. (2026).",
          tags$em("An AI Productivity Boom? Don't Count
                   Your (Productivity Data) Chickens."),
          "The Budget Lab at Yale.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Goldsmith, A.H. & Casey, J.F. (2024).",
          tags$em("The fourth industrial revolution
                   and the future of work: Reasons
                   to worry and policies to consider."),
          "Southern Economic Journal, 91(2), 333-350.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Letzing, J. (2025).",
          tags$em("AI could make us more productive,
                   can it also make us better paid?"),
          "World Economic Forum.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Schmidt, L., Hampole, M., Papanikolaou, D.,
           & Seegmiller, B. (2025).",
          tags$em("Artificial Intelligence and the
                   Labor Market."),
          "NBER Working Paper No. 33509.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("Schneider, D. (2023).",
          tags$em("Pay, Productivity, and the
                   Labor Share."),
          "Medium.",
          style = "font-size: 13px;
                   margin-bottom: 8px;"),
        p("World Bank. (2024).",
          tags$em("GDP per capita (current US$) -
                   European Union."),
          "World Development Indicators.",
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
