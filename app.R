library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(scales)

load_panel_data <- function() {
  data_path <- file.path("data", "state_panel.csv")
  if (!file.exists(data_path)) {
    return(NULL)
  }
  read.csv(data_path, stringsAsFactors = FALSE)
}

to_title <- function(x) {
  tools::toTitleCase(x)
}

ui <- fluidPage(
  titlePanel("Productivity vs Wages Atlas"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("Private nonfarm, state-level annual metrics from the BLS productivity family."),
      selectInput("year", "Year", choices = NULL),
      selectInput("state", "State", choices = NULL)
    ),
    mainPanel(
      width = 9,
      uiOutput("missing_data_notice"),
      tabsetPanel(
        tabPanel("Map View", plotOutput("gap_map", height = "550px")),
        tabPanel("State Comparison", plotOutput("state_trends", height = "420px")),
        tabPanel("Gap Rankings", tableOutput("rank_table")),
        tabPanel(
          "About",
          h4("Methods"),
          p("This app compares labor productivity and hourly compensation across states."),
          p("All trend lines are rebased to 2007 = 100 for readability."),
          p("Gap metric = labor productivity index minus hourly compensation index."),
          p("Interpretation note: these are descriptive comparisons, not causal claims."),
          tags$ul(
            tags$li(tags$a(href = "https://www.bls.gov/lpc/state-productivity.htm", target = "_blank", "BLS Productivity by State")),
            tags$li(tags$a(href = "https://www.bls.gov/cew/", target = "_blank", "BLS QCEW")),
            tags$li(tags$a(href = "https://www.bea.gov/data/gdp/gdp-state", target = "_blank", "BEA GDP by State")),
            tags$li(tags$a(href = "https://www.bea.gov/data/employment/employment-state", target = "_blank", "BEA Employment by State")),
            tags$li(tags$a(href = "https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area", target = "_blank", "BEA Regional Price Parities"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  panel_data <- reactiveVal(load_panel_data())

  observe({
    df <- panel_data()
    req(!is.null(df), nrow(df) > 0)

    years <- sort(unique(df$year))
    states <- sort(unique(df$state_name))

    updateSelectInput(session, "year", choices = years, selected = max(years))
    updateSelectInput(
      session,
      "state",
      choices = setNames(states, to_title(states)),
      selected = if ("california" %in% states) "california" else states[1]
    )
  })

  output$missing_data_notice <- renderUI({
    if (!is.null(panel_data())) {
      return(NULL)
    }
    tags$div(
      class = "alert alert-warning",
      "Data not found. Run `Rscript data-raw/run_all.R` from the project root first."
    )
  })

  yearly_data <- reactive({
    req(panel_data(), input$year)
    panel_data() %>% filter(year == input$year)
  })

  output$gap_map <- renderPlot({
    req(yearly_data())

    us_map <- map_data("state")
    map_df <- us_map %>%
      left_join(
        yearly_data() %>% select(state_name, gap_index_2007),
        by = c("region" = "state_name")
      )

    ggplot(map_df, aes(long, lat, group = group, fill = gap_index_2007)) +
      geom_polygon(color = "white", linewidth = 0.15) +
      coord_fixed(1.3) +
      scale_fill_gradient2(
        low = "#B2182B",
        mid = "#F7F7F7",
        high = "#2166AC",
        midpoint = 0,
        na.value = "grey90",
        labels = label_number(accuracy = 0.1),
        name = "Gap\n(Index pts)"
      ) +
      labs(
        title = paste("Productivity-Compensation Gap by State (", input$year, ")", sep = ""),
        subtitle = "Gap = labor productivity index (2007 = 100) minus hourly compensation index (2007 = 100)",
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })

  output$state_trends <- renderPlot({
    req(panel_data(), input$state)

    df <- panel_data() %>%
      filter(state_name == input$state) %>%
      arrange(year) %>%
      select(
        year,
        labor_productivity_index_2007,
        hourly_compensation_index_2007
      ) %>%
      pivot_longer(
        cols = -year,
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric = recode(
          metric,
          labor_productivity_index_2007 = "Labor Productivity",
          hourly_compensation_index_2007 = "Hourly Compensation"
        )
      )

    ggplot(df, aes(x = year, y = value, color = metric)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      scale_color_manual(values = c("Labor Productivity" = "#2166AC", "Hourly Compensation" = "#B2182B")) +
      labs(
        title = paste("State Trend:", to_title(input$state)),
        subtitle = "Both series rebased to 2007 = 100",
        x = NULL,
        y = "Index (2007 = 100)",
        color = NULL
      ) +
      theme_minimal(base_size = 12)
  })

  output$rank_table <- renderTable({
    req(yearly_data())
    yearly_data() %>%
      transmute(
        State = to_title(state_name),
        `Productivity (2007=100)` = round(labor_productivity_index_2007, 1),
        `Compensation (2007=100)` = round(hourly_compensation_index_2007, 1),
        `Gap` = round(gap_index_2007, 1)
      ) %>%
      arrange(desc(Gap))
  })
}

shinyApp(ui, server)
