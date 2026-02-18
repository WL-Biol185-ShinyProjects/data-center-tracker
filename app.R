library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

load_panel_data <- function() {
  data_path <- file.path("data", "state_panel.csv")
  if (!file.exists(data_path)) {
    return(NULL)
  }
  read.csv(data_path, stringsAsFactors = FALSE)
}

load_levels_data <- function() {
  data_path <- file.path("data", "state_levels_panel.csv")
  if (!file.exists(data_path)) {
    return(NULL)
  }
  read.csv(data_path, stringsAsFactors = FALSE)
}

to_title <- function(x) {
  tools::toTitleCase(x)
}

range_label <- function(values) {
  years <- sort(unique(values[!is.na(values)]))
  if (length(years) == 0) {
    return("No data")
  }
  if (length(years) == 1) {
    return(as.character(years))
  }
  paste0(min(years), "-", max(years))
}

levels_metric_labels <- c(
  gdp_per_job_real_2017 = "GDP per Job (Real, 2017 dollars)",
  qcew_avg_weekly_wage_real_rpp = "Weekly Wage (Real, RPP-adjusted)",
  wage_to_productivity_ratio_real = "Wage-to-Productivity Ratio"
)
levels_metric_choices <- stats::setNames(names(levels_metric_labels), levels_metric_labels)

load_us_map <- function() {
  if (!requireNamespace("maps", quietly = TRUE)) {
    return(NULL)
  }
  ggplot2::map_data("state")
}

ui <- fluidPage(
  titlePanel("Productivity vs Wages Atlas"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("Private nonfarm, state-level annual metrics from the BLS productivity family."),
      selectInput("year", "Year", choices = NULL),
      selectInput("state", "State", choices = NULL),
      selectInput("levels_metric", "Levels Map Metric", choices = levels_metric_choices)
    ),
    mainPanel(
      width = 9,
      uiOutput("missing_data_notice"),
      uiOutput("data_freshness_banner"),
      tabsetPanel(
        id = "main_tab",
        tabPanel(
          "Map View",
          plotOutput("gap_map", height = "550px"),
          tags$br(),
          downloadButton("download_gap_map", "Download Map Data (CSV)")
        ),
        tabPanel(
          "State Comparison",
          plotOutput("state_trends", height = "420px"),
          tags$br(),
          downloadButton("download_state_trends", "Download State Trend Data (CSV)")
        ),
        tabPanel(
          "Gap Rankings",
          tableOutput("rank_table"),
          tags$br(),
          downloadButton("download_rankings", "Download Ranking Data (CSV)")
        ),
        tabPanel(
          "Levels Comparison",
          p("Cross-sectional levels view: BEA real GDP per job vs QCEW private-sector wages (RPP-adjusted)."),
          uiOutput("levels_unavailable_note"),
          plotOutput("levels_map", height = "520px"),
          uiOutput("levels_scatter_ui"),
          tableOutput("levels_rank_table"),
          tags$br(),
          downloadButton("download_levels", "Download Levels Data (CSV)")
        ),
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
  levels_data <- reactiveVal(load_levels_data())

  observe({
    df <- panel_data()
    req(!is.null(df), nrow(df) > 0)

    states <- sort(unique(df$state_name))
    updateSelectInput(
      session,
      "state",
      choices = setNames(states, to_title(states)),
      selected = if ("california" %in% states) "california" else states[1]
    )
  })

  observe({
    growth_df <- panel_data()
    req(!is.null(growth_df), nrow(growth_df) > 0)

    years <- sort(unique(growth_df$year))

    if (identical(input$main_tab, "Levels Comparison") && !is.null(levels_data()) && !is.null(input$levels_metric)) {
      ldf <- levels_data()
      metric_col <- input$levels_metric

      if (metric_col %in% names(ldf)) {
        metric_ok <- !is.na(ldf[[metric_col]])
        scatter_ok <- !is.na(ldf$gdp_per_job_real_2017) & !is.na(ldf$qcew_avg_weekly_wage_real_rpp)
        candidate_years <- sort(unique(ldf$year[metric_ok & scatter_ok]))
        if (length(candidate_years) > 0) {
          years <- candidate_years
        }
      }
    }

    selected_year <- isolate(input$year)
    selected_year <- if (!is.null(selected_year) && selected_year %in% years) selected_year else max(years)
    updateSelectInput(session, "year", choices = years, selected = selected_year)
  })

  output$missing_data_notice <- renderUI({
    if (!is.null(panel_data()) && !is.null(levels_data())) {
      return(NULL)
    }

    notes <- list()
    if (is.null(panel_data())) {
      notes <- c(notes, tags$li("Growth data missing: run `Rscript data-raw/run_all.R`."))
    }
    if (is.null(levels_data())) {
      notes <- c(notes, tags$li("Levels data missing: run `Rscript data-raw/run_levels_all.R`."))
    }

    tags$div(
      class = "alert alert-warning",
      tags$strong("Data setup required:"),
      tags$ul(notes)
    )
  })

  output$data_freshness_banner <- renderUI({
    if (is.null(panel_data()) && is.null(levels_data())) {
      return(NULL)
    }

    growth_span <- if (!is.null(panel_data())) {
      range_label(panel_data()$year)
    } else {
      "No data"
    }

    levels_span_gdp <- if (!is.null(levels_data())) {
      range_label(levels_data()$year[!is.na(levels_data()$gdp_per_job_real_2017)])
    } else {
      "No data"
    }

    levels_span_qcew_nominal <- if (!is.null(levels_data())) {
      range_label(levels_data()$year[!is.na(levels_data()$qcew_avg_weekly_wage_nominal)])
    } else {
      "No data"
    }

    levels_span_qcew_real <- if (!is.null(levels_data())) {
      range_label(levels_data()$year[!is.na(levels_data()$qcew_avg_weekly_wage_real_rpp)])
    } else {
      "No data"
    }

    tags$div(
      class = "alert alert-info",
      tags$strong("Data Freshness"),
      tags$ul(
        tags$li(paste("Growth panel (BLS productivity-family):", growth_span)),
        tags$li(paste("Levels GDP/job (BEA):", levels_span_gdp)),
        tags$li(paste("Levels weekly wage nominal (QCEW):", levels_span_qcew_nominal)),
        tags$li(paste("Levels weekly wage real (QCEW + RPP):", levels_span_qcew_real))
      )
    )
  })

  yearly_data <- reactive({
    req(panel_data(), input$year)
    panel_data() %>% filter(year == input$year)
  })

  output$gap_map <- renderPlot({
    req(yearly_data())
    us_map <- load_us_map()
    validate(
      need(!is.null(us_map), "Map dependency missing: install package `maps` to render map views.")
    )

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

  levels_yearly_data <- reactive({
    req(levels_data(), input$year)
    levels_data() %>% filter(year == input$year)
  })

  output$levels_unavailable_note <- renderUI({
    validate(
      need(!is.null(levels_data()), "Run `Rscript data-raw/run_levels_all.R` to build levels data.")
    )

    if (nrow(levels_yearly_data()) > 0) {
      return(NULL)
    }

    tags$div(
      class = "alert alert-warning",
      paste0("No levels data available for year ", input$year, ".")
    )
  })

  output$levels_map <- renderPlot({
    validate(
      need(!is.null(levels_data()), "Run `Rscript data-raw/run_levels_all.R` to build levels data.")
    )

    metric_col <- input$levels_metric
    metric_label <- levels_metric_labels[[metric_col]]

    plot_df <- levels_yearly_data() %>%
      filter(!is.na(.data[[metric_col]])) %>%
      select(state_name, value = all_of(metric_col))

    validate(
      need(nrow(plot_df) > 0, paste("No map data for year", input$year, "and selected metric"))
    )

    us_map <- load_us_map()
    validate(
      need(!is.null(us_map), "Map dependency missing: install package `maps` to render map views.")
    )

    map_df <- us_map %>%
      left_join(plot_df, by = c("region" = "state_name"))

    value_labels <- if (metric_col %in% c("gdp_per_job_real_2017", "qcew_avg_weekly_wage_real_rpp")) {
      label_dollar(accuracy = 1)
    } else {
      label_number(accuracy = 0.001)
    }

    ggplot(map_df, aes(long, lat, group = group, fill = value)) +
      geom_polygon(color = "white", linewidth = 0.15) +
      coord_fixed(1.3) +
      scale_fill_viridis_c(
        option = "C",
        na.value = "grey90",
        labels = value_labels,
        name = metric_label
      ) +
      labs(
        title = paste("Levels Map (", input$year, ")", sep = ""),
        subtitle = metric_label,
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

  levels_scatter_data <- reactive({
    req(levels_yearly_data())
    levels_yearly_data() %>%
      filter(!is.na(gdp_per_job_real_2017), !is.na(qcew_avg_weekly_wage_real_rpp))
  })

  output$levels_scatter_ui <- renderUI({
    if (requireNamespace("plotly", quietly = TRUE)) {
      return(plotly::plotlyOutput("levels_scatter", height = "420px"))
    }
    plotOutput("levels_scatter_static", height = "420px")
  })

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$levels_scatter <- plotly::renderPlotly({
      validate(
        need(!is.null(levels_data()), "Run `Rscript data-raw/run_levels_all.R` to build levels data.")
      )

      df <- levels_scatter_data() %>%
        mutate(
          hover_text = sprintf(
            "State: %s (%s)<br>GDP/job: %s<br>Real weekly wage: %s<br>Wage/productivity ratio: %.4f",
            to_title(state_name),
            state_abbr,
            label_dollar(accuracy = 1)(gdp_per_job_real_2017),
            label_dollar(accuracy = 1)(qcew_avg_weekly_wage_real_rpp),
            wage_to_productivity_ratio_real
          )
        )

      validate(
        need(nrow(df) > 0, paste("No levels data for year", input$year))
      )

      gg <- ggplot(df, aes(
        x = gdp_per_job_real_2017,
        y = qcew_avg_weekly_wage_real_rpp,
        text = hover_text,
        label = state_abbr
      )) +
        geom_point(color = "#2166AC", alpha = 0.8, size = 2.6) +
        geom_text(size = 2.6, check_overlap = TRUE, vjust = -0.6, color = "#444444") +
        geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.9) +
        scale_x_continuous(labels = label_dollar()) +
        scale_y_continuous(labels = label_dollar()) +
        labs(
          title = paste("State Levels Comparison (", input$year, ")", sep = ""),
          subtitle = "x: BEA real GDP per job | y: QCEW private average weekly wage (RPP-adjusted)",
          x = "Real GDP per job (2017 dollars)",
          y = "Real weekly wage (RPP-adjusted)"
        ) +
        theme_minimal(base_size = 12)

      plotly::ggplotly(gg, tooltip = "text") |>
        plotly::config(displayModeBar = FALSE)
    })
  }

  output$levels_scatter_static <- renderPlot({
    validate(
      need(!is.null(levels_data()), "Run `Rscript data-raw/run_levels_all.R` to build levels data.")
    )

    df <- levels_scatter_data()

    validate(
      need(nrow(df) > 0, paste("No levels data for year", input$year))
    )

    ggplot(df, aes(x = gdp_per_job_real_2017, y = qcew_avg_weekly_wage_real_rpp, label = state_abbr)) +
      geom_point(color = "#2166AC", alpha = 0.8, size = 2.6) +
      geom_text(size = 2.6, check_overlap = TRUE, vjust = -0.6, color = "#444444") +
      geom_smooth(method = "lm", se = FALSE, color = "#B2182B", linewidth = 0.9) +
      scale_x_continuous(labels = label_dollar()) +
      scale_y_continuous(labels = label_dollar()) +
      labs(
        title = paste("State Levels Comparison (", input$year, ")", sep = ""),
        subtitle = "x: BEA real GDP per job | y: QCEW private average weekly wage (RPP-adjusted)",
        x = "Real GDP per job (2017 dollars)",
        y = "Real weekly wage (RPP-adjusted)"
      ) +
      theme_minimal(base_size = 12)
  })

  output$levels_rank_table <- renderTable({
    validate(
      need(!is.null(levels_data()), "Run `Rscript data-raw/run_levels_all.R` to build levels data.")
    )

    metric_col <- input$levels_metric
    metric_is_dollar <- metric_col %in% c("gdp_per_job_real_2017", "qcew_avg_weekly_wage_real_rpp")

    levels_yearly_data() %>%
      filter(!is.na(.data[[metric_col]])) %>%
      transmute(
        State = to_title(state_name),
        `Selected Metric` = if (metric_is_dollar) round(.data[[metric_col]], 0) else round(.data[[metric_col]], 4),
        `GDP per Job (Real)` = round(gdp_per_job_real_2017, 0),
        `Weekly Wage (Nominal)` = round(qcew_avg_weekly_wage_nominal, 0),
        `Weekly Wage (Real, RPP)` = round(qcew_avg_weekly_wage_real_rpp, 0),
        `RPP` = round(rpp_all_items_index, 1),
        `Wage/Productivity Ratio` = round(wage_to_productivity_ratio_real, 4)
      ) %>%
      arrange(desc(`Selected Metric`))
  })

  output$download_gap_map <- downloadHandler(
    filename = function() sprintf("gap_map_%s.csv", input$year),
    content = function(file) {
      df <- yearly_data() %>%
        transmute(
          state_name,
          state_abbr,
          year,
          labor_productivity_index_2007,
          hourly_compensation_index_2007,
          gap_index_2007
        )
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_state_trends <- downloadHandler(
    filename = function() sprintf("state_trend_%s_%s.csv", input$state, input$year),
    content = function(file) {
      df <- panel_data() %>%
        filter(state_name == input$state) %>%
        transmute(
          state_name,
          state_abbr,
          year,
          labor_productivity_index_2007,
          hourly_compensation_index_2007,
          gap_index_2007
        )
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_rankings <- downloadHandler(
    filename = function() sprintf("gap_rankings_%s.csv", input$year),
    content = function(file) {
      df <- yearly_data() %>%
        transmute(
          state_name,
          state_abbr,
          year,
          labor_productivity_index_2007,
          hourly_compensation_index_2007,
          gap_index_2007
        ) %>%
        arrange(desc(gap_index_2007))
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_levels <- downloadHandler(
    filename = function() sprintf("levels_%s_%s.csv", input$levels_metric, input$year),
    content = function(file) {
      metric_col <- input$levels_metric
      df <- levels_yearly_data() %>%
        filter(!is.na(.data[[metric_col]])) %>%
        transmute(
          state_name,
          state_abbr,
          year,
          selected_metric = metric_col,
          selected_metric_value = .data[[metric_col]],
          gdp_per_job_real_2017,
          qcew_avg_weekly_wage_nominal,
          qcew_avg_weekly_wage_real_rpp,
          rpp_all_items_index,
          wage_to_productivity_ratio_real
        )
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
