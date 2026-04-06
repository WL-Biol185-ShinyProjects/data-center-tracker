# ============================================================
# server_levels_tab.R - Server Logic for Levels Comparison
# ============================================================
# Owner: [PERSON 4 or PERSON 2]
# Handles: levels map, scatter plot, levels ranking table.
# ============================================================


levels_tab_server <- function(input, output, session,
                              levels_data) {

  selected_levels_metric <- reactive({
    req(!is.null(levels_data()))

    metric_col <- input$levels_metric

    req(!is.null(metric_col), nzchar(metric_col))
    req(metric_col %in% names(levels_metric_labels))
    req(metric_col %in% names(levels_data()))

    metric_col
  })
  
  # ---- Levels: Filtered to Selected Year ----
  levels_yearly_data <- reactive({
    req(levels_data(), input$year)
    levels_data() %>% filter(year == input$year)
  })
  
  
  # ---- Levels: Unavailable Note ----
  output$levels_unavailable_note <- renderUI({
    validate(need(!is.null(levels_data()),
                  "Levels data not found. Run the levels build script."))
    
    if (nrow(levels_yearly_data()) > 0) return(NULL)
    
    tags$div(
      class = "alert alert-warning",
      paste0("No levels data available for year ", input$year, ".")
    )
  })
  
  
  # ---- Levels: Choropleth Map ----
  output$levels_map <- renderPlot({
    validate(need(!is.null(levels_data()),
                  "Levels data not loaded."))
    
    metric_col   <- selected_levels_metric()
    metric_label <- levels_metric_labels[[metric_col]]
    
    # Filter to rows with data for chosen metric
    plot_df <- levels_yearly_data() %>%
      filter(!is.na(.data[[metric_col]])) %>%
      select(state_name, value = all_of(metric_col))
    
    validate(need(nrow(plot_df) > 0,
                  paste("No map data for year", input$year)))
    
    us_map <- load_us_map()
    validate(need(!is.null(us_map),
                  "Install the `maps` package to see the map."))
    
    # Join data to map polygons
    map_df <- us_map %>%
      left_join(plot_df, by = c("region" = "state_name"))
    
    # Pick label format based on metric type
    if (metric_col %in% c("gdp_per_job_real_2017",
                          "qcew_avg_weekly_wage_real_rpp")) {
      value_labels <- label_dollar(accuracy = 1)
    } else if (metric_col == "wage_to_productivity_ratio_real") {
      value_labels <- label_percent(accuracy = 0.1)
    } else {
      value_labels <- label_number(accuracy = 0.001)
    }
    
    # Draw the levels map
    ggplot(map_df, aes(long, lat, group = group, fill = value)) +
      geom_polygon(color = "white", linewidth = 0.15) +
      coord_fixed(1.3) +
      scale_fill_viridis_c(
        option   = "C",
        na.value = "grey90",
        labels   = value_labels,
        name     = metric_label
      ) +
      labs(
        title    = paste0("Levels Map (", input$year, ")"),
        subtitle = metric_label,
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  
  # ---- Levels: Scatter Data ----
  # Filters to rows with both GDP and wage values
  # Note: 2024 is excluded because RPP data is not yet available,
  # so real wage cannot be calculated - scatter defaults to 2023
  levels_scatter_data <- reactive({
    req(levels_yearly_data())
    
    df <- levels_yearly_data() %>%
      filter(!is.na(gdp_per_job_real_2017),
             !is.na(qcew_avg_weekly_wage_real_rpp))
    
    # If no rows (e.g. 2024 selected), fall back to most recent valid year
    if (nrow(df) == 0) {
      df <- levels_data() %>%
        filter(!is.na(gdp_per_job_real_2017),
               !is.na(qcew_avg_weekly_wage_real_rpp)) %>%
        filter(year == max(year, na.rm = TRUE))
    }
    
    df
  })
  
  
  # ---- Levels: Scatter Plot UI ----
  # Use the static renderer consistently across environments.
  # The plotly conversion has been error-prone on some Posit Workbench setups.
  output$levels_scatter_ui <- renderUI({
    plotOutput("levels_scatter_static", height = "420px")
  })
  
  
  # ---- Levels: Plotly Scatter ----
  # Interactive scatter: GDP per job (x) vs weekly wage (y), one dot per state
  if (FALSE && requireNamespace("plotly", quietly = TRUE)) {
    output$levels_scatter <- plotly::renderPlotly({
      
      df <- as.data.frame(levels_scatter_data())
      
      validate(need(nrow(df) > 0,
                    paste("No levels data for year", input$year)))
      
      # Build hover text using $ notation to avoid aes() scoping issues
      df$hover_text <- paste0(
        "State: ", to_title(df$state_name), " (", df$state_abbr, ")",
        "<br>GDP/job: ",     label_dollar(accuracy = 1)(df$gdp_per_job_real_2017),
        "<br>Weekly wage: ", label_dollar(accuracy = 1)(df$qcew_avg_weekly_wage_real_rpp)
      )
      
      gg <- ggplot(df, aes(
        x     = gdp_per_job_real_2017,
        y     = qcew_avg_weekly_wage_real_rpp,
        text  = hover_text,
        label = state_abbr
      )) +
        geom_point(color = "#0072B2", alpha = 0.8, size = 2.6) +
        geom_text(size = 2.6, check_overlap = TRUE,
                  vjust = -0.6, color = "#444444") +
        geom_smooth(method = "lm", se = FALSE,
                    formula = y ~ x,
                    color = "#E69F00", linewidth = 0.9) +
        scale_x_continuous(labels = label_dollar()) +
        scale_y_continuous(labels = label_dollar()) +
        labs(
          title    = paste0("State Levels Comparison (", input$year, ")"),
          subtitle = "x: Real GDP per job | y: Real weekly wage (RPP-adj)",
          x = "Real GDP per job (2017 dollars)",
          y = "Real weekly wage (RPP-adjusted)"
        ) +
        theme_minimal(base_size = 12)
      plotly::ggplotly(gg, tooltip = "text") |>
        plotly::config(displayModeBar = FALSE)
    })
  }
  
  
  # ---- Levels: Static Scatter Fallback ----
  # Only used if plotly is not installed
  output$levels_scatter_static <- renderPlot({
    
    df <- as.data.frame(levels_scatter_data())
    
    validate(need(nrow(df) > 0,
                  paste("No levels data for year", input$year)))
    
    ggplot(df, aes(x = gdp_per_job_real_2017,
                   y = qcew_avg_weekly_wage_real_rpp,
                   label = state_abbr)) +
      geom_point(color = "#0072B2", alpha = 0.8, size = 2.6) +
      geom_text(size = 2.6, check_overlap = TRUE,
                vjust = -0.6, color = "#444444") +
      geom_smooth(method = "lm", se = FALSE,
                  formula = y ~ x,
                  color = "#E69F00", linewidth = 0.9) +
      scale_x_continuous(labels = label_dollar()) +
      scale_y_continuous(labels = label_dollar()) +
      labs(
        title    = paste0("State Levels Comparison (", input$year, ")"),
        subtitle = "x: Real GDP per job | y: Real weekly wage (RPP-adj)",
        x = "Real GDP per job (2017 dollars)",
        y = "Real weekly wage (RPP-adjusted)"
      ) +
      theme_minimal(base_size = 12)
  })
  
  
  # ---- Levels: Ranking Table ----
  # FIX: pull metric column as plain vector using df[[]] instead of
  # .data[[]] inside transmute, which caused "subscript out of bounds"
  output$levels_rank_table <- renderTable({
    validate(need(!is.null(levels_data()),
                  "Levels data not loaded."))
    
    metric_col       <- selected_levels_metric()
    metric_is_dollar <- metric_col %in% c("gdp_per_job_real_2017",
                                          "qcew_avg_weekly_wage_real_rpp")
    
    # Convert to plain data frame first to allow safe df[[col]] indexing
    df <- as.data.frame(levels_yearly_data() %>%
                          filter(!is.na(.data[[metric_col]])))
    
    # Extract and round the selected metric as a plain vector
    metric_values <- if (metric_is_dollar) {
      round(df[[metric_col]], 0)
    } else {
      round(df[[metric_col]], 4)
    }

    safe_round_col <- function(data, col_name, digits) {
      if (!col_name %in% names(data)) {
        return(rep(NA_real_, nrow(data)))
      }
      round(data[[col_name]], digits)
    }
    
    # Build table using $ notation throughout
    result <- data.frame(
      State                    = to_title(df$state_name),
      `Selected Metric`        = metric_values,
      `GDP per Job (Real)`     = safe_round_col(df, "gdp_per_job_real_2017", 0),
      `Weekly Wage (Nominal)`  = safe_round_col(df, "qcew_avg_weekly_wage_nominal", 0),
      `Weekly Wage (Real RPP)` = safe_round_col(df, "qcew_avg_weekly_wage_real_rpp", 0),
      `RPP`                    = safe_round_col(df, "rpp_all_items_index", 1),
      `Comp/Prod Ratio`        = safe_round_col(df, "wage_to_productivity_ratio_real", 4),
      check.names = FALSE
    )
    
    result[order(result[["Selected Metric"]], decreasing = TRUE), ]
  })
  
  
  # ---- Levels: Download ----
  output$download_levels <- downloadHandler(
    filename = function() {
      sprintf("levels_%s_%s.csv", selected_levels_metric(), input$year)
    },
    content = function(file) {
      metric_col <- selected_levels_metric()
      df <- levels_yearly_data() %>%
        filter(!is.na(.data[[metric_col]]))
      write.csv(df, file, row.names = FALSE)
    }
  )
}
