# ============================================================
# server_growth_tabs.R - Server Logic for Growth Tabs
# ============================================================
# Owner: [PERSON 2 + PERSON 3 coordinate here]
#
# This handles: Map View, State Comparison, Gap Rankings.
# All three share the same data + wage basis toggle,
# so they live together to avoid duplication.
#
# IMPORTANT: This file defines a function that gets called
# inside the main server(). All input$/output$ references
# work because they run inside that server scope.
# ============================================================


growth_tabs_server <- function(input, output, session,
                               panel_data, levels_data) {

  # ---- Fill State Dropdown ----
  observe({
    df <- panel_data()
    req(!is.null(df), nrow(df) > 0)

    states <- sort(unique(df$state_name))
    updateSelectInput(
      session, "state",
      choices  = setNames(states, to_title(states)),
      selected = if ("california" %in% states) "california" else states[1]
    )
  })


  # ---- Fill Year Dropdown ----
  observe({
    growth_df <- panel_data()
    req(!is.null(growth_df), nrow(growth_df) > 0)

    years <- sort(unique(growth_df$year))

    # If on Levels tab, narrow years to what has data
    if (identical(input$main_tab, "Levels Comparison") &&
        !is.null(levels_data()) &&
        !is.null(input$levels_metric)) {

      ldf        <- levels_data()
      metric_col <- input$levels_metric

      if (metric_col %in% names(ldf)) {
        metric_ok  <- !is.na(ldf[[metric_col]])
        scatter_ok <- !is.na(ldf$gdp_per_job_real_2017) &
                      !is.na(ldf$qcew_avg_weekly_wage_real_rpp)
        candidate_years <- sort(unique(ldf$year[metric_ok & scatter_ok]))
        if (length(candidate_years) > 0) {
          years <- candidate_years
        }
      }
    }

    selected_year <- isolate(input$year)
    selected_year <- if (!is.null(selected_year) &&
                         selected_year %in% years) {
      selected_year
    } else {
      max(years)
    }
    updateSelectInput(session, "year",
                      choices = years, selected = selected_year)
  })


  # ---- Growth Metric Config ----
  # Decides which columns to use based on real/nominal toggle
  growth_metric_config <- reactive({
    df <- panel_data()
    req(!is.null(df), nrow(df) > 0)

    has_real <- all(c("hourly_compensation_real_index_2007",
                      "gap_real_index_2007") %in% names(df))
    has_nominal_gap <- "gap_nominal_index_2007" %in% names(df)
    use_real <- identical(input$growth_wage_basis, "real") && has_real

    list(
      mode       = if (use_real) "real" else "nominal",
      comp_col   = if (use_real) "hourly_compensation_real_index_2007"
                   else "hourly_compensation_index_2007",
      gap_col    = if (use_real) "gap_real_index_2007"
                   else if (has_nominal_gap) "gap_nominal_index_2007"
                   else "gap_index_2007",
      comp_label = if (use_real) "Hourly Compensation (Real)"
                   else "Hourly Compensation (Nominal)",
      gap_label  = if (use_real) "Real Gap" else "Nominal Gap"
    )
  })


  # ---- Yearly Data (filtered to selected year) ----
  yearly_data <- reactive({
    req(panel_data(), input$year)
    panel_data() %>% filter(year == input$year)
  })


  # ---- MAP: Render Leaflet Choropleth ----
  output$gap_map <- renderLeaflet({
    req(yearly_data())
    growth_cfg <- growth_metric_config()
    
    # -- Step 1: get this year's data --
    map_data <- yearly_data() %>%
      transmute(
        state_name,
        gap_value = .data[[growth_cfg$gap_col]]
      )
    
    # -- Step 2: join in lat/lon from our lookup --
    map_data <- map_data %>%
      left_join(state_coords, by = "state_name") %>%
      filter(!is.na(lat), !is.na(lon))
    
    # -- Step 3: build a color palette --
    # red = negative gap, white = zero, blue = positive
    color_range <- range(map_data$gap_value, na.rm = TRUE)
    pal <- colorNumeric(
      palette  = c("#B2182B", "#F7F7F7", "#2166AC"),
      domain   = color_range
    )
    
    # -- Step 4: build the popup text for each state --
    map_data <- map_data %>%
      mutate(
        popup_text = paste0(
          "<b>", to_title(state_name), "</b><br>",
          growth_cfg$gap_label, ": ",
          round(gap_value, 1), " pts"
        )
      )
    
    # -- Step 5: draw the leaflet map --
    # Uses CartoDB.Positron for a clean light background
    # (shown in the class lesson as a nice option)
    leaflet(data = map_data) %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng     = ~lon,
        lat     = ~lat,
        radius  = ~abs(gap_value) * 0.8,
        color   = ~pal(gap_value),
        fillOpacity = 0.7,
        stroke  = TRUE,
        weight  = 1,
        popup   = ~popup_text,
        label   = ~to_title(state_name)
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~gap_value,
        title    = "Gap (Index pts)",
        opacity  = 0.8
      )
  })


  # ---- COMPARISON: Render Line Chart ----
  output$state_trends <- renderPlot({
    req(panel_data(), input$state)
    growth_cfg <- growth_metric_config()

    # Filter to selected state and reshape for plotting
    df <- panel_data() %>%
      filter(state_name == input$state) %>%
      arrange(year) %>%
      select(
        year,
        labor_productivity_index_2007,
        compensation_index = all_of(growth_cfg$comp_col)
      ) %>%
      pivot_longer(
        cols      = -year,
        names_to  = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric = recode(
          metric,
          labor_productivity_index_2007 = "Labor Productivity",
          compensation_index            = growth_cfg$comp_label
        )
      )

    # Draw the line chart
    ggplot(df, aes(x = year, y = value, color = metric)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      scale_color_manual(
        values = stats::setNames(
          c("#2166AC", "#B2182B"),
          c("Labor Productivity", growth_cfg$comp_label)
        )
      ) +
      labs(
        title    = paste("State Trend:", to_title(input$state)),
        subtitle = "Both series rebased to 2007 = 100",
        x     = NULL,
        y     = "Index (2007 = 100)",
        color = NULL
      ) +
      theme_minimal(base_size = 12)
  })


  # ---- RANKINGS: Render Table ----
  output$rank_table <- renderTable({
    req(yearly_data())
    growth_cfg <- growth_metric_config()

    out <- yearly_data() %>%
      transmute(
        State                    = to_title(state_name),
        `Productivity (2007=100)` = round(labor_productivity_index_2007, 1),
        Compensation             = round(.data[[growth_cfg$comp_col]], 1),
        Gap                      = round(.data[[growth_cfg$gap_col]], 1)
      ) %>%
      arrange(desc(Gap))

    # Rename compensation column to include real/nominal label
    names(out)[3] <- paste0(growth_cfg$comp_label, " (2007=100)")
    out
  })


  # ---- Download Handlers ----
  output$download_gap_map <- downloadHandler(
    filename = function() {
      sprintf("gap_map_%s_%s.csv", input$growth_wage_basis, input$year)
    },
    content = function(file) {
      write.csv(yearly_data(), file, row.names = FALSE)
    }
  )

  output$download_state_trends <- downloadHandler(
    filename = function() {
      sprintf("state_trend_%s_%s.csv", input$state, input$growth_wage_basis)
    },
    content = function(file) {
      df <- panel_data() %>%
        filter(state_name == input$state)
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_rankings <- downloadHandler(
    filename = function() {
      sprintf("gap_rankings_%s_%s.csv", input$growth_wage_basis, input$year)
    },
    content = function(file) {
      write.csv(yearly_data(), file, row.names = FALSE)
    }
  )


  # ---- Return shared reactives for Levels tab to use ----
  return(list(
    growth_metric_config = growth_metric_config,
    yearly_data          = yearly_data
  ))
}
