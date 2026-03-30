# ============================================================
# server_outsideUS_tab.R - Outside US Tab Server Logic
# ============================================================
# Owner: [PERSON - assign name here]
# Handles: line chart and CSV download for Outside US tab.
# Data columns: year, quarter_avr, avr_gdp, compensation, productivity
# ============================================================


outside_us_tab_server <- function(input, output, session,
                                  outside_us_data) {
  
  # ---- Filter data to selected year range ----
  outside_us_filtered <- reactive({
    req(outside_us_data())
    
    outside_us_data() %>%
      filter(
        year >= input$outside_year_range[1],
        year <= input$outside_year_range[2]
      )
  })
  
  
  # ---- Render the line chart ----
  output$outside_us_plot <- renderPlot({
    
    df <- outside_us_filtered()
    
    validate(need(nrow(df) > 0, "No data available for selected year range."))
    
    # Get selected series from checkbox
    selected <- input$outside_series
    validate(need(length(selected) > 0, "Please select at least one series."))
    
    # -- Build a long format data frame with only selected series --
    # Map column names to display labels
    series_labels <- c(
      quarter_avr  = "EU Labor Productivity",
      avr_gdp      = "EU GDP per Capita",
      compensation = "US Compensation",
      productivity = "US Labor Productivity"
    )
    
    # Color blind friendly colors for each series
    series_colors <- c(
      "EU Labor Productivity"   = "#0072B2",  # blue
      "EU GDP per Capita"       = "#E69F00",  # orange
      "US Compensation"         = "#009E73",  # green
      "US Labor Productivity"   = "#CC79A7"   # pink/purple
    )
    
    # Convert wide to long for ggplot
    df_long <- df %>%
      select(year, all_of(selected)) %>%
      tidyr::pivot_longer(
        cols      = -year,
        names_to  = "series",
        values_to = "value"
      ) %>%
      mutate(
        # Replace column name with display label
        series = series_labels[series]
      ) %>%
      filter(!is.na(value))
    
    validate(need(nrow(df_long) > 0, "No data available for selected series."))
    
    # -- Draw the line chart --
    # Use facet_wrap so each series gets its own y-axis scale.
    # This prevents EU GDP per Capita from squashing the others.
    ggplot(df_long, aes(x = year, y = value,
                        color = series, group = series)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      scale_color_manual(values = series_colors) +
      scale_x_continuous(breaks = seq(1995, 2023, by = 4)) +
      facet_wrap(~ series, scales = "free_y") +
      labs(
        title    = "EU Productivity vs. US Compensation Over Time",
        subtitle = paste0(input$outside_year_range[1],
                          " - ", input$outside_year_range[2]),
        x        = "Year",
        y        = "Index Value"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title       = element_text(family = "Georgia",
                                        face   = "bold",
                                        size   = 14),
        axis.text        = element_text(color = "black"),
        axis.title       = element_text(color = "black"),
        legend.position  = "none",
        panel.grid.minor = element_blank(),
        strip.text       = element_text(face = "bold", size = 11)
      )
  })
  
  
  # ---- Render the summary table ----
  output$outside_us_table <- renderTable({
    
    df <- outside_us_filtered()
    
    validate(need(nrow(df) > 0, "No data available."))
    
    # Show only selected series columns plus year
    selected <- input$outside_series
    
    # Map column names to display labels
    series_labels <- c(
      quarter_avr  = "EU Labor Productivity",
      avr_gdp      = "EU GDP per Capita",
      compensation = "US Compensation",
      productivity = "US Labor Productivity"
    )
    
    # Build result table with readable column names
    result <- df %>%
      select(year, all_of(selected)) %>%
      arrange(year)
    
    # Rename columns to display labels
    colnames(result) <- c("Year",
                          series_labels[selected])
    
    result
  })
  
  
  # ---- Download handler ----
  output$download_outside_us <- downloadHandler(
    filename = function() {
      paste0("outside_us_data_",
             input$outside_year_range[1], "_",
             input$outside_year_range[2], ".csv")
    },
    content = function(file) {
      write.csv(outside_us_filtered(), file, row.names = FALSE)
    }
  )
}

