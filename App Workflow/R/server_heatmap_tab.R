# ============================================================
# server_heatmap_tab.R - Heat Map Tab Server Logic
# ============================================================
# Owner: [YOUR NAME]
# This file contains the server code for the Heat Map tab.
# It filters data by year, sorts states, and draws geom_tile.
# ============================================================

# ---- Heat Map Server Function ----
heatmap_tab_server <- function(input, output, session,
                               panel_data) {
  
  # ---- Render the heat map ----
  output$heatmap_plot <- renderPlot({
    
    # -- Step 1: Get the data --
    df <- panel_data()
    
    # -- Step 2: Check that data loaded --
    if (is.null(df)) {
      return(NULL)
    }
    
    # -- Step 3: Filter to selected year range --
    df_filtered <- df %>%
      filter(
        year >= input$heatmap_year_range[1],
        year <= input$heatmap_year_range[2]
      )
    
    # -- Step 4: Figure out sort order for states --
    # Get the latest year in the filtered data
    latest_year <- max(df_filtered$year, na.rm = TRUE)
    
    # Get gap values for the latest year only
    latest_gaps <- df_filtered %>%
      filter(year == latest_year) %>%
      select(state_name, gap_real_index_2007)
    
    # Sort based on user choice
    if (input$heatmap_sort == "gap_desc") {
      
      # Largest gap on top
      state_order <- latest_gaps %>%
        arrange(gap_real_index_2007) %>%
        pull(state_name)
      
    } else if (input$heatmap_sort == "gap_asc") {
      
      # Smallest gap on top
      state_order <- latest_gaps %>%
        arrange(desc(gap_real_index_2007)) %>%
        pull(state_name)
      
    } else {
      
      # Alphabetical (reversed so A is on top)
      state_order <- sort(unique(df_filtered$state_name),
                          decreasing = TRUE)
      
    }
    
    # -- Step 5: Set state_name as a factor with our order --
    df_filtered <- df_filtered %>%
      mutate(
        state_name = factor(state_name, levels = state_order)
      )
    
    # -- Step 6: Build the heat map with ggplot + geom_tile --
    ggplot(df_filtered,
           aes(x    = year,
               y    = state_name,
               fill = gap_real_index_2007)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low      = "#2166ac",
        mid      = "#ffffbf",
        high     = "#b2182b",
        midpoint = 0,
        name     = "Gap Index\n(2007 = 0)"
      ) +
      scale_x_continuous(
        breaks = seq(2007, 2023, by = 2)
      ) +
      labs(
        x = "Year",
        y = ""
      ) +
      theme_minimal() +
      theme(
        axis.text.y  = element_text(size = 8),
        axis.text.x  = element_text(size = 10),
        legend.title  = element_text(size = 10),
        legend.text   = element_text(size = 9),
        panel.grid    = element_blank()
      )
    # ---- end ggplot ----
    
  })
  # ---- end renderPlot ----
  
}
# ---- end heatmap_tab_server ----