# ============================================================
# server_heatmap_tab.R - Heat Map Tab Server Logic
# ============================================================
# UPDATED: Shows all 50 states by default when no state
# is selected. Fixes lowercase state names with to_title().
# Sorts states by gap size so patterns are visible.
# ============================================================

# ---- Heat Map Server Function ----
heatmap_tab_server <- function(input, output, session,
                               panel_data) {
  
  # ---- Fill state dropdown on load ----
  observe({
    df <- panel_data()
    req(!is.null(df), nrow(df) > 0)
    
    states <- sort(unique(df$state_name))
    state_choices <- setNames(states, to_title(states))
    
    # Use updateSelectizeInput for multiple = TRUE ----
    updateSelectizeInput(
      session,
      inputId  = "heatmap_state",
      choices  = state_choices,
      selected = NULL
    )
  })
  
  
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
    
    # -- Step 4: If user picked specific states, filter --
    #    If blank (NULL or length 0), show ALL states --
    if (!is.null(input$heatmap_state) &&
        length(input$heatmap_state) > 0) {
      
      df_filtered <- df_filtered %>%
        filter(state_name %in% input$heatmap_state)
    }
    
    # -- Step 5: Make sure we have rows to plot --
    req(nrow(df_filtered) > 0)
    
    # -- Step 6: Add display name and sort by gap --
    df_filtered <- df_filtered %>%
      mutate(
        display_name = to_title(state_name)
      )
    
    # -- Step 7: Build the heat map with ggplot + geom_tile --
    ggplot(df_filtered,
           aes(x    = year,
               y    = reorder(display_name, gap_real_index_2007),
               fill = gap_real_index_2007)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low      = "#0072B2",
        mid      = "#F5F5F5",
        high     = "#E69F00",
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
        axis.text.y   = element_text(size = 7, color = "black"),
        axis.text.x   = element_text(size = 11, color = "black"),
        legend.title   = element_text(size = 11, color = "black"),
        legend.text    = element_text(size = 10, color = "black"),
        panel.grid     = element_blank()
      )
    # ---- end ggplot ----
    
  })
  # ---- end renderPlot ----
  
}
# ---- end heatmap_tab_server ----