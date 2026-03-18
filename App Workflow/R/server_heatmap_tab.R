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
  
  # ---- Fill state dropdown on load ----
  observe({
    df <- panel_data()
    req(!is.null(df), nrow(df) > 0)
    
    states <- sort(unique(df$state_name))
    state_choices <- setNames(states, to_title(states))
    
    updateSelectInput(
      session,
      inputId  = "heatmap_state",
      choices  = state_choices,
      selected = states[1]
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
    
    # -- Step 3: Check that a state is selected --
    req(input$heatmap_state)
    
    # -- Step 4: Filter to selected state and year range --
    df_filtered <- df %>%
      filter(
        state_name == input$heatmap_state,
        year >= input$heatmap_year_range[1],
        year <= input$heatmap_year_range[2]
      )
    
    
    
    # -- Step 6: Build the heat map with ggplot + geom_tile --
    ggplot(df_filtered,
           aes(x    = year,
               y    = state_name,
               fill = gap_real_index_2007)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low      = "#0072B2",  # blue - safe for all color blindness types
        mid      = "#F5F5F5",  # light grey - neutral midpoint
        high     = "#E69F00",  # orange - safe for all color blindness types
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