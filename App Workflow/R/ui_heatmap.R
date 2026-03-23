# ============================================================
# ui_heatmap.R - Heat Map Tab UI
# ============================================================
# UPDATED: Now supports selecting multiple states, or
# leaving blank to show all 50 states at once.
# ============================================================

# ---- Heat Map Tab ----
heatmap_tab <- tabPanel(
  title = "Heat Map",
  
  # ---- Tab heading ----
  h3("Productivity-Wage Gap Heat Map"),
  p("Each tile shows the gap between productivity growth
     and wage growth for a state in a given year.
     Darker orange = larger gap. Values are indexed to 2007 = 100."),
  
  # ---- Sidebar layout: controls on left, plot on right ----
  sidebarLayout(
    
    # ---- Sidebar: user controls ----
    sidebarPanel(
      width = 3,
      
      # -- State selector - pick multiple or leave blank for all --
      selectizeInput(
        inputId  = "heatmap_state",
        label    = "Select State(s):",
        choices  = NULL,
        multiple = TRUE,
        options  = list(
          placeholder = "Leave blank for all states..."
        )
      ),
      
      # -- Helper text --
      helpText("Leave blank to show all 50 states."),
      
      # -- Year range slider --
      sliderInput(
        inputId = "heatmap_year_range",
        label   = "Select Year Range:",
        min     = 2007,
        max     = 2023,
        value   = c(2007, 2023),
        step    = 1,
        sep     = ""
      )
      
    ),
    # ---- end sidebarPanel ----
    
    # ---- Main panel: the heat map plot ----
    mainPanel(
      width = 9,
      plotOutput(
        outputId = "heatmap_plot",
        height   = "900px"
      )
    )
    # ---- end mainPanel ----
    
  )
  # ---- end sidebarLayout ----
  
)
# ---- end tabPanel ----