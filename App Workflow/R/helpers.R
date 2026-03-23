# ============================================================
# helpers.R - Shared Data Loading & Utility Functions
# ============================================================
# Owner: [DATA PERSON]
# Everyone sources this. Only the data person edits it.
# ============================================================


# ---- Load Growth Panel Data ----
load_panel_data <- function() {
  data_path <- file.path("data", "state_panel.csv")
  if (!file.exists(data_path)) {
    return(NULL)
  }
  read.csv(data_path, stringsAsFactors = FALSE)
}


# ---- Load Levels Data ----
load_levels_data <- function() {
  data_path <- file.path("data", "state_levels_panel.csv")
  if (!file.exists(data_path)) {
    return(NULL)
  }
  df <- read.csv(data_path, stringsAsFactors = FALSE)

  # Compute ratio: annual real pay / real GDP per job
  if (all(c("qcew_avg_annual_pay_real_rpp",
            "gdp_per_job_real_2017") %in% names(df))) {
    df <- df %>%
      mutate(
        wage_to_productivity_ratio_real = ifelse(
          is.na(qcew_avg_annual_pay_real_rpp) |
            is.na(gdp_per_job_real_2017) |
            gdp_per_job_real_2017 <= 0,
          NA_real_,
          qcew_avg_annual_pay_real_rpp / gdp_per_job_real_2017
        )
      )
  }

  df
}


# ---- Helper: Title Case ----
to_title <- function(x) {
  tools::toTitleCase(x)
}


# ---- Helper: Year Range Label ----
range_label <- function(values) {
  years <- sort(unique(values[!is.na(values)]))
  if (length(years) == 0) return("No data")
  if (length(years) == 1) return(as.character(years))
  paste0(min(years), "-", max(years))
}


# ---- Helper: Load US Map ----
load_us_map <- function() {
  if (!requireNamespace("maps", quietly = TRUE)) {
    return(NULL)
  }
  ggplot2::map_data("state")
}

# ---- State Coordinates Lookup ----
# Built-in R data: gives lat/lon for each state center
state_coords <- data.frame(
  state_name = tolower(state.name),
  lat        = state.center$y,
  lon        = state.center$x,
  stringsAsFactors = FALSE
)

# ---- Levels Metric Labels (used by Levels tab) ----
levels_metric_labels <- c(
  gdp_per_job_real_2017            = "GDP per Job (Real, 2017 dollars)",
  qcew_avg_weekly_wage_real_rpp    = "Weekly Wage (Real, RPP-adjusted)",
  wage_to_productivity_ratio_real  = "Compensation-to-Productivity Ratio"
)

# NEW (fixed) - input$levels_metric correctly returns the column name
levels_metric_choices <- stats::setNames(
  names(levels_metric_labels),   # VALUES = column names (what gets returned)
  unname(levels_metric_labels)   # NAMES  = display labels (what user sees)
)
# ---- Load Outside US Data ----  #new EU Data
load_outside_us_data <- function() {
  data_path <- file.path("data", "EUdata.csv")
  if (!file.exists(data_path)) {
    return(NULL)
  }
  read.csv(data_path, stringsAsFactors = FALSE)
}
