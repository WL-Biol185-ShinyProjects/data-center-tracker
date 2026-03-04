#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = FALSE)
file_flag <- "--file="
script_path <- sub(file_flag, "", args[grep(file_flag, args)][1])
script_dir <- dirname(normalizePath(script_path))
project_root <- normalizePath(file.path(script_dir, ".."))
setwd(project_root)

source(file.path("R", "helpers.R"))

base_year <- 2007

metric_specs <- data.frame(
  metric_key = c("labor_productivity", "hourly_compensation", "unit_labor_cost"),
  series_prefix = c("IPUZNL000", "IPUZNU130", "IPUZNU101"),
  stringsAsFactors = FALSE
)

message("Building deterministic BLS/FRED series IDs by state FIPS...")
state_key <- state_fips_lookup() %>%
  select(state_name, state_abbr, state_fips2)

series_catalog <- tidyr::crossing(
  state_key,
  metric_specs
) %>%
  mutate(series_id = paste0(series_prefix, state_fips2, "0000")) %>%
  select(state_name, state_abbr, series_id, metric_key) %>%
  arrange(metric_key, state_name)

validate_series_catalog <- function(catalog) {
  expected_patterns <- c(
    labor_productivity = "^IPUZNL000[0-9]{6}$",
    hourly_compensation = "^IPUZNU130[0-9]{6}$",
    unit_labor_cost = "^IPUZNU101[0-9]{6}$"
  )

  problems <- character(0)
  for (metric in names(expected_patterns)) {
    metric_rows <- catalog %>% filter(metric_key == metric)
    bad_ids <- metric_rows %>%
      filter(!grepl(expected_patterns[[metric]], series_id))
    dup_states <- metric_rows$state_name[duplicated(metric_rows$state_name)]
    if (nrow(bad_ids) > 0) {
      problems <- c(
        problems,
        sprintf(
          "%s has non-canonical IDs: %s",
          metric,
          paste(unique(bad_ids$series_id), collapse = ", ")
        )
      )
    }
    if (length(dup_states) > 0) {
      problems <- c(
        problems,
        sprintf(
          "%s has duplicate states: %s",
          metric,
          paste(unique(dup_states), collapse = ", ")
        )
      )
    }
  }
  if (length(problems) > 0) {
    stop(
      paste(
        c(
          "Series catalog validation failed.",
          problems
        ),
        collapse = "\n- "
      )
    )
  }
}

validate_series_catalog(series_catalog)

expected_states <- nrow(state_lookup())
coverage <- series_catalog %>%
  count(metric_key, name = "n_states")
message("Coverage by metric:")
for (i in seq_len(nrow(coverage))) {
  message(sprintf("  %s: %s states", coverage$metric_key[i], coverage$n_states[i]))
}
if (any(coverage$n_states < expected_states)) {
  warning(
    sprintf(
      "Some metrics have fewer than %s state/DC series. Search pages may have changed.",
      expected_states
    )
  )
}

message("Downloading time series data...")
series_rows <- lapply(seq_len(nrow(series_catalog)), function(i) {
  row <- series_catalog[i, ]
  df <- tryCatch(
    fetch_fred_series(row$series_id),
    error = function(e) {
      stop(
        sprintf(
          "Failed to fetch series %s (%s, %s): %s",
          row$series_id,
          row$metric_key,
          row$state_abbr,
          conditionMessage(e)
        )
      )
    }
  )
  if (nrow(df) == 0) {
    stop(
      sprintf(
        "Series %s (%s, %s) returned no rows.",
        row$series_id,
        row$metric_key,
        row$state_abbr
      )
    )
  }
  df$state_name <- row$state_name
  df$state_abbr <- row$state_abbr
  df$metric_key <- row$metric_key
  df$series_id <- row$series_id
  df
})

state_metrics_long <- bind_rows(series_rows) %>%
  arrange(state_name, metric_key, year) %>%
  group_by(state_name, metric_key) %>%
  mutate(value_index_2007 = rebase_values(value, year, base_year = base_year)) %>%
  ungroup() %>%
  mutate(
    metric_label = case_when(
      metric_key == "labor_productivity" ~ "Labor Productivity",
      metric_key == "hourly_compensation" ~ "Hourly Compensation",
      metric_key == "unit_labor_cost" ~ "Unit Labor Cost",
      TRUE ~ metric_key
    )
  )

missing_base_comp <- state_metrics_long %>%
  filter(metric_key == "hourly_compensation", year == base_year) %>%
  filter(is.na(value)) %>%
  pull(state_abbr)
if (length(missing_base_comp) > 0) {
  stop(
    sprintf(
      "Base-year hourly compensation is missing for: %s",
      paste(unique(missing_base_comp), collapse = ", ")
    )
  )
}

message("Downloading national GDP deflator (A191RD3A086NBEA)...")
gdp_deflator <- fetch_fred_series("A191RD3A086NBEA") %>%
  transmute(year, gdp_deflator_index_2017 = value)

state_panel_raw <- state_metrics_long %>%
  select(state_name, state_abbr, year, metric_key, value) %>%
  pivot_wider(names_from = metric_key, values_from = value) %>%
  left_join(gdp_deflator, by = "year") %>%
  mutate(
    hourly_compensation_real_2017 = hourly_compensation / (gdp_deflator_index_2017 / 100)
  )

state_panel_index <- state_metrics_long %>%
  select(state_name, state_abbr, year, metric_key, value_index_2007) %>%
  pivot_wider(
    names_from = metric_key,
    values_from = value_index_2007,
    names_glue = "{metric_key}_index_2007"
  )

state_panel_real_index <- state_panel_raw %>%
  group_by(state_name, state_abbr) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    hourly_compensation_real_index_2007 = rebase_values(
      hourly_compensation_real_2017,
      year,
      base_year = base_year
    )
  ) %>%
  ungroup() %>%
  select(state_name, state_abbr, year, hourly_compensation_real_index_2007)

state_panel <- state_panel_raw %>%
  left_join(state_panel_index, by = c("state_name", "state_abbr", "year")) %>%
  left_join(state_panel_real_index, by = c("state_name", "state_abbr", "year")) %>%
  mutate(
    gap_nominal_index_2007 = labor_productivity_index_2007 - hourly_compensation_index_2007,
    gap_real_index_2007 = labor_productivity_index_2007 - hourly_compensation_real_index_2007,
    # Keep legacy column name, but default it to real-gap for safer interpretation.
    gap_index_2007 = gap_real_index_2007
  ) %>%
  arrange(year, state_name)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(series_catalog, file.path("data", "series_catalog.csv"), row.names = FALSE)
utils::write.csv(state_metrics_long, file.path("data", "state_metrics_long.csv"), row.names = FALSE)
utils::write.csv(state_panel, file.path("data", "state_panel.csv"), row.names = FALSE)

message("Done.")
message(sprintf("Series discovered: %s", nrow(series_catalog)))
message(sprintf("Rows written to data/state_metrics_long.csv: %s", nrow(state_metrics_long)))
message(sprintf("Rows written to data/state_panel.csv: %s", nrow(state_panel)))
