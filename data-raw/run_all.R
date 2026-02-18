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
  label_prefix = c(
    "Labor Productivity for Private Nonfarm in ",
    "Hourly Compensation for Private Nonfarm in ",
    "Unit Labor Costs for Private Nonfarm in "
  ),
  search_text = c(
    "Labor Productivity for Private Nonfarm in",
    "Hourly Compensation for Private Nonfarm in",
    "Unit Labor Costs for Private Nonfarm in"
  ),
  stringsAsFactors = FALSE
)

message("Discovering state series IDs from FRED...")
catalog_parts <- lapply(seq_len(nrow(metric_specs)), function(i) {
  spec <- metric_specs[i, ]
  links <- discover_metric_series(
    search_text = spec$search_text,
    label_prefix = spec$label_prefix,
    max_pages = 10
  )
  links$metric_key <- spec$metric_key
  links
})

series_catalog <- bind_rows(catalog_parts) %>%
  arrange(metric_key, state_name)

if (nrow(series_catalog) == 0) {
  stop("No series IDs were discovered. Check internet access and search parsing.")
}

lookup <- state_lookup()

# Some states can be skipped by broad search ranking; recover them with targeted searches.
for (i in seq_len(nrow(metric_specs))) {
  spec <- metric_specs[i, ]
  existing_states <- series_catalog %>%
    filter(metric_key == spec$metric_key) %>%
    pull(state_name)
  missing_states <- setdiff(lookup$state_name, existing_states)

  if (length(missing_states) == 0) {
    next
  }

  recovered <- lapply(missing_states, function(st) {
    target_search <- paste(spec$search_text, st)
    search_url <- sprintf(
      "https://fred.stlouisfed.org/searchresults/?st=%s",
      URLencode(target_search, reserved = TRUE)
    )
    html <- download_text(search_url)
    links <- extract_series_links(html, spec$label_prefix)
    links <- links %>% filter(state_name == st)
    if (nrow(links) == 0) {
      return(NULL)
    }
    links$metric_key <- spec$metric_key
    links
  })

  recovered <- bind_rows(recovered)
  if (nrow(recovered) > 0) {
    series_catalog <- bind_rows(series_catalog, recovered)
  }
}

series_catalog <- series_catalog %>%
  distinct(metric_key, state_name, .keep_all = TRUE) %>%
  arrange(metric_key, state_name)

# For labor productivity, prefer the level series (IPUZNL000...) over growth-rate series (IPUZNL001...).
series_catalog <- series_catalog %>%
  mutate(
    series_id = if_else(
      metric_key == "labor_productivity",
      sub("^IPUZNL001", "IPUZNL000", series_id),
      series_id
    )
  )

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
  df <- fetch_fred_series(row$series_id)
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

state_panel_raw <- state_metrics_long %>%
  select(state_name, state_abbr, year, metric_key, value) %>%
  pivot_wider(names_from = metric_key, values_from = value)

state_panel_index <- state_metrics_long %>%
  select(state_name, state_abbr, year, metric_key, value_index_2007) %>%
  pivot_wider(
    names_from = metric_key,
    values_from = value_index_2007,
    names_glue = "{metric_key}_index_2007"
  )

state_panel <- state_panel_raw %>%
  left_join(state_panel_index, by = c("state_name", "state_abbr", "year")) %>%
  mutate(
    gap_index_2007 = labor_productivity_index_2007 - hourly_compensation_index_2007
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
