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

start_year <- 2008L
end_year <- 2024L
base_year <- 2008L

bea_sas_url <- "https://apps.bea.gov/regional/zip/SASUMMARY.zip"
bea_rpp_url <- "https://apps.bea.gov/regional/zip/RPP.zip"

bea_sas_zip <- file.path("data-raw", "cache", "SASUMMARY.zip")
bea_rpp_zip <- file.path("data-raw", "cache", "RPP.zip")

download_if_missing(bea_sas_url, bea_sas_zip)
download_if_missing(bea_rpp_url, bea_rpp_zip)

state_key <- state_fips_lookup() %>%
  mutate(state_area_fips5 = paste0(state_fips2, "000"))

read_bea_long <- function(zip_path, member_name) {
  raw_df <- read.csv(
    unz(zip_path, member_name),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  year_cols <- grep("^[0-9]{4}$", names(raw_df), value = TRUE)
  raw_df %>%
    mutate(
      GeoFIPS = trimws(GeoFIPS),
      GeoName = trimws(GeoName)
    ) %>%
    pivot_longer(cols = all_of(year_cols), names_to = "year", values_to = "value_raw") %>%
    mutate(
      year = as.integer(year),
      value = parse_numeric_safe(value_raw)
    ) %>%
    select(-value_raw)
}

bea_sas_long <- read_bea_long(bea_sas_zip, "SASUMMARY__ALL_AREAS_1998_2024.csv") %>%
  filter(year >= start_year, year <= end_year)

bea_gdp <- bea_sas_long %>%
  filter(LineCode == 1, GeoFIPS %in% state_key$state_area_fips5) %>%
  transmute(
    state_area_fips5 = GeoFIPS,
    year,
    gdp_real_millions_2017 = value
  )

bea_employment <- bea_sas_long %>%
  filter(LineCode == 15, GeoFIPS %in% state_key$state_area_fips5) %>%
  transmute(
    state_area_fips5 = GeoFIPS,
    year,
    employment_jobs_bea = value
  )

bea_rpp_long <- read_bea_long(bea_rpp_zip, "SARPP_STATE_2008_2023.csv") %>%
  filter(LineCode == 5, GeoFIPS %in% state_key$state_area_fips5) %>%
  transmute(
    state_area_fips5 = GeoFIPS,
    year,
    rpp_all_items_index = value
  )

pull_qcew_year <- function(year) {
  zip_path <- file.path("data-raw", "cache", sprintf("%s_annual_singlefile.zip", year))
  zip_url <- sprintf(
    "https://data.bls.gov/cew/data/files/%s/csv/%s_annual_singlefile.zip",
    year,
    year
  )

  download_if_missing(zip_url, zip_path)

  # Keep only private all-industry state rows to avoid loading the full file into memory.
  awk_cmd <- sprintf(
    "unzip -p %s | awk 'NR==1 || $0 ~ /^\"[0-9]{2}000\",\"5\",\"10\",/ {print}'",
    shQuote(zip_path)
  )

  df <- read.csv(
    pipe(awk_cmd),
    stringsAsFactors = FALSE,
    colClasses = c(
      area_fips = "character",
      year = "character",
      annual_avg_wkly_wage = "numeric",
      avg_annual_pay = "numeric"
    )
  )
  df %>%
    transmute(
      state_area_fips5 = sprintf("%05s", gsub("[^0-9]", "", area_fips)),
      year = as.integer(year),
      qcew_avg_weekly_wage_nominal = as.numeric(annual_avg_wkly_wage),
      qcew_avg_annual_pay_nominal = as.numeric(avg_annual_pay)
    ) %>%
    filter(state_area_fips5 %in% state_key$state_area_fips5)
}

message("Downloading/parsing QCEW state annual files...")
qcew_state <- bind_rows(lapply(seq.int(start_year, end_year), pull_qcew_year))

state_levels_panel <- state_key %>%
  select(state_name, state_abbr, state_fips2, state_area_fips5) %>%
  tidyr::crossing(year = seq.int(start_year, end_year)) %>%
  left_join(bea_gdp, by = c("state_area_fips5", "year")) %>%
  left_join(bea_employment, by = c("state_area_fips5", "year")) %>%
  left_join(qcew_state, by = c("state_area_fips5", "year")) %>%
  left_join(bea_rpp_long, by = c("state_area_fips5", "year")) %>%
  mutate(
    gdp_per_job_real_2017 = (gdp_real_millions_2017 * 1e6) / employment_jobs_bea,
    qcew_avg_weekly_wage_real_rpp = qcew_avg_weekly_wage_nominal / (rpp_all_items_index / 100),
    qcew_avg_annual_pay_real_rpp = qcew_avg_annual_pay_nominal / (rpp_all_items_index / 100)
  ) %>%
  group_by(state_name) %>%
  mutate(
    gdp_per_job_real_index_2008 = rebase_values(gdp_per_job_real_2017, year, base_year = base_year),
    qcew_weekly_wage_real_index_2008 = rebase_values(qcew_avg_weekly_wage_real_rpp, year, base_year = base_year),
    qcew_annual_pay_real_index_2008 = rebase_values(qcew_avg_annual_pay_real_rpp, year, base_year = base_year)
  ) %>%
  ungroup() %>%
  mutate(
    wage_to_productivity_ratio_real = qcew_avg_weekly_wage_real_rpp / gdp_per_job_real_2017
  ) %>%
  arrange(year, state_name)

state_levels_long <- state_levels_panel %>%
  select(
    state_name,
    state_abbr,
    state_fips2,
    state_area_fips5,
    year,
    gdp_per_job_real_2017,
    qcew_avg_weekly_wage_nominal,
    qcew_avg_weekly_wage_real_rpp,
    rpp_all_items_index,
    wage_to_productivity_ratio_real
  ) %>%
  pivot_longer(
    cols = c(
      gdp_per_job_real_2017,
      qcew_avg_weekly_wage_nominal,
      qcew_avg_weekly_wage_real_rpp,
      rpp_all_items_index,
      wage_to_productivity_ratio_real
    ),
    names_to = "metric_key",
    values_to = "value"
  )

dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(qcew_state, file.path("data", "qcew_state_private_wages.csv"), row.names = FALSE)
write.csv(state_levels_panel, file.path("data", "state_levels_panel.csv"), row.names = FALSE)
write.csv(state_levels_long, file.path("data", "state_levels_long.csv"), row.names = FALSE)

message("Done.")
message(sprintf("Rows in state_levels_panel.csv: %s", nrow(state_levels_panel)))
message(sprintf("Rows in state_levels_long.csv: %s", nrow(state_levels_long)))
