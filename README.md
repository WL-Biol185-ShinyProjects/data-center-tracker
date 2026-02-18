# Productivity vs Wages Atlas (BIOL 185)

State-by-state Shiny atlas for comparing productivity and wages in the U.S.

Live app:
https://jacksonmaroon.shinyapps.io/productivity-vs-wages-atlas/

## Team

- Toland
- Jack
- Cesar
- Thomas

## What This Project Measures

Core question:
How much have productivity and compensation diverged across states over time, and how do state wage levels compare with state productivity levels?

This app has two analysis frames:

1. Growth/Index frame (`2007 = 100`) from the BLS state productivity family.
2. Levels frame (`GDP per job`, `weekly wages`, `real wage ratio`) from BEA + QCEW + BEA RPP.

## Data Provenance (Explicit)

All numeric values come from public government datasets. The ETL scripts are deterministic transformations of those sources.

| Metric family | Upstream source | Pull path in this repo | Years in current app data | Geography |
|---|---|---|---|---|
| Labor productivity, hourly compensation, unit labor cost | BLS Productivity by State (series discovered and downloaded via FRED endpoints) | `data-raw/run_all.R` using helpers in `R/helpers.R` | 2007-2024 | 50 states + DC |
| State GDP and state employment | BEA Regional `SASUMMARY` zip | `data-raw/run_levels_all.R` | 2008-2024 | 50 states + DC |
| Private-sector wages | BLS QCEW annual singlefile zips | `data-raw/run_levels_all.R` | 2008-2024 | 50 states + DC |
| Regional price parity adjustment | BEA `RPP` zip | `data-raw/run_levels_all.R` | 2008-2023 in source file; app currently serves years with available joined data | 50 states + DC |

Canonical source links:

- BLS Productivity by State: https://www.bls.gov/lpc/state-productivity.htm
- BLS QCEW: https://www.bls.gov/cew/
- BEA GDP by State: https://www.bea.gov/data/gdp/gdp-state
- BEA Employment by State: https://www.bea.gov/data/employment/employment-state
- BEA Regional Price Parities: https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area

## Methods (Step By Step)

### 1) Growth/index panel (`data/state_panel.csv`)

Pipeline:

1. Discover state-level series IDs for:
   - labor productivity
   - hourly compensation
   - unit labor cost
2. Download annual values by series.
3. Rebase each state+metric series to `2007 = 100`.
4. Build gap metric:
   - `gap_index_2007 = labor_productivity_index_2007 - hourly_compensation_index_2007`

Key script:
- `data-raw/run_all.R`

Key output:
- `data/state_panel.csv`

### 2) Levels panel (`data/state_levels_panel.csv`)

Pipeline:

1. Download BEA `SASUMMARY` and `RPP` zip files.
2. Download QCEW annual state files for each year in range.
3. Keep state private all-industry rows from QCEW.
4. Join state/year records and compute:
   - `gdp_per_job_real_2017 = (gdp_real_millions_2017 * 1e6) / employment_jobs_bea`
   - `qcew_avg_weekly_wage_real_rpp = qcew_avg_weekly_wage_nominal / (rpp_all_items_index / 100)`
   - `wage_to_productivity_ratio_real = qcew_avg_weekly_wage_real_rpp / gdp_per_job_real_2017`
5. Build index views for levels metrics (`2008 = 100`).

Key script:
- `data-raw/run_levels_all.R`

Key output:
- `data/state_levels_panel.csv`

## Variable Dictionary (Main App Metrics)

- `labor_productivity_index_2007`: Labor productivity index, rebased to 2007.
- `hourly_compensation_index_2007`: Hourly compensation index, rebased to 2007.
- `unit_labor_cost_index_2007`: Unit labor cost index, rebased to 2007.
- `gap_index_2007`: Productivity index minus compensation index.
- `gdp_per_job_real_2017`: Real GDP per job (2017 dollars).
- `qcew_avg_weekly_wage_nominal`: Nominal weekly wage from QCEW.
- `qcew_avg_weekly_wage_real_rpp`: RPP-adjusted weekly wage.
- `wage_to_productivity_ratio_real`: Real wage divided by real GDP per job.

## Reproducibility

You can reproduce this project in two ways:

### A) Reproduce the deployed app exactly (recommended for grading demos)

This uses committed `data/*.csv` files (same mode as deployment).

1. Install packages:

```r
install.packages(c("shiny", "dplyr", "tidyr", "ggplot2", "maps", "scales", "plotly", "testthat"))
```

2. Run app:

```bash
Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
```

### B) Rebuild data from upstream public sources

This can produce updated values if upstream files are revised.

1. Rebuild growth panel:

```bash
Rscript data-raw/run_all.R
```

2. Rebuild levels panel:

```bash
Rscript data-raw/run_levels_all.R
```

3. Run helper tests:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

4. Quick output sanity check:

```bash
Rscript -e 'p<-read.csv("data/state_panel.csv"); l<-read.csv("data/state_levels_panel.csv"); cat("state_panel:",nrow(p),"rows | years",min(p$year),"-",max(p$year),"| states",length(unique(p$state_abbr)),"\n"); cat("state_levels_panel:",nrow(l),"rows | years",min(l$year),"-",max(l$year),"| states",length(unique(l$state_abbr)),"\n")'
```

Current data snapshot in this repo (as checked on 2026-02-18):

- `state_panel.csv`: 918 rows, years 2007-2024, 51 geographies (50 states + DC)
- `state_levels_panel.csv`: 867 rows, years 2008-2024, 51 geographies (50 states + DC)

## App Use (3 Steps)

1. Pick `Year` and `State` in the sidebar.
2. Explore `Map View`, `State Comparison`, `Gap Rankings`, and `Levels Comparison`.
3. Download the filtered CSV from each tab’s download button.

## Quality and Trust Notes

- No AI-generated synthetic data is used in analysis outputs.
- Data values are sourced from BLS/BEA public datasets and transformed by visible scripts in this repo.
- The app does not fabricate missing years. If a year/metric combination is unavailable, it is not offered in the selector.
- A data-freshness banner in the app reports observed coverage windows by metric family.

## Limitations (Important)

- Growth panel series IDs are discovered by scraping FRED search results; if FRED page structure changes, discovery may need maintenance.
- Source concepts differ across systems (for example, output concept vs covered employment concept), so cross-source comparisons are descriptive, not causal.
- RPP availability and source revisions can change real-wage comparability for the latest years.

## Deliverables

- Deliverables index: `deliverables/README.md`
- Screenshots for slides/report: `deliverables/screenshots/`

## Repository Structure

```text
.
├── app.R
├── R/helpers.R
├── data/
├── data-raw/
├── tests/
└── deliverables/
```
