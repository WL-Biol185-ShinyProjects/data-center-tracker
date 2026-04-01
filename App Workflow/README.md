# Productivity vs Wages Atlas (BIOL 185)

State-by-state Shiny atlas for comparing productivity and wages in the U.S.

Live app:
https://jacksonmaroon.shinyapps.io/productivity-vs-wages-atlas/

## Team

- Nicolas
- Jack
- Cesar
- Thomas

## AI  Acknowledgment

This project used Claude (Anthropic) as an AI assistant during development.


## What This Project Measures

Core question:
How much have productivity and compensation diverged across states over time, and how do state wage levels compare with state productivity levels?

This app has two analysis frames:

1. Growth/Index frame (`2007 = 100`) from the BLS state productivity family.
2. Levels frame (`GDP per job`, `weekly wages`, `real wage ratio`) from BEA + QCEW + BEA RPP.

## Data Provenance (Explicit)

All numeric values come from public government datasets. The ETL scripts are deterministic transformations of those sources.

| Metric family | Exact dataset endpoints used | Pull path in this repo | Pull date used for current committed snapshot | Years in current app data | Geography |
|---|---|---|---|---|---|
| Labor productivity, hourly compensation, unit labor cost + GDP deflator | FRED CSV API endpoints used directly in ETL: `https://fred.stlouisfed.org/graph/fredgraph.csv?id=<SERIES_ID>` and GDP deflator `https://fred.stlouisfed.org/graph/fredgraph.csv?id=A191RD3A086NBEA` | `data-raw/run_all.R` using helpers in `R/helpers.R` | 2026-03-04 (fresh pull during `run_all.R`) | 2007-2024 | 50 states + DC |
| State GDP and state employment | BEA regional zip: `https://apps.bea.gov/regional/zip/SASUMMARY.zip`, member file `SASUMMARY__ALL_AREAS_1998_2024.csv`, line codes used: `1` (real GDP), `15` (total employment jobs) | `data-raw/run_levels_all.R` | 2026-02-18 (cached input used in current build) | 2008-2024 | 50 states + DC |
| Private-sector wages | BLS QCEW annual singlefile zips: `https://data.bls.gov/cew/data/files/<YEAR>/csv/<YEAR>_annual_singlefile.zip` for years 2008-2024; row filter applied in ETL: state-level `area_fips` ending `000`, `own_code=5` (private), `industry_code=10` (all industries) | `data-raw/run_levels_all.R` | 2026-02-18 (cached input used in current build) | 2008-2024 | 50 states + DC |
| Regional price parity adjustment | BEA regional zip: `https://apps.bea.gov/regional/zip/RPP.zip`, member file `SARPP_STATE_2008_2023.csv`, line code used: `5` (all items RPP) | `data-raw/run_levels_all.R` | 2026-02-18 (cached input used in current build) | 2008-2023 in source file; app currently serves years with available joined data | 50 states + DC |

Canonical source links:

- FRED CSV endpoint (exact transport used by ETL): https://fred.stlouisfed.org/graph/fredgraph.csv?id=IPUZNL000010000
- Full list of all state FRED series IDs used in this snapshot: `data/series_catalog.csv`
- FRED GDP deflator series used for real-growth transform: https://fred.stlouisfed.org/graph/fredgraph.csv?id=A191RD3A086NBEA
- BEA SASUMMARY zip used in ETL: https://apps.bea.gov/regional/zip/SASUMMARY.zip
- BEA RPP zip used in ETL: https://apps.bea.gov/regional/zip/RPP.zip
- BLS QCEW annual singlefile zip (first year used): https://data.bls.gov/cew/data/files/2008/csv/2008_annual_singlefile.zip
- BLS QCEW annual singlefile zip (latest year used): https://data.bls.gov/cew/data/files/2024/csv/2024_annual_singlefile.zip
- EU Worker Productivity European Central Bank (Accessed March 18 2026): https://data.ecb.europa.eu/data/datasets/MNA/MNA.Q.Y.I9.W0.S1.S1._Z.LPR_HW._Z.OTQ._Z.IX.LR.N
- EU GDP Data World Bank Data (Accessed March 18 2026): https://data.worldbank.org/indicator/NY.GDP.PCAP.CD

## Methods (Step By Step)

### 1) Growth/index panel (`data/state_panel.csv`)

Pipeline:

1. Build deterministic FRED series IDs from state FIPS for:
   - labor productivity (`IPUZNL000[STATE_FIPS]0000`)
   - hourly compensation (`IPUZNU130[STATE_FIPS]0000`)
   - unit labor cost (`IPUZNU101[STATE_FIPS]0000`)
2. Validate IDs against canonical patterns and fail fast on malformed mappings.
3. Download annual values by series.
4. Rebase each state+metric series to `2007 = 100`.
5. Build nominal and real gap metrics:
   - `gap_nominal_index_2007 = labor_productivity_index_2007 - hourly_compensation_index_2007`
   - `gap_real_index_2007 = labor_productivity_index_2007 - hourly_compensation_real_index_2007`
   - `gap_index_2007` is kept as the app-facing default alias and currently equals `gap_real_index_2007`.

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
   - `qcew_avg_annual_pay_real_rpp = qcew_avg_annual_pay_nominal / (rpp_all_items_index / 100)`
   - `wage_to_productivity_ratio_real = qcew_avg_annual_pay_real_rpp / gdp_per_job_real_2017`
5. Build index views for levels metrics (`2008 = 100`).

Key script:
- `data-raw/run_levels_all.R`

Key output:
- `data/state_levels_panel.csv`

Additional deterministic outputs written by ETL:

- `data/state_metrics_long.csv` (growth long-form series by metric/state/year)
- `data/series_catalog.csv` (all deterministic FRED series IDs used)
- `data/qcew_state_private_wages.csv` (filtered QCEW private all-industry state rows)
- `data/state_levels_long.csv` (levels long-form series by metric/state/year)

## Variable Dictionary (Main App Metrics)

- `labor_productivity_index_2007`: Labor productivity index, rebased to 2007.
- `hourly_compensation_index_2007`: Hourly compensation index, rebased to 2007.
- `hourly_compensation_real_index_2007`: Real hourly compensation index (GDP-deflator adjusted), rebased to 2007.
- `unit_labor_cost_index_2007`: Unit labor cost index, rebased to 2007.
- `gap_nominal_index_2007`: Productivity index minus nominal compensation index.
- `gap_real_index_2007`: Productivity index minus real compensation index.
- `gap_index_2007`: App default gap alias (currently equal to `gap_real_index_2007`).
- `gdp_per_job_real_2017`: Real GDP per job (2017 dollars).
- `qcew_avg_weekly_wage_nominal`: Nominal weekly wage from QCEW.
- `qcew_avg_weekly_wage_real_rpp`: RPP-adjusted weekly wage.
- `qcew_avg_annual_pay_nominal`: Nominal annual pay from QCEW.
- `qcew_avg_annual_pay_real_rpp`: RPP-adjusted annual pay from QCEW.
- `wage_to_productivity_ratio_real`: Annual real pay divided by real GDP per job.

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

Current data snapshot in this repo (as checked on 2026-03-04):

- `state_panel.csv`: 918 rows, years 2007-2024, 51 geographies (50 states + DC)
- `state_levels_panel.csv`: 867 rows, years 2008-2024, 51 geographies (50 states + DC)
- `state_metrics_long.csv`: 2,703 rows, years 2007-2024, 51 geographies x 3 growth metrics (not a full rectangular grid because `unit_labor_cost` is unavailable for 2007 in all geographies)
- `series_catalog.csv`: 153 rows, 51 geographies x 3 deterministic FRED metric IDs
- `qcew_state_private_wages.csv`: 867 rows, years 2008-2024, filtered state private all-industry wages
- `state_levels_long.csv`: 4,335 rows, years 2008-2024, 5 levels metrics in long format
- Snapshot build note:
  - Growth panel (`run_all.R`) was pulled/refreshed on 2026-03-04.
  - Levels panel (`run_levels_all.R`) was rebuilt on 2026-03-04 using cached upstream zip inputs pulled on 2026-02-18.
  - Levels 2024 caveat (current snapshot): `wage_to_productivity_ratio_real` is `NA` for all 51 geographies because RPP source coverage currently ends in 2023.
  - Levels-tab year control currently resolves to years with complete levels scatter inputs (`gdp_per_job_real_2017` and `qcew_avg_weekly_wage_real_rpp`), which is 2008-2023 for this snapshot.

## Using Posit Workbench (WLU)

These steps are for teammates using:
https://positworkbench.wlu.edu

### First time setup (one-time)

1. Log in to Posit Workbench and open an RStudio session.
2. Open the `Terminal` tab in RStudio.
3. Clone the project and move into it:

```bash
git clone https://github.com/WL-Biol185-ShinyProjects/data-center-tracker.git
cd data-center-tracker
```

4. Install required R packages once:

```bash
Rscript -e 'install.packages(c("shiny","dplyr","tidyr","ggplot2","maps","scales","plotly","testthat"), repos="https://cloud.r-project.org")'
```

### Daily workflow (each time you work)

1. Open `Terminal` in RStudio.
2. Go to the project folder and pull latest changes:

```bash
cd ~/data-center-tracker
git pull origin main
```

3. Start the app:

```bash
Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
```

4. Open the app from the RStudio `Viewer` pane (or the URL shown in console).
5. Stop the app when done with `Ctrl + C` in the terminal.

### If something does not run

- If you see `there is no package called ...`, run the one-time package install command again.
- If `git pull` says there are local changes you do not want, ask a teammate before continuing.
- If local run is blocked, use the deployed app directly:
  https://jacksonmaroon.shinyapps.io/productivity-vs-wages-atlas/

## App Use (3 Steps)

1. Pick controls relevant to your current tab (`Map View`: `Year` + `Growth Wage Basis`; `Gap Rankings`: `Year` + `Growth Wage Basis`; `State Comparison`: `State` + `Growth Wage Basis`; `Levels Comparison`: `Year` + `Levels Map Metric`).
2. Explore `Map View`, `State Comparison`, `Gap Rankings`, and `Levels Comparison`.
3. Download the filtered CSV from each tab’s download button.

## Limitations (Important)

- Growth panel uses deterministic state-FIPS series IDs plus fail-fast validation to avoid silent series mis-mapping.
- In this snapshot, `unit_labor_cost` is missing for 2007 in all 51 geographies, so `state_metrics_long.csv` is not a perfect 51 x 3 x 18 grid.
- Source concepts differ across systems (for example, BEA total-economy GDP/jobs vs QCEW private-sector wages), so cross-source comparisons are descriptive, not causal.
- Deflation concepts differ in levels views (RPP for wages vs chained 2017 dollars for GDP/job), so levels ratios should not be interpreted as structural labor-share estimates.
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
