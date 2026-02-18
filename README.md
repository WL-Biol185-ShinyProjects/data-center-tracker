# Productivity vs Wages Atlas (BIOL 185)

This repository is now scoped to our BIOL 185 class project on state-level wage and productivity decoupling in the United States.

## Team

- Toland
- Jack
- Cesar
- Thomas

## Project Question

How much have wages and productivity diverged over time by state, and which states show the largest gaps?

## Planned Data

- State Real GDP: `[STATE]RGSP`
- State Employment: `[STATE]NA`
- State Wages: `[STATE]AVGWAGES`
- CPI (for inflation adjustment)

## Source Links (Canonical)

- BLS Productivity by State: https://www.bls.gov/lpc/state-productivity.htm
- BLS QCEW: https://www.bls.gov/cew/
- BLS OEWS: https://www.bls.gov/oes/
- BEA GDP by State: https://www.bea.gov/data/gdp/gdp-state
- BEA Employment by State: https://www.bea.gov/data/employment/employment-state
- BEA Regional Price Parities: https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area
- BLS CES State and Metro (SAE): https://www.bls.gov/sae/

Note: ETL v1 currently fetches BLS state productivity-family series through FRED CSV endpoints for reliable scripted access.

## Planned Outputs

- Interactive map of state-level productivity-wage gap
- State comparison line charts (indexed series)
- Gap ranking views
- Methods and data limitations documentation

## Repository Structure

```text
.
├── app.R
├── data/
├── data-raw/
│   └── run_all.R
├── R/
│   └── helpers.R
└── tests/
    └── testthat/
        └── test_helpers.R
```

## Quick Start

1. Install required R packages:

```r
install.packages(c("shiny", "dplyr", "tidyr", "ggplot2", "maps", "scales", "testthat"))
```

2. Build data outputs:

```bash
Rscript data-raw/run_all.R
```

3. Build levels outputs (BEA + QCEW + RPP):

```bash
Rscript data-raw/run_levels_all.R
```

4. Run tests:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

5. Run the Shiny app:

```bash
Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
```

## Current Status

- Repository has been repurposed from the prior Data Center concept.
- ETL now pulls state series for labor productivity, hourly compensation, and unit labor costs.
- App MVP includes map view, state trend view, rankings table, and methods tab.

## Data Outputs

- `data/series_catalog.csv` - discovered state-to-series mapping.
- `data/state_metrics_long.csv` - tidy long data by state, year, and metric.
- `data/state_panel.csv` - app-ready wide panel with gap metric.
- `data/qcew_state_private_wages.csv` - state private-sector QCEW wage inputs.
- `data/state_levels_panel.csv` - levels panel for GDP/job vs wages.
- `data/state_levels_long.csv` - tidy long version of levels metrics.

## App Notes

- `Levels Comparison` now includes:
  - a state choropleth map for a selected levels metric
  - a GDP/job vs real-wage scatter
  - a rankings table
- If the selected year lacks RPP-backed real wages (for example 2024), the app automatically shows the latest available year for that metric.
