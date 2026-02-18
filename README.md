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

1. Install required R packages (to be finalized by team).
2. Build data outputs:

```bash
Rscript data-raw/run_all.R
```

3. Run the Shiny app:

```bash
Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
```

## Current Status

- Repository has been repurposed from the prior Data Center concept.
- Starter scaffold and placeholders are in place.
- Next step is implementing ETL and first visual prototype.
