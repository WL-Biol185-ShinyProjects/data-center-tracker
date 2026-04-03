# Productivity vs Wages Atlas

This repository's active Shiny app lives in:
`App Workflow/`

Run locally with:

```bash
Rscript -e 'shiny::runApp("App Workflow", launch.browser = TRUE)'
```

## Project Layout

- `App Workflow/`: the live app, app-specific data, and the main project README
- `data-raw/`: ETL scripts and helper functions used to rebuild committed app data
- `deliverables/`: screenshots and presentation/report assets
- `legacy/`: archived historical artifacts kept for record-keeping, not used by the current app

## Canonical Data Location

The app reads from:
`App Workflow/data/`

The ETL scripts in `data-raw/` now refresh that same folder directly so the rebuild path matches the running app.
