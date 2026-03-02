# ============================================================
# data_download.R — Person 4's File
# The Great Decoupling: DATA + ABOUT TAB
# ============================================================
#
# YOUR JOB:
# 1. Write the scripts that download real data from FRED
# 2. Save the data as CSV files in the data/ folder
# 3. Build the About tab with project info and methods
#
# WHAT TO DO:
# - Part A: Write code to download and save FRED data
# - Part B: Build the About tab UI (no server needed)
# - When you are done, the "integrator" will copy your
#   finished code into the main app.R
#
# HOW TO TEST:
# Part A: Run the download section line by line in RStudio
# Part B: Click "Run App" to test the About tab
# ============================================================


# ---- Load Libraries ----
library(shiny)
library(tidyverse)


# ============================================================
# PART A: DATA DOWNLOAD
# ============================================================
# This section is NOT part of the Shiny app.
# You run this ONCE to download data and save it as CSVs.
# Then the app loads the saved CSVs (much faster).
#
# IMPORTANT: Do NOT run this part every time the app starts.
# Only run it when you need fresh data.
# ============================================================

# ---- Step 1: Set Up Your FRED API Key ----
# Go to https://fred.stlouisfed.org/docs/api/api_key.html
# Sign up for a free API key.
# Paste your key below (keep the quotes).

# fred_api_key <- "YOUR_KEY_HERE"


# ---- Step 2: Download GDP Data for One State (test) ----
# Before downloading all 50 states, test with one.
# This is a placeholder — replace with real FRED API calls.

# Example of what the download code might look like:
#
# test_url <- paste0(
#   "https://api.stlouisfed.org/fred/series/observations",
#   "?series_id=VANGSP",
#   "&api_key=", fred_api_key,
#   "&file_type=json"
# )
#
# response <- httr::GET(test_url)
# data     <- jsonlite::fromJSON(
#   httr::content(response, "text")
# )


# ---- Step 3: Save Data as CSV ----
# After downloading, save to the data/ folder.
# The app will read these CSVs at startup.

# Example:
# write_csv(my_data, "data/gdp_by_state.csv")
# write_csv(my_wages, "data/wages_by_state.csv")


# ---- Placeholder: Create Fake CSV for Testing ----
# This creates a fake data file so the rest of the team
# can build their tabs while you work on real downloads.

fake_state_data <- tibble(
  state_name   = rep(c("Virginia", "California", "Texas"),
                     each = 27),
  year         = rep(1997:2023, 3),
  productivity = c(
    100 + cumsum(rnorm(27, mean = 2, sd = 1)),
    100 + cumsum(rnorm(27, mean = 2.5, sd = 1.2)),
    100 + cumsum(rnorm(27, mean = 1.8, sd = 0.9))
  ),
  income = c(
    100 + cumsum(rnorm(27, mean = 1, sd = 0.8)),
    100 + cumsum(rnorm(27, mean = 0.8, sd = 0.7)),
    100 + cumsum(rnorm(27, mean = 1.1, sd = 0.9))
  )
)

# Calculate the gap
fake_state_data <- fake_state_data %>%
  mutate(gap = productivity - income)

# Save to data/ folder (create folder if it doesn't exist)
dir.create("data", showWarnings = FALSE)
write_csv(fake_state_data, "data/state_data.csv")

# Confirm it saved
cat("Saved fake data to data/state_data.csv\n")
cat("Rows:", nrow(fake_state_data), "\n")
cat("Columns:", ncol(fake_state_data), "\n")


# ============================================================
# PART B: ABOUT TAB
# ============================================================
# This is the informational page — no server code needed.
# It just displays text about the project.
# ============================================================

# ---- UI for About Tab ----
tab_about_ui <- fluidPage(

  h2("About This Project"),

  h4("What is the Great Decoupling?"),
  p(
    "Since the late 1970s, productivity in the U.S. has",
    "grown much faster than typical worker income. This",
    "phenomenon is known as the 'Great Decoupling.' This",
    "app lets you explore that gap state by state."
  ),

  h4("Data Sources"),
  p(
    "All data comes from the Federal Reserve Economic",
    "Data (FRED) API maintained by the Federal Reserve",
    "Bank of St. Louis."
  ),
  tags$ul(
    tags$li("GDP by state (real, chained dollars)"),
    tags$li("Total employment by state"),
    tags$li("Average wages by state"),
    tags$li("Time period: 1997 to 2023")
  ),

  h4("Methods"),
  p(
    "Productivity is calculated as real GDP divided by",
    "total employment (GDP per worker). Income is measured",
    "using average wages. Both measures are indexed to a",
    "base year (1997 = 100) so we can compare growth rates",
    "across states regardless of their size."
  ),

  h4("Our Team"),
  p(
    "This project was created for BIO 185: Visualizing",
    "Big Data at Washington & Lee University."
  ),
  tags$ul(
    tags$li("Person 1 — Home tab"),
    tags$li("Person 2 — Map tab"),
    tags$li("Person 3 — State Comparison tab"),
    tags$li("Person 4 — Data & About tab")
  ),

  h4("Code"),
  p(
    "All source code is available on GitHub at:",
    tags$a(
      href = "https://github.com/yourname/great-decoupling",
      "github.com/yourname/great-decoupling"
    )
  )

  # NEXT STEPS for Person 4:
  # - Replace fake data download with real FRED API calls
  # - Update the methods section with exact FRED series IDs
  # - Add your real team member names
  # - Update the GitHub link
)


# ---- Test This Tab By Itself ----
# The About tab has no server logic, but we still need
# an empty server function to run the mini test app.

ui <- navbarPage(
  title = "Test: About Tab",
  tabPanel("About", tab_about_ui)
)

server <- function(input, output) {
  # Nothing needed here — About is just text
}

shinyApp(ui = ui, server = server)
