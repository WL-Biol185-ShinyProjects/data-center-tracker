source(file.path("R", "helpers.R"))

testthat::test_that("extract_series_links parses state links and ignores non-states", {
  html <- paste(
    '<a href="/series/AAA111" aria-label="Labor Productivity for Private Nonfarm in Alabama">',
    '<a href="/series/BBB222" aria-label="Labor Productivity for Private Nonfarm in the South Census Region">',
    '<a href="/series/CCC333" aria-label="Labor Productivity for Private Nonfarm in District of Columbia">'
  )

  out <- extract_series_links(html, "Labor Productivity for Private Nonfarm in ")

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true("alabama" %in% out$state_name)
  testthat::expect_true("district of columbia" %in% out$state_name)
})

testthat::test_that("rebase_values returns 100 for the base year", {
  values <- c(80, 100, 120)
  years <- c(2006, 2007, 2008)
  rebased <- rebase_values(values, years, base_year = 2007)

  testthat::expect_equal(rebased[2], 100)
  testthat::expect_equal(round(rebased[3], 1), 120)
})

testthat::test_that("state_fips_lookup includes 51 state/DC rows", {
  out <- state_fips_lookup()
  testthat::expect_equal(nrow(out), 51)
  testthat::expect_true(all(c("state_name", "state_abbr", "state_fips2") %in% names(out)))
})
