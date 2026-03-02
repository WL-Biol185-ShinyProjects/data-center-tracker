# Utility helpers for Productivity vs Wages Atlas.

state_lookup <- function() {
  data.frame(
    state_name = c(tolower(state.name), "district of columbia"),
    state_abbr = c(state.abb, "DC"),
    stringsAsFactors = FALSE
  )
}

state_fips_lookup <- function() {
  data.frame(
    state_abbr = c(
      "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
      "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
      "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
      "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
      "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
    ),
    state_fips2 = c(
      "01", "02", "04", "05", "06", "08", "09", "10", "11", "12",
      "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
      "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
      "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
      "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
    ),
    stringsAsFactors = FALSE
  ) |>
    merge(state_lookup(), by = "state_abbr")
}

download_if_missing <- function(url, dest_path) {
  if (file.exists(dest_path)) {
    return(invisible(dest_path))
  }
  dir.create(dirname(dest_path), showWarnings = FALSE, recursive = TRUE)
  utils::download.file(url, destfile = dest_path, mode = "wb", quiet = TRUE)
  invisible(dest_path)
}

parse_numeric_safe <- function(x) {
  x <- gsub(",", "", x, fixed = TRUE)
  x <- gsub("(NA)", "", x, fixed = TRUE)
  x <- trimws(x)
  suppressWarnings(as.numeric(x))
}

download_text <- function(url) {
  connection <- url(url, open = "rb")
  on.exit(close(connection), add = TRUE)
  paste(readLines(connection, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

extract_series_links <- function(search_html, label_prefix) {
  pattern <- '<a href="/series/([A-Z0-9]+)" aria-label="([^"]+)"'
  matches <- gregexpr(pattern, search_html, perl = TRUE)
  raw_hits <- regmatches(search_html, matches)[[1]]

  if (length(raw_hits) == 0) {
    return(data.frame())
  }

  parsed <- lapply(raw_hits, function(hit) {
    parts <- regmatches(hit, regexec(pattern, hit, perl = TRUE))[[1]]
    data.frame(
      series_id = parts[2],
      label = parts[3],
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, parsed)
  keep <- startsWith(out$label, label_prefix)
  out <- out[keep, , drop = FALSE]

  if (nrow(out) == 0) {
    return(data.frame())
  }

  out$state_name <- tolower(trimws(sub(paste0("^", label_prefix), "", out$label)))
  out$state_name <- sub("^the ", "", out$state_name)

  out <- merge(out, state_lookup(), by = "state_name")
  out <- out[!duplicated(out$state_name), c("state_name", "state_abbr", "series_id")]
  rownames(out) <- NULL
  out
}

discover_metric_series <- function(search_text, label_prefix, max_pages = 10) {
  hits <- list()
  encoded <- URLencode(search_text, reserved = TRUE)

  for (page_id in seq_len(max_pages)) {
    search_url <- sprintf(
      "https://fred.stlouisfed.org/searchresults/?st=%s&pageID=%s",
      encoded,
      page_id
    )
    html <- download_text(search_url)
    page_hits <- extract_series_links(html, label_prefix)

    if (nrow(page_hits) > 0) {
      hits[[length(hits) + 1]] <- page_hits
    }
  }

  if (length(hits) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, hits)
  out <- out[!duplicated(out$state_name), ]
  rownames(out) <- NULL
  out
}

fetch_fred_series <- function(series_id) {
  series_url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", series_id)
  out <- utils::read.csv(series_url, stringsAsFactors = FALSE, na.strings = c(".", "NA", ""))

  names(out) <- c("date", "value")
  out$date <- as.Date(out$date)
  out$year <- as.integer(format(out$date, "%Y"))
  out$value <- as.numeric(out$value)
  out <- out[!is.na(out$value), c("year", "value")]
  rownames(out) <- NULL
  out
}

rebase_values <- function(values, years, base_year = 2007) {
  if (length(values) != length(years)) {
    stop("values and years must have the same length")
  }

  base_idx <- which(years == base_year)[1]
  if (is.na(base_idx)) {
    base_idx <- which(!is.na(values))[1]
  }
  if (is.na(base_idx) || values[base_idx] == 0) {
    return(rep(NA_real_, length(values)))
  }

  (values / values[base_idx]) * 100
}
