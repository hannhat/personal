# ============================================================
# 01_collect_data.R
# Scrape player projections (ZiPS, Steamer, ATC) and standings
# for seasons 2015-2024.
#
# FanGraphs projection pages follow predictable URL patterns.
# This script builds a cache of raw CSVs so you don't re-scrape.
# ============================================================

#source("00_setup.R")

# ---- Helpers -----------------------------------------------

#' Download a FanGraphs projection table for a given system/year/type.
#'
#' FanGraphs exposes projections at URLs like:
#'   https://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer&team=0&lg=all&players=0
#' But the actual *data download* endpoint (which returns CSV) is:
#'   https://www.fangraphs.com/api/projections?type=steamer&stats=bat&pos=all&team=0&players=0&lg=all
#' That API endpoint is what we target here.
#'
#' NOTE: FanGraphs only serves *current-season* projections at the live
#' endpoint. For HISTORICAL projections (2015-2023) you have two options:
#'   1. Use the Wayback Machine (slow, unreliable).
#'   2. Use the pre-cached CSV export files that FG makes available via
#'      their data download buttons (see `download_fg_projection_csv()`).
#' We use approach 2 where possible and fall back to Wayback for older years.

FG_API_BASE <- "https://www.fangraphs.com/api/projections"

#' Map friendly system names to FanGraphs API type strings.
system_map <- c(
  "steamer"  = "steamer",
  "zips"     = "zips",
  "atc"      = "atc",
  "steamerr" = "steamerr",   # Steamer ROS (rest-of-season)
  "zipsdc"   = "zipsdc"      # ZiPS DC (depth chart weighted)
)

#' Fetch one projection table from the FanGraphs API.
#' Returns a data frame or NULL on failure.
fetch_fg_projection <- function(system, stats = c("bat", "pit"), year = NULL,
                                pos = "all", team = 0, lg = "all") {
  stats <- match.arg(stats)
  type  <- system_map[tolower(system)]
  if (is.na(type)) stop("Unknown projection system: ", system)
  
  # FanGraphs API does not support year parameter for historical data in the
  # simple endpoint; for current season projections this works fine.
  url <- glue(
    "{FG_API_BASE}?type={type}&stats={stats}&pos={pos}&team={team}&players=0&lg={lg}"
  )
  
  resp <- tryCatch(
    httr::GET(url, httr::add_headers(`User-Agent` = "R research script")),
    error = function(e) NULL
  )
  
  if (is.null(resp) || httr::http_error(resp)) {
    warning("Failed to fetch: ", url)
    return(NULL)
  }
  
  content <- httr::content(resp, as = "text", encoding = "UTF-8")
  df <- tryCatch(jsonlite::fromJSON(content, flatten = TRUE), error = function(e) NULL)
  if (is.null(df) || !is.data.frame(df)) return(NULL)
  
  df <- janitor::clean_names(df)
  df$projection_system <- system
  df$season <- year %||% as.integer(format(Sys.Date(), "%Y"))
  df$type <- stats
  df
}

# Null-coalescing operator (base R doesn't have one until 4.4)
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ---- Historical projection scraping strategy ----------------
#
# For seasons BEFORE the current year, FanGraphs does not expose historical
# projections through its API. The two practical approaches are:
#
# A) FanGraphs leaderboard download (RECOMMENDED for 2020+):
#    FanGraphs provides data export buttons on leaderboard pages. These
#    can be accessed programmatically via the `baseballr::fg_batter_leaders()`
#    and `baseballr::fg_pitcher_leaders()` functions, which pull *actual stats*,
#    not projections. For historical PROJECTIONS specifically, you need to
#    download the export CSVs manually or via Wayback Machine.
#
# B) Wayback Machine CDX API (for 2015-2019):
#    We can query the Wayback Machine for snapshots of FanGraphs projection
#    pages taken in late March/early April of each season (when pre-season
#    projections are finalised). This is slow but works.
#
# C) Community-hosted archives:
#    Some researchers have archived FanGraphs projection CSVs on GitHub.
#    Notably, the "baseballProjections" repo by Neil Paine (FiveThirtyEight)
#    and other similar repos. Worth checking GitHub before scraping.
#
# The function below implements approach B for historical seasons.

#' Get the best Wayback Machine snapshot URL for a FanGraphs projection page.
get_wayback_url <- function(system, stats, year) {
  # Target: late March / early April snapshot (before Opening Day)
  timestamp <- glue("{year}0401000000")
  fg_url <- glue(
    "https://www.fangraphs.com/projections.aspx?pos=all&stats={stats}&type={system}&team=0&lg=all&players=0"
  )
  cdx_url <- glue(
    "http://archive.org/wayback/available?url={URLencode(fg_url, reserved=TRUE)}&timestamp={timestamp}"
  )
  
  resp <- tryCatch(httr::GET(cdx_url), error = function(e) NULL)
  if (is.null(resp) || httr::http_error(resp)) return(NULL)
  
  parsed <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
  parsed$archived_snapshots$closest$url %||% NULL
}

#' Parse a FanGraphs HTML projection table from a (possibly archived) URL.
parse_fg_html_table <- function(url, system, stats, year) {
  page <- tryCatch(rvest::read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  
  # FanGraphs projection tables are inside <div class="rgMasterTable"> or
  # similar; the actual <table> element we want is the data grid.
  # This selector has been stable for several years.
  tbl <- tryCatch(
    page |>
      rvest::html_element("table.rgMasterTable") |>
      rvest::html_table(header = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(tbl) || nrow(tbl) == 0) {
    # Try alternative selector used on newer FG page layouts
    tbl <- tryCatch(
      page |>
        rvest::html_element("#LeaderBoard1_dg1_ctl00") |>
        rvest::html_table(header = TRUE),
      error = function(e) NULL
    )
  }
  
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  
  tbl <- janitor::clean_names(tbl)
  tbl$projection_system <- system
  tbl$season <- year
  tbl$type <- stats
  tbl
}


# ---- Standings from baseballr / Baseball Reference ----------

#' Fetch final standings for a given season.
#' Uses baseballr's bref_standings_on_date() to pull end-of-season records.
fetch_standings <- function(year) {
  cache_path <- here::here(glue("sabermetrics_playground/data/raw/standings/standings_{year}.rds"))
  if (fs::file_exists(cache_path)) {
    message(glue("  [cache] standings {year}"))
    return(readRDS(cache_path))
  }
  
  message(glue("  [fetch] standings {year}"))
  # Use the last full day of the regular season. These are approximate;
  # adjust if a season ended on a different date.
  season_end_dates <- c(
    "2015" = "2015-10-04", "2016" = "2016-10-02", "2017" = "2017-10-01",
    "2018" = "2018-09-30", "2019" = "2019-09-29", "2020" = "2020-09-27",
    "2021" = "2021-10-03", "2022" = "2022-10-05", "2023" = "2023-10-01",
    "2024" = "2024-09-29"
  )
  end_date <- season_end_dates[as.character(year)]
  if (is.na(end_date)) {
    warning("No end date configured for year: ", year)
    return(NULL)
  }
  
  # baseballr wraps the Baseball Reference standings page
  standings <- tryCatch(
    baseballr::bref_standings_on_date(end_date, division = "Overall", from = FALSE),
    error = function(e) {
      warning("Failed to fetch standings for ", year, ": ", e$message)
      NULL
    }
  )
  
  if (!is.null(standings)) {
    standings$season <- year
    saveRDS(standings, cache_path)
  }
  standings
}


# ---- Team win projections from FanGraphs -------------------
#
# FanGraphs publishes pre-season team win projections (derived from their
# depth chart projections). The URL pattern is:
#   https://www.fangraphs.com/depthcharts.aspx?position=Standings
# These are available for current season via the API; historical versions
# require Wayback Machine or manual collection.
#
# For the EDA in this project, an alternative is to RECONSTRUCT team-level
# win projections by aggregating individual player WAR projections with
# assumed playing time. Script 02 handles this reconstruction.

fetch_fg_team_projections <- function(year) {
  cache_path <- here::here(glue("sabermetrics_playground/data/raw/projections/fg_team_wins_{year}.rds"))
  if (fs::file_exists(cache_path)) {
    message(glue("  [cache] FG team projections {year}"))
    return(readRDS(cache_path))
  }
  
  # For current season: hit the API
  url <- "https://www.fangraphs.com/api/depth-charts/standings"
  resp <- tryCatch(httr::GET(url), error = function(e) NULL)
  if (!is.null(resp) && !httr::http_error(resp)) {
    df <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                             flatten = TRUE)
    if (is.data.frame(df)) {
      df <- janitor::clean_names(df)
      df$season <- year
      saveRDS(df, cache_path)
      return(df)
    }
  }
  NULL
}


# ---- Main collection loop ----------------------------------

SEASONS      <- 2016:2024
SYSTEMS      <- c("steamer", "zips", "atc")
STATS_TYPES  <- c("bat", "pit")

collect_projections <- function(seasons = SEASONS,
                                systems = SYSTEMS,
                                stats   = STATS_TYPES) {
  all_projections <- list()
  
  for (yr in seasons) {
    for (sys in systems) {
      for (st in stats) {
        cache_path <- here::here(
          glue("sabermetrics_playground/data/raw/projections/{sys}_{st}_{yr}.rds")
        )
        
        if (fs::file_exists(cache_path)) {
          message(glue("  [cache] {sys} {st} {yr}"))
          all_projections[[glue("{sys}_{st}_{yr}")]] <- readRDS(cache_path)
          next
        }
        
        message(glue("  [fetch] {sys} {st} {yr}"))
        
        df <- NULL
        
        if (yr == as.integer(format(Sys.Date(), "%Y"))) {
          # Current season: use the live API
          df <- fetch_fg_projection(sys, st, yr)
        } else {
          # Historical: try Wayback Machine
          wb_url <- get_wayback_url(sys, st, yr)
          if (!is.null(wb_url)) {
            df <- parse_fg_html_table(wb_url, sys, st, yr)
          }
        }
        
        if (!is.null(df)) {
          print(cache_path)
          saveRDS(df, cache_path)
          all_projections[[glue("{sys}_{st}_{yr}")]] <- df
          message(glue("    -> {nrow(df)} rows"))
        } else {
          message(glue("    -> FAILED"))
        }
        
        Sys.sleep(1.5)  # be polite to servers
      }
    }
  }
  
  all_projections
}


# ---- Run collection ----------------------------------------

message("=== Collecting player projections ===")
projections_raw <- collect_projections()

message("\n=== Collecting standings ===")
standings_raw <- purrr::map(SEASONS, fetch_standings)
names(standings_raw) <- SEASONS

message("\n=== Collection complete ===")
message(glue("  Projection tables collected: {length(projections_raw)}"))
message(glue("  Seasons with standings: {sum(!sapply(standings_raw, is.null))}"))

# Save master lists
saveRDS(projections_raw, here::here("sabermetrics_playground/data/raw/projections/all_projections_raw.rds"))
saveRDS(standings_raw,   here::here("sabermetrics_playground/data/raw/standings/all_standings_raw.rds"))
