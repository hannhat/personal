# ============================================================
# 02_process_data.R
# Clean raw projection and standings data.
# Compute derived variables: WAR/PA, depth metrics, team aggregates.
# ============================================================

source("00_setup.R")

projections_raw <- readRDS(here::here("data/raw/projections/all_projections_raw.rds"))
standings_raw   <- readRDS(here::here("data/raw/standings/all_standings_raw.rds"))


# ---- Column standardisation --------------------------------
#
# FanGraphs column names differ slightly across years and systems.
# We normalise to a common schema.

BATTER_COLS <- c(
  player_name   = "name",        # try multiple possible source names
  player_id     = "playerid",
  team          = "team",
  season        = "season",
  projection_system = "projection_system",
  pa            = "pa",          # plate appearances
  war           = "war",
  woba          = "woba",
  wraa          = "wraa",
  ops           = "ops",
  avg           = "avg",
  obp           = "obp",
  slg           = "slg",
  bb_pct        = "bb_percent",
  k_pct         = "k_percent",
  iso           = "iso",
  babip         = "babip",
  hr            = "hr"
)

PITCHER_COLS <- c(
  player_name   = "name",
  player_id     = "playerid",
  team          = "team",
  season        = "season",
  projection_system = "projection_system",
  ip            = "ip",
  war           = "war",
  era           = "era",
  fip           = "fip",
  xfip          = "xfip",
  k_9           = "k_9",
  bb_9          = "bb_9",
  hr_9          = "hr_9",
  k_pct         = "k_percent",
  bb_pct        = "bb_percent",
  whip          = "whip",
  babip         = "babip"
)

#' Attempt to extract a column from a data frame by trying multiple candidate
#' names. Returns NA vector if none found.
coalesce_col <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  rep(NA_real_, nrow(df))
}

#' Standardise a raw batter projection data frame.
standardise_batters <- function(df) {
  tibble(
    player_name        = coalesce_col(df, c("name", "player_name", "playername", "name_1")),
    player_id          = coalesce_col(df, c("playerid", "player_id", "fg_id")),
    team               = coalesce_col(df, c("team", "team_name", "tm")),
    season             = df$season[[1]],
    projection_system  = df$projection_system[[1]],
    pa                 = as.numeric(coalesce_col(df, c("pa", "plate_appearances"))),
    war                = as.numeric(coalesce_col(df, c("war", "fwar", "fangraphs_war"))),
    woba               = as.numeric(coalesce_col(df, c("woba", "w_oba"))),
    ops                = as.numeric(coalesce_col(df, c("ops"))),
    avg                = as.numeric(coalesce_col(df, c("avg", "ba"))),
    obp                = as.numeric(coalesce_col(df, c("obp"))),
    slg                = as.numeric(coalesce_col(df, c("slg"))),
    bb_pct             = as.numeric(coalesce_col(df, c("bb_percent", "bb_pct", "bb"))),
    k_pct              = as.numeric(coalesce_col(df, c("k_percent", "k_pct", "so"))),
    hr                 = as.numeric(coalesce_col(df, c("hr"))),
    babip              = as.numeric(coalesce_col(df, c("babip")))
  ) |>
    filter(!is.na(player_name), !is.na(pa), pa > 0)
}

#' Standardise a raw pitcher projection data frame.
standardise_pitchers <- function(df) {
  tibble(
    player_name        = coalesce_col(df, c("name", "player_name", "playername")),
    player_id          = coalesce_col(df, c("playerid", "player_id", "fg_id")),
    team               = coalesce_col(df, c("team", "team_name", "tm")),
    season             = df$season[[1]],
    projection_system  = df$projection_system[[1]],
    ip                 = as.numeric(coalesce_col(df, c("ip", "innings_pitched"))),
    war                = as.numeric(coalesce_col(df, c("war", "fwar"))),
    era                = as.numeric(coalesce_col(df, c("era"))),
    fip                = as.numeric(coalesce_col(df, c("fip"))),
    xfip               = as.numeric(coalesce_col(df, c("xfip", "x_fip"))),
    k_9                = as.numeric(coalesce_col(df, c("k_9", "so9", "k9"))),
    bb_9               = as.numeric(coalesce_col(df, c("bb_9", "bb9"))),
    whip               = as.numeric(coalesce_col(df, c("whip"))),
    babip              = as.numeric(coalesce_col(df, c("babip")))
  ) |>
    filter(!is.na(player_name), !is.na(ip), ip > 0)
}

# Apply standardisation
message("Standardising projection data...")

batters_std <- projections_raw |>
  keep(\(df) !is.null(df) && isTRUE(df$type[[1]] == "bat")) |>
  map(standardise_batters) |>
  list_rbind()

pitchers_std <- projections_raw |>
  keep(\(df) !is.null(df) && isTRUE(df$type[[1]] == "pit")) |>
  map(standardise_pitchers) |>
  list_rbind()

message(glue("  Batters: {nrow(batters_std)} rows across {n_distinct(batters_std$season)} seasons"))
message(glue("  Pitchers: {nrow(pitchers_std)} rows across {n_distinct(pitchers_std$season)} seasons"))


# ---- Derived variables: WAR per PA / per IP ----------------

batters_std <- batters_std |>
  mutate(
    war_per_600pa = war / pa * 600,     # normalise to 600 PA = ~full season
    war_per_pa    = war / pa,
    is_starter    = pa >= 400,           # rough cut for projected starters
    is_bench      = pa > 50 & pa < 400,
    role          = case_when(
      pa >= 400 ~ "Starter",
      pa >= 150 ~ "Platoon/Semi-regular",
      pa >= 50  ~ "Bench",
      TRUE       ~ "Fringe"
    )
  )

pitchers_std <- pitchers_std |>
  mutate(
    war_per_100ip  = war / ip * 100,
    is_starter_sp  = ip >= 140,
    role           = case_when(
      ip >= 140 ~ "SP Starter",
      ip >= 60  ~ "SP/RP",
      ip >= 20  ~ "RP Regular",
      TRUE       ~ "Fringe RP"
    )
  )


# ---- Standardise standings ---------------------------------

#' Extract wins, losses, team name, and season from a standings data frame.
#' Baseball Reference standings pages vary somewhat in column naming.
standardise_standings <- function(df, year) {
  if (is.null(df)) return(NULL)
  df <- janitor::clean_names(df)
  tibble(
    team   = coalesce_col(df, c("tm", "team", "team_name", "franchise")),
    wins   = as.integer(coalesce_col(df, c("w", "wins"))),
    losses = as.integer(coalesce_col(df, c("l", "losses"))),
    season = year
  ) |>
    filter(!is.na(team), !is.na(wins)) |>
    mutate(win_pct = wins / (wins + losses))
}

standings_std <- imap(standings_raw, standardise_standings) |>
  compact() |>
  list_rbind()

message(glue("  Standings: {nrow(standings_std)} team-seasons"))


# ---- Team name harmonisation --------------------------------
#
# FanGraphs and Baseball Reference use different team abbreviations and
# full names. We create a lookup table to harmonise them.

team_crosswalk <- tribble(
  ~fg_abbr,  ~bref_abbr, ~full_name,
  "ARI",     "ARI",      "Arizona Diamondbacks",
  "ATL",     "ATL",      "Atlanta Braves",
  "BAL",     "BAL",      "Baltimore Orioles",
  "BOS",     "BOS",      "Boston Red Sox",
  "CHC",     "CHC",      "Chicago Cubs",
  "CHW",     "CWS",      "Chicago White Sox",
  "CIN",     "CIN",      "Cincinnati Reds",
  "CLE",     "CLE",      "Cleveland Guardians",  # was Indians pre-2022
  "COL",     "COL",      "Colorado Rockies",
  "DET",     "DET",      "Detroit Tigers",
  "HOU",     "HOU",      "Houston Astros",
  "KCR",     "KCR",      "Kansas City Royals",
  "LAA",     "LAA",      "Los Angeles Angels",
  "LAD",     "LAD",      "LAD",
  "MIA",     "MIA",      "Miami Marlins",
  "MIL",     "MIL",      "Milwaukee Brewers",
  "MIN",     "MIN",      "Minnesota Twins",
  "NYM",     "NYM",      "New York Mets",
  "NYY",     "NYY",      "New York Yankees",
  "OAK",     "OAK",      "Oakland Athletics",
  "PHI",     "PHI",      "Philadelphia Phillies",
  "PIT",     "PIT",      "Pittsburgh Pirates",
  "SDP",     "SDP",      "San Diego Padres",
  "SEA",     "SEA",      "Seattle Mariners",
  "SFG",     "SFG",      "San Francisco Giants",
  "STL",     "STL",      "St. Louis Cardinals",
  "TBR",     "TBR",      "Tampa Bay Rays",
  "TEX",     "TEX",      "Texas Rangers",
  "TOR",     "TOR",      "Toronto Blue Jays",
  "WSN",     "WSN",      "Washington Nationals"
)

# Function to fuzzy-match team abbreviations
harmonise_team <- function(team_col, source = c("fg", "bref")) {
  source <- match.arg(source)
  lookup_col <- if (source == "fg") "fg_abbr" else "bref_abbr"
  team_crosswalk$full_name[match(team_col, team_crosswalk[[lookup_col]])]
}


# ---- Aggregate to team level --------------------------------

#' For each team-season-system, compute:
#' - Total projected WAR (batters + pitchers)
#' - Depth metrics: how many players exist above various WAR/PA thresholds
#' - Concentration metrics: is WAR concentrated in a few players?

compute_team_depth_metrics <- function(batters, pitchers) {
  
  # --- Batter aggregation ---
  batter_team <- batters |>
    group_by(team, season, projection_system) |>
    summarise(
      # Total projected offensive WAR
      total_batter_war        = sum(war, na.rm = TRUE),
      # Projected WAR for starters only
      starter_batter_war      = sum(war[is_starter], na.rm = TRUE),
      # DEPTH: number of non-starters (bench/platoon) with war_per_600pa >= threshold
      n_viable_bench          = sum(is_bench & war_per_600pa >= 1.0, na.rm = TRUE),
      n_above1war_bench       = sum(is_bench & war_per_600pa >= 1.0, na.rm = TRUE),
      n_above2war_bench       = sum(is_bench & war_per_600pa >= 2.0, na.rm = TRUE),
      n_above1war_total       = sum(war_per_600pa >= 1.0, na.rm = TRUE),
      # CONCENTRATION: Gini-like measure -- what share of WAR is in top 3 hitters?
      top3_batter_war_share   = {
        sorted_war <- sort(war, decreasing = TRUE)
        if (sum(war, na.rm = TRUE) > 0)
          sum(head(sorted_war, 3), na.rm = TRUE) / sum(war, na.rm = TRUE)
        else NA_real_
      },
      # WAR/PA quality of depth players (players with pa 50-399)
      depth_war_per_600pa_mean = mean(war_per_600pa[is_bench], na.rm = TRUE),
      depth_war_per_600pa_sd   = sd(war_per_600pa[is_bench], na.rm = TRUE),
      # Optionality value: total war in non-starter players
      bench_pool_war           = sum(war[is_bench], na.rm = TRUE),
      n_batters_total          = n(),
      n_starters               = sum(is_starter, na.rm = TRUE),
      n_bench                  = sum(is_bench, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Pitcher aggregation ---
  pitcher_team <- pitchers |>
    group_by(team, season, projection_system) |>
    summarise(
      total_pitcher_war       = sum(war, na.rm = TRUE),
      starter_pitcher_war     = sum(war[is_starter_sp], na.rm = TRUE),
      bullpen_war             = sum(war[!is_starter_sp], na.rm = TRUE),
      n_viable_arms           = sum(war_per_100ip >= 1.0, na.rm = TRUE),
      top3_pitcher_war_share  = {
        sorted_war <- sort(war, decreasing = TRUE)
        if (sum(war, na.rm = TRUE) > 0)
          sum(head(sorted_war, 3), na.rm = TRUE) / sum(war, na.rm = TRUE)
        else NA_real_
      },
      n_pitchers_total        = n(),
      .groups = "drop"
    )
  
  # --- Join ---
  left_join(batter_team, pitcher_team, by = c("team", "season", "projection_system")) |>
    mutate(
      total_projected_war = total_batter_war + total_pitcher_war,
      # Replacement-level teams win ~47 games; 1 WAR ~ 1 win above replacement
      # League-average replacement-level offense/pitching gives ~47 wins
      projected_wins_raw  = 47 + total_projected_war,
      # Pythagoras-implied wins can also be used if run projections are available
    )
}

message("Computing team-level depth metrics...")
team_projections <- compute_team_depth_metrics(batters_std, pitchers_std)
message(glue("  Team-season-system rows: {nrow(team_projections)}"))


# ---- Merge with actual standings ----------------------------

# Harmonise team names before joining
# NOTE: This join requires that team abbreviations match between FG and BREF.
# The crosswalk above handles most cases; manual fixes may be needed for
# some team name variants in older BREF data.

standings_harmonised <- standings_std |>
  mutate(team_full = harmonise_team(team, "bref")) |>
  select(team_bref = team, team_full, season, actual_wins = wins,
         actual_losses = losses, actual_win_pct = win_pct)

team_projections_harmonised <- team_projections |>
  mutate(team_full = harmonise_team(team, "fg"))

analysis_df <- team_projections_harmonised |>
  left_join(standings_harmonised, by = c("team_full", "season")) |>
  mutate(
    # Core outcome: did this team outperform its projected wins?
    win_delta            = actual_wins - projected_wins_raw,
    # Positive = outperformed projection
    outperformed         = win_delta > 0
  )

message(glue("  Analysis data frame: {nrow(analysis_df)} rows, {ncol(analysis_df)} columns"))
message(glue("  Team-seasons with actual wins: {sum(!is.na(analysis_df$actual_wins))}"))


# ---- Save processed data -----------------------------------

saveRDS(batters_std,      here::here("data/processed/batters_std.rds"))
saveRDS(pitchers_std,     here::here("data/processed/pitchers_std.rds"))
saveRDS(standings_std,    here::here("data/processed/standings_std.rds"))
saveRDS(team_projections, here::here("data/processed/team_projections.rds"))
saveRDS(analysis_df,      here::here("data/processed/analysis_df.rds"))
saveRDS(team_crosswalk,   here::here("data/processed/team_crosswalk.rds"))

message("Processing complete. Files written to data/processed/.")
