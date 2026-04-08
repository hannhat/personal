# CLAUDE.md — MLB Team Wins Projection System

## Project Summary

This is a course project (ST451 — Bayesian Machine Learning, due May 7, 2026) that builds
an MLB team-level batting WAR projection system. The core hypothesis is that modeling the
dependence between player performance and playing time allocation — rather than treating
them as independent — improves team-level projections.

The system has three components:
1. **Bayesian MARCEL** — hierarchical Bayesian player performance projection (wRC+)
2. **Playing time classification** — Bayesian multinomial logistic regression classifying
   players monthly into starter/platoon/bench categories
3. **Injury model** — simple monthly availability model (built last)

These feed into a Monte Carlo simulation that draws player performance, simulates monthly
playing time allocation, and produces team-level batting WAR distributions.

The full specification document (specification_document.pdf) contains detailed model
architecture, evaluation strategy, and theoretical motivation. Refer to it for modeling
decisions. This file focuses on data infrastructure and working guidelines.

---

## Guidelines for Working With Me

### General Principles
- I place an extremely high premium on understanding everything I implement. Consult me
  frequently on what you are doing, even for tasks I've delegated to you.
- Do not make modeling decisions without asking me first. If something requires a judgment
  call about model specification, priors, features, or evaluation, flag it.
- When writing code, include comments explaining non-obvious choices.
- Prefer simple, readable code over clever code. I need to understand and present this work.

### Levels of Involvement

**Level 1 — Data pipeline (you lead, I review):**
You actively write code for data scraping, pulling, cleaning, and caching. I review and
approve before running. This is where I want the most help.

**Level 2 — Statistical modeling (I lead, you assist):**
I write the modeling code. You help with specific implementation problems (PyMC syntax,
debugging convergence issues, etc.) but I decide what models to run.

**Level 3 — Conceptual discussion (no code):**
I discuss modeling ideas, interpretation, and extensions with you. No code output — just
reasoning and suggestions.

**Level 4 — Visualization (you lead, I review):**
You actively write code for charts, tables, and presentation of results. I specify what I
want to see and review the output.

---

## Data Pipeline — Sources and Acquisition

All data should be cached locally after first pull to avoid repeated API calls.
Use a /data directory with subdirectories by source.

### 1. Season-Level Batting Stats (FanGraphs via pybaseball)

**What:** All available season-level batting stats. One row per player per season.
**Years:** 2015–2025 (10 seasons).
**Source:** `pybaseball.batting_stats(start_season, end_season)`
**Notes:**
- Pull with `qual=0` to get all players, not just qualified hitters.
- This is the primary data source for the Bayesian MARCEL and component regression.
- fWAR is included in this pull and serves as individual-level evaluation ground truth.
- Games played and team games give a simple injury propensity proxy.
- League averages can be derived by aggregating across players per season.
**Output:** `data/fangraphs/fg_hitters.csv`

### 2. Monthly Batting Data (Statcast via pybaseball)

**What:** Pitch-level data aggregated to player-month: PA, hits, HR, BB, K, wOBA
components, batted ball outcomes. Need to compute monthly wRC+ or wOBA from these.
**Years:** 2015–2025 (Statcast era only).
**Source:** `pybaseball.statcast(start_dt, end_dt)` queried month by month, then
aggregated by batter ID.
**Notes:**
- Statcast returns pitch-level data. Aggregate to plate appearances first (filter to rows
  where `events` is not null), then to player-month summaries.
- This is a large data pull. Cache aggressively. Consider pulling one season at a time
  and saving intermediate files.
- For pre-2015 data, use Baseball Reference game logs via pybaseball as a fallback
  (less granular but has PA counts).
- Used for: training the playing time classification model, calibrating monthly wRC+
  variance for the simulation, and evaluation ground truth for playing time predictions.
**Output:** `data/statcast/monthly_batting_{year}.csv` (one file per year)

### 3. Player Position and Defensive Data

**What:** Defensive innings or games at each position by player by season.
**Years:** 2010–2025.
**Source:** FanGraphs fielding data via pybaseball, or Baseball Reference fielding logs.
**Notes:**
- Used for positional eligibility in the team-level allocation step.
- A player is eligible at a position if they logged significant time there in the prior
  season (threshold TBD, maybe 100+ innings or 20+ games).
**Output:** `data/fangraphs/fielding_data.csv`

### 4. Opening Day Lineups

**What:** Starting lineup (batting order + positions) for each team's first game of each
season.
**Years:** 2015–2025.
**Source:** Baseball Reference box scores via pybaseball. Use `schedule_and_record(year,
team)` to find the first game, then scrape the box score for the starting lineup.
**Notes:**
- Used as a binary feature in the playing time classification model (was this player in the
  Opening Day starting lineup?).
- May require scraping individual game pages — check what pybaseball exposes directly
  before writing custom scrapers.
**Output:** `data/opening_day/lineups_{year}.csv`

### 5. Service Time and Contract Data

**What:** MLB debut year (for approximate service time), and ideally salary/contract data.
**Years:** All players in the dataset.
**Source:**
- Debut year: `pybaseball.playerid_lookup()` or Lahman database (via pybaseball).
- Salary (optional extension): Spotrac (spotrac.com) or Baseball Reference player pages.
  Would require scraping.
**Notes:**
- Service time = current year minus debut year. Crude but functional proxy for
  organizational commitment and playing time stickiness.
- Salary data is a better proxy but requires a separate scraping effort. Start with debut
  year; add salary later if time permits.
**Output:** `data/players/service_time.csv`

### 6. Injury Data

**What:** At minimum, games played per season (already in source 1). Optionally, IL
transaction history with dates and stint type (10-day vs 60-day).
**Source:**
- Basic: derived from source 1 (games played / team games).
- Detailed: Prosports Transactions (prosportstransactions.com) — searchable database of
  all MLB IL placements. Would require scraping.
- Alternative: Retrosheet transaction logs.
**Notes:**
- The injury model is built last. Start with the games-played proxy from source 1.
- If scraping IL data, want: player name/ID, date placed on IL, date activated, IL type.
- This is lower priority than sources 1-5. Do not scrape until the core pipeline is working.
**Output:** `data/injury/il_transactions.csv` (if scraped)

---

## Data Pipeline — Implementation Notes

### Player ID Alignment
Different sources use different player IDs (FanGraphs ID, MLBAM ID, Baseball Reference
ID). The Lahman/Chadwick lookup table (available via pybaseball) maps between them.
Build a master player ID crosswalk early and join everything on it.

### Caching
All raw data pulls should be cached to disk immediately. Never re-pull data that has
already been successfully retrieved. Use a simple check: if the output file exists and is
non-empty, skip the pull.

### Data Validation
After each pull, run basic sanity checks:
- Expected number of rows (roughly 400-800 player-seasons per year for batting stats).
- No fully null columns.
- PA and games played are non-negative.
- wRC+ values are in a plausible range (0–250 for almost all players).
- All expected teams (30) are present in each season.

---

## Directory Structure

```
project/
├── CLAUDE.md                  # This file
├── specification_document.pdf # Full project specification
├── data/
│   ├── fangraphs/            # Season-level batting, fielding, projections
│   ├── statcast/             # Monthly pitch/PA-level data
│   ├── opening_day/          # Opening Day lineups
│   ├── players/              # Player IDs, service time, contracts
│   ├── injury/               # IL transaction data (if scraped)
│   ├── evaluation/           # Team standings, ground truth
│   └── holdout/              # 2024-2025 data, kept separate
├── notebooks/                 # Jupyter notebooks for modeling
├── src/                       # Reusable Python modules
│   ├── data_pipeline.py      # Data acquisition and cleaning functions
│   ├── marcel.py             # Bayesian MARCEL implementation
│   ├── component_model.py    # Component regression implementation
│   ├── playing_time.py       # Playing time classification model
│   ├── simulation.py         # Monte Carlo simulation pipeline
│   └── evaluation.py         # Evaluation metrics and comparisons
└── output/                    # Figures, tables, results
```

