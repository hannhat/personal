"""
pull_season_batting.py

Pulls season-level batting stats for all players (2015-2025) from FanGraphs
via pybaseball and saves to data/fangraphs/fg_hitters.csv.

- qual=0 to include all players, not just qualified hitters
- Skips re-pull if output file already exists and is non-empty (cache-first)
- Runs basic sanity checks after pull (per CLAUDE.md validation spec)
"""

import pathlib
import time
import pandas as pd
import pybaseball

# Enable pybaseball's built-in disk cache to avoid redundant network calls
pybaseball.cache.enable()

# ── Paths ──────────────────────────────────────────────────────────────────
ROOT = pathlib.Path(__file__).resolve().parents[2]  # project root
OUTPUT_PATH = ROOT / "data" / "fangraphs" / "fg_hitters.csv"

START_YEAR = 2016  # 2015 dropped: OAA (Statcast fielding) starts in 2016
END_YEAR = 2025


def pull() -> pd.DataFrame:
    """
    Pull one year at a time to avoid large single requests timing out on
    FanGraphs (HTTP 524 Cloudflare gateway timeouts are a known pybaseball
    issue with multi-year pulls).
    """
    frames = []
    for year in range(START_YEAR, END_YEAR + 1):
        print(f"  Pulling {year}...", end=" ", flush=True)
        df_year = pybaseball.batting_stats(year, year, qual=0)
        print(f"{len(df_year):,} players")
        frames.append(df_year)
        time.sleep(1)  # be polite to FanGraphs between requests

    df = pd.concat(frames, ignore_index=True)
    print(f"  Total: {len(df):,} player-seasons across {END_YEAR - START_YEAR + 1} seasons.")
    return df


def validate(df: pd.DataFrame) -> None:
    """Sanity checks per CLAUDE.md data validation spec."""
    seasons = df["Season"].unique()
    rows_per_season = len(df) / len(seasons)
    assert rows_per_season >= 400, f"Unexpectedly few rows/season: {rows_per_season:.0f}"

    fully_null_cols = [c for c in df.columns if df[c].isnull().all()]
    assert not fully_null_cols, f"Fully null columns: {fully_null_cols}"

    assert (df["PA"] >= 0).all(), "Negative PA values found"

    expected = set(range(START_YEAR, END_YEAR + 1))
    missing = expected - set(seasons)
    if missing:
        print(f"  Warning: missing seasons {sorted(missing)}")

    # Check all 30 teams present in each season
    teams_per_season = df.groupby("Season")["Team"].nunique()
    low_team_seasons = teams_per_season[teams_per_season < 30]
    if not low_team_seasons.empty:
        print(f"  Warning: fewer than 30 teams in seasons: {low_team_seasons.to_dict()}")

    print("  Validation passed.")


def main():
    if OUTPUT_PATH.exists() and OUTPUT_PATH.stat().st_size > 0:
        print(f"Cache hit — file already exists, skipping pull.\n  {OUTPUT_PATH}")
        return

    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)

    df = pull()

    # Drop columns that are entirely null across all seasons (e.g. deprecated
    # FanGraphs pitch-type fields like FT% (sc)) — they carry no information.
    null_cols = [c for c in df.columns if df[c].isnull().all()]
    if null_cols:
        print(f"  Dropping {len(null_cols)} fully-null columns: {null_cols}")
        df = df.drop(columns=null_cols)

    validate(df)

    df.to_csv(OUTPUT_PATH, index=False)
    print(f"  Saved to {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
