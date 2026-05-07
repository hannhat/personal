"""
scrape_bref_standard_batting.py

Scrapes the "Standard Batting" league table from Baseball Reference for each
season 2016-2025, extracts team_start and team_end per player-season, and
joins the result to data/fangraphs/fg_hitters_with_gs.csv.

BRef table layout for traded players (2TM row is always first):
    Player      Team
    Luis Arraez  2TM   ← total row; IGNORED
    Luis Arraez  MIA   ← first team (started season here)  → team_start
    Luis Arraez  SDP   ← last team  (ended  season here)  → team_end

Single-team players have one row: team_start == team_end.

Outputs
-------
data/bref_standard_batting.csv       cleaned player-season table with team_start/team_end
data/bref_combined.csv               merged with bref_appearances.csv (GS + team cols)
data/bref_sb_unmatched.csv           rows that could not be matched to fg_hitters_with_gs
data/fangraphs/fg_hitters_with_gs.csv  updated in-place with team_start, team_end columns

Usage
-----
python3 src/data_pipeline/scrape_bref_standard_batting.py
python3 src/data_pipeline/scrape_bref_standard_batting.py --skip-scrape
"""

import argparse
import io
import re
import time
import unicodedata
from pathlib import Path
from typing import Optional

import pandas as pd
from thefuzz import fuzz, process as fuzz_process

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

ROOT     = Path(__file__).resolve().parents[2]
DATA_DIR = ROOT / "data"

FG_PATH      = DATA_DIR / "fangraphs" / "fg_hitters_with_gs.csv"
BREF_APP_CSV = DATA_DIR / "bref_appearances.csv"
SB_CSV       = DATA_DIR / "bref_standard_batting.csv"
COMBINED_CSV = DATA_DIR / "bref_combined.csv"
UNMATCHED_CSV = DATA_DIR / "bref_sb_unmatched.csv"

SEASONS        = list(range(2016, 2026))
FUZZY_THRESHOLD = 90
CF_WAIT_SECS    = 8

# Regex to strip BRef footnote markers (* = HOF/All-Star, # = other)
_FOOTNOTE_RE = re.compile(r'[*#]+')

# Multi-team total rows (2TM, 3TM, 4TM, ...)
_MULTI_TEAM_RE = re.compile(r'^\d+TM$')


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def normalize_name(s: str) -> str:
    """Strip accents, footnote markers, lowercase, strip whitespace."""
    s = _FOOTNOTE_RE.sub("", str(s))
    nfkd = unicodedata.normalize("NFD", s)
    ascii_only = "".join(c for c in nfkd if unicodedata.category(c) != "Mn")
    return ascii_only.lower().strip()


def clean_player_name(s: str) -> str:
    """Remove BRef footnote markers only — keep original casing and accents."""
    return _FOOTNOTE_RE.sub("", str(s)).strip()


# ---------------------------------------------------------------------------
# Step 1 — Scrape
# ---------------------------------------------------------------------------

def scrape_season(page, season: int) -> Optional[pd.DataFrame]:
    url = f"https://www.baseball-reference.com/leagues/majors/{season}-standard-batting.shtml"
    try:
        page.goto(url, timeout=30_000)
        time.sleep(CF_WAIT_SECS)
        html = page.content()
        if "Just a moment" in html:
            return None
        tables = pd.read_html(io.StringIO(html), attrs={"id": "players_standard_batting"})
        if not tables:
            return None
        df = tables[0]
        # Drop repeated header rows
        df = df[df["Rk"].astype(str) != "Rk"].copy()
        df = df[["Player", "Team"]].copy()
        df["Season"] = season
        return df
    except Exception as e:
        print(f"  exception ({season}): {e}")
        return None


def scrape_all(seasons: list) -> pd.DataFrame:
    from playwright.sync_api import sync_playwright
    from playwright_stealth import Stealth

    frames = []

    with sync_playwright() as p:
        browser = p.chromium.launch(
            headless=True,
            args=["--disable-blink-features=AutomationControlled", "--no-sandbox"],
        )
        ctx = browser.new_context(
            user_agent=(
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/124.0.0.0 Safari/537.36"
            ),
            viewport={"width": 1280, "height": 800},
        )
        Stealth().apply_stealth_sync(ctx)
        page = ctx.new_page()

        for season in seasons:
            df = scrape_season(page, season)
            if df is not None and len(df) > 0:
                frames.append(df)
                print(f"  {season}: {len(df)} rows")
            else:
                print(f"  {season}: FAILED")
            time.sleep(4)   # polite delay between pages

        browser.close()

    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


# ---------------------------------------------------------------------------
# Step 2 — Aggregate to player-season level
# ---------------------------------------------------------------------------

def build_team_cols(raw: pd.DataFrame) -> pd.DataFrame:
    """
    For each (Player, Season):
      - Drop rows where Team matches /^\\d+TM$/ (2TM, 3TM, ...)
      - team_start = Team on first remaining row (chronologically first)
      - team_end   = Team on last  remaining row (chronologically last)
      - Clean Player column of footnote markers
    """
    # Filter out multi-team total rows
    raw = raw[~raw["Team"].astype(str).str.match(_MULTI_TEAM_RE)].copy()

    # Preserve original table order with a sequential key
    raw = raw.reset_index(drop=True)
    raw["_order"] = raw.index

    agg = (
        raw.groupby(["Player", "Season"], sort=False)
        .agg(
            team_start=("Team", "first"),
            team_end=("Team",   "last"),
        )
        .reset_index()
    )

    # Clean player name (strip *, #) — keep accents; normalization happens at join time
    agg["Name_bref"] = agg["Player"].apply(clean_player_name)
    agg = agg.drop(columns="Player")

    return agg[["Name_bref", "Season", "team_start", "team_end"]]


# ---------------------------------------------------------------------------
# Step 3 — Merge with bref_appearances.csv
# ---------------------------------------------------------------------------

def merge_with_appearances(sb: pd.DataFrame) -> pd.DataFrame:
    """
    Left-join standard batting (team cols) with bref_appearances (GS) on
    normalized (Name_bref, Season).  Returns the combined BRef table.
    """
    if not BREF_APP_CSV.exists():
        print(f"  Warning: {BREF_APP_CSV} not found — skipping appearance merge.")
        return sb

    app = pd.read_csv(BREF_APP_CSV)   # columns: Name_bref, Season, GS

    sb["_norm"]  = sb["Name_bref"].apply(normalize_name)
    app["_norm"] = app["Name_bref"].apply(normalize_name)

    merged = sb.merge(
        app[["_norm", "Season", "GS"]],
        on=["_norm", "Season"],
        how="left",
    ).drop(columns="_norm")

    n_matched = merged["GS"].notna().sum()
    print(f"  Appearance merge: {n_matched}/{len(merged)} rows have GS from appearances table")
    return merged


# ---------------------------------------------------------------------------
# Step 4 — Fuzzy join to fg_hitters_with_gs
# ---------------------------------------------------------------------------

def fuzzy_join(fg: pd.DataFrame, sb: pd.DataFrame, threshold: int = FUZZY_THRESHOLD) -> pd.DataFrame:
    """
    Add team_start and team_end to fg by matching (normalized Name, Season).
    Existing columns (including GS and manual edits) are never overwritten.
    """
    fg  = fg.copy()
    sb  = sb.copy()

    fg["_norm"] = fg["Name"].apply(normalize_name)
    sb["_norm"] = sb["Name_bref"].apply(normalize_name)

    # Per-season FG lookup: norm_name → list of IDfg
    from collections import defaultdict
    season_lookup = defaultdict(dict)
    for _, row in fg.iterrows():
        season_lookup[row["Season"]].setdefault(row["_norm"], []).append(row["IDfg"])

    matched_rows  = []
    unmatched_rows = []

    for _, row in sb.iterrows():
        season     = row["Season"]
        norm       = row["_norm"]
        name_bref  = row["Name_bref"]
        team_start = row["team_start"]
        team_end   = row["team_end"]

        lookup = season_lookup.get(season, {})

        if norm in lookup:
            id_list = lookup[norm]
            if len(id_list) == 1:
                matched_rows.append({
                    "IDfg": id_list[0], "Season": season,
                    "team_start": team_start, "team_end": team_end,
                    "Name_bref": name_bref, "match_score": 100, "match_type": "exact",
                })
            else:
                unmatched_rows.append({
                    "Name_bref": name_bref, "Season": season,
                    "team_start": team_start, "team_end": team_end,
                    "reason": "ambiguous_exact", "IDfg_candidates": str(id_list),
                    "match_score": 100,
                })
        else:
            if not lookup:
                unmatched_rows.append({
                    "Name_bref": name_bref, "Season": season,
                    "team_start": team_start, "team_end": team_end,
                    "reason": "season_not_in_fg", "IDfg_candidates": "",
                    "match_score": None,
                })
                continue

            result = fuzz_process.extractOne(
                norm, list(lookup.keys()),
                scorer=fuzz.token_sort_ratio,
                score_cutoff=threshold,
            )
            if result:
                match_norm, score = result[0], result[1]
                id_list = lookup[match_norm]
                if len(id_list) == 1:
                    matched_rows.append({
                        "IDfg": id_list[0], "Season": season,
                        "team_start": team_start, "team_end": team_end,
                        "Name_bref": name_bref, "match_score": score, "match_type": "fuzzy",
                    })
                else:
                    unmatched_rows.append({
                        "Name_bref": name_bref, "Season": season,
                        "team_start": team_start, "team_end": team_end,
                        "reason": "fuzzy_ambiguous", "IDfg_candidates": str(id_list),
                        "match_score": score,
                    })
            else:
                unmatched_rows.append({
                    "Name_bref": name_bref, "Season": season,
                    "team_start": team_start, "team_end": team_end,
                    "reason": "no_match_above_threshold", "IDfg_candidates": "",
                    "match_score": None,
                })

    # Build team cols df keyed by (IDfg, Season)
    match_df = pd.DataFrame(matched_rows)
    if len(match_df):
        match_df["IDfg"] = match_df["IDfg"].astype(int)
        fg_out = fg.drop(columns=["_norm"]).merge(
            match_df[["IDfg", "Season", "team_start", "team_end"]],
            on=["IDfg", "Season"],
            how="left",
        )
    else:
        fg_out = fg.drop(columns=["_norm"])
        fg_out["team_start"] = pd.NA
        fg_out["team_end"]   = pd.NA

    # Save unmatched
    pd.DataFrame(unmatched_rows).to_csv(UNMATCHED_CSV, index=False)

    # Summary
    n_exact = sum(1 for r in matched_rows if r["match_type"] == "exact")
    n_fuzzy = sum(1 for r in matched_rows if r["match_type"] == "fuzzy")
    n_miss  = len(unmatched_rows)
    print(f"\n--- Join summary ---")
    print(f"  Exact matches:  {n_exact:>5}")
    print(f"  Fuzzy matches:  {n_fuzzy:>5}")
    print(f"  Unmatched:      {n_miss:>5}  → {UNMATCHED_CSV}")

    if n_fuzzy:
        print("\n  Fuzzy-matched pairs (review for errors):")
        fuzzy_df = (
            pd.DataFrame([r for r in matched_rows if r["match_type"] == "fuzzy"])
            .merge(fg[["IDfg", "Name"]].drop_duplicates(), on="IDfg")
            .rename(columns={"Name": "fg_name"})
        )
        print(fuzzy_df[["Name_bref", "fg_name", "Season", "match_score"]].sort_values("match_score").to_string(index=False))

    covered = fg_out["team_start"].notna().sum()
    print(f"\n  FG rows with team_start: {covered} / {len(fg_out)} ({100*covered/len(fg_out):.1f}%)")

    return fg_out


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def parse_args():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--skip-scrape", action="store_true",
                   help="Use cached bref_standard_batting.csv instead of scraping")
    p.add_argument("--threshold", type=int, default=FUZZY_THRESHOLD,
                   help=f"Minimum fuzzy-match score (default {FUZZY_THRESHOLD})")
    return p.parse_args()


def main():
    args = parse_args()

    # Step 1 — scrape (or load cache)
    if args.skip_scrape and SB_CSV.exists():
        print("Loading cached standard batting data...")
        raw = pd.read_csv(SB_CSV)
        # raw here is already the aggregated table; rebuild from scratch if needed
        print(f"  {len(raw)} rows loaded from {SB_CSV}")
        sb = raw  # already aggregated if cached at the sb level
    else:
        print("=== Step 1: Scraping BRef standard batting ===")
        raw = scrape_all(SEASONS)
        print(f"Scraped {len(raw)} total rows across {len(SEASONS)} seasons")

        # Step 2 — aggregate to player-season
        print("\n=== Step 2: Aggregate to player-season level ===")
        sb = build_team_cols(raw)
        sb.to_csv(SB_CSV, index=False)
        print(f"Saved {len(sb)} player-seasons → {SB_CSV}")

    # Step 3 — merge with bref_appearances
    print("\n=== Step 3: Merge with bref_appearances ===")
    combined = merge_with_appearances(sb)
    combined.to_csv(COMBINED_CSV, index=False)
    print(f"Saved combined BRef table → {COMBINED_CSV}")

    # Step 4 — fuzzy join team cols to fg_hitters_with_gs
    print("\n=== Step 4: Join team_start / team_end to fg_hitters_with_gs ===")
    fg = pd.read_csv(FG_PATH)

    # Drop old team cols if rerunning
    for col in ["team_start", "team_end"]:
        if col in fg.columns:
            fg = fg.drop(columns=col)

    fg_out = fuzzy_join(fg, sb, threshold=args.threshold)
    fg_out.to_csv(FG_PATH, index=False)
    print(f"\nSaved → {FG_PATH}")


if __name__ == "__main__":
    main()
