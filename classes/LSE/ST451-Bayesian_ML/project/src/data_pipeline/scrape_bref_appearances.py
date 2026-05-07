"""
scrape_bref_appearances.py

Scrapes the "Full Season Roster & Appearances" table from Baseball Reference
team-season pages, extracts Games Started (GS) for every player, and fuzzy-joins
the result to data/fangraphs/fg_hitters.csv.

BRef uses accented player names; FanGraphs uses ASCII.  Name matching strips
accents on both sides before comparing, so the fuzzy step only fires for genuine
spelling differences.

Outputs
-------
data/bref_raw/{TEAM}_{YEAR}.csv          individual cached pages (skipped if exists)
data/bref_appearances_raw.csv            merged raw data across all team-seasons
data/bref_appearances.csv               GS aggregated by (Name_bref, Season)
data/bref_dup_names.csv                 BRef: same normalized name, same season,
                                         multiple team rows (traded OR true collision)
data/fg_dup_names.csv                   FG: same normalized name, same season,
                                         multiple IDfg values
data/bref_unmatched.csv                 BRef rows with no FG match above threshold
data/fangraphs/fg_hitters_with_gs.csv   fg_hitters.csv with GS column appended

Usage
-----
# Full run (scrape + join):
python3 src/data_pipeline/scrape_bref_appearances.py

# Skip scraping if raw files already cached:
python3 src/data_pipeline/scrape_bref_appearances.py --skip-scrape

# Limit seasons or adjust rate limit:
python3 src/data_pipeline/scrape_bref_appearances.py --seasons 2022 2024 --delay 6
"""

import argparse
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

FG_SRC       = DATA_DIR / "fangraphs"  / "fg_hitters.csv"
FG_DST       = DATA_DIR / "fangraphs"  / "fg_hitters_with_gs.csv"
BREF_RAW_DIR = DATA_DIR / "bref_raw"
BREF_RAW_CSV = DATA_DIR / "bref_appearances_raw.csv"
BREF_AGG_CSV = DATA_DIR / "bref_appearances.csv"
BREF_DUP_CSV = DATA_DIR / "bref_dup_names.csv"
FG_DUP_CSV   = DATA_DIR / "fg_dup_names.csv"
UNMATCHED_CSV = DATA_DIR / "bref_unmatched.csv"

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

TEAMS = [
    "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET",
    "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK",
    "PHI", "PIT", "SDP", "SEA", "SFG", "STL", "TBR", "TEX", "TOR", "WSN",
]

DEFAULT_SEASONS  = list(range(2016, 2025))
FUZZY_THRESHOLD  = 90     # minimum score to accept a fuzzy match
CF_WAIT_SECS     = 6      # seconds to wait for Cloudflare to clear on each page


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def normalize_name(s: str) -> str:
    """Strip accents, lowercase, strip whitespace.

    BRef stores 'José Buttó'; FanGraphs stores 'Jose Butto'.
    After normalization both become 'jose butto', enabling exact matching.
    """
    nfkd = unicodedata.normalize("NFD", str(s))
    ascii_only = "".join(c for c in nfkd if unicodedata.category(c) != "Mn")
    return ascii_only.lower().strip()


# ---------------------------------------------------------------------------
# Step 1 — Scrape
# ---------------------------------------------------------------------------

def _scrape_one(page, team: str, season: int) -> Optional[pd.DataFrame]:
    url = f"https://www.baseball-reference.com/teams/{team}/{season}.shtml"
    try:
        page.goto(url, timeout=30_000)
        time.sleep(CF_WAIT_SECS)          # let Cloudflare JS challenge resolve
        html = page.content()
        if "Just a moment" in html:       # still blocked
            return None
        tables = pd.read_html(html, attrs={"id": "appearances"})
        if not tables:
            return None
        df = tables[0]
        # BRef inserts repeated header rows where Rk == "Rk" — drop them
        df = df[df["Rk"].astype(str) != "Rk"].copy()
        df = df[["Player", "GS"]].copy()
        df["GS"] = pd.to_numeric(df["GS"], errors="coerce")
        df = df.dropna(subset=["GS", "Player"])
        df["GS"]       = df["GS"].astype(int)
        df["team_bref"] = team
        df["Season"]    = season
        return df
    except Exception as e:
        print(f"      exception: {e}")
        return None


def scrape_all(seasons: list, delay: float = 6.0) -> list:
    """Scrape every team × season combination, caching each to BREF_RAW_DIR."""
    from playwright.sync_api import sync_playwright
    from playwright_stealth import Stealth

    BREF_RAW_DIR.mkdir(parents=True, exist_ok=True)
    total  = len(TEAMS) * len(seasons)
    done   = 0
    failed = []

    with sync_playwright() as p:
        browser = p.chromium.launch(
            headless=True,
            args=["--disable-blink-features=AutomationControlled", "--no-sandbox"],
        )
        context = browser.new_context(
            user_agent=(
                "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                "AppleWebKit/537.36 (KHTML, like Gecko) "
                "Chrome/124.0.0.0 Safari/537.36"
            ),
            viewport={"width": 1280, "height": 800},
        )
        Stealth().apply_stealth_sync(context)
        page = context.new_page()

        for season in seasons:
            for team in TEAMS:
                done += 1
                cache_file = BREF_RAW_DIR / f"{team}_{season}.csv"

                if cache_file.exists():
                    print(f"  [{done:>4}/{total}] {team} {season} — cached")
                    time.sleep(0.1)
                    continue

                df = _scrape_one(page, team, season)
                if df is not None and len(df) > 0:
                    df.to_csv(cache_file, index=False)
                    print(f"  [{done:>4}/{total}] {team} {season} — {len(df)} rows")
                else:
                    failed.append((team, season))
                    print(f"  [{done:>4}/{total}] {team} {season} — FAILED")

                time.sleep(delay)

        browser.close()

    if failed:
        print(f"\nFailed ({len(failed)} pages):")
        for t, s in failed:
            print(f"  {t} {s}")

    return failed


def merge_raw() -> pd.DataFrame:
    files = sorted(BREF_RAW_DIR.glob("*.csv"))
    if not files:
        raise FileNotFoundError(
            f"No cached CSVs in {BREF_RAW_DIR}. "
            "Run without --skip-scrape to fetch data first."
        )
    raw = pd.concat([pd.read_csv(f) for f in files], ignore_index=True)
    raw.to_csv(BREF_RAW_CSV, index=False)
    print(f"Merged {len(files)} files → {BREF_RAW_CSV}  ({len(raw)} rows)")
    return raw


# ---------------------------------------------------------------------------
# Step 2 — Aggregate GS by (Player, Season)
# ---------------------------------------------------------------------------

def aggregate_gs(raw: pd.DataFrame) -> pd.DataFrame:
    """Sum GS across all teams a player appeared on in a given season."""
    agg = (
        raw.groupby(["Player", "Season"], as_index=False)["GS"]
        .sum()
        .rename(columns={"Player": "Name_bref"})
    )
    agg.to_csv(BREF_AGG_CSV, index=False)
    print(f"Aggregated BRef GS → {BREF_AGG_CSV}  ({len(agg)} rows)")
    return agg


# ---------------------------------------------------------------------------
# Step 3 — Duplicate-name reports
# ---------------------------------------------------------------------------

def report_duplicates(fg: pd.DataFrame, raw: pd.DataFrame) -> None:
    """
    Print and save two duplicate-name reports.

    FG duplicates  — same normalized name in same season maps to 2+ IDfg values.
                     These are genuinely different players who share a name;
                     fuzzy matching cannot resolve them without manual disambiguation.

    BRef duplicates — same normalized player name appears on 2+ team pages in the
                      same season.  This includes traded players (correct to sum GS)
                      AND true name collisions (GS would be summed incorrectly).
                      Review: if total GS > ~162 it is almost certainly a collision.
    """

    # --- FG ---
    fg["_norm"] = fg["Name"].apply(normalize_name)
    fg_counts = (
        fg.groupby(["_norm", "Season"])["IDfg"]
        .nunique()
        .reset_index(name="n_ids")
    )
    fg_multi = fg_counts[fg_counts["n_ids"] > 1]
    if len(fg_multi):
        fg_dups = (
            fg_multi[["_norm", "Season"]]
            .merge(fg[["IDfg", "Name", "Season", "_norm"]], on=["_norm", "Season"])
            .drop(columns="_norm")
            .sort_values(["Name", "Season"])
        )
    else:
        fg_dups = pd.DataFrame(columns=["IDfg", "Name", "Season"])

    fg_dups.to_csv(FG_DUP_CSV, index=False)
    print(f"\n--- FG duplicate names ({len(fg_dups)} rows, {len(fg_multi)} name-season combos) ---")
    if len(fg_dups):
        print(fg_dups.to_string(index=False))
    else:
        print("  None found.")
    print(f"  Saved → {FG_DUP_CSV}")

    # --- BRef ---
    raw["_norm"] = raw["Player"].apply(normalize_name)
    bref_counts = (
        raw.groupby(["_norm", "Season"])["team_bref"]
        .nunique()
        .reset_index(name="n_teams")
    )
    bref_multi = bref_counts[bref_counts["n_teams"] > 1]
    if len(bref_multi):
        bref_dups = (
            bref_multi[["_norm", "Season"]]
            .merge(
                raw[["Player", "Season", "team_bref", "GS", "_norm"]],
                on=["_norm", "Season"],
            )
            .drop(columns="_norm")
            .sort_values(["Player", "Season", "team_bref"])
        )
    else:
        bref_dups = pd.DataFrame(columns=["Player", "Season", "team_bref", "GS"])

    bref_dups.to_csv(BREF_DUP_CSV, index=False)
    print(f"\n--- BRef multi-team name-seasons ({len(bref_dups)} rows, {len(bref_multi)} name-season combos) ---")
    print("  Traded players show here (GS sum is correct).")
    print("  If total GS across teams > ~162, it may be a true name collision.")
    if len(bref_dups):
        # Show totals alongside detail for quick review
        totals = bref_dups.groupby(["Player", "Season"])["GS"].sum().reset_index(name="GS_total")
        display = bref_dups.merge(totals, on=["Player", "Season"]).sort_values(
            ["Player", "Season", "team_bref"]
        )
        print(display.to_string(index=False))
    else:
        print("  None found.")
    print(f"  Saved → {BREF_DUP_CSV}")


# ---------------------------------------------------------------------------
# Step 4 — Fuzzy join to FG
# ---------------------------------------------------------------------------

def fuzzy_join(
    fg: pd.DataFrame,
    bref_agg: pd.DataFrame,
    threshold: int = FUZZY_THRESHOLD,
) -> pd.DataFrame:
    """
    Match each (Name_bref, Season) row in bref_agg to an IDfg in fg.

    Strategy
    --------
    1. Normalize accents on both sides.
    2. Exact match on (norm_name, Season) — covers ~95 %+ of rows.
    3. Fuzzy match (token_sort_ratio) within the same season for the rest.
    4. Rows below threshold or with ambiguous exact matches are left unmatched
       and written to UNMATCHED_CSV for manual review.

    Returns fg with a 'GS' column appended (NaN for players absent in BRef).
    """
    fg       = fg.copy()
    bref_agg = bref_agg.copy()

    fg["_norm"]       = fg["Name"].apply(normalize_name)
    bref_agg["_norm"] = bref_agg["Name_bref"].apply(normalize_name)

    # Per-season lookup: norm_name → list[IDfg]
    from collections import defaultdict
    season_lookup = defaultdict(dict)  # season → {norm_name: [IDfg, ...]}
    for _, row in fg.iterrows():
        season_lookup[row["Season"]].setdefault(row["_norm"], []).append(row["IDfg"])

    matched_rows  = []   # {IDfg, Season, GS, match_score, match_type}
    unmatched_rows = []  # rows that couldn't be resolved

    for _, row in bref_agg.iterrows():
        season    = row["Season"]
        norm      = row["_norm"]
        gs        = row["GS"]
        name_bref = row["Name_bref"]

        lookup = season_lookup.get(season, {})

        if norm in lookup:
            id_list = lookup[norm]
            if len(id_list) == 1:
                matched_rows.append({
                    "IDfg": id_list[0], "Season": season, "GS": gs,
                    "Name_bref": name_bref, "match_score": 100, "match_type": "exact",
                })
            else:
                # Exact name match but multiple IDfg — ambiguous without manual fix
                unmatched_rows.append({
                    "Name_bref": name_bref, "Season": season, "GS": gs,
                    "reason": "ambiguous_exact",
                    "IDfg_candidates": str(id_list),
                    "match_score": 100,
                })
        else:
            if not lookup:
                unmatched_rows.append({
                    "Name_bref": name_bref, "Season": season, "GS": gs,
                    "reason": "season_not_in_fg", "IDfg_candidates": "",
                    "match_score": None,
                })
                continue

            result = fuzz_process.extractOne(
                norm,
                list(lookup.keys()),
                scorer=fuzz.token_sort_ratio,
                score_cutoff=threshold,
            )
            if result:
                match_norm, score = result[0], result[1]
                id_list = lookup[match_norm]
                if len(id_list) == 1:
                    matched_rows.append({
                        "IDfg": id_list[0], "Season": season, "GS": gs,
                        "Name_bref": name_bref, "match_score": score, "match_type": "fuzzy",
                    })
                else:
                    unmatched_rows.append({
                        "Name_bref": name_bref, "Season": season, "GS": gs,
                        "reason": "fuzzy_ambiguous",
                        "IDfg_candidates": str(id_list),
                        "match_score": score,
                    })
            else:
                unmatched_rows.append({
                    "Name_bref": name_bref, "Season": season, "GS": gs,
                    "reason": "no_match_above_threshold",
                    "IDfg_candidates": "", "match_score": None,
                })

    # Merge GS onto FG by (IDfg, Season)
    match_df = pd.DataFrame(matched_rows)
    if len(match_df):
        match_df["IDfg"] = match_df["IDfg"].astype(int)
        fg_with_gs = fg.drop(columns=["_norm"]).merge(
            match_df[["IDfg", "Season", "GS"]],
            on=["IDfg", "Season"],
            how="left",
        )
    else:
        fg_with_gs = fg.drop(columns=["_norm"])
        fg_with_gs["GS"] = pd.NA

    # Save unmatched for review
    unmatched_df = pd.DataFrame(unmatched_rows)
    unmatched_df.to_csv(UNMATCHED_CSV, index=False)

    # Summary
    n_exact  = sum(1 for r in matched_rows if r["match_type"] == "exact")
    n_fuzzy  = sum(1 for r in matched_rows if r["match_type"] == "fuzzy")
    n_miss   = len(unmatched_rows)
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
        print(
            fuzzy_df[["Name_bref", "fg_name", "Season", "match_score", "GS"]]
            .sort_values("match_score")
            .to_string(index=False)
        )

    covered_fg = fg_with_gs["GS"].notna().sum()
    total_fg   = len(fg_with_gs)
    print(f"\n  FG rows with GS:  {covered_fg} / {total_fg}  ({100*covered_fg/total_fg:.1f} %)")

    return fg_with_gs


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def parse_args():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument(
        "--seasons", nargs=2, type=int, metavar=("START", "END"),
        default=[DEFAULT_SEASONS[0], DEFAULT_SEASONS[-1]],
        help="Inclusive season range, e.g. --seasons 2016 2024",
    )
    p.add_argument(
        "--delay", type=float, default=6.0,
        help="Seconds between page requests (default 6)",
    )
    p.add_argument(
        "--skip-scrape", action="store_true",
        help="Skip scraping and use cached files in data/bref_raw/",
    )
    p.add_argument(
        "--threshold", type=int, default=FUZZY_THRESHOLD,
        help=f"Minimum fuzzy-match score to accept (default {FUZZY_THRESHOLD})",
    )
    return p.parse_args()


def main():
    args   = parse_args()
    seasons = list(range(args.seasons[0], args.seasons[1] + 1))

    print(f"Seasons: {seasons[0]}–{seasons[-1]}  ({len(seasons)} years, {len(TEAMS)} teams = "
          f"{len(seasons)*len(TEAMS)} pages)")

    # Step 1 — scrape
    if not args.skip_scrape:
        print("\n=== Step 1: Scraping BRef ===")
        scrape_all(seasons, delay=args.delay)
    else:
        print("\nSkipping scrape (--skip-scrape set).")

    # Step 2 — merge raw and aggregate
    print("\n=== Step 2: Merge raw + aggregate GS ===")
    raw      = merge_raw()
    bref_agg = aggregate_gs(raw)

    # Step 3 — duplicate name reports (BEFORE join so user can review)
    print("\n=== Step 3: Duplicate name reports ===")
    fg = pd.read_csv(FG_SRC)
    report_duplicates(fg, raw)

    # Step 4 — fuzzy join
    print("\n=== Step 4: Fuzzy join to FG ===")
    fg = pd.read_csv(FG_SRC)   # reload without the _norm column added in report_duplicates
    fg_with_gs = fuzzy_join(fg, bref_agg, threshold=args.threshold)

    fg_with_gs.to_csv(FG_DST, index=False)
    print(f"\nSaved → {FG_DST}")
    print("To rebuild train/test/validation splits, re-run src/data_pipeline/clean_fg_hitters.py"
          " after pointing SRC at fg_hitters_with_gs.csv.")


if __name__ == "__main__":
    main()
