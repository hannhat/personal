"""
pull_fielding_stats.py

Pulls FanGraphs season-level fielding stats (2016–2025) for all players,
pivots to one row per player-season, and merges positional columns into
fg_hitters.csv.

Also drops any 2015 rows from fg_hitters.csv, since OAA
(Statcast-based) only begins in 2016 and we want consistent metric coverage.

Positional columns added per position (C, 1B, 2B, 3B, SS, LF, CF, RF, P):
  G_{pos}   — games played at that position
  Inn_{pos} — innings played at that position

Defensive value columns added per position:
  OAA_{pos} — Outs Above Average at that position
              (null for C and P — OAA does not cover catchers or pitchers)
  FRM       — catcher framing runs (single column; only populated for catchers;
              pitcher FRM is an artifact and is ignored)

Caching:
  Raw fielding data is pulled year by year but not saved to disk — the full
  merged fg_hitters.csv is the canonical output and serves as the cache.
  Re-run the script to refresh fielding data from FanGraphs.
"""

import pathlib
import time
import pandas as pd
import pybaseball

pybaseball.cache.enable()

# ── Paths ──────────────────────────────────────────────────────────────────
ROOT = pathlib.Path(__file__).resolve().parents[2]
FIELDING_DIR = ROOT / "data" / "fangraphs"
BATTING_PATH = ROOT / "data" / "fangraphs" / "fg_hitters.csv"

START_YEAR = 2016
END_YEAR   = 2025

POSITIONS = ["C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "P"]


# ── Pull ───────────────────────────────────────────────────────────────────

def pull_year(year: int) -> pd.DataFrame:
    """Pull raw FanGraphs fielding stats for one season."""
    print(f"  {year}: pulling...", end=" ", flush=True)
    df = pybaseball.fielding_stats(year, year, qual=0)
    print(f"{len(df):,} rows")
    time.sleep(1)  # be polite to FanGraphs
    return df


def pull_all() -> pd.DataFrame:
    """Pull all years and concatenate raw fielding data."""
    frames = []
    for year in range(START_YEAR, END_YEAR + 1):
        frames.append(pull_year(year))
    return pd.concat(frames, ignore_index=True)


# ── Pivot ──────────────────────────────────────────────────────────────────

def pivot_to_wide(df_raw: pd.DataFrame) -> pd.DataFrame:
    """
    Convert one-row-per-player-position-season to one-row-per-player-season.

    For each position in POSITIONS we extract G, Inn, and OAA.
    FRM is pulled from catcher rows only (single column, not per-position).
    """
    frames = []

    for pos in POSITIONS:
        pos_df = df_raw[df_raw["Pos"] == pos][["IDfg", "Season", "G", "Inn", "OAA"]].copy()
        pos_df = pos_df.rename(columns={
            "G":   f"G_{pos}",
            "Inn": f"Inn_{pos}",
            "OAA": f"OAA_{pos}",
        })
        frames.append(pos_df)

    # Merge all positions on IDfg + Season (outer so players who played
    # only some positions don't lose rows).
    wide = frames[0]
    for pos_df in frames[1:]:
        wide = wide.merge(pos_df, on=["IDfg", "Season"], how="outer")

    # FRM: catchers only. Pitcher FRM is a framing-credit artifact, not
    # the catcher's own framing value — exclude it.
    frm = (
        df_raw[df_raw["Pos"] == "C"][["IDfg", "Season", "FRM"]]
        .rename(columns={"FRM": "FRM"})
    )
    wide = wide.merge(frm, on=["IDfg", "Season"], how="left")

    return wide


# ── Merge into batting stats ───────────────────────────────────────────────

def merge_into_batting(wide: pd.DataFrame) -> None:
    """
    Load fg_hitters.csv, drop 2015 rows, left-join positional
    columns, and overwrite the file.
    """
    batting = pd.read_csv(BATTING_PATH)
    original_rows = len(batting)

    # Drop 2015 — OAA unavailable, and START_YEAR in pull_season_batting.py
    # has been updated to 2016 so future re-pulls will be consistent.
    batting = batting[batting["Season"] != 2015].copy()
    dropped = original_rows - len(batting)
    if dropped:
        print(f"  Dropped {dropped:,} rows from 2015.")

    # IDfg is stored as int in fielding data but may be float in batting CSV
    # after a round-trip through CSV. Align types before joining.
    batting["IDfg"] = batting["IDfg"].astype("Int64")
    wide["IDfg"]    = wide["IDfg"].astype("Int64")

    before_cols = len(batting.columns)
    batting = batting.merge(wide, on=["IDfg", "Season"], how="left")
    added_cols = len(batting.columns) - before_cols
    print(f"  Added {added_cols} positional columns.")

    # Primary position: the position with the most innings in the season.
    # idxmax returns the column name (e.g. "Inn_SS"); strip the prefix to get
    # the position string. Rows with no fielding data (pure DHs) stay null.
    inn_cols = [f"Inn_{p}" for p in POSITIONS]
    batting["primary_pos"] = (
        batting[inn_cols]
        .idxmax(axis=1)                        # e.g. "Inn_SS"
        .where(batting[inn_cols].notna().any(axis=1))  # null if all Inn are NaN
        .str.replace("Inn_", "", regex=False)  # e.g. "SS"
    )

    print(f"  Final shape: {batting.shape}")

    batting.to_csv(BATTING_PATH, index=False)
    print(f"  Saved to {BATTING_PATH}")


# ── Validation ────────────────────────────────────────────────────────────

def validate(batting: pd.DataFrame) -> None:
    """Basic sanity checks on the merged output."""
    assert 2015 not in batting["Season"].values, "2015 data still present"

    pos_g_cols = [f"G_{p}" for p in POSITIONS]
    for col in pos_g_cols:
        assert col in batting.columns, f"Missing column: {col}"
        # Games at a position must be non-negative where populated
        assert (batting[col].dropna() >= 0).all(), f"Negative values in {col}"

    assert "FRM" in batting.columns, "FRM column missing"
    assert "primary_pos" in batting.columns, "primary_pos column missing"
    assert batting["primary_pos"].isin([*POSITIONS, None]).all(), \
        "Unexpected value in primary_pos"
    print("  Validation passed.")


# ── Main ──────────────────────────────────────────────────────────────────

def main():
    FIELDING_DIR.mkdir(parents=True, exist_ok=True)

    print("Pulling FanGraphs fielding stats (2016–2025)...")
    df_raw = pull_all()

    print("Pivoting to wide format...")
    wide = pivot_to_wide(df_raw)
    print(f"  Wide fielding table: {wide.shape}")

    print("Merging into fg_hitters.csv...")
    merge_into_batting(wide)

    batting = pd.read_csv(BATTING_PATH)
    validate(batting)
    print("Done.")


if __name__ == "__main__":
    main()
