"""
Split fg_hitters.csv into train/test/validation sets and filter out pitchers.

Train:      2016–2023
Test:       2024
Validation: 2025

Pitchers (primary_pos == 'P') are excluded, except Shohei Ohtani (IDfg 19755).
"""

import pandas as pd
from pathlib import Path

OHTANI_IDFG = 19755

SRC = Path(__file__).resolve().parents[2] / "data" / "fangraphs" / "fg_hitters_with_gs.csv"
DST = Path(__file__).resolve().parents[2] / "data" / "fg_builds"

SPLITS = {
    "fg_hitters_train.csv":      range(2016, 2024),
    "fg_hitters_test.csv":       [2024],
    "fg_hitters_validation.csv": [2025],
}


def main():
    df = pd.read_csv(SRC)

    # Remove pitchers, keep Ohtani
    is_pitcher = df["primary_pos"] == "P"
    is_ohtani = df["IDfg"] == OHTANI_IDFG
    df = df[~is_pitcher | is_ohtani].copy()

    DST.mkdir(parents=True, exist_ok=True)

    for filename, seasons in SPLITS.items():
        subset = df[df["Season"].isin(seasons)]
        out = DST / filename
        subset.to_csv(out, index=False)
        print(f"{filename}: {len(subset)} rows, {subset['IDfg'].nunique()} players, "
              f"seasons {sorted(subset['Season'].unique())}")


if __name__ == "__main__":
    main()
