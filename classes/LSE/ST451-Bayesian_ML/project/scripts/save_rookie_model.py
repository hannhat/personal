"""
Fits rookie Model A (wRC+ + Age) from rookie_projections.ipynb and saves
trace + meta to data/model_runs/rookie_projections/.
Run once before simulation.ipynb.
"""
import sys
sys.path.insert(0, '..')

import numpy as np
import pandas as pd
import pymc as pm
import arviz as az
import scipy as sp
import pickle
from pathlib import Path

# ── Locate project root ───────────────────────────────────────────────────────
data_path = Path(__file__).resolve().parent.parent
while not (data_path / "data" / "fangraphs" / "fg_hitters.csv").exists() and data_path != data_path.parent:
    data_path = data_path.parent
DATA_DIR = data_path
print(f"DATA_DIR = {DATA_DIR}")

# ── Load data ─────────────────────────────────────────────────────────────────
train_df = pd.read_csv(DATA_DIR / "data" / "fg_builds" / "fg_hitters_train.csv")
rookies  = train_df[train_df["rookie"] == 1].copy()
print(f"Rookie player-seasons in training data: {len(rookies):,}")

# ── Build model dataset ───────────────────────────────────────────────────────
ROLE_ORDER = ['bench', 'platoon', 'starter']

rook_model = (
    rookies
    .dropna(subset=["wRC+", "Age", "role"])
    .query("role in @ROLE_ORDER")
    .copy()
    .reset_index(drop=True)
)
rook_model["role_code"] = rook_model["role"].map(
    {r: i for i, r in enumerate(ROLE_ORDER)}
)
print(f"Modeling rows: {len(rook_model):,}")
print("Role distribution:")
print(rook_model["role"].value_counts().reindex(ROLE_ORDER))


def fit_rookie_model(df, feature_cols, label="model"):
    K    = len(ROLE_ORDER)
    work = df.dropna(subset=feature_cols + ["role_code"]).reset_index(drop=True)
    print(f"\n=== {label} ({len(work):,} obs) ===")
    print(f"Features: {feature_cols}")

    X = work[feature_cols].copy()
    scaler = {}
    for col in feature_cols:
        if work[col].nunique() > 2:
            mu, sigma = work[col].mean(), work[col].std()
            if sigma > 0:
                X[col] = (X[col] - mu) / sigma
                scaler[col] = {"mean": float(mu), "std": float(sigma)}

    X_np = X.to_numpy(dtype=float)
    y_np = work["role_code"].to_numpy(dtype=int)

    with pm.Model() as model:
        X_data = pm.Data("X", X_np, mutable=True)
        y_data = pm.Data("y", y_np, mutable=True)
        beta = pm.Normal("beta", mu=0, sigma=2.5, shape=X_np.shape[1])
        cutpoints = pm.Normal(
            "cutpoints", mu=0, sigma=5, shape=K - 1,
            transform=pm.distributions.transforms.ordered,
            initval=np.linspace(-1.5, 1.5, K - 1),
        )
        eta = pm.math.dot(X_data, beta)
        pm.OrderedLogistic("role", eta=eta, cutpoints=cutpoints, observed=y_data)

    with model:
        trace = pm.sample(
            draws=1000, tune=1000, target_accept=0.9,
            random_seed=42, nuts_sampler="numpyro", progressbar=True,
        )

    summary = az.summary(trace, var_names=["beta", "cutpoints"], round_to=3)
    summary.index = (
        [f"beta[{c}]" for c in feature_cols]
        + [f"cutpoint[{ROLE_ORDER[i]}/{ROLE_ORDER[i+1]}]" for i in range(K - 1)]
    )
    print(summary.to_string())

    meta = {
        "label": label,
        "feature_cols": feature_cols,
        "scaler": scaler,
        "role_order": ROLE_ORDER,
        "summary": summary,
        "n_obs": len(work),
    }
    return trace, meta


# ── Fit Model A ───────────────────────────────────────────────────────────────
trace_a, meta_a = fit_rookie_model(rook_model, ["wRC+", "Age"], label="Model A (wRC+ + Age)")

# ── Save ──────────────────────────────────────────────────────────────────────
save_dir = DATA_DIR / "data" / "model_runs" / "rookie_projections"
save_dir.mkdir(parents=True, exist_ok=True)

with open(save_dir / "rookie_model_a_trace.pkl", "wb") as f:
    pickle.dump(trace_a, f)
with open(save_dir / "rookie_model_a_meta.pkl", "wb") as f:
    pickle.dump(meta_a, f)

print(f"\nSaved rookie Model A → {save_dir}/")
print(f"  features : {meta_a['feature_cols']}")
print(f"  n_obs    : {meta_a['n_obs']:,}")
