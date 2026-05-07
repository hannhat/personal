"""
Bayesian MARCEL model for wRC+ projection.

Block 1: Hierarchical pooling — infers latent true-talent wRC+ per player-season
with partial pooling toward a league-wide population distribution.

Block 2: Season weighting — adds a weighted projection from prior seasons and a
prediction likelihood. Two variants:
  - Two-season (primary): Beta weight on t-1 vs. t-2; target years 2018–2023.
  - Three-season (sensitivity): Dirichlet weights on t-1/t-2/t-3; target years 2019–2023.

Block 3: Aging curve — adjusts the Block 2 projection for the player's age in the
target season. Two variants:
  - Triangular (baseline): symmetric linear decline, 2 aging params.
  - Asymmetric quadratic (primary): different pre-/post-peak curvature, 3 aging params.

The combined Block 1+2+3 model is built via build_marcel_model(), which accepts
n_seasons and aging_form parameters covering all four sensitivity combinations.

Standalone Block 1 and Block 2 utilities are preserved below for reference.
"""

import numpy as np
import pandas as pd
import pymc as pm
import arviz as az


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

OHTANI_IDFG = 19755
TRAIN_SEASONS = list(range(2016, 2024))  # 2016–2023
MIN_PA = 100  # default minimum PA threshold

# Two-season target years: 2018–2023 (t-1 requires 2017, t-2 requires 2016).
PREDICTION_SEASONS = list(range(2018, 2024))

# Three-season target years: 2019–2023 (t-3 requires 2016).
PREDICTION_SEASONS_3S = list(range(2019, 2024))


# ---------------------------------------------------------------------------
# Data preparation
# ---------------------------------------------------------------------------

def load_and_filter(path: str, min_pa: int = 100) -> pd.DataFrame:
    """Load a pre-cleaned FanGraphs hitters CSV and apply the PA filter.

    Parameters
    ----------
    path   : path to fg_hitters_train.csv (or similar pre-cleaned file).
    min_pa : minimum plate appearances required to include a player-season.
             Default 100. Applied to all model-fitting rows (Block 1 and
             Block 2 prior seasons).

    Returns a long-format DataFrame with one row per qualifying (player, season).
    """
    df = pd.read_csv(path)
    df = df[df["PA"] >= min_pa]

    cols = ["IDfg", "Season", "Name", "Age", "PA", "wRC+", "primary_pos"]
    df = df[cols].reset_index(drop=True)

    df["player_season"] = df["IDfg"].astype(str) + "_" + df["Season"].astype(str)

    return df


def run_sanity_checks(df: pd.DataFrame, min_pa: int = 100) -> None:
    """Assert Block 1 data quality requirements. Raises on failure."""
    assert df["PA"].min() >= min_pa, f"PA min is {df['PA'].min()}, expected >= {min_pa}"
    assert set(df["Season"].unique()).issubset(set(TRAIN_SEASONS)), "Unexpected seasons"
    assert df[["IDfg", "Season", "Name", "PA", "wRC+"]].notna().all().all(), "Missing values"
    assert df.duplicated(subset=["IDfg", "Season"]).sum() == 0, "Duplicate (IDfg, Season)"

    pitchers = df[df["primary_pos"] == "P"]
    assert set(pitchers["IDfg"].unique()) <= {OHTANI_IDFG}, "Non-Ohtani pitchers found"

    extreme = df[(df["wRC+"] < 0) | (df["wRC+"] > 250)]
    if len(extreme) > 0:
        print(f"WARNING: {len(extreme)} rows with wRC+ outside [0, 250]:")
        print(extreme[["IDfg", "Name", "Season", "PA", "wRC+"]].to_string(index=False))

    print(f"Sanity checks passed. {len(df)} rows, {df['IDfg'].nunique()} unique players.")


# ---------------------------------------------------------------------------
# Block 2: Season weighting — data preparation
# ---------------------------------------------------------------------------

def build_prediction_table(df: pd.DataFrame, n_seasons: int = 2) -> pd.DataFrame:
    """Build the Block 2 prediction input table.

    For each (player, target_season) in the appropriate prediction window, look
    up the player's prior 1–n_seasons in df and record positional indices into
    df's row order (which matches the θ vector in Block 1).

    Parameters
    ----------
    df        : DataFrame from load_and_filter (Block 1 input).
    n_seasons : 2 (two-season Beta weighting) or 3 (three-season Dirichlet).
                Determines the target year window and number of prior indices.

    Returns
    -------
    DataFrame with columns: IDfg, Name, Season, wRC+, PA,
        theta_idx_t1, theta_idx_t2, [theta_idx_t3 if n_seasons==3], n_prior, pred_key.
    """
    if n_seasons not in (2, 3):
        raise ValueError(f"n_seasons must be 2 or 3, got {n_seasons}")

    pred_seasons = PREDICTION_SEASONS if n_seasons == 2 else PREDICTION_SEASONS_3S

    idx_map = {
        (row.IDfg, row.Season): i
        for i, row in df.iterrows()
    }

    rows = []
    for target_season in pred_seasons:
        targets = df[df["Season"] == target_season]
        for _, tgt in targets.iterrows():
            pid = tgt["IDfg"]

            idx_t1 = idx_map.get((pid, target_season - 1), -1)
            idx_t2 = idx_map.get((pid, target_season - 2), -1)

            if n_seasons == 3:
                idx_t3 = idx_map.get((pid, target_season - 3), -1)
                n_prior = sum(1 for idx in [idx_t1, idx_t2, idx_t3] if idx != -1)
            else:
                n_prior = sum(1 for idx in [idx_t1, idx_t2] if idx != -1)

            if n_prior == 0:
                continue

            row = {
                "IDfg": pid,
                "Name": tgt["Name"],
                "Season": target_season,
                "wRC+": tgt["wRC+"],
                "PA": tgt["PA"],
                "theta_idx_t1": idx_t1,
                "theta_idx_t2": idx_t2,
                "n_prior": n_prior,
            }
            if n_seasons == 3:
                row["theta_idx_t3"] = idx_t3

            rows.append(row)

    pred_df = pd.DataFrame(rows).reset_index(drop=True)
    pred_df["pred_key"] = (
        pred_df["IDfg"].astype(str) + "_" + pred_df["Season"].astype(str)
    )
    return pred_df


def run_prediction_sanity_checks(
    pred_df: pd.DataFrame,
    n_seasons: int = 2,
    min_pa: int = 100,
) -> None:
    """Assert Block 2 prediction table quality requirements."""
    assert pred_df["PA"].min() >= min_pa, (
        f"Prediction table PA min is {pred_df['PA'].min()}, expected >= {min_pa}"
    )

    pred_seasons = PREDICTION_SEASONS if n_seasons == 2 else PREDICTION_SEASONS_3S
    assert set(pred_df["Season"].unique()).issubset(set(pred_seasons))

    valid_n_prior = {1, 2} if n_seasons == 2 else {1, 2, 3}
    assert set(pred_df["n_prior"].unique()).issubset(valid_n_prior)

    idx_cols = ["theta_idx_t1", "theta_idx_t2"]
    if n_seasons == 3:
        idx_cols.append("theta_idx_t3")

    for _, row in pred_df.iterrows():
        n_valid = sum(1 for c in idx_cols if row[c] != -1)
        assert n_valid == row["n_prior"], (
            f"n_prior mismatch for {row['IDfg']}_{row['Season']}: "
            f"n_prior={row['n_prior']} but {n_valid} valid indices"
        )

    print(
        f"Prediction table checks passed. {len(pred_df)} rows, "
        f"n_prior distribution: {pred_df['n_prior'].value_counts().sort_index().to_dict()}"
    )


# ---------------------------------------------------------------------------
# Block 1: Model construction (standalone)
# ---------------------------------------------------------------------------

def build_block1_model(df: pd.DataFrame) -> pm.Model:
    """Construct the Block 1 hierarchical pooling model (standalone)."""
    wrc_obs = df["wRC+"].values.astype(float)
    pa_obs = df["PA"].values.astype(float)
    coords = {"player_season": df["player_season"].values}

    with pm.Model(coords=coords) as model:
        wrc_data = pm.MutableData("wrc_data", wrc_obs)
        pa_data = pm.MutableData("pa_data", pa_obs)

        mu_theta = pm.Normal("mu_theta", mu=100, sigma=5)
        sigma_theta = pm.HalfNormal("sigma_theta", sigma=20)
        sigma_obs = pm.HalfNormal("sigma_obs", sigma=500)

        # Non-centered parameterization to avoid Neal's funnel geometry
        theta_raw = pm.Normal("theta_raw", mu=0, sigma=1, dims="player_season")
        theta = pm.Deterministic("theta", mu_theta + sigma_theta * theta_raw, dims="player_season")

        obs_sd = sigma_obs / pm.math.sqrt(pa_data)
        pm.Normal("wrc_plus", mu=theta, sigma=obs_sd, observed=wrc_data, dims="player_season")

    return model


# ---------------------------------------------------------------------------
# Block 1: Fitting and diagnostics (standalone)
# ---------------------------------------------------------------------------

def fit_block1(
    model: pm.Model,
    target_accept: float = 0.9,
    tune: int = 1000,
    draws: int = 1000,
    chains: int = 4,
    random_seed: int = 42,
) -> az.InferenceData:
    """Sample the Block 1 model and return the trace with posterior predictive."""
    with model:
        trace = pm.sample(
            draws=draws,
            tune=tune,
            chains=chains,
            target_accept=target_accept,
            random_seed=random_seed,
            return_inferencedata=True,
        )
        pm.sample_posterior_predictive(trace, extend_inferencedata=True)

    return trace


def run_diagnostics(trace: az.InferenceData, df: pd.DataFrame, output_dir: str = ".") -> dict:
    """Run and print Block 1 diagnostics. Returns summary dict."""
    import os
    import matplotlib.pyplot as plt

    os.makedirs(output_dir, exist_ok=True)
    results = {}

    rhat = az.rhat(trace)
    max_rhat_scalar = max(
        float(rhat["mu_theta"].values),
        float(rhat["sigma_theta"].values),
        float(rhat["sigma_obs"].values),
        float(rhat["theta_raw"].max().values),
    )
    results["max_rhat"] = max_rhat_scalar
    rhat_ok = max_rhat_scalar <= 1.01
    print(f"Max R-hat: {max_rhat_scalar:.4f} — {'PASS' if rhat_ok else 'FAIL'}")

    divergences = trace.sample_stats["diverging"].values.sum()
    results["divergences"] = int(divergences)
    print(f"Divergences: {divergences}")

    summary = az.summary(trace, var_names=["mu_theta", "sigma_theta", "sigma_obs"])
    print("\nPosterior summary (hyper-parameters):")
    print(summary)
    results["summary"] = summary

    mu_mean = float(trace.posterior["mu_theta"].mean())
    sigma_theta_mean = float(trace.posterior["sigma_theta"].mean())
    sigma_obs_mean = float(trace.posterior["sigma_obs"].mean())
    print(f"\nmu_theta mean:    {mu_mean:.1f}  (expected 95–110)")
    print(f"sigma_theta mean: {sigma_theta_mean:.1f}  (expected 12–25)")
    print(f"sigma_obs mean:   {sigma_obs_mean:.1f}  (expected 300–600)")

    in_range = (95 <= mu_mean <= 110) and (12 <= sigma_theta_mean <= 25) and (300 <= sigma_obs_mean <= 600)
    if not in_range:
        print("WARNING: one or more posterior means outside expected range — investigate.")
    results["in_range"] = in_range

    az.plot_posterior(trace, var_names=["mu_theta", "sigma_theta", "sigma_obs"])
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "block1_posterior.png"), dpi=150)
    plt.show()

    az.plot_energy(trace)
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "block1_energy.png"), dpi=150)
    plt.show()

    observed_wrc = df["wRC+"].values
    ppc_wrc = trace.posterior_predictive["wrc_plus"].values.reshape(-1, len(observed_wrc))
    rng = np.random.default_rng(0)
    draw_idx = rng.choice(ppc_wrc.shape[0], size=200, replace=False)

    fig, ax = plt.subplots(figsize=(8, 5))
    for i in draw_idx:
        ax.hist(ppc_wrc[i], bins=60, range=(-50, 300), density=True, alpha=0.02, color="steelblue")
    ax.hist(observed_wrc, bins=60, range=(-50, 300), density=True, alpha=0.8,
            color="orange", label="Observed", histtype="step", linewidth=2)
    ax.set_xlabel("wRC+")
    ax.set_ylabel("Density")
    ax.set_title("Posterior Predictive Check: wRC+")
    ax.legend()
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "block1_ppc.png"), dpi=150)
    plt.show()

    return results


# ---------------------------------------------------------------------------
# Block 2: Joint model construction (standalone two-season)
# ---------------------------------------------------------------------------

def build_block2_model(df: pd.DataFrame, pred_df: pd.DataFrame) -> pm.Model:
    """Construct the joint Block 1 + Block 2 model (two-season Beta weighting).

    Block 1: hierarchical pooling (rate_like).
    Block 2: Beta-distributed weight on t-1; remaining weight assigned to t-2
             (prediction likelihood).
    """
    import pytensor.tensor as pt

    wrc_obs = df["wRC+"].values.astype(float)
    pa_obs = df["PA"].values.astype(float)
    coords = {
        "player_season": df["player_season"].values,
        "pred_row": pred_df["pred_key"].values,
    }

    target_wrc = pred_df["wRC+"].values.astype(float)
    target_pa = pred_df["PA"].values.astype(float)

    idx_t1 = pred_df["theta_idx_t1"].values.astype(int)
    idx_t2 = pred_df["theta_idx_t2"].values.astype(int)

    mask_t1 = (idx_t1 != -1).astype(float)
    mask_t2 = (idx_t2 != -1).astype(float)

    safe_t1 = np.where(idx_t1 == -1, 0, idx_t1)
    safe_t2 = np.where(idx_t2 == -1, 0, idx_t2)

    with pm.Model(coords=coords) as model:
        wrc_data = pm.MutableData("wrc_data", wrc_obs)
        pa_data = pm.MutableData("pa_data", pa_obs)

        mu_theta = pm.Normal("mu_theta", mu=100, sigma=5)
        sigma_theta = pm.HalfNormal("sigma_theta", sigma=20)
        sigma_obs = pm.HalfNormal("sigma_obs", sigma=500)

        theta_raw = pm.Normal("theta_raw", mu=0, sigma=1, dims="player_season")
        theta = pm.Deterministic(
            "theta", mu_theta + sigma_theta * theta_raw, dims="player_season"
        )

        obs_sd = sigma_obs / pm.math.sqrt(pa_data)
        pm.Normal("wrc_plus", mu=theta, sigma=obs_sd, observed=wrc_data,
                  dims="player_season")

        # Beta(5, 4): expected w_t1 = 5/9 ≈ 0.556 (slight recency lean).
        w = pm.Beta("w", alpha=5.0, beta=4.0)

        target_wrc_data = pm.MutableData("target_wrc_data", target_wrc)
        target_pa_data = pm.MutableData("target_pa_data", target_pa)
        mask_t1_data = pm.MutableData("mask_t1_data", mask_t1)
        mask_t2_data = pm.MutableData("mask_t2_data", mask_t2)
        safe_t1_data = pm.MutableData("safe_t1_data", safe_t1)
        safe_t2_data = pm.MutableData("safe_t2_data", safe_t2)

        theta_t1 = theta[safe_t1_data]
        theta_t2 = theta[safe_t2_data]

        raw_w_t1 = w * mask_t1_data
        raw_w_t2 = (1.0 - w) * mask_t2_data

        w_sum = raw_w_t1 + raw_w_t2
        norm_w_t1 = raw_w_t1 / w_sum
        norm_w_t2 = raw_w_t2 / w_sum

        theta_proj = pm.Deterministic(
            "theta_proj",
            norm_w_t1 * theta_t1 + norm_w_t2 * theta_t2,
            dims="pred_row",
        )

        pred_sd = sigma_obs / pm.math.sqrt(target_pa_data)
        pm.Normal("wrc_pred", mu=theta_proj, sigma=pred_sd,
                  observed=target_wrc_data, dims="pred_row")

    return model


# ---------------------------------------------------------------------------
# Block 2: Fitting and diagnostics (standalone)
# ---------------------------------------------------------------------------

def fit_block2(
    model: pm.Model,
    target_accept: float = 0.95,
    tune: int = 2000,
    draws: int = 1000,
    chains: int = 4,
    random_seed: int = 42,
) -> az.InferenceData:
    """Sample the joint Block 1 + Block 2 model."""
    with model:
        trace = pm.sample(
            draws=draws,
            tune=tune,
            chains=chains,
            target_accept=target_accept,
            random_seed=random_seed,
            return_inferencedata=True,
        )
        pm.sample_posterior_predictive(trace, extend_inferencedata=True)

    return trace


def run_block2_diagnostics(
    trace: az.InferenceData,
    pred_df: pd.DataFrame,
    block1_summary: dict = None,
    output_dir: str = ".",
) -> dict:
    """Run Block 2 diagnostics. Optionally compare to Block 1 posteriors."""
    import os
    import matplotlib.pyplot as plt

    os.makedirs(output_dir, exist_ok=True)
    results = {}

    rhat = az.rhat(trace)
    rhat_vals = [
        float(rhat["mu_theta"].values),
        float(rhat["sigma_theta"].values),
        float(rhat["sigma_obs"].values),
        float(rhat["theta_raw"].max().values),
        float(rhat["w"].values),
    ]
    max_rhat_scalar = max(rhat_vals)
    results["max_rhat"] = max_rhat_scalar
    rhat_ok = max_rhat_scalar <= 1.01
    print(f"Max R-hat: {max_rhat_scalar:.4f} — {'PASS' if rhat_ok else 'FAIL'}")

    divergences = int(trace.sample_stats["diverging"].values.sum())
    results["divergences"] = divergences
    print(f"Divergences: {divergences}")

    hyper_summary = az.summary(trace, var_names=["mu_theta", "sigma_theta", "sigma_obs"])
    print("\nPosterior summary (hyper-parameters):")
    print(hyper_summary)

    w_summary = az.summary(trace, var_names=["w"])
    print("\nPosterior summary (weight on t-1):")
    print(w_summary)
    results["hyper_summary"] = hyper_summary
    results["w_summary"] = w_summary

    w_mean = float(trace.posterior["w"].mean())
    print(f"\nPosterior means: w[t-1]={w_mean:.3f}, w[t-2]={1 - w_mean:.3f}")
    if w_mean < 0.5:
        print("NOTE: weight on t-1 is below 0.5; less recency lean than expected.")
    results["w_mean"] = w_mean

    mu_mean = float(trace.posterior["mu_theta"].mean())
    sigma_theta_mean = float(trace.posterior["sigma_theta"].mean())
    sigma_obs_mean = float(trace.posterior["sigma_obs"].mean())

    if block1_summary is not None:
        shifts = {
            "mu_theta": abs(mu_mean - block1_summary["mu_theta"]),
            "sigma_theta": abs(sigma_theta_mean - block1_summary["sigma_theta"]),
            "sigma_obs": abs(sigma_obs_mean - block1_summary["sigma_obs"]),
        }
        print("\nShift from Block 1 posteriors:")
        for k, v in shifts.items():
            flag = " *** FLAG" if v > 3 else ""
            print(f"  {k}: {v:.2f}{flag}")
        results["shifts"] = shifts

    ess_w = az.ess(trace, var_names=["w"])
    print(f"\nESS for w: {float(ess_w['w'].values):.0f}")
    results["ess_w"] = float(ess_w["w"].values)

    az.plot_posterior(trace, var_names=["mu_theta", "sigma_theta", "sigma_obs"])
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "block2_posterior_hyper.png"), dpi=150)
    plt.show()

    az.plot_posterior(trace, var_names=["w"])
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "block2_w_posterior.png"), dpi=150)
    plt.show()

    az.plot_energy(trace)
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "block2_energy.png"), dpi=150)
    plt.show()

    observed_target = pred_df["wRC+"].values
    ppc_pred = trace.posterior_predictive["wrc_pred"].values.reshape(-1, len(observed_target))
    rng = np.random.default_rng(0)
    draw_idx = rng.choice(ppc_pred.shape[0], size=200, replace=False)

    fig, ax = plt.subplots(figsize=(8, 5))
    for i in draw_idx:
        ax.hist(ppc_pred[i], bins=60, range=(-50, 300), density=True,
                alpha=0.02, color="steelblue")
    ax.hist(observed_target, bins=60, range=(-50, 300), density=True, alpha=0.8,
            color="orange", label="Observed target wRC+", histtype="step", linewidth=2)
    ax.set_xlabel("wRC+")
    ax.set_ylabel("Density")
    ax.set_title("Block 2 Prediction PPC: Target-Year wRC+")
    ax.legend()
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "block2_pred_ppc.png"), dpi=150)
    plt.show()

    return results


# ---------------------------------------------------------------------------
# Full MARCEL model: Blocks 1 + 2 + 3
# ---------------------------------------------------------------------------

def build_marcel_model(
    df: pd.DataFrame,
    pred_df: pd.DataFrame,
    n_seasons: int = 2,
    aging_form: str = "asymmetric_quadratic",
) -> pm.Model:
    """Construct the full Bayesian MARCEL model (Blocks 1 + 2 + 3).

    Block 1: hierarchical pooling (rate_like).
    Block 2: season weighting (prediction likelihood).
    Block 3: aging curve adjustment applied to theta_proj.

    Parameters
    ----------
    df         : Block 1 input (from load_and_filter with desired min_pa).
    pred_df    : Block 2 prediction table (from build_prediction_table with
                 matching n_seasons).
    n_seasons  : 2 (two-season Beta) or 3 (three-season Dirichlet).
    aging_form : 'triangular' or 'asymmetric_quadratic'.
    """
    if n_seasons not in (2, 3):
        raise ValueError(f"n_seasons must be 2 or 3, got {n_seasons}")
    if aging_form not in ("triangular", "asymmetric_quadratic"):
        raise ValueError(
            f"aging_form must be 'triangular' or 'asymmetric_quadratic', got '{aging_form}'"
        )

    wrc_obs = df["wRC+"].values.astype(float)
    pa_obs = df["PA"].values.astype(float)
    coords = {
        "player_season": df["player_season"].values,
        "pred_row": pred_df["pred_key"].values,
    }

    target_wrc = pred_df["wRC+"].values.astype(float)
    target_pa = pred_df["PA"].values.astype(float)

    age_target = (
        pred_df
        .merge(df[["IDfg", "Season", "Age"]], on=["IDfg", "Season"], how="left")
        ["Age"]
        .values.astype(float)
    )

    idx_t1 = pred_df["theta_idx_t1"].values.astype(int)
    idx_t2 = pred_df["theta_idx_t2"].values.astype(int)
    mask_t1 = (idx_t1 != -1).astype(float)
    mask_t2 = (idx_t2 != -1).astype(float)
    safe_t1 = np.where(idx_t1 == -1, 0, idx_t1)
    safe_t2 = np.where(idx_t2 == -1, 0, idx_t2)

    if n_seasons == 3:
        idx_t3 = pred_df["theta_idx_t3"].values.astype(int)
        mask_t3 = (idx_t3 != -1).astype(float)
        safe_t3 = np.where(idx_t3 == -1, 0, idx_t3)

    with pm.Model(coords=coords) as model:
        # === Block 1: rate_like ===
        wrc_data = pm.MutableData("wrc_data", wrc_obs)
        pa_data = pm.MutableData("pa_data", pa_obs)

        mu_theta = pm.Normal("mu_theta", mu=100, sigma=5)
        sigma_theta = pm.HalfNormal("sigma_theta", sigma=20)
        sigma_obs = pm.HalfNormal("sigma_obs", sigma=500)

        theta_raw = pm.Normal("theta_raw", mu=0, sigma=1, dims="player_season")
        theta = pm.Deterministic(
            "theta", mu_theta + sigma_theta * theta_raw, dims="player_season"
        )

        obs_sd = sigma_obs / pm.math.sqrt(pa_data)
        pm.Normal("wrc_plus", mu=theta, sigma=obs_sd, observed=wrc_data,
                  dims="player_season")

        # === Block 2: season weighting ===
        target_wrc_data = pm.MutableData("target_wrc_data", target_wrc)
        target_pa_data = pm.MutableData("target_pa_data", target_pa)
        mask_t1_data = pm.MutableData("mask_t1_data", mask_t1)
        mask_t2_data = pm.MutableData("mask_t2_data", mask_t2)
        safe_t1_data = pm.MutableData("safe_t1_data", safe_t1)
        safe_t2_data = pm.MutableData("safe_t2_data", safe_t2)

        theta_t1 = theta[safe_t1_data]
        theta_t2 = theta[safe_t2_data]

        if n_seasons == 2:
            # Beta(5, 4): expected weight on t-1 ≈ 0.556 (slight recency lean)
            w = pm.Beta("w", alpha=5.0, beta=4.0)

            raw_w_t1 = w * mask_t1_data
            raw_w_t2 = (1.0 - w) * mask_t2_data
            w_sum = raw_w_t1 + raw_w_t2
            theta_proj = pm.Deterministic(
                "theta_proj",
                (raw_w_t1 / w_sum) * theta_t1 + (raw_w_t2 / w_sum) * theta_t2,
                dims="pred_row",
            )

        else:  # n_seasons == 3
            # Dirichlet([5, 4, 3]): prior means [5/12, 4/12, 3/12] = recency lean t-1 > t-2 > t-3
            w = pm.Dirichlet("w", a=np.array([5., 4., 3.]))

            mask_t3_data = pm.MutableData("mask_t3_data", mask_t3)
            safe_t3_data = pm.MutableData("safe_t3_data", safe_t3)
            theta_t3 = theta[safe_t3_data]

            raw_w_t1 = w[0] * mask_t1_data
            raw_w_t2 = w[1] * mask_t2_data
            raw_w_t3 = w[2] * mask_t3_data
            w_sum = raw_w_t1 + raw_w_t2 + raw_w_t3
            theta_proj = pm.Deterministic(
                "theta_proj",
                (raw_w_t1 / w_sum) * theta_t1
                + (raw_w_t2 / w_sum) * theta_t2
                + (raw_w_t3 / w_sum) * theta_t3,
                dims="pred_row",
            )

        # === Block 3: aging curve ===
        age_target_data = pm.MutableData("age_target_data", age_target)
        a_0 = pm.Normal("a_0", mu=29, sigma=2)

        if aging_form == "triangular":
            beta = pm.HalfNormal("beta", sigma=0.5)
            age_effect = -beta * pm.math.abs(age_target_data - a_0)

        else:  # asymmetric_quadratic
            beta_young = pm.HalfNormal("beta_young", sigma=0.3)
            beta_old = pm.HalfNormal("beta_old", sigma=0.3)

            age_diff = age_target_data - a_0
            # Piecewise: pre-peak uses beta_young curvature, post-peak uses beta_old
            age_effect = pm.math.switch(
                pm.math.ge(age_diff, 0.0),
                -beta_old * age_diff**2,
                -beta_young * age_diff**2,
            )

        theta_proj_aged = pm.Deterministic(
            "theta_proj_aged",
            theta_proj + age_effect,
            dims="pred_row",
        )

        pred_sd = sigma_obs / pm.math.sqrt(target_pa_data)
        pm.Normal("wrc_pred", mu=theta_proj_aged, sigma=pred_sd,
                  observed=target_wrc_data, dims="pred_row")

    return model


# ---------------------------------------------------------------------------
# Full MARCEL model: Fitting
# ---------------------------------------------------------------------------

def fit_marcel(
    model: pm.Model,
    target_accept: float = 0.95,
    tune: int = 2000,
    draws: int = 1000,
    chains: int = 4,
    random_seed: int = 42,
) -> az.InferenceData:
    """Sample the full MARCEL model (Blocks 1 + 2 + 3)."""
    with model:
        trace = pm.sample(
            draws=draws,
            tune=tune,
            chains=chains,
            target_accept=target_accept,
            random_seed=random_seed,
            return_inferencedata=True,
        )
        pm.sample_posterior_predictive(trace, extend_inferencedata=True)
    return trace


# ---------------------------------------------------------------------------
# Full MARCEL model: Diagnostics
# ---------------------------------------------------------------------------

def run_marcel_diagnostics(
    trace: az.InferenceData,
    pred_df: pd.DataFrame,
    n_seasons: int = 2,
    aging_form: str = "asymmetric_quadratic",
    run_tag: str = None,
    block2_summary: dict = None,
    output_dir: str = ".",
) -> dict:
    """Run full MARCEL diagnostics including aging curve visualisation.

    Parameters
    ----------
    n_seasons     : 2 or 3 — controls weight variable interpretation.
    aging_form    : 'triangular' or 'asymmetric_quadratic'.
    run_tag       : label string used in output filenames (e.g. '2s_quad_100PA').
                    Derived from n_seasons + aging_form if not provided.
    block2_summary: dict with keys 'mu_theta', 'sigma_theta', 'sigma_obs', and
                    'w' (scalar for n_seasons=2) from a no-aging reference fit.
                    Omit to skip the shift check.
    output_dir    : directory where diagnostic PNGs are saved.
    """
    import os
    import matplotlib.pyplot as plt

    os.makedirs(output_dir, exist_ok=True)
    results = {}

    if run_tag is None:
        aging_short = "tri" if aging_form == "triangular" else "quad"
        run_tag = f"{n_seasons}s_{aging_short}"

    aging_vars = ["a_0", "beta"] if aging_form == "triangular" else ["a_0", "beta_young", "beta_old"]

    # ---- R-hat ----
    rhat = az.rhat(trace)
    rhat_vals = [
        float(rhat["mu_theta"].values),
        float(rhat["sigma_theta"].values),
        float(rhat["sigma_obs"].values),
        float(rhat["theta_raw"].max().values),
        float(rhat["a_0"].values),
    ]
    # w is scalar for n_seasons=2 (Beta), vector for n_seasons=3 (Dirichlet)
    rhat_vals.append(float(rhat["w"].max().values) if n_seasons == 3
                     else float(rhat["w"].values))
    if aging_form == "triangular":
        rhat_vals.append(float(rhat["beta"].values))
    else:
        rhat_vals.append(float(rhat["beta_young"].values))
        rhat_vals.append(float(rhat["beta_old"].values))

    max_rhat = max(rhat_vals)
    results["max_rhat"] = max_rhat
    rhat_ok = max_rhat <= 1.01
    print(f"Max R-hat: {max_rhat:.4f} — {'PASS' if rhat_ok else 'FAIL'}")

    # ---- Divergences ----
    divergences = int(trace.sample_stats["diverging"].values.sum())
    results["divergences"] = divergences
    print(f"Divergences: {divergences}")

    # ---- Posterior summaries ----
    hyper_summary = az.summary(trace, var_names=["mu_theta", "sigma_theta", "sigma_obs", "w"])
    print("\nPosterior summary (Block 1+2 hyper-parameters):")
    print(hyper_summary)
    results["hyper_summary"] = hyper_summary

    aging_summary = az.summary(trace, var_names=aging_vars)
    print(f"\nPosterior summary (Block 3 aging — {aging_form}):")
    print(aging_summary)
    results["aging_summary"] = aging_summary

    # ---- Weight summary ----
    if n_seasons == 2:
        w_mean = float(trace.posterior["w"].mean())
        print(f"\nPosterior means: w[t-1]={w_mean:.3f}, w[t-2]={1 - w_mean:.3f}")
        if w_mean < 0.5:
            print("NOTE: weight on t-1 below 0.5 — less recency lean than expected.")
        results["w_mean"] = w_mean
    else:
        w_samp = trace.posterior["w"].values.reshape(-1, 3)
        w_means = w_samp.mean(axis=0)
        print(f"\nPosterior means: w[t-1]={w_means[0]:.3f}, w[t-2]={w_means[1]:.3f}, w[t-3]={w_means[2]:.3f}")
        p_non_mono = float((w_samp[:, 2] > w_samp[:, 1]).mean())
        print(f"Non-monotonicity check: P(w[t-3] > w[t-2]) = {p_non_mono:.1%}")
        if p_non_mono > 0.5:
            print("WARNING: posterior assigns more weight to t-3 than t-2 — consider two-season model.")
        results["w_means"] = w_means
        results["p_non_mono"] = p_non_mono

    # ---- Aging parameter sanity checks ----
    a_0_mean = float(trace.posterior["a_0"].mean())
    print(f"\na_0 posterior mean: {a_0_mean:.2f}  (expected 27–30)")
    if not (27 <= a_0_mean <= 30):
        print("WARNING: a_0 outside expected range [27, 30] — investigate.")

    if aging_form == "triangular":
        beta_mean = float(trace.posterior["beta"].mean())
        print(f"beta posterior mean: {beta_mean:.3f}  (expected 0.3–1.0)")
    else:
        by_mean = float(trace.posterior["beta_young"].mean())
        bo_mean = float(trace.posterior["beta_old"].mean())
        print(f"beta_young posterior mean: {by_mean:.3f}  (expected 0.1–0.5)")
        print(f"beta_old posterior mean:   {bo_mean:.3f}  (expected 0.1–0.5)")

    # ---- Block 2 shift check (optional) ----
    if block2_summary is not None:
        shifts = {
            "mu_theta": abs(float(trace.posterior["mu_theta"].mean()) - block2_summary["mu_theta"]),
            "sigma_theta": abs(float(trace.posterior["sigma_theta"].mean()) - block2_summary["sigma_theta"]),
            "sigma_obs": abs(float(trace.posterior["sigma_obs"].mean()) - block2_summary["sigma_obs"]),
        }
        if n_seasons == 2 and "w" in block2_summary:
            shifts["w"] = abs(float(trace.posterior["w"].mean()) - block2_summary["w"])
        print("\nShift from reference (no-aging) posteriors — flag if > 2:")
        for k, v in shifts.items():
            flag = " *** FLAG" if v > 2 else ""
            print(f"  {k}: {v:.3f}{flag}")
        results["shifts"] = shifts

    # ---- ESS for aging parameters ----
    ess = az.ess(trace, var_names=aging_vars)
    print("\nESS for aging parameters:")
    for v in aging_vars:
        print(f"  {v}: {float(ess[v].values):.0f}")
    results["ess_aging"] = ess

    # ---- Plots ----

    # 1. Posterior — Block 1+2 hyper-parameters
    az.plot_posterior(trace, var_names=["mu_theta", "sigma_theta", "sigma_obs", "w"])
    plt.tight_layout()
    #plt.savefig(os.path.join(output_dir, f"{run_tag}_posterior_hyper.png"), dpi=150)
    plt.show()

    # 2. Posterior — aging parameters
    az.plot_posterior(trace, var_names=aging_vars)
    plt.tight_layout()
    #plt.savefig(os.path.join(output_dir, f"{run_tag}_posterior_aging.png"), dpi=150)
    plt.show()

    # 3. Pair plot — aging parameters (identifiability check)
    az.plot_pair(trace, var_names=aging_vars, divergences=True)
    plt.tight_layout()
    #plt.savefig(os.path.join(output_dir, f"{run_tag}_pair_aging.png"), dpi=150)
    plt.show()

    # 4. Energy plot
    az.plot_energy(trace)
    plt.tight_layout()
    #plt.savefig(os.path.join(output_dir, f"{run_tag}_energy.png"), dpi=150)
    plt.show()

    # 5. Prediction PPC
    observed_target = pred_df["wRC+"].values
    ppc_pred = trace.posterior_predictive["wrc_pred"].values.reshape(-1, len(observed_target))
    rng = np.random.default_rng(0)
    draw_idx = rng.choice(ppc_pred.shape[0], size=200, replace=False)

    fig, ax = plt.subplots(figsize=(8, 5))
    for i in draw_idx:
        ax.hist(ppc_pred[i], bins=60, range=(-50, 300), density=True,
                alpha=0.02, color="steelblue")
    ax.hist(observed_target, bins=60, range=(-50, 300), density=True, alpha=0.8,
            color="orange", label="Observed target wRC+", histtype="step", linewidth=2)
    ax.set_xlabel("wRC+")
    ax.set_ylabel("Density")
    ax.set_title(f"Prediction PPC: Target-Year wRC+ ({run_tag})")
    ax.legend()
    plt.tight_layout()
    #plt.savefig(os.path.join(output_dir, f"{run_tag}_pred_ppc.png"), dpi=150)
    plt.show()

    # 6. Aging curve — posterior mean + 94% HDI across ages 22–38
    ages = np.arange(22, 39, dtype=float)
    a_0_samp = trace.posterior["a_0"].values.flatten()

    if aging_form == "triangular":
        beta_samp = trace.posterior["beta"].values.flatten()
        age_effects = np.stack([
            -beta_samp * np.abs(age - a_0_samp) for age in ages
        ])  # (n_ages, n_samples)
    else:
        by_samp = trace.posterior["beta_young"].values.flatten()
        bo_samp = trace.posterior["beta_old"].values.flatten()
        age_effects = np.stack([
            np.where(
                (age - a_0_samp) >= 0,
                -bo_samp * (age - a_0_samp) ** 2,
                -by_samp * (age - a_0_samp) ** 2,
            )
            for age in ages
        ])  # (n_ages, n_samples)

    mean_effect = age_effects.mean(axis=1)
    hdi_vals = az.hdi(age_effects.T, hdi_prob=0.94)  # (n_ages, 2)

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.plot(ages, mean_effect, color="steelblue", linewidth=2, label="Posterior mean")
    ax.fill_between(ages, hdi_vals[:, 0], hdi_vals[:, 1],
                    color="steelblue", alpha=0.25, label="94% HDI")
    ax.axhline(0, color="black", linewidth=0.8, linestyle="--")
    ax.axvline(a_0_samp.mean(), color="orange", linewidth=1.5, linestyle=":",
               label=f"Mean peak age = {a_0_samp.mean():.1f}")
    ax.set_xlabel("Age")
    ax.set_ylabel("Age effect on wRC+")
    ax.set_title(f"Implied Aging Curve ({run_tag})")
    ax.legend()
    plt.tight_layout()
    #plt.savefig(os.path.join(output_dir, f"{run_tag}_aging_curve.png"), dpi=150)
    plt.show()

    results["ages"] = ages
    results["mean_age_effect"] = mean_effect

    return results
