"""
Monte Carlo team wRC+ simulation — 2024 MLB Season.
Mirrors simulation.ipynb end-to-end; run this to regenerate output CSVs.

Procedure 1 (p1): stochastic — draw wRC+ ~ Normal each iteration, multinomial role draw.
  Output: mean, median, p10, p90 per team; 6 cols per player.

Procedure 2 (p2): deterministic — use regression mean directly, argmax role assignment,
  single normalised PA pass. Output: one wRC+ per team; 2 cols per player.
"""
import sys
sys.path.insert(0, '..')

import numpy as np
import pandas as pd
import pickle
import scipy as sp
from pathlib import Path
from itertools import product

# ── Locate project root ───────────────────────────────────────────────────────
data_path = Path(__file__).resolve().parent.parent
while not (data_path / "data" / "fangraphs" / "fg_hitters.csv").exists() and data_path != data_path.parent:
    data_path = data_path.parent
DATA_DIR = data_path
print(f"DATA_DIR = {DATA_DIR}")

# ── Parameters ────────────────────────────────────────────────────────────────
N            = 500
TEAM_BUDGET  = 6100
STARTER_CAP  = 5400   # max total PA allocated to starters per team (~9 starters × 600)
PA_BY_ROLE   = {'bench': 70, 'platoon': 350, 'starter': 600}

REGRESSION_MODELS = {
    'BMC':            ('BMC_mean',            'BMC_pred_sd'),
    'BMC_theta':      ('BMC_theta_mean',      'BMC_theta_sd'),
    'wRC_homo':       ('wRC_homo_mean',        'wRC_homo_sd'),
    'BBK_Brl_homo':   ('BBK_Brl_homo_mean',    'BBK_Brl_homo_sd'),
    'wRC_delta_homo': ('wRC_delta_homo_mean',   'wRC_delta_homo_sd'),
}
CLASSIFICATION_MODELS = ['lr_m02', 'lr_m07', 'lr_m09']

n_p1 = len(REGRESSION_MODELS) * len(CLASSIFICATION_MODELS)
print(f"N = {N}  |  team budget = {TEAM_BUDGET} PA  |  starter cap = {STARTER_CAP}")
print(f"p1 (stochastic): {n_p1} combos × {N} iterations")
print(f"p2 (deterministic): {n_p1} combos × 1 pass")

# ── Load data ─────────────────────────────────────────────────────────────────
reg_df = pd.read_csv(DATA_DIR / 'data' / 'model_outputs' / 'regression_outputs_2024.csv')
train  = pd.read_csv(DATA_DIR / 'data' / 'fg_builds' / 'fg_hitters_train.csv')
test   = pd.read_csv(DATA_DIR / 'data' / 'fg_builds' / 'fg_hitters_test.csv')
print(f"Regression outputs: {len(reg_df)}  |  Train: {len(train)}  |  Test: {len(test)}")

# ── Build player table ────────────────────────────────────────────────────────
players = test[['IDfg', 'Name', 'Team', 'Age', 'wRC+', 'primary_pos',
                'rookie', 'role', 'same_team']].copy()

reg_cols = (['Name', 'team_start']
            + [c for pair in REGRESSION_MODELS.values() for c in pair])
players = players.merge(reg_df[reg_cols], on='Name', how='left')
players['team_start'] = players['team_start'].fillna(players['Team'])

for mean_col, sd_col in REGRESSION_MODELS.values():
    missing = players[mean_col].isna()
    players.loc[missing & (players['Age'] <= 25), mean_col] = 86.0
    players.loc[missing & (players['Age'] <= 25), sd_col]   = 12.0
    players.loc[missing & (players['Age'] >  25), mean_col] = 76.0
    players.loc[missing & (players['Age'] >  25), sd_col]   =  5.0

t1 = (
    train[train['Season'] == 2023]
    [['IDfg', 'PA', 'GS_pct', 'wRC+', 'BsR', 'Def', 'role', 'same_team']]
    .rename(columns={'PA': 'PA_t1', 'GS_pct': 'GS_pct_t1', 'wRC+': 'wRC+_t1',
                     'BsR': 'BsR_t1', 'Def': 'Def_t1',
                     'role': 'role_t1', 'same_team': 'same_team_t1'})
)
players = players.merge(t1, on='IDfg', how='left')
players['starter_t1'] = (
    (players['role_t1'] == 'starter')
    .where(players['role_t1'].notna())
    .astype('Float64')
)
players['same_team_t1_x_starter_t1'] = players['same_team_t1'] * players['starter_t1']

MODEL_T1_FEATURES = {
    'lr_m02': ['starter_t1', 'PA_t1'],
    'lr_m07': ['wRC+_t1', 'starter_t1', 'Def_t1'],
    'lr_m09': ['wRC+_t1', 'starter_t1', 'BsR_t1', 'Def_t1', 'same_team_t1_x_starter_t1'],
}
for model, feats in MODEL_T1_FEATURES.items():
    players[f'has_t1_{model}'] = players[feats].notna().all(axis=1)

players = players.reset_index(drop=True)
print(f"\nPlayer table: {len(players)} rows x {players.shape[1]} cols")
for m in CLASSIFICATION_MODELS:
    n_main = players[f'has_t1_{m}'].sum()
    print(f"  {m}: {n_main} main  |  {len(players)-n_main} rookie fallback")

# ── Load models ───────────────────────────────────────────────────────────────
RUNS_DIR = DATA_DIR / 'data' / 'model_runs'

class_traces, class_metas = {}, {}
for tag in CLASSIFICATION_MODELS:
    with open(RUNS_DIR / 'playing_time' / f'{tag}_trace.pkl', 'rb') as f:
        class_traces[tag] = pickle.load(f)
    with open(RUNS_DIR / 'playing_time' / f'{tag}_meta.pkl', 'rb') as f:
        class_metas[tag] = pickle.load(f)
    print(f"Loaded {tag}: {class_metas[tag]['feature_cols']}")

with open(RUNS_DIR / 'rookie_projections' / 'rookie_model_a_trace.pkl', 'rb') as f:
    rook_trace = pickle.load(f)
with open(RUNS_DIR / 'rookie_projections' / 'rookie_model_a_meta.pkl', 'rb') as f:
    rook_meta  = pickle.load(f)
print(f"Loaded rookie fallback: {rook_meta['feature_cols']}")

# ── Core functions ────────────────────────────────────────────────────────────
ROLE_LIST = ['bench', 'platoon', 'starter']
PA_ARRAY  = np.array([PA_BY_ROLE[r] for r in ROLE_LIST], dtype=float)  # [70, 350, 600]


def compute_role_probs(trace, meta, df):
    """Posterior mean role probabilities: (n_players, 3) — [bench, platoon, starter]."""
    feature_cols = meta['feature_cols']
    X = df[feature_cols].astype(float).copy()
    for col, params in meta['scaler'].items():
        X[col] = (X[col] - params['mean']) / params['std']
    X_np = X.to_numpy()
    beta = trace.posterior['beta'].values.reshape(-1, len(feature_cols))
    cp   = trace.posterior['cutpoints'].values.reshape(-1, 2)
    eta  = X_np @ beta.T
    cum  = sp.special.expit(cp[np.newaxis] - eta[:, :, np.newaxis])
    pad0 = np.zeros((len(X_np), beta.shape[0], 1))
    pad1 = np.ones( (len(X_np), beta.shape[0], 1))
    prbs = np.diff(np.concatenate([pad0, cum, pad1], axis=2), axis=2)
    return prbs.mean(axis=1)


def _normalize_pa(roles, raw_pa, players, teams):
    """Two-bucket PA normalisation shared by both procedures."""
    n       = len(players)
    norm_pa = np.zeros(n)
    for team in teams:
        tm           = (players['team_start'] == team).to_numpy()
        is_starter   = (roles == 2) & tm
        is_non_start = (roles != 2) & tm

        raw_s_total = raw_pa[is_starter].sum()
        if raw_s_total > STARTER_CAP:
            norm_pa[is_starter] = raw_pa[is_starter] * STARTER_CAP / raw_s_total
            starter_final = STARTER_CAP
        else:
            norm_pa[is_starter] = raw_pa[is_starter]
            starter_final = float(raw_s_total)

        non_start_budget = TEAM_BUDGET - starter_final
        raw_ns_total     = raw_pa[is_non_start].sum()
        if raw_ns_total > 0:
            norm_pa[is_non_start] = raw_pa[is_non_start] * non_start_budget / raw_ns_total
        elif is_non_start.sum() > 0:
            norm_pa[is_non_start] = non_start_budget / is_non_start.sum()
    return norm_pa


def run_simulation(players, reg_model, class_model,
                   class_traces, class_metas, rook_trace, rook_meta,
                   N=500, rng=None):
    """
    Procedure 1 — stochastic Monte Carlo.
    Draws wRC+ ~ Normal(mean, SD) each iteration; multinomial role draw.
    Returns wrc_sims (N, n), pa_sims (N, n).
    """
    if rng is None:
        rng = np.random.default_rng(42)

    mean_col, sd_col = REGRESSION_MODELS[reg_model]
    reg_mean = players[mean_col].to_numpy(dtype=float)
    reg_sd   = players[sd_col].to_numpy(dtype=float)
    has_t1   = players[f'has_t1_{class_model}'].to_numpy(dtype=bool)
    trace_c  = class_traces[class_model]
    meta_c   = class_metas[class_model]
    n        = len(players)
    teams    = players['team_start'].dropna().unique()

    wrc_sims = np.zeros((N, n))
    pa_sims  = np.zeros((N, n))

    for i in range(N):
        wrc_i = rng.normal(reg_mean, reg_sd)
        wrc_sims[i] = wrc_i

        probs = np.zeros((n, 3))
        if has_t1.sum() > 0:
            df_main = players[has_t1].copy(); df_main['wRC+'] = wrc_i[has_t1]
            probs[has_t1] = compute_role_probs(trace_c, meta_c, df_main)
        if (~has_t1).sum() > 0:
            df_fall = players[~has_t1].copy(); df_fall['wRC+'] = wrc_i[~has_t1]
            probs[~has_t1] = compute_role_probs(rook_trace, rook_meta, df_fall)

        cumpr  = probs.cumsum(axis=1)
        u      = rng.random(n)[:, np.newaxis]
        roles  = (u > cumpr).sum(axis=1)
        pa_sims[i] = _normalize_pa(roles, PA_ARRAY[roles], players, teams)

    return wrc_sims, pa_sims


def run_deterministic(players, reg_model, class_model,
                      class_traces, class_metas, rook_trace, rook_meta):
    """
    Procedure 2 — single deterministic pass.
    Uses regression mean wRC+ directly; hard role assignment via argmax;
    same two-bucket PA normalisation.
    Returns wrc_det (n,), pa_det (n,).
    """
    mean_col, _ = REGRESSION_MODELS[reg_model]
    reg_mean    = players[mean_col].to_numpy(dtype=float)
    has_t1      = players[f'has_t1_{class_model}'].to_numpy(dtype=bool)
    trace_c     = class_traces[class_model]
    meta_c      = class_metas[class_model]
    n           = len(players)
    teams       = players['team_start'].dropna().unique()

    # Set wRC+ to regression mean in a working copy
    df_work = players.copy()
    df_work['wRC+'] = reg_mean

    probs = np.zeros((n, 3))
    if has_t1.sum() > 0:
        probs[has_t1] = compute_role_probs(trace_c, meta_c, df_work[has_t1])
    if (~has_t1).sum() > 0:
        probs[~has_t1] = compute_role_probs(rook_trace, rook_meta, df_work[~has_t1])

    roles  = probs.argmax(axis=1)           # hard allocation
    raw_pa = PA_ARRAY[roles]
    pa_det = _normalize_pa(roles, raw_pa, players, teams)

    return reg_mean, pa_det


# ── Run all combinations ──────────────────────────────────────────────────────
results_p1 = {}   # (reg, cls) → {'wrc_sims': (N,n), 'pa_sims': (N,n)}
results_p2 = {}   # (reg, cls) → {'wrc_det': (n,),   'pa_det':  (n,)}

rng   = np.random.default_rng(0)
n_combos = len(REGRESSION_MODELS) * len(CLASSIFICATION_MODELS)
done  = 0

for reg_name, class_name in product(REGRESSION_MODELS, CLASSIFICATION_MODELS):
    done += 1
    print(f"[{done:>2}/{n_combos}] {reg_name} × {class_name}", flush=True)

    wrc_sims, pa_sims = run_simulation(
        players, reg_name, class_name,
        class_traces, class_metas, rook_trace, rook_meta,
        N=N, rng=rng,
    )
    results_p1[(reg_name, class_name)] = {'wrc_sims': wrc_sims, 'pa_sims': pa_sims}

    wrc_det, pa_det = run_deterministic(
        players, reg_name, class_name,
        class_traces, class_metas, rook_trace, rook_meta,
    )
    results_p2[(reg_name, class_name)] = {'wrc_det': wrc_det, 'pa_det': pa_det}

print(f"\nDone — {done} p1 + {done} p2 combinations.")

# ── Team-level output ─────────────────────────────────────────────────────────
teams = sorted(players['team_start'].dropna().unique())
team_rows = {t: {'team': t} for t in teams}

for (reg_name, class_name), sims in results_p1.items():
    col_prefix = f"{reg_name}_{class_name}_p1"
    wrc_sims, pa_sims = sims['wrc_sims'], sims['pa_sims']
    for team in teams:
        tm       = (players['team_start'] == team).to_numpy()
        team_avg = (wrc_sims[:, tm] * pa_sims[:, tm]).sum(axis=1) / TEAM_BUDGET
        team_rows[team][f'{col_prefix}_mean']   = round(float(team_avg.mean()), 2)
        team_rows[team][f'{col_prefix}_median'] = round(float(np.median(team_avg)), 2)
        team_rows[team][f'{col_prefix}_p10']    = round(float(np.percentile(team_avg, 10.0)), 2)
        team_rows[team][f'{col_prefix}_p90']    = round(float(np.percentile(team_avg, 90.0)), 2)

for (reg_name, class_name), det in results_p2.items():
    col_prefix = f"{reg_name}_{class_name}_p2"
    wrc_det, pa_det = det['wrc_det'], det['pa_det']
    for team in teams:
        tm = (players['team_start'] == team).to_numpy()
        team_wrc = (wrc_det[tm] * pa_det[tm]).sum() / TEAM_BUDGET
        team_rows[team][f'{col_prefix}_wRC+'] = round(float(team_wrc), 2)

team_df = pd.DataFrame(list(team_rows.values())).set_index('team').sort_index()
out_team = DATA_DIR / 'data' / 'model_outputs' / 'model_team_sim_2024.csv'
team_df.to_csv(out_team)
print(f"\nSaved team output → {out_team}")
print(f"  {team_df.shape[0]} teams, {team_df.shape[1]} columns")
print(f"  (p1: {n_combos}×4 = {n_combos*4}  +  p2: {n_combos}×1 = {n_combos})")

# ── Player-level output ───────────────────────────────────────────────────────
base_df = players[['Name', 'team_start', 'Age', 'primary_pos',
                   'role', 'rookie', 'wRC+']].copy()
base_df = base_df.rename(columns={'wRC+': 'actual_wRC+'}).reset_index(drop=True)
blocks  = [base_df]

for (reg_name, class_name), sims in results_p1.items():
    col_prefix       = f"{reg_name}_{class_name}_p1"
    mean_col, sd_col = REGRESSION_MODELS[reg_name]
    wrc_sims, pa_sims = sims['wrc_sims'], sims['pa_sims']
    blocks.append(pd.DataFrame({
        f'{col_prefix}_sim_mean_wrc': wrc_sims.mean(axis=0).round(1),
        f'{col_prefix}_reg_mean_wrc': players[mean_col].to_numpy().round(1),
        f'{col_prefix}_reg_sd_wrc':   players[sd_col].to_numpy().round(1),
        f'{col_prefix}_median_PA':    np.median(pa_sims,  axis=0).round(1),
        f'{col_prefix}_p025_PA':      np.percentile(pa_sims,  2.5, axis=0).round(1),
        f'{col_prefix}_p975_PA':      np.percentile(pa_sims, 97.5, axis=0).round(1),
    }))

for (reg_name, class_name), det in results_p2.items():
    col_prefix = f"{reg_name}_{class_name}_p2"
    mean_col, _ = REGRESSION_MODELS[reg_name]
    blocks.append(pd.DataFrame({
        f'{col_prefix}_reg_mean_wrc': players[mean_col].to_numpy().round(1),
        f'{col_prefix}_PA':           det['pa_det'].round(1),
    }))

player_df = pd.concat(blocks, axis=1)
out_player = DATA_DIR / 'data' / 'model_outputs' / 'model_playing_time_2024.csv'
player_df.to_csv(out_player, index=False)
print(f"Saved player output → {out_player}")
print(f"  {player_df.shape[0]} players, {player_df.shape[1]} columns")
print(f"  (p1: {n_combos}×6 = {n_combos*6}  +  p2: {n_combos}×2 = {n_combos*2}  +  7 base)")
