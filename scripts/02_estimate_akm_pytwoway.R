# scripts/02_estimate_akm_pytwoway.R

library(data.table)
library(reticulate)


run_akm <- function(
    data_path = "outputs/data/simulated_dads_long.csv",
    envname = "r-pytwoway",
    ncore = 1
) {
  use_virtualenv(envname, required = TRUE)
  
  panel <- fread(data_path)
  
  required_cols <- c("i", "j", "y", "t")
  missing_cols <- setdiff(required_cols, names(panel))
  
  if (length(missing_cols) > 0) {
    stop("Missing columns in AKM input data: ", paste(missing_cols, collapse = ", "))
  }
  
  panel[, i := as.integer(i)]
  panel[, j := as.integer(j)]
  panel[, t := as.integer(t)]
  panel[, y := as.numeric(y)]
  
  py$input_df <- as.data.frame(panel)
  py$output_dir <- normalizePath("outputs/data", winslash = "/", mustWork = TRUE)
  py$ncore <- as.integer(ncore)
  
  py_run_string("
from pathlib import Path
import json
import numpy as np
import pandas as pd
import bipartitepandas as bpd
import pytwoway as tw

out = Path(output_dir)

df = input_df.copy()
df = df[['i', 'j', 'y', 't']].copy()
df['i'] = df['i'].astype(int)
df['j'] = df['j'].astype(int)
df['t'] = df['t'].astype(int)
df['y'] = df['y'].astype(float)

# Build bipartite dataframe.
bdf = bpd.BipartiteDataFrame(df, track_id_changes=True)

# Keep this simple for the first pass: connected set, no leave-one-out corrections.
clean_params = bpd.clean_params({
    'connectedness': 'connected',
    'drop_returns': 'returners',
    'drop_single_stayers': False,
    'copy': False
})

bdf = bdf.clean(clean_params)

# Estimate AKM fixed effects. We turn off HO/HE corrections here.
# The goal is to recover estimated alpha_i and psi_j, not bias-corrected variance components.
fe_params = tw.fe_params({
    'weighted': False,
    'ho': False,
    'he': False,
    'feonly': False,
    'attach_fe_estimates': True,
    'ncore': int(ncore),
    'solver': 'minres',
    'preconditioner': None,
    'progress_bars': False,
    'verbose': False
})

fe_estimator = tw.FEEstimator(bdf, fe_params)
fe_estimator.fit(rng=np.random.default_rng(123))

adata_out = pd.DataFrame(fe_estimator.adata).copy()

required_output_cols = ['i', 'j', 'y', 't', 'alpha_hat', 'psi_hat']
missing_output_cols = [c for c in required_output_cols if c not in adata_out.columns]

if len(missing_output_cols) > 0:
    raise RuntimeError(
        'AKM estimation ran, but these expected columns are missing: '
        + ', '.join(missing_output_cols)
    )

obs_effects = adata_out[required_output_cols].copy()
obs_effects.to_csv(out / 'akm_observation_effects.csv', index=False)

worker_effects = (
    obs_effects
    .groupby('i', as_index=False)
    .agg(alpha_hat=('alpha_hat', 'mean'))
)

firm_effects = (
    obs_effects
    .groupby('j', as_index=False)
    .agg(psi_hat=('psi_hat', 'mean'))
)

worker_effects.to_csv(out / 'akm_worker_effects.csv', index=False)
firm_effects.to_csv(out / 'akm_firm_effects.csv', index=False)

summary_text = str(fe_estimator.summary)
res_text = json.dumps(fe_estimator.res, default=str, indent=2)

(out / 'akm_summary.txt').write_text(summary_text, encoding='utf-8')
(out / 'akm_res.json').write_text(res_text, encoding='utf-8')

akm_status = {
    'n_obs_after_cleaning': int(len(obs_effects)),
    'n_workers_after_cleaning': int(worker_effects.shape[0]),
    'n_firms_after_cleaning': int(firm_effects.shape[0])
}
")

status <- py_to_r(py$akm_status)

cat("\nAKM estimation finished.\n")
cat("Observations after cleaning:", status$n_obs_after_cleaning, "\n")
cat("Workers after cleaning:", status$n_workers_after_cleaning, "\n")
cat("Establishments after cleaning:", status$n_firms_after_cleaning, "\n")
cat("\nSaved:\n")
cat("  outputs/data/akm_observation_effects.csv\n")
cat("  outputs/data/akm_worker_effects.csv\n")
cat("  outputs/data/akm_firm_effects.csv\n")
cat("  outputs/data/akm_summary.txt\n")
cat("  outputs/data/akm_res.json\n")

invisible(status)
}

run_akm()