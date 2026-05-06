# scripts/03_estimate_blm_pytwoway.R

library(data.table)
library(reticulate)


run_blm <- function(
    data_path = "outputs/data/simulated_dads_long.csv",
    envname = "r-pytwoway",
    nk = 10,
    nl = 6,
    n_init = 10,
    n_best = 3,
    ncore = 1,
    seed = 123
) {
  use_virtualenv(envname, required = TRUE)
  
  panel <- fread(data_path)
  
  required_cols <- c("i", "j", "y", "t")
  missing_cols <- setdiff(required_cols, names(panel))
  
  if (length(missing_cols) > 0) {
    stop("Missing columns in BLM input data: ", paste(missing_cols, collapse = ", "))
  }
  
  panel[, i := as.integer(i)]
  panel[, j := as.integer(j)]
  panel[, t := as.integer(t)]
  panel[, y := as.numeric(y)]
  
  py$input_df <- as.data.frame(panel)
  py$output_dir <- normalizePath("outputs/data", winslash = "/", mustWork = TRUE)
  py$nk <- as.integer(nk)
  py$nl <- as.integer(nl)
  py$n_init <- as.integer(n_init)
  py$n_best <- as.integer(n_best)
  py$ncore <- as.integer(ncore)
  py$seed <- as.integer(seed)
  
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

nk = int(nk)
nl = int(nl)
n_init = int(n_init)
n_best = int(n_best)
ncore = int(ncore)
seed = int(seed)

bdf = bpd.BipartiteDataFrame(df, track_id_changes=True)

clean_params = bpd.clean_params({
    'drop_returns': 'returners',
    'copy': False
})

cluster_params = bpd.cluster_params({
    'measures': bpd.measures.CDFs(),
    'grouping': bpd.grouping.KMeans(n_clusters=nk),
    'is_sorted': True,
    'copy': False
})

# Prepare data for BLM.
bdf = bdf.clean(clean_params)
bdf = bdf.collapse(is_sorted=True, copy=False)
bdf = bdf.cluster(cluster_params)

# Save firm type mapping when available.
clustered_long = pd.DataFrame(bdf).copy()

firm_type_parts = []

if ('j' in clustered_long.columns) and ('g' in clustered_long.columns):
    tmp = clustered_long[['j', 'g']].drop_duplicates().copy()
    tmp = tmp.rename(columns={'g': 'blm_firm_type'})
    firm_type_parts.append(tmp)

# Convert to event-study format.
bdf = bdf.to_eventstudy(is_sorted=True, copy=False)
event_df = pd.DataFrame(bdf).copy()

# Additional fallback firm type mapping from event-study columns.
for j_col, g_col in [('j1', 'g1'), ('j2', 'g2')]:
    if (j_col in event_df.columns) and (g_col in event_df.columns):
        tmp = event_df[[j_col, g_col]].drop_duplicates().copy()
        tmp = tmp.rename(columns={j_col: 'j', g_col: 'blm_firm_type'})
        firm_type_parts.append(tmp)

if len(firm_type_parts) > 0:
    firm_types = pd.concat(firm_type_parts, ignore_index=True)
    firm_types = firm_types.dropna()
    firm_types['j'] = firm_types['j'].astype(int)
    firm_types['blm_firm_type'] = firm_types['blm_firm_type'].astype(int)
    firm_types = firm_types.drop_duplicates().sort_values(['j', 'blm_firm_type'])
    firm_types = firm_types.drop_duplicates(subset=['j'], keep='first')
    firm_types.to_csv(out / 'blm_firm_types.csv', index=False)

event_df.head(1000).to_csv(out / 'blm_eventstudy_head.csv', index=False)

movers = bdf.get_worker_m(is_sorted=True)

jdata = bdf.loc[movers, :]
sdata = bdf.loc[~movers, :]

blm_params = tw.blm_params({
    'nl': nl,
    'nk': nk,
    'a1_mu': 0,
    'a1_sig': 1,
    'a2_mu': 0,
    'a2_sig': 1,
    's1_low': 0.1,
    's1_high': 1.0,
    's2_low': 0.1,
    's2_high': 1.0,
    'n_iters_movers': 500,
    'n_iters_stayers': 500,
    'threshold_movers': 1e-6,
    'threshold_stayers': 1e-6,
    'verbose': 1
})

blm_estimator = tw.BLMEstimator(blm_params)

rng = np.random.default_rng(seed)

blm_estimator.fit(
    jdata=jdata,
    sdata=sdata,
    n_init=n_init,
    n_best=n_best,
    ncore=ncore,
    rng=rng
)

model = blm_estimator.model

def matrix_to_long(mat, value_name):
    mat = np.asarray(mat)
    rows = []
    for l in range(mat.shape[0]):
        for k in range(mat.shape[1]):
            rows.append({
                'worker_type': l,
                'firm_type': k,
                value_name: float(mat[l, k])
            })
    return pd.DataFrame(rows)

matrix_to_long(model.A1, 'A1').to_csv(out / 'blm_A1.csv', index=False)
matrix_to_long(model.A2, 'A2').to_csv(out / 'blm_A2.csv', index=False)
matrix_to_long(model.S1, 'S1').to_csv(out / 'blm_S1.csv', index=False)
matrix_to_long(model.S2, 'S2').to_csv(out / 'blm_S2.csv', index=False)

pd.DataFrame(model.pk0).to_csv(out / 'blm_pk0.csv', index=False)
pd.DataFrame(model.pk1).to_csv(out / 'blm_pk1.csv', index=False)

summary = {
    'nk': nk,
    'nl': nl,
    'n_init': n_init,
    'n_best': n_best,
    'ncore': ncore,
    'seed': seed,
    'n_mover_rows_eventstudy': int(len(jdata)),
    'n_stayer_rows_eventstudy': int(len(sdata)),
    'connectedness': None if model.connectedness is None else float(model.connectedness),
    'likelihood_movers': None if model.lik1 is None else float(model.lik1),
    'likelihood_stayers': None if model.lik0 is None else float(model.lik0)
}

(out / 'blm_summary.json').write_text(json.dumps(summary, indent=2), encoding='utf-8')

with open(out / 'blm_summary.txt', 'w', encoding='utf-8') as f:
    for key, value in summary.items():
        f.write(f'{key}: {value}\\n')

blm_status = summary
")

status <- py_to_r(py$blm_status)

cat("\nBLM estimation finished.\n")
cat("Mover rows in event-study data:", status$n_mover_rows_eventstudy, "\n")
cat("Stayer rows in event-study data:", status$n_stayer_rows_eventstudy, "\n")
cat("Connectedness:", status$connectedness, "\n")
cat("\nSaved:\n")
cat("  outputs/data/blm_summary.json\n")
cat("  outputs/data/blm_summary.txt\n")
cat("  outputs/data/blm_A1.csv\n")
cat("  outputs/data/blm_A2.csv\n")
cat("  outputs/data/blm_S1.csv\n")
cat("  outputs/data/blm_S2.csv\n")
cat("  outputs/data/blm_pk0.csv\n")
cat("  outputs/data/blm_pk1.csv\n")
cat("  outputs/data/blm_firm_types.csv, if available\n")

invisible(status)
}

run_blm()