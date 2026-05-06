# scripts/04_evaluate_results.R

library(data.table)
library(ggplot2)

center_var <- function(x) {
  x - mean(x, na.rm = TRUE)
}

safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  
  if (sum(ok) < 2) {
    return(NA_real_)
  }
  
  cor(x[ok], y[ok])
}

safe_rmse <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  sqrt(mean((x[ok] - y[ok])^2))
}

evaluate_pair <- function(dt, true_col, estimated_col, object_name) {
  true_centered <- center_var(dt[[true_col]])
  estimated_centered <- center_var(dt[[estimated_col]])
  
  data.table(
    object = object_name,
    n = sum(is.finite(true_centered) & is.finite(estimated_centered)),
    correlation = safe_cor(true_centered, estimated_centered),
    rmse_centered = safe_rmse(true_centered, estimated_centered),
    mean_true = mean(dt[[true_col]], na.rm = TRUE),
    mean_estimated = mean(dt[[estimated_col]], na.rm = TRUE),
    var_true = var(dt[[true_col]], na.rm = TRUE),
    var_estimated = var(dt[[estimated_col]], na.rm = TRUE)
  )
}

required_files <- c(
  "outputs/data/truth_firms.csv",
  "outputs/data/truth_workers.csv",
  "outputs/data/simulated_dads_truth_panel.csv",
  "outputs/data/akm_firm_effects.csv",
  "outputs/data/akm_worker_effects.csv",
  "outputs/data/akm_observation_effects.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required files. Run scripts 01 and 02 first. Missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

truth_firms <- fread("outputs/data/truth_firms.csv")
truth_workers <- fread("outputs/data/truth_workers.csv")
truth_panel <- fread("outputs/data/simulated_dads_truth_panel.csv")

akm_firms <- fread("outputs/data/akm_firm_effects.csv")
akm_workers <- fread("outputs/data/akm_worker_effects.csv")
akm_obs <- fread("outputs/data/akm_observation_effects.csv")

truth_firms[, j := as.integer(j)]
truth_workers[, i := as.integer(i)]
truth_panel[, `:=`(
  i = as.integer(i),
  j = as.integer(j),
  t = as.integer(t)
)]

akm_firms[, j := as.integer(j)]
akm_workers[, i := as.integer(i)]
akm_obs[, `:=`(
  i = as.integer(i),
  j = as.integer(j),
  t = as.integer(t)
)]

# AKM firm FE recovery.
firm_eval <- merge(
  truth_firms,
  akm_firms,
  by = "j",
  all = FALSE
)

firm_eval[, psi_centered := center_var(psi)]
firm_eval[, psi_hat_centered := center_var(psi_hat)]

firm_metrics <- evaluate_pair(
  firm_eval,
  true_col = "psi",
  estimated_col = "psi_hat",
  object_name = "firm_fe"
)

# AKM worker FE recovery.
worker_eval <- merge(
  truth_workers[, .(i, alpha, mover)],
  akm_workers,
  by = "i",
  all = FALSE
)

worker_eval[, alpha_centered := center_var(alpha)]
worker_eval[, alpha_hat_centered := center_var(alpha_hat)]

worker_metrics <- evaluate_pair(
  worker_eval,
  true_col = "alpha",
  estimated_col = "alpha_hat",
  object_name = "worker_fe"
)

# AKM sorting / covariance comparison at observation level.
obs_eval <- merge(
  truth_panel[, .(i, j, t, alpha, psi, y)],
  akm_obs[, .(i, j, t, alpha_hat, psi_hat)],
  by = c("i", "j", "t"),
  all = FALSE
)

obs_eval[, alpha_c := center_var(alpha)]
obs_eval[, psi_c := center_var(psi)]
obs_eval[, alpha_hat_c := center_var(alpha_hat)]
obs_eval[, psi_hat_c := center_var(psi_hat)]

sorting_metrics <- data.table(
  object = "worker_firm_sorting_observation_level",
  n = nrow(obs_eval),
  true_cov_alpha_psi = cov(obs_eval$alpha_c, obs_eval$psi_c, use = "complete.obs"),
  estimated_cov_alpha_psi = cov(obs_eval$alpha_hat_c, obs_eval$psi_hat_c, use = "complete.obs"),
  true_cor_alpha_psi = safe_cor(obs_eval$alpha_c, obs_eval$psi_c),
  estimated_cor_alpha_psi = safe_cor(obs_eval$alpha_hat_c, obs_eval$psi_hat_c)
)

akm_metrics <- rbindlist(
  list(firm_metrics, worker_metrics),
  fill = TRUE
)

fwrite(akm_metrics, "outputs/tables/akm_recovery_metrics.csv")
fwrite(sorting_metrics, "outputs/tables/akm_sorting_metrics.csv")
fwrite(firm_eval, "outputs/tables/akm_firm_true_vs_estimated.csv")
fwrite(worker_eval, "outputs/tables/akm_worker_true_vs_estimated.csv")

# Figures: AKM firm effects.
p_firm <- ggplot(
  firm_eval,
  aes(x = psi_centered, y = psi_hat_centered)
) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AKM: true vs estimated establishment fixed effects",
    x = "True establishment FE, centered",
    y = "Estimated establishment FE, centered"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/akm_true_vs_estimated_firm_fe.png",
  plot = p_firm,
  width = 7,
  height = 5,
  dpi = 300
)

# Figures: AKM worker effects.
p_worker <- ggplot(
  worker_eval,
  aes(x = alpha_centered, y = alpha_hat_centered)
) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "AKM: true vs estimated worker fixed effects",
    x = "True worker FE, centered",
    y = "Estimated worker FE, centered"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/akm_true_vs_estimated_worker_fe.png",
  plot = p_worker,
  width = 7,
  height = 5,
  dpi = 300
)

# BLM evaluation is necessarily less direct:
# BLM estimates latent types, not one continuous FE per worker and establishment.
if (file.exists("outputs/data/blm_firm_types.csv")) {
  blm_firm_types <- fread("outputs/data/blm_firm_types.csv")
  blm_firm_types[, j := as.integer(j)]
  blm_firm_types[, blm_firm_type := as.integer(blm_firm_type)]
  
  blm_firm_eval <- merge(
    truth_firms,
    blm_firm_types,
    by = "j",
    all = FALSE
  )
  
  blm_type_summary <- blm_firm_eval[
    ,
    .(
      n_firms = .N,
      mean_true_psi = mean(psi),
      sd_true_psi = sd(psi),
      min_true_psi = min(psi),
      max_true_psi = max(psi)
    ),
    by = blm_firm_type
  ][order(blm_firm_type)]
  
  fwrite(blm_firm_eval, "outputs/tables/blm_firm_types_vs_true_psi.csv")
  fwrite(blm_type_summary, "outputs/tables/blm_firm_type_summary.csv")
  
  p_blm <- ggplot(
    blm_firm_eval,
    aes(x = factor(blm_firm_type), y = psi)
  ) +
    geom_boxplot() +
    labs(
      title = "BLM: distribution of true establishment FE by estimated firm type",
      x = "Estimated BLM firm type",
      y = "True establishment FE"
    ) +
    theme_minimal()
  
  ggsave(
    filename = "outputs/figures/blm_true_psi_by_estimated_firm_type.png",
    plot = p_blm,
    width = 7,
    height = 5,
    dpi = 300
  )
} else {
  warning("outputs/data/blm_firm_types.csv not found. BLM type evaluation skipped.")
}

cat("\nEvaluation finished.\n")
cat("Saved:\n")
cat("  outputs/tables/akm_recovery_metrics.csv\n")
cat("  outputs/tables/akm_sorting_metrics.csv\n")
cat("  outputs/tables/akm_firm_true_vs_estimated.csv\n")
cat("  outputs/tables/akm_worker_true_vs_estimated.csv\n")
cat("  outputs/figures/akm_true_vs_estimated_firm_fe.png\n")
cat("  outputs/figures/akm_true_vs_estimated_worker_fe.png\n")

if (file.exists("outputs/tables/blm_firm_type_summary.csv")) {
  cat("  outputs/tables/blm_firm_type_summary.csv\n")
  cat("  outputs/tables/blm_firm_types_vs_true_psi.csv\n")
  cat("  outputs/figures/blm_true_psi_by_estimated_firm_type.png\n")
}