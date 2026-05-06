# scripts/01_simulate_dads.R

library(data.table)

simulate_dads_2yr <- function(
    n_workers = 20000,
    n_firms = 1000,
    p_move = 0.25,
    sigma_alpha = 1.0,
    sigma_psi = 0.5,
    sigma_eps = 0.3,
    year2_effect = 0.05,
    seed = 123
) {
  set.seed(seed)
  
  workers <- data.table(
    i = 0:(n_workers - 1),
    alpha = rnorm(n_workers, mean = 0, sd = sigma_alpha)
  )
  
  firms <- data.table(
    j = 0:(n_firms - 1),
    psi = rnorm(n_firms, mean = 0, sd = sigma_psi)
  )
  
  # First-year establishment assignment.
  workers[, j1 := sample(firms$j, .N, replace = TRUE)]
  
  # Movers switch establishment between year 1 and year 2.
  workers[, mover := runif(.N) < p_move]
  
  workers[, j2 := j1]
  
  n_movers <- sum(workers$mover)
  
  if (n_movers > 0) {
    workers[mover == TRUE, j2 := sample(firms$j, .N, replace = TRUE)]
    
    # Enforce actual moves: j2 must differ from j1 for movers.
    same_firm <- workers[mover == TRUE & j2 == j1, which = TRUE]
    
    while (length(same_firm) > 0) {
      workers[same_firm, j2 := sample(firms$j, .N, replace = TRUE)]
      same_firm <- workers[mover == TRUE & j2 == j1, which = TRUE]
    }
  }
  
  year1 <- workers[, .(
    i,
    j = j1,
    t = 1L,
    alpha,
    mover
  )]
  
  year2 <- workers[, .(
    i,
    j = j2,
    t = 2L,
    alpha,
    mover
  )]
  
  panel_truth <- rbind(year1, year2)
  panel_truth <- merge(panel_truth, firms, by = "j", all.x = TRUE)
  
  panel_truth[, lambda_t := year2_effect * (t == 2L)]
  panel_truth[, eps := rnorm(.N, mean = 0, sd = sigma_eps)]
  panel_truth[, y := alpha + psi + lambda_t + eps]
  
  setorder(panel_truth, i, t)
  
  panel_pytwoway <- panel_truth[, .(
    i = as.integer(i),
    j = as.integer(j),
    y = as.numeric(y),
    t = as.integer(t)
  )]
  
  list(
    panel_pytwoway = panel_pytwoway,
    panel_truth = panel_truth[, .(
      i, j, t, y, alpha, psi, lambda_t, eps, mover
    )],
    truth_workers = workers[, .(
      i, alpha, mover, j1, j2
    )],
    truth_firms = firms[, .(
      j, psi
    )],
    params = data.table(
      n_workers = n_workers,
      n_firms = n_firms,
      p_move = p_move,
      sigma_alpha = sigma_alpha,
      sigma_psi = sigma_psi,
      sigma_eps = sigma_eps,
      year2_effect = year2_effect,
      seed = seed
    )
  )
}

sim <- simulate_dads_2yr(
  n_workers = 20000,
  n_firms = 1000,
  p_move = 0.25,
  sigma_alpha = 1.0,
  sigma_psi = 0.5,
  sigma_eps = 0.3,
  year2_effect = 0.05,
  seed = 123
)

fwrite(sim$panel_pytwoway, "outputs/data/simulated_dads_long.csv")
fwrite(sim$panel_truth, "outputs/data/simulated_dads_truth_panel.csv")
fwrite(sim$truth_workers, "outputs/data/truth_workers.csv")
fwrite(sim$truth_firms, "outputs/data/truth_firms.csv")
fwrite(sim$params, "outputs/data/simulation_params.csv")

cat("\nSimulation finished.\n")
cat("Saved:\n")
cat("  outputs/data/simulated_dads_long.csv\n")
cat("  outputs/data/simulated_dads_truth_panel.csv\n")
cat("  outputs/data/truth_workers.csv\n")
cat("  outputs/data/truth_firms.csv\n")
cat("  outputs/data/simulation_params.csv\n\n")

cat("Basic checks:\n")
cat("  Observations:", nrow(sim$panel_pytwoway), "\n")
cat("  Workers:", uniqueN(sim$panel_pytwoway$i), "\n")
cat("  Establishments:", uniqueN(sim$panel_pytwoway$j), "\n")
cat("  Movers:", sum(sim$truth_workers$mover), "\n")
cat("  Stayers:", sum(!sim$truth_workers$mover), "\n")
cat("  Mean y:", mean(sim$panel_pytwoway$y), "\n")
cat("  Var y:", var(sim$panel_pytwoway$y), "\n")