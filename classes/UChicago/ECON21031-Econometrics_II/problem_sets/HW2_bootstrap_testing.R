library(ggplot2)
library(ivreg)
  
#runtime around 25-30 minutes

set.seed(2304)
gamma = c(0.25, 0.10, 0.025, 0)

columns= c("gamma", "Standard approximation", "Efron's Percentile (EPI)",
           "Pivot Percentile (PPI)", "Bootstrapped SE")
results_frame = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(results_frame) = columns

for (g in gamma) {
  numsims <- 100
  betatrue <- 0
  n <- 500
  levels <- c(0.20)
  numbs <- 1000
  betahat <- rep(NA, numsims)
  results_normal <- 0
  results_epi <- 0
  results_ppi <- 0
  results_sebs <- 0
  
  for (m in seq_len(numsims)) {
    df <- ns_dgp(gamma=g, n = n)
    iv <- ivreg(data = df, y ~ 0 + d | 0 +
                  z)
    betahat[m] <- iv$coefficients["d"]
    se <- summary(iv)$coefficients["d", "Std. Error"]
    
    # Standard normal asymptotic approximation for comparison
    for (a in seq_along(levels)) {
      lb <- betahat[m] - se * qnorm(1 - levels[a] / 2)
      ub <- betahat[m] + se * qnorm(1 - levels[a] / 2)
      if ((lb <= betatrue) & (ub >= betatrue)) {
        results_normal <- results_normal + 1
      }
    }
    
    # Bootstrap Monte Carlo simulation
    betaboot <- rep(NA, numbs)
    for (b in seq_len(numbs)) {
      df_boot <- df[sample(seq_len(n), n, replace = TRUE), ]
      betaboot[b] <- ivreg(data = df_boot, y ~ 0 + d | 0 +
                             z)$coefficients["d"]
    }
    sebs[m] <- sd(betaboot)
    
    # Use quantiles from the simulation to construct EPI and PPI
    qlower <- quantile(betaboot, levels / 2)
    qupper <- quantile(betaboot, 1 - levels / 2)
    
    for (a in seq_along(levels)) {
      lb_epi <- qlower[a]
      ub_epi <- qupper[a]
      if ((lb_epi <= betatrue) & (ub_epi >= betatrue)) {
        results_epi <- results_epi + 1
      }
      lb_ppi <- 2 * betahat[m] - qupper[a]
      ub_ppi <- 2 * betahat[m] - qlower[a]
      if ((lb_ppi <= betatrue) & (ub_ppi >= betatrue)) {
        results_ppi <- results_ppi + 1
      }
      lb_sebs <- betahat[m] - qnorm(.975) * sebs[m]
      ub_sebs <- betahat[m] + qnorm(.975) * sebs[m]
      if ((lb_sebs <= betatrue) & (ub_sebs >= betatrue)) {
        results_sebs <- results_sebs + 1
      }
    }
  }

  results <- c(g*numsims, results_normal, results_epi,
               results_ppi, results_sebs)
  results <- results / numsims
  print(results)
  results_frame[nrow(results_frame) + 1,] <- results

}

print(results_frame)

