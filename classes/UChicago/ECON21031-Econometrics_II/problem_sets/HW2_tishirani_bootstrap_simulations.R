library(ggplot2)
library(ivmte)

set.seed(2304)

columns= c("Target", "MTE Specification", "Point Estimate", "Bootstrapped SE", 
           "Standard Approximation CI", "Efron's Percentile (EPI)", "Pivot Percentile (PPI)")
results_frame = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(results_frame) = columns

targets = list('ate', 'atu', 'att')
specs = list("linear", "quadratic")


numbs <- 500
levels <- c(0.1)

for (targ in targets) {
  for (spec in specs) {
    
    if (spec == "linear") {
      ivmt <- ivmte(data = lmw,
                       target = targ,
                       m0 = ~ u,
                       m1 = ~ u,
                       ivlike = hours_worked ~ connected + treat_low + treat_med + treat_full,
                       propensity = connected ~ treat_low + treat_med + treat_full,
                       noisy = FALSE)      
    }
    else {
      print("help")
      ivmt <- ivmte(data = lmw,
                       target = targ,
                       m0 = ~ u + u^2,
                       m1 = ~ u + u^2,
                       ivlike = hours_worked ~ connected + treat_low + treat_med +
                         treat_full,
                       propensity = connected ~ treat_low + treat_med + treat_full,
                       noisy = FALSE)
    }
    betahat <- ivmt$point.estimate
      
    # Bootstrap Monte Carlo simulation
    betaboot <- rep(NA, numbs)
    for (b in seq_len(numbs)) {
      df_boot <- lmw[sample(seq_len(4368), replace = TRUE), ]
      if (spec == "linear") {
        ivmt_bs <- ivmte(data = df_boot,
                      target = targ,
                      m0 = ~ u,
                      m1 = ~ u,
                      ivlike = hours_worked ~ connected + treat_low + treat_med + treat_full,
                      propensity = connected ~ treat_low + treat_med + treat_full,
                      noisy = FALSE)      
      }
      else {
        ivmt_bs <- ivmte(data = df_boot,
                      target = targ,
                      m0 = ~ u + u^2,
                      m1 = ~ u + u^2,
                      ivlike = hours_worked ~ connected + treat_low + treat_med +
                        treat_full,
                      propensity = connected ~ treat_low + treat_med + treat_full,
                      noisy = FALSE)
      }
      betaboot[b] <- ivmt_bs$point.estimate
    }
    sebs <- sd(betaboot)
    
    # Use quantiles from the simulation to construct EPI and PPI
    qlower <- quantile(betaboot, levels / 2)
    qupper <- quantile(betaboot, 1 - levels / 2)
    
    for (a in seq_along(levels)) {
      lb_epi <- qlower[a]
      ub_epi <- qupper[a]
      results_epi <- paste0("[", round(lb_epi[[1]], digits=2), ",",
                            round(ub_epi[[1]], digits=2), "]")
      
      lb_ppi <- 2 * betahat - qupper[a]
      ub_ppi <- 2 * betahat - qlower[a]
      results_ppi <- paste0("[", round(lb_ppi[[1]], digits=2), ",",
                            round(ub_ppi[[1]], digits=2), "]")
      
      lb_sebs <- betahat - qnorm(.975) * sebs
      ub_sebs <- betahat + qnorm(.975) * sebs
      results_sebs <- paste0("[", round(lb_sebs, digits=2), ",",
                             round(ub_sebs, digits=2), "]")
    }
    results <- c(targ, spec, betahat, sebs, results_epi,
                 results_ppi, results_sebs)
    results_frame[nrow(results_frame) + 1,] <- results
  }
}

print(results_frame)