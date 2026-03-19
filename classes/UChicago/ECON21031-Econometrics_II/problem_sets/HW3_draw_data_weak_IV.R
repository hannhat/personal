library(ivreg)

set.seed(43)

drawdata <- function(n = 1000, betatrue = 1) {
  u <- runif(n, min = -2, max = 2)
  x <- rnorm(n, mean = 2, sd = 1)
  y <- 1 + betatrue * x + u
  return(data.frame(y = y, x = x))
}

numsims <- 5000
results <- data.frame(bhat = rep(NA, numsims), type = "Actual (infeasible)")

for (m in seq_len(numsims)) {
  df <- drawdata()
  results[m, "bhat"] <- lm(data = df, y ~ x)$coeff["x"]
}

ns_dgp <- function(gamma, corr_uv = .99, n = 1000) {
  vc <- cbind(c(1, corr_uv), c(corr_uv, 1))
  uv <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = vc) # bivariate normal (U,V)
  z <- rnorm(n, mean = 0, sd = 1) # instrument
  d <- gamma * z + uv[, 2] # endogenous variable
  y <- uv[, 1] # y = u = 0*d + u, so true coefficient is zero
  return(data.frame(y = y, d = d, z = z)) 
}
  
iv_mc <- function(gamma, corr_uv = .99, n = 1000, m = 10000) {
  results <- data.frame(gamma = rep(gamma, m), n = rep(n, m), betahativ =
                          rep(NA, m))
  for (mm in 1:m) {
    df <- ns_dgp(gamma, corr_uv, n)
    results[mm, "betahativ"] <- ivreg(data = df, y ~ 0 + d | 0 +
                                        z)$coefficients["d"]
  }
  return(results)
}