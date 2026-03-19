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
