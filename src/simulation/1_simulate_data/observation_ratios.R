# simulation replications
n <- 1e3
results <- data.frame(
  rep = 1:n, N = NA, p1 = NA, p2 = NA, M = NA,
  true_ratio = NA, obs_ratio = NA, error = NA
)

# simulations
for (i in 1:n) {
  # population
  N <- sample(1e3:1e5, 1)

  # detection rates
  p1 <- runif(1, 0.01, 0.25)
  p2 <- runif(1, 0.01, 0.15)

  true_ratio <- p1 / p2

  # number of repeated measurements
  M <- sample(1:7, 1)

  # imperfect observations
  y1 <- rpois(M, N * p1)
  y2 <- rpois(M, N * p2)
  obs_ratio <- mean(y1) / mean(y2)

  # percent error
  error <- abs(obs_ratio - true_ratio) / true_ratio

  # save results to table
  results[i, ] <- c(i, N, p1, p2, M, true_ratio, obs_ratio, error)
}

# define sigma (value that 95% of absolute percent errors are less than)
sigma <- quantile(results$error, probs = c(0.95))
print(sigma)

# check that 95% of observed ratios are within +/- sigma % of true ratio
mean(results$obs_ratio >= (results$true_ratio - results$true_ratio * sigma) & 
       results$obs_ratio <= (results$true_ratio + results$true_ratio * sigma))


#---- check lognormal parameterisation ----#

# select random ratio
mu <- sample(results$true_ratio, 1)

# draw values from lognormal with sd = sigma / 2
x <- rlnorm(1e5, log(mu), sigma / 2)

# plot density with mu
plot(density(x))
abline(v = mu, col = "red")

# check that 95% of random draws (i.e. observed ratios; x) are within +/- sigma % of mu
mean(x >= (mu - mu * sigma) & x <= (mu + mu * sigma))

abline(v = mu + mu * sigma, col = "red", lty = 2)
abline(v = mu - mu * sigma, col = "red", lty = 2)


