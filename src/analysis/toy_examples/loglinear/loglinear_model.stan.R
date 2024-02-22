#install.packages("rstan")
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)



modelString <- "data {
  int<lower=1> N;      // Number of observations
  int<lower=1> K;      // Number of geo
  int<lower=1, upper=K> orig[N];  
  int<lower=1, upper=K> dest[N];  
}

parameters {
  real alpha;  // Intercept
  vector[K] beta1;  // Coefficients for orig
  vector[K] beta2;  // Coefficients for dest
}

model {
  vector[N] lambda;

  for (i in 1:N) {
    lambda[i] = alpha + beta1[orig[i]] + beta2[dest[i]];
  }

}

generated quantities {
  int<lower=0> Freq_pred[N];  // Simulated values for Freq

  for (i in 1:N) {
    Freq_pred[i] = poisson_rng(lambda[i]);   // Values from Poisson based on rate lambda
  }
}
"

writeLines(modelString, "src/analysis/toy_examples/loglinear/loglinear_model.stan")

fit_lognormal <- stan(file = "src/analysis/toy_examples/loglinear/loglinear_model.stan",
               data = sim_data_list,
               iter = 500, 
               thin = 1, 
               warmup = 100,
               verbose = FALSE, 
               chains = 3, cores = 3, 
               seed = 26)

print(fit_lognormal)
plot(fit_lognormal)

plot(fit_lognormal, plotfun = "dens", 
     pars = c("Freq_pred[1]"), inc_warmup = F)
