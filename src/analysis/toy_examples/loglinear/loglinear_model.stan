data {
  int<lower=0> N;          // Number of observations
  vector[N] orig;     
  vector[N] dest;     
}

parameters {
  real alpha;              // Intercept
  real beta_orig;     // Coefficient for orig
  real beta_dest;     // Coefficient for dest
}

model {
  // Priors
  alpha ~ normal(0, 1);
  beta_orig ~ normal(0, 1);
  beta_dest ~ normal(0, 1);
 
  // Likelihood
  lambda = exp(alpha + beta_orig * orig + beta_dest * dest);
  

  // Poisson likelihood for the unobserved variable Freq
  target += poisson_log_lpmf(lambda | alpha + beta_orig * orig + beta_dest * dest);
}

generated quantities {
  int<lower=0> Freq_pred[N];  // Simulated values for Freq

  for (i in 1:N) {
    Freq_pred[i] = poisson_rng(lambda[i]);   // Values from Poisson based on rate lambda
  }
}
