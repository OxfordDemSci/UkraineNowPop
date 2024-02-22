data {
  int<lower=0> N;      // Number of observations
  vector[N] orig;   
  vector[N] dest;  
  vector[N] tot;   
}

parameters {
  real beta_tot;       // Total
  real beta_orig;      // Coefficient for orig
  real beta_dest;      // Coefficient for dest
  vector<lower=0>[N] lambda;  // Latent variable for Poisson rate
}

model {
  // Priors
  beta_tot ~ gamma(1, 1);
  beta_orig ~ gamma(1, 1);
  beta_dest ~ gamma(1, 1);
  
 lambda ~ normal(0, 1);

}

generated quantities {
  vector[N] Freq_pred;  // Predicted values for Freq

  for (i in 1:N) {
    Freq_pred[i] = poisson_rng(lambda[i]);   // Values from Poisson based on rate lambda
  }
}

