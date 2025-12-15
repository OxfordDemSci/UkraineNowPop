functions {
  // slice ti vector for specific year t
  array[] int t_slice(int t, int I) {
    int a = 1 + t * I - I; // starting index for year t
    int b = t * I; // ending index for year t
    int n = b - a + 1; // number of elements for year t
    
    array[n] int result; // result of indexes
    for (i in 1 : n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
  
  // slice ti vector for specific previous year t-1
  array[] int t_slice_lag(int t, int I) {
    int a = 1 + t * I - 2 * I; // starting index for year t-1
    int b = t * I - I; // ending index for year t-1
    int n = b - a + 1; // number of elements for year t-1
    
    array[n] int result; // result of indexes
    for (i in 1 : n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
}
data {
  // dimensions
  int<lower=0> T; // number of weeks
  int<lower=0> I; // number of locations
  int<lower=0> K_r; // number of covariates on population growth rates
  int<lower=0> K_p; // number of covariates on Facebook detection rates
  
  // population
  vector<lower=0>[T] y_N_tot; // total population among locations
  vector<lower=0>[I] N0; // baseline population at each location
  
  vector<lower=0>[I] N1; // cod-ps population at each location for time t
  real<lower=0> ci_N1; // confidence in N1. (i.e. 0.95 probability that the true N[t_N1] is within ci_N1*100 percent of N1)
  
  // population covariates
  matrix[T * I, K_r] X_r; // covariates on population growth rates
  matrix[T * I, K_p] X_p; // covariates on Facebook detection rates (note: these are weekly but could be daily)
  
  // Facebook data
  int<lower=0> n_F; // total sample size for F
  array[n_F] real<lower=0> y_F; // Facebook daily active users
  
  // Instagram data
  int<lower=0> n_G; // total sample size for G
  array[n_G] real<lower=0> y_G; // Instagram daily active users
  
  // year x location indexing for long format
  array[T * I] int<lower=0> tt; // year index for ti vector
  array[T * I] int<lower=0> ii; // location index for ti vector
  
  array[I] int<lower=0> ti_N0; // ti (year, location) index for N0
  array[I] int<lower=0> ti_N1; // ti (year, location) index for N1
  
  array[T * I - I] int<lower=0> ti_N; // ti (year, location) index for N[t]
  
  array[n_F] int<lower=0> ti_F; // ti (year, location) index for F
  array[n_G] int<lower=0> ti_G; // ti (year, location) index for G
}
parameters {
  // population
  vector[T * I] r; // population growth rates
  real alpha_r; // random intercept for population growth rates
  vector[K_r] beta_r; // covariate effects on growth rates
  real log_sigma_r; // variation in growth rates
  
  // Facebook and Instagram
  real alpha_p; // intercept for Facebook detection rates
  real phi_p; // intercept offset for Instagram detection rates
  vector[K_p] beta_p; // covariate effects on Instagram detection rates
  real log_sigma_F; // residual variation
  real log_sigma_G; // residual variation
  
  vector[T] delta_p;
  real log_sigma_delta_p;
  
  vector[I] gamma_p;
  real log_sigma_gamma_p;
}
transformed parameters {
  vector<lower=0>[T * I] N; // population estimates
  vector<lower=0>[T] N_tot; // total population at each time step
  vector[T * I] mu_r; // expected growth rates
  vector<lower=0>[T * I] p_F;
  vector<lower=0>[T * I] p_G;
  vector[T * I] log_p_F;
  vector[T * I] log_p_G;
  vector[n_F + n_G] log_lik;

  // population process model
  N[ti_N0] = N0;
  for (t in 2 : T) {
    N[t_slice(t, I)] = N[t_slice_lag(t, I)] .* r[t_slice(t, I)];
  }
  
  // total population
  for (t in 1 : T) {
    N_tot[t] = sum(N[t_slice(t, I)]);
  }
  
  // regression on population growth rates
  mu_r = alpha_r + X_r * beta_r;
  
  // regression on Facebook detection rates
  log_p_F = alpha_p + delta_p[tt] + gamma_p[ii] + X_p * beta_p;
  p_F = exp(log_p_F);
  
  log_p_G = p_F + phi_p;
  p_G = exp(log_p_G);

  // elements of log-likelihood (case-wise log_lik required for LOO-CV)
  for(i in 1:n_F){
    log_lik[i] = lognormal_lpdf(y_F[i] | log(N[ti_F[i]] .* p_F[ti_F[i]]), exp(log_sigma_F));
  }
  for(i in 1:n_G){
    log_lik[i + n_F] = lognormal_lpdf(y_G[i] | log(N[ti_G[i]] .* p_G[ti_G[i]]), exp(log_sigma_G));
  }
}
model {
  // log-likelihood
  target += sum(log_lik);
  
  // empirical priors
  N_tot ~ lognormal(log(y_N_tot), 0.01 / 2);
  N[ti_N1] ~ lognormal(log(N1), ci_N1 / 2);
  
  // population growth rates
  r ~ lognormal(mu_r, exp(log_sigma_r));
  
  // random effects
  delta_p ~ normal(0, exp(log_sigma_delta_p));
  gamma_p ~ normal(0, exp(log_sigma_gamma_p));
}
generated quantities {
  // in-sample posterior predictive check
  array[n_F] real<lower=0> F_hat;
  array[n_G] real<lower=0> G_hat;

  // observation models
  F_hat = lognormal_rng(log(N[ti_F] .* p_F[ti_F]), exp(log_sigma_F));
  G_hat = lognormal_rng(log(N[ti_G] .* p_G[ti_G]), exp(log_sigma_G));    
}
