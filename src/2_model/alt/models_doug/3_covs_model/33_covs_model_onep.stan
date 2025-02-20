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
  
  // baseline
  vector<lower=0>[T] y_N_tot; // total population among locations
  vector<lower=0>[I] N0; // baseline population at each location
  
  // population covariates
  matrix[T * I, K_r] X_r; // covariates on population growth rates
  matrix[T * I, K_p] X_p; // covariates on Facebook detection rates (note: these are weekly but could be daily)
  
  // Facebook data
  int<lower=0> n_F; // total sample size for F
  array[n_F] int<lower=0> y_F; // Facebook daily active users
  
  // Instagram data
  int<lower=0> n_G; // total sample size for G
  array[n_G] int<lower=0> y_G; // Instagram daily active users
  
  // observation ratio
  int<lower=0> n_FG; // total sample size with F and G
  vector<lower=0>[n_FG] y_FG_ratio; // ratio of G to F
  
  // year x location indexing for long format
  array[T * I] int<lower=0> tt; // year index for ti vector
  array[T * I] int<lower=0> ii; // location index for ti vector
  
  array[I] int<lower=0> ti_N0; // ti (year, location) index for N0
  array[T * I - I] int<lower=0> ti_N; // ti (year, location) index for N[t]
  array[T * I - I] int<lower=0> ti_N_lag; // ti (year, location) index for N[t-1]
  
  array[n_F] int<lower=0> ti_F; // ti (year, location) index for F
  array[n_G] int<lower=0> ti_G; // ti (year, location) index for G
  array[n_FG] int<lower=0> ti_FG; // ti (year, location) index for F and G
}
parameters {
  // population
  vector[T * I] r; // population growth rates
  real alpha_r; // random intercept for population growth rates
  vector[K_r] beta_r; // covariate effects on growth rates
  real<lower=0> sigma_r; // variation in growth rates
  
  // Facebook and Instagram
  vector<lower=0>[T * I] p_F; // Facebook detection rates
  vector<lower=0>[T * I] p_G; // Instagram detection rates
  
  real alpha_p; // intercept for Facebook detection rates
  real phi_p; // intercept offset for Instagram detection rates
  vector[K_p] beta_p; // covariate effects on Instagram detection rates
  real<lower=0> sigma_p_F; // residual variation
  real<lower=0> sigma_p_G; // residual variation
  
  vector[T] delta_p;
  real<lower=0> sigma_delta_p;
  
  vector[I] gamma_p;
  real<lower=0> sigma_gamma_p;
}
transformed parameters {
  vector<lower=0>[T * I] N; // population estimates
  vector<lower=0>[T] N_tot; // total population at each time step
  vector[T * I] mu_r; // expected growth rates
  vector[T * I] mu_p; // expected Facebook detection rates
  vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios
  
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
  mu_p = alpha_p + delta_p[tt] + gamma_p[ii] + X_p * beta_p;
  
  // observation ratio of Instagram to Facebook
  FG_ratio = p_G[ti_FG] ./ p_F[ti_FG];
}
model {
  // likelihoods
  y_F ~ poisson(N[ti_F] .* p_F[ti_F]);
  y_G ~ poisson(N[ti_G] .* p_G[ti_G]);
  
  y_N_tot ~ lognormal(log(N_tot), 0.01 / 2);
  
  // observation models
  p_F ~ lognormal(mu_p, sigma_p_F);
  p_G ~ lognormal(mu_p + phi_p, sigma_p_G);
  
  // population growth rates
  r ~ lognormal(mu_r, sigma_r);
  
  // priors:  population
  alpha_r ~ normal(0, 5);
  beta_r ~ normal(0, 1);
  sigma_r ~ normal(0, 1);
  
  // priors:  Facebook and Instagram detection rate
  FG_ratio ~ lognormal(log(y_FG_ratio), 0.05 / 2);
  
  alpha_p ~ normal(0, 5);
  phi_p ~ normal(0, 1);
  beta_p ~ normal(0, 1);
  sigma_p_F ~ normal(0, 1);
  sigma_p_G ~ normal(0, 1);
  
  delta_p ~ normal(0, sigma_delta_p);
  sigma_delta_p ~ normal(0, 1);
  
  gamma_p ~ normal(0, sigma_gamma_p);
  sigma_gamma_p ~ normal(0, 1);
}
generated quantities {
  // in-sample posterior predictive check
  array[n_F] int<lower=0> F_hat;
  array[n_G] int<lower=0> G_hat;
  
  F_hat = poisson_rng(N[ti_F] .* p_F[ti_F]);
  G_hat = poisson_rng(N[ti_G] .* p_G[ti_G]);
  
  // out-of-sample leave-one-out cross-validation
  vector[n_F] log_lik_F;
  for (n in 1 : n_F) {
    log_lik_F[n] = poisson_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]]);
  }
  
  vector[n_G] log_lik_G;
  for (n in 1 : n_G) {
    log_lik_G[n] = poisson_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]]);
  }
}
