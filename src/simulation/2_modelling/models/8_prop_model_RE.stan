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
  
  // baseline
  vector<lower=0>[T] y_N_tot; // total population among locations
  vector<lower=0>[I] N0; // baseline population at each location
  
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
  array[T-1] vector[I-1] logit_p; // population proportions in each oblast
  real mu_p0;  
  vector[T-1] mu_p_t; // time effect
  vector[I-1] mu_p_i; // location effect
  real log_sigma_p_t;
  real log_sigma_p_i;  
  real log_sigma_p;
  
  // Facebook
  vector<lower=0>[T * I] p_F; // Facebook user ratios
  real mu_p_F0;
  vector[T] mu_p_F_t; // time effect
  vector[I] mu_p_F_i; // location effect
  real log_sigma_p_F_t;
  real log_sigma_p_F_i;
  real log_sigma_p_F;
  
  // Instagram
  vector<lower=0>[T * I] p_GF; 
  real mu_p_GF;
  real log_sigma_p_GF;
  
  real log_kappa_F; // over-dispersion
  real log_kappa_G;
  
}
transformed parameters {
  array[T-1] simplex[I] p;
  vector<lower=0>[T * I] N; // population estimates
  vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios
  vector<lower=0>[T * I] p_G = p_F .* p_GF;
  vector[T*I] mu_p_F = mu_p_F0 + to_vector(mu_p_F_t * mu_p_F_i');
  matrix[T-1,I-1] mu_p = mu_p0 + mu_p_t * mu_p_i';  
  
  
  // population process model
  p[1] = softmax(append_row(logit_p[1],1));
  N[ti_N0] = N0;
  for (t in 2 : T) {
    if(t < T){
      p[t] = softmax(append_row(logit_p[t],1));
    }
    N[t_slice(t, I)] = y_N_tot[t] * p[t-1];
  }
  
  // observation ratio of Instagram to Facebook
  FG_ratio = p_G[ti_FG] ./ p_F[ti_FG];
}
model {
  // likelihoods
  y_F ~ neg_binomial_2(N[ti_F] .* p_F[ti_F],exp(-log_kappa_F));
  y_G ~ neg_binomial_2(N[ti_G] .* p_G[ti_G],exp(-log_kappa_G));
  
  p_F ~ lognormal(mu_p_F, exp(log_sigma_p_F));
  p_GF ~ lognormal(mu_p_GF,exp(log_sigma_p_GF));
  
  // population growth rates
  for(t in 1:(T-1)){
    logit_p[t] ~ normal(mu_p[t,],exp(log_sigma_p));
  }
  mu_p_t ~ normal(0, exp(log_sigma_p_t));
  mu_p_i ~ normal(0, exp(log_sigma_p_i));
  mu_p0 ~ normal(0,5);  
  
  // priors:  Facebook user ratio
  mu_p_F_t ~ normal(0, exp(log_sigma_p_F_t));
  mu_p_F_i ~ normal(0, exp(log_sigma_p_F_i));
  mu_p_F0 ~ normal(0,5);
  
  // priors:  Instagram user ratio
  mu_p_GF ~ normal(0,5);
}
generated quantities {
  // in-sample posterior predictive check
  array[n_F] int<lower=0> F_hat;
  array[n_G] int<lower=0> G_hat;
  
  F_hat = neg_binomial_2_rng(N[ti_F] .* p_F[ti_F],exp(-log_kappa_F));
  G_hat = neg_binomial_2_rng(N[ti_G] .* p_G[ti_G],exp(-log_kappa_G));
  
  // out-of-sample leave-one-out cross-validation
  vector[n_F] log_lik_F;
  for (n in 1 : n_F) {
    log_lik_F[n] = neg_binomial_2_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]],exp(-log_kappa_F));
  }
  
  vector[n_G] log_lik_G;
  for (n in 1 : n_G) {
    log_lik_G[n] = neg_binomial_2_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]],exp(-log_kappa_F));
  }
}
