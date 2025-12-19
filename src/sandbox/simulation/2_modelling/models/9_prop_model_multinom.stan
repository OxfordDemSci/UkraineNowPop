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
  array[I] int<lower=0> N0;
  
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
  array[T] vector[I-1] logit_p; // population proportions in each oblast
  real log_sigma_p;
  
  real<lower=-1,upper=1> ar_tot;
  real log_sigma_tot;
  real mu_tot;
  
  // Facebook
  vector[T * I] log_p_F;
  real mu_p_F; // p_F mean
  real log_sigma_p_F;
  vector[I] mu_p_F_i; // location random effect
  real log_sigma_p_F_i;  
  
  // Instagram
  vector[T * I] log_p_GF;
  real mu_p_GF;
  real log_sigma_p_GF;
  
  real log_kappa_F; // over-dispersion scale parameter
  real log_kappa_G;
  
}
transformed parameters {
  array[T] simplex[I] p;
  vector<lower=0>[T * I] N; // population estimates
  vector<lower=0>[T * I] p_F = exp(log_p_F);
  vector<lower=0>[T * I] p_GF = exp(log_p_GF);
  vector<lower=0>[T * I] p_G = p_F .* p_GF;
  vector[n_F+n_G+T] log_lik;
  real log_prior = 0;  
  
  // population process model
  p[1] = softmax(append_row(logit_p[1],1));
  N[ti_N0] = y_N_tot[1] * p[1]; 
  log_lik[n_F+n_G+T] = multinomial_lpmf(N0 | p[1]);  
  for (t in 2 : T) {
    log_prior += normal_lpdf(logit_p[t] | logit_p[t-1],exp(log_sigma_p));
    p[t] = softmax(append_row(logit_p[t],1));
  
    N[t_slice(t, I)] = y_N_tot[t] * p[t];    
    
    // ar(1) model on total population time series
    log_lik[t-1] = lognormal_lpdf(y_N_tot[t] | mu_tot + ar_tot*(log(y_N_tot[t-1]) - mu_tot),exp(log_sigma_tot));
    
    // model on p_F and p_G
    log_prior += normal_lpdf(log_p_F[t_slice(t,I)] | mu_p_F + mu_p_F_i, exp(log_sigma_p_F));
    log_prior += normal_lpdf(log_p_GF[t_slice(t,I)] | mu_p_GF, exp(log_sigma_p_GF));
  }
  
  // model on p_F and p_G, initial time
  log_prior += normal_lpdf(log_p_F[t_slice(1,I)] | mu_p_F + mu_p_F_i, exp(log_sigma_p_F));
  log_prior += normal_lpdf(log_p_GF[t_slice(1,I)] | mu_p_GF, exp(log_sigma_p_GF));
  
  for (n in 1 : n_F) {
    log_lik[n + T-1] = neg_binomial_2_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]],exp(-log_kappa_F));
  }
  for (n in 1 : n_G) {
    log_lik[n + n_F + T-1] = neg_binomial_2_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]],exp(-log_kappa_G));
  }
}
model {
  // likelihoods
  target += sum(log_lik);
  
  // priors
  target += log_prior;
  
  // priors: FB/Instagram user ratios
  mu_p_F_i ~ normal(0, exp(log_sigma_p_F_i));
}
generated quantities {
  // in-sample posterior predictive check
  array[n_F] int<lower=0> F_hat;
  array[n_G] int<lower=0> G_hat;
  array[T] real<lower=0> y_N_tot_hat;
  array[I] int<lower=0> N0_hat;
  
  N0_hat = multinomial_rng(p[1]);
  
  F_hat = neg_binomial_2_rng(N[ti_F] .* p_F[ti_F],exp(-log_kappa_F));
  G_hat = neg_binomial_2_rng(N[ti_G] .* p_G[ti_G],exp(-log_kappa_G));
  
  y_N_tot_hat[1] = lognormal_rng((log(y_N_tot[2])-mu_tot)/ar_tot+mu_tot,exp(log_sigma_tot)/abs(ar_tot));
  y_N_tot_hat[2:T] = lognormal_rng(mu_tot + ar_tot*(log(y_N_tot[1:(T-1)]) - mu_tot),exp(log_sigma_tot));
}
