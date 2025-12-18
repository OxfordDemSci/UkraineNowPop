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
  // vector[T * I] r; // population growth rates
  // array[T-1] simplex[I] p; // population proportions in each oblast
  array[T-1] vector[I-1] logit_p; // population proportions in each oblast
  // real<lower=0> sigma_p;
  real mu_p0;  
  vector[T-1] mu_p_t; // time effect
  vector[I-1] mu_p_i; // location effect
  real log_sigma_p_t;
  real log_sigma_p_i;  
  real log_sigma_p;
  
  // Facebook
  vector<lower=0>[T * I] p_F; // Facebook user ratios
  // matrix[T,I] p_F;
  // real mu_p_F;
  real mu_p_F0;
  // vector[T] mu_p_F; // expected value
  vector[T] mu_p_F_t; // time effect
  vector[I] mu_p_F_i; // location effect
  real log_sigma_p_F_t;
  real log_sigma_p_F_i;
  // real mu_mu_p_F;
  // real<lower=0> sigma_mu_p_F;
  // real<lower=0> sigma_p_F; // residual variation
  real log_sigma_p_F;
  
  // Instagram
  // vector<lower=0,upper=1>[T * I] p_GF; 
  vector<lower=0>[T * I] p_GF; 
  // matrix<lower=0,upper=1>[T,I] p_GF;
  // real<lower=0,upper=1> mu_p_GF;
  real mu_p_GF;
  // vector[I] mu_p_G; // expected value
  // real mu_mu_p_G;
  // real<lower=0> sigma_mu_p_G;
  // real<lower=0> sigma_p_GF; // residual variation
  real log_sigma_p_GF;
  
  real log_kappa_F; // over-dispersion
  real log_kappa_G;
  
}
transformed parameters {
  array[T-1] simplex[I] p;
  vector<lower=0>[T * I] N; // population estimates
  // vector<lower=0>[T] N_tot; // total population at each time step
  vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios
  vector<lower=0>[T * I] p_G = p_F .* p_GF;
  // matrix<lower=0>[T,I] p_G = p_F .* p_GF;
  // matrix[T,I] mu_p_F = mu_p_F_t * mu_p_F_i';
  vector[T*I] mu_p_F = mu_p_F0 + to_vector(mu_p_F_t * mu_p_F_i');
  matrix[T-1,I-1] mu_p = mu_p0 + mu_p_t * mu_p_i';  
  
  
  // population process model
  p[1] = softmax(append_row(logit_p[1],1));
  N[ti_N0] = N0;
  for (t in 2 : T) {
    if(t < T){
      p[t] = softmax(append_row(logit_p[t],1));
    }
    // N[t_slice(t, I)] = N[t_slice_lag(t, I)] .* r[t_slice(t, I)];
    N[t_slice(t, I)] = y_N_tot[t] * p[t-1];
  }
  
  // total population
  // for (t in 1 : T) {
    // N[t_slice(t, I)] = 
    // N_tot[t] = sum(N[t_slice(t, I)]);
  // }
  
  // observation ratio of Instagram to Facebook
  FG_ratio = p_G[ti_FG] ./ p_F[ti_FG];
}
model {
  // likelihoods
  // y_F ~ poisson(N[ti_F] .* p_F[ti_F]);
  // y_G ~ poisson(N[ti_G] .* p_G[ti_G]);
  y_F ~ neg_binomial_2(N[ti_F] .* p_F[ti_F],exp(-log_kappa_F));
  y_G ~ neg_binomial_2(N[ti_G] .* p_G[ti_G],exp(-log_kappa_G));
  
  // y_FG_ratio ~ lognormal(log(FG_ratio), 0.02 / 2);
  
  // y_N_tot ~ lognormal(log(N_tot) - (0.01 / 2)^2/2, 0.01 / 2);
  
  // observation models
  // p_F ~ lognormal(mu_p_F[tt], sigma_p_F);
  // p_G ~ lognormal(mu_p_G[ii], sigma_p_G);
  // p_F ~ lognormal(mu_p_F, sigma_p_F);
  // p_F ~ lognormal(mu_p_F, sigma_p_F);
  p_F ~ lognormal(mu_p_F, exp(log_sigma_p_F));
  // p_F ~ beta(sigma_p_F*mu_p_F,sigma_p_F*(1-mu_p_F));
  // p_GF ~ beta(sigma_p_GF*mu_p_GF,sigma_p_GF*(1-mu_p_GF));
  // p_GF ~ beta(exp(log_sigma_p_GF)*mu_p_GF,exp(log_sigma_p_GF)*(1-mu_p_GF));
  p_GF ~ lognormal(mu_p_GF,exp(log_sigma_p_GF));
  
  // population growth rates
  // r ~ lognormal(0, 0.1 / 2);
  // logit_p[1] ~ normal(0,10);
  // for(t in 2:(T-1)){
  //   // logit_p[t] ~ normal(logit_p[t-1],sigma_p);
  //   logit_p[t] ~ normal(logit_p[t-1],exp(log_sigma_p));
  // }
  for(t in 1:(T-1)){
    logit_p[t] ~ normal(mu_p[t,],exp(log_sigma_p));
  }
  // sigma_p ~ normal(0,5)T[0,];
  mu_p_t ~ normal(0, exp(log_sigma_p_t));
  mu_p_i ~ normal(0, exp(log_sigma_p_i));
  mu_p0 ~ normal(0,5);  
  
  // priors:  Facebook user ratio
  // mu_p_F ~ normal(mu_mu_p_F, sigma_mu_p_F);
  // mu_mu_p_F ~ normal(0, 5);
  // sigma_mu_p_F ~ normal(0, 1);
  // mu_p_F ~ uniform(0, 1);
  // mu_p_F ~ normal(0, 5);
  // mu_p_F_t ~ normal(0, 5);
  // mu_p_F_i ~ normal(0, 5);
  mu_p_F_t ~ normal(0, exp(log_sigma_p_F_t));
  mu_p_F_i ~ normal(0, exp(log_sigma_p_F_i));
  mu_p_F0 ~ normal(0,5);
  
  // sigma_p_F ~ normal(0, 1)T[0,];
  
  // priors:  Instagram user ratio
  // mu_p_G ~ normal(mu_mu_p_G, sigma_mu_p_G);
  // mu_mu_p_G ~ normal(0, 5);
  // sigma_mu_p_G ~ normal(0, 1);
  // mu_p_GF ~ uniform(0, 1);
  // mu_p_GF ~ beta(mu_p_GF_n*mu_p_GF_mu,mu_p_GF_n*(1-mu_p_GF_mu));
  mu_p_GF ~ normal(0,5);
  
  // sigma_p_GF ~ normal(0, 5)T[0,];
}
generated quantities {
  // in-sample posterior predictive check
  array[n_F] int<lower=0> F_hat;
  array[n_G] int<lower=0> G_hat;
  
  // F_hat = poisson_rng(N[ti_F] .* p_F[ti_F]);
  // G_hat = poisson_rng(N[ti_G] .* p_G[ti_G]);
  F_hat = neg_binomial_2_rng(N[ti_F] .* p_F[ti_F],exp(-log_kappa_F));
  G_hat = neg_binomial_2_rng(N[ti_G] .* p_G[ti_G],exp(-log_kappa_G));
  
  // out-of-sample leave-one-out cross-validation
  vector[n_F] log_lik_F;
  for (n in 1 : n_F) {
    // log_lik_F[n] = poisson_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]]);
    log_lik_F[n] = neg_binomial_2_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]],exp(-log_kappa_F));
  }
  
  vector[n_G] log_lik_G;
  for (n in 1 : n_G) {
    // log_lik_G[n] = poisson_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]]);
    log_lik_G[n] = neg_binomial_2_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]],exp(-log_kappa_G));
  }
}
