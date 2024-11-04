functions {
  
  // slice ti vector for specific year t
  array[] int t_slice(int t, int I){
    int a = 1 + t * I - I;  // starting index for year t
    int b = t * I;  // ending index for year t
    int n = b - a + 1;  // number of elements for year t
    
    array[n] int result;  // result of indexes
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
  
  // slice ti vector for specific previous year t-1
  array[] int t_slice_lag(int t, int I){
    int a = 1 + t * I - 2 * I;  // starting index for year t-1
    int b = t * I - I;  // ending index for year t-1
    int n = b - a + 1;  // number of elements for year t-1
    
    array[n] int result;  // result of indexes
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
}

data {
  
  // dimensions
  int<lower=0> T;  // number of weeks
  int<lower=0> I;  // number of locations
  int<lower=0> K;  // number of covariates on population growth rates

  // baseline
  vector<lower=0>[T] N_tot;  // total population among locations
  vector<lower=0>[I] N0;  // baseline population at each location
  vector<lower=0, upper=1>[I] p0_F; // baseline detection
  vector<lower=0, upper=1>[I] p0_G; // baseline detection for G
  
  // population covariates
  matrix[T*I,K] X;  // covariates on population growth rates
  
  // Facebook data
  int<lower=0> n_F;  // total sample size for F
  array[n_F] int<lower=0> y_F;  // Facebook daily active users
  array[n_F] int<lower=0> ti_F;  // ti (year, location) index for F

  // Instagram data
  int<lower=0> n_G;  // total sample size for G
  array[n_G] int<lower=0> y_G;  // Instagram daily active users
  array[n_G] int<lower=0> ti_G;  // ti (year, location) index for G

  // indexing for long format
  array[T*I] int<lower=0> tt; 
  array[T*I] int<lower=0> ii;
  array[I] int<lower=0> ti_N0;
  array[T*I-I] int<lower=0> ti_N;
  array[T*I-I] int<lower=0> ti_N_lag;
}

parameters {
  
  // population
  vector[T*I] r;  // population growth rates
  real<lower=0> sigma_r;  // variation in growth rates
  real alpha_r;  // intercept for population growth rates
  vector[K] beta_r; // covariate effects on growth rates

  // Facebook
  real alpha_p_F; // intercept
  vector[T] delta_p_F;  // random effect (by week) of baseline detection at each location
  real mu_delta_p_F;  // average effect of baseline detection
  real<lower=0> sd_delta_p_F;  // variation in effect of baseline detection among weeks

  // Instagram
  real alpha_p_G; // intercept
  vector[T] delta_p_G;  // random effect (by week) of baseline detection at each location
  real mu_delta_p_G;  // average effect of baseline detection
  real<lower=0> sd_delta_p_G;  // variation in effect of baseline detection among weeks
}

transformed parameters {
  
  vector<lower=0>[T*I] N;  // population estimates
  vector[T*I] mu_r;  // expected growth rates
  vector<lower=0>[T*I] p_F; // Facebook user ratios
  vector<lower=0>[T*I] p_G; // Instagram user ratios
  
  
  // population process model
  N[ti_N0] = N0;
  for(t in 2:T){
    N[t_slice(t,I)] = N[t_slice_lag(t,I)] .* exp(r[t_slice(t,I)]);
  }
  
  // regression on population growth rates
  mu_r = alpha_r + X * beta_r;
  
  // regression on Facebook active penetration
  p_F = exp(alpha_p_F + delta_p_F[tt] .* log(p0_F[ii]));

  // regression on Instagram active penetration
  p_G = exp(alpha_p_G + delta_p_G[tt] .* log(p0_G[ii]));
}


model {
  
  // likelihoods (observation models)
  y_F ~ poisson(N[ti_F] .* p_F[ti_F]);
  y_G ~ poisson(N[ti_G] .* p_G[ti_G]);

  // total population constraint
  for(t in 1:T){
    sum(N[t_slice(t,I)]) ~ lognormal(log(N_tot[t]), 1e-3);
  }

  // population growth rates
  r ~ normal(mu_r, sigma_r);
  
  // priors:  population
  sigma_r ~ cauchy(0, 1);
  alpha_r ~ normal(0, 1);
  beta_r ~ normal(0, 5);

  // priors:  Facebook user ratio
  alpha_p_F ~ normal(0, 1); 
  delta_p_F ~ normal(mu_delta_p_F, sd_delta_p_F);
  mu_delta_p_F ~ normal(1, 1);
  sd_delta_p_F ~ cauchy(0, 1);

  // priors:  Instagram user ratio
  alpha_p_G ~ normal(0, 1); 
  delta_p_G ~ normal(mu_delta_p_G, sd_delta_p_G);
  mu_delta_p_G ~ normal(1, 1);
  sd_delta_p_G ~ cauchy(0, 1);
}
