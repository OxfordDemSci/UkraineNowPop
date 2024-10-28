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

  // population covariates
  matrix[T*I,K] X;  // covariates on population growth rates
  
  // Facebook data
  int<lower=0> n_F;  // total sample size for F
  array[n_F] int<lower=0> y_F;  // Facebook daily active users

  // Instagram data
  int<lower=0> n_G;  // total sample size for G
  array[n_G] int<lower=0> y_G;  // Instagram daily active users
  
  // observation ratio
  int<lower=0> n_FG;  // total sample size with F and G
  vector<lower=0>[n_FG] y_FG_ratio;  // ratio of G to F

  // year x location indexing for long format
  array[T*I] int<lower=0> tt;  // year index for ti vector
  array[T*I] int<lower=0> ii;  // location index for ti vector

  array[I] int<lower=0> ti_N0;  // ti (year, location) index for N0
  array[T*I-I] int<lower=0> ti_N;  // ti (year, location) index for N[t]
  array[T*I-I] int<lower=0> ti_N_lag; // ti (year, location) index for N[t-1]

  array[n_F] int<lower=0> ti_F;  // ti (year, location) index for F
  array[n_G] int<lower=0> ti_G;  // ti (year, location) index for G
  array[n_FG] int<lower=0> ti_FG;  // ti (year, location) index for F and G
}

parameters {
  
  // population
  vector[T*I] r;  // population growth rates
  real<lower=0> sigma_r;  // variation in growth rates
  real alpha_r;  // intercept for population growth rates
  vector[K] beta_r; // covariate effects on growth rates

  // Facebook
  vector<lower=0>[T*I] p_F; // Facebook user ratios
  real alpha_p_F; // intercept
  real<lower=0> sigma_p_F; // residual variation

  // Instagram
  vector<lower=0>[T*I] p_G; // Instagram user ratios
  real alpha_p_G; // intercept
  real<lower=0> sigma_p_G; // residual variation
}

transformed parameters {
  
  vector<lower=0>[T*I] N;  // population estimates
  vector[T*I] mu_r;  // expected growth rates
  vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios
  
  
  // population process model
  N[ti_N0] = N0;
  for(t in 2:T){
    N[t_slice(t,I)] = N[t_slice_lag(t,I)] .* r[t_slice(t,I)];
  }
  
  // regression on population growth rates
  mu_r = alpha_r + X * beta_r;
  
  // observation ratio of Instagram to Facebook
  FG_ratio = p_G[ti_FG] ./ p_F[ti_FG];
}


model {
  
  // likelihoods
  y_F ~ poisson(N[ti_F] .* p_F[ti_F]);
  y_G ~ poisson(N[ti_G] .* p_G[ti_G]);
  
  // observation models
  p_F ~ lognormal(alpha_p_F, sigma_p_F);
  p_G ~ lognormal(alpha_p_G, sigma_p_G);
  
  y_FG_ratio ~ lognormal(log(FG_ratio), 0.02/2);

  // total population constraint
  for(t in 1:T){
    N_tot[t] ~ lognormal(log(sum(N[t_slice(t,I)])), 0.01/2);
  }

  // population growth rates
  r ~ lognormal(mu_r, sigma_r);
  
  // priors:  population
  alpha_r ~ normal(0, 1);
  beta_r ~ normal(0, 1);
  sigma_r ~ cauchy(0, 1);

  // priors:  Facebook user ratio
  alpha_p_F ~ normal(0, 5); 
  sigma_p_F ~ cauchy(0, 1); 
  
  // priors:  Instagram user ratio
  alpha_p_G ~ normal(0, 5); 
  sigma_p_G ~ cauchy(0, 1); 
}
