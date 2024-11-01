functions {
  array[] int t_slice(int t, int I){
    int a = 1 + t * I - I;
    int b = t * I;
    int n = b - a + 1;
    
    array[n] int result;
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
  
  array[] int t_slice_lag(int t, int I){
    int a = 1 + t * I - 2 * I;
    int b = t * I - I;
    int n = b - a + 1;
    
    array[n] int result;
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
}

data {
  
  int<lower=0> T;  // number of weeks
  int<lower=0> I;  // number of locations
  int<lower=0> K;  // number of covariates on population growth rates

  vector<lower=0>[T] N_tot;  // total population among locations
  vector<lower=0>[I] N0;  // baseline population at each location

  matrix[T*I,K] X;  // covariates on population growth rates
  
  array[T*I] int<lower=0> tt; 
  array[T*I] int<lower=0> ii;
  array[I] int<lower=0> ti_N0;
  array[T*I-I] int<lower=0> ti_N;
  array[T*I-I] int<lower=0> ti_N_lag;
  
  int<lower=0> n_F;  // total sample size for F
  array[n_F] int<lower=0> y_F;  // Facebook daily active users
  array[n_F] int<lower=0> ti_F;  // ti (year, location) index for F
  vector<lower=0, upper=1>[I] p0_F; // baseline detection
}

parameters {
  
  vector[T*I] r;  // population growth rates
  real<lower=0> sigma_r;  // variation in growth rates
  real alpha_r;  // intercept for population growth rates
  vector[K] beta_r; // covariate effects on growth rates

  real alpha_p_F;  // intercept for detection
  vector[T] delta_p_F;  // random effect (by week) of baseline detection at each location
  real mu_delta_p_F;  // average effect of baseline detection
  real<lower=0> sd_delta_p_F;  // variation in effect of baseline detection among weeks
}


transformed parameters {
  
  vector<lower=0>[T*I] N;  // population estimates
  vector[T*I] mu_r;  // expected growth rates
  vector<lower=0, upper=1>[T*I] p_F; // detection probabilities
  
  
  // population process model
  N[ti_N0] = N0;
  for(t in 2:T){
    N[t_slice(t,I)] = N[t_slice_lag(t,I)] .* exp(r[t_slice(t,I)]);
  }
  
  // regression on population growth rates
  mu_r = alpha_r + X * beta_r;
  
  // regression on detection probability (penetration rate)
  p_F = inv_logit(alpha_p_F + delta_p_F[tt] .* logit(p0_F[ii]));
}


model {
  
  // likelihood (observation model)
  y_F ~ poisson(N[ti_F] .* p_F[ti_F]);

  // total population constraint
  for(t in 1:T){
    sum(N[t_slice(t,I)]) ~ lognormal(log(N_tot[t]), 1e-3);
  }

  // population growth rates
  r ~ normal(mu_r, sigma_r);

  // priors:  population
  sigma_r ~ cauchy(0, 3);
  alpha_r ~ normal(0, 10);
  beta_r ~ normal(0, 10);

  // priors:  detection
  alpha_p_F ~ normal(0, 10);
  delta_p_F ~ normal(mu_delta_p_F, sd_delta_p_F);
  mu_delta_p_F ~ normal(1, 2);
  sd_delta_p_F ~ cauchy(0, 2);
}
