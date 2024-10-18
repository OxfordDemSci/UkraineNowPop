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

  vector<lower=0>[T] N_tot;  // total population among locations
  vector<lower=0>[I] N0;  // baseline population at each location
  vector<lower=0, upper=1>[I] p0; // baseline detection

  array[I] int<lower=0> ti_N0;
  array[T*I-I] int<lower=0> ti_N;
  array[T*I-I] int<lower=0> ti_N_lag;
  
  array[T*I] int<lower=0> tt; 
  array[T*I] int<lower=0> ii;

  int<lower=0> n_F;  // total sample size for F
  array[n_F] int<lower=0> y_F;  // Facebook daily active users
  array[n_F] int<lower=0> ti_F;  // ti (year, location) index for F
}

parameters {
  
  vector[T*I] r;  // population growth rates
  real mu;  // expected growth rates
  real<lower=0> sigma;  // variation in growth rates
  
  real alpha;  // intercept for detection
  vector[T] delta;  // random effect (by week) of baseline detection at each location
  real mu_delta;  // average effect of baseline detection
  real<lower=0> sd_delta;  // variation in effect of baseline detection among weeks
}


transformed parameters {
  
  vector<lower=0>[T*I] N;  // population estimates
  vector<lower=0, upper=1>[T*I] p; // detection probabilities
  
  
  // population process model
  N[ti_N0] = N0;
  
  // failed attempt 1
  // N[ti_N] = N[ti_N_lag] .* exp(r[ti_N]);
  
  for(t in 2:T){
    N[t_slice(t,I)] = N[t_slice_lag(t,I)] .* exp(r[t_slice(t,I)]);
  }

  // observation model
  p = inv_logit(alpha + delta[tt] .* logit(p0[ii]));
}


model {
  
  // likelihood
  y_F ~ poisson(N[ti_F] .* p[ti_F]);

  // total population constraint
  for(t in 1:T){
    sum(N[t_slice(t,I)]) ~ lognormal(log(N_tot[t]), 1e-3);
  }

  // population growth rates
  r ~ normal(mu, sigma);

  // priors:  population
  mu ~ normal(0, 1);
  sigma ~ cauchy(0, 1);

  // priors:  detection
  alpha ~ normal(0, 1);
  delta ~ normal(mu_delta, sd_delta);
  mu_delta ~ normal(1, 1);
  sd_delta ~ cauchy(0, 1);
}
