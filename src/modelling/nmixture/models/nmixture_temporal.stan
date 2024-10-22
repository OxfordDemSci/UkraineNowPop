data {
  
  int<lower=0> T;  // number of weeks
  int<lower=0> I;  // number of locations
  int<lower=0> M_max;  // number of repeat observations
  array[T,I] int<lower=0> M;  // number of repeat observations
  int<lower=0> K;  // number of covariates on population growth rates
  array[T] matrix[I,K] X; // covariates on population growth rates
  vector<lower=0>[T] N_tot;  // total population among locations
  vector<lower=0>[I] N0;  // baseline population at each location
  vector<lower=0, upper=1>[I] p0; // baseline detection
  array[T,I,M_max] int<lower=0> y_F;  // Facebook daily active users
}


parameters {
  
  real mu_r;  // expected growth rates
  real<lower=0> sigma_r;  // variation in growth rates
  real alpha_r; // intercept for growth rates
  vector[K] beta_r; // effects on growth rates
  
  real alpha_p;  // intercept for detection
  vector[T] delta_p;  // random effect (by week) of baseline detection at each location
  real mu_delta_p;  // average effect of baseline detection
  real<lower=0> sd_delta_p;  // variation in effect of baseline detection among weeks
}


transformed parameters {
  
  array[T] vector<lower=0>[I] N;  // population estimates
  array[T] vector[I] r;  // population growth rates
  array[T] vector<lower=0, upper=1>[I] p; // detection probabilities
  
  
  // regression on population growth rates
  r[1] = rep_vector(0, I);
  for(t in 2:T){
    r[t] = alpha_r + X[t] * beta_r;
  }
  
  // population process model
  N[1] = N0;
  for(t in 2:T){
    N[t] = N[t-1] .* exp(r[t]);
  }
  
  // regression on detection probability
  for(t in 1:T){
    for(i in 1:I){
      p[t,i] = inv_logit(alpha_p + delta_p[t] * logit(p0[i]));
    }
  }
}


model {
  
  // likelihood (observation model)
  for(t in 1:T){
    for(i in 1:I){
      y_F[t,i,1:M[t,i]] ~ poisson(N[t,i] * p[t,i]);
    }
  }
  
  // total population constraint
  for(t in 1:T){
    sum(N[t]) ~ lognormal(log(N_tot[t]), 1e-3);
  }

  // population growth rates
  for(t in 1:T){
    r[t] ~ normal(mu_r, sigma_r);
  }

  // priors:  population
  mu_r ~ normal(0, 1);
  sigma_r ~ cauchy(0, 1);
  alpha_r ~ normal(0, 10);
  beta_r ~ normal(0, 10);

  // priors:  detection
  alpha_p ~ normal(0, 1);
  delta_p ~ normal(mu_delta_p, sd_delta_p);
  mu_delta_p ~ normal(1, 1);
  sd_delta_p ~ cauchy(0, 1);
}
