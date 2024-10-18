data {
  
  int<lower=0> T;  // number of weeks
  int<lower=0> I;  // number of locations
  int<lower=0> M_max;  // number of repeat observations
  array[T,I] int<lower=0> M;  // number of repeat observations
  vector<lower=0>[T] N_tot;  // total population among locations
  vector<lower=0>[I] N0;  // baseline population at each location
  vector<lower=0, upper=1>[I] p0; // baseline detection
  array[T,I,M_max] int<lower=0> y_F;  // Facebook daily active users
}


parameters {
  
  array[T] vector[I] r;  // population growth rates
  real mu;  // expected growth rates
  real<lower=0> sigma;  // variation in growth rates
  
  real alpha;  // intercept for detection
  vector[T] delta;  // random effect (by week) of baseline detection at each location
  real mu_delta;  // average effect of baseline detection
  real<lower=0> sd_delta;  // variation in effect of baseline detection among weeks
}


transformed parameters {
  
  array[T] vector<lower=0>[I] N;  // population estimates
  array[T] vector<lower=0, upper=1>[I] p; // detection probabilities
  
  
  // population process model
  N[1] = N0;
  for(t in 2:T){
    N[t] = N[t-1] .* exp(r[t]);
  }
  
  // observation model
  for(t in 1:T){
    for(i in 1:I){
      p[t,i] = inv_logit(alpha + delta[t] * logit(p0[i]));
    }
  }
}


model {
  
  // likelihood
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
    r[t] ~ normal(mu, sigma);
  }

  // priors:  population
  mu ~ normal(0, 1);
  sigma ~ cauchy(0, 1);

  // priors:  detection
  alpha ~ normal(0, 1);
  delta ~ normal(mu_delta, sd_delta);
  mu_delta ~ normal(1, 1);
  sd_delta ~ cauchy(0, 1);
}
