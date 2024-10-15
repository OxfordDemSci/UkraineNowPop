data {
  int<lower=0> T;  // number of weeks
  int<lower=0> I;  // number of locations
  int<lower=0> M;  // number of repeat observations
  array[T] int<lower=0> N_tot;  // total population size
  array[I] int<lower=0> N0;  // baseline population for each location
  array[T,I,M] int<lower=0> F;  // Facebook daily active users
}

parameters {
  array[T,I] real r;
  array[T] real gamma;
  array[I] real delta;
  real alpha;
  real<lower=0> sd_gamma;
  real<lower=0> sd_delta;
}

transformed parameters {
  array[T,I] real<lower=0> N;
  array[T] real<lower=0> N_sum;
  array[T,I] real<lower=0, upper=1> p;
  
  // baseline population
  N[1,] = N0;
  
  // population process model
  for(t in 2:T){
    for(i in 1:I){
      N[t,i] = N[t-1,i] * exp(r[t,i]);
    }
  }
  
  // estimated population sum among locations
  for(t in 1:T){
    N_sum[t] = sum(N[t,]);
  }
  
  // detection process model
  for(t in 1:T){
    for(i in 1:I){
      p[t,i] = inv_logit(alpha + gamma[t] + delta[i]);
    }
  }
}

model {
  
  // likelihood
  for(t in 1:T){
    for(i in 1:I){
      F[t,i,] ~ poisson(N[t,i] * p[t,i]);
    }
  }
  
  // total population constraint
  for(t in 1:T){
    N_sum[t] ~ lognormal(log(N_tot[t]), 1e-3);
  }

  // priors:  population
  for(t in 1:T){
    r[t,] ~ normal(0, 1);
  }
  
  // priors:  detection
  alpha ~ normal(0, 5);
  sd_gamma ~ cauchy(0, 1);
  sd_delta ~ cauchy(0, 1);
  for(t in 1:T){
    gamma ~ normal(0, sd_gamma);
  }
  for(i in 1:I){
    delta ~ normal(0, sd_delta);
  }
}
