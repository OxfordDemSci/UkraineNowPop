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
  array[T] real<lower=0, upper=1> rho;
}

transformed parameters {
  array[T,I] real<lower=0> N;
  array[T] real<lower=0> N_sum;
  
  # baseline population
  N[1,] = N0;
  
  # population process model
  for(t in 2:T){
    for(i in 1:I){
      N[t,i] = N[t-1,i] * exp(r[t,i]);
    }
  }
  
  # estimated population sum among locations
  for(t in 1:T){
    N_sum[t] = sum(N[t,]);
  }
}

model {
  
  // likelihood
  for(t in 1:T){
    for(i in 1:I){
      F[t,i,] ~ poisson(N[t,i] * rho[t]);
    }
  }
  
  // total population constraint
  for(t in 1:T){
    N_sum[t] ~ lognormal(log(N_tot[t]), 1e-3);
  }

  // priors
  rho ~ beta(1, 1);
  for(t in 1:T){
    r[t,] ~ normal(0, 1);
  }
}
