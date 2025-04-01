data {
  int<lower=0> T;  // number of weeks
  int<lower=0> I;  // number of locations
  int<lower=0> M;  // number of repeat observations
  array[T] int<lower=0> N_tot;  // total population size
  array[T,I,M] int<lower=0> F;  // Facebook daily active users
}

parameters {
  array[T,I] real<lower=0> N;
  array[T] real<lower=0, upper=1> rho;
}

transformed parameters {
  array[T] real<lower=0> N_sum;
  
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
    N[t,] ~ uniform(0, N_tot[t]);
  }
}
