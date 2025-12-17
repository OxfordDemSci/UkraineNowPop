data {
  int<lower=0> T;  // number of time steps (i.e. weeks)
  int<lower=0> I;  // number of locations (i.e. admin units)
  int<lower=0> M;  // number of occassions (i.e. days)
  array[T,I,M] int<lower=0> F;  // Facebook users
  array[I] real<lower=0> N0;  // initial population sizes
}

transformed data {
  real<lower=0> N_tot = sum(N0);
}

parameters {
  array[T,I] real<lower=0, upper=1> rho;
  array[T,I] real<lower=0> lambda;
  real<lower=0> sigma;
}

transformed parameters{
  array[T] real<lower=0> lambda_tot;
  
  for(t in 1:T){
    lambda_tot[t] = sum(lambda[t,]);
  }
}

model {
  
  // likelihood
  for(t in 2:T){
    for(i in 1:I){
      F[t,i,] ~ poisson(lambda[t,i] .* rho[t,i]);
      lambda[t,i] ~ lognormal(log(lambda[t-1,i]), sigma);
    }
  }
  
  // priors
  sigma ~ cauchy(0, 1);
  lambda[1,] ~ lognormal(log(N0), 1e-3);
  for(t in 1:T){
    rho[t,] ~ beta(1, 1);
    lambda_tot[t] ~ normal(N_tot, 1);
  }
}
