data {
  int<lower=0> I;  // number of locations
  int<lower=0> M;  // number of repeat observations
  int<lower=0> N_tot;  // total population size
  array[I,M] int<lower=0> F;  // Facebook daily active users
}

parameters {
  real<lower=0, upper=1> rho;
  array[I] real<lower=0> lambda;
}

transformed parameters {
  real<lower=0> lambda_sum = sum(lambda);
}

model {
  
  // likelihood
  for(i in 1:I){

    for(N in max(F[i,]):N_tot){
      target += poisson_lpmf(N | lambda[i]) + binomial_lpmf(F[i,] | N, rho);
    }
      
  }
  
  // total population constraint
  lambda_sum ~ lognormal(log(N_tot), 1e-3);

  // priors
  rho ~ beta(1, 1);
  lambda ~ uniform(0, N_tot);
}

// generated quantities {
//   array[I] int<lower=0> N = poisson_rng(lambda);
// }
