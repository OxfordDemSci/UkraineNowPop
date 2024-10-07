data {
  int<lower=1> J; 
  int<lower=1> T; 
  
  int<lower=1> F_obs_count; 
  array[F_obs_count] int<lower=1, upper=T> Fobs_time;
  array[F_obs_count] int<lower=1, upper=J> Fobs_oblast;
  array[F_obs_count] int<lower=0> F_obs; 
  
  int<lower=1> F_miss_count;
  array[F_miss_count] int<lower=1, upper=T> Fmiss_time;
  array[F_miss_count] int<lower=1, upper=J> Fmiss_oblast;
  
  vector<lower=0>[J] nuF0_obs;
  vector<lower=0>[J] N0_obs; 
  vector[T] ref_in; 
  vector[T] ref_out; 
}

parameters {
  vector<lower=0>[F_miss_count] lambda_Fmiss;
  
  matrix<lower=0>[J, T] nuF; 
  array[T] simplex[J] prop;
}

transformed parameters {
  matrix<lower=0>[J, T] N; 
  vector[T] total_N; 

  // Population model
  N[, 1] = N0_obs;
  total_N[1] = sum(N0_obs);
  

  for (t in 2:T) {
    total_N[t] = total_N[1] + ref_in[t] - ref_out[t];
    N[, t] = prop[t] * total_N[t];
  }
}

model {
  // Observation model for observed data
  for (n in 1:F_obs_count) {
    int t = Fobs_time[n];
    int j = Fobs_oblast[n];
    F_obs[n] ~ poisson(N[j, t] * nuF[j, t]);
  }
  
  // Model for missing data
  for (n in 1:F_miss_count) {
    int t = Fmiss_time[n];
    int j = Fmiss_oblast[n];
    lambda_Fmiss[n] ~ normal(N[j, t] * nuF[j, t], 1); 
  }
  
  
  // Facebook penetration rate
  for (j in 1:J) {
    nuF[j, 1] ~ normal(nuF0_obs[j], 0.001); 
  }

  for (t in 2:T) {
    for (j in 1:J) {
      nuF[j, t] ~ lognormal(log(nuF[j, t-1]), 0.001);
    }
  }

  // Priors for 'prop'
  for (t in 1:T) {
    prop[t] ~ dirichlet(rep_vector(1, J));
  }
}


generated quantities {
  array[F_miss_count] int<lower=0> F_miss;
  for (n in 1:F_miss_count) {
    F_miss[n] = poisson_rng(lambda_Fmiss[n]);
  }
}
