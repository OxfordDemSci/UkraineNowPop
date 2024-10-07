data {
  int<lower=1> J; 
  int<lower=1> T; 
  
  int<lower=1> F_obs_count; 
  array[F_obs_count] int<lower=1, upper=T> Fobs_t;
  array[F_obs_count] int<lower=1, upper=J> Fobs_j;
  array[F_obs_count] int<lower=0> F_obs; 
  
  int<lower=1> F_miss_count;
  array[F_miss_count] int<lower=1, upper=T> Fmiss_t;
  array[F_miss_count] int<lower=1, upper=J> Fmiss_j;
  
  vector<lower=0>[J] nuF0_obs;
  
  int<lower=1> G_obs_count;
  array[G_obs_count] int<lower=1, upper=T> Gobs_t;
  array[G_obs_count] int<lower=1, upper=J> Gobs_j;
  array[G_obs_count] int<lower=0> G_obs;
  
  int<lower=1> G_miss_count;
  array[G_miss_count] int<lower=1, upper=T> Gmiss_t;
  array[G_miss_count] int<lower=1, upper=J> Gmiss_j;
  
  int<lower=1> GF_ratio_obs_count;
  array[GF_ratio_obs_count] int<lower=1, upper=T> GF_ratio_obs_t;
  array[GF_ratio_obs_count] int<lower=1, upper=J> GF_ratio_obs_j;
  vector<lower=0>[GF_ratio_obs_count] GF_ratio_obs;
  
  int<lower=0> GF_ratio_miss_count;
  array[GF_ratio_miss_count] int<lower=1, upper=J> GF_ratio_miss_j;
  array[GF_ratio_miss_count] int<lower=1, upper=T> GF_ratio_miss_t;
  
  vector<lower=0>[J] N0_obs; 
  vector[T] ref_in; 
  vector[T] ref_out; 
}

parameters {
  vector<lower=0>[F_miss_count] lambda_Fmiss;
  vector<lower=0>[G_miss_count] lambda_Gmiss;
  
  matrix<lower=0>[J, T] nuF; 
  
  matrix<lower=0>[J, T] GF_ratio;
  real<lower=0> sigma_G;
  real<lower=0> sigma_obs_G;
  
  array[T] simplex[J] prop;
}

transformed parameters {
  matrix<lower=0>[J, T] nuG;
  for (j in 1:J) {
    for (t in 1:T) {
        nuG[j, t] = nuF[j, t] .* GF_ratio[j, t];
      }}
  
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
  // Observation model for observed data (Facebook)
  for (n in 1:F_obs_count) {
    int t = Fobs_t[n];
    int j = Fobs_j[n];
    F_obs[n] ~ poisson(N[j, t] .* nuF[j, t]);
  }
  
  // Model for missing data (Facebook)
  for (n in 1:F_miss_count) {
    int t = Fmiss_t[n];
    int j = Fmiss_j[n];
    lambda_Fmiss[n] ~ normal(N[j, t] .* nuF[j, t], 1); 
  }
  
  // Observation model for observed data (Instagram)
  for (n in 1:G_obs_count) {
    int t = Gobs_t[n];
    int j = Gobs_j[n];
    G_obs[n] ~ poisson(N[j, t] .* nuG[j, t]);
  }
  
  // Model for missing data (Instagram)
  for (n in 1:G_miss_count) {
    int t = Gmiss_t[n];
    int j = Gmiss_j[n];
    lambda_Gmiss[n] ~ normal(N[j, t] .* nuG[j, t], 1); 
  }
  
  
  
  
  // Facebook penetration rate
  for (j in 1:J) {
    nuF[j, 1] ~ normal(nuF0_obs[j], 0.001); 
  }

  for (t in 2:T) {
    for (j in 1:J) {
      nuF[j, t] ~ lognormal(log(nuF[j, t-1]), 0.001);
    }}
    
    
  // Likelihood for observed GF_ratio
  for (n in 1:GF_ratio_obs_count) {
    int j = GF_ratio_obs_j[n];
    int t = GF_ratio_obs_t[n];
    GF_ratio_obs[n] ~ normal(GF_ratio[j, t], sigma_obs_G); 
  }

  // Priors for GF_ratio parameters
  for (j in 1:J) {
    for (t in 1:T) {
      GF_ratio[j, t] ~ lognormal(log(0.7), sigma_G);
    }}
  
  
  sigma_G ~ normal(0, 0.2);  
  sigma_obs_G ~ normal(0, 0.2);  


  // Priors for 'prop'
  for (t in 1:T) {
    prop[t] ~ dirichlet(rep_vector(1, J));
  }}


generated quantities {
  array[F_miss_count] int<lower=0> F_miss;
  for (n in 1:F_miss_count) {
    F_miss[n] = poisson_rng(lambda_Fmiss[n]);
  }
  
  array[G_miss_count] int<lower=0> G_miss;
  for (n in 1:G_miss_count) {
    G_miss[n] = poisson_rng(lambda_Gmiss[n]);
  }
}
