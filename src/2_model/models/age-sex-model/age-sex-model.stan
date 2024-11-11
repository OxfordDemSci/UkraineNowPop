
data {
  // Number of observed values in each data source
  int<lower=1> N_obs_F;    
  int<lower=1> N_obs_G;    
  int<lower=1> N_obs_GFr;
  
  int<lower=1> N_combinations;  // Should be 486
  int<lower=1> N_time;          // Should be 117 (total number of weeks)
  
  // Combination indices for each observation
  array[N_obs_F] int<lower=1, upper=N_combinations> F_combination_id;
  array[N_obs_G] int<lower=1, upper=N_combinations> G_combination_id;
  array[N_obs_GFr] int<lower=1, upper=N_combinations> GFr_combination_id;
  
  // Time indices for each observation
  array[N_obs_F] int<lower=1, upper=N_time> F_time;
  array[N_obs_G] int<lower=1, upper=N_time> G_time;
  array[N_obs_GFr] int<lower=1, upper=N_time> GFr_time;
  
  // Observed counts
  array[N_obs_F] int<lower=0> y_F;
  array[N_obs_G] int<lower=0> y_G;
  array[N_obs_GFr] real<lower=0> y_FG_ratio;
   
  vector<lower=0>[N_time] y_N_tot;
  vector<lower=0>[N_combinations] N0;
}

parameters {
  matrix<lower=0>[N_combinations, N_time] p_F;
  matrix<lower=0>[N_combinations, N_time] p_G;
  
  vector<lower=0>[N_combinations] r;
  real<lower=0> sigma_r;
  real alpha_r;
  
  real mu_p_F;
  real<lower=0> sigma_p_F;
  real mu_p_G;
  real<lower=0> sigma_p_G;
}

transformed parameters {
  matrix<lower=0>[N_combinations, N_time] N;  
  N[, 1] = N0;
  
  for (t in 2:N_time) {
    N[, t] = N[, t-1] .* r;
  }
  
  
  vector<lower=0>[N_time] N_tot; 
  N_tot = N' * rep_vector(1.0, N_combinations);
  
  
  vector[N_obs_GFr] p_G_vecGFr;
  vector[N_obs_GFr] p_F_vecGFr;
  vector[N_obs_GFr] FG_ratio;

 for (i in 1:N_obs_GFr) {
    p_G_vecGFr[i] = p_G[GFr_combination_id[i], GFr_time[i]];
    p_F_vecGFr[i] = p_F[GFr_combination_id[i], GFr_time[i]];
  }
  
  FG_ratio = p_G_vecGFr ./ p_F_vecGFr;
  
}

model {
  // Extracting N and p values for y_F observations using loops
  vector[N_obs_F] N_obs_vecF;
  vector[N_obs_F] p_F_vecF;
  for (i in 1:N_obs_F) {
    N_obs_vecF[i] = N[F_combination_id[i], F_time[i]];
    p_F_vecF[i] = p_F[F_combination_id[i], F_time[i]];
  }


  // Extract N and p values for y_G observations using loops
  vector[N_obs_G] N_obs_vecG;
  vector[N_obs_G] p_G_vecG;
  for (i in 1:N_obs_G) {
    N_obs_vecG[i] = N[G_combination_id[i], G_time[i]];
    p_G_vecG[i] = p_G[G_combination_id[i], G_time[i]];
  }
  

  // Vectorised Poisson likelihoods
  y_F ~ poisson(N_obs_vecF .* p_F_vecF);
  y_G ~ poisson(N_obs_vecG .* p_G_vecG);

  // Likelihood for FG_ratio observations
  y_FG_ratio ~ lognormal(log(FG_ratio), 0.02);  

  // Likelihood for total population observations
  y_N_tot ~ lognormal(log(N_tot), 0.01);  

  
  // Priors for r with hierarchical structure
  r ~ lognormal(alpha_r, sigma_r);
  alpha_r ~ normal(0, 1);     
  sigma_r ~ cauchy(0, 1);     


  to_vector(p_F) ~ lognormal(mu_p_F, sigma_p_F);
  to_vector(p_G) ~ lognormal(mu_p_G, sigma_p_G);
  
  mu_p_F ~ normal(0, 5); 
  sigma_p_F ~ cauchy(0, 1); 
  
  mu_p_G ~ normal(0, 5); 
  sigma_p_G ~ cauchy(0, 1);
  
}


