functions {
  // Slice ti vector for specific year t
  array[] int t_slice(int t, int I) {
    int a = 1 + t * I - I;  
    int b = t * I;          
    int n = b - a + 1;      
    array[n] int result;
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
  
  // Slice ti vector for specific previous year t-1
  array[] int t_slice_lag(int t, int I) {
    int a = 1 + t * I - 2 * I; 
    int b = t * I - I;         
    int n = b - a + 1;         
    array[n] int result;
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
}
data {
  // Dimensions
  int<lower=0> T;         // number of time steps
  int<lower=0> I;         // number of locations
  int<lower=0> C;         // number of 2 sexes * 5 age groups
  int<lower=0> K_r;       // number of covariates on population growth rates
  int<lower=0> K_p;       // number of covariates on detection rates
  
  vector<lower=0>[T] y_N_tot; // total population among locations

  // Baseline population for each sex-age combination and location:
  array[C] vector<lower=0>[I] N0;
  array[I] int<lower=0> ti_N0;
  
  // Covariate matrices - we can include age-sex specific covariates
  array[C] matrix[T * I, K_r] X_r;
  array[C] matrix[T * I, K_p] X_p;
    
  // Facebook data
  int<lower=0> n_F;                     
  vector<lower=0>[n_F] y_F;             
  array[n_F] int<lower=1, upper=C> comb_F;
  array[n_F] int<lower=0> ti_F;
  
  // Instagram data
  int<lower=0> n_G;          
  vector<lower=0>[n_G] y_G;  
  array[n_G] int<lower=1, upper=C> comb_G; 
  array[n_G] int<lower=0> ti_G;
  
  // Indexing for time and location for the (T*I) grid:
  array[T * I] int<lower=0> tt;  
  array[T * I] int<lower=0> ii;  
}

parameters {
  // Population process growth rates 
  array[C] vector<lower=0>[T * I] r;
  real alpha_r;
  vector[K_r] beta_r;
  real log_sigma_r;
  
  // Detection model parameters
  real alpha_p;
  real phi_p;
  vector[K_p] beta_p;
  real log_sigma_F;
  real log_sigma_G;
  
  // Random effects for detection rates (different across age-sex combinations)
  array[C] vector[T] delta_p; 
  array[C] vector[I] gamma_p;
  real log_sigma_delta_p;
  real log_sigma_gamma_p;
}
transformed parameters {
  array[C] vector<lower=0>[T * I] N;
  vector<lower=0>[T] N_tot; 
  array[C] vector<lower=0>[T * I] p_F;
  array[C] vector<lower=0>[T * I] p_G;
  
  vector[n_F + n_G] log_lik;
  
  for (c in 1:C) {
    for (i in 1:I) {
      N[c][ti_N0[i]] = N0[c][i];
    }
    for (t in 2:T) {
      N[c][t_slice(t, I)] = N[c][t_slice_lag(t, I)] .* r[c][t_slice(t, I)];
    }}

  for (t in 1:T) {
    real total = 0;
    for (c in 1:C) {
      total += sum(N[c][t_slice(t, I)] );
      }
      N_tot[t] = total;
      }
  

  for (c in 1:C) {
   for (i in 1:(T * I)) {
    p_F[c][i] = exp(alpha_p + delta_p[c][tt[i]] + gamma_p[c][ii[i]]
                        + dot_product(X_p[c][i, ], beta_p));
    p_G[c][i] = p_F[c][i] * exp(phi_p);
  }}

  // Computing log-likelihood for Facebook and Instagram
  for (i in 1:n_F) {
    int c = comb_F[i]; 
    log_lik[i] = lognormal_lpdf(y_F[i] 
                  | log(N[c][ti_F[i]] * p_F[c][ti_F[i]]), exp(log_sigma_F));
  }
  // Process Instagram observations:
  for (i in 1:n_G) {
    int c = comb_G[i];
    log_lik[i + n_F] = lognormal_lpdf(y_G[i] 
                  | log(N[c][ti_G[i]] * p_G[c][ti_G[i]]), exp(log_sigma_G));
  }
}
model {
  // Overall likelihood:
  target += sum(log_lik);
  
  // Prior: growth rates per age-sex combination, allowing each combination having different covariates
  for (c in 1:C){
    r[c] ~ lognormal(alpha_r + X_r[c] * beta_r, exp(log_sigma_r));
  }
  
  // Priors on random effects for detection rates
  for (c in 1:C) {
    delta_p[c] ~ normal(0, exp(log_sigma_delta_p));
    gamma_p[c] ~ normal(0, exp(log_sigma_gamma_p));
  }

  N_tot ~ lognormal(log(y_N_tot), 0.01 / 2);
}
generated quantities {
// Posterior predictive checks for Facebook and Instagram data.
vector[n_F] F_hat;
vector[n_G] G_hat;
  
for (i in 1:n_F) {
  int c = comb_F[i];
  F_hat[i] = lognormal_rng(log(N[c][ti_F[i]] * p_F[c][ti_F[i]]), exp(log_sigma_F));
  }
  for (i in 1:n_G) {
    int c = comb_G[i];
    G_hat[i] = lognormal_rng(log(N[c][ti_G[i]] * p_G[c][ti_G[i]]), exp(log_sigma_G));
  }
}
