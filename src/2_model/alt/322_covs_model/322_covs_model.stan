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
  int<lower=0> S;         // number of 2 sexes * 5 age groups
  int<lower=0> K_r;       // number of covariates on population growth rates
  int<lower=0> K_p;       // number of covariates on detection rates
  
  vector<lower=0>[T] y_N_tot; // total population among locations

  // Baseline population for each sex combination and location:
  array[S] vector<lower=0>[I] N0;
  array[I] int<lower=0> ti_N0;
  
  // Covariate matrices - we can include age-sex specific covariates
  array[S] matrix[T * I, K_r] X_r;
  array[S] matrix[T * I, K_p] X_p;
    
  // Facebook data
  int<lower=0> n_F;                     
  vector<lower=0>[n_F] y_F;             
  array[n_F] int<lower=1, upper=S> sex_F;
  array[n_F] int<lower=0> ti_F;
  
  // Instagram data
  int<lower=0> n_G;          
  vector<lower=0>[n_G] y_G;  
  array[n_G] int<lower=1, upper=S> sex_G; 
  array[n_G] int<lower=0> ti_G;
  
  // Indexing for time and location for the (T*I) grid:
  array[T * I] int<lower=0> tt;  
  array[T * I] int<lower=0> ii;  
}

parameters {
  // Population process growth rates 
  array[S] vector<lower=0>[T * I] r;
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
  array[S] vector[T] delta_p; 
  array[S] vector[I] gamma_p;
  real log_sigma_delta_p;
  real log_sigma_gamma_p;
}
transformed parameters {
  array[S] vector<lower=0>[T * I] N;
  vector<lower=0>[T] N_tot; 
  array[S] vector<lower=0>[T * I] p_F;
  array[S] vector<lower=0>[T * I] p_G;
  
  vector[n_F + n_G] log_lik;
  
  for (s in 1:S) {
    for (i in 1:I) {
      N[s][ti_N0[i]] = N0[s][i];
    }
    for (t in 2:T) {
      N[s][t_slice(t, I)] = N[s][t_slice_lag(t, I)] .* r[s][t_slice(t, I)];
    }}

  for (t in 1:T) {
    real total = 0;
    for (s in 1:S) {
      total += sum(N[s][t_slice(t, I)] );
      }
      N_tot[t] = total;
      }
  

  for (s in 1:S) {
   for (i in 1:(T * I)) {
    p_F[s][i] = exp(alpha_p + delta_p[s][tt[i]] + gamma_p[s][ii[i]]
                        + dot_product(X_p[s][i, ], beta_p));
    p_G[s][i] = p_F[s][i] * exp(phi_p);
  }}

  // Computing log-likelihood for Facebook and Instagram
  for (i in 1:n_F) {
    int s = sex_F[i]; 
    log_lik[i] = lognormal_lpdf(y_F[i] 
                  | log(N[s][ti_F[i]] * p_F[s][ti_F[i]]), exp(log_sigma_F));
  }
  // Process Instagram observations:
  for (i in 1:n_G) {
    int s = sex_G[i];
    log_lik[i + n_F] = lognormal_lpdf(y_G[i] 
                  | log(N[s][ti_G[i]] * p_G[s][ti_G[i]]), exp(log_sigma_G));
  }
}
model {
  // Overall likelihood:
  target += sum(log_lik);
  
  // Prior: growth rates per age-sex combination, allowing each combination having different covariates
  for (s in 1:S){
    r[s] ~ lognormal(alpha_r + X_r[s] * beta_r, exp(log_sigma_r));
  }
  
  // Priors on random effects for detection rates
  for (s in 1:S) {
    delta_p[s] ~ normal(0, exp(log_sigma_delta_p));
    gamma_p[s] ~ normal(0, exp(log_sigma_gamma_p));
  }

  N_tot ~ lognormal(log(y_N_tot), 0.01 / 2);
}
generated quantities {
// Posterior predictive checks for Facebook and Instagram data.
vector[n_F] F_hat;
vector[n_G] G_hat;
  
for (i in 1:n_F) {
  int s = sex_F[i];
  F_hat[i] = lognormal_rng(log(N[s][ti_F[i]] * p_F[s][ti_F[i]]), exp(log_sigma_F));
  }
  for (i in 1:n_G) {
    int s = sex_G[i];
    G_hat[i] = lognormal_rng(log(N[s][ti_G[i]] * p_G[s][ti_G[i]]), exp(log_sigma_G));
  }
}
