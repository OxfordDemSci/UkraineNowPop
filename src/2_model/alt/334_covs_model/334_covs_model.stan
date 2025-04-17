functions {
  // slice t×(I×A×S) vector for specific time t
  array[] int t_slice(int t, int I, int A, int S) {
    int C = I * A * S;          // total combos per time t
    int a = 1 + (t - 1) * C;    // start index for time t
    array[C] int result;
    for (i in 1:C)
      result[i] = a + i - 1;
    return result;
  }
  // slice for previous time t-1
  array[] int t_slice_lag(int t, int I, int A, int S) {
    int C = I * A * S;
    int a = 1 + (t - 2) * C;    // start index for time t−1
    array[C] int result;
    for (i in 1:C)
      result[i] = a + i - 1;
    return result;
  }
}
data {
  // Dimensions
  int<lower=1> T;            // number of time steps
  int<lower=1> I;            // locations
  int<lower=1> A;            // age‐groups
  int<lower=1> S;            // sexes
  int<lower=0> K_r;          // covariates on pop growth
  int<lower=0> K_p;          // covariates on detections
  
  vector<lower=0>[T] y_N_tot;

  // Baseline population for each sex combination and location:
  vector<lower=0>[I * A * S] N0;
  
  // Covariate matrices - we can include age-sex specific covariates
  matrix[T * I * A * S, K_r] X_r;
  matrix[T * I * A * S, K_p] X_p;
  
  // Facebook data
   int<lower=0> n_F;
  array[n_F] real<lower=0> y_F;
  array[n_F] int<lower=0> tias_F;
  
  // Instgram data
  int<lower=0> n_G;
  array[n_G] real<lower=0> y_G;
  array[n_G] int<lower=0> tias_G;
  
  // Indexing for time and location for the (T*I) grid:
  array[T * I * A * S] int<lower=1> tt;  // time
  array[T * I * A * S] int<lower=1> ii;  // location
  array[T * I * A * S] int<lower=1> aa;  // age‐group
  array[T * I * A * S] int<lower=1> ss;  // sex

  array[I * A * S]       int<lower=1> ias_N0;    
}

parameters {
  // Population process growth rates 
  vector<lower=0>[T * I * A * S] r;
  real alpha_r;
  array[A, S] vector[K_r] beta_r;
  vector[K_r] mu_beta_r; 
  vector<lower=0>[K_r] sigma_beta_r;
  real log_sigma_r;

  // Detection model parameters
  real alpha_p;
  vector[A] phi_p; 
  real<lower=0>    sigma_phi_p;

  array[A, S] vector[K_p] beta_p;
  vector[K_p] mu_beta_p;
  vector<lower=0>[K_p]    sigma_beta_p;
  
  real     log_sigma_F;
  real     log_sigma_G;
  
  // Random effects for detection rates (different across age-sex combinations)
  vector[T]    delta_p;
  real         log_sigma_delta_p;
  vector[I]    gamma_p_i;
  vector[A]    gamma_p_a;
  vector[S]    gamma_p_s;
  real         log_sigma_gamma_i;
  real         log_sigma_gamma_a;
  real         log_sigma_gamma_s;
}

transformed parameters {
  vector<lower=0>[T * I * A * S] N;
  vector<lower=0>[T] N_tot;
  vector<lower=0>[T * I * A * S] p_F;
  vector<lower=0>[T * I * A * S] p_G;
  vector[n_F + n_G] log_lik;
  
  N[ias_N0] = N0;
  for (t in 2:T) {
  N[t_slice(t, I, A, S)] = N[t_slice_lag(t, I, A, S)] .* r[t_slice(t,     I, A, S)];
  }

  for (t in 1:T) {
    N_tot[t] = sum(N[t_slice(t, I, A, S)]);
  }
  
 vector[T * I * A * S] nu;
  for (n in 1:(T * I * A * S)) {
  nu[n] = alpha_p
         + delta_p[tt[n]]
         + gamma_p_i[ii[n]]
         + gamma_p_a[aa[n]]
         + gamma_p_s[ss[n]]
         + dot_product( X_p[n] , beta_p[ aa[n], ss[n] ] );
  }

  p_F = exp(nu);
  p_G = exp(nu + phi_p[aa]); 
  
  for (i in 1:n_F){
    log_lik[i] = lognormal_lpdf(y_F[i] | log(N[tias_F[i]] * p_F[tias_F[i]]), exp(log_sigma_F));
  }
  for (j in 1:n_G){
    log_lik[n_F+j] = lognormal_lpdf(y_G[j] | log(N[tias_G[j]] * p_G[tias_G[j]]), exp(log_sigma_G));
   }


  // Computing log-likelihood for Facebook and Instagram
  for (i in 1:n_F){
    log_lik[i] = lognormal_lpdf(y_F[i] | log(N[tias_F[i]] * p_F[tias_F[i]]), exp(log_sigma_F));
  }
  for (j in 1:n_G){
    log_lik[n_F+j] = lognormal_lpdf(y_G[j] | log(N[tias_G[j]] * p_G[tias_G[j]]), exp(log_sigma_G));
  }
}
model {
  // Overall likelihood:
  target += sum(log_lik);
  
  N_tot ~ lognormal(log(y_N_tot), 0.01 / 2);

// Prior: growth rates per age-sex combination, allowing each combination having different covariates
vector[T * I * A * S] eta_r;
for (n in 1:(T * I * A * S)) {
 eta_r[n] = alpha_r + dot_product( X_r[n], beta_r[aa[n], ss[n]] );
  }

r ~ lognormal(eta_r, exp(log_sigma_r));
   
  // Priors on random effects for detection rates
  delta_p ~ normal(0, exp(log_sigma_delta_p));
  gamma_p_i ~ normal(0, exp(log_sigma_gamma_i));
  gamma_p_a ~ normal(0, exp(log_sigma_gamma_a));
  gamma_p_s ~ normal(0, exp(log_sigma_gamma_s));
}
generated quantities {
  array[n_F] real<lower=0> F_hat;
  array[n_G] real<lower=0> G_hat;
  for (i in 1:n_F)
    F_hat[i] = lognormal_rng(log(N[tias_F[i]] * p_F[tias_F[i]]), exp(log_sigma_F));
  for (j in 1:n_G)
    G_hat[j] = lognormal_rng(log(N[tias_G[j]] * p_G[tias_G[j]]), exp(log_sigma_G));
}
