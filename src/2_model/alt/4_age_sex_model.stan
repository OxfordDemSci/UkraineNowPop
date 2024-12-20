functions {
  // slice tias vector for specific year t
  array[] int t_slice(int t, int C) {
    int a = 1 + t * C - C; // starting index for year t
    int b = t * C; // ending index for year t
    int n = b - a + 1; // number of elements for year t
    
    array[n] int result; // result of indexes
    for (i in 1 : n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
 // slice ti vector for specific previous year t-1
  array[] int t_slice_lag(int t, int C) {
    int a = 1 + t * C - 2 * C; // starting index for year t-1
    int b = t * C - C; // ending index for year t-1
    int n = b - a + 1; // number of elements for year t-1
    
    array[n] int result; // result of indexes
    for (i in 1 : n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
}

data {
  // dimensions
  int<lower=0> T; // number of weeks
  int<lower=0> I; // number of locations
  int<lower=0> G; // number of age groups
  int<lower=0> S; // number of sexes 
  int<lower=0> C; // number of possible combinations of I*G*S

  int<lower=0> K_r; // number of covariates on population growth rates
  
  // baseline
  vector<lower=0>[T] y_N_tot; // total population among locations
  vector<lower=0>[I * G * S] N0; // baseline population at each location
  //vector<lower=0>[I * G * S] p_F0; // baseline detectio rate
  
  matrix[T * I, K_r] X_r; // covariates on population growth rates

  // Facebook data
  int<lower=0> n_F; // total sample size for F
  array[n_F] int<lower=0> y_F; // Facebook daily active users
  
  // Instagram data
  int<lower=0> n_G; // total sample size for G
  array[n_G] int<lower=0> y_G; // Instagram daily active users
  
    // year x location indexing for long format
  array[T * I * G * S] int<lower=0> tt; // year index for ti vector      
  array[T * I * G * S] int<lower=0> ii; // location index for ti vector  
  array[T * I * G * S] int<lower=0> gg; // location index for ti vector
  array[T * I * G * S] int<lower=0> ss; // location index for ti vector
  
  array[I * G * S] int<lower=0> tias_N0; // c index for N0
  array[T * C - C] int<lower=0> tias_N; // tc (year, c) index for N[t]      
  array[T * C - C] int<lower=0> tias_N_lag; // tc (year, c) index for N[t-1]
  
  array[n_F] int<lower=0> tias_F; // tias (year, location, age, sex) index for F
  array[n_G] int<lower=0> tias_G; // tias (year, location, age, sex) index for G
}
parameters {
  // population
  vector[T * I * G * S] r; // population growth rates
  real alpha_r; // random intercept for population growth rates
  vector[K_r] beta_r; // covariate effects on growth rates
  real<lower=0> sigma_r; // variation in growth rates
  
  // Social media
  vector<lower=0>[T * I * G * S] p_F; // Facebook detection ratios
  vector<lower=0>[T * I * G * S] p_G; // Instagram detection ratios
  
  real alpha_p; // intercept for Facebook detection rates
  real phi_p; // intercept offset for Instagram detection rates
  real<lower=0> sigma_p_F; // residual variation
  real<lower=0> sigma_p_G; // residual variation
  
  vector[I] gamma_p;
  real<lower=0> sigma_gamma_p;

}
transformed parameters {
  vector<lower=0>[T * I * G * S] N; // population estimates
  vector<lower=0>[T] N_tot; // total population at each time step
  
  vector[T * I] mu_r; // expected growth rates
  vector[T * I] mu_p; // expected Facebook detection rates

  // population process model
  N[tias_N0] = N0;
  for (t in 2 : T) {
    N[t_slice(t, C)] = N[t_slice_lag(t, C)] .* r[t_slice(t, C)];
  }
  
  // total population
  for (t in 1 : T) {
    N_tot[t] = sum(N[t_slice(t, C)]);
  }
  
  // regression on population growth rates
  mu_r = alpha_r + X_r * beta_r;
  
  // regression on Facebook detection rates
  mu_p = alpha_p + gamma_p[ii];
}
model {
  y_F ~ poisson(N[tias_F] .* p_F[tias_F]);
  y_G ~ poisson(N[tias_G] .* p_G[tias_G]);
  
  y_N_tot ~ lognormal(log(N_tot), 0.01 / 2);
  
  
  p_F ~ lognormal(mu_p, sigma_p_F);
  p_G ~ lognormal(mu_p + phi_p, sigma_p_G);


  r ~ lognormal(mu_r, sigma_r);
  

  // priors:  population
  alpha_r ~ normal(0, 5);
  beta_r ~ normal(0, 1);
  sigma_r ~ normal(0, 1);
  
  alpha_p ~ normal(0, 5);
  phi_p ~ normal(0, 1);
  //beta_p ~ normal(0, 1);
  sigma_p_F ~ normal(0, 1);
  sigma_p_G ~ normal(0, 1);
  
  //delta_p ~ normal(0, sigma_delta_p);
  //sigma_delta_p ~ normal(0, 1);
  
  gamma_p ~ normal(0, sigma_gamma_p);
  sigma_gamma_p ~ normal(0, 1);
  
}
