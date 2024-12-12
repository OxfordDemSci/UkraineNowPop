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
  
  
  // baseline
  vector<lower=0>[T] y_N_tot; // total population among locations
  vector<lower=0>[I * G * S] N0; // baseline population at each location
  
  // Facebook data
  int<lower=0> n_F; // total sample size for F
  array[n_F] int<lower=0> y_F; // Facebook daily active users
  
  // Instagram data
  int<lower=0> n_G; // total sample size for G
  array[n_G] int<lower=0> y_G; // Instagram daily active users
  
  // observation ratio
  int<lower=0> n_FG; // total sample size with F and G
  vector<lower=0>[n_FG] y_FG_ratio; // ratio of G to F
  
  // year x location indexing for long format
  array[T * I * G * S] int<lower=0> tt; // year index for tc vector      
  array[T * I * G * S] int<lower=0> ii; // location index for tc vector  
  array[T * I * G * S] int<lower=0> gg; // location index for tc vector
  array[T * I * G * S] int<lower=0> ss; // location index for tc vector
  
  array[I * G * S] int<lower=0> tias_N0; // c (location, age, sex) index for N0
  array[T * C - C] int<lower=0> tias_N; // tc (year, combination) index for N[t]      
  array[T * C - C] int<lower=0> tias_N_lag; // ti (year, comb) index for N[t-1]
  
  array[n_F] int<lower=0> tias_F; // tias (year, location, age, sex) index for F
  array[n_G] int<lower=0> tias_G; // tias (year, location, age, sex) index for G
  array[n_FG] int<lower=0> tias_FG; // tias (year, location, age, sex) index for F and G
}
parameters {
  // population
  array[T-1] vector[C-1] logit_p; // population proportions in each oblast
  real mu_p0;  
  vector[T-1] mu_p_t; // time effect
  vector[C-1] mu_p_ias; // location-age-sex effect
  real log_sigma_p_t;
  real log_sigma_p_ias;  
  real log_sigma_p;
  
  // Facebook
  vector<lower=0>[T * C] p_F; // Facebook user ratios
  real mu_p_F0;
  vector[T] mu_p_F_t; // time effect
  vector[C] mu_p_F_ias; // location effect
  real log_sigma_p_F_t;
  real log_sigma_p_F_ias;
  real log_sigma_p_F;
  
  // Instagram
  vector<lower=0>[T * C] p_GF; 
  real mu_p_GF;
  real log_sigma_p_GF;
  
  real log_kappa_F; // over-dispersion
  real log_kappa_G;
  
}
transformed parameters {
  array[T-1] simplex[C] p;
  vector<lower=0>[T * C] N; // population estimates
  vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios
  vector<lower=0>[T * C] p_G = p_F .* p_GF;
  vector[T*C] mu_p_F = mu_p_F0 + to_vector(mu_p_F_t * mu_p_F_ias');
  matrix[T-1, C-1] mu_p = mu_p0 + mu_p_t * mu_p_ias';  
  
  
  // population process model
  p[1] = softmax(append_row(logit_p[1],1));
  N[tias_N0] = N0;
  for (t in 2 : T) {
    if(t < T){
      p[t] = softmax(append_row(logit_p[t],1));
    }
    N[t_slice(t, C)] = y_N_tot[t] * p[t-1];
  }
  
  // observation ratio of Instagram to Facebook
  FG_ratio = p_G[tias_FG] ./ p_F[tias_FG];
}

model {
 // likelihoods
  y_F ~ neg_binomial_2(N[tias_F] .* p_F[tias_F],exp(-log_kappa_F));
  y_G ~ neg_binomial_2(N[tias_G] .* p_G[tias_G],exp(-log_kappa_G));
  
  p_F ~ lognormal(mu_p_F, exp(log_sigma_p_F));
  p_GF ~ lognormal(mu_p_GF,exp(log_sigma_p_GF));
  
  // population growth rates
  for(t in 1:(T-1)){
    logit_p[t] ~ normal(mu_p[t,],exp(log_sigma_p));
  }
  mu_p_t ~ normal(0, exp(log_sigma_p_t));
  mu_p_ias ~ normal(0, exp(log_sigma_p_ias));
  mu_p0 ~ normal(0,5);  
  
  // priors:  Facebook user ratio
  mu_p_F_t ~ normal(0, exp(log_sigma_p_F_t));
  mu_p_F_ias ~ normal(0, exp(log_sigma_p_F_ias));
  mu_p_F0 ~ normal(0,5);
  
  // priors:  Instagram user ratio
  mu_p_GF ~ normal(0,5);

  // priors for over-dispersion parameters
  log_kappa_F ~ normal(0,1);
  log_kappa_G ~ normal(0,1);
}
