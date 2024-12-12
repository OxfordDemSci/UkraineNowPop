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
  array[T * I * G * S] int<lower=0> tt; // year index for ti vector      
  array[T * I * G * S] int<lower=0> ii; // location index for ti vector  
  array[T * I * G * S] int<lower=0> gg; // location index for ti vector
  array[T * I * G * S] int<lower=0> ss; // location index for ti vector
  
  array[I * G * S] int<lower=0> tias_N0; // c index for N0
  array[T * C - C] int<lower=0> tias_N; // tc (year, c) index for N[t]      
  array[T * C - C] int<lower=0> tias_N_lag; // tc (year, c) index for N[t-1]
  
  array[n_F] int<lower=0> tias_F; // tias (year, location, age, sex) index for F
  array[n_G] int<lower=0> tias_G; // tias (year, location, age, sex) index for G
  array[n_FG] int<lower=0> tias_FG; // tias (year, location, age, sex) index for F and G
}
parameters {
  // population
  vector[T * I * G * S] r; // population growth rates
  
  // Facebook
  vector<lower=0>[T * I * G * S] p_F; // Facebook user ratios
  //vector[T] mu_p_F; // expected value                          
  real mu_p_F;
  //real mu_mu_p_F;
  //real<lower=0> sigma_mu_p_F;
  real<lower=0> sigma_p_F; // residual variation
  
  // Instagram
  vector<lower=0>[T * C] p_GF;
  real mu_p_GF;
  real log_sigma_p_GF;

  //vector<lower=0>[T * I * G * S] p_G; // Instagram user ratios
  //vector[I] mu_p_G; // expected value
  //real mu_p_G;
  //real mu_mu_p_G;
  //real<lower=0> sigma_mu_p_G;
  //real<lower=0> sigma_p_G; // residual variation
}
transformed parameters {
  vector<lower=0>[T * I * G * S] N; // population estimates
  vector<lower=0>[T] N_tot; // total population at each time step
  
  // population process model
  N[tias_N0] = N0;
  for (t in 2 : T) {
    N[t_slice(t, C)] = N[t_slice_lag(t, C)] .* r[t_slice(t, C)];
  }
  
  // total population
  for (t in 1 : T) {
    N_tot[t] = sum(N[t_slice(t, C)]);
  }
  
  //vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios
  vector<lower=0>[n_FG] FG_ratio = p_GF[tias_FG];
  vector<lower=0>[T * C] p_G = p_F .* p_GF;
  }
model {
  // likelihoods
  y_F ~ poisson(N[tias_F] .* p_F[tias_F]);
  y_G ~ poisson(N[tias_G] .* p_G[tias_G]);
  
  y_FG_ratio ~ lognormal(log(p_GF[tias_FG]), 0.02/2);
  //y_FG_ratio ~ lognormal(log(FG_ratio), 0.02 / 2);
  
  y_N_tot ~ lognormal(log(N_tot), 0.01 / 2);
  
  // observation models
  p_F ~ lognormal(mu_p_F, sigma_p_F);   
  
  mu_p_GF ~ normal(0,5);
  log_sigma_p_GF ~ normal(0,1);
  p_GF ~ lognormal(mu_p_GF, exp(log_sigma_p_GF));

  
  // population growth rates
  r ~ lognormal(0, 0.1 / 2);
  
  // priors:  Facebook user ratio
  mu_p_F ~ normal(0, 3);
  //mu_p_F ~ normal(mu_mu_p_F, sigma_mu_p_F);
  //mu_mu_p_F ~ normal(0, 5);
  //sigma_mu_p_F ~ normal(0, 1);
  
  sigma_p_F ~ normal(0, 1);
  
  // priors:  Instagram user ratio
  //mu_p_G ~ normal(0, 3);
  //mu_p_G ~ normal(mu_mu_p_G, sigma_mu_p_G);
  //mu_mu_p_G ~ normal(0, 5);
  //sigma_mu_p_G ~ normal(0, 1);
  
  //sigma_p_G ~ normal(0, 1);
}
