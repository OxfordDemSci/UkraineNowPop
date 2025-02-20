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

  // indexing for long format
  array[T * I * G * S] int<lower=0> tt; // year index for tias vector      
  array[T * I * G * S] int<lower=0> ii; // location index for tias vector  
  array[T * I * G * S] int<lower=0> gg; // location index for tias vector
  array[T * I * G * S] int<lower=0> ss; // location index for tias vector
  
  array[T * I * G * S] int<lower=0> ti; 
  array[T * I * G * S] int<lower=0> ta; 
  array[T * I * G * S] int<lower=0> ts; 
  array[T * I * G * S] int<lower=0> ia; 
  array[T * I * G * S] int<lower=0> is; 
  array[T * I * G * S] int<lower=0> as;  

  //int<lower=0> K_r; // number of covariates on population growth rates
    
  vector<lower=0>[T] y_N_tot; // total population among locations
  vector<lower=0>[I * G * S] N0; // baseline population at each location
  
  // Facebook data
  int<lower=0> n_F; // total sample size for F
  array[n_F] int<lower=0> y_F; // Facebook daily active users
  
  // Instagram data
  int<lower=0> n_G; // total sample size for G
  array[n_G] int<lower=0> y_G; // Instagram daily active users
  
  array[I * G * S] int<lower=0> tias_N0; // c index for N0
  array[T * C - C] int<lower=0> tias_N; // tc (year, c) index for N[t]      
  
  array[n_F] int<lower=0> tias_F; // tias (year, location, age, sex) index for F
  array[n_G] int<lower=0> tias_G; // tias (year, location, age, sex) index for G
}
parameters {
  vector[T * I] alpha_ti;    // time-location interaction effects
  vector[T * G] alpha_ta;    // time-age interaction effects
  vector[T * S] alpha_ts;    // time-sex interaction effects
  vector[I * G] alpha_ia; // location-age interaction effects
  vector[I * S] alpha_is; // location-sex interaction effects
  vector[G * S] alpha_as; // age-sex interaction effects
  
  vector<lower=0>[G - 1] alpha_a_raw; // Age-specific effects (excluding youngest age group)
  vector<lower=0>[S - 1] alpha_s_raw; // Sex-specific effects (excluding females)

  
  real<lower=0> sigma_alpha_ti;
  real<lower=0> sigma_alpha_ta;
  real<lower=0> sigma_alpha_ts;
  real<lower=0> sigma_alpha_ia;
  real<lower=0> sigma_alpha_is;
  real<lower=0> sigma_alpha_as;

  real<lower=0> sigma_N; // variation in scaled population sizes
  real<lower=0> sigma_N_star; // variation in true population sizes
  
  vector<lower=0>[T * I * G * S] p_F; // Facebook detection ratios
  vector<lower=0>[T * I * G * S] p_G; // Instagram detection ratios
  
  real alpha_p; // intercept for Facebook detection rates
  real phi_p; // intercept offset for Instagram detection rates
  real<lower=0> sigma_p_F; // residual variation
  real<lower=0> sigma_p_G; // residual variation
  
  vector[T] delta_p;
  real<lower=0> sigma_delta_p;
  
  vector[I] gamma_p;
  real<lower=0> sigma_gamma_p;

}
transformed parameters {
  vector<lower=0>[T] N_tot; // total population at each time step
  vector[T * I * G * S] mu_p; // expected Facebook detection rates
  vector<lower=0>[T * C] N_star; // True population stocks
  vector[T * C] N;    // Scaled population stock
 
  vector[I] alpha_i;  
  vector[G] alpha_a;  
  vector[S] alpha_s;

  alpha_i[1] = 0;
  for (g in 2:I) {
    alpha_i[i] = alpha_i_raw[i - 1];
  }
  
  alpha_a[1] = 0;
  for (g in 2:G) {
    alpha_a[g] = alpha_a_raw[g - 1];
  }

  // Set females to 0
  alpha_s[1] = 0;
  for (s in 2:S) {
    alpha_s[s] = alpha_s_raw[s - 1];
  }


  mu_p = alpha_p + delta_p[tt] + gamma_p[ii];

 
  N_star[tias_N0] = N0;
  N[tias_N0] = N0;
  

  // total population
  for (t in 1 : T) {
    N_tot[t] = sum(N[t_slice(t, C)]);
  }

  
}

model {
  y_N_tot ~ lognormal(log(N_tot), 0.01 / 2);
  
  
  y_F ~ poisson(N[tias_F] .* p_F[tias_F]);
  y_G ~ poisson(N[tias_G] .* p_G[tias_G]);
  
  p_F ~ lognormal(mu_p, sigma_p_F);
  p_G ~ lognormal(mu_p + phi_p, sigma_p_G);

  


 for (t in 2 : T) {
    N[t_slice(t, C)] ~ lognormal( log(N_star[t_slice_lag(t, C)])
                          + alpha_ti[ti[t_slice(t, C)]]
                          + alpha_ta[ta[t_slice(t, C)]]
                          + alpha_ia[ia[t_slice(t, C)]]
                          + alpha_as[as[t_slice(t, C)]], sigma_N);   


    N_star[t_slice(t, C)] ~ lognormal( beta[tt], sigma_N_star);   // include covariates here
  }

  

  alpha_ti ~ normal(alpha_i[ii], sigma_alpha_ti);
  alpha_ta ~ normal(alpha_a[gg], sigma_alpha_ta);
  alpha_ia ~ normal(alpha_a[gg], sigma_alpha_ia);
  alpha_as ~ normal(alpha_s[ss], sigma_alpha_as);

  alpha_i_raw ~ normal(0, 1);
  alpha_a_raw ~ normal(0, 1);
  alpha_s_raw ~ normal(0, 1);

  sigma_alpha_ti ~ normal(0, 1);
  sigma_alpha_ta ~ normal(0, 1);
  sigma_alpha_ia ~ normal(0, 1);
  sigma_alpha_as ~ normal(0, 1);



  alpha_p ~ normal(0, 5);
  phi_p ~ normal(0, 1);
  
  sigma_p_F ~ normal(0, 1);
  sigma_p_G ~ normal(0, 1);
  
  delta_p ~ normal(0, sigma_delta_p);
  sigma_delta_p ~ normal(0, 1);

  beta ~ normal(0, 1)

  sigma_N ~ normal(0, 1);
  sigma_N_star ~ normal(0, 1);
    
}
