functions {
  // Slice vector for specific time t accounting for I oblasts, G age groups, and S sexes
  array[] int t_slice(int t, int I, int G, int S) {
    int C = I * G * S; // Total combinations per time t
    int a = 1 + (t - 1) * C; // Starting index for time t
    int n = C; // Number of elements for time t

    array[n] int result; // Resulting indexes
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }

  // Slice vector for previous time t-1 accounting for I oblasts, G age groups, and S sexes
  array[] int t_slice_lag(int t, int I, int G, int S) {
    int C = I * G * S; 
    int a = 1 + (t - 2) * C; // Starting index for time t-1
    int n = C; 

    array[n] int result; 
    for (i in 1:n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
}

data {
  // Dimensions
  int<lower=1> T; // Number of weeks
  int<lower=1> I; // Number of locations (oblasts)
  int<lower=1> G; // Number of age groups
  int<lower=1> S; // Number of sexes

  // Baseline 
  vector<lower=0>[I * G * S] N0; // Baseline population N[1, i, g, s]

    vector<lower=0>[T] y_N_tot; // Total observed population at time t

  // Facebook data
  int<lower=1> n_F; // total sample size for F
  array[n_F] int<lower=1> y_F;  // Facebook daily active users
  array[n_F] int<lower=1> idx_F; // Indices mapping to N and p_F

  // Instagram data
  int<lower=1> n_G; // total sample size for G
  array[n_G] int<lower=1> y_G; / Instagram daily active users
  array[n_G] int<lower=1> idx_G; // Indices mapping to N and p_G

  // Observed ratio
  int<lower=1> n_FG; // total sample size with F and G
  vector<lower=0>[n_FG] y_FG_ratio; // ratio of G to F
  array[n_FG] int<lower=1> idx_FG; // Indices mapping to N, p_F, and p_G
}

parameters {
  // Population
  vector<lower=0>[I * G * S * (T - 1)] r; // Growth rates for each combination

  // Facebook user ratios
  vector<lower=0>[T * I * G * S] p_F; // Facebook user ratios
  vector<lower=0>[T] mu_p_F;
  real<lower=0> sigma_p_F; 

  // Instagram user ratios
  vector<lower=0>[T * I * G * S] p_G; // Instagram user ratios
  vector<lower=0>[I * G * S] mu_p_G; 
  real<lower=0> sigma_p_G; 
}

transformed parameters {
  vector<lower=0>[T * I * G * S] N; // population estimates
  vector<lower=0>[T] N_tot; // total population at each time step
  vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios

  // Population process
  for (n in 1:(I * G * S)) {
    N[n] = N0[n];
  }

  
  for (t in 2:T) {
    array[I * G * S] int idx_t = t_slice(t, I, G, S);
    array[I * G * S] int idx_t_lag = t_slice_lag(t, I, G, S);
    for (n in 1:(I * G * S)) {
      N[idx_t[n]] = N[idx_t_lag[n]] * r[(t - 2) * I * G * S + n];
    }
  }


   for (t in 1:T) {
    array[I * G * S] int idx_t = t_slice(t, I, G, S);
    N_tot[t] = sum(N[idx_t]);
  }

  
  for (n in 1:n_FG) {
    FG_ratio[n] = p_G[idx_FG[n]] / p_F[idx_FG[n]];
  }
}

model {
  
  for (n in 1:n_F) {
    y_F[n] ~ poisson(N[idx_F[n]] * p_F[idx_F[n]]);
  }

  for (n in 1:n_G) {
    y_G[n] ~ poisson(N[idx_G[n]] * p_G[idx_G[n]]);
  }

  y_FG_ratio ~ lognormal(log(FG_ratio), 0.01); 

  y_N_tot ~ lognormal(log(N_tot), 0.005);

  for (t in 1:T) {
    array[I * G * S] int idx_t = t_slice(t, I, G, S);
    p_F[idx_t] ~ lognormal(log(mu_p_F[t]), sigma_p_F);
  }
  mu_p_F ~ lognormal(0, 1); 
  sigma_p_F ~ normal(0, 1); 

  
 for (n in 1:(I * G * S)) {
    for (t in 1:T) {
      int idx = (t - 1) * I * G * S + n;
      p_G[idx] ~ lognormal(log(mu_p_G[n]), sigma_p_G);
    }
  }
  mu_p_G ~ lognormal(0, 1); 
  sigma_p_G ~ normal(0, 1); 

  // Population growth rates
  r ~ lognormal(0, 0.05);
}
