functions {
  // slice T * (I * A * S) vector for specific time t
  array[] int t_slice(int t, int I, int A, int S) {
    int C = I * A * S;
    int a = 1 + (t - 1) * C;
    array[C] int result;
    for (i in 1:C)
      result[i] = a + i - 1;
    return result;
  }
  // slice for previous time t−1
  array[] int t_slice_lag(int t, int I, int A, int S) {
    int C = I * A * S;
    int a = 1 + (t - 2) * C;
    array[C] int result;
    for (i in 1:C)
      result[i] = a + i - 1;
    return result;
  }
}

data {
  int<lower=1> T;
  int<lower=1> I;
  int<lower=1> A;
  int<lower=1> S;
  int<lower=0> K_r;
  int<lower=0> K_p;

  vector<lower=0>[T] y_N_tot;
  vector<lower=0>[I * A * S] N0;

  matrix[T * I * A * S, K_r] X_r;
  matrix[T * I * A * S, K_p] X_p;

  int<lower=0> n_F;
  int<lower=0> n_G;
  array[n_F] real<lower=0> y_F;
  array[n_G] real<lower=0> y_G;
  array[n_F] int<lower=1>  tias_F;
  array[n_G] int<lower=1>  tias_G;

  array[T * I * A * S] int<lower=1> tt;
  array[T * I * A * S] int<lower=1> ii;
  array[T * I * A * S] int<lower=1> aa;
  array[T * I * A * S] int<lower=1> ss;
  array[I * A * S] int<lower=1> ias_N0;
}

transformed data {
  int C = I * A * S;
  array[T, C] int slice;
  array[T, C] int slice_lag;
  for (t in 1:T) {
    slice[t] = t_slice(t, I, A, S);
    if (t > 1) slice_lag[t] = t_slice_lag(t, I, A, S);
  }
}

parameters {
  // pop growth
  vector<lower=0>[T * I * A * S] r;
  real alpha_r;
  matrix[A * S, K_r] beta_r;
  real log_sigma_r;

  // detection
  real alpha_p;
  vector[A] phi_p;
  vector[K_p] beta_p;

  real sigma_F;
  real sigma_G;


  vector[T] delta_p;
  real sigma_delta_p;
  vector[I] gamma_p_i;
  vector[A] gamma_p_a;
  vector[S] gamma_p_s;
  real sigma_gamma_i;
  real sigma_gamma_a;
  real sigma_gamma_s;
}

transformed parameters {
  vector[T * I * A * S] N;
  N = rep_vector(0, T * I * A * S);
  N[ias_N0] = N0;
  for (t in 2:T)
    N[slice[t]] = N[slice_lag[t]] .* r[slice[t]];
  
  vector[T] N_tot;
  for (t in 1:T)
    N_tot[t] = sum(N[slice[t]]);


  vector[T * I * A * S] nu = 
      alpha_p
    + delta_p[tt]
    + gamma_p_i[ii]
    + gamma_p_a[aa]
    + gamma_p_s[ss]
    + X_p * beta_p;

  
  vector[T * I * A * S] p_F = exp(nu);
  vector[T * I * A * S] p_G = exp(nu + phi_p[aa]);
}

model {
  r ~ lognormal(eta_r, sigma_r);
  
  
  delta_p   ~ normal(0, sigma_delta_p);
  gamma_p_i ~ normal(0, sigma_gamma_i);
  gamma_p_a ~ normal(0, sigma_gamma_a);
  gamma_p_s ~ normal(0, sigma_gamma_s);

  
  N_tot ~ lognormal(log(y_N_tot), 0.01 / 2);

 vector[T * I * A * S] eta_r;
 for (n in 1:(T * I * A * S)) {
 eta_r[n] = alpha_r + dot_product( X_r[n], beta_r[aa[n], ss[n]] );
  }
  
  target += lognormal_lpdf(y_F | log(N[tias_F] .* p_F[tias_F]), sigma_F);
  target += lognormal_lpdf(y_G | log(N[tias_G] .* p_G[tias_G]), sigma_G);
}

generated quantities {
  array[n_F] real muF;
  array[n_F] real sigmaF;
  array[n_F] real F_hat;

  array[n_G] real muG;
  array[n_G] real sigmaG;
  array[n_G] real G_hat;

  
  for (i in 1:n_F) {
    muF[i]    = log( N[tias_F[i]] * p_F[tias_F[i]] );
    }
  
  for (j in 1:n_G) {
    muG[j]    = log( N[tias_G[j]] * p_G[tias_G[j]] );
    }

  F_hat = lognormal_rng(muF, sigmaF);
  G_hat = lognormal_rng(muG, sigmaG);
}
