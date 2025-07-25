data {
  int<lower=1> T;
  int<lower=1> I;
  int<lower=1> A;               // total age groups (including young); here 6
  int<lower=1> S;
  int<lower=1> C_full;         //C_full=I*A*S; here 84
  int<lower=1> C_adult;        // I * (A - 1) * S

  int<lower=0> K_r;
  int<lower=0> K_p;

  // Observed TOTAL population per time (Adult + Young)
  vector<lower=1>[T] y_N_tot;
  
  // Covariates
  matrix[T * I * (A-1) * S,  K_r] X_r;
  matrix[T * I * (A-1) * S,  K_p] X_p;

  // Adult-only outcome data
  int<lower=0> n_F;
  int<lower=0> n_G;
  array[n_F] real<lower=0> y_F;
  array[n_G] real<lower=0> y_G;
  array[n_F] int<lower=1> tias_F;
  array[n_G] int<lower=1> tias_G;

  array[T * I * (A-1) * S] int<lower=1> tt;
  array[T * I * (A-1) * S] int<lower=1> ii;
  array[T * I * (A-1) * S] int<lower=1> aa;
  array[T * I * (A-1) * S] int<lower=1> ss;

  vector<lower=1e-6>[T * I * S] ratio_young_adult;

  vector<lower=1>[C_adult] A0;
  vector<lower=1>[C_adult] A1;
  vector<lower=1>[C_adult] A2;

  vector<lower=1>[I * S] Y0;
  vector<lower=1>[I * S] Y1;
  vector<lower=1>[I * S] Y2;

  array[T * (I * S)] int<lower=1,upper= T*C_full> young_idx;  //for young in N_full
  array[T * C_adult] int<lower=1,upper= T*C_full> adult_idx;  //for old in N_full

  array[T, C_full]   int slice_full;
  array[T, C_adult]  int slice_adult;
  array[T, C_adult]  int slice_adult_lag;  
  array[T, I * S]    int slice_young;
  
  int<lower=1> N_fem_a;
  array[T * I, 3] int<lower=1, upper=T*C_adult> idx_female_triple;
  }
parameters {
  vector[T * C_adult] log_r;
  //vector[T * I * S] log_r_y;
  real alpha_r;
  vector[K_r] beta_r;
  real<lower=1e-26> sigma_r;
  
  real alpha_p;
  vector[K_p] beta_p;
  //vector[A-1] phi_p_a;
  real phi_p;
  

  real<lower=1e-26> sigma_F;
  real<lower=1e-26> sigma_G;

  real log_sigma_delta_p;
  real log_sigma_gamma_p_i;
  real log_sigma_gamma_p_a;
  real log_sigma_gamma_p_s;

  real log_sigma_delta_r;
  real log_sigma_gamma_r_i;
  real log_sigma_gamma_r_a;
  real log_sigma_gamma_r_s;

  vector[T] delta_p_raw;
  vector[I] gamma_p_i_raw;
  vector[A-1] gamma_p_a_raw;
  vector[S] gamma_p_s_raw;

  vector[T] delta_r_raw;
  vector[I] gamma_r_i_raw;
  vector[A-1] gamma_r_a_raw;
  vector[S] gamma_r_s_raw;
}

transformed parameters {
  real sigma_delta_r   = exp(log_sigma_delta_r);
  real sigma_gamma_r_i = exp(log_sigma_gamma_r_i);
  real sigma_gamma_r_a = exp(log_sigma_gamma_r_a);
  real sigma_gamma_r_s = exp(log_sigma_gamma_r_s);

  real sigma_delta_p   = exp(log_sigma_delta_p);
  real sigma_gamma_p_i = exp(log_sigma_gamma_p_i);
  real sigma_gamma_p_a = exp(log_sigma_gamma_p_a);
  real sigma_gamma_p_s = exp(log_sigma_gamma_p_s);
  
  vector[T]   delta_r   = sigma_delta_r   * delta_r_raw;
  vector[I]   gamma_r_i = sigma_gamma_r_i * gamma_r_i_raw;
  vector[A-1] gamma_r_a = sigma_gamma_r_a * gamma_r_a_raw;
  vector[S]   gamma_r_s = sigma_gamma_r_s * gamma_r_s_raw;

  vector[T]   delta_p   = sigma_delta_p   * delta_p_raw;
  vector[I]   gamma_p_i = sigma_gamma_p_i * gamma_p_i_raw;
  vector[A-1] gamma_p_a = sigma_gamma_p_a * gamma_p_a_raw;
  vector[S]   gamma_p_s = sigma_gamma_p_s * gamma_p_s_raw;

  vector[T * I * (A-1) * S] nu_r_log
    = alpha_r
    + delta_r[tt]
    + gamma_r_i[ii]
    + gamma_r_a[aa]
    + gamma_r_s[ss]
    + X_r * beta_r;

  vector[T * I * (A-1) * S] nu_p
    = alpha_p
    + delta_p[tt]
    + gamma_p_i[ii]
    + gamma_p_a[aa]
    + gamma_p_s[ss]
    + X_p * beta_p;

  vector[T * I * (A-1) * S] p_F = inv_logit(nu_p);
  vector[T * I * (A-1) * S] p_G = inv_logit(nu_p + phi_p);


  vector[T * I * (A-1) * S] N_adult;

  N_adult[slice_adult[1]] = A0;
  N_adult[slice_adult[80]] = A1;
  N_adult[slice_adult[117]] = A2;
  
  vector[T * C_adult]  r = exp(log_r);
  for (t in 2:T) {
    if (t == 80 || t == 117) continue;
    N_adult[slice_adult[t]] = N_adult[slice_adult_lag[t]] .* r[slice_adult[t]];
  }

vector[T * I] N_female_repof = rep_vector(0, T * I);
for (a in 1:3)
  N_female_repof += N_adult[idx_female_triple[, a]];



vector[T * I * S] N_female_repof_sex = to_vector(rep_matrix(N_female_repof', S)');

vector[T * I * S] N_young = N_female_repof_sex .* ratio_young_adult; 
N_young[slice_young[1]]   = Y0;
N_young[slice_young[80]]  = Y1;
N_young[slice_young[117]] = Y2;

//vector[T * I * S] r_y = exp(log_r_y);

vector[T * C_full] N_full;
N_full[young_idx] = N_young;
N_full[adult_idx] = N_adult;

  
  vector[T] N_tot;
  for (t in 1:T)
    N_tot[t] = sum(N_full[slice_full[t]]);

}

model {
  sigma_F ~ exponential(1);
  sigma_G ~ exponential(1);
  
  alpha_r ~ normal(0, 2);
  beta_r  ~ normal(0, 1);
  sigma_r ~ normal(0, 1);
  alpha_p ~ normal(0, 2);
  beta_p  ~ normal(0, 0.5);
  //phi_p_a ~ normal(0, 1);
  phi_p ~ normal(0, 1);
  
  delta_r_raw   ~ normal(0, 0.5);
  gamma_r_i_raw ~ normal(0, 0.5);
  gamma_r_a_raw ~ normal(0, 0.5);
  gamma_r_s_raw ~ normal(0, 0.5);

  delta_p_raw   ~ normal(0, 0.5);
  gamma_p_i_raw ~ normal(0, 0.5);
  gamma_p_a_raw ~ normal(0, 0.5);
  gamma_p_s_raw ~ normal(0, 0.5);

  //log_sigma_delta_r   ~ normal(log(0.1), 1);
  //log_sigma_gamma_r_i ~ normal(log(0.1), 1);
  //log_sigma_gamma_r_a ~ normal(log(0.1), 1);
  //log_sigma_gamma_r_s ~ normal(log(0.1), 1);

  //log_sigma_delta_p   ~ normal(log(0.1), 1);
  //log_sigma_gamma_p_i ~ normal(log(0.1), 1);
  //log_sigma_gamma_p_a ~ normal(log(0.1), 1);
  //log_sigma_gamma_p_s ~ normal(log(0.1), 1);

  

  // Adult growth multipliers
  log_r ~ normal(nu_r_log, sigma_r);

  //log_r_y ~ normal(log(r_female_mean_sex), 0.05);
  
  for (t in 1:T) {
    if (t != 80 && t != 117 && N_tot[t] > 0)
    N_tot[t] ~ lognormal(log(y_N_tot[t]), 0.005);
  }

  target += lognormal_lpdf(y_F | log(N_adult[tias_F] .* p_F[tias_F]), sigma_F);
  target += lognormal_lpdf(y_G | log(N_adult[tias_G] .* p_G[tias_G]), sigma_G);
}

generated quantities {
  array[n_F] real F_hat;
  array[n_G] real G_hat;

  
  for (i in 1:n_F) {
    F_hat[i] = lognormal_rng( log(N_adult[tias_F[i]] .* p_F[tias_F[i]]), sigma_F);
  }

  for (j in 1:n_G) {
    G_hat[j] = lognormal_rng( log(N_adult[tias_G[j]] .* p_G[tias_G[j]]), sigma_G);
  }
}
