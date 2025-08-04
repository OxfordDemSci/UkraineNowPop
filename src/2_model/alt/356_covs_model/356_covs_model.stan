data {
  int<lower=1> T;
  int<lower=1> I;
  int<lower=1> A;
  int<lower=1> S;
  int<lower=1> C_full;               // = I * A * S
  int<lower=1> C_adult;              // = I * (A-1) * S
  int<lower=0> K_r;
  int<lower=0> K_p;

  vector<lower=1>[T] y_N_tot;
  matrix[T * I * (A-1) * S, K_r] X_r;
  matrix[T * I * (A-1) * S, K_p] X_p;

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

  vector<lower=1e-6>[T * I * S] ratio_children_female;

  vector<lower=1>[C_adult] A0;   
  vector<lower=1>[C_adult] A1;   
  vector<lower=1>[C_adult] A2;   
  vector<lower=1>[I * S]   Y0;
  vector<lower=1>[I * S]   Y1;
  vector<lower=1>[I * S]   Y2;

  array[T * (I * S)] int<lower=1, upper=T*C_full> young_idx;
  array[T * C_adult] int<lower=1, upper=T*C_full> adult_idx;

  array[T, C_full] int slice_full;
  array[T, C_adult] int slice_adult;
  array[T, C_adult] int slice_adult_lag;
  array[T, I * S] int slice_young;

  int<lower=1> N_fem_a;
  array[T * I, 3] int<lower=1, upper=T*C_adult> idx_female_triple;
}

parameters {
  vector[T * C_adult] log_r; 
  real alpha_r;
  vector[K_r] beta_r;
  real<lower=0> sigma_r;

  real<lower=0> sigma_delta_r;
  real<lower=0> sigma_gamma_r_i;
  real<lower=0> sigma_gamma_r_a;
  real<lower=0> sigma_gamma_r_s;

  vector[T] delta_r_raw;
  vector[I] gamma_r_i_raw;
  vector[A-1] gamma_r_a_raw;
  vector[S] gamma_r_s_raw;

  
  real alpha_p;
  vector[K_p] beta_p;
  real phi_p;

  real<lower=0> sigma_delta_p;
  real<lower=0> sigma_gamma_p_i;
  real<lower=0> sigma_gamma_p_a;
  real<lower=0> sigma_gamma_p_s;

  vector[T] delta_p_raw;
  vector[I] gamma_p_i_raw;
  vector[A-1] gamma_p_a_raw;
  vector[S] gamma_p_s_raw;

  real<lower=1e-26> sigma_F;
  real<lower=1e-26> sigma_G;

  real<lower=0> sigma_phi_p_i;
  real<lower=0> sigma_phi_p_a;
  real<lower=0> sigma_phi_p_s;

  vector[I] phi_p_i_raw;
  vector[A-1] phi_p_a_raw;
  vector[S] phi_p_s_raw;
}

transformed parameters {
  vector[T] delta_p = sigma_delta_p   * delta_p_raw;
  vector[I] gamma_p_i = sigma_gamma_p_i * gamma_p_i_raw;
  vector[A-1] gamma_p_a = sigma_gamma_p_a * gamma_p_a_raw;
  vector[S] gamma_p_s = sigma_gamma_p_s * gamma_p_s_raw;

  vector[T * I * (A-1) * S] nu_p = alpha_p 
                                    + delta_p[tt] 
                                    + gamma_p_i[ii]
                                    + gamma_p_a[aa] 
                                    + gamma_p_s[ss] 
                                    + X_p * beta_p;

  vector[I] phi_p_i = sigma_phi_p_i * phi_p_i_raw;
  vector[A-1] phi_p_a = sigma_phi_p_a * phi_p_a_raw;
  vector[S] phi_p_s = sigma_phi_p_s * phi_p_s_raw;

  vector[T * I * (A-1) * S] p_F = inv_logit(nu_p);
  vector[T * I * (A-1) * S] p_G = inv_logit(nu_p + phi_p_i[ii] + phi_p_a[aa] + phi_p_s[ss]);

 
 
  vector[T] delta_r = sigma_delta_r * delta_r_raw;
  vector[I] gamma_r_i = sigma_gamma_r_i * gamma_r_i_raw;
  vector[A-1] gamma_r_a = sigma_gamma_r_a * gamma_r_a_raw;
  vector[S] gamma_r_s = sigma_gamma_r_s * gamma_r_s_raw;


  vector[T * I * (A-1) * S] nu_r_log = alpha_r 
                                    + delta_r[tt] 
                                    + gamma_r_i[ii]
                                    + gamma_r_a[aa] 
                                    + gamma_r_s[ss]
                                    + X_r * beta_r;  

  vector[T * I * (A-1) * S] N_adult;
  vector[T * I * S] N_young;

  N_adult[slice_adult[1]]   = A0;
  N_adult[slice_adult[80]]  = A1;
  N_adult[slice_adult[117]] = A2;

  vector[T * C_adult] r = exp(log_r);
  for (t in 2:T) {
    if (t == 80 || t == 117) continue;
    N_adult[slice_adult[t]] = N_adult[slice_adult_lag[t]] .* r[slice_adult[t]];
  }

  vector[T * I] N_female_repof = rep_vector(0, T * I);
  for (a in 1:3)
    N_female_repof += N_adult[idx_female_triple[, a]];


  vector[T * I * S] N_female_repof_sex = to_vector(rep_matrix(N_female_repof', S)');
  N_young = N_female_repof_sex .* ratio_children_female;
  N_young[slice_young[1]] = Y0;
  N_young[slice_young[80]] = Y1;
  N_young[slice_young[117]] = Y2;

  vector[T * C_full] N_full;
  N_full[young_idx] = N_young;
  N_full[adult_idx] = N_adult;

  vector[T] N_tot;
  for (t in 1:T)
    N_tot[t] = sum(N_full[slice_full[t]]);
}

model {
  alpha_r ~ normal(0, 0.2);
  beta_r  ~ normal(0, 0.5);
  sigma_r ~ normal(0, 0.05);          // ~±5 % weekly shift

  alpha_p ~ normal(0, 2);
  beta_p  ~ normal(0, 0.5);
  phi_p   ~ normal(0, 0.3);


  sigma_delta_r   ~ normal(0, 0.05);
  sigma_gamma_r_i ~ normal(0, 0.05);
  sigma_gamma_r_a ~ normal(0, 0.05);
  sigma_gamma_r_s ~ normal(0, 0.05);

  delta_r_raw   ~ std_normal();
  gamma_r_i_raw ~ std_normal();
  gamma_r_a_raw ~ std_normal();
  gamma_r_s_raw ~ std_normal();

  sigma_delta_p   ~ normal(0, 0.05);
  sigma_gamma_p_i ~ normal(0, 0.05);
  sigma_gamma_p_a ~ normal(0, 0.05);
  sigma_gamma_p_s ~ normal(0, 0.05);

  delta_p_raw   ~ std_normal();
  gamma_p_i_raw ~ std_normal();
  gamma_p_a_raw ~ std_normal();
  gamma_p_s_raw ~ std_normal();

  sigma_phi_p_i ~ normal(0, 0.05);
  sigma_phi_p_a ~ normal(0, 0.05);
  sigma_phi_p_s ~ normal(0, 0.05);

  phi_p_i_raw ~ std_normal();
  phi_p_a_raw ~ std_normal();
  phi_p_s_raw ~ std_normal();

  sigma_F ~ exponential(5);
  sigma_G ~ exponential(5);

  log_r ~ normal(nu_r_log, sigma_r);

  for (t in 1:T)
    if (t != 1 && t != 80 && t != 117)
      N_tot[t] ~ lognormal(log(y_N_tot[t]), 0.005);

  y_F ~ lognormal(log(N_adult[tias_F] .* p_F[tias_F]), sigma_F);
  y_G ~ lognormal(log(N_adult[tias_G] .* p_G[tias_G]), sigma_G);
}

generated quantities {
  array[n_F] real F_hat;
  array[n_G] real G_hat;

  for (i in 1:n_F)
    F_hat[i] = lognormal_rng(log(N_adult[tias_F[i]] .* p_F[tias_F[i]]), sigma_F);

  for (j in 1:n_G)
    G_hat[j] = lognormal_rng(log(N_adult[tias_G[j]] .* p_G[tias_G[j]]), sigma_G);
}
