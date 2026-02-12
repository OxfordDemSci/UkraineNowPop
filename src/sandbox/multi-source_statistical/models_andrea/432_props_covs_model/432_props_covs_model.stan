functions {
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
  int<lower=0> T;
  int<lower=0> I;
  int<lower=0> C;
  int<lower=0> K_p;
  int<lower=0> K_r;

  vector<lower=0>[T] y_N_tot;
  array[C] vector<lower=0>[I] N0;

  array[C] matrix[T * I, K_r] X_r;
  array[C] matrix[T * I, K_p] X_p;

  int<lower=0> n_F;
  vector<lower=0>[n_F] y_F;
  array[n_F] int<lower=1, upper=C> comb_F;
  array[n_F] int<lower=1> ti_F;

  int<lower=0> n_G;
  vector<lower=0>[n_G] y_G;
  array[n_G] int<lower=1, upper=C> comb_G;
  array[n_G] int<lower=1> ti_G;

  array[T * I] int<lower=0> tt;
  array[T * I] int<lower=0> ii;
}

parameters {
  array[C, T - 1] vector[I - 1] logit_pi_free;
  real log_sigma_pi;
  real log_sigma_r;

  real alpha_r;
  vector[K_r] beta_r;

  real alpha_p;
  real phi_p;
  vector[K_p] beta_p;
  real log_sigma_F;
  real log_sigma_G;

  array[C] vector[T] delta_p;
  array[C] vector[I] gamma_p;
  real log_sigma_delta_p;
  real log_sigma_gamma_p;
}

transformed parameters {
  array[C] vector<lower=0>[T * I] N;
  array[C] vector[T * I] r;

  array[T, C] simplex[I] pi;
  array[T, C] vector[I - 1] logit_pi;

  array[C] vector<lower=0>[T * I] p_F;
  array[C] vector<lower=0>[T * I] p_G;

  vector[n_F + n_G] log_lik;

  for (c in 1:C) {
    pi[1, c] = N0[c] / sum(N0[c]);
    
    array[I] int slice1 = t_slice(1, I);
    
    for (i in 1:I)
      N[c][slice1[i]] = y_N_tot[1] * pi[1, c][i];

    for (i in 1:(I - 1))
      logit_pi[1, c][i] = log(pi[1, c][i] / pi[1, c][I]);

    for (i in 1:I)
      r[c][slice1[i]] = 0;

    for (t in 2:T) {
      array[I] int slice_t = t_slice(t, I);
      array[I] int slice_t_lag = t_slice_lag(t, I);

      logit_pi[t, c] = logit_pi_free[c, t - 1];

      {
        vector[I] temp;
        for (i in 1:(I - 1))
          temp[i] = logit_pi[t, c][i];
        temp[I] = 0;
        pi[t, c] = softmax(temp);
      }

      for (i in 1:I)
        N[c][slice_t[i]] = y_N_tot[t] * pi[t, c][i];

      for (i in 1:I)
        r[c][slice_t[i]] = log(N[c][slice_t[i]] / N[c][slice_t_lag[i]]);
    }
  }

  for (c in 1:C) {
    for (i in 1:(T * I)) {
      p_F[c][i] = exp(alpha_p + delta_p[c][tt[i]] + gamma_p[c][ii[i]] + dot_product(row(X_p[c], i), beta_p));
      p_G[c][i] = exp(p_F[c][i] + phi_p);
    }
  }

  for (i in 1:n_F) {
  int c = comb_F[i];
  log_lik[i] = lognormal_lpdf(y_F[i] | 
    log(N[c][ti_F[i]] * p_F[c][ti_F[i]]), exp(log_sigma_F));
}

for (i in 1:n_G) {
  int c = comb_G[i];
  log_lik[i + n_F] = lognormal_lpdf(y_G[i] | 
    log(N[c][ti_G[i]] * p_G[c][ti_G[i]]), exp(log_sigma_G));
}
}

model {
  target += sum(log_lik);

// Prior: growth rates per age-sex combination, allowing each combination having different covariates
  for (c in 1:C){
    r[c] ~ normal(alpha_r + X_r[c] * beta_r, exp(log_sigma_r));
  }
  
  // Priors on random effects for detection rates
  for (c in 1:C) {
    delta_p[c] ~ normal(0, exp(log_sigma_delta_p));
    gamma_p[c] ~ normal(0, exp(log_sigma_gamma_p));
  }
}
generated quantities {
  vector[n_F] F_hat;
  vector[n_G] G_hat;

  for (i in 1:n_F)
    F_hat[i] = lognormal_rng(log(N[comb_F[i]][ti_F[i]] * p_F[comb_F[i]][ti_F[i]]), exp(log_sigma_F));

  for (i in 1:n_G)
    G_hat[i] = lognormal_rng(log(N[comb_G[i]][ti_G[i]] * p_G[comb_G[i]][ti_G[i]]), exp(log_sigma_G));
}
