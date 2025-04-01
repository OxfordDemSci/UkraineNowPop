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
  int<lower=0> S;
  int<lower=0> K_p;
  int<lower=0> K_r;

  vector<lower=0>[T] y_N_tot;
  array[S] vector<lower=0>[I] N0;

  array[S] matrix[T * I, K_r] X_r;
  array[S] matrix[T * I, K_p] X_p;

  int<lower=0> n_F;
  vector<lower=0>[n_F] y_F;
  array[n_F] int<lower=1, upper=S> sex_F;
  array[n_F] int<lower=1> ti_F;

  int<lower=0> n_G;
  vector<lower=0>[n_G] y_G;
  array[n_G] int<lower=1, upper=S> sex_G;
  array[n_G] int<lower=1> ti_G;

  array[T * I] int<lower=0> tt;
  array[T * I] int<lower=0> ii;
}

parameters {
  array[S, T - 1] vector[I - 1] logit_pi_free;
  real log_sigma_pi;
  real log_sigma_r;

  real alpha_r;
  vector[K_r] beta_r;

  real alpha_p;
  real phi_p;
  vector[K_p] beta_p;
  real log_sigma_F;
  real log_sigma_G;

  array[S] vector[T] delta_p;
  array[S] vector[I] gamma_p;
  real log_sigma_delta_p;
  real log_sigma_gamma_p;
}

transformed parameters {
  array[S] vector<lower=0>[T * I] N;
  array[S] vector[T * I] r;

  array[T, S] simplex[I] pi;
  array[T, S] vector[I - 1] logit_pi;

  array[S] vector<lower=0>[T * I] p_F;
  array[S] vector<lower=0>[T * I] p_G;

  vector[n_F + n_G] log_lik;

  for (s in 1:S) {
    pi[1, s] = N0[s] / sum(N0[s]);
    
    array[I] int slice1 = t_slice(1, I);
    
    for (i in 1:I)
      N[s][slice1[i]] = y_N_tot[1] * pi[1, s][i];

    for (i in 1:(I - 1))
      logit_pi[1, s][i] = log(pi[1, s][i] / pi[1, s][I]);

    for (i in 1:I)
      r[s][slice1[i]] = 0;

    for (t in 2:T) {
      array[I] int slice_t = t_slice(t, I);
      array[I] int slice_t_lag = t_slice_lag(t, I);

      logit_pi[t, s] = logit_pi_free[s, t - 1];

      {
        vector[I] temp;
        for (i in 1:(I - 1))
          temp[i] = logit_pi[t, s][i];
        temp[I] = 0;
        pi[t, s] = softmax(temp);
      }

      for (i in 1:I)
        N[s][slice_t[i]] = y_N_tot[t] * pi[t, s][i];

      for (i in 1:I)
        r[s][slice_t[i]] = log(N[s][slice_t[i]] / N[s][slice_t_lag[i]]);
    }
  }

  for (s in 1:S) {
    for (i in 1:(T * I)) {
      p_F[s][i] = exp(alpha_p + delta_p[s][tt[i]] + gamma_p[s][ii[i]] + dot_product(row(X_p[s], i), beta_p));
      p_G[s][i] = exp(p_F[s][i] + phi_p);
    }
  }

  for (i in 1:n_F) {
  int s = sex_F[i];
  log_lik[i] = lognormal_lpdf(y_F[i] | 
    log(N[s][ti_F[i]] * p_F[s][ti_F[i]]), exp(log_sigma_F));
}

for (i in 1:n_G) {
  int s = sex_G[i];
  log_lik[i + n_F] = lognormal_lpdf(y_G[i] | 
    log(N[s][ti_G[i]] * p_G[s][ti_G[i]]), exp(log_sigma_G));
}
}

model {
  target += sum(log_lik);

// Prior: growth rates per age-sex combination, allowing each combination having different covariates
  for (s in 1:S){
    r[s] ~ normal(alpha_r + X_r[s] * beta_r, exp(log_sigma_r));
  }
  
  // Priors on random effects for detection rates
  for (s in 1:S) {
    delta_p[s] ~ normal(0, exp(log_sigma_delta_p));
    gamma_p[s] ~ normal(0, exp(log_sigma_gamma_p));
  }
}
generated quantities {
  vector[n_F] F_hat;
  vector[n_G] G_hat;

  for (i in 1:n_F)
    F_hat[i] = lognormal_rng(log(N[sex_F[i]][ti_F[i]] * p_F[sex_F[i]][ti_F[i]]), exp(log_sigma_F));

  for (i in 1:n_G)
    G_hat[i] = lognormal_rng(log(N[sex_G[i]][ti_G[i]] * p_G[sex_G[i]][ti_G[i]]), exp(log_sigma_G));
}
