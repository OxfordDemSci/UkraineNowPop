functions {
  array[] int t_slice(int t, int I, int A, int S) {
    int C = I * A * S;          
    int a = 1 + (t - 1) * C;    
    array[C] int result;
    for (i in 1:C)
      result[i] = a + i - 1;
    return result;
  }
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
  int<lower=1> C;  

  int<lower=0> K_r;  
  int<lower=0> K_p;  

  vector<lower=0>[T] y_N_tot;     
  vector<lower=0>[C] N0;        

  matrix[T * C, K_r] X_r;
  matrix[T * C, K_p] X_p; 
  
  int<lower=0> n_F;
  vector<lower=0>[n_F] y_F;
  array[n_F] int<lower=1,upper=C> ias_F; 
  array[n_F] int<lower=1,upper=T> t_F;

  int<lower=0> n_G;
  vector<lower=0>[n_G] y_G;
  array[n_G] int<lower=1,upper=C> ias_G;
  array[n_G] int<lower=1,upper=T> t_G;

  array[T*C] int<lower=1,upper=T> tt;
  array[T*C] int<lower=1,upper=I> ii;
  array[T*C] int<lower=1,upper=A> aa;
  array[T*C] int<lower=1,upper=S> ss;
}
transformed data {
  array[n_F] int idx_F;
  for (n in 1:n_F)
    idx_F[n] = (t_F[n] - 1) * C + ias_F[n];

  array[n_G] int idx_G;
  for (m in 1:n_G)
    idx_G[m] = (t_G[m] - 1) * C + ias_G[m];
}
parameters {
  // Logits for the joint (i,a,s) simplex at t>1
  array[T-1] vector[C-1] logit_pi_free;

  real alpha_r;
  vector[K_r] beta_r;
  real log_sigma_r;

  // Detection probability submodel
  real alpha_p;
  vector[K_p] beta_p;
  real phi_p;
  real log_sigma_F;
  real log_sigma_G;

  vector[T] delta_p; 
  vector[I] gamma_p_i;
  vector[A] gamma_p_a;
  vector[S] gamma_p_s;
  real log_sigma_delta_p;
  real log_sigma_gamma_i;
  real log_sigma_gamma_a;
  real log_sigma_gamma_s;
}

transformed parameters {
  vector[T * C] N;   
  vector[T * C] r;    
  array[T] simplex[C] pi;

  vector[T * C] p_F;  
  vector[T * C] p_G;

  vector[n_F + n_G] log_lik;

  // t = 1 
  pi[1] = N0 / sum(N0);
  {
    array[C] int sl1 = t_slice(1, I, A, S);
    for (c in 1:C) {
      int idx = sl1[c];
      N[idx] = y_N_tot[1] * pi[1][c];
      
      r[idx] = 0;
    }
  }

  // t>1 
  for (t in 2:T) {
    vector[C] tmp;
    for (c in 1:(C-1))
     tmp[c] = logit_pi_free[t-1][c];
     tmp[C] = 0;
    
    pi[t] = softmax(tmp);

    array[C] int sl = t_slice(t, I, A, S);
    array[C] int sllag = t_slice_lag(t, I, A, S);

    for (c in 1:C) {
      int idx    = sl[c];
      int idx_l  = sllag[c];
      real pct   = pi[t][c];
      
      N[idx] = y_N_tot[t] * pct;
      r[idx] = log( N[idx] / N[idx_l] );
    }
  }

  // Detection‐probabilities
  vector[T * C] nu = rep_vector(alpha_p, T * C)
               + delta_p[tt]
               + gamma_p_i[ii]
               + gamma_p_a[aa]
               + gamma_p_s[ss]
               + X_p * beta_p; 

  p_F = exp(nu);
  p_G = exp(nu + phi_p);
               
  // log‐likelihood
  vector[n_F] mu_F = log( N[idx_F] .* p_F[idx_F] );
  vector[n_G] mu_G = log( N[idx_G] .* p_G[idx_G] );
}

model {
  // data likelihood
  target += lognormal_lpdf(y_F | mu_F, exp(log_sigma_F)) + lognormal_lpdf(y_G | mu_G, exp(log_sigma_G));

  // growth‐rate priors on each (t,c)
  r ~ normal( alpha_r + X_r * beta_r, exp(log_sigma_r) );


  delta_p ~ normal(0, exp(log_sigma_delta_p));
  gamma_p_i ~ normal(0, exp(log_sigma_gamma_i));
  gamma_p_a ~ normal(0, exp(log_sigma_gamma_a));
  gamma_p_s ~ normal(0, exp(log_sigma_gamma_s));
}

generated quantities {
  vector[n_F] F_hat;
  vector[n_G] G_hat;

  for (n in 1:n_F)
    F_hat[n] = lognormal_rng(mu_F[n], exp(log_sigma_F));

  for (m in 1:n_G)
    G_hat[m] = lognormal_rng(mu_G[m], exp(log_sigma_G));
}
