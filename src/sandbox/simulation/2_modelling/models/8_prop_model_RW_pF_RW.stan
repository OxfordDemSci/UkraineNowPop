functions {
  // slice ti vector for specific year t
  array[] int t_slice(int t, int I) {
    int a = 1 + t * I - I; // starting index for year t
    int b = t * I; // ending index for year t
    int n = b - a + 1; // number of elements for year t
    
    array[n] int result; // result of indexes
    for (i in 1 : n) {
      result[i] = a + (i - 1);
    }
    return result;
  }
  
  // slice ti vector for specific previous year t-1
  array[] int t_slice_lag(int t, int I) {
    int a = 1 + t * I - 2 * I; // starting index for year t-1
    int b = t * I - I; // ending index for year t-1
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
  
  // baseline
  vector<lower=0>[T] y_N_tot; // total population among locations
  vector<lower=0>[I] N0; // baseline population at each location
  // array[I] int N0;
  
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
  array[T * I] int<lower=0> tt; // year index for ti vector
  array[T * I] int<lower=0> ii; // location index for ti vector
  
  array[I] int<lower=0> ti_N0; // ti (year, location) index for N0
  array[T * I - I] int<lower=0> ti_N; // ti (year, location) index for N[t]
  array[T * I - I] int<lower=0> ti_N_lag; // ti (year, location) index for N[t-1]
  
  array[n_F] int<lower=0> ti_F; // ti (year, location) index for F
  array[n_G] int<lower=0> ti_G; // ti (year, location) index for G
  array[n_FG] int<lower=0> ti_FG; // ti (year, location) index for F and G
}
parameters {
  // population
  // simplex[I] p0;
  array[T-1] vector[I-1] logit_p; // population proportions in each oblast
  // array[T] vector[I-1] logit_p; // population proportions in each oblast  
  real log_sigma_p;
  
  real<lower=-1,upper=1> ar_tot;
  real log_sigma_tot;
  real mu_tot;
  
  // Facebook
  // vector<lower=0>[T * I] p_F; // Facebook user ratios
  vector[T * I] log_p_F;
  // vector[I] log_p_F;
  // vector[I] mu_p_F; // p_F mean
  real mu_p_F; // p_F mean
  real log_sigma_p_F;
  // real log_nu_p_F;
  vector[I] mu_p_F_i; // location random effect
  real log_sigma_p_F_i;  
  // real<lower=-1,upper=1> ar_p_F; // p_F ar(1) parameter
  
  // Instagram
  // vector<lower=0>[T * I] p_GF;
  vector[T * I] log_p_GF;
  real mu_p_GF;
  real log_sigma_p_GF;
  // vector[I] mu_p_GF_i; // location random effect
  // real log_sigma_p_GF_i;    
  
  real log_kappa_F; // over-dispersion scale parameter
  real log_kappa_G;
  // real<lower=0,upper=1> disp_F; // over-dispersion mixture parameter
  // real<lower=0,upper=1> disp_G; // over-dispersion mixture parameter
  
}
transformed parameters {
  // array[T-1] simplex[I] p;
  array[T] simplex[I] p;
  vector<lower=0>[T * I] N; // population estimates
  // vector<lower=0>[n_FG] FG_ratio; // ratio of Instagram to Facebook user ratios
  // vector[T * I] log_p_F;
  // vector[T * I] log_p_GF;
  vector<lower=0>[T * I] p_F = exp(log_p_F);
  vector<lower=0>[T * I] p_GF = exp(log_p_GF);
  vector<lower=0>[T * I] p_G = p_F .* p_GF;
  // vector<lower=0>[T * I] p_F;
  // vector<lower=0>[T * I] p_G;
  vector[n_F+n_G+T-1] log_lik;
  // vector[n_F+n_G+T] log_lik;
  real log_prior = 0;  
  // vector[I-1] logit_p0 = log(p0[1:(I-1)]) - log(p0[I]);
  vector[I-1] logit_p0;  
  
  // population process model
  // p[1] = softmax(append_row(logit_p[1],1));
  // p[1] = p0;
  p[1] = N0/sum(N0);
  logit_p0 = log(p[1,1:(I-1)]) - log(p[1,I]);
  N[ti_N0] = N0;
  // N[ti_N0] = y_N_tot[1] * p[1]; 
  
  // log_lik[n_F+n_G+T] = multinomial_lpmf(N0 | p0);  
  log_prior += normal_lpdf(logit_p[1] | logit_p0, exp(log_sigma_p));
  for (t in 2 : T) {
    if(t < T){
      // p[t] = softmax(append_row(logit_p[t],1));
      log_prior += normal_lpdf(logit_p[t] | logit_p[t-1],exp(log_sigma_p));
    }
    p[t] = softmax(append_row(logit_p[t-1],1));
  
    // N[t_slice(t, I)] = y_N_tot[t] * p[t-1];
    N[t_slice(t, I)] = y_N_tot[t] * p[t];    
    
    // ar(1) model on total population time series
    log_lik[t-1] = lognormal_lpdf(y_N_tot[t] | mu_tot + ar_tot*(log(y_N_tot[t-1]) - mu_tot),exp(log_sigma_tot));
    
    // time-invariant model on p_F
    log_prior += normal_lpdf(log_p_F[t_slice(t,I)] | mu_p_F + mu_p_F_i, exp(log_sigma_p_F));
    // log_prior += student_t_lpdf(log_p_F[t_slice(t,I)] | exp(log_nu_p_F), mu_p_F + mu_p_F_i, exp(log_sigma_p_F));    
    // log_prior += normal_lpdf(log_p_GF[t_slice(t,I)] | mu_p_GF + mu_p_GF_i, exp(log_sigma_p_GF));
    log_prior += normal_lpdf(log_p_GF[t_slice(t,I)] | mu_p_GF, exp(log_sigma_p_GF));
    // log_p_F[t_slice(t,I)] = mu_p_F + mu_p_F_i;
    // log_p_GF[t_slice(t,I)] = mu_p_GF + mu_p_GF_i;
    // p_F[t_slice(t,I)] = exp(mu_p_F + mu_p_F_i);
    // p_G[t_slice(t,I)] = exp(mu_p_F + mu_p_F_i + mu_p_GF + mu_p_GF_i);
    // p_G[t_slice(t,I)] = p_F[t_slice(t,I)] .* p_GF[t_slice(t,I)];
    // p_G[t_slice(t,I)] = p_F[t_slice(t,I)] .* exp(mu_p_GF);    
    
    // random walk model on p_F time series
    // log_prior += normal_lpdf(log_p_F[t_slice(t,I)] | log_p_F[t_slice_lag(t, I)],exp(log_sigma_p_F));
    
    // ar(1) model on p_F time series
    // log_prior += normal_lpdf(log_p_F[t_slice(t,I)] | mu_p_F + ar_p_F*(log_p_F[t_slice_lag(t, I)]-mu_p_F),exp(log_sigma_p_F));    
  }
  
  // time-invariant model on p_F
  log_prior += normal_lpdf(log_p_F[t_slice(1,I)] | mu_p_F + mu_p_F_i, exp(log_sigma_p_F));
  // log_prior += student_t_lpdf(log_p_F[t_slice(1,I)] | exp(log_nu_p_F), mu_p_F + mu_p_F_i, exp(log_sigma_p_F));   
  // log_prior += normal_lpdf(log_p_GF[t_slice(1,I)] | mu_p_GF + mu_p_GF_i, exp(log_sigma_p_GF));
  log_prior += normal_lpdf(log_p_GF[t_slice(1,I)] | mu_p_GF, exp(log_sigma_p_GF));
  // log_p_F[t_slice(1,I)] = mu_p_F + mu_p_F_i;
  // log_p_GF[t_slice(1,I)] = mu_p_GF + mu_p_GF_i;
  // p_F[t_slice(1,I)] = exp(mu_p_F + mu_p_F_i);
  // p_G[t_slice(1,I)] = exp(mu_p_F + mu_p_F_i + mu_p_GF + mu_p_GF_i);
  // p_G[t_slice(1,I)] = p_F[t_slice(1,I)] .* p_GF[t_slice(1,I)];  
  // p_G[t_slice(1,I)] = p_F[t_slice(1,I)] .* exp(mu_p_GF); 
  
  for (n in 1 : n_F) {
    log_lik[n + T-1] = neg_binomial_2_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]],exp(-log_kappa_F));
    // log_lik[n + T-1] = neg_binomial_2_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]],
    //   exp(-log_kappa_F)*(disp_F*(N[ti_F[n]] .* p_F[ti_F[n]]) + (1-disp_F)));
    // log_lik[n + T-1] = neg_binomial_2_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]],
    //   exp(-log_kappa_F)/(disp_F/(N[ti_F[n]] .* p_F[ti_F[n]]) + (1-disp_F)));    
    // log_lik[n + T-1] = poisson_lpmf(y_F[n] | N[ti_F[n]] .* p_F[ti_F[n]]);
  }
  for (n in 1 : n_G) {
    log_lik[n + n_F + T-1] = neg_binomial_2_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]],exp(-log_kappa_G));
    // log_lik[n + n_F + T-1] = neg_binomial_2_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]],
    //   exp(-log_kappa_G)*(disp_G*(N[ti_G[n]] .* p_G[ti_G[n]]) + (1-disp_G)));    
    // log_lik[n + n_F + T-1] = neg_binomial_2_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]],
    //   exp(-log_kappa_G)/(disp_G/(N[ti_G[n]] .* p_G[ti_G[n]]) + (1-disp_G)));     
    // log_lik[n + n_F + T-1] = poisson_lpmf(y_G[n] | N[ti_G[n]] .* p_G[ti_G[n]]);
  }
  
  // observation ratio of Instagram to Facebook
  // FG_ratio = p_G[ti_FG] ./ p_F[ti_FG];
}
model {
  // likelihoods
  // y_F ~ neg_binomial_2(N[ti_F] .* p_F[ti_F],exp(-log_kappa_F));
  // y_G ~ neg_binomial_2(N[ti_G] .* p_G[ti_G],exp(-log_kappa_G));
  
  // y_N_tot[2:T] ~ lognormal(mu_tot + ar_tot*(log(y_N_tot[1:(T-1)]) - mu_tot),exp(log_sigma_tot));
  target += sum(log_lik);
  
  // priors: population growth rates
  // log_p_F[t_slice(1, I)] ~ normal(mu_p_F,exp(log_sigma_p_F));
  // log_p_F[t_slice(T, I)] ~ normal(mu_p_F,exp(log_sigma_p_F));
  // for(t in 2:(T-1)){
    // logit_p[t] ~ normal(logit_p[t-1],exp(log_sigma_p));
    // p_F[t_slice(t, I)] ~ lognormal(log(p_F[t_slice_lag(t, I)]) - exp(2*log_sigma_p_F)/2,exp(log_sigma_p_F));
    // log_p_F[t_slice(t, I)] ~ normal(mu_p_F + ar_p_F*(log_p_F[t_slice(t-1, I)]-mu_p_F),exp(log_sigma_p_F));
    // log_p_F[t_slice(t, I)] ~ normal(mu_p_F,exp(log_sigma_p_F));
  // }
  // p_F[t_slice(T, I)] ~ lognormal(log(p_F[t_slice_lag(T, I)]) - exp(2*log_sigma_p_F)/2,exp(log_sigma_p_F));
  // log_p_F[t_slice(T, I)] ~ normal(mu_p_F + ar_p_F*(log_p_F[t_slice(T-1, I)]-mu_p_F),exp(log_sigma_p_F));  
  target += log_prior;
  
  // priors: FB/Instagram user ratios
  // log_p_F ~ normal(mu_p_F,exp(log_sigma_p_F));
  // p_GF ~ lognormal(mu_p_GF,exp(log_sigma_p_GF));  
  // log_p_GF ~ normal(mu_p_GF,exp(log_sigma_p_GF));   
  // mu_p_GF ~ normal(0,5);
  mu_p_F_i ~ normal(0, exp(log_sigma_p_F_i));
  // mu_p_GF_i ~ normal(0, exp(log_sigma_p_GF_i));  
}
generated quantities {
  // in-sample posterior predictive check
  array[n_F] int<lower=0> F_hat;
  array[n_G] int<lower=0> G_hat;
  array[T] real<lower=0> y_N_tot_hat;
  
  F_hat = neg_binomial_2_rng(N[ti_F] .* p_F[ti_F],exp(-log_kappa_F));
  G_hat = neg_binomial_2_rng(N[ti_G] .* p_G[ti_G],exp(-log_kappa_G));
  // F_hat = neg_binomial_2_rng(N[ti_F] .* p_F[ti_F],
  //   exp(-log_kappa_F)*(disp_F*(N[ti_F] .* p_F[ti_F]) + (1-disp_F)));
  // G_hat = neg_binomial_2_rng(N[ti_G] .* p_G[ti_G],
  //   exp(-log_kappa_G)*(disp_G*(N[ti_G] .* p_G[ti_G]) + (1-disp_G)));
  // F_hat = neg_binomial_2_rng(N[ti_F] .* p_F[ti_F],
  //   exp(-log_kappa_F)/(disp_F/(N[ti_F] .* p_F[ti_F]) + (1-disp_F)));
  // G_hat = neg_binomial_2_rng(N[ti_G] .* p_G[ti_G],
  //   exp(-log_kappa_G)/(disp_G/(N[ti_G] .* p_G[ti_G]) + (1-disp_G)));  
  // F_hat = poisson_rng(N[ti_F] .* p_F[ti_F]);
  // G_hat = poisson_rng(N[ti_G] .* p_G[ti_G]);  
  
  y_N_tot_hat[1] = lognormal_rng((log(y_N_tot[2])-mu_tot)/ar_tot+mu_tot,exp(log_sigma_tot)/abs(ar_tot));
  y_N_tot_hat[2:T] = lognormal_rng(mu_tot + ar_tot*(log(y_N_tot[1:(T-1)]) - mu_tot),exp(log_sigma_tot));
}
