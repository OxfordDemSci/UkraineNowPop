init_generator <- function(md, chain_id = 1) {
  set.seed(2025 + chain_id)           

  T        <- md$T
  I        <- md$I
  A        <- md$A
  S        <- md$S
  A_adult  <- A - 1                                     
  K_r      <- md$K_r
  K_p      <- md$K_p

  C_adult  <- I * A_adult * S                           
  N_cells  <- T * C_adult                               

  draw_sigma <- function(lo = .02, hi = .10) runif(1, lo, hi)
  beta_r <- if (K_r) rep(0,  K_r) else numeric(0)
  beta_p <- if (K_p) rep(0,  K_p) else numeric(0)

  alpha_r <- 0
  alpha_p <- 0
  phi_p   <- 0

  sigma_F <- runif(1, .05, .20)
  sigma_G <- runif(1, .05, .20)
  sigma_delta_r   <- draw_sigma()
  sigma_gamma_r_i <- draw_sigma()
  sigma_gamma_r_a <- draw_sigma()
  sigma_gamma_r_s <- draw_sigma()

  delta_r_raw   <- rnorm(T)
  gamma_r_i_raw <- rnorm(I)
  gamma_r_a_raw <- rnorm(A_adult)
  gamma_r_s_raw <- rnorm(S)

  sigma_delta_p   <- draw_sigma()
  sigma_gamma_p_i <- draw_sigma()
  sigma_gamma_p_a <- draw_sigma()
  sigma_gamma_p_s <- draw_sigma()

  delta_p_raw   <- rnorm(T)
  gamma_p_i_raw <- rnorm(I)
  gamma_p_a_raw <- rnorm(A_adult)
  gamma_p_s_raw <- rnorm(S)

  sigma_phi_p_i <- draw_sigma()
  sigma_phi_p_a <- draw_sigma()
  sigma_phi_p_s <- draw_sigma()

  phi_p_i_raw <- rnorm(I)
  phi_p_a_raw <- rnorm(A_adult)
  phi_p_s_raw <- rnorm(S)

  nu_r_log <- 0
  sigma_r  <- runif(1, .01, .08)
  log_r    <- rnorm(N_cells, mean = nu_r_log, sd = sigma_r)

  list(
    beta_r = beta_r,
    beta_p = beta_p,
    alpha_r = alpha_r,
    alpha_p = alpha_p,
    phi_p   = phi_p,

    sigma_F = sigma_F,
    sigma_G = sigma_G,

    sigma_delta_r   = sigma_delta_r,
    sigma_gamma_r_i = sigma_gamma_r_i,
    sigma_gamma_r_a = sigma_gamma_r_a,
    sigma_gamma_r_s = sigma_gamma_r_s,
    delta_r_raw     = delta_r_raw,
    gamma_r_i_raw   = gamma_r_i_raw,
    gamma_r_a_raw   = gamma_r_a_raw,
    gamma_r_s_raw   = gamma_r_s_raw,

    sigma_delta_p   = sigma_delta_p,
    sigma_gamma_p_i = sigma_gamma_p_i,
    sigma_gamma_p_a = sigma_gamma_p_a,
    sigma_gamma_p_s = sigma_gamma_p_s,
    delta_p_raw     = delta_p_raw,
    gamma_p_i_raw   = gamma_p_i_raw,
    gamma_p_a_raw   = gamma_p_a_raw,
    gamma_p_s_raw   = gamma_p_s_raw,

    sigma_phi_p_i = sigma_phi_p_i,
    sigma_phi_p_a = sigma_phi_p_a,
    sigma_phi_p_s = sigma_phi_p_s,
    phi_p_i_raw   = phi_p_i_raw,
    phi_p_a_raw   = phi_p_a_raw,
    phi_p_s_raw   = phi_p_s_raw,

    log_r   = log_r,
    sigma_r = sigma_r,
    nu_r_log = nu_r_log  )}
