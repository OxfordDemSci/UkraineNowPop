init_generator <- function(md, chain_id = 1) {
  set.seed(2025 + chain_id)

  T   <- md$T
  I   <- md$I
  A   <- md$A
  S   <- md$S
  K_r <- md$K_r
  K_p <- md$K_p

  if (is.null(md$C_adult)) {
    stop("md$C_adult is NULL; please pass C_adult in md (= I*(A-1)*S).")
  }
  C_adult <- md$C_adult
  N_cells <- T * C_adult  

  A_free <- max(A - 2, 0) 
  Tm1    <- max(T - 1, 0) 

  if (!is.null(md$tt) && length(md$tt) != N_cells) {
    stop(sprintf("length(md$tt)=%d but T*C_adult=%d (T=%d, C_adult=%d).",
                 length(md$tt), N_cells, T, C_adult))
  }
  if (!is.null(md$X_r) && nrow(md$X_r) != N_cells) {
    stop(sprintf("nrow(md$X_r)=%d but T*C_adult=%d (T=%d, C_adult=%d).",
                 nrow(md$X_r), N_cells, T, C_adult))
  }
  if (!is.null(md$X_p) && nrow(md$X_p) != N_cells) {
    stop(sprintf("nrow(md$X_p)=%d but T*C_adult=%d (T=%d, C_adult=%d).",
                 nrow(md$X_p), N_cells, T, C_adult))
  }

  draw_sigma <- function(lo, hi) runif(1, lo, hi)

  beta_r <- if (K_r > 0) rep(0, K_r) else numeric(0)
  beta_p <- if (K_p > 0) rep(0, K_p) else numeric(0)

  alpha_r <- 0
  alpha_p <- 0
  phi_p   <- 0

  sigma_F <- runif(1, 0.05, 0.20)
  sigma_G <- runif(1, 0.05, 0.20)

  sigma_r <- runif(1, 0.005, 0.03)

  sigma_delta_r   <- draw_sigma(0.005, 0.05)
  sigma_gamma_r_i <- draw_sigma(0.005, 0.05)
  sigma_gamma_r_a <- draw_sigma(0.005, 0.05)
  sigma_gamma_r_s <- draw_sigma(0.005, 0.05)

  sigma_delta_p   <- draw_sigma(0.005, 0.05)
  sigma_gamma_p_i <- draw_sigma(0.005, 0.05)
  sigma_gamma_p_a <- draw_sigma(0.005, 0.05)
  sigma_gamma_p_s <- draw_sigma(0.005, 0.05)

  sigma_phi_p_a <- draw_sigma(0.005, 0.05)

  delta_r_innov_raw <- if (Tm1 > 0) rnorm(Tm1) else numeric(0)
  delta_p_innov_raw <- if (Tm1 > 0) rnorm(Tm1) else numeric(0)

  gamma_r_i_raw <- if (I > 0) rnorm(I) else numeric(0)
  gamma_p_i_raw <- if (I > 0) rnorm(I) else numeric(0)

  gamma_r_s_raw <- if (S > 0) rnorm(S) else numeric(0)
  gamma_p_s_raw <- if (S > 0) rnorm(S) else numeric(0)

  gamma_r_a_raw <- if (A_free > 0) rnorm(A_free) else numeric(0)
  gamma_p_a_raw <- if (A_free > 0) rnorm(A_free) else numeric(0)

  phi_p_a_raw   <- if (A_free > 0) rnorm(A_free) else numeric(0)

  log_r_raw <- rnorm(N_cells, mean = 0, sd = 1)
  log_r_raw <- pmin(pmax(log_r_raw, -3), 3)  

  list(
    log_r_raw = log_r_raw,
    sigma_r   = sigma_r,
    alpha_r   = alpha_r,
    beta_r    = beta_r,

    sigma_delta_r   = sigma_delta_r,
    sigma_gamma_r_i = sigma_gamma_r_i,
    sigma_gamma_r_a = sigma_gamma_r_a,
    sigma_gamma_r_s = sigma_gamma_r_s,

    delta_r_innov_raw = delta_r_innov_raw,
    gamma_r_i_raw     = gamma_r_i_raw,
    gamma_r_a_raw     = gamma_r_a_raw,
    gamma_r_s_raw     = gamma_r_s_raw,

    alpha_p = alpha_p,
    beta_p  = beta_p,
    phi_p   = phi_p,

    sigma_delta_p   = sigma_delta_p,
    sigma_gamma_p_i = sigma_gamma_p_i,
    sigma_gamma_p_a = sigma_gamma_p_a,
    sigma_gamma_p_s = sigma_gamma_p_s,

    delta_p_innov_raw = delta_p_innov_raw,
    gamma_p_i_raw     = gamma_p_i_raw,
    gamma_p_a_raw     = gamma_p_a_raw,
    gamma_p_s_raw     = gamma_p_s_raw,

    sigma_F = sigma_F,
    sigma_G = sigma_G,

    sigma_phi_p_a = sigma_phi_p_a,
    phi_p_a_raw   = phi_p_a_raw
  )
}
