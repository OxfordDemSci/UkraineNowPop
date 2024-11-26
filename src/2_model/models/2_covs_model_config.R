prepare_md <- function(
  idx, idx_F, idx_G, 
  covs, codps, outside_border, 
  last_date,
  process_drop_locations, observation_drop_locations,
  process_covariates, process_scale_factors,
  observation_covariates, observation_scale_factors
){
  
  # model data
  md <- list()

  # set seed for random number generators
  seed <- round(runif(1, 1, 1e6))
  set.seed(seed)

  #---- location and date filtering ----#

  observation_drop_locations <- c(process_drop_locations, observation_drop_locations)

  # new i indexes after dropping locations
  i_idx <- idx |>
    select(i_key) |>
    filter(!i_key %in% process_drop_locations) |>
    distinct() |>
    arrange(i_key) |>
    mutate(i = row_number())

  #---- indexes ----#

  # master index
  md$idx <- idx |>
    select(t, t_key, t_name, i_key, i_name) |>
    # drop locations and update i
    right_join(i_idx, by = "i_key") |>
    # create ti index
    arrange(t, i) |>
    mutate(ti = row_number()) |>
    # discard dates later than last_date
    filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) |>
    # arrange columns
    select(ti, t, i, t_key, t_name, i_key, i_name)

  # Facebook
  md$idx_F <- idx_F |>
    select(t, i, m, value) |>
    # update i index to account for locations dropped from process model
    left_join(idx |> select(i, i_key) |> distinct()) |>
    select(-i) |>
    left_join(i_idx) |>
    # drop locations not in process model
    right_join(md$idx |> select(t, i, ti)) |>
    # drop locations excluded from observation model
    filter(!i_key %in% observation_drop_locations) |>
    # drop data with zeros and NAs
    filter(value > 0 & is.finite(value)) |>
    # arrange columns
    select(ti, t, i, m, value)

  # Instagram
  md$idx_G <- idx_G |>
    select(t, i, m, value) |>
    # update i index to account for locations dropped from process model
    left_join(idx |> select(i, i_key) |> distinct()) |>
    select(-i) |>
    left_join(i_idx) |>
    # drop locations not in process model
    right_join(md$idx |> select(t, i, ti)) |>
    # drop locations excluded from observation model
    filter(!i_key %in% observation_drop_locations) |>
    # drop data with zeros and NAs
    filter(value > 0 & is.finite(value)) |>
    # arrange columns
    select(ti, t, i, m, value)

  #---- prepare data ----#

  md$I <- length(unique(md$idx$i))
  md$T <- length(unique(md$idx$t))

  # Facebook
  md$y_F <- md$idx_F$value
  md$n_F <- length(md$y_F)
  md$ti_F <- md$idx_F$ti

  # Instagram
  md$y_G <- md$idx_G$value
  md$n_G <- length(md$y_G)
  md$ti_G <- md$idx_G$ti

  # Facebook:Instagram ratio
  md$ti_FG <- unique(md$ti_F[which(md$ti_F %in% md$ti_G)])
  md$n_FG <- length(md$ti_FG)

  md$y_FG_ratio <- c()
  for (i in 1:length(md$ti_FG)) {
    ti <- md$ti_FG[i]
    G_ti <- mean(md$y_G[which(md$ti_G == ti)])
    F_ti <- mean(md$y_F[which(md$ti_F == ti)])
    md$y_FG_ratio[i] <- G_ti / F_ti
  }

  drop <- which(!is.finite(md$y_FG_ratio))
  if (length(drop) > 0) {
    md$y_FG_ratio <- md$y_FG_ratio[-drop]
    md$ti_FG <- md$ti_FG[-drop]
    md$n_FG <- md$n_FG - length(drop)
  }

  ## population ##

  # baseline population
  md$N0 <- codps[as.character(unique(md$idx$i_key[order(md$idx$i)])), "T_TL"]

  # total population at each time step
  weekly_avg <- outside_border |>
    mutate(week = floor_date(as.Date(date), "week", week_start = 1)) |>
    group_by(week) |>
    summarise(avg_value = mean(individuals, na.rm = TRUE)) |>
    filter(week >= min(md$idx$t_name) &
      week <= max(md$idx$t_name))

  md$y_N_tot <- as.integer(sum(md$N0) - weekly_avg$avg_value)

  rm(weekly_avg)


  # indexing: long format for N
  md$ti_N0 <- md$idx$ti[md$idx$t == 1]
  md$ti_N <- md$idx$ti[md$idx$t > 1]
  md$ti_N_lag <- md$idx$ti[md$idx$t > 1] - md$I

  md$tt <- md$idx$t
  md$ii <- md$idx$i

  ## covariates ##

  # on growth rates
  md$X_r <- md$idx
  k <- 0
  for (covariate_name in process_covariates) {
    for (scale_factor in process_scale_factors) {
      k <- k + 1
      col_name <- paste0("x", k)

      md$X_r <- md$X_r |>
        left_join(
          covs |>
            filter(covariate == covariate_name) |>
            select(i, t, all_of(scale_factor)) |>
            rename(!!col_name := all_of(scale_factor)) |>
            mutate(!!col_name := coalesce(!!sym(col_name), min(!!sym(col_name), na.rm = T))),
          by = c("i", "t")
        )
    }
  }
  md$X_r <- md$X_r |>
    select(paste0("x", 1:k))
  md$K_r <- k

  # on Facebook and Instagram detection rates
  md$X_p_F <- md$idx
  k <- 0
  for (covariate_name in observation_covariates) {
    for (scale_factor in observation_scale_factors) {
      k <- k + 1
      col_name <- paste0("x", k)

      md$X_p_F <- md$X_p_F |>
        left_join(
          covs |>
            filter(covariate == covariate_name) |>
            select(i, t, all_of(scale_factor)) |>
            rename(!!col_name := all_of(scale_factor)) |>
            mutate(!!col_name := coalesce(!!sym(col_name), min(!!sym(col_name), na.rm = T))),
          by = c("i", "t")
        )
    }
  }
  md$X_p_G <- md$X_p_F <- md$X_p_F |>
    select(paste0("x", 1:k))
  md$K_p_F <- md$K_p_G <- k

  return(md)
}



#---- initial values ----#
init_generator <- function(md = md, chain_id = 1) {
  result <- list()

  N <- matrix(NA, nrow = md$T, ncol = md$I)
  theta <- md$N0 / sum(md$N0)

  N <- t(rmultinom(n = md$T, size = md$y_N_tot, prob = md$N0 / sum(md$N0)))
  for (t in 1:md$T) {
    for (i in 1:md$I) {
      ti <- which(md$tt == t & md$ii == i)
      if (any(md$y_F[md$ti_F == ti] > N[t, i])) {
        N[t, i] <- max(md$y_F[md$ti_F == ti])
      }
    }
  }

  result[["N_tot"]] <- md$y_N_tot
  result[["N"]] <- reshape2::melt(N, varnames = c("T", "I"))$value
  result[["r"]] <- rlnorm(md$T * md$I, 0, 0.1 / 2)
  result[["mu_r"]] <- rnorm(md$T * md$I, 0, 0.1)
  result[["alpha_r"]] <- rnorm(md$I, 0, 3)
  result[["mu_alpha_r"]] <- rnorm(1, 0, 3)
  result[["sigma_alpha_r"]] <- runif(1, 0, 0.2)
  result[["beta_r"]] <- rnorm(md$K_r, 0, 1)
  result[["sigma_r"]] <- runif(1, 0, 0.5)

  result[["p_F"]] <- rlnorm(md$T * md$I, log(mean(md$y_F, na.rm = T) / mean(md$N0)), 0.5)
  result[["mu_p_F"]] <- runif(md$T * md$I, -4, -2)
  result[["alpha_p_F"]] <- runif(md$T, -4, -2)
  result[["mu_alpha_p_F"]] <- runif(1, -4, -2)
  result[["sigma_alpha_p_F"]] <- runif(1, 0, 0.2)
  result[["beta_p_F"]] <- rnorm(md$K_p_F, 0, 1)
  result[["sigma_p_F"]] <- runif(1, 0, 0.2)

  result[["p_G"]] <- rlnorm(md$T * md$I, log(mean(md$y_G, na.rm = T) / mean(md$N0)), 0.5)
  result[["mu_p_G"]] <- runif(md$T * md$I, -4, -2)
  result[["alpha_p_G"]] <- runif(md$I, -4, -2)
  result[["mu_alpha_p_G"]] <- runif(1, -4, -2)
  result[["sigma_alpha_p_G"]] <- runif(1, 0, 0.2)
  result[["beta_p_G"]] <- rnorm(md$K_p_G, 0, 1)
  result[["sigma_p_G"]] <- runif(1, 0, 0.2)

  return(result)
}
