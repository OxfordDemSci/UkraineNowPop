library(dplyr)
library(lubridate)

model_data <- function(
    idx, idx_F, idx_G,
    covs, codps, outside_border,
    codps_N1, confidence_N1, date_N1,
    last_date,
    process_drop_locations, observation_drop_locations,
    process_cov_select, observation_cov_select) {
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
  md$N0 <- codps |>
    arrange(match(fb_key, i_idx$i_key)) |>
    select(T_TL) |>
    pull()

  # interim codps
  md$N1 <- codps_N1 |>
    group_by(ADM1_PCODE) |>
    mutate(T_TL_ADM1 = sum(T_TL, na.rm = T)) |>
    left_join(codps |> select(ADM1_PCODE, fb_key)) |>
    select(fb_key, ADM1_PCODE, ADM1_NAME, T_TL_ADM1) |>
    rename(T_TL = T_TL_ADM1) |>
    distinct() |>
    ungroup() |>
    arrange(match(fb_key, i_idx$i_key)) |>
    select(T_TL) |>
    pull()

  md$ci_N1 <- confidence_N1

  # total population at each time step
  weekly_avg <- outside_border |>
    mutate(week = floor_date(as.Date(date), "week", week_start = 1)) |>
    group_by(week) |>
    summarise(avg_value = mean(individuals, na.rm = TRUE)) |>
    filter(week >= min(md$idx$t_name) &
      week <= max(md$idx$t_name))

  md$y_N_tot <- as.integer(sum(md$N0) - weekly_avg$avg_value)
  md$y_N_tot[1] <- sum(md$N0)

  rm(weekly_avg)


  # indexing: long format for N
  md$ti_N0 <- md$idx |>
    filter(t == 1) |>
    select(ti) |>
    pull()

  md$ti_N1 <- md$idx |>
    filter(t_name == as.character(floor_date(as.Date(date_N1), "week", week_start = 1))) |>
    select(ti) |>
    pull()

  md$ti_N <- md$idx$ti[md$idx$t > 1]
  md$ti_N_lag <- md$idx$ti[md$idx$t > 1] - md$I

  md$tt <- md$idx$t
  md$ii <- md$idx$i

  ## covariates ##
  covs$time_std[is.na(covs$time_std)] <- "none"
  covs$space_std[is.na(covs$space_std)] <- "none"

  # on growth rates
  rcov_select <- process_cov_select |> filter(select == 1)
  rcov_select$time_std[is.na(rcov_select$time_std)] <- "none"
  rcov_select$space_std[is.na(rcov_select$space_std)] <- "none"

  md$X_r <- md$idx
  for (k in 1:nrow(rcov_select)) {
    col_name <- paste0("x", k)
    md$X_r <- md$X_r |>
      left_join(
        covs |>
          filter(
            covariate == rcov_select$covariate[k] &
              sum_stat == rcov_select$sum_stat[k] &
              time_std == rcov_select$time_std[k] &
              space_std == rcov_select$space_std[k]
          ) |>
          select(t, i, value_std) |>
          rename(!!col_name := value_std)
      )
  }
  md$X_r <- md$X_r |>
    select(paste0("x", 1:k))
  md$K_r <- k

  # on Facebook and Instagram detection rates
  pcov_select <- observation_cov_select |> filter(select == 1)
  pcov_select$time_std[is.na(pcov_select$time_std)] <- "none"
  pcov_select$space_std[is.na(pcov_select$space_std)] <- "none"

  md$X_p <- md$idx
  for (k in 1:nrow(pcov_select)) {
    col_name <- paste0("x", k)
    md$X_p <- md$X_p |>
      left_join(
        covs |>
          filter(
            covariate == pcov_select$covariate[k] &
              sum_stat == pcov_select$sum_stat[k] &
              time_std == pcov_select$time_std[k] &
              space_std == pcov_select$space_std[k]
          ) |>
          select(t, i, value_std) |>
          rename(!!col_name := value_std)
      )
  }
  md$X_p <- md$X_p |>
    select(paste0("x", 1:k))
  md$K_p <- k

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

  result[["N"]] <- reshape2::melt(N, varnames = c("T", "I"))$value
  result[['log_sigma_pi']] <- rnorm(1,-3.05,0.05)
  
  result[['logit_pi']] <- 
    matrix(
      rnorm((md$T)*(md$I-1),
            rep(log((md$N0/sum(md$N0))[1:(md$I-1)] / 
                      (1-sum((md$N0/sum(md$N0))[1:(md$I-1)]))),times=md$T),
            0.25),
      nrow=md$T,ncol=md$I-1)  
  
  result[["p_F"]] <- rlnorm(md$T * md$I, log(mean(md$y_F, na.rm = T) / mean(md$N0)), 0.5)
  result[["p_G"]] <- rlnorm(md$T * md$I, log(mean(md$y_G, na.rm = T) / mean(md$N0)), 0.5)

  result[["alpha_p"]] <- runif(1, -4, -2)
  result[["phi_p"]] <- rnorm(1, 0, 0.5)
  result[["beta_p"]] <- runif(md$K_p, -1, -1)
  result[["delta_p"]] <- rnorm(md$T, -1, 1)
  result[["gamma_p"]] <- rnorm(md$I, -1, 1)
  
  result[["log_sigma_F"]] <- log(runif(1, 0, 0.2))
  result[["log_sigma_G"]] <- log(runif(1, 0, 0.2))
  result[["log_sigma_delta_p"]] <- log(runif(1, 0, 0.1))
  result[["log_sigma_gamma_p"]] <- log(runif(1, 0, 0.1))

  return(result)
}
