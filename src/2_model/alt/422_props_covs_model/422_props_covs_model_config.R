library(dplyr)
library(lubridate)
library(tidyr)

env <- new.env()
source(here::here(".env"), local = env)

dir.create(file.path(here::here(), "wd"), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(here::here(), "wd"))

repo_dir <- env$repo_dir
data_dir <- file.path(env$repo_dir, "data")
src_dir <- file.path(repo_dir, "src", "2_model")
in_dir <- env$in_dir
out_dir <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

country <- "UA"
model_name <- "422_props_covs_model"

idx = read.csv(file.path(out_dir, paste0(tolower(country), "_master_index", ".csv")))
idx_F = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_facebook_audience", ".csv")))
idx_G = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_instagram_audience", ".csv")))
covs = read.csv(file.path(out_dir, "covariates", "final", "ua_covariates_oblast.csv"))
codps = read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv"))
outside_border = read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))
last_date = "2024-05-14"
process_drop_locations = c()
observation_drop_locations = c(3782,3788,3791,3797)
process_cov_select = read.csv(file.path(in_dir, "ua_r_covariates_oblast_selection.csv"))
observation_cov_select = read.csv(file.path(in_dir, "ua_p_covariates_oblast_selection.csv"))


md <- list()

seed <- round(runif(1, 1, 1e6))
set.seed(seed)
md$seed <- seed
  
# ---- Location and Date Filtering ---- #
combined_drop_locations <- c(process_drop_locations, observation_drop_locations)
  
# Create new location indexes (i) based on unique i_key values (after dropping some locations).
i_idx <- idx %>% 
    select(i_key) %>% 
    filter(!i_key %in% process_drop_locations) %>% 
    distinct() %>% 
    arrange(i_key) %>% 
    mutate(i = row_number())
  
  # master index
  md$idx <- idx |>
  select(t, t_key, t_name, i_key, i_name, #a, a_key, a_name, 
         s, s_key, s_name) |>
  left_join(i_idx, by = "i_key") |>
  arrange(t, i, #a, 
                s) |>
  group_by(s) |>
  mutate( #tias = row_number(),
          ti = row_number()) |> ungroup() |>
  filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) |>
  select(ti, t, i, s, t_key, t_name, i_key, i_name, s, s_key, s_name)
 #select(tias, t, i, a, s, t_key, t_name, i_key, i_name, a_key, a_name, s, s_key, s_name)
  
  
# Facebook data.
md$idx_F <- idx_F |>
    select(t, i, #a, 
      s, m, value) |>
    left_join(idx |> select(i, i_key, #a, a_key, 
      s, s_key) |> distinct()) |>
    select(-c(i)) |>
    left_join(i_idx) |>
    right_join(md$idx |> select(t, i, #a, 
      s, ti)) |>
    filter(!i_key %in% combined_drop_locations,
            value > 0 & is.finite(value)) |>
    dplyr::select(ti, t, i, #a, 
      s, m, value)

md$idx_G <- idx_G |>
        select(t, i, #a, 
          s, m, value) |>
        left_join(idx |> select(i, i_key, #a, a_key, 
          s, s_key) |> distinct()) |>
        select(-c(i)) |>
        left_join(i_idx) |>
        right_join(md$idx |> select(t, i, #a, 
          s, ti)) |>
        filter(!i_key %in% combined_drop_locations,
                value > 0 & is.finite(value)) |>
        dplyr::select(ti, t, i, #a, 
          s, m, value)


md$S <- length(unique(md$idx$s))
md$I <- length(unique(md$idx$i))
md$T <- length(unique(md$idx$t))
  
# Facebook summary.
md$y_F <- md$idx_F$value
md$n_F <- length(md$y_F)
md$ti_F <- md$idx_F$ti
md$sex_F <- md$idx_F$s

# Instagram summary.
md$y_G <- md$idx_G$value
md$n_G <- length(md$y_G)
md$ti_G <- md$idx_G$ti
md$sex_G <- md$idx_G$s

N0t <- codps |>
    arrange(match(fb_key, i_idx$i_key)) |>
    select(F_TL, M_TL) |>
    as.matrix() %>% t()

md$N0 <- split(N0t, seq_len(nrow(N0t)))

# total population at each time step
weekly_avg <- outside_border |>
  mutate(week = floor_date(as.Date(date), "week", week_start = 1)) |>
  group_by(week) |>
  summarise(avg_value = mean(individuals, na.rm = TRUE)) |>
  filter(week >= min(md$idx$t_name) &
    week <= max(md$idx$t_name))

md$N01 <- codps |>
    arrange(match(fb_key, i_idx$i_key)) |>
    select(T_TL) |>
    pull()

md$y_N_tot <- as.integer(sum(md$N01) - weekly_avg$avg_value)
md$y_N_tot[1] <- sum(md$N01)

rm(weekly_avg)


# indexing: long format for N
md$ti_N0 <- unique(md$idx$ti[md$idx$t == 1])
md$ti_N <- unique(md$idx$ti[md$idx$t > 1])
md$ti_N_lag <- unique(md$idx$ti[md$idx$t > 1] - (md$I)  )

md$tt <- md$idx %>%
  distinct(t, i) %>%
  arrange(t, i) %>% 
  pull(t)              

md$ii <- md$idx %>%
  distinct(t, i) %>%
  arrange(t, i) %>% 
  pull(i)   

md$ss <- md$idx %>%
  pull(s)  

t <- unique(md$idx$t)
i <- unique(md$idx$i)
s <- unique(md$idx$s)



## Covariates ##
covs$time_std[is.na(covs$time_std)]  <- "none"
covs$space_std[is.na(covs$space_std)] <- "none"
  
# Covariates on growth rates.
rcov_select <- process_cov_select %>% filter(select == 1)
rcov_select$time_std[is.na(rcov_select$time_std)]  <- "none"
rcov_select$space_std[is.na(rcov_select$space_std)] <- "none"
  
X_r <- md$idx
  for (k in 1:nrow(rcov_select)) {
    col_name <- paste0("x", k)
    X_r <- X_r %>% 
      left_join(
        covs %>% 
          filter(
            covariate == rcov_select$covariate[k] &
              sum_stat  == rcov_select$sum_stat[k] &
              time_std  == rcov_select$time_std[k] &
              space_std == rcov_select$space_std[k]
          ) %>% 
          select(t, i, value_std) %>% 
          rename(!!col_name := value_std),
        by = c("t", "i")
      )
  }
X_r <- X_r %>% distinct(t, i, .keep_all = TRUE)
X_r_master <- as.matrix(X_r %>% select(starts_with("x"))) # dimensions: (T * I) x K_r
K_r <- ncol(X_r_master)
# Replicating the same matrix of variables for each sex.
X_r_array <- array(rep(X_r_master, times = md$S), 
                   dim = c(nrow(X_r_master), K_r, md$S))
md$X_r <- aperm(X_r_array, c(3, 1, 2))
md$K_r <- K_r
  

# Covariates on detection rates.
pcov_select <- observation_cov_select %>% filter(select == 1)
pcov_select$time_std[is.na(pcov_select$time_std)]  <- "none"
pcov_select$space_std[is.na(pcov_select$space_std)] <- "none"
  
 X_p <- md$idx
  for (k in 1:nrow(pcov_select)) {
    col_name <- paste0("x", k)
    X_p <- X_p %>% 
      left_join(
        covs %>% 
          filter(
            covariate == pcov_select$covariate[k] &
              sum_stat  == pcov_select$sum_stat[k] &
              time_std  == pcov_select$time_std[k] &
              space_std == pcov_select$space_std[k]
          ) %>% 
          select(t, i, value_std) %>% 
          rename(!!col_name := value_std),
        by = c("t", "i")
      )
  }
X_p <- X_p %>% distinct(t, i, .keep_all = TRUE)
X_p_master <- as.matrix(X_p %>% select(starts_with("x"))) # dimensions: (T * I) x K_p
K_p <- ncol(X_p_master)
# Replicating the same matrix of covariates for each sex. If different this needs to be modified
X_p_array <- array(rep(X_p_master, times = md$S), 
                   dim = c(nrow(X_p_master), K_p, md$S))
md$X_p <- aperm(X_p_array, c(3, 1, 2))
md$K_p <- K_p


#saveRDS(md, file.path(out_dir, "modelling", model_name, "mcmc", paste0("md_", model_name, ".rds")))




# Initial values
init_generator <- function(md, chain_id = 1) {
  result <- list()

  pi_init <- lapply(md$N0, function(x) x / sum(x))
  N <- array(NA, dim = c(md$S, md$T, md$I))

  for (s in 1:md$S) {
    for (t in 1:md$T) {
      N[s, t, ] <- rmultinom(1, size = md$y_N_tot[t], prob = pi_init[[s]]) + 10
    }
  }

  for (idx in seq_along(md$ti_F)) {
    s <- md$sex_F[idx]
    ti <- md$ti_F[idx]
    t_idx <- ceiling(ti / md$I)
    i_idx <- (ti - 1) %% md$I + 1
    if (md$y_F[idx] > N[s, t_idx, i_idx]) {
      N[s, t_idx, i_idx] <- md$y_F[idx] + 1
    }
  }

  for (idx in seq_along(md$ti_G)) {
    s <- md$sex_G[idx]
    ti <- md$ti_G[idx]
    t_idx <- ceiling(ti / md$I)
    i_idx <- (ti - 1) %% md$I + 1
    if (md$y_G[idx] > N[s, t_idx, i_idx]) {
      N[s, t_idx, i_idx] <- md$y_G[idx] + 1
    }
  }

  result[["N"]] <- as.vector(N)
  result[["r"]] <- rlnorm(md$S * md$T * md$I, meanlog = 0, sdlog = 0.005)

  result[["alpha_p"]] <- rnorm(1, 0.5, 0.1)
  result[["phi_p"]]   <- rnorm(1, 0.1, 0.05)
  result[["beta_p"]]  <- rnorm(md$K_p, 0, 0.1)

  result[["delta_p"]] <- array(rnorm(md$S * md$T, 0, 0.05), dim = c(md$S, md$T))
  result[["gamma_p"]] <- array(rnorm(md$S * md$I, 0, 0.05), dim = c(md$S, md$I))

  result[["log_sigma_delta_p"]] <- log(0.05)
  result[["log_sigma_gamma_p"]] <- log(0.05)
  result[["log_sigma_F"]]       <- log(0.1)
  result[["log_sigma_G"]]       <- log(0.1)
  
  result[["log_sigma_pi"]] <- log(0.1)
  result[["log_sigma_r"]] <- log(0.1)
  result[["alpha_r"]] <- rnorm(1, 0, 0.1)
  result[["beta_r"]]  <- rnorm(md$K_r, 0, 0.1)

  return(result)
}
