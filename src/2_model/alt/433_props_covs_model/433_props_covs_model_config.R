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
model_name <- "433_props_covs_model"

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
  select(t, t_key, t_name, i_key, i_name, a, a_key, a_name, 
         s, s_key, s_name) |>
  left_join(i_idx, by = "i_key") |>
  arrange(t, i, a, s) |>
  group_by(a, s) |>
  mutate( #tias = row_number(),
          ti = row_number()) |> ungroup() |>
  group_by(t, i) |>
  mutate(as = row_number()) |> ungroup() |>
  filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) |>
  select(ti, as, t, i, a, s, t_key, t_name, i_key, i_name, a_key, a_name, s_key, s_name)
  
  
# Facebook data.
md$idx_F <- idx_F |>
    select(t, i, a, s, m, value) |>
    left_join(idx |> select(i, i_key, a, a_key, s, s_key) |> distinct()) |>
    select(-c(i)) |>
    left_join(i_idx) |>
    right_join(md$idx |> select(t, i, a, s, ti, as)) |>
    filter(!i_key %in% combined_drop_locations,
            value > 0 & is.finite(value)) |>
    dplyr::select(ti, as, t, i, a, s, m, value)

md$idx_G <- idx_G |>
        select(t, i, a, s, m, value) |>
        left_join(idx |> select(i, i_key, a, a_key, s, s_key) |> distinct()) |>
        select(-c(i)) |>
        left_join(i_idx) |>
        right_join(md$idx |> select(t, i, a, s, ti, as)) |>
        filter(!i_key %in% combined_drop_locations,
                value > 0 & is.finite(value)) |>
        dplyr::select(ti, as, t, i, a, s, m, value)


md$A <- length(unique(md$idx$a))
md$S <- length(unique(md$idx$s))
md$I <- length(unique(md$idx$i))
md$T <- length(unique(md$idx$t))
md$C <- length(unique(md$idx$as))
  
# Facebook summary.
md$y_F <- md$idx_F$value
md$n_F <- length(md$y_F)
md$ti_F <- md$idx_F$ti
md$comb_F <- md$idx_F$as

# Instagram summary.
md$y_G <- md$idx_G$value
md$n_G <- length(md$y_G)
md$ti_G <- md$idx_G$ti
md$comb_G <- md$idx_G$as


group_vars <- list(
  F_20_29  = c("F_20_24", "F_25_29"),
  F_30_39  = c("F_30_34", "F_35_39"),
  F_40_49  = c("F_40_44", "F_45_49"),
  F_50_59  = c("F_50_54", "F_55_59"),
  F_60Plus = c("F_60_64", "F_65_69", "F_70_74", "F_75_79", "F_80Plus"),

  M_20_29  = c("M_20_24", "M_25_29"),
  M_30_39  = c("M_30_34", "M_35_39"),
  M_40_49  = c("M_40_44", "M_45_49"),
  M_50_59  = c("M_50_54", "M_55_59"),
  M_60Plus = c("M_60_64", "M_65_69", "M_70_74", "M_75_79", "M_80Plus")
)

codps_ordered <- codps |>
  arrange(match(fb_key, i_idx$i_key))

N0_df <- sapply(group_vars, function(cols) {
  rowSums(codps_ordered[, cols, drop = FALSE], na.rm = TRUE)
})

N0t <- t(N0_df)
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
  distinct(a, s) %>%
  arrange(a, s) %>% 
  pull(s)   

md$aa <- md$idx  %>%
  distinct(a, s) %>%
  arrange(a, s) %>% 
  pull(a)   

t <- unique(md$idx$t)
i <- unique(md$idx$i)
s <- unique(md$idx$s)
a <- unique(md$idx$a)



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
# Replicating the same matrix of variables for each sex and age group.
X_r_array <- array(rep(X_r_master, times = md$C), 
                   dim = c(nrow(X_r_master), K_r, md$C))
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
X_p_array <- array(rep(X_p_master, times = md$C), 
                   dim = c(nrow(X_p_master), K_p, md$C))
md$X_p <- aperm(X_p_array, c(3, 1, 2))
md$K_p <- K_p


#saveRDS(md, file.path(out_dir, "modelling", model_name, "mcmc", paste0("md_", model_name, ".rds")))




