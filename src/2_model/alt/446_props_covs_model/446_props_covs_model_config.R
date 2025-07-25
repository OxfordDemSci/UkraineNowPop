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
model_name <- "446_props_covs_model"


idx = read.csv(file.path(out_dir, paste0(tolower(country), "_master_index", ".csv")))
idx_F = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_facebook_audience", ".csv")))
idx_G = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_instagram_audience", ".csv")))
covs = read.csv(file.path(out_dir, "covariates", "final", "ua_covariates_oblast.csv"))
codps = read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv"))
outside_border = read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))
last_date = "2022-12-31"
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

selected_locations <- c(1, 7, 16, 20, 22, 23, 25, 27)
selected_locations_key <- c(3778, 3784, 3794, 3798, 3800, 3801, 3803, 4290)

# Create new location indexes (i) based on unique i_key values (after dropping some locations).
i_idx <- idx %>% 
    filter(i %in% selected_locations) %>%
    dplyr::select(i_key) %>% 
    filter(!i_key %in% process_drop_locations) %>% 
    distinct() %>% 
    arrange(i_key) %>% 
    mutate(i = row_number())
  
  # master index
  md$idx <- idx |>
  filter(i %in% selected_locations) %>%
  dplyr::select(t, t_key, t_name, i_key, i_name, a, a_key, a_name, 
         s, s_key, s_name, tias) |>
  left_join(i_idx, by = "i_key") |>
  arrange(t, i, a, s) |>
  group_by(t) |>
  mutate(ias = row_number()) |> ungroup() |>
  arrange(t, i, a, s) %>%
  filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) |>
  select(ias, tias, t, i, a, s, t_key, t_name, i_key, i_name, a_key, a_name, s_key, s_name)
  
  
# Facebook data.
md$idx_F <- idx_F |>
    filter(i %in% selected_locations) %>%
    dplyr::select(t, i, a, s, m, value) |>
    left_join(idx |> select(i, i_key, a, a_key, s, s_key) |> distinct()) |>
    select(-c(i)) |>
    left_join(i_idx) |>
    right_join(md$idx |> select(t, i, a, s, ias)) |>
    filter(!i_key %in% combined_drop_locations,
            value > 0 & is.finite(value)) |>
    arrange(t, i, a, s) %>%
    dplyr::select(t, i, a, s, m, ias, value)

md$idx_G <- idx_G |>
  filter(i %in% selected_locations) %>%
  dplyr::select(t, i, a, s, m, value) |>
  left_join(idx |> select(i, i_key, a, a_key, s, s_key) |> distinct()) |>
  select(-c(i)) |>
  left_join(i_idx) |>
  right_join(md$idx |> select(t, i, a, s, ias)) |>
  filter(!i_key %in% combined_drop_locations,
                value > 0 & is.finite(value)) |>
  arrange(t, i, a, s) %>%
  dplyr::select(t, i, a, s, m, ias, value)


md$T <- length(unique(md$idx$t))
md$I <- length(unique(md$idx$i))
md$A <- length(unique(md$idx$a))
md$S <- length(unique(md$idx$s))


md$C <- length(unique(md$idx$ias))
  

md$y_F <- md$idx_F$value
md$n_F <- length(md$y_F)
md$t_F <- md$idx_F$t
md$ias_F <- md$idx_F$ias

md$y_G <- md$idx_G$value
md$n_G <- length(md$y_G)
md$t_G <- md$idx_G$t
md$ias_G <- md$idx_G$ias


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


group_map <- tibble::enframe(group_vars, name = "age_sex_group", value = "demog") %>%
  unnest(demog)

md$N01 <- codps %>%
  pivot_longer(
    cols      = 8:58,
    names_to  = "demog",
    values_to = "pop0") %>%
  left_join(group_map, by = "demog") %>%
  filter(!is.na(age_sex_group)) %>%
  separate(age_sex_group, into = c("sex", "raw_age"), sep = "_", extra = "merge") %>%
  mutate(age_group10y = case_when(
            raw_age == "60Plus" ~ "60-999",
            TRUE ~ stringr::str_replace(raw_age, "_", "-")),
    a = as.integer(factor(age_group10y,
          levels = c("20-29","30-39","40-49","50-59","60-999"))),
    s = if_else(sex=="F", 2L, 1L)) %>%
  rename(i_key = fb_key) %>%
  group_by(i_key, ADM1_PCODE, age_group10y, a, sex, s) %>%
  summarise(value = sum(pop0, na.rm = TRUE), .groups = "drop") %>%
  left_join(i_idx, by = c("i_key")) %>%
  filter(i_key %in% selected_locations_key) %>%
  arrange(i, a, s) %>%
  mutate(ias = row_number()) %>%
  select(ias, i, a, s, value) 

md$N0 <- md$N01$value

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

md$y_N_tot <- as.integer((sum(md$N01) - weekly_avg$avg_value)*0.33)
md$y_N_tot[1] <- sum(md$N01)*0.33

rm(weekly_avg)


md$tt <- md$idx %>%
  distinct(t, i, a, s) %>%
  arrange(t, i, a, s) %>% 
  pull(t)              

md$ii <- md$idx %>%
  distinct(t, i, a, s) %>%
  arrange(t, i, a, s) %>% 
  pull(i)   

md$aa <- md$idx  %>%
  distinct(t, i, a, s) %>%
  arrange(t, i, a, s) %>% 
  pull(a)   

md$ss <- md$idx %>%
  distinct(t, i, a, s) %>%
  arrange(t, i, a, s) %>% 
  pull(s)  

t <- unique(md$idx$t)
i <- unique(md$idx$i)
a <- unique(md$idx$a)
s <- unique(md$idx$s)


covs$time_std[is.na(covs$time_std)]  <- "none"
covs$space_std[is.na(covs$space_std)] <- "none"
  
# Covariates on growth rates.
rcov_select <- process_cov_select %>% filter(select == 1)
rcov_select$time_std[is.na(rcov_select$time_std)]  <- "none"
rcov_select$space_std[is.na(rcov_select$space_std)] <- "none"

cov_names <- rcov_select %>%
  transmute(nice_name = paste(covariate, sum_stat, time_std, space_std, sep = "_")) %>%
  pull(nice_name)

idx <- expand.grid(
  t = 1:md$T,
  i = 1:md$I,
  a = 1:md$A,
  s = 1:md$S) %>%
  arrange(t, i, a, s)

Xr_df <- idx
for (k in seq_along(cov_names)) {
  sel     <- rcov_select[k, ]
  col_real <- cov_names[k]        
  cov_k   <- covs %>%
    filter(
      covariate == sel$covariate,
      sum_stat  == sel$sum_stat,
      time_std  == sel$time_std,
      space_std == sel$space_std
    ) %>%
    select(t, i, value_std) %>%
    rename(!!col_real := value_std) 

  Xr_df <- left_join(Xr_df, cov_k, by = c("t", "i"))
}
Xr_df[is.na(Xr_df)] <- 0

md$X_r <- as.matrix(select(Xr_df, "war_fires_raw_none_none")) #all_of(cov_names)))
md$K_r <- ncol(md$X_r)




# Covariates on detection rates.
pcov_select <- observation_cov_select %>% filter(select == 1)
pcov_select$time_std[is.na(pcov_select$time_std)]  <- "none"
pcov_select$space_std[is.na(pcov_select$space_std)] <- "none"
  
cov_names <- pcov_select %>%
  transmute(nice_name = paste(covariate, sum_stat, time_std, space_std, sep = "_")) %>%
  pull(nice_name)

Xp_df <- idx
for (k in seq_along(cov_names)) {
  sel     <- pcov_select[k, ]
  col_real <- cov_names[k]        
  cov_k   <- covs %>%
    filter(
      covariate == sel$covariate,
      sum_stat  == sel$sum_stat,
      time_std  == sel$time_std,
      space_std == sel$space_std
    ) %>%
    select(t, i, value_std) %>%
    rename(!!col_real := value_std) 

  Xp_df <- left_join(Xp_df, cov_k, by = c("t", "i"))
}
Xp_df[is.na(Xp_df)] <- 0

md$X_p <- as.matrix(select(Xp_df, "occupied_raw_none_none")) #all_of(cov_names)))
md$K_p <- ncol(md$X_p)


#saveRDS(md, file.path(out_dir, "modelling", model_name, "mcmc", paste0("md_", model_name, ".rds")))



