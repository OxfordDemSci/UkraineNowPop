library("tidyverse")
library("cmdstanr")
library("posterior")
library("bayesplot")
color_scheme_set("viridis")

dir= c("~/DemSci/projects/2023_WHO_Ukraine_Population/tmp")
dir.create(dir, showWarnings = F, recursive = T)


load("UkraineNowPop_dataset.RData")


N_lat <- SMdata2_10ya %>%
  dplyr::select(combination_id, pcode_num, age_group0_num, gender_num, week_num) %>%
  dplyr::distinct(age_group0_num, gender_num, pcode_num, week_num, .keep_all = TRUE) %>%
  arrange(week_num, combination_id) %>%
  mutate(N_idx = row_number()) 


F_obs <- SMdata2_10ya %>%
  dplyr::select(Nc_idx, combination_id, 
                pcode_num, age_group0_num, gender_num, week_num, meas_num, 
                y_F) %>%
  filter(!is.na(y_F)) %>%
  arrange(week_num, combination_id, meas_num) %>%
  mutate(Fobs_idx = row_number()) 


G_obs <- SMdata2_10ya %>%
  dplyr::select(Nc_idx, combination_id, 
                pcode_num, age_group0_num, gender_num, week_num, meas_num,  
                y_G) %>%
  filter(!is.na(y_G)) %>%
  arrange(week_num, combination_id, meas_num) %>%
  mutate(Gobs_idx = row_number())  


GF_ratio_obs <- SMdata2_10ya %>%
  dplyr::select(Nc_idx, combination_id, 
                pcode_num, age_group0_num, gender_num, week_num, 
                y_FG_ratio, y_F, y_G) %>%
  filter(!is.na(y_FG_ratio), !is.na(y_F), !is.na(y_G)) %>%
  arrange(week_num, combination_id) %>%
  mutate(GF_ratio_obs_idx = row_number()) 


stan_input <- list(
  N_combinations = max(N_lat$combination_id),  # Total combinations
  N_time = max(N_lat$week_num),
  #N_obs = max(N_lat$N_idx),
  
  F_combination_id = F_obs$combination_id,
  F_time = F_obs$week_num,
  N_obs_F  = max(F_obs$Fobs_idx),
  y_F = F_obs$y_F,
  
  G_combination_id = G_obs$combination_id,
  G_time = G_obs$week_num,
  N_obs_G  = max(G_obs$Gobs_idx),
  y_G = G_obs$y_G,
  
  GFr_combination_id = GF_ratio_obs$combination_id,
  GFr_time = GF_ratio_obs$week_num,
  N_obs_GFr  = max(GF_ratio_obs$GF_ratio_obs_idx),
  y_FG_ratio = GF_ratio_obs$y_FG_ratio,
  
  N0 = cod_data_10ya$pop0,
  
  y_N_tot = 43320154 + refugees_tot10yage$ref_inc - refugees_tot10yage$ref_outc
)



mod <- cmdstan_model(file.path(dir, "SRF_Data analysis/results/nov/results_20241110/stan_model.stan"))

fit <- mod$sample(
  data = stan_input,
  seed = 123,
  chains = 3, 
  parallel_chains = 3,
  iter_warmup = 5000, 
  iter_sampling = 5000,
  thin = 100)


est_N <- fit$summary(
  variables = c("N"),
  posterior::default_summary_measures(),
  extra_quantiles = ~posterior::quantile2(., probs = c(.0275, .975))
)
