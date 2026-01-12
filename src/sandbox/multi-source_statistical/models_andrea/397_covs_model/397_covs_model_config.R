library(dplyr)
library(lubridate)
library(tidyr)

env <- new.env()

source(here::here(".env"), local = env)

wd_dir <- file.path(here::here(), "wd")
dir.create(wd_dir, showWarnings = FALSE, recursive = TRUE)
setwd(wd_dir)

repo_dir <- env$repo_dir
data_dir <- file.path(repo_dir, "data")
in_dir   <- env$in_dir
out_dir  <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

country    <- "UA"
model_name <- "397_covs_model"

idx = read.csv(file.path(out_dir, paste0(tolower(country), "_master_index", ".csv")))
idx_F = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_facebook_audience", ".csv")))
idx_G = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_instagram_audience", ".csv")))
covs = read.csv(file.path(out_dir, "covariates", "final", "ua_covariates_oblast.csv"))
codps = read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv"))
codps_N1 = read.csv(file.path(in_dir, "cod-ps_2023", "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv")) 
codps_N2 = read.csv(file.path(in_dir, "cod-ps_2024", "DO_NOT_SHARE_UKR_ADM2_POP_2024_Sept_27.csv"))
outside_border = read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))
last_date = "2024-05-14"
process_drop_locations = c()
observation_drop_locations = c(3782,3788,3791,3797) #3782, 3791, 3797, 3788
process_cov_select = read.csv(file.path(in_dir, "ua_r_covariates_oblast_selection.csv"))
observation_cov_select = read.csv(file.path(in_dir, "ua_p_covariates_oblast_selection.csv"))

md <- list()

seed <- round(runif(1, 1, 1e6))
set.seed(seed)
md$seed <- seed

combined_drop_locations <- c(process_drop_locations, observation_drop_locations)

selected_locations <- as.integer(setdiff(seq_len(27), c(11, 5, 13, 19)))

selected_locations_key <- as.integer(c(
  3800, 3801, 3781, 3804, 3802, 3803, 3783, 3790, 3787,
  3792, 3793, 3794, 3795, 3796, 3798, 3799, 3784, 3785,
  3786, 3778, 3780, 3779, 4290
))

selected_locations_pcode <- c(
  #"UA01", "UA14", "UA44", "UA85",
  "UA71", "UA74", "UA73", "UA12", "UA26", "UA63", "UA65", "UA68", "UA35",
  "UA32", "UA46", "UA48", "UA51", "UA53", "UA56", "UA59", "UA61", "UA05",
  "UA07", "UA21", "UA23", "UA18", "UA80"
)

i_idx <- idx %>%
  select(i_key) %>%
  distinct() %>%
  filter(i_key %in% selected_locations_key) %>%
  arrange(i_key) %>%
  mutate(i = row_number())

md$idx <- idx %>%
  filter(i %in% selected_locations, a != 7) %>%
  select(
    t, t_key, t_name, i_key, i_name,
    a, a_key, a_name, s, s_key, s_name
  ) %>%
  left_join(i_idx, by = "i_key") %>%
  arrange(t, i, a, s) %>%
  mutate(tias = row_number()) %>%
  group_by(a, s) %>%
  mutate(ti = row_number()) %>%
  ungroup() %>%
  group_by(t, i) %>%
  mutate(as = row_number()) %>%
  ungroup() %>%
  filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) %>%
  select(
    ti, as, tias, t, i, a, s,
    t_key, t_name, i_key, i_name,
    a_key, a_name, s_key, s_name
  )

md$A <- 7
md$S <- 2
md$I <- 23
md$T <- 117
md$C_full  <- md$I * md$A * md$S
md$C_adult <- md$I * (md$A - 1) * md$S
md$C_young <- md$I * md$S

adult_map <- expand.grid(
  t = seq_len(md$T),
  i = seq_len(md$I),
  a = 1:(md$A - 1),
  s = seq_len(md$S)
) %>%
  arrange(t, i, a, s) %>%
  mutate(tias_adult = row_number()) %>%
  select(t, i, a, s, tias_adult)

idx_keys <- idx %>%
  select(i, i_key, a, a_key, s, s_key) %>%
  distinct()

md$idx_F <- idx_F %>%
  filter(i %in% selected_locations, a < 6) %>%
  select(t, i, a, s, m, value) %>%
  left_join(idx_keys, by = c("i", "a", "s")) %>%
  select(-i) %>%
  left_join(i_idx, by = "i_key") %>%
  right_join(md$idx %>% select(t, i, a, s, ti, as, tias), by = c("t", "i", "a", "s")) %>%
  left_join(adult_map, by = c("t", "i", "a", "s")) %>%
  filter(!i_key %in% combined_drop_locations, value > 0, is.finite(value)) %>%
  group_by(ti, as, tias, tias_adult, t, i, a, s) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  select(ti, as, tias_full = tias, tias_adult, t, i, a, s, value) %>%
  arrange(t, i, a, s)

md$idx_G <- idx_G %>%
  filter(i %in% selected_locations, a < 6) %>%
  select(t, i, a, s, m, value) %>%
  left_join(idx_keys, by = c("i", "a", "s")) %>%
  select(-i) %>%
  left_join(i_idx, by = "i_key") %>%
  right_join(md$idx %>% select(t, i, a, s, ti, as, tias), by = c("t", "i", "a", "s")) %>%
  left_join(adult_map, by = c("t", "i", "a", "s")) %>%
  filter(!i_key %in% combined_drop_locations, value > 0, is.finite(value)) %>%
  group_by(ti, as, tias, tias_adult, t, i, a, s) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  select(ti, as, tias_full = tias, tias_adult, t, i, a, s, value) %>%
  arrange(t, i, a, s)

md$y_F    <- md$idx_F$value
md$n_F    <- length(md$y_F)
md$tias_F <- md$idx_F$tias_adult

md$y_G    <- md$idx_G$value
md$n_G    <- length(md$y_G)
md$tias_G <- md$idx_G$tias_adult

cols_to_pivot <- c(
  "F_00_04", "F_05_09", "F_10_14", "F_15_19", "F_20_24", "F_25_29", "F_30_34", "F_35_39",
  "F_40_44", "F_45_49", "F_50_54", "F_55_59", "F_60_64", "F_65_69", "F_70_74", "F_75_79",
  "F_80Plus",
  "M_00_04", "M_05_09", "M_10_14", "M_15_19", "M_20_24", "M_25_29", "M_30_34", "M_35_39",
  "M_40_44", "M_45_49", "M_50_54", "M_55_59", "M_60_64", "M_65_69", "M_70_74", "M_75_79",
  "M_80Plus"
)

date_start <- "2022-02-25"
date_end   <- last_date

time_index_expanded <- tibble(
  collection_date = seq(as.Date(date_start), as.Date(date_end), by = 1)
) |>
  mutate(
    t_name = floor_date(as.Date(collection_date), "week", week_start = 1),
    t_key  = stringr::str_replace_all(as.character(t_name), "-", "") |> as.integer(),
    t      = t_name |> as.character() |> haven::as_factor() |> as.integer()
  )

time_index <- time_index_expanded |>
  distinct(t_name, t_key, t) |>
  arrange(t)

###############
# COD-PS 2022
###############
md$N010 <- codps %>%
  select(ADM1_PCODE, fb_key, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop0"
  ) |>
  separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") |>
  filter(sex != "T", age_min != "TL") |>
  rename(pcode = ADM1_PCODE, i_key = fb_key) |>
  filter(i_key %in% selected_locations_key) %>%
  mutate(
    age_group10y = case_when(
      age_min == "00" & age_max == "04" ~ "00-14",
      age_min == "05" & age_max == "09" ~ "00-14",
      age_min == "10" & age_max == "14" ~ "00-14",
      age_min == "15" & age_max == "19" ~ "15-19",
      age_min == "20" & age_max == "24" ~ "20-29",
      age_min == "25" & age_max == "29" ~ "20-29",
      age_min == "30" & age_max == "34" ~ "30-39",
      age_min == "35" & age_max == "39" ~ "30-39",
      age_min == "40" & age_max == "44" ~ "40-49",
      age_min == "45" & age_max == "49" ~ "40-49",
      age_min == "50" & age_max == "54" ~ "50-59",
      age_min == "55" & age_max == "59" ~ "50-59",
      age_min == "60" & age_max == "64" ~ "60-999",
      age_min == "65" & age_max == "69" ~ "60-999",
      age_min == "70" & age_max == "74" ~ "60-999",
      age_min == "75" & age_max == "79" ~ "60-999",
      age_min == "80Plus"               ~ "60-999"
    )
  ) |>
  group_by(i_key, pcode, age_group10y, sex) |>
  summarise(value = sum(pop0), .groups = "drop") |>
  mutate(
    a = case_when(
      age_group10y == "00-14"  ~ 7L,
      age_group10y == "15-19"  ~ 6L,
      age_group10y == "20-29"  ~ 1L,
      age_group10y == "30-39"  ~ 2L,
      age_group10y == "40-49"  ~ 3L,
      age_group10y == "50-59"  ~ 4L,
      age_group10y == "60-999" ~ 5L
    ),
    s = if_else(sex == "F", 2L, 1L)
  ) |>
  left_join(i_idx, by = "i_key") |>
  arrange(i, a, s) |>
  mutate(ias = row_number()) %>%
  group_by(i, s) %>%
  mutate(is = row_number()) %>%
  ungroup() %>%
  filter(!is.na(a)) %>%
  select(ias, is, i, a, s, value)

md$N0      <- md$N010$value
md$ias_N0  <- md$N010$ias

md$Y0 <- md$N010 |>
  arrange(i, a, s) |>
  filter(a == 7) %>%
  pull(value)

md$A0 <- md$N010 |>
  arrange(i, a, s) |>
  filter(a != 7) %>%
  pull(value)

md$N01 <- codps |>
  filter(fb_key %in% selected_locations_key) %>%
  arrange(match(fb_key, i_idx$i_key)) |>
  pull(T_TL)

full_pop22 <- codps %>%
  select(fb_key, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = c("sex", "age_min", "age_max"),
    names_sep = "_",
    values_to = "pop0"
  ) %>%
  filter(sex != "T", age_min != "TL")

total_pop_all22 <- sum(full_pop22$pop0, na.rm = TRUE)
total_pop_sel22 <- full_pop22 %>%
  filter(fb_key %in% selected_locations_key) %>%
  summarise(sum_sel = sum(pop0, na.rm = TRUE)) %>%
  pull(sum_sel)

prop_sel22 <- total_pop_sel22 / total_pop_all22
prop_sel22

##############
# COD-PS 2023
##############
md$N110 <- codps_N1 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop1"
  ) |>
  group_by(ADM1_PCODE, demog) |>
  summarise(pop1 = sum(pop1, na.rm = TRUE), .groups = "drop") |>
  left_join(codps[, c("ADM1_PCODE", "fb_key")], by = "ADM1_PCODE") %>%
  separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") |>
  filter(sex != "T", age_min != "TL") |>
  rename(pcode = ADM1_PCODE, i_key = fb_key) |>
  filter(i_key %in% selected_locations_key) %>%
  mutate(
    age_group10y = case_when(
      age_min == "00" & age_max == "04" ~ "00-14",
      age_min == "05" & age_max == "09" ~ "00-14",
      age_min == "10" & age_max == "14" ~ "00-14",
      age_min == "15" & age_max == "19" ~ "15-19",
      age_min == "20" & age_max == "24" ~ "20-29",
      age_min == "25" & age_max == "29" ~ "20-29",
      age_min == "30" & age_max == "34" ~ "30-39",
      age_min == "35" & age_max == "39" ~ "30-39",
      age_min == "40" & age_max == "44" ~ "40-49",
      age_min == "45" & age_max == "49" ~ "40-49",
      age_min == "50" & age_max == "54" ~ "50-59",
      age_min == "55" & age_max == "59" ~ "50-59",
      age_min == "60" & age_max == "64" ~ "60-999",
      age_min == "65" & age_max == "69" ~ "60-999",
      age_min == "70" & age_max == "74" ~ "60-999",
      age_min == "75" & age_max == "79" ~ "60-999",
      age_min == "80Plus"               ~ "60-999"
    )
  ) %>%
  group_by(i_key, pcode, age_group10y, sex) |>
  summarise(value = sum(pop1), .groups = "drop") |>
  mutate(
    a = case_when(
      age_group10y == "00-14"  ~ 7L,
      age_group10y == "15-19"  ~ 6L,
      age_group10y == "20-29"  ~ 1L,
      age_group10y == "30-39"  ~ 2L,
      age_group10y == "40-49"  ~ 3L,
      age_group10y == "50-59"  ~ 4L,
      age_group10y == "60-999" ~ 5L
    ),
    s = if_else(sex == "F", 2L, 1L)
  ) |>
  left_join(i_idx, by = "i_key") |>
  arrange(i, a, s) |>
  mutate(ias = row_number()) %>%
  group_by(i, s) %>%
  mutate(is = row_number()) %>%
  ungroup() %>%
  filter(!is.na(a)) %>%
  select(ias, is, i, a, s, value)

md$N1     <- md$N110$value
md$ias_N1 <- md$N110$ias

md$Y1 <- md$N110 |>
  arrange(i, a, s) |>
  filter(a == 7) %>%
  pull(value)

md$A1 <- md$N110 |>
  arrange(i, a, s) |>
  filter(a != 7) %>%
  pull(value)

md$N11 <- codps_N1 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop1"
  ) |>
  group_by(ADM1_PCODE) |>
  summarise(pop1 = sum(pop1, na.rm = TRUE), .groups = "drop") |>
  left_join(codps[, c("ADM1_PCODE", "fb_key")], by = "ADM1_PCODE") %>%
  filter(fb_key %in% selected_locations_key) %>%
  arrange(match(fb_key, i_idx$i_key)) %>%
  pull(pop1)

pop1_date <- as.Date("2023-07-01")
pop1_week <- floor_date(pop1_date, "week", week_start = 1)
t_pop1 <- time_index %>%
  filter(t_name == pop1_week) %>%
  pull(t)
t_pop1

full_pop23 <- codps_N1 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop1"
  ) %>%
  group_by(ADM1_PCODE, demog) %>%
  summarise(pop1 = sum(pop1, na.rm = TRUE), .groups = "drop") %>%
  separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") %>%
  filter(sex != "T", age_min != "TL")

total_pop_all23 <- sum(full_pop23$pop1, na.rm = TRUE)

full_pop23_sel <- full_pop23 %>%
  left_join(codps %>% select(ADM1_PCODE, fb_key), by = "ADM1_PCODE") %>%
  filter(fb_key %in% selected_locations_key)

total_pop_sel23 <- sum(full_pop23_sel$pop1, na.rm = TRUE)
prop_sel23 <- total_pop_sel23 / total_pop_all23
prop_sel23

################
# COD-PS 2024
################
md$N210 <- codps_N2 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop2"
  ) %>%
  group_by(ADM1_PCODE, demog) %>%
  summarise(pop2 = sum(pop2, na.rm = TRUE), .groups = "drop") %>%
  left_join(codps[, c("ADM1_PCODE", "fb_key")], by = "ADM1_PCODE") %>%
  separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") %>%
  filter(sex != "T", age_min != "TL") %>%
  rename(pcode = ADM1_PCODE, i_key = fb_key) %>%
  filter(i_key %in% selected_locations_key) %>%
  mutate(
    age_group10y = case_when(
      age_min == "00" & age_max == "04" ~ "00-14",
      age_min == "05" & age_max == "09" ~ "00-14",
      age_min == "10" & age_max == "14" ~ "00-14",
      age_min == "15" & age_max == "19" ~ "15-19",
      age_min == "20" & age_max == "24" ~ "20-29",
      age_min == "25" & age_max == "29" ~ "20-29",
      age_min == "30" & age_max == "34" ~ "30-39",
      age_min == "35" & age_max == "39" ~ "30-39",
      age_min == "40" & age_max == "44" ~ "40-49",
      age_min == "45" & age_max == "49" ~ "40-49",
      age_min == "50" & age_max == "54" ~ "50-59",
      age_min == "55" & age_max == "59" ~ "50-59",
      age_min == "60" & age_max == "64" ~ "60-999",
      age_min == "65" & age_max == "69" ~ "60-999",
      age_min == "70" & age_max == "74" ~ "60-999",
      age_min == "75" & age_max == "79" ~ "60-999",
      age_min == "80Plus"               ~ "60-999"
    )
  ) %>%
  group_by(i_key, pcode, age_group10y, sex) %>%
  summarise(value = sum(pop2), .groups = "drop") %>%
  mutate(
    a = case_when(
      age_group10y == "00-14"  ~ 7L,
      age_group10y == "15-19"  ~ 6L,
      age_group10y == "20-29"  ~ 1L,
      age_group10y == "30-39"  ~ 2L,
      age_group10y == "40-49"  ~ 3L,
      age_group10y == "50-59"  ~ 4L,
      age_group10y == "60-999" ~ 5L
    ),
    s = if_else(sex == "F", 2L, 1L)
  ) %>%
  left_join(i_idx, by = "i_key") %>%
  arrange(i, a, s) %>%
  mutate(ias = row_number()) %>%
  group_by(i, s) %>%
  mutate(is = row_number()) %>%
  ungroup() %>%
  filter(!is.na(a)) %>%
  select(ias, is, i, a, s, value)

md$N2     <- md$N210$value
md$ias_N2 <- md$N210$ias

md$Y2 <- md$N210 |>
  arrange(i, a, s) |>
  filter(a == 7) %>%
  pull(value)

md$A2 <- md$N210 |>
  arrange(i, a, s) |>
  filter(a != 7) %>%
  pull(value)

md$N21 <- codps_N2 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop2"
  ) |>
  group_by(ADM1_PCODE) |>
  summarise(pop2 = sum(pop2, na.rm = TRUE), .groups = "drop") |>
  left_join(codps[, c("ADM1_PCODE", "fb_key")], by = "ADM1_PCODE") %>%
  arrange(match(fb_key, i_idx$i_key)) %>%
  pull(pop2)

pop2_date <- as.Date("2024-05-14")
pop2_week <- floor_date(pop2_date, "week", week_start = 1)
t_pop2 <- time_index %>%
  filter(t_name == pop2_week) %>%
  pull(t)
t_pop2

full_pop24 <- codps_N2 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop2"
  ) %>%
  group_by(ADM1_PCODE, demog) %>%
  summarise(pop2 = sum(pop2, na.rm = TRUE), .groups = "drop") %>%
  separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") %>%
  filter(sex != "T", age_min != "TL")

total_pop_all24 <- sum(full_pop24$pop2, na.rm = TRUE)

full_pop24_sel <- full_pop24 %>%
  left_join(codps %>% select(ADM1_PCODE, fb_key), by = "ADM1_PCODE") %>%
  filter(fb_key %in% selected_locations_key)

total_pop_sel24 <- sum(full_pop24_sel$pop2, na.rm = TRUE)
prop_sel24 <- total_pop_sel24 / total_pop_all24
prop_sel24

######################################
# Total population at every time step
######################################
weekly_avg <- outside_border |>
  mutate(week = floor_date(as.Date(date), "week", week_start = 1)) |>
  group_by(week) |>
  summarise(avg_value = mean(individuals, na.rm = TRUE), .groups = "drop") |>
  filter(
    week >= min(md$idx$t_name),
    week <= max(md$idx$t_name)
  )

border_df <- time_index %>%
  select(t, t_name) %>%
  left_join(weekly_avg %>% rename(t_name = week), by = "t_name") %>%
  mutate(avg_value = if_else(t == 1, 0, avg_value))

md$y_N_tot <- border_df %>%
  mutate(
    baseline = case_when(
      t <  t_pop1 ~ sum(md$N01),
      t == t_pop1 ~ sum(md$N11),
      t <  t_pop2 ~ sum(md$N11),
      t == t_pop2 ~ sum(md$N21),
      TRUE        ~ sum(md$N21)
    ),
    prop = case_when(
      t <= t_pop1 ~ prop_sel22,
      t_pop1 < t & t <= t_pop2 ~ prop_sel23,
      TRUE ~ prop_sel24
    ),
    ytmp    = baseline - (avg_value * prop),
    y_N_tot = as.integer(ytmp)
  ) %>%
  pull(y_N_tot)

rm(weekly_avg)

children <- md$N010 %>%
  filter(a == 7) %>%
  mutate(t = 1) %>%
  rename(children = value) %>%
  rbind(
    md$N110 %>% filter(a == 7) %>% mutate(t = t_pop1) %>% rename(children = value)
  ) %>%
  rbind(
    md$N210 %>% filter(a == 7) %>% mutate(t = t_pop2) %>% rename(children = value)
  )

female_repo <- md$N010 %>%
  filter(a >= 1 & a <= 3, s == 2) %>%
  group_by(i) %>%
  summarise(female_repo = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(t = 1) %>%
  rbind(
    md$N110 %>%
      filter(a >= 1 & a <= 3, s == 2) %>%
      group_by(i) %>%
      summarise(female_repo = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(t = t_pop1)
  ) %>%
  rbind(
    md$N210 %>%
      filter(a >= 1 & a <= 3, s == 2) %>%
      group_by(i) %>%
      summarise(female_repo = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(t = t_pop2)
  )

md$ratio_children_female <- expand.grid(
  t = seq_len(md$T),
  i = seq_len(md$I),
  s = seq_len(md$S)
) %>%
  left_join(children, by = c("t", "i")) %>%
  left_join(female_repo, by = c("t", "i")) %>%
  mutate(ratio_children_female = children / female_repo) %>%
  group_by(i, s) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(ratio_children_female_im = zoo::na.approx(ratio_children_female, x = t, na.rm = FALSE)) %>%
  ungroup() %>%
  arrange(t, i, s) %>%
  pull(ratio_children_female_im)

table(!is.na(md$ratio_children_female))

t <- unique(md$idx$t)
i <- unique(md$idx$i)
a <- seq_len(md$A - 1)
s <- unique(md$idx$s)

I <- md$I
A <- md$A
S <- md$S
T <- md$T

C_full  <- I * A * S
C_adult <- I * (A - 1) * S
C_young <- I * S
IS <- I * S

adult_grid <- expand.grid(t = 1:T, i = 1:I, a = 1:(A - 1), s = 1:S) |>
  dplyr::arrange(t, i, a, s)

md$tt <- adult_grid$t
md$ii <- adult_grid$i
md$aa <- adult_grid$a
md$ss <- adult_grid$s

md$adult_grid <- adult_grid[order(adult_grid$t, adult_grid$i, adult_grid$a, adult_grid$s), ]
md$female_core_mask <- as.integer(md$adult_grid$a %in% 1:7 & md$adult_grid$s == 2)
md$adult_grid$idx_tis <- (md$adult_grid$t - 1) * I * S + (md$adult_grid$i - 1) * S + md$adult_grid$s
md$tis_index <- md$adult_grid$idx_tis

full_grid <- expand.grid(t = 1:T, i = 1:I, a = 1:A, s = 1:S) %>%
  arrange(t, i, a, s) %>%
  mutate(idx_full = row_number())

md$young_pos_full <- full_grid %>%
  filter(t == 1, a == 7) %>%
  arrange(i, a, s) %>%
  pull(idx_full) %>%
  as.integer()

md$young_idx <- full_grid %>%
  filter(a == 7) %>%
  arrange(t, i, a, s) %>%
  pull(idx_full) %>%
  as.integer()

md$adult_pos_full <- full_grid %>%
  filter(t == 1, a != 7) %>%
  arrange(i, a, s) %>%
  pull(idx_full) %>%
  as.integer()

md$adult_idx <- full_grid %>%
  filter(a != 7) %>%
  arrange(t, i, a, s) %>%
  pull(idx_full) %>%
  as.integer()

md$N_y <- T * length(md$young_pos_full)
md$N_a <- T * length(md$adult_pos_full)

md$slice_full       <- matrix(seq_len(T * C_full),  nrow = T, byrow = TRUE)
md$slice_adult      <- matrix(seq_len(T * C_adult), nrow = T, byrow = TRUE)
md$slice_adult_lag  <- rbind(rep(1L, C_adult), md$slice_adult[-T, ])

md$slice_young      <- matrix(seq_len(T * C_young), nrow = T, byrow = TRUE)
md$slice_young_lag  <- rbind(rep(1L, C_young), md$slice_young[-T, ])

f_core <- (md$ss == 2L) & (md$aa %in% 1:3)
md$idx_female_a234 <- which(f_core)
md$idx_female_triple <- matrix(md$idx_female_a234, ncol = 3, byrow = TRUE)
md$N_fem_a <- length(md$idx_female_a234)

idx_a1519 <- adult_map %>% filter(a == 6) %>% arrange(t, i, s) %>% pull(tias_adult)
idx_a2029 <- adult_map %>% filter(a == 1) %>% arrange(t, i, s) %>% pull(tias_adult)

md$idx_a1519 <- as.integer(idx_a1519)
md$idx_a2029 <- as.integer(idx_a2029)
md$N_tie <- length(idx_a1519)
length(idx_a2029)

standardise_cols <- function(df, vars) {
  stats <- vector("list", length(vars))
  names(stats) <- vars

  for (v in vars) {
    x <- df[[v]]
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)

    if (!is.finite(s) || s <= 0) {
      df[[v]] <- 0
      stats[[v]] <- c(mean = m, sd = 1)
    } else {
      df[[v]] <- (x - m) / s
      stats[[v]] <- c(mean = m, sd = s)
    }
  }

  list(df = df, stats = stats)
}

covs$time_std[is.na(covs$time_std)]   <- "none"
covs$space_std[is.na(covs$space_std)] <- "none"

rcov_select <- process_cov_select %>%
  filter(select == 1) %>%
  mutate(across(c(time_std, space_std), ~ replace(., is.na(.), "none")))

cov_names_r <- rcov_select %>%
  transmute(nice_name = paste(covariate, sum_stat, time_std, space_std, sep = "_")) %>%
  pull()

idx <- expand.grid(
  t = 1:md$T,
  i = 1:md$I,
  a = min(md$aa):max(md$aa),
  s = 1:md$S
) %>%
  arrange(t, i, a, s)

Xr_df <- idx
for (k in seq_along(cov_names_r)) {
  sel      <- rcov_select[k, ]
  col_real <- cov_names_r[k]

  cov_k <- covs %>%
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

std_r <- standardise_cols(Xr_df, cov_names_r)
Xr_df <- std_r$df
md$X_r_means <- sapply(std_r$stats, `[[`, "mean")
md$X_r_sds   <- sapply(std_r$stats, `[[`, "sd")

md$X_r <- as.matrix(Xr_df["acled_withfatalities_raw_none_none"])
md$K_r <- ncol(md$X_r)

pcov_select <- observation_cov_select %>%
  filter(select == 1) %>%
  mutate(across(c(time_std, space_std), ~ replace(., is.na(.), "none")))

cov_names_p <- pcov_select %>%
  transmute(nice_name = paste(covariate, sum_stat, time_std, space_std, sep = "_")) %>%
  pull()

Xp_df <- idx
for (k in seq_along(cov_names_p)) {
  sel      <- pcov_select[k, ]
  col_real <- cov_names_p[k]

  cov_k <- covs %>%
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

std_p <- standardise_cols(Xp_df, cov_names_p)
Xp_df <- std_p$df
md$X_p_means <- sapply(std_p$stats, `[[`, "mean")
md$X_p_sds   <- sapply(std_p$stats, `[[`, "sd")

md$X_p <- as.matrix(Xp_df["pwtt_sum_24week_none_none"])
md$K_p <- ncol(md$X_p)

saveRDS(md, file.path(out_dir, "modelling", model_name, "mcmc", paste0("md_", model_name, ".rds")))
