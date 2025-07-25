library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)


env <- new.env()
#source("/data/home/andrea/git/OxfordDemSci/UkraineNowPop/.env", local = env)
#dir.create(file.path("/data/home/andrea/git/OxfordDemSci"), showWarnings = FALSE, recursive = TRUE)
#setwd(file.path("/data/home/andrea/git/OxfordDemSci"))


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
model_name <- "351_covs_model"

idx = read.csv(file.path(out_dir, paste0(tolower(country), "_master_index", ".csv")))
idx_F = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_facebook_audience", ".csv")))
idx_G = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_instagram_audience", ".csv")))
covs = read.csv(file.path(out_dir, "covariates", "final", "ua_covariates_oblast.csv"))
codps22 = read.csv(file.path(data_dir, "COD-PS", "2022", "population_baseline22.csv"))
codps23 = read.csv(file.path(data_dir, "COD-PS", "2023", "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv")) 
codps24 = readxl::read_excel(file.path(data_dir, "COD-PS", "2024", "[restricted release] UKR_ADM2_POP_2024_Sept_27.xlsx"), sheet = 2)
outside_border = read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))
#last_date = "2022-12-31"
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

selected_locations <- #seq(1, 27, by = 1#
                      c(1, #2
                        #4, #6
                        7, #6
                        16, #4
                        20, #5
                        22, #2
                        23, #3
                        #25, #6
                        27  #1
                        ) %>% as.integer()
selected_locations_key <- 
  #c(3788, 3800, 3801, 3781, 3782, 3804, 3802, 3803, 3783, 3790, 3787, 3791, 3792, 3793, 3794, 3795, 3796, 3798,
  #3799, 3784, 3785, 3786, 3778, 3780, 3779, 4290, 3797)%>% as.integer()
  c(3778, #3781, 
    3784, 
  3794, 3798, 3800, 3801, #3803, 
  4290) %>% as.integer()

# Create new location indexes (i) based on unique i_key values (after dropping some locations).
i_idx <- idx %>%
  dplyr::select(i_key) %>%
  distinct() %>%
  filter(i_key %in% selected_locations_key) %>%
  arrange(i_key) %>%
  mutate(i = row_number())

md$idx <- idx %>%
  filter(i %in% selected_locations) %>%
  select(t, t_key, t_name, i_key, i_name,
         a, a_key, a_name, s, s_key, s_name, e, e_name) %>%
  left_join(i_idx, by = "i_key") %>%
  arrange(t, i, a, s) %>%
  mutate(tias = row_number()) %>%              
  group_by(a, s) %>%
  mutate(ti = row_number()) %>% ungroup() %>%
  group_by(t, i) %>%
  mutate(as = row_number()) %>% ungroup() %>%
  filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) %>%
  select(ti, as, tias, t, i, a, s,
         t_key, t_name, i_key, i_name,
         a_key, a_name, s_key, s_name, e, e_name)

adult_map <- md$idx %>%
  filter(a != 1) %>%     
  arrange(t, i, a, s) %>%
  mutate(tias_adult = row_number()) %>%    
  select(t, i, a, s, tias_adult)

md$idx_F <- idx_F %>%
  filter(i %in% selected_locations) %>%
  filter(a != 1) %>%                       
  select(t, i, a, s, m, value) %>%
  left_join(idx %>% select(i, i_key, a, a_key, s, s_key) %>% distinct(),
            by = c("i","a","s")) %>%
  select(-i) %>%
  left_join(i_idx, by = "i_key") %>%
  right_join(md$idx %>% select(t, i, a, s, ti, as, tias),
             by = c("t","i","a","s")) %>%
  left_join(adult_map) %>%  
  filter(!i_key %in% combined_drop_locations,
         value > 0, is.finite(value)) %>%
  select(ti, as, tias_full = tias, tias_adult, t, i, a, s, m, value) %>%
  arrange(t, i, a, s)

md$idx_G <- idx_G %>%
  filter(i %in% selected_locations) %>%
  filter(a != 1) %>%
  select(t, i, a, s, m, value) %>%
  left_join(idx %>% select(i, i_key, a, a_key, s, s_key) %>% distinct(),
            by = c("i","a","s")) %>%
  select(-i) %>%
  left_join(i_idx, by = "i_key") %>%
  right_join(md$idx %>% select(t, i, a, s, ti, as, tias),
             by = c("t","i","a","s")) %>%
  left_join(adult_map, by = c("t","i","a","s")) %>%
  filter(!i_key %in% combined_drop_locations,
         value > 0, is.finite(value)) %>%
  select(ti, as, tias_full = tias, tias_adult, t, i, a, s, m, value) %>%
  arrange(t, i, a, s)


md$y_F    <- md$idx_F$value
md$n_F    <- length(md$y_F)
md$tias_F <- md$idx_F$tias_adult  

md$y_G    <- md$idx_G$value
md$n_G    <- length(md$y_G)
md$tias_G <- md$idx_G$tias_adult


md$A <- length(unique(md$idx$a))
md$S <- length(unique(md$idx$s))
md$I <- length(unique(md$idx$i))
md$T <- length(unique(md$idx$t))
md$C_full <- max(md$idx$s)*max(md$idx$a)*max(md$idx$i)
md$C_adult <- max(md$idx$s)*(max(md$idx$a)-1)*max(md$idx$i)
md$E <- length(unique(md$idx$e))


cols_to_pivot <- c("F_00_04","F_05_09","F_10_14","F_15_19","F_20_24","F_25_29","F_30_34","F_35_39","F_40_44","F_45_49","F_50_54","F_55_59","F_60_64","F_65_69","F_70_74","F_75_79","F_80Plus",
                   "M_00_04","M_05_09","M_10_14","M_15_19","M_20_24","M_25_29","M_30_34","M_35_39","M_40_44","M_45_49","M_50_54","M_55_59","M_60_64","M_65_69","M_70_74","M_75_79","M_80Plus")

young_cols <- c(
      "F_00_04","F_05_09","F_10_14","F_15_19",
      "M_00_04","M_05_09","M_10_14","M_15_19"
    )

adult_cols <- c(
    "F_20_24","F_25_29","F_30_34","F_35_39","F_40_44","F_45_49")



date_start <- "2022-02-25"
date_end   <- "2024-05-14"
                   
time_index_expanded <- tibble(
    collection_date = seq(as.Date(date_start), as.Date(date_end), by = 1)) |>
    mutate(t_name = floor_date(as.Date(collection_date), "week", week_start = 1),
           t_key  = stringr::str_replace_all(as.character(t_name), "-", "") |> as.integer(),
           t = t_name |> as.character() |> haven::as_factor() |> as.integer())
                  
time_index <- time_index_expanded |>
                    distinct(t_name, t_key, t) |>
                    arrange(t)
###############
# COD-PS 2022
###############
md$N010 <- codps22 %>%
  select(ADM1_PCODE, fb_key, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop0"
  )|>
separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") |>
filter(sex!="T", age_min!="TL") |>
rename(pcode=ADM1_PCODE, i_key=fb_key) |>
filter(i_key %in% selected_locations_key) %>%
mutate(age_group10y = case_when(
                  age_min=="00" & age_max=="04" ~ "00-19",
                  age_min=="05" & age_max=="09" ~ "00-19",
                  age_min=="10" & age_max=="14" ~ "00-19",
                  age_min=="15" & age_max=="19" ~ "00-19",
                  age_min=="20" & age_max=="24" ~ "20-29",
                  age_min=="25" & age_max=="29" ~ "20-29",
                  age_min=="30" & age_max=="34" ~ "30-39",
                  age_min=="35" & age_max=="39" ~ "30-39",
                  age_min=="40" & age_max=="44" ~ "40-49",
                  age_min=="45" & age_max=="49" ~ "40-49",
                  age_min=="50" & age_max=="54" ~ "50-59",
                  age_min=="55" & age_max=="59" ~ "50-59",
                  age_min=="60" & age_max=="64" ~ "60-999",
                  age_min=="65" & age_max=="69" ~ "60-999",
                  age_min=="70" & age_max=="74" ~ "60-999",
                  age_min=="75" & age_max=="79" ~ "60-999",
                  age_min=="80Plus" ~ "60-999" )) |>
group_by(i_key, pcode, age_group10y, sex) |>
summarise(value =sum(pop0)) |> ungroup() |>
mutate(a = case_when(age_group10y=="00-19"~1, age_group10y=="20-29"~2, age_group10y=="30-39"~3, age_group10y=="40-49"~4, age_group10y=="50-59"~5, age_group10y=="60-999"~6) %>% as.integer(),
       s = ifelse(sex=="F", 2, 1) %>% as.integer()) |>
left_join(i_idx) |>
arrange(i, a, s) |>
mutate(ias = row_number()) %>%
group_by(i, s) %>%
mutate(is = row_number()) %>% ungroup() %>%
filter(!is.na(a)) %>%
select(ias, is, i, a, s, value)

md$N0 <- md$N010$value
md$ias_N0 <- md$N010$ias

md$Y0 <- md$N010 |>
  arrange(i, a, s) |>
  filter(a=='1') %>%
    select(value) |>
  pull()

md$A0 <- md$N010 |>
  arrange(i, a, s) |>
  filter(a!=1) %>%
    select(value) |>
  pull()

md$N01 <- codps22 |>
    arrange(match(fb_key, i_idx$i_key)) |>
    select(T_TL) |>
    pull()

full_pop22 <- codps22 %>%
  select(fb_key, all_of(cols_to_pivot)) %>%
  pivot_longer(all_of(cols_to_pivot),
               names_to = c("sex","age_min","age_max"),
               names_sep = "_",
               values_to = "pop0") %>%
  filter(sex != "T", age_min != "TL")
total_pop_all22 <- sum(full_pop22$pop0, na.rm=TRUE)
total_pop_sel22 <- full_pop22 %>%
  filter(fb_key %in% selected_locations_key) %>%
  summarize(sum_sel = sum(pop0, na.rm=TRUE)) %>%
  pull(sum_sel)
prop_sel22 <- total_pop_sel22 / total_pop_all22

##############
# COD-PS 2023
##############
md$N110 <- codps23 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop1"
  )|>
group_by(ADM1_PCODE, demog) %>%
summarise(pop1 = sum(pop1, na.rm=T)) %>% ungroup() %>%
left_join(codps22[,c("ADM1_PCODE", "fb_key")]) %>%
separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") |>
filter(sex!="T", age_min!="TL") |>
rename(pcode=ADM1_PCODE, i_key=fb_key) |>
filter(i_key %in% selected_locations_key) %>%
mutate(age_group10y = case_when(
                  age_min=="00" & age_max=="04" ~ "00-19",
                  age_min=="05" & age_max=="09" ~ "00-19",
                  age_min=="10" & age_max=="14" ~ "00-19",
                  age_min=="15" & age_max=="19" ~ "00-19",
                  age_min=="20" & age_max=="24" ~ "20-29",
                  age_min=="25" & age_max=="29" ~ "20-29",
                  age_min=="30" & age_max=="34" ~ "30-39",
                  age_min=="35" & age_max=="39" ~ "30-39",
                  age_min=="40" & age_max=="44" ~ "40-49",
                  age_min=="45" & age_max=="49" ~ "40-49",
                  age_min=="50" & age_max=="54" ~ "50-59",
                  age_min=="55" & age_max=="59" ~ "50-59",
                  age_min=="60" & age_max=="64" ~ "60-999",
                  age_min=="65" & age_max=="69" ~ "60-999",
                  age_min=="70" & age_max=="74" ~ "60-999",
                  age_min=="75" & age_max=="79" ~ "60-999",
                  age_min=="80Plus" ~ "60-999" )) |>
group_by(i_key, pcode, age_group10y, sex) |>
summarise(value =sum(pop1)) |> ungroup() |>
mutate(a = case_when(age_group10y=="00-19"~1, age_group10y=="20-29"~2, age_group10y=="30-39"~3, age_group10y=="40-49"~4, age_group10y=="50-59"~5, age_group10y=="60-999"~6) %>% as.integer(),
       s = ifelse(sex=="F", 2, 1) %>% as.integer()) |>
left_join(i_idx) |>
arrange(i, a, s) |>
mutate(ias = row_number()) %>%
group_by(i, s) %>%
mutate(is = row_number()) %>% ungroup() %>%
filter(!is.na(a)) %>%
select(ias, is, i, a, s, value)

md$N1 <- md$N110$value
md$ias_N1 <- md$N110$ias

md$Y1 <- md$N110 |>
  arrange(i, a, s) |>
  filter(a=='1') %>%
    select(value) |>
  pull()

md$A1 <- md$N110 |>
  arrange(i, a, s) |>
  filter(a!=1) %>%
    select(value) |>
  pull()


md$N11 <- codps23 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop1"
  )|>
  group_by(ADM1_PCODE) %>%
  summarise(pop1 = sum(pop1, na.rm=T)) %>% ungroup() %>%
  left_join(codps22[,c("ADM1_PCODE", "fb_key")]) %>%
  arrange(match(fb_key, i_idx$i_key)) %>%
  pull(pop1)


pop1_date <- as.Date("2023-09-01")
pop1_week <- floor_date(pop1_date, "week", week_start = 1)
t_pop1 <- time_index %>%
  filter(t_name == pop1_week) %>%
  pull(t)


full_pop23 <- codps23 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols       = all_of(cols_to_pivot),
    names_to   = "demog",
    values_to  = "pop1"
  ) %>%
  group_by(ADM1_PCODE, demog) %>%
  summarise(pop1 = sum(pop1, na.rm = TRUE), .groups = "drop") %>% ungroup() %>%
  separate(demog, into = c("sex","age_min","age_max"), sep = "_") %>%
  filter(sex != "T", age_min != "TL")

total_pop_all23 <- sum(full_pop23$pop1, na.rm = TRUE)

full_pop23_sel <- full_pop23 %>%
  left_join(
    codps22 %>% select(ADM1_PCODE, fb_key),
    by = "ADM1_PCODE"
  ) %>%
  filter(fb_key %in% selected_locations_key)

total_pop_sel23 <- sum(full_pop23_sel$pop1, na.rm = TRUE)
prop_sel23 <- total_pop_sel23 / total_pop_all23

################
# COD-PS 2024
################
md$N210 <- codps24 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop2"
  ) %>%
  group_by(ADM1_PCODE, demog) %>%
  summarise(pop2 = sum(pop2, na.rm = TRUE)) %>% ungroup() %>%
  left_join(codps22[,c("ADM1_PCODE", "fb_key")]) %>%
  separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") %>%
  filter(sex != "T", age_min != "TL") %>%
  rename(pcode = ADM1_PCODE, i_key = fb_key) %>%
  filter(i_key %in% selected_locations_key) %>%
  mutate(
    age_group10y = case_when(
      age_min=="00" & age_max=="04" ~ "00-19",
                  age_min=="05" & age_max=="09" ~ "00-19",
                  age_min=="10" & age_max=="14" ~ "00-19",
                  age_min=="15" & age_max=="19" ~ "00-19",
                  age_min=="20" & age_max=="24" ~ "20-29",
                  age_min=="25" & age_max=="29" ~ "20-29",
                  age_min=="30" & age_max=="34" ~ "30-39",
                  age_min=="35" & age_max=="39" ~ "30-39",
                  age_min=="40" & age_max=="44" ~ "40-49",
                  age_min=="45" & age_max=="49" ~ "40-49",
                  age_min=="50" & age_max=="54" ~ "50-59",
                  age_min=="55" & age_max=="59" ~ "50-59",
                  age_min=="60" & age_max=="64" ~ "60-999",
                  age_min=="65" & age_max=="69" ~ "60-999",
                  age_min=="70" & age_max=="74" ~ "60-999",
                  age_min=="75" & age_max=="79" ~ "60-999",
                  age_min=="80Plus" ~ "60-999"
    )) %>%
  group_by(i_key, pcode, age_group10y, sex) %>%
  summarise(value = sum(pop2), .groups = "drop") %>% ungroup() %>%
  mutate(a = case_when(age_group10y=="00-19"~1, age_group10y=="20-29"~2, age_group10y=="30-39"~3, 
  age_group10y=="40-49"~4, age_group10y=="50-59"~5, age_group10y=="60-999"~6) %>% as.integer(),
    s = ifelse(sex == "F", 2, 1) %>% as.integer()) %>%
  left_join(i_idx) %>%
  arrange(i, a, s) %>%
  mutate(ias = row_number()) %>%
  group_by(i, s) %>%
  mutate(is = row_number()) %>% ungroup() %>%
  filter(!is.na(a)) %>%
  select(ias, is, i, a, s, value)


md$N2 <- md$N210$value
md$ias_N2 <- md$N210$ias

md$Y2 <- md$N210 |>
  arrange(i, a, s) |>
  filter(a=='1') %>%
    select(value) |>
  pull()

md$A2 <- md$N210 |>
  arrange(i, a, s) |>
  filter(a!=1) %>%
    select(value) |>
  pull()


md$N21 <- codps24 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop2"
  )|>
  group_by(ADM1_PCODE) %>%
  summarise(pop2 = sum(pop2, na.rm=T)) %>% ungroup() %>%
  left_join(codps22[,c("ADM1_PCODE", "fb_key")]) %>%
  arrange(match(fb_key, i_idx$i_key)) %>%
  pull(pop2)


pop2_date <- as.Date("2024-05-15")   
pop2_week <- floor_date(pop2_date, "week", week_start = 1)
t_pop2    <- time_index %>%
  filter(t_name == pop2_week) %>%
  pull(t)

full_pop24 <- codps24 %>%
  select(ADM1_PCODE, ADM2_PCODE, all_of(cols_to_pivot)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),
    names_to  = "demog",
    values_to = "pop2"
  ) %>%
  group_by(ADM1_PCODE, demog) %>%
  summarise(pop2 = sum(pop2, na.rm = TRUE), .groups = "drop") %>% ungroup() %>%
  separate(demog, into = c("sex","age_min","age_max"), sep = "_") %>%
  filter(sex != "T", age_min != "TL")
total_pop_all24 <- sum(full_pop24$pop2, na.rm = TRUE)
full_pop24_sel <- full_pop24 %>%
  left_join(codps22 %>% select(ADM1_PCODE, fb_key),
    by = "ADM1_PCODE") %>%
  filter(fb_key %in% selected_locations_key)
total_pop_sel24 <- sum(full_pop24_sel$pop2, na.rm = TRUE)
prop_sel24 <- total_pop_sel24 / total_pop_all24


######################################
# Total population at every time step
######################################
weekly_avg <- outside_border |>
  mutate(week = floor_date(as.Date(date), "week", week_start = 1)) |>
  group_by(week) |>
  summarise(avg_value = mean(individuals, na.rm = TRUE)) |>
  filter(week >= min(md$idx$t_name) &
    week <= max(md$idx$t_name))

border_df <- time_index %>%
  select(t, t_name) %>%
  left_join(weekly_avg %>% rename(t_name = week), 
    by = "t_name") %>%
  mutate(avg_value = if_else(t == 1, 0, avg_value))

md$y_N_tot <- border_df %>%
  mutate(baseline = case_when(
      t <  t_pop1 ~ sum(md$N01),
      t == t_pop1 ~ sum(md$N11),
      t <  t_pop2 ~ sum(md$N11),
      t == t_pop2 ~ sum(md$N21),
      TRUE        ~ sum(md$N21)),
    prop = case_when(
      t <= t_pop1 ~ prop_sel22,
      t_pop1 <  t & t <= t_pop2 ~ prop_sel23,
      TRUE ~ prop_sel24
    ),
    ytmp = (baseline - avg_value) * prop,
    y_N_tot = as.integer(ytmp)) %>%
  pull(y_N_tot)

rm(weekly_avg)


young <- md$N010 %>% filter(a==1) %>% mutate(t=1) %>% rename(young=value) %>%
  rbind(md$N110 %>% filter(a==1) %>% mutate(t=t_pop1) %>% rename(young=value)) %>%
  rbind(md$N210 %>% filter(a==1) %>% mutate(t=t_pop2) %>% rename(young=value))
  
adult <- md$N010 %>% filter((a==2|a==3|a==4), s==2) %>% group_by(i) %>% summarise(adult = sum(value, na.rm=T)) %>% ungroup() %>% mutate(t=1) %>%
  rbind(md$N110 %>% filter((a==2|a==3|a==4), s==2) %>% group_by(i) %>% summarise(adult = sum(value, na.rm=T)) %>% ungroup() %>% mutate(t=t_pop1)) %>%
  rbind(md$N210 %>% filter((a==2|a==3|a==4), s==2) %>% group_by(i) %>% summarise(adult = sum(value, na.rm=T)) %>% ungroup() %>% mutate(t=t_pop2))

md$ratio_young_adult <- expand.grid(
  t = seq(1, md$T, by = 1),
  i = seq(1, md$I, by = 1),
  s = seq(1, md$S, by = 1)
) %>%
left_join(young) %>%
left_join(adult) %>%
mutate(ratio_young_adult = young/adult) %>%
group_by(i, s) %>%           
arrange(t, .by_group = TRUE) %>%
mutate(ratio_young_adult_im = zoo::na.approx(ratio_young_adult, x = t, na.rm = FALSE)) %>%
ungroup() %>%
arrange(t, i, s) %>%
select(ratio_young_adult_im) |>
pull()


md$tt <- md$idx %>%
  distinct(t, i, a, s) %>%
  filter(a!=1) %>%
  arrange(t, i, a, s) %>% 
  pull(t)              

md$ii <- md$idx %>%
  distinct(t, i, a, s) %>%
  filter(a!=1) %>%
  arrange(t, i, a, s) %>% 
  pull(i)   

md$ss <- md$idx %>%
  distinct(t, i, a, s) %>%
  filter(a!=1) %>%
  arrange(t, i, a, s) %>% 
  pull(s)   

md$aa <- md$idx  %>%
  distinct(t, i, a, s) %>%
  filter(a!=1) %>%
  arrange(t, i, a, s) %>% 
  pull(a)   
md$aa <- md$aa - 1L





t <- unique(md$idx$t)
i <- unique(md$idx$i)
a <- unique(md$idx$a)
s <- unique(md$idx$s)
e <- unique(md$idx$e)

I = md$I
A = md$A
S = md$S
T = md$T
A_adult = md$A-1
C_full  <- I * A * S    
C_adult <- I * (A-1) * S 
IS <- I * S              



adult_grid <- expand.grid(
  t = 1:T,
  i = 1:I,
  a = 1:(A-1), 
  s = 1:S
)
md$adult_grid <- adult_grid[order(adult_grid$t, adult_grid$i, adult_grid$a, adult_grid$s), ]
md$female_core_mask <- as.integer(md$adult_grid$a %in% 1:3 & md$adult_grid$s == 2)
md$adult_grid$idx_tis <- (md$adult_grid$t - 1) * I * S + (md$adult_grid$i - 1) * S + md$adult_grid$s
md$tis_index <- md$adult_grid$idx_tis


md$young_pos_full <- tidyr::expand_grid(i = 1:I, s = 1:S) %>%
  dplyr::arrange(i, s) %>%                
  dplyr::mutate(
    full_idx = ((i - 1) * A + (1 - 1)) * S + s  
  ) %>%
  dplyr::pull(full_idx) %>%
  as.integer()

md$adult_pos_full <- tidyr::expand_grid(
  i    = 1:md$I,
  a_ad = 1:(md$A - 1),  
  s    = 1:md$S
) %>%
dplyr::arrange(i, a_ad, s) %>%   
dplyr::mutate(
  a = a_ad + 1L,                 
  full_idx = ((i - 1) * md$A + (a - 1)) * md$S + s
) %>%
dplyr::pull(full_idx) %>%
as.integer()

md$N_y <- T * length(md$young_pos_full)
md$N_a <- T * length(md$adult_pos_full)

md$young_idx <- 
  rep((0:(T-1)) * md$C_full, each = length(md$young_pos_full)) +
  rep(md$young_pos_full, times = T)

md$adult_idx <- 
  rep((0:(T-1)) * md$C_full, each = length(md$adult_pos_full)) +
  rep(md$adult_pos_full, times = T)


length(md$young_idx)  
length(md$adult_idx)  


md$slice_full  <- matrix(seq_len(T * C_full),  nrow = T, byrow = TRUE)
md$slice_adult <- matrix(seq_len(T * C_adult), nrow = T, byrow = TRUE)
md$slice_adult_lag <- rbind(
  rep(1L, C_adult),
  md$slice_adult[-T, ])
md$slice_young      <- matrix(seq_len(T * I * S), nrow = T, byrow = TRUE)
md$slice_young_lag  <- rbind(
  rep(1L, I * S),
  md$slice_young[-T, ])



#md$t_slice <- function(t, block_size) {
#  a <- 1 + (t - 1) * block_size   
#  b <- t       * block_size       
#  a:b                             
#}


#md$slice_full       <- sapply(1:T,   md$t_slice, block_size = md$C_full,   simplify = FALSE)
#md$slice_adult      <- sapply(1:T,   md$t_slice, block_size = md$C_adult,  simplify = FALSE)
#md$slice_adult_lag  <- sapply(2:T,   md$t_slice, block_size = md$C_adult,  simplify = FALSE)
#md$slice_young      <- sapply(1:T,   md$t_slice, block_size = md$I*md$S,   simplify = FALSE)
#md$slice_young_lag  <- sapply(2:T,   md$t_slice, block_size = md$I*md$S,   simplify = FALSE)

#md$t_slice <- NULL 

f_core <- (md$ss == 2L) & (md$aa %in% 2:4)      # length T*C_adult
md$idx_female_a234 <- which(f_core)                  # 1-based
md$idx_female_triple <- matrix(md$idx_female_a234,
                               ncol = 3, byrow = TRUE) # dims (T*I) × 3

md$N_fem_a <- length(md$idx_female_a234)



#md$mother_mask <- (md$ss == 2) & (md$aa %in% 2:4)
#md$idx_female_a234 <- which(md$mother_mask)   
#md$N_fem_a  <- length(md$idx_female_a234)
#md$female_core_mask <- as.numeric(md$mother_mask) 

## Covariates ##
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
  a = min(md$aa):max(md$aa),
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

