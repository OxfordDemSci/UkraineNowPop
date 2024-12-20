# model data
md <- list()

# set seed for random number generators
if ("seed" %in% names(md)) {
  seed <- md$seed
} else {
  seed <- round(runif(1, 1, 1e6))
}
set.seed(seed)


#---- location and date filtering ----#

last_date <- "2024-05-14" # max(idx$t_name)
#drop_locations <- c(3782, 3788, 3791, 3797)

# new i indexes after dropping locations
i_idx <- idx |>
  select(i_key) |>
  #filter(!i_key %in% drop_locations) |>
  distinct() |>
  arrange(i_key) |>
  mutate(i = row_number())


#---- indexes ----#

# master index
md$idx <- idx |>
  select(t, t_key, t_name, i_key, i_name, a, a_key, a_name, s, s_key, s_name) |>
  #filter(!i_key %in% drop_locations) |>
  left_join(i_idx, by = "i_key") |>
  arrange(t, i, a, s) |>
  mutate(tias = row_number()) |>
  filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) |>
  select(tias, t, i, a, s, t_key, t_name, i_key, i_name, a_key, a_name, s, s_key, s_name)

# Facebook
md$idx_F <- idx_F |>
  select(t, i, a, s, m, value) |>
  left_join(idx |> select(i, i_key, a, a_key, s, s_key) |> distinct()) |>
  select(-c(i)) |>
  left_join(i_idx) |>
  right_join(md$idx |> select(t, i, a, s, tias)) |>
  filter(value > 0 & is.finite(value)) |>
  select(tias, t, i, a, s, m, value)

# Instagram
md$idx_G <- idx_G |>
  select(t, i, a, s, m, value) |>
  left_join(idx |> select(i, i_key, a, a_key, s, s_key) |> distinct()) |>
  select(-c(i)) |>
  left_join(i_idx) |>
  right_join(md$idx |> select(t, i, a, s, tias)) |>
  filter(value > 0 & is.finite(value)) |>
  select(tias, t, i, a, s, m, value)

rm(last_date)


#---- prepare data ----#

md$I <- max(md$idx$i)
md$T <- max(md$idx$t)
md$G <- max(md$idx$a)
md$S <- max(md$idx$s)

md$C <- max(md$idx$s)*max(md$idx$a)*max(md$idx$i)


## social media ##

# Facebook
md$y_F <- md$idx_F$value
md$n_F <- length(md$y_F)
md$tias_F <- md$idx_F$tias

# Instagram
md$y_G <- md$idx_G$value
md$n_G <- length(md$y_G)
md$tias_G <- md$idx_G$tias

rm(drop, F_tias, G_tias, i)


## population ##

# baseline population
md$N01 <- codps |>
pivot_longer(cols = 8:58,
    names_to = c("demog"),
    values_to = "pop0") |>
separate(demog, into = c("sex", "age_min", "age_max"), sep = "_") |>
filter(sex!="T", age_min!="TL") |>
rename(pcode=ADM1_PCODE, i_key=fb_key) |>
mutate(age_group10y = case_when(
                  age_min=="00" & age_max=="04" ~ "0-9",
                  age_min=="05" & age_max=="09" ~ "0-9",
                  age_min=="10" & age_max=="14" ~ "10-19",
                  age_min=="15" & age_max=="19" ~ "10-19",
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
#filter(age_max>19, !i_key %in% drop_locations) |>
group_by(i_key, pcode, age_group10y, sex) |>
summarise(value =sum(pop0)) |> ungroup() |>
mutate(a = case_when(age_group10y=="20-29"~1, age_group10y=="30-39"~2, age_group10y=="40-49"~3, age_group10y=="50-59"~4, age_group10y=="60-999"~5) %>% as.integer(),
       s = ifelse(sex=="F", 2, 1) %>% as.integer()) |>
left_join(i_idx) |>
arrange(i, a, s) |>
mutate(ias = row_number()) |>
filter(!is.na(a)) |>
select(ias, i, a, s, value)

md$N0 <- md$N01$value
md$n_N0 <- length(md$N0)
md$ias_N <- md$N01$ias

  


# total population at each time step
md$N00 <- codps[as.character(unique(md$idx$i_key[order(md$idx$i)])), "T_TL"]   
weekly_avg <- outside_border |>
  mutate(week = floor_date(as.Date(date), "week", week_start = 1)) |>
  group_by(week) |>
  summarise(avg_value = mean(individuals, na.rm = TRUE)) |>
  filter(week >= min(md$idx$t_name) &
    week <= max(md$idx$t_name))

md$y_N_tot <- as.integer(sum(md$N00) - weekly_avg$avg_value)

rm(weekly_avg)

# indexing: long format for N
md$tias_N0 <- md$idx$tias[md$idx$t == 1]
md$tias_N <- md$idx$tias[md$idx$t > 1]
md$tias_N_lag <- md$idx$tias[md$idx$t > 1] - (md$I*md$G*md$S)  

md$tt <- md$idx$t
md$ii <- md$idx$i
md$gg <- md$idx$a
md$ss <- md$idx$s

t <- unique(md$idx$t)
i <- unique(md$idx$i)
g <- unique(md$idx$a)
s <- unique(md$idx$s)



 ## covariates ##
covs <- read.csv(
  file.path(out_dir, "covariates", "final", paste0("ua_warfires_oblast.csv"))
) |>
right_join(idx) |> arrange(t, i, a, s) 

md$X_r <- matrix(covs$value, ncol = 1)
md$K_r <- 1



#---- initial values ----#
init_generator <- function(md, chain_id = 1) {
  result <- list()
  N <- array(NA, dim = c(md$T, md$I, md$G, md$S))

  # Compute probabilities for the multinomial draw
  total_N0 <- sum(md$N0)
  base_prob <- md$N0 / total_N0

  # For each time step, draw from a multinomial distribution
  # to allocate y_N_tot[t] across the C = I*G*S combinations
  for (t in 1:md$T) {
    counts <- rmultinom(1, md$y_N_tot[t], base_prob)
    N[t,,,] <- array(counts, dim = c(md$I, md$G, md$S))
  }

  for (t in 1:md$T) {
    for (i in 1:md$I) {
      for (g in 1:md$G) {
        for (s in 1:md$S) {
          tias <- which(md$tt == t & md$ii == i & md$gg == g & md$ss == s)
          y_F_values <- md$y_F[md$tias_F %in% tias]
          if (length(y_F_values) > 0) {
            max_y_F <- max(y_F_values, na.rm = TRUE)
            if (!is.na(max_y_F) && max_y_F > N[t, i, g, s]) {
              N[t, i, g, s] <- max_y_F
            }}}}}}

  result[["N"]] <- as.vector(N)
  result[["N_tot"]] <- md$y_N_tot

  result[["r"]] <- rlnorm(md$T * md$C, 0, 0.1 / 2)

  result[["p_F"]] <- rlnorm(md$T * md$C,
                            meanlog = -1, # a bit larger than original to avoid near-0
                            sdlog = 0.5)

  result[["p_GF"]] <- rlnorm(md$T * md$C,
                             meanlog = 0,  # centre around 1 in log scale
                             sdlog = 0.5)

  result[["mu_p_F"]] <- runif(1, -3, -1) 
  result[["sigma_p_F"]] <- runif(1, 0.1, 0.5) 

  result[["mu_p_GF"]] <- rnorm(1, 0, 1) 
  result[["log_sigma_p_GF"]] <- rnorm(1, 0, 1)

  return(result)
}
inits <- lapply(1:chains, function(id) init_generator(md = md, chain_id = id))
inits



result <- inits[[1]]

N_values <- result$N
N_array <- array(N_values, dim = c(md$T, md$I, md$G, md$S))

summary(N_values)

# Calculate specific statistics
mean(N_values, na.rm = TRUE)
sd(N_values, na.rm = TRUE)
min(N_values, na.rm = TRUE)
max(N_values, na.rm = TRUE)


verify_probabilities <- function(md, inits) {
  age_sex_prob <- matrix(0, nrow = md$G, ncol = md$S)
  total_N0 <- sum(md$N0)
  for (g in 1:md$G) {
    for (s in 1:md$S) {
      sum_N0_gs <- sum(md$N0[md$gg == g & md$ss == s], na.rm = TRUE)
      age_sex_prob[g, s] <- sum_N0_gs / total_N0
    }}
  
  location_prob <- array(0, dim = c(md$I, md$G, md$S))
  for (g in 1:md$G) {
    for (s in 1:md$S) {
      sum_N0_gs <- sum(md$N0[md$gg == g & md$ss == s], na.rm = TRUE)
      for (i in 1:md$I) {
        combination_idx <- (i - 1) * md$G * md$S + (g - 1) * md$S + s
        N0_value <- md$N0[combination_idx]
        location_prob[i, g, s] <- N0_value / sum_N0_gs
      }}}
  
  total_prob <- array(0, dim = c(md$I, md$G, md$S))
  for (g in 1:md$G) {
    for (s in 1:md$S) {
      for (i in 1:md$I) {
        total_prob[i, g, s] <- age_sex_prob[g, s] * location_prob[i, g, s]
      }
    }
  }
  
  total_prob[total_prob == 0] <- 0.0001
  
  sum_total_prob <- sum(total_prob, na.rm = TRUE)
  total_prob_norm <- total_prob / sum_total_prob
  
  for (t in 1:md$T) {
    probs_vector <- as.vector(total_prob_norm)
    sum_probs <- sum(probs_vector, na.rm = TRUE)
    cat(sprintf("Sum of probabilities for time %d: %.6f\n", t, sum_probs))
  }
}
verify_probabilities(md, inits)


