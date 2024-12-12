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
drop_locations <- c(3782, 3788, 3791, 3797)

# new i indexes after dropping locations
i_idx <- idx |>
  select(i_key) |>
  filter(!i_key %in% drop_locations) |>
  distinct() |>
  arrange(i_key) |>
  mutate(i = row_number())


#---- indexes ----#

# master index
md$idx <- idx |>
  select(t, t_key, t_name, i_key, i_name, a, a_key, a_name, s, s_key, s_name) |>
  filter(!i_key %in% drop_locations) |>
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

# Facebook:Instagram ratio
md$tias_FG <- unique(md$tias_F[which(md$tias_F %in% md$tias_G)])
md$n_FG <- length(md$tias_FG)

md$y_FG_ratio <- c()
for (i in 1:length(md$tias_FG)) {
  tias <- md$tias_FG[i]
  G_tias <- mean(md$y_G[which(md$tias_G == tias)])
  F_tias <- mean(md$y_F[which(md$tias_F == tias)])
  md$y_FG_ratio[i] <- G_tias / F_tias
}

drop <- which(!is.finite(md$y_FG_ratio))
if (length(drop) > 0) {
  md$y_FG_ratio <- md$y_FG_ratio[-drop]
  md$tias_FG <- md$tias_FG[-drop]
  md$n_FG <- md$n_FG - length(drop)
}

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
filter(age_max>19, !i_key %in% drop_locations) |>
group_by(i_key, pcode, age_group10y, sex) |>
summarise(value =sum(pop0)) |> ungroup() |>
mutate(a = case_when(age_group10y=="20-29"~1, age_group10y=="30-39"~2, age_group10y=="40-49"~3, age_group10y=="50-59"~4, age_group10y=="60-999"~5) %>% as.integer(),
       s = ifelse(sex=="F", 2, 1) %>% as.integer()) |>
left_join(i_idx) |>
arrange(i, a, s) |>
mutate(ias = row_number()) |>
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





#---- initial values ----#
init_generator <- function(md, chain_id = 1) {
  result <- list()
  N <- matrix(NA, nrow = md$T, ncol = md$C)
  base_prob <- md$N0 / sum(md$N0)

  N <- t(rmultinom(n = md$T, size = md$y_N_tot, prob = base_prob))
  for (t in 1:md$T) {
    for (c in 1:md$C) {
      i <- ((c-1) %/% (md$S * md$G)) + 1
      sg <- (c-1) %% (md$S * md$G)
      s <- (sg %/% md$G) + 1
      g <- (sg %% md$G) + 1
      
      # Find indices for this t,i,s,g
      tias_ind <- which(md$tt == t & md$ii == i & md$ss == s & md$gg == g)
  
      if (length(tias_ind) > 0) {
        vals <- md$y_F[md$tias_F %in% tias_ind]
        if (length(vals) > 0) {
          max_y_F <- max(vals, na.rm = TRUE)
          if (!is.na(max_y_F) && max_y_F > N[t, c]) {
            N[t, c] <- max_y_F
          }}}}}  

  N_vec <- as.vector(t(N))
  result[["N"]] <- N_vec
  result[["p_F"]] <- rlnorm(md$T * md$C, meanlog = -1, sdlog = 0.5)
  result[["p_GF"]] <- runif(md$T * md$C, min=0.1, max=0.9)        
 
  
  result[["mu_p_F0"]] <- runif(1, -4, -2)
  result[["mu_p_F_t"]] <- rnorm(md$T, 0, 0.1)
  result[["mu_p_F_ias"]] <- rnorm(md$C, 0, 0.1)
  result[["log_sigma_p_F_t"]] <- rnorm(1, 0, 1)
  result[["log_sigma_p_F_ias"]] <- rnorm(1, 0, 1)
  result[["log_sigma_p_F"]] <- rnorm(1, 0, 1)

  result[["mu_p_GF"]] <- rnorm(1, 0, 1)
  result[["log_sigma_p_GF"]] <- rnorm(1, 0, 1)

  result[["mu_p0"]] <- rnorm(1,0,1)
  result[["mu_p_t"]] <- rnorm(md$T-1,0,0.1)
  result[["mu_p_ias"]] <- rnorm(md$C-1,0,0.1)
  result[["log_sigma_p_t"]] <- runif(1,-1,0)
  result[["log_sigma_p_ias"]] <- runif(1,-1,0)
  result[["log_sigma_p"]] <- runif(1,-1,0)

  base_probs <- md$N0 / sum(md$N0)
  ref_probs <- base_probs[1:(md$C-1)]
  logit_init <- log(ref_probs/(1 - sum(ref_probs)))

  result[["logit_p"]] <- matrix(
    rnorm((md$T-1)*(md$C-1), 
          mean = rep(logit_init, times = md$T-1),
          sd = 0.25),
    nrow = md$T-1, ncol = md$C-1
  )

  result[["log_kappa_F"]] <- rnorm(1, 0, 1)
  result[["log_kappa_G"]] <- rnorm(1, 0, 1)

  result[["mu_p_F"]] <- runif(1,-3,-1)
  result[["mu_p_G"]] <- runif(1,-3,-1)
  result[["sigma_p_F"]] <- runif(1, 0, 0.2)
  result[["sigma_p_G"]] <- runif(1, 0, 0.2)

  result[["N_tot"]] <- md$y_N_tot

  return(result)
}


inits <- lapply(1:chains, function(id) init_generator(md = md, chain_id = id))
inits

