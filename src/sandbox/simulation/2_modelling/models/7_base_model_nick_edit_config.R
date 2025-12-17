#---- load data ----#

# load data
mddir <- file.path(wd, 'out', 'simulation')
# md <- readRDS(file.path(mddir, 'simulated_data.rds'))
md <- readRDS(file.path(mddir, 'md_1_base_model.rds'))


# set seed for random number generators
if('seed' %in% names(md)){
  seed <- md$seed
} else {
  seed <- round(runif(1, 1, 1e6))
}
set.seed(seed)



#---- prepare data ----#


# ## population ##
# 
# # total population at each time step
# md$y_N_tot <- apply(md$N_true, 1, sum)
# 
# # baseline population
# md$N0 <- md$N_true[1,]
# 
# # index to reference T x I combinations in vector format
# idx <- data.frame(T = rep(1:md$T, each=md$I), 
#                   I = rep(1:md$I, md$T), 
#                   ti = 1:(md$T*md$I))
# 
# # indexing: long format for N
# md$ti_N0 <- idx$ti[idx$T==1]
# md$ti_N <- idx$ti[idx$T>1]
# md$ti_N_lag <- idx$ti[idx$T>1]-md$I
# 
# md$tt <- idx$T
# md$ii <- idx$I
# 
# # long format for population covariates (X_r)
# melt_X <- reshape2::melt(md$X, varnames=c('T', 'I', 'K'))
# idx_X <- idx
# for(k in 1:md$K){
#   idx_X <- merge(idx_X, melt_X[melt_X$K==k,c('T','I','value')], by=c('T','I'))
#   names(idx_X)[names(idx_X)=='value'] <- paste0('x',k)
# }
# idx_X <- idx_X[order(idx_X$ti),]
# row.names(idx_X) <- idx_X$ti
# 
# md$X <- idx_X[,paste0('x', 1:md$K)]
# 
# 
# ## Facebook ##
# 
# # baseline detection
# # md$p0_F <- apply(md$y1[1,,], 1, max, na.rm=T) / md$N0
# 
# # prepare to convert y to long format
# y_long <- reshape2::melt(md$y1, varnames=c('T', 'I', 'M'))
# y_long <- y_long[!is.na(y_long$value),]
# 
# y_idx <- y_long[,c('T','I','M')]
# y_idx <- merge(y_idx, idx, by=c('T', 'I'))
# y_idx <- y_idx[order(y_idx$M, y_idx$I, y_idx$T),]
# 
# # long format for y
# md$y_F <- y_long$value
# md$ti_F <- y_idx$ti
# md$n_F <- length(md$y_F)
# 
# # fill missing data
# md$y1[is.na(md$y1)] <- 999999999
# 
# # rename
# names(md)[names(md)=='p1_true'] <- 'p_F_true'
# names(md)[names(md)=='y1'] <- 'y_F_orig'
# md$M1 <- NULL
# 
# 
# ## Instagram ##
# 
# # baseline detection
# # md$p0_G <- apply(md$y2[1,,], 1, max, na.rm=T) / md$N0
# 
# # prepare to convert y to long format
# y_long <- reshape2::melt(md$y2, varnames=c('T', 'I', 'M'))
# y_long <- y_long[!is.na(y_long$value),]
# 
# y_idx <- y_long[,c('T','I','M')]
# y_idx <- merge(y_idx, idx, by=c('T', 'I'))
# y_idx <- y_idx[order(y_idx$M, y_idx$I, y_idx$T),]
# 
# # long format for y
# md$y_G <- y_long$value
# md$ti_G <- y_idx$ti
# md$n_G <- length(md$y_G)
# 
# # fill missing data
# md$y2[is.na(md$y2)] <- 999999999
# 
# # rename
# names(md)[names(md)=='p2_true'] <- 'p_G_true'
# names(md)[names(md)=='y2'] <- 'y_G_orig'
# md$M2 <- NULL
# 
# 
# ## Facebook:Instagram ratio ###
# md$ti_FG <- unique(md$ti_F[which(md$ti_F %in% md$ti_G)])
# md$n_FG <- length(md$ti_FG)
# 
# md$y_FG_ratio <- c()
# for(i in 1:length(md$ti_FG)){
#   ti <- md$ti_FG[i]
#   G_ti <- mean(md$y_G[which(md$ti_G == ti)], na.rm=T)
#   F_ti <- mean(md$y_F[which(md$ti_F == ti)], na.rm=T)
#   md$y_FG_ratio[i] <- G_ti / F_ti
# }
# 
# 
# 
# 
# ## save to disk ##
# saveRDS(md, file.path(outdir, paste0('md_', model_name, '.rds')))


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
  
  # result[["p_F"]] <- rlnorm(md$T * md$I, log(mean(md$y_F, na.rm = T) / mean(md$N0)), 0.5)
  result[["p_F"]] <- 
    # matrix(
      rlnorm(md$T * md$I, log(mean(md$y_F, na.rm = T) / mean(md$N0)), 0.5)#,
    # nrow = md$T, ncol=md$I)
  # result[["mu_p_F"]] <- runif(md$T, -4, -2)
  result[["mu_p_F"]] <- runif(1, -4, -2)
  result[["mu_mu_p_F"]] <- runif(1, -4, -2)
  result[["sigma_mu_p_F"]] <- runif(1, 0, 0.2)
  result[["sigma_p_F"]] <- runif(1, 0, 0.2)
  
  result[["p_G"]] <- rlnorm(md$T * md$I, log(mean(md$y_G, na.rm = T) / mean(md$N0)), 0.5)
  result[['p_GF']] <- runif(md$T * md$I)
  # result[['p_GF']] <- matrix(runif(md$T * md$I),nrow=md$T,ncol=md$I)
  # result[["mu_p_G"]] <- runif(md$I, -4, -2)
  result[['mu_p_GF']] <- runif(1)
  result[["mu_mu_p_G"]] <- runif(1, -4, -2)
  result[["sigma_mu_p_G"]] <- runif(1, 0, 0.2)
  # result[["sigma_p_G"]] <- runif(1, 0, 0.2)
  result[['sigma_p_GF']] <- runif(1, 0, 0.05)
  
  result[['logit_p']] <- 
    matrix(
      rnorm((md$T-1)*(md$I-1),
            rep(log((md$N0/sum(md$N0))[1:(md$I-1)] / 
            (1-sum((md$N0/sum(md$N0))[1:(md$I-1)]))),times=md$T-1),
            0.25),
      nrow=md$T-1,ncol=md$I-1)
  result[['sigma_p']] <- runif(1,0,2)
      
  
  return(result)
}

# #---- initial values ----#
# init_generator <- function(md=md, chain_id=1){
#   result <- list()
# 
#   N <- matrix(NA, nrow=md$T, ncol=md$I)
#   for(t in 1:md$T){
#     if(dim(md$y_F_orig)[3]==1){
#       theta <- md$y_F_orig[t,,1] / md$y_N_tot
#     } else {
#       theta <- apply(md$y_F_orig[t,,], 1, max, na.rm=T) / md$y_N_tot[t]
#     }
# 
#     theta <- theta / sum(theta)
# 
#     N[t,] <- rbinom(md$I, md$y_N_tot[t], theta)
#   }
# 
#   result[['N_tot']] <- md$y_N_tot
#   result[['N']] <- reshape2::melt(N, varnames=c('T', 'I'))$value
#   result[['r']] <- rlnorm(md$T * md$I, 0, 0.1)
#   result[['sigma_r']] <- runif(1, 0, 0.5)
#   result[['mu_r']] <- rnorm(md$T * md$I, 0, 0.1)
#   result[['alpha_r']] <- rnorm(1, 0, 3)
#   result[['beta_r']] <- rnorm(md$K, 0, 1)
# 
#   result[['p_F']] <- rlnorm(md$T * md$I, log(mean(md$p_F)), 0.5)
#   result[['mu_p_F']] <- rnorm(1, 1, 1)
#   result[['sigma_p_F']] <- runif(1, 0, 0.05)
# 
#   # result[['p_G']] <- rlnorm(md$T * md$I, log(mean(md$p_G)), 0.5)
#   result[['p_GF']] <- runif(md$T * md$I)
#   # result[['mu_p_G']] <- rnorm(1, 1, 1)
#   result[['mu_p_GF']] <- runif(1)
#   # result[['sigma_p_G']] <- runif(1, 0, 0.05)
#   result[['sigma_p_GF']] <- runif(1, 0, 0.05)
# 
#   return(result)
# }



# # model data
# md <- list()
# 
# # set seed for random number generators
# if ("seed" %in% names(md)) {
#   seed <- md$seed
# } else {
#   seed <- round(runif(1, 1, 1e6))
# }
# set.seed(seed)
# 
# 
# #---- location and date filtering ----#
# 
# last_date <- "2023-02-25" # max(idx$t_name)
# drop_locations <- c(3782, 3788, 3791, 3797)
# 
# # new i indexes after dropping locations
# i_idx <- idx |>
#   select(i_key) |>
#   filter(!i_key %in% drop_locations) |>
#   distinct() |>
#   arrange(i_key) |>
#   mutate(i = row_number())
# 
# 
# #---- indexes ----#
# 
# # master index
# md$idx <- idx |>
#   select(t, t_key, t_name, i_key, i_name) |>
#   filter(!i_key %in% drop_locations) |>
#   left_join(i_idx, by = "i_key") |>
#   arrange(t, i) |>
#   mutate(ti = row_number()) |>
#   filter(t_name <= floor_date(as.Date(last_date), "week", week_start = 1)) |>
#   select(ti, t, i, t_key, t_name, i_key, i_name)
# 
# # Facebook
# md$idx_F <- idx_F |>
#   select(t, i, m, value) |>
#   left_join(idx |> select(i, i_key) |> distinct()) |>
#   select(-i) |>
#   left_join(i_idx) |>
#   right_join(md$idx |> select(t, i, ti)) |>
#   filter(value > 0 & is.finite(value)) |>
#   select(ti, t, i, m, value)
# 
# # Instagram
# md$idx_G <- idx_G |>
#   select(t, i, m, value) |>
#   left_join(idx |> select(i, i_key) |> distinct()) |>
#   select(-i) |>
#   left_join(i_idx) |>
#   right_join(md$idx |> select(t, i, ti)) |>
#   filter(value > 0 & is.finite(value)) |>
#   select(ti, t, i, m, value)
# 
# rm(last_date)
# 
# 
# #---- prepare data ----#
# 
# md$I <- max(md$idx$i)
# md$T <- max(md$idx$t)
# 
# 
# ## social media ##
# 
# # Facebook
# md$y_F <- md$idx_F$value
# md$n_F <- length(md$y_F)
# md$ti_F <- md$idx_F$ti
# 
# # Instagram
# md$y_G <- md$idx_G$value
# md$n_G <- length(md$y_G)
# md$ti_G <- md$idx_G$ti
# 
# # Facebook:Instagram ratio
# md$ti_FG <- unique(md$ti_F[which(md$ti_F %in% md$ti_G)])
# md$n_FG <- length(md$ti_FG)
# 
# md$y_FG_ratio <- c()
# for (i in 1:length(md$ti_FG)) {
#   ti <- md$ti_FG[i]
#   G_ti <- mean(md$y_G[which(md$ti_G == ti)])
#   F_ti <- mean(md$y_F[which(md$ti_F == ti)])
#   md$y_FG_ratio[i] <- G_ti / F_ti
# }
# 
# drop <- which(!is.finite(md$y_FG_ratio))
# if (length(drop) > 0) {
#   md$y_FG_ratio <- md$y_FG_ratio[-drop]
#   md$ti_FG <- md$ti_FG[-drop]
#   md$n_FG <- md$n_FG - length(drop)
# }
# 
# rm(drop, F_ti, G_ti, i)
# 
# 
# ## population ##
# 
# # baseline population
# md$N0 <- codps[as.character(unique(md$idx$i_key[order(md$idx$i)])), "T_TL"]
# 
# # total population at each time step
# weekly_avg <- outside_border |>
#   mutate(week = floor_date(as.Date(date), "week", week_start = 1)) |>
#   group_by(week) |>
#   summarise(avg_value = mean(individuals, na.rm = TRUE)) |>
#   filter(week >= min(md$idx$t_name) &
#            week <= max(md$idx$t_name))
# 
# md$y_N_tot <- as.integer(sum(md$N0) - weekly_avg$avg_value)
# 
# rm(weekly_avg)
# 
# # indexing: long format for N
# md$ti_N0 <- md$idx$ti[md$idx$t == 1]
# md$ti_N <- md$idx$ti[md$idx$t > 1]
# md$ti_N_lag <- md$idx$ti[md$idx$t > 1] - md$I
# 
# md$tt <- md$idx$t
# md$ii <- md$idx$i
# 
# 
# 
# #---- initial values ----#
# init_generator <- function(md = md, chain_id = 1) {
#   result <- list()
#   
#   N <- matrix(NA, nrow = md$T, ncol = md$I)
#   theta <- md$N0 / sum(md$N0)
#   
#   N <- t(rmultinom(n = md$T, size = md$y_N_tot, prob = md$N0 / sum(md$N0)))
#   for (t in 1:md$T) {
#     for (i in 1:md$I) {
#       ti <- which(md$tt == t & md$ii == i)
#       if (any(md$y_F[md$ti_F == ti] > N[t, i])) {
#         N[t, i] <- max(md$y_F[md$ti_F == ti])
#       }
#     }
#   }
#   
#   result[["N_tot"]] <- md$y_N_tot
#   result[["N"]] <- reshape2::melt(N, varnames = c("T", "I"))$value
#   result[["r"]] <- rlnorm(md$T * md$I, 0, 0.1 / 2)
#   
#   result[["p_F"]] <- rlnorm(md$T * md$I, log(mean(md$y_F, na.rm = T) / mean(md$N0)), 0.5)
#   result[["mu_p_F"]] <- runif(md$T, -4, -2)
#   result[["mu_mu_p_F"]] <- runif(1, -4, -2)
#   result[["sigma_mu_p_F"]] <- runif(1, 0, 0.2)
#   result[["sigma_p_F"]] <- runif(1, 0, 0.2)
#   
#   result[["p_G"]] <- rlnorm(md$T * md$I, log(mean(md$y_G, na.rm = T) / mean(md$N0)), 0.5)
#   result[["mu_p_G"]] <- runif(md$I, -4, -2)
#   result[["mu_mu_p_G"]] <- runif(1, -4, -2)
#   result[["sigma_mu_p_G"]] <- runif(1, 0, 0.2)
#   result[["sigma_p_G"]] <- runif(1, 0, 0.2)
#   
#   return(result)
# }
