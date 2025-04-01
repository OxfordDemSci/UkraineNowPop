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



#---- prepare simulated data ----#


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
  
  result[["p_F"]] <- rlnorm(md$T * md$I, log(mean(md$y_F, na.rm = T) / mean(md$N0)), 0.5)
  # result[["mu_p_F"]] <- runif(md$I, -4, -2)
  result[["mu_p_F"]] <- runif(1, -4, -2)  
  result[["mu_mu_p_F"]] <- runif(1, -4, -2)
  result[["sigma_mu_p_F"]] <- runif(1, 0, 0.2)
  result[["sigma_p_F"]] <- runif(1, 0, 0.2)
  
  result[["p_G"]] <- rlnorm(md$T * md$I, log(mean(md$y_G, na.rm = T) / mean(md$N0)), 0.5)
  result[['p_GF']] <- runif(md$T * md$I)
  result[['mu_p_GF']] <- runif(1)
  result[["mu_mu_p_G"]] <- runif(1, -4, -2)
  result[["sigma_mu_p_G"]] <- runif(1, 0, 0.2)
  result[['sigma_p_GF']] <- runif(1, 0, 0.05)
  
  result[['logit_p']] <- 
    matrix(
      rnorm((md$T-1)*(md$I-1),
            rep(log((md$N0/sum(md$N0))[1:(md$I-1)] / 
                      (1-sum((md$N0/sum(md$N0))[1:(md$I-1)]))),times=md$T-1),
            0.25),
      nrow=md$T-1,ncol=md$I-1)
  result[['sigma_p']] <- runif(1,0,2)
  result[['ar_tot']] <- runif(1,0.5,0.95)  
  result[['mu_tot']] <- rnorm(1,log(tail(md$y_N_tot,1)),sd(log(md$y_N_tot)))
  
  
  return(result)
}
