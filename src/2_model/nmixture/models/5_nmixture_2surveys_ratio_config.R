#---- load data ----#

sim_data <- TRUE

# load simulated data
if(sim_data){
  mddir <- file.path(wd, 'out', 'simulation')
  md <- readRDS(file.path(mddir, 'md.rds'))
} else {
  mddir <- file.path(file.path('K://DemSci', 'projects', '2023_WHO_Ukraine_Population', 'output', 'population_proxy', 'model_data'))
  md <- readRDS(file.path(mddir, 'facebook_md.rds'))
}


# set seed for random number generators
if('seed' %in% names(md)){
  seed <- md$seed
} else {
  seed <- round(runif(1, 1, 1e6))
}
set.seed(seed)



#---- prepare data ----#


## population ##

# total population at each time step
md$N_tot <- apply(md$N_true, 1, sum)

# baseline population
md$N0 <- md$N_true[1,]

# index to reference T x I combinations in vector format
idx <- data.frame(T = rep(1:md$T, each=md$I), 
                  I = rep(1:md$I, md$T), 
                  ti = 1:(md$T*md$I))

# indexing: long format for N
md$ti_N0 <- idx$ti[idx$T==1]
md$ti_N <- idx$ti[idx$T>1]
md$ti_N_lag <- idx$ti[idx$T>1]-md$I

md$tt <- idx$T
md$ii <- idx$I

# long format for population covariates (X_r)
melt_X <- reshape2::melt(md$X, varnames=c('T', 'I', 'K'))
idx_X <- idx
for(k in 1:md$K){
  idx_X <- merge(idx_X, melt_X[melt_X$K==k,c('T','I','value')], by=c('T','I'))
  names(idx_X)[names(idx_X)=='value'] <- paste0('x',k)
}
idx_X <- idx_X[order(idx_X$ti),]
row.names(idx_X) <- idx_X$ti

md$X <- idx_X[,paste0('x', 1:md$K)]



## Facebook ##

# baseline detection
# md$p0_F <- apply(md$y1[1,,], 1, max, na.rm=T) / md$N0

# prepare to convert y to long format
y_long <- reshape2::melt(md$y1, varnames=c('T', 'I', 'M'))
y_long <- y_long[!is.na(y_long$value),]

y_idx <- y_long[,c('T','I','M')]
y_idx <- merge(y_idx, idx, by=c('T', 'I'))
y_idx <- y_idx[order(y_idx$M, y_idx$I, y_idx$T),]

# long format for y
md$y_F <- y_long$value
md$ti_F <- y_idx$ti
md$n_F <- length(md$y_F)

# fill missing data
md$y1[is.na(md$y1)] <- 999999999

# rename
names(md)[names(md)=='p1_true'] <- 'p_F_true'
names(md)[names(md)=='y1'] <- 'y_F_orig'
md$M1 <- NULL


## Instagram ##

# baseline detection
# md$p0_G <- apply(md$y2[1,,], 1, max, na.rm=T) / md$N0

# prepare to convert y to long format
y_long <- reshape2::melt(md$y2, varnames=c('T', 'I', 'M'))
y_long <- y_long[!is.na(y_long$value),]

y_idx <- y_long[,c('T','I','M')]
y_idx <- merge(y_idx, idx, by=c('T', 'I'))
y_idx <- y_idx[order(y_idx$M, y_idx$I, y_idx$T),]

# long format for y
md$y_G <- y_long$value
md$ti_G <- y_idx$ti
md$n_G <- length(md$y_G)

# fill missing data
md$y2[is.na(md$y2)] <- 999999999

# rename
names(md)[names(md)=='p2_true'] <- 'p_G_true'
names(md)[names(md)=='y2'] <- 'y_G_orig'
md$M2 <- NULL


## Facebook:Instagram ratio ###
md$ti_FG <- unique(md$ti_F[which(md$ti_F %in% md$ti_G)])
md$n_FG <- length(md$ti_FG)

md$y_FG_ratio <- c()
for(i in 1:length(md$ti_FG)){
  ti <- md$ti_FG[i]
  G_ti <- mean(md$y_G[which(md$ti_G == ti)], na.rm=T)
  F_ti <- mean(md$y_F[which(md$ti_F == ti)], na.rm=T)
  md$y_FG_ratio[i] <- G_ti / F_ti
}




## save to disk ##
saveRDS(md, file.path(outdir, paste0('md_', model_name, '.rds')))





#---- initial values ----#
init_generator <- function(md=md, chain_id=1){
  result <- list()
  
  N <- matrix(NA, nrow=md$T, ncol=md$I)
  for(t in 1:md$T){
    theta <- apply(md$y_F_orig[t,,], 1, max, na.rm=T) / md$N_tot[t]
    theta <- theta / sum(theta)
    
    N[t,] <- rbinom(md$I, md$N_tot[t], theta)
  }

  result[['N']] <- reshape2::melt(N, varnames=c('T', 'I'))$value
  result[['r']] <- rlnorm(md$T * md$I, 0, 0.1)
  result[['sigma_r']] <- runif(1, 0, 0.5)
  result[['mu_r']] <- rnorm(md$T * md$I, 0, 0.1)
  result[['alpha_r']] <- rnorm(1, 0, 3)
  result[['beta_r']] <- rnorm(md$K, 0, 1)

  result[['mu_p_F']] <- rnorm(md$T * md$I, log(mean(md$p_F)), 1)
  result[['p_F']] <- rlnorm(md$T * md$I, log(mean(md$p_F)), 0.5)
  result[['alpha_p_F']] <- rnorm(1, 1, 1)
  result[['sigma_p_F']] <- runif(1, 0, 0.05)
  result[['delta_p_F']] <- rnorm(md$T, 1, 0.5)
  result[['mu_delta_p_F']] <- rnorm(1, 1, 0.5)
  result[['sd_delta_p_F']] <- runif(1, 0, 0.5)

  result[['mu_p_G']] <- rnorm(md$T * md$I, log(mean(md$p_G)), 1)
  result[['p_G']] <- rlnorm(md$T * md$I, log(mean(md$p_G)), 0.5)
  result[['alpha_p_G']] <- rnorm(1, 1, 1)
  result[['sigma_p_G']] <- runif(1, 0, 0.05)
  result[['delta_p_G']] <- rnorm(md$T, 1, 0.5)
  result[['mu_delta_p_G']] <- rnorm(1, 1, 0.5)
  result[['sd_delta_p_G']] <- runif(1, 0, 0.5)

  return(result)
}

