#---- load data ----#

sim_data <- FALSE

# load simulated data
if(sim_data){
  mddir <- file.path(wd, 'out', 'simulation')
  md <- readRDS(file.path(indir, 'md.rds'))
} else {
  mddir <- file.path(file.path('K://DemSci', 'projects', '2023_WHO_Ukraine_Population', 'output', 'population_proxy', 'model_data'))
  md <- readRDS(file.path(indir, 'facebook_md.rds'))
}


# set seed for random number generators
if('seed' %in% names(md)){
  seed <- md$seed
} else {
  seed <- round(runif(1, 1, 1e6))
}
set.seed(seed)



#---- prepare data ----#

# total population at each time step
md$N_tot <- apply(md$N_true, 1, sum)

# baseline population
md$N0 <- md$N_true[1,]

# baseline detection
md$p0 <- apply(md$y[1,,], 1, max, na.rm=T) / md$N0

# index to reference T x I combinations in vector format
idx <- data.frame(T = rep(1:md$T, each=md$I), 
                  I = rep(1:md$I, md$T), 
                  ti = 1:(md$T*md$I))

# prepare to convert y to long format
y_long <- reshape2::melt(md$y, varnames=c('T', 'I', 'M'))
y_long <- y_long[!is.na(y_long$value),]

y_idx <- y_long[,c('T','I','M')]
y_idx <- merge(y_idx, idx, by=c('T', 'I'))
y_idx <- y_idx[order(y_idx$M, y_idx$I, y_idx$T),]

# indexing: long format for N
md$ti_N0 <- idx$ti[idx$T==1]
md$ti_N <- idx$ti[idx$T>1]
md$ti_N_lag <- idx$ti[idx$T>1]-md$I

md$tt <- idx$T
md$ii <- idx$I

# long format for y
md$y_F <- y_long$value
md$ti_F <- y_idx$ti
md$n_F <- length(md$y_F)

# long format for X_r
melt_X <- reshape2::melt(md$X, varnames=c('T', 'I', 'K'))
idx_X <- idx
for(k in 1:md$K){
  idx_X <- merge(idx_X, melt_X[melt_X$K==k,c('T','I','value')], by=c('T','I'))
  names(idx_X)[names(idx_X)=='value'] <- paste0('x',k)
}
idx_X <- idx_X[order(idx_X$ti),]
row.names(idx_X) <- idx_X$ti

md$X <- idx_X[,paste0('x', 1:md$K)]

# fill missing data
md$y[is.na(md$y)] <- 999999999

# rename
names(md)[names(md)=='y'] <- 'y_F_orig'

# save to disk
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
  result[['sigma_r']] <- runif(1, 0, 0.1)
  # result[['mu_r']] <- rnorm(md$T * md$I, 0, 0.1)
  result[['mu_r']] <- rnorm(1, 0, 0.1)
  result[['alpha_r']] <- rnorm(1, 0, 3)
  result[['beta_r']] <- rnorm(md$K, 0, 1)

  result[['p']] <- runif(md$T * md$I, 0.05, 0.25)
  result[['alpha_p']] <- rnorm(1, 0, 3)
  result[['delta_p']] <- rnorm(md$T, 0, 0.5)
  result[['mu_delta_p']] <- rnorm(1, 0, 1)
  result[['sd_delta_p']] <- runif(1, 0, 0.5)
  
  return(result)
}

