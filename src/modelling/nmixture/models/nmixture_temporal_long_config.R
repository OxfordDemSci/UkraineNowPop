#---- load data ----#

# load simulated data
indir <- file.path(wd, 'out', 'simulation')
md <- readRDS(file.path(indir, 'md.rds'))

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
md$p0 <- apply(md$F[1,,], 1, max) / md$N0
# md$p0 <- scale(md$p0)

# prepare to convert to long format
md$F_orig <- md$F

idx <- data.frame(T=rep(1:md$T, each=md$I), I=rep(1:md$I, md$T), ti=1:(md$T*md$I))

F_long <- reshape2::melt(md$F, varnames=c('T', 'I', 'M'))
F_long <- F_long[!is.na(F_long$value),]

F_idx <- F_long[,c('T','I','M')]
F_idx <- merge(F_idx, idx, by=c('T', 'I'))
F_idx <- F_idx[order(F_idx$I, F_idx$T, F_idx$M),]

# long format for N
md$ti_N0 <- idx$ti[idx$T==1]
md$ti_N <- idx$ti[idx$T>1]
md$ti_N_lag <- idx$ti[idx$T>1]-md$I

md$tt <- idx$T
md$ii <- idx$I

# long format for F
md$y_F <- F_long$value
md$ti_F <- F_idx$ti

md$n_F <- length(md$F)

# drop data not needed
md$M_true <- NULL
md$M <- NULL

# save to disk
saveRDS(md, file.path(outdir, paste0('md_', model_name, '.rds')))





#---- initial values ----#
init_generator <- function(md=md, chain_id=1){
  result <- list()
  
  N <- matrix(NA, nrow=md$T, ncol=md$I)
  for(t in 1:md$T){
    theta <- apply(md$F_orig[t,,], 1, max) / md$N_tot[t]
    theta <- theta / sum(theta)
    
    N[t,] <- rbinom(md$I, md$N_tot[t], theta)
  }

  result[['N']] <- reshape2::melt(N, varnames=c('T', 'I'))$value
  result[['r']] <- rlnorm(md$T * md$I, 0, 0.1)
  result[['mu']] <- rnorm(1, 0, 0.1)
  result[['sigma']] <- runif(1, 0, 0.1)
  
  result[['p']] <- runif(md$T * md$I, 0.05, 0.25)
  result[['alpha']] <- rnorm(1, 0, 3)
  result[['delta']] <- rnorm(md$T, 0, 0.5)
  result[['mu_delta']] <- rnorm(1, 0, 1)
  result[['sd_delta']] <- runif(1, 0, 0.5)
  
  return(result)
}


