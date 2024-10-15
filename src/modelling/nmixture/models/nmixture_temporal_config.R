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

# save to disk
saveRDS(md, file.path(outdir, paste0('md_', model_name, '.rds')))



#---- initial values ----#
init_generator <- function(md=md, chain_id=1){
  result <- list()
  
  N <- matrix(NA, nrow=md$T, ncol=md$I)
  for(t in 1:md$T){
    theta <- apply(md$F[t,,], 1, max) / md$N_tot[t]
    theta <- theta / sum(theta)
    
    N[t,] <- rbinom(md$I, md$N_tot[t], theta)
  }

  result[['N']] <- N
  result[['r']] <- matrix(rnorm(md$T * md$I, 0, 0.5), nrow=md$T, ncol=md$I)
  result[['mu']] <- rnorm(1, 0, 0.1)
  result[['sigma']] <- runif(1, 0, 0.5)
  
  result[['p']] <- matrix(runif(md$T * md$I, 0.05, 0.25), nrow=md$T, ncol=md$I)
  result[['alpha']] <- rnorm(1, 0, 3)
  result[['gamma']] <- rnorm(md$T, 0, 0.5)
  result[['delta']] <- rnorm(md$I, 0, 0.5)
  result[['sd_gamma']] <- runif(1, 0, 0.5)
  result[['sd_delta']] <- runif(1, 0, 0.5)
  
  return(result)
}


