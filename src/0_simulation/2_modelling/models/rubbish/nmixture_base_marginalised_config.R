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

md$T <- NULL
md$F <- md$F[1,,]

md$N_tot <- sum(md$N_true[1,])

# save to disk
saveRDS(md, file.path(outdir, paste0('md_', model_name, '.rds')))



#---- initial values ----#
init_generator <- function(md=md, chain_id=1){
  result <- list()
  
  theta <- apply(md$F, 1, max) / md$N_tot
  theta <- theta / sum(theta)
  
  lambda <- md$N_tot * theta
  N <- rpois(md$I, lambda)
  
  rho <- runif(1, 0.05, 0.25)
  
  result[['rho']] <- rho
  result[['lambda']] <- lambda
  result[['N']] <- N
  
  return(result)
}


