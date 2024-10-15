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

md$N_true <- md$N_true
md$p_true <- md$p_true

saveRDS(md, file.path(outdir, paste0('md_', model_name, '.rds')))


#---- initial values ----#
init_generator <- function(md=md, chain_id=1){
  result <- list()
  
  prop <- apply(md$F, 1, max) / md$N_tot
  prop <- prop / sum(prop)
  
  lambda <- md$N_tot * prop
  N <- rpois(md$I, lambda)
  
  p <- runif(1, 0.05, 0.5)
  
  result[['p']] <- p
  result[['lambda']] <- lambda
  result[['N']] <- N
  
  result[['.RNG.seed']] <- seed
  result[['.RNG.name']] <- 'base::Super-Duper'
  
  return(result)
}


