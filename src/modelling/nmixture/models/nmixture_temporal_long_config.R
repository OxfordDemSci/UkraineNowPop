#---- load data ----#

# load simulated data
indir <- file.path(wd, 'out', 'simulation')
md <- readRDS(file.path(indir, 'md.rds'))

# indir <- file.path(file.path('K://DemSci', 'projects', '2023_WHO_Ukraine_Population', 'output', 'population_proxy', 'model_data'))
# md <- readRDS(file.path(indir, 'facebook_md.rds'))

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
md$p0 <- apply(md$y[1,,], 1, max) / md$N0
# md$p0 <- scale(md$p0)

# prepare to convert to long format
idx <- data.frame(T=rep(1:md$T, each=md$I), I=rep(1:md$I, md$T), ti=1:(md$T*md$I))

y_long <- reshape2::melt(md$y, varnames=c('T', 'I', 'M'))
y_long <- y_long[!is.na(y_long$value),]

y_idx <- y_long[,c('T','I','M')]
y_idx <- merge(y_idx, idx, by=c('T', 'I'))
y_idx <- y_idx[order(y_idx$I, y_idx$T, y_idx$M),]

# long format for N
md$ti_N0 <- idx$ti[idx$T==1]
md$ti_N <- idx$ti[idx$T>1]
md$ti_N_lag <- idx$ti[idx$T>1]-md$I

md$tt <- idx$T
md$ii <- idx$I

# long format for y
md$y_F <- y_long$value
md$ti_F <- y_idx$ti

md$n_F <- length(md$y_F)


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
  result[['mu']] <- rnorm(1, 0, 0.1)
  result[['sigma']] <- runif(1, 0, 0.1)
  
  result[['p']] <- runif(md$T * md$I, 0.05, 0.25)
  result[['alpha']] <- rnorm(1, 0, 3)
  result[['delta']] <- rnorm(md$T, 0, 0.5)
  result[['mu_delta']] <- rnorm(1, 0, 1)
  result[['sd_delta']] <- runif(1, 0, 0.5)
  
  return(result)
}

