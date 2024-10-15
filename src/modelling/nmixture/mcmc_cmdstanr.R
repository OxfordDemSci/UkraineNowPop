# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)

# check working directory
getwd()

# working directory
wd <- file.path(getwd(), 'wd')
dir.create(wd, recursive=T, showWarnings=F)

# directories
srcdir <- file.path('src', 'modelling', 'nmixture')
outdir <- file.path(wd, 'out', 'modelling', 'nmixture')
dir.create(outdir, showWarnings=F, recursive=T)



#---- configure model ----#

# define model name
model_name <- 'nmixture_base'

# soure model-specific config code
source(file.path(srcdir, 'models', paste0(model_name, '_config.R')))



#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
warmup <- 2e3
samples <- 3e3
inits <- lapply(1:chains, function(id) init_generator(md=md, chain_id=id))

# compile the stan model
mod <- cmdstan_model(file.path(srcdir, 'models', paste0(model_name, '.stan')))

# run MCMC to sample from the posterior distribution of our model, given our data
fit <- mod$sample(data = md,
                  parallel_chains = chains,
                  init = inits,
                  iter_sampling = samples,
                  iter_warmup = warmup,
                  save_warmup = TRUE,
                  seed = md$seed)

# save fitted model to disk
fit$save_object(file=file.path(outdir, paste0('fit_', model_name, '.rds')))



#---- quick checks ----#

print(fit, max_rows=13)

mcmc_trace(fit$draws('lambda_sum'))
mean(fit$draws('lambda_sum'))
md$N_tot


mcmc_trace(fit$draws('rho'))
mean(fit$draws('rho'))
mean(md$p_true)


i <- 1
mcmc_trace(fit$draws(paste0('lambda[',i,']')))
print(mean(fit$draws(paste0('lambda[',i,']'))))
print(md$N_true[i])


for(i in 1:md$I){
  print(mean(fit$draws(paste0('lambda[',i,']'))))
  print(md$N_true[i])
  print('')
}


