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
model_name <- 'nmixture_temporal_long'

# soure model-specific config code
source(file.path(srcdir, 'models', paste0(model_name, '_config.R')))



#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
warmup <- 1e3
samples <- 1e3
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

print(fit, max_rows=1e3)
summary(fit$summary()[['rhat']])

i <- sample(1:md$I, 1)
t <- sample(2:md$T, 1)

mcmc_trace(fit$draws(paste0('N[',t,',',i,']')))
print(mean(fit$draws(paste0('N[',t,',',i,']'))))
print(md$N_true[t,i])

mcmc_trace(fit$draws(paste0('r[',t,',',i,']')))
print(mean(fit$draws(paste0('r[',t,',',i,']'))))

mcmc_trace(fit$draws('mu'))
print(mean(fit$draws('mu')))

mcmc_trace(fit$draws('sigma'))
print(mean(fit$draws('sigma')))


mcmc_trace(fit$draws(paste0('p[',t,',',i,']')))
print(mean(fit$draws(paste0('p[',t,',',i,']'))))
print(mean(md$p_true[t,i]))

mcmc_trace(fit$draws(paste0('delta[',t,']')))
print(mean(fit$draws(paste0('delta[',t,']'))))

mcmc_trace(fit$draws('mu_delta'))
print(mean(fit$draws('mu_delta')))

mcmc_trace(fit$draws('sd_delta'))
print(mean(fit$draws('sd_delta')))

mcmc_trace(fit$draws('alpha'))
print(mean(fit$draws('alpha')))




#---- old code -----#

mcmc_trace(fit$draws('N_sum'))
mean(fit$draws('N_sum'))
md$N_tot


mcmc_trace(fit$draws('p'))
mean(fit$draws('p'))
mean(md$p_true)

for(i in 1:md$I){
  print(mean(fit$draws(paste0('N[',i,']'))))
  print(md$N_true[i])
  print('')
}


