# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# load libraries
library(runjags)
library(coda)

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
model_name <- 'nmixture_jags'

# soure model-specific config code
source(file.path(srcdir, 'models', paste0(model_name, '_config.R')))



#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
adapt <- 5e3
warmup <- 10e3
samples <- 100e3
inits <- lapply(1:chains, function(id) init_generator(md=md, chain_id=id))

# run MCMC to sample from the posterior distribution of our model, given our data
fit <- run.jags(model = file.path('src', 'modelling', 'nmixture', 'models', paste0(model_name, '.R')),
                monitor = c('rho', 'N'),
                data = md,
                n.chains = chains,
                inits = inits,
                sample = samples,
                adapt = adapt,
                burnin = warmup)

# save fitted model to disk
saveRDS(fit, file.path(outdir, paste0('fit_', model_name, '.rds')))



#---- quick check ----#

draws <- as.mcmc(fit)


# plot(draws[,'rho'])

mean(draws[,'rho'])
mean(md$p_true)


# plot(draws[,'N[1]'])

for(i in 1:md$I){
  print(mean(draws[,paste0('N[',i,']')]))
  print(mean(md$N_true[i]))
}



