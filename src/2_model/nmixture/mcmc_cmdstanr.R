# cleanup
rm(list=ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)

# check working directory
getwd()

# working directory
wd <- file.path(getwd(), 'wd')
dir.create(wd, recursive=T, showWarnings=F)

# directories
srcdir <- file.path('src', '2_model', 'nmixture')
indir <- file.path(wd, 'in')
outdir <- file.path(wd, 'out', 'modelling', 'nmixture')
dir.create(outdir, showWarnings=F, recursive=T)

#---- load data ----#

# baseline population
codps <- read.csv(file.path(indir, 'COD-PS', 'population_baseline.csv'))
row.names(codps) <- codps$fb_key

# border crossings
outside_border <- read.csv(file.path(wd, 'out', 'population_proxy', 'crossing_borders', 'dat_refugees.csv'))

# master index
idx <- read.csv(file.path(wd, 'out', 'ua_master_index.csv'))

# social media audiences
idx_F <- read.csv(file.path(wd, 'out', 'population_proxy', 'social_media_audience', 'ua_facebook_audience.csv'))
idx_G <- read.csv(file.path(wd, 'out', 'population_proxy', 'social_media_audience', 'ua_instagram_audience.csv'))

#---- configure model ----#

# define model name
model_name <- '1_base_model'

# soure model-specific config code
source(file.path(srcdir, 'models', paste0(model_name, '_config.R')))



#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
warmup <- 2e3
samples <- 4e3
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


# fit <- readRDS(file.path(outdir, paste0('fit_', model_name, '.rds')))
# md  <- readRDS(file.path(outdir, paste0('md_', model_name, '.rds')))



# quick diagnostics
fit_summary <- fit$summary()
print(fit_summary)

not_converged <- which(fit_summary[['rhat']] > 1.1) # 1.01 is cutoff for publication quality
fit_summary[not_converged,]


#---- time series plots ----#

plot_vars <- c('N', 'p_F', 'p_G')

for(y_name in plot_vars){
  
  dat <- fit$draws(y_name, format='df')
  
  for(i in 1:md$I){
    
    i_dat <- dat[paste0(y_name,'[',which(md$ii == i),']')]
    i_name <- paste0(codps$ADM1_EN[codps$fb_key==md$idx$i_key[i]],
                     ' (', i, ')')
    
    plot(NA,
         main = i_name,
         ylab = y_name,
         xlab = 'time',
         xlim = c(1, md$T), 
         ylim = c(min(apply(i_dat, 2, quantile, probs=c(0.025))),
                  max(apply(i_dat, 2, quantile, probs=c(0.975)))))

    for(t in 1:md$T){
      j <- which(md$tt==t & md$ii==i) 
      
      points(x = t, 
             y = mean(fit$draws(paste0(y_name,'[',j,']'))))
      
      arrows(x0 = t, 
             x1 = t,
             y0 = quantile(fit$draws(paste0(y_name,'[',j,']')), probs=c(0.025)),
             y1 = quantile(fit$draws(paste0(y_name,'[',j,']')), probs=c(0.975)),
             length = 0,
             lty = 2)
    }
  }
}



#---- check total population ----#
names_N_tot <- paste0('N_tot[', 1:md$T, ']')
N_tot <- apply(fit$draws(names_N_tot, format='df'), 2, mean)

plot(x = md$y_N_tot,
     y = N_tot[names_N_tot]
)
abline(0, 1, col='red')



#---- check observation ratios ----#
names_FG_ratio <- paste0('FG_ratio[', 1:md$n_FG, ']')
FG_ratio <- apply(fit$draws(names_FG_ratio, format='df'), 2, mean)

plot(x = md$y_FG_ratio,
     y = FG_ratio[names_FG_ratio]
)
abline(0, 1, col='red')
  

#---- trace plot checks (long format) ----#

i <- sample(1:md$I, 1)
t <- sample(2:md$T, 1)

j <- which(md$tt==t & md$ii==i)


mcmc_trace(fit$draws(paste0('N[',j,']')))
print(mean(fit$draws(paste0('N[',j,']'))))
print(md$N_true[t,i])

mcmc_trace(fit$draws(paste0('r[',j,']')))
print(mean(fit$draws(paste0('r[',j,']'))))
print(exp(md$r_true[t,i]))

mcmc_trace(fit$draws('sigma_r'))
print(mean(fit$draws('sigma_r')))

mcmc_trace(fit$draws('alpha_r'))
print(mean(fit$draws('alpha_r')))

mcmc_trace(fit$draws('beta_r'))
md$beta_r_true

for(k in 1:md$K){
  print(mcmc_trace(fit$draws(paste0('beta_r[',k,']'))))
  print(mean(fit$draws(paste0('beta_r[',k,']'))))
  print(md$beta_r_true[paste0('beta', k)])
}



mcmc_trace(fit$draws(paste0('p_F[',j,']')))
print(mean(fit$draws(paste0('p_F[',j,']'))))
print(mean(md$p_F_true[t,i]))

mcmc_trace(fit$draws('mu_p_F'))
print(mean(fit$draws('mu_p_F')))

mcmc_trace(fit$draws('sigma_p_F'))
print(mean(fit$draws('sigma_p_F')))




mcmc_trace(fit$draws(paste0('p_G[',j,']')))
print(mean(fit$draws(paste0('p_G[',j,']'))))
print(mean(md$p_G_true[t,i]))

mcmc_trace(fit$draws('mu_p_G'))
print(mean(fit$draws('mu_p_G')))

mcmc_trace(fit$draws('sigma_p_G'))
print(mean(fit$draws('sigma_p_G')))








