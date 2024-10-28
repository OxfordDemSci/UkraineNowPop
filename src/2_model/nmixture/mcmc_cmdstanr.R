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
srcdir <- file.path('src', '2_model', 'nmixture')
indir <- file.path(wd, 'in')
outdir <- file.path(wd, 'out', 'modelling', 'nmixture')
dir.create(outdir, showWarnings=F, recursive=T)

# load data
codps <- read.csv(file.path(indir, 'COD-PS', 'ukr_admpop_adm1_2022.csv'))



#---- configure model ----#

# define model name
model_name <- '5_nmixture_2surveys_ratio'

# soure model-specific config code
source(file.path(srcdir, 'models', paste0(model_name, '_config.R')))



#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
warmup <- 2e3
samples <- 2e3
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



#---- population time series plots ----#

plot_vars <- list(N='N_true', p_F='p_F_true', p_G='p_G_true')

for(p in 1:length(plot_vars)){
  
  y_name <- names(plot_vars)[p]
  y_true_name <- plot_vars[[y_name]]
  
  for(i in 1:md$I){
    
    plot(x = 1:md$T, 
         y = md[[y_true_name]][,i], 
         type = 'l',
         main = paste0('Population ', i),
         ylab = y_true_name,
         xlab = 'time')
    
    lines(x = 1:md$T, 
          y = md[[y_true_name]][,i], 
          col = 'red'
    )
    
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


#---- observed vs predicted plots (long format) ----#

plot_vars <- list(N='N_true', p_F='p_F_true', p_G='p_G_true')

for(p in 1:length(plot_vars)){
  y_name <- names(plot_vars)[p]
  y_true_name <- plot_vars[[y_name]]

  plot(NA, 
       main = y_name,
       xlim = range(md[[y_true_name]]), 
       ylim = range(md[[y_true_name]]), 
       xlab = 'observed', 
       ylab = 'predicted')
  
  y_hat <- apply(fit$draws(paste0(y_name, '[',1:(md$T*md$I),']'), format='df'), 2, mean)
  y_hat_lower <- apply(fit$draws(paste0(y_name, '[',1:(md$T*md$I),']'), format='df'), 2, quantile, probs=c(0.025))
  y_hat_upper <- apply(fit$draws(paste0(y_name, '[',1:(md$T*md$I),']'), format='df'), 2, quantile, probs=c(0.975))
  
  for(t in 2:md$T){
    for(i in 1:md$I){
      
      j <- which(md$tt==t & md$ii==i) 
      
      points(x = md[[y_true_name]][t,i], 
             y = y_hat[j])
      
      arrows(x0 = md[[y_true_name]][t,i], 
             x1 = md[[y_true_name]][t,i],
             y0 = y_hat_lower[j],
             y1 = y_hat_upper[j],
             length = 0)
    }
  }
  abline(0, 1, col='red')
}


#---- check observation ratios ----#

if('y_FG_ratio' %in% names(md) & 'FG_ratio[1]' %in% fit_summary$variable){
  names_FG_ratio <- paste0('FG_ratio[', 1:md$n_FG, ']')
  FG_ratio <- apply(fit$draws(names_FG_ratio, format='df'), 2, mean)
  
  plot(x = md$y_FG_ratio,
       y = FG_ratio[names_FG_ratio]
  )
  abline(0, 1, col='red')
  
  # hist(FG_ratio[names_FG_ratio] / md$y_FG_ratio)
  
} else {

  # ratio_true <- c()
  # ratio_est <- c()
  # 
  # for(t in 1:md$T){
  #   for(i in 1:md$I){
  #     j <- which(md$tt==t & md$ii==i) 
  #     
  #     ratio_true <- c(ratio_true, mean(md$y_G_orig[t,i,], na.rm=T) / mean(md$y_F_orig[t,i,], na.rm=T))
  #     ratio_est <- c(ratio_est, mean(fit$draws(paste0('p_G[',j,']'))) / mean(fit$draws(paste0('p_F[',j,']'))))
  #   }
  # }
  # 
  # ratio_diff <- ratio_est / ratio_true
  # summary(ratio_diff)
  # hist(ratio_diff)
  # which(ratio_diff > 1.05 | ratio_diff < 0.95)
}


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

mcmc_trace(fit$draws('alpha_p_F'))
print(mean(fit$draws('alpha_p_F')))

mcmc_trace(fit$draws('sigma_p_F'))
print(mean(fit$draws('sigma_p_F')))




mcmc_trace(fit$draws(paste0('p_G[',j,']')))
print(mean(fit$draws(paste0('p_G[',j,']'))))
print(mean(md$p_G_true[t,i]))

mcmc_trace(fit$draws('alpha_p_G'))
print(mean(fit$draws('alpha_p_G')))

mcmc_trace(fit$draws('sigma_p_G'))
print(mean(fit$draws('sigma_p_G')))








#---- quick observed vs predicted plot (array format) ----#
plot(NA, 
     xlim = range(md$N_true), 
     ylim = range(md$N_true), 
     xlab = 'observed N', 
     ylab = 'predicted N')

for(t in 2:md$T){
  for(i in 1:md$I){
    points(x = md$N_true[t,i], 
           y = mean(fit$draws(paste0('N[',t,',',i,']'))))
  }
}
abline(0, 1, col='red')


#---- quick checks (array format)----#

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


