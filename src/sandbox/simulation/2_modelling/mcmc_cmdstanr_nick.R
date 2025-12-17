# cleanup
rm(list=ls()); gc(); cat("\014"); try(dev.off(), silent=T)

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)

# check working directory
getwd()

# working directory
wd <- file.path(getwd(), 'wd')
dir.create(wd, recursive=T, showWarnings=F)

# directories
srcdir <- file.path('src', 'simulation', '2_modelling')
outdir <- file.path(wd, 'out', 'simulation', 'nmixture')
dir.create(outdir, showWarnings=F, recursive=T)


#---- configure model ----#

# define model name
model_name <- '9_prop_model_multinom'

# soure model-specific config code
source(file.path(srcdir, 'models', paste0(model_name, '_config.R')))

#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
warmup <- 2e2
samples <- 2e2
inits <- lapply(1:chains, function(id) init_generator(md=md, chain_id=id))

# compile the stan model
mod <- cmdstan_model(file.path(srcdir, 'models', paste0(model_name, '.stan')))

# run MCMC to sample from the posterior distribution of our model, given our data
fit <- mod$sample(data = md,
                  parallel_chains = chains,
                  init = inits,
                  iter_sampling = samples,
                  iter_warmup = warmup,
                  save_warmup = FALSE,
                  seed = md$seed)

# save fitted model to disk
fit$save_object(file=file.path(outdir, paste0('fit_', model_name,'-',Sys.time(), '.rds')))

fit <- readRDS(tail(Sys.glob(file.path(outdir, paste0('fit_', model_name,'-202*'))),1))
# fit <- readRDS(file.path(outdir, paste0('fit_', model_name, '.rds')))
# md  <- readRDS(file.path(outdir, paste0('md_', model_name, '.rds')))

# plot estimates
sims <- fit$draws(format='df')

hist(sims$log_nu)
hist(sims$`logit_p[1,1]`)
hist(sims$`logit_p[2,1]`)
hist(exp(sims$mu_p_GF+exp(2*sims$log_sigma_p_GF)/2))
# hist(sims$sigma_p_GF)
# hist(sims$sigma_p_F)
# hist(sims$sigma_p)
hist(exp(sims$mu_p_GF))
hist(sims$mu_p_GF)
hist(sims$mu_p_F)
hist(sims$log_sigma_p_GF)
hist(sims$log_sigma_p_F)
hist(exp(sims$log_nu_p_F))
hist(sims$log_sigma_p_F_t)
hist(sims$log_sigma_p_F_i)
hist(sims$log_sigma_p_GF_i)
hist(sims$log_sigma_p)
hist(sims$log_kappa_F)
hist(sims$log_kappa_G)
hist(sims$disp_F)
hist(sims$disp_G)
hist(sims$ar_tot)
hist(sims$mu_tot)
hist(sims$log_sigma_tot)
hist(sims$ar_p_F)
hist(sims$`mu_p_F[1]`)
hist(sims$`mu_p_F[2]`)
hist(sims$`mu_p_F[3]`)

vioplot::vioplot(sims %>% select(starts_with('mu_p_F_i[')))
vioplot::vioplot(sims %>% select(starts_with('mu_p_GF_i[')))

# look at y_FG_ratio by site
hist(md$y_FG_ratio)

fg.hat <- apply(
  # sims %>% select(starts_with('p_GF[')),
  (sims %>% select(starts_with('p_G[')))/(sims %>% select(starts_with('p_F['))),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(md$y_FG_ratio,fg.hat[1,md$ti_FG],pch='.',ylim=range(md$y_FG_ratio))
arrows(x0 = md$y_FG_ratio, 
       x1 = md$y_FG_ratio,
       y0 = fg.hat[2,md$ti_FG],
       y1 = fg.hat[3,md$ti_FG],
       length = 0,
       lty = 2)
lines(md$y_FG_ratio,md$y_FG_ratio,col='red')
mean((md$y_FG_ratio <= fg.hat[3,md$ti_FG])&(md$y_FG_ratio >= fg.hat[2,md$ti_FG]))

fg.hat <- apply(
  sims %>% select(starts_with('FG_ratio[')),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(md$y_FG_ratio,fg.hat[1,],pch='.')
arrows(x0 = md$y_FG_ratio, 
       x1 = md$y_FG_ratio,
       y0 = fg.hat[2,],
       y1 = fg.hat[3,],
       length = 0,
       lty = 2)
lines(md$y_FG_ratio,md$y_FG_ratio,col='red')
mean((md$y_FG_ratio <= fg.hat[3,])&(md$y_FG_ratio >= fg.hat[2,]))

f.hat <- apply(
  sims %>% select(starts_with('F_hat[')),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(log(md$y_F),log(f.hat[1,]),pch='.',ylim=range(log(f.hat)))
arrows(x0 = log(md$y_F), 
       x1 = log(md$y_F),
       y0 = log(f.hat[2,]),
       y1 = log(f.hat[3,]),
       length = 0,
       lty = 2)
lines(log(md$y_F),log(md$y_F),col='red')
mean((md$y_F <= f.hat[3,])&(md$y_F >= f.hat[2,]))
# points(log(md$y_F),log(f.hat[2,]),col='red')
# points(md$y_F,f.hat[3,],col='blue')
# points(md$y_F,f.hat[2,],col='red')
# points(md$y_F,f.hat[3,],col='blue')

g.hat <- apply(
  sims %>% select(starts_with('G_hat[')),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(log(md$y_G),log(g.hat[1,]),pch='.',ylim=range(log(g.hat)))
arrows(x0 = log(md$y_G), 
       x1 = log(md$y_G),
       y0 = log(g.hat[2,]),
       y1 = log(g.hat[3,]),
       length = 0,
       lty = 2)
lines(log(md$y_G),log(md$y_G),col='red')
mean((md$y_G <= g.hat[3,])&(md$y_G >= g.hat[2,]))

loc <- 8
p_F.hat <- apply(
  sims %>% select(starts_with('p_F[')),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
p_G.hat <- apply(
  sims %>% select(starts_with('p_G[')),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(
  p_F.hat[1,md$ii==loc],type='l',ylim=range(p_F.hat[,md$ii==loc],p_G.hat[,md$ii==loc]),
  xlab = 'Week', ylab = 'p'
  )
lines(p_F.hat[2,md$ii==loc])
lines(p_F.hat[3,md$ii==loc])
lines(p_G.hat[1,md$ii==loc],col='red')
lines(p_G.hat[2,md$ii==loc],col='red')
lines(p_G.hat[3,md$ii==loc],col='red')


loc <- 1
N.hat <- apply(
  sims %>% select(starts_with('N[')),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(
  N.hat[1,md$ii==loc],type='l',ylim=range(N.hat[,md$ii==loc]),
  xlab = 'Week', ylab = 'N'
)
lines(N.hat[2,md$ii==loc])
lines(N.hat[3,md$ii==loc])
loc <- loc + 1

loc <- 1
p.hat <- apply(
  sims %>% select(starts_with('p[')) %>% select(ends_with(paste0(',',loc,']'))), 
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(
  p.hat[1,],type='l',ylim=range(p.hat),
  xlab = 'Week', ylab = 'p'
)
lines(p.hat[2,])
lines(p.hat[3,])
loc <- loc + 1

N_tot.hat <- apply(
  sims %>% select(starts_with('y_N_tot_hat[')),
  2, function(x){quantile(x,probs=c(0.5,0.025,0.975))})
plot(
  N_tot.hat[1,],type='l',ylim=range(N_tot.hat),
  xlab = 'Week', ylab = 'N'
)
lines(N_tot.hat[2,])
lines(N_tot.hat[3,])
points(md$y_N_tot)

plot(md$y_N_tot)
plot(log(md$y_N_tot))
plot(diff(md$y_N_tot))
plot(diff(log(md$y_N_tot)))

loc <- 7
# plot(md$y_F/(rep(md$y_N_tot,each=md$I)[md$ti_F]))
plot(md$y_F[which(md$ti_F %in% which(md$ii==loc))],ylim=c(0,max(md$y_F[which(md$ti_F %in% which(md$ii==loc))])))
points(md$y_G[which(md$ti_G %in% which(md$ii==loc))])
loc <- loc+1

md$N0

for(loc in 1:md$I){
  print(range(md$y_F[which(md$ti_F %in% which(md$ii==loc))]/md$N0[loc]))
  # print((md$y_F[which(md$ti_F %in% which(md$ii==loc))])[1]/md$N0[loc])    
}

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


