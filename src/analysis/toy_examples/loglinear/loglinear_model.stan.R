#install.packages("rstan")
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

fit_lognormal <- stan(file = "src/analysis/toy_examples/loglinear/loglinear_model.stan",
               data = sim_data_list,
               iter = 500, 
               thin = 1, 
               warmup = 100,
               verbose = FALSE, 
               chains = 3, cores = 3, 
               seed = 26)

print(fit_lognormal)
plot(fit_lognormal)

plot(fit_lognormal, plotfun = "dens", 
     pars = c("Freq_pred[1]"), inc_warmup = F)
