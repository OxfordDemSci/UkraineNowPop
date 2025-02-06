# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# cores for parallel processing
ncores <- 2

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
repo_dir <- env$repo_dir
src_dir <- file.path(repo_dir, "src", "2_model")
in_dir <- env$in_dir
out_dir <- file.path(env$out_dir, "modelling")

# source functions
source(file.path(src_dir, "2_eval_fun.R"))


#---- load data ----#
model_name <- "3_covs_model"

fit <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("fit_", model_name, ".rds")))
md <- readRDS(file.path(out_dir, model_name, "mcmc", paste0("md_", model_name, ".rds")))

# consider subsetting MCMC chains to keep the final X draws


#---- create directories ----#
dir.create(file.path(out_dir, model_name, "eval"), showWarnings = F, recursive = T)
dir.create(file.path(out_dir, model_name, "eval", "time_series_plots"), showWarnings = F, recursive = T)
dir.create(file.path(out_dir, model_name, "eval", "trace_plots"), showWarnings = F, recursive = T)


#---- check total population ----#
if(!model_name %in% c('2_props_model')){
  plot_total_pop_fit(outfile = file.path(out_dir, model_name, "eval", "total_population.jpg"))
}


#---- in-sample posterior predictive check ----#
plot_postpred_fit(dat = "y_F", hat = "F_hat", outfile = file.path(out_dir, model_name, "eval", "postpredict_insamp_facebook.jpg"))
plot_postpred_fit(dat = "y_G", hat = "G_hat", outfile = file.path(out_dir, model_name, "eval", "postpredict_insamp_instagram.jpg"))


#---- time series plots ----#
plot_vars <- list(
  "1_base_model" = c("N", "r", "p_F", "p_G"),
  "2_props_model" = c("N", "pi", "p_F", "p_G"),
  "3_covs_model" = c("N", "r", "p_F", "p_G")
)

plot_time_series(
  fit = fit,
  md = md,
  model_name = model_name,
  plot_vars = plot_vars[[model_name]],
  locs = 1:md$I
)


#---- trace plots ----#

## global parameters
pars <- list(
  "1_base_model" = c("sigma_p_F", "sigma_p_G"),
  "2_props_model" = c(
    "log_sigma_pi", 
    "alpha_p", "phi_p" , "log_sigma_delta_p", "log_sigma_gamma_p", 
    "log_sigma_F", "log_sigma_G"
  ),
  "3_covs_model" = c(
    "alpha_r", "beta_r", "log_sigma_r",
    "alpha_p", "phi_p" , "log_sigma_delta_p", "log_sigma_gamma_p", 
    "log_sigma_F", "log_sigma_G"
  )
)

plot_trace(
  params = pars[[model_name]], 
  outfile = file.path(out_dir, model_name, "eval", "trace_plots", "0_global_parameters.jpg")
)


## location-specific parameters
pars <- list(
  "1_base_model" = c("mu_p_G"),
  "2_props_model" = c("gamma_p"),
  "3_covs_model" = c("gamma_p")
)

for (i in 1:length(pars[[model_name]])) {
  plot_trace(
    params = pars[[model_name]], 
    outfile = file.path(out_dir, model_name, "eval", "trace_plots", paste0("0_location_parameters_", i, ".jpg"))
  )
}


## time-specific parameters
pars <- list(
  "1_base_model" = c("mu_p_F", "N_tot"),
  "2_props_model" = c("delta_p"),
  "3_covs_model" = c("delta_p", "N_tot")
)

for (i in 1:length(pars[[model_name]])) {
  plot_trace(
    params = pars[[model_name]], 
    outfile = file.path(out_dir, model_name, "eval", "trace_plots", paste0("0_time_parameters_", i, ".jpg"))
  )
}


#---- diagnostics that are slow to run ----#

## summary statistics
fit_summary <- fit$summary(.cores = ncores)
print(fit_summary)

not_converged <- which(fit_summary[["rhat"]] > 1.1) # 1.01 is cutoff for publication quality
fit_summary[not_converged, ]

write.csv(fit_summary, file.path(out_dir, model_name, "eval", "fit_summary.csv"), row.names = F)


## LOO cross-validation
loo <- fit$loo("log_lik", cores = ncores)
print(loo)
plot(loo)

# save to disk
saveRDS(loo, file.path(out_dir, model_name, "eval", "loo.rds"))



## trace plots for location-time-specific parameters
plot_all_trace <- FALSE
if(plot_all_trace){
  
  pars <- list(
    "1_base_model" = c("N", "r", "p_F", "p_G"),
    "2_props_model" = c("N", "pi", "p_F", "p_G"),
    "3_covs_model" = c("N", "r", "p_F", "p_G")
  )
  
  for (i in 1:md$I) {
    for (t in 1:md$T) {
      i_name <- gsub(" ", "_", paste(i, t, unique(md$idx$i_name[md$idx$i == i])))
      j <- which(md$tt == t & md$ii == i)
  
      pars_ti <- c('pi')
      pars_model <- pars[[model_name]]
      pars_indexed <- c(paste0(pars_model[!pars_model %in% pars_ti], "[", j, "]"))
      if(any(pars_model %in% pars_ti)){
        pars_indexed <- c(pars_indexed, c(paste0(pars_model[pars_model %in% pars_ti], "[", t, ",", i, "]")))
      }
  
      plot_trace(
        params = pars_indexed, 
        outfile = file.path(out_dir, model_name, "eval", "trace_plots", paste0(i_name, ".jpg"))
      )
    }
  }  
}



#---- old eval code ----#

#---- check observation ratios ----#
# jpeg(
#   filename = file.path(out_dir, model_name, "eval", "observation_ratio.jpg"),
#   height = 720, width = 720
# )

# names_FG_ratio <- paste0("FG_ratio[", 1:md$n_FG, "]")
# FG_ratio <- apply(fit$draws(names_FG_ratio, format = "df"), 2, mean)

# plot(
#   x = md$y_FG_ratio,
#   y = FG_ratio[names_FG_ratio],
#   main = "Observation Ratio Check",
#   xlab = "Observed",
#   ylab = "Predicted"
# )
# abline(0, 1, col = "red")

# dev.off()


# # Check dependence of F to F:G, etc.
# jpeg(
#   filename = file.path(out_dir, model_name, "eval", "observation_ratio_dependency.jpg"),
#   height = 1080, width = 720
# )

# layout(
#   mat = matrix(1:2, ncol = 1, nrow = 2),
#   heights = rep(1, 2)
# )

# x <- y <- c()

# for (i in 1:md$n_F) {
#   ti <- md$ti_F[i]
#   if (ti %in% md$ti_FG) {
#     y <- c(y, md$y_F[i])
#     x <- c(x, md$y_FG_ratio[md$ti_FG == ti])
#   }
# }

# plot(
#   x, y,
#   xlab = "G:F",
#   ylab = "F",
#   main = paste0("Facebook\n", paste("Spearman R =", round(cor(x, y, method = "spearman"), 2)))
# )

# x <- y <- c()

# for (i in 1:md$n_G) {
#   ti <- md$ti_G[i]
#   if (ti %in% md$ti_FG) {
#     y <- c(y, md$y_G[i])
#     x <- c(x, md$y_FG_ratio[md$ti_FG == ti])
#   }
# }

# plot(
#   x, y,
#   xlab = "G:F",
#   ylab = "G",
#   main = paste0("Instagram\n", paste("Spearman R =", round(cor(x, y, method = "spearman"), 2)))
# )

# dev.off()
# rm(x, y, i, ti)
