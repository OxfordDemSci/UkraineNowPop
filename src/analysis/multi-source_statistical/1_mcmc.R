# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
repo_dir <- env$repo_dir
data_dir <- file.path(env$repo_dir, "data")
src_dir <- file.path(repo_dir, "src", "analysis", "multi-source_statistical")

in_dir <- env$in_dir
out_dir <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = F, recursive = T)


#---- configure model data ----#

# define model name
model_name <- "4_props_covs_model"

# source model-specific config functions
source(file.path(src_dir, "models", paste0(model_name, "_config.R")))

# create model output directory
dir.create(file.path(out_dir, "modelling", model_name, "mcmc"), recursive = T, showWarnings = F)

# load data
if (file.exists(file.path(in_dir, "cod-ps_2023", "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv"))) {
  codps_N1 <- read.csv(file.path(in_dir, "cod-ps_2023", "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv"))
} else {
  codps_N1 <- read.csv(file.path(in_dir, "cod-ps_2023", "UKR_ADM2_POP_2023_sim.csv"))
}

# create model data
md <- model_data(
  idx = read.csv(file.path(out_dir, "ua_master_index.csv")),
  idx_F = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", "ua_facebook_audience.csv")),
  idx_G = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", "ua_instagram_audience.csv")),
  covs = read.csv(file.path(out_dir, "covariates", "final", "ua_covariates_oblast.csv")),
  codps = read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv")),
  codps_N1 = codps_N1,
  date_N1 = "2023-07-01",
  confidence_N1 = 0.1, # 95% chance true pop is within confidence_N1*100 percent of codps_N1 estimate
  outside_border = read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv")),
  last_date = "2023-08-31", # max(idx$t_name)
  process_drop_locations = c(), # 3782=Donetska, 3791=Luhanksa, 3788=Crimea, 3797=Sevastopol
  observation_drop_locations = c(3782, 3788, 3791, 3797),
  process_cov_select = read.csv(file.path(in_dir, "ua_r_covariates_oblast_selection.csv")),
  observation_cov_select = read.csv(file.path(in_dir, "ua_p_covariates_oblast_selection.csv"))
)

# idx = read.csv(file.path(out_dir, "ua_master_index.csv"))
# idx_F = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", "ua_facebook_audience.csv"))
# idx_G = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", "ua_instagram_audience.csv"))
# covs = read.csv(file.path(out_dir, "covariates", "final", "ua_covariates_oblast.csv"))
# codps = read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv"))
# codps_N1 = codps_N1
# date_N1 = "2023-07-01"
# confidence_N1 = 0.1 # 95% chance true pop is within confidence_N1*100 percent of codps_N1 estimate
# outside_border = read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))
# last_date = "2023-08-31" # max(idx$t_name)
# process_drop_locations = c() # 3782=Donetska, 3791=Luhanksa, 3788=Crimea, 3797=Sevastopol
# observation_drop_locations = c(3782, 3788, 3791, 3797)
# process_cov_select = read.csv(file.path(in_dir, "ua_r_covariates_oblast_selection.csv"))
# observation_cov_select = read.csv(file.path(in_dir, "ua_p_covariates_oblast_selection.csv"))

# save model data to disk
saveRDS(md, file.path(out_dir, "modelling", model_name, "mcmc", paste0("md_", model_name, ".rds")))



#---- fit model ----#

# MCMC configuration
chains <- 4
warmup <- 1e3
samples <- 1e3
inits <- lapply(1:chains, function(id) init_generator(md = md, chain_id = id))

# compile the stan model
mod <- cmdstan_model(file.path(src_dir, "models", paste0(model_name, ".stan")))

# run MCMC
fit <- mod$sample(
  data = md,
  parallel_chains = chains,
  init = inits,
  iter_sampling = samples,
  iter_warmup = warmup,
  save_warmup = TRUE,
  seed = md$seed
)

# save fitted model to disk
fit$save_object(file = file.path(out_dir, "modelling", model_name, "mcmc", paste0("fit_", model_name, ".rds")))
