# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)
library(here)

# load environment
env <- new.env()
source(here::here(".env"), local = env)

# working directory
dir.create(file.path(here::here(), "wd"), showWarnings = F, recursive = T)
setwd(file.path(here::here(), "wd"))

# directories
repo_dir <- env$repo_dir
data_dir <- file.path(env$repo_dir, 'data')
src_dir <- file.path(repo_dir, "src", "2_model")


in_dir <- env$in_dir
out_dir <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = F, recursive = T)



#---- load data ----#

# baseline population
codps <- read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv"))
row.names(codps) <- codps$fb_key

# border crossings
#outside_border <- read.csv(file.path(data_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))
outside_border <- read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))

# master index
idx <- read.csv(file.path(out_dir, "ua_master_index.csv"))

# social media audiences
idx_F <- read.csv(file.path(out_dir, "population_proxy", "social_media_audience", "ua_facebook_audience.csv"))
idx_G <- read.csv(file.path(out_dir, "population_proxy", "social_media_audience", "ua_instagram_audience.csv"))



#---- configure model data ----#

# define model name
model_name <- "2_age_sex_model"

dir.create(file.path(out_dir, "modelling", model_name, "mcmc"), recursive = T, showWarnings = F)

# soure model-specific config code
source(file.path(src_dir, "models", paste0(model_name, "_config.R")))

# save model data to disk
saveRDS(md, file.path(out_dir, "modelling", model_name, "mcmc", paste0("md_", model_name, ".rds")))



#---- fit model ----#

# MCMC configuration
chains <- 4
warmup <- 5e3
samples <- 5e3
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
