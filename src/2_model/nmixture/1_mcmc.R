# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidyverse)

# check working directory
getwd()

# working directory
wd <- file.path(getwd(), "wd")
dir.create(wd, recursive = T, showWarnings = F)

# directories
srcdir <- file.path("src", "2_model", "nmixture")
indir <- file.path(wd, "in")
outdir <- file.path(wd, "out", "modelling", "nmixture")
dir.create(outdir, showWarnings = F, recursive = T)

#---- load data ----#

# baseline population
codps <- read.csv(file.path(indir, "COD-PS", "population_baseline.csv"))
row.names(codps) <- codps$fb_key

# border crossings
outside_border <- read.csv(file.path(wd, "out", "population_proxy", "crossing_borders", "dat_refugees.csv"))

# master index
idx <- read.csv(file.path(wd, "out", "ua_master_index.csv"))

# social media audiences
idx_F <- read.csv(file.path(wd, "out", "population_proxy", "social_media_audience", "ua_facebook_audience.csv"))
idx_G <- read.csv(file.path(wd, "out", "population_proxy", "social_media_audience", "ua_instagram_audience.csv"))

#---- configure model ----#

# define model name
model_name <- "1_base_model"

dir.create(file.path(outdir, model_name, 'mcmc'), recursive=T, showWarnings=F)

# soure model-specific config code
source(file.path(srcdir, "models", paste0(model_name, "_config.R")))



#---- MCMC for Bayesian model ----#

# MCMC configuration
chains <- 4
warmup <- 2e3
samples <- 4e3
inits <- lapply(1:chains, function(id) init_generator(md = md, chain_id = id))

# compile the stan model
mod <- cmdstan_model(file.path(srcdir, "models", paste0(model_name, ".stan")))

# run MCMC to sample from the posterior distribution of our model, given our data
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
fit$save_object(file = file.path(outdir, model_name, 'mcmc', paste0("fit_", model_name, ".rds")))
