# cleanup
rm(list = ls())
gc()

# load libraries
library(cmdstanr)
library(posterior)
library(bayesplot)
library(here)
library(parallel)
library(dplyr)
library(lubridate)
library(reshape2)

env <- new.env()
source(here::here(".env"), local = env)

dir.create(file.path(here::here(), "wd"), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(here::here(), "wd"))

repo_dir <- env$repo_dir
data_dir <- file.path(env$repo_dir, "data")
src_dir <- file.path(repo_dir, "src", "2_model")
in_dir <- env$in_dir
out_dir <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

country <- "UA"
model_name <- "432_props_covs_model"

source(file.path(src_dir, "models", paste0(model_name, "_config.R")))

model_out_dir <- file.path(out_dir, "modelling", model_name, "mcmc")
dir.create(model_out_dir, recursive = TRUE, showWarnings = FALSE)


idx = read.csv(file.path(out_dir, paste0(tolower(country), "_master_index", ".csv")))
idx_F = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_facebook_audience", ".csv")))
idx_G = read.csv(file.path(out_dir, "population_proxy", "social_media_audience", paste0(tolower(country), "_instagram_audience", ".csv")))
covs = read.csv(file.path(out_dir, "covariates", "final", "ua_covariates_oblast.csv"))
codps = read.csv(file.path(data_dir, "cod-ps", "population_baseline.csv"))
outside_border = read.csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))
last_date = "2024-05-14"
process_drop_locations = c()
observation_drop_locations = c(3782,3788,3791,3797)
process_cov_select = read.csv(file.path(in_dir, "ua_r_covariates_oblast_selection.csv"))
observation_cov_select = read.csv(file.path(in_dir, "ua_p_covariates_oblast_selection.csv"))


chains <- 4
warmup <- 800
samples <- 800

mod <- cmdstan_model(file.path(src_dir, "models", paste0(model_name, ".stan")))
#inits <- lapply(1:chains, function(id) init_generator(md = md, chain_id = id))

fit <- mod$sample(
    data = md,
    parallel_chains = chains,
    #init = inits,
    iter_sampling = samples,
    iter_warmup = warmup,
    save_warmup = TRUE,
    seed = md$seed
  )
  
fit$save_object(file = file.path(model_out_dir, paste0("fit_", model_name, ".rds")))
