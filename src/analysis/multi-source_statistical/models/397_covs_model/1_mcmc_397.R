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

dir.create(file.path(here::here(), "UkraineNowPop/wd"), showWarnings = FALSE, recursive = TRUE)
setwd(file.path(here::here(), "UkraineNowPop/wd"))


src_dir <- file.path(env$repo_dir, "src")
out_dir <- file.path(env$out_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

country <- "UA"
model_name <- "397_covs_model"

model_out_dir <- file.path(out_dir, "modelling", model_name, "mcmc")
dir.create(model_out_dir, recursive = TRUE, showWarnings = FALSE)

chains <- 4
warmup <- 500
samples <- 1500

md <- readRDS(file.path(model_out_dir, paste0("md_", model_name, ".rds")))

mod <- cmdstan_model(file.path(src_dir, "analysis", "multi-source_statistical", "models", model_name, paste0(model_name, ".stan")), cpp_options = list(stan_threads = TRUE))

fit <- mod$sample(
    data = md,
    parallel_chains = chains,
    iter_sampling = samples,
    iter_warmup = warmup,
    save_warmup = TRUE,
    seed = md$seed,
    threads_per_chain = 4
    )

fit$save_object(file = file.path(model_out_dir, paste0("fit_", model_name, ".rds")))

