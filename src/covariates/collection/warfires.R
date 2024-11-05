# Load necessary libraries
library(git2r)
library(tidyverse)
library(fs)

# Load environment variables from .env file
env <- new.env()
source(here::here('.env'), local=env)
out_dir <- env$out_dir

country <- 'UA'

# Clone repository
repo_url = "https://github.com/TheEconomist/the-economist-war-fire-model.git"
repo_dir <- file.path(out_dir, "covariates", "raw", "the-economist-war-fire-model")
git2r::clone(repo_url, repo_dir)
