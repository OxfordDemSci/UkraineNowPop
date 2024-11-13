# Load required libraries
library(sf)
library(here)
library(tidyverse)

# Load the .env file
env <- new.env()
source(here::here('.env'), local=env)

print(paste("Input directory is :", env$in_dir))
print(paste("Output directory is :", env$out_dir))

in_dir <- env$in_dir
out_dir <- env$out_dir
