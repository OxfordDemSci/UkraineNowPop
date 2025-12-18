# This script queries a UNHCR API endpoint to retrieve daily counts of border crossings at 
# the Ukrainian border.

# cleanup
rm(list = ls())
gc()
cat("\014")
try(dev.off(), silent = T)

# setup environment
env <- new.env()
source(here::here(".env"), local = env)

# directories
out_dir <- file.path(env$out_dir, "population_proxy", "crossing_borders")
dir.create(out_dir, showWarnings = F, recursive = T)

# LAST DATE: 2025-02-28. Now filled manually from pdf

# outward border crossings
out_refugee_file <- file.path(out_dir, "refugees_out_daily.csv")

download.file(
  url = "https://data.unhcr.org/population/get/timeseries?export=csv&widget_id=324084&sv_id=54&population_group=5460&frequency=day&fromDate=1900-01-01",
  destfile = out_refugee_file
)

dat_refugees_out <- read.csv(out_refugee_file,
  stringsAsFactors = F,
  skip = 1,
  skipNul = T,
  sep = ";"
)
names(dat_refugees_out) <- c("date", "individuals")



# inward border crossings
in_refugee_file <- file.path(out_dir, "refugees_in_daily.csv")

download.file(
  url = "https://data.unhcr.org/population/get/timeseries?export=csv&widget_id=324085&sv_id=54&population_group=5472&frequency=day&fromDate=1900-01-01",
  destfile = in_refugee_file
)

dat_refugees_in <- read.csv(in_refugee_file,
  stringsAsFactors = F,
  skip = 1,
  skipNul = T,
  sep = ";"
)
names(dat_refugees_in) <- c("date", "individuals")

# net border crossings
dat_refugees <- dat_refugees_out
dat_refugees$individuals <- dat_refugees_out$individuals - dat_refugees_in$individuals

# remove NAs
dat_refugees <- dat_refugees[!is.na(dat_refugees$individuals), ]

# save to disk
write.csv(dat_refugees, file.path(out_dir, "dat_refugees.csv"), row.names = F)

