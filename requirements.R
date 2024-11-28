# note: use ./launch.sh to install linux system dependencies required by these packages

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (pkg == "cmdstanr") {
        install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
      } else {
        install.packages(pkg, dependencies = TRUE)
      }
    }
  }
}

pkgs <- c(
  "cmdstanr",
  "posterior",
  "bayesplot",
  "loo",
  "sf",
  "here",
  "tidyverse",
  "tidyquant",
  "jsonlite",
  "httr",
  "DBI",
  "RPostgres",
  "git2r",
  "future.apply"
)

install_if_missing(pkgs)
