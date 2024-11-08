# note: use ./launch.sh to install linux system dependencies required by these packages

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

pkgs <- c("sf", "here", "tidyverse", "jsonlite", "httr", "DBI", "RPostgres", "cmdstanr", "posterior", "bayesplot")
install.packages(pkgs)
