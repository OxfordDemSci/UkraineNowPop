# note: use ./launch.sh to install linux system dependencies required by these packages

install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("\nInstalling package:", pkg, "\n"))
      if (pkg == "cmdstanr") {
        install.packages(
          "cmdstanr",
          repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
        )
      } else {
        install.packages(pkg, dependencies = TRUE)
      }
    }
  }
}

pkgs <- c(
  "tidyverse",
  "tidyquant",
  "data.table",
  "sf",
  "tmap",
  "cmdstanr",
  "posterior",
  "bayesplot",
  "jsonlite",
  "RPostgres",
  "here",
  "git2r",
  "future.apply",
  "paletteer",
  "corrplot",
  "kableExtra"
)

install_if_missing(pkgs)
