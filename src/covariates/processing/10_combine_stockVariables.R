source("R_helpers/generic.R")

dir.create(file.path(out_dir, "covariates", "final"), showWarnings = FALSE, recursive = TRUE)

master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
cov_list <- list.files(file.path(out_dir, "covariates", "interim"), pattern = "^ua", full.names = T)

cov <- cov_list %>%
  lapply(read_csv) %>%
  reduce(full_join)

write_csv(cov, file.path(out_dir, "covariates", "final", paste0(tolower(country), "_covariates_oblast", output_label, ".csv")))
