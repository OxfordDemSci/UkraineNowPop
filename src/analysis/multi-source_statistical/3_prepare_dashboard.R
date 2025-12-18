# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "src", "helpers", "R_helpers", "generic.R"))
library(cmdstanr)
library(matrixStats)

# Script parameter
model_name <- "211_age_sex_model"

# Load data
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
fit <- readRDS(file.path(out_dir, "model", "age_sex_model", paste0("fit_", model_name, ".rds")))

# Create dashboard indexing
master_index <- master_index |>
  separate(col = a_name, into = c("age_min0", "age_max0"), sep = "_", convert = TRUE) |>
  mutate(
    country = rep("UKR", length(ADM1_PCODE)),
    admin_level = rep(1, length(ADM1_PCODE)) |> as.integer(),
    age_min = ifelse(str_detect(age_min0, "Plus"), str_sub(age_min0, 1, 2), round(as.numeric(age_min0), 0)) |> as.integer(),
    age_max = ifelse(str_detect(age_min0, "Plus"), 999, round(as.numeric(age_max0), 0)) |> as.integer()
  ) |>
  rename(
    "pcode" = "ADM1_PCODE",
    "day" = "t_name",
    "sex" = "s"
  )

# Convert fit object to dashboard input csv
convert_pop_1Darray_dashboard <- function(fit_object, master_index = master_index) {
  pop_stocks <- fit_object$draws(variables = "N", format = "df")

  pop_dashboard <- pop_stocks |>
    select(-starts_with(".")) |>
    t() |>
    as.data.frame()

  pop_dashboard <- pop_dashboard |>
    rownames_to_column(var = "parameter") |>
    mutate(parameter = str_sub(parameter, 3, -2) |> as.integer()) |>
    left_join(master_index |>
      select(country, admin_level, pcode, day, age_min, age_max, sex, parameter), by = "parameter") |>
    mutate(
      pop = rowMeans(pick(starts_with("V")), na.rm = TRUE) |> as.integer(),
      pop_lower = rowQuantiles(as.matrix(pick(starts_with("V"))), probs = 0.025) |> as.integer(),
      pop_upper = rowQuantiles(as.matrix(pick(starts_with("V"))), probs = 0.975) |> as.integer(),
      across(starts_with("V"), ~ as.integer(round(.))),
      pop_posterior = paste0("[", apply(pick(starts_with("V")), 1, function(x) paste(x, collapse = ",")), "]")
    ) |>
    select(-starts_with("V", ignore.case = FALSE))

  write.csv(pop_dashboard,
    file = file.path(env$repo_dir, "src", "dashboard", "api", "app", "data", "db-data", "pop.csv"),
    row.names = FALSE
  )
  return(pop_dashboard)
}

pop <- convert_pop_1Darray_dashboard(fit)
