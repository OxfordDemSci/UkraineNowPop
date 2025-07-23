# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))

# Script parameter

# Load data
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
flows_hromada_agesex <- data.table::fread(file.path(out_dir, "model", "deterministic", "mobilePhone_deterministic_agesex_domestic.csv"))


level <- "oblast"

# prepare stocks data
pop_oblast <- flows_hromada_agesex |>
  as_tibble() |>
  group_by(t, a_name, s_name, i_name = destination_oblast) |>
  summarise(
    pop = sum(monthlyFlow_hat_calibrated),
    .groups = "drop"
  )

# convert to dashboard indexing
pop_oblast <- pop_oblast |>
  separate(col = a_name, into = c("age_min0", "age_max0"), sep = "-", convert = TRUE) |>
  mutate(
    country = "UKR",
    admin_level = 1,
    age_min = ifelse(str_detect(age_min0, "Plus"), str_sub(age_min0, 1, 2), round(as.numeric(age_min0), 0)) |> as.integer(),
    age_max = ifelse(str_detect(age_min0, "Plus"), 999, round(as.numeric(age_max0), 0)) |> as.integer(),
    sex = ifelse(s_name == "M", 1, 2),
    pop = as.integer(pop)
  ) |>
  left_join(master_index |>
    distinct(i_name, ADM1_PCODE)) |>
  rename(
    "pcode" = "ADM1_PCODE",
    "day" = "t",
  ) |>
  select(country, admin_level, pcode, day, age_min, age_max, sex, pop) |>
  rowwise() |>
  mutate(
    pop_upper = pop,
    pop_lower = pop,
    pop_posterior = paste0("[", paste(as.integer(rnorm(100, pop, 1)), collapse = ", "), "]")
  )

write.csv(pop_oblast,
  file = file.path(env$repo_dir, "src", "dashboard", "api", "app", "data", "db-data", "pop.csv"),
  row.names = FALSE
)

# prepare flows data

flows_oblast <- flows_hromada_agesex |>
  filter(!origin_oblast %in% c("Abroad", "Unknown")) |>
  filter(destination_oblast != "Abroad") |>
  as_tibble() |>
  group_by(t, a_name, s_name, origin_i_name = origin_oblast, destination_i_name = destination_oblast) |>
  summarise(
    count = sum(monthlyFlow_hat_calibrated) |> as.integer(),
    .groups = "drop"
  )

# convert to dashboard indexing
flows_oblast_prep <- flows_oblast |>
  separate(col = a_name, into = c("age_min0", "age_max0"), sep = "-", convert = TRUE) |>
  mutate(
    country = "UKR",
    admin_level = 1,
    age_min = ifelse(str_detect(age_min0, "Plus"), str_sub(age_min0, 1, 2), round(as.numeric(age_min0), 0)) |> as.integer(),
    age_max = ifelse(str_detect(age_min0, "Plus"), 999, round(as.numeric(age_max0), 0)) |> as.integer(),
    sex = ifelse(s_name == "M", 1, 2)
  ) |>
  left_join(master_index |>
    distinct(i_name, ADM1_PCODE) |>
    rename(origin = ADM1_PCODE), by = c("origin_i_name" = "i_name")) |>
  left_join(master_index |>
    distinct(i_name, ADM1_PCODE) |>
    rename(destination = ADM1_PCODE), by = c("destination_i_name" = "i_name")) |>
  rename(
    "day" = "t",
  ) |>
  select(country, admin_level, origin, destination, day, age_min, age_max, sex, count)

flows_oblast_prep <- flows_oblast_prep |>
  group_by(day, age_min, age_max, sex) |>
  mutate(
    probability = count / sum(count)
  )

write_csv(flows_oblast_prep,
  file = file.path(env$repo_dir, "src", "dashboard", "api", "app", "data", "db-data", "migration.csv")
)
