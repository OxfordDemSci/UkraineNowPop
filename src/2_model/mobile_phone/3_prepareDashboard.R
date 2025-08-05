# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))

# Script parameter

# Load data
pcodes <- read_csv(file.path(here::here("src/dashboard/api/app/data/db-data/global_pcodes.csv")))
flows_hromada_agesex <- data.table::fread(file.path(out_dir, "model", "deterministic", "mobilePhone_deterministic_agesex_domestic.csv"))


flows_hromada_agesex <- flows_hromada_agesex |>
  mutate(
    origin_hromada = ifelse(origin_hromada == "abroad", "Abroad", origin_hromada),
    destination_hromada = ifelse(destination_hromada == "abroad", "Abroad", destination_hromada),
    origin_hromada = ifelse(origin_hromada == "Kyiv", "UA800000", origin_hromada),
    origin_hromada = str_sub(origin_hromada, 1, 9),
    origin_raion = ifelse(origin_raion == "Kyiv", "UA8000", origin_raion),
    origin_raion = str_sub(origin_hromada, 1, 6),
    destination_hromada = ifelse(destination_hromada == "Kyiv", "UA800000", destination_hromada),
    destination_hromada = str_sub(destination_hromada, 1, 9),
    destination_raion = ifelse(destination_raion == "Kyiv", "UA8000", destination_raion),
    destination_raion = str_sub(destination_hromada, 1, 6)
  )

pcodes <- pcodes |>
  filter(`Admin Level` == 1 & Location == "UKR") |>
  rename(pcode = `P-Code`)


# prepare stocks data

prepare_stocks <- function(raw_flows, level) {
  admin_level <- ifelse(level == "oblast", 1, ifelse(level == "raion", 2, ifelse(level == "hromada", 3, NA)))

  pop_stocks <- raw_flows |>
    as_tibble() |>
    rename(i_name = paste0("destination_", level)) |>
    group_by(t, a_name, s_name, i_name) |>
    summarise(
      pop = sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    )

  # convert to dashboard indexing

  if (level == "oblast") {
    pop_stocks <- pop_stocks |>
      left_join(pcodes |>
        select(i_name = Name, pcode))
  } else {
    pop_stocks <- pop_stocks |>
      rename(pcode = i_name)
  }

  pop_stocks <- pop_stocks |>
    separate(col = a_name, into = c("age_min0", "age_max0"), sep = "-", convert = TRUE) |>
    mutate(
      country = "UKR",
      admin_level = admin_level,
      age_min = ifelse(str_detect(age_min0, "Plus"), str_sub(age_min0, 1, 2), round(as.numeric(age_min0), 0)) |> as.integer(),
      age_max = ifelse(str_detect(age_min0, "Plus"), 999, round(as.numeric(age_max0), 0)) |> as.integer(),
      sex = ifelse(s_name == "M", 1, 2),
      pop = as.integer(pop)
    ) |>
    rename(
      "day" = "t",
    ) |>
    select(country, admin_level, pcode, day, age_min, age_max, sex, pop) |>
    rowwise() |>
    mutate(
      pop_upper = pop,
      pop_lower = pop,
      pop_posterior = paste0("[", paste(as.integer(rnorm(100, pop, 1)), collapse = ", "), "]")
    )
  return(pop_stocks)
}

pop_stocks_all <- lapply(
  c("hromada", "raion", "oblast"),
  function(l) prepare_stocks(flows_hromada_agesex, l)
)

pop_stocks_all <- bind_rows(
  pop_stocks_all
)

write.csv(pop_stocks_all,
  file = file.path(env$repo_dir, "src", "dashboard", "api", "app", "data", "db-data", "pop.csv"),
  row.names = FALSE
)

# prepare flows data

prepare_flows <- function(raw_flows, level) {
  admin_level <- ifelse(level == "oblast", 1, ifelse(level == "raion", 2, ifelse(level == "hromada", 3, NA)))

  pop_flows <- raw_flows |>
    rename(origin_i_name = paste0("origin_", level), destination_i_name = paste0("destination_", level)) |>
    filter(!origin_i_name %in% c("Abroad", "Unknown")) |>
    filter(destination_i_name != "Abroad") |>
    as_tibble() |>
    group_by(t, a_name, s_name, origin_i_name, destination_i_name) |>
    summarise(
      count = sum(monthlyFlow_hat_calibrated) |> as.integer(),
      .groups = "drop"
    )

  # convert to dashboard indexing

  if (level == "oblast") {
    pop_flows <- pop_flows |>
      left_join(pcodes |>
        select(i_name = Name, origin = pcode), by = c("origin_i_name" = "i_name")) |>
      left_join(pcodes |>
        select(i_name = Name, destination = pcode), by = c("destination_i_name" = "i_name"))
  } else {
    pop_flows <- pop_flows |>
      rename(
        origin = origin_i_name,
        destination = destination_i_name
      )
  }

  flows_prep <- pop_flows |>
    separate(col = a_name, into = c("age_min0", "age_max0"), sep = "-", convert = TRUE) |>
    mutate(
      country = "UKR",
      admin_level = admin_level,
      age_min = ifelse(str_detect(age_min0, "Plus"), str_sub(age_min0, 1, 2), round(as.numeric(age_min0), 0)) |> as.integer(),
      age_max = ifelse(str_detect(age_min0, "Plus"), 999, round(as.numeric(age_max0), 0)) |> as.integer(),
      sex = ifelse(s_name == "M", 1, 2)
    ) |>
    rename(
      "day" = "t",
    ) |>
    select(country, admin_level, origin, destination, day, age_min, age_max, sex, count)

  flows_prep <- flows_prep |>
    group_by(day, age_min, age_max, sex) |>
    mutate(
      probability = count / sum(count)
    )

    return(flows_prep)
}

pop_flows_all <- lapply(
  c("hromada", "raion", "oblast"),
  function(l) prepare_flows(flows_hromada_agesex, l)
)

pop_flows_all <- bind_rows(
  pop_flows_all
)

write_csv(pop_flows_all,
  file = file.path(env$repo_dir, "src", "dashboard", "api", "app", "data", "db-data", "migration.csv")
)

