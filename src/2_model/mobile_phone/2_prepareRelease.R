source(file.path(here::here(), "R_helpers/generic.R"))

# create output directory
dir.create(file.path(out_dir, "model", "deterministic", "deliverables", "202508"), recursive = T, showWarnings = F)

# load data
pcodes <- read_csv(file.path(here::here("src/dashboard/api/app/data/db-data/global_pcodes.csv")))
flows_hromada_agesex <- data.table::fread(file.path(out_dir, "model", "deterministic", "mobilePhone_deterministic_agesex.csv"))

pcodes <- pcodes |>
  filter(`Admin Level` == 1 & Location == "UKR") |>
  rename(pcode = `P-Code`)

# prepare flows data
flows_hromada_agesex <- flows_hromada_agesex |>
  mutate(
    origin_hromada_PCODE = ifelse(origin_hromada == "Kyiv", "UA8000000", origin_hromada),
    origin_hromada_PCODE = ifelse(origin_hromada_PCODE != "Abroad", str_sub(origin_hromada_PCODE, 1, 9), origin_hromada_PCODE),
    origin_raion_PCODE = ifelse(origin_raion == "Kyiv", "UA8000", origin_raion),
    origin_raion_PCODE = ifelse(origin_hromada_PCODE != "Abroad", str_sub(origin_raion_PCODE, 1, 6), origin_raion_PCODE),
    destination_hromada_PCODE = ifelse(destination_hromada == "Kyiv", "UA8000000", destination_hromada),
    destination_hromada_PCODE = ifelse(destination_hromada_PCODE != "Abroad", str_sub(destination_hromada_PCODE, 1, 9), destination_hromada_PCODE),
    destination_raion_PCODE = ifelse(destination_raion == "Kyiv", "UA8000", destination_raion),
    destination_raion_PCODE = ifelse(destination_hromada_PCODE != "Abroad", str_sub(destination_hromada_PCODE, 1, 6), destination_raion_PCODE),
  ) |>
  rename(pop_estimated = monthlyFlow_hat_calibrated) |>
  select(
    t, a_name, s_name, starts_with("origin"), starts_with("destination"), pop_estimated
  )


flows_hromada_agesex <- flows_hromada_agesex |>
  left_join(
    pcodes |>
      select(destination_oblast = Name, destination_oblast_PCODE = pcode)
  ) |>
  left_join(
    pcodes |>
      select(origin_oblast = Name, origin_oblast_PCODE = pcode)
  )

flows_hromada_agesex <- flows_hromada_agesex |>
  mutate(
    origin_oblast_PCODE = ifelse(origin_oblast == "Abroad", "Abroad", origin_oblast_PCODE),
    origin_oblast_PCODE = ifelse(origin_oblast == "Unknown", "Unknown", origin_oblast_PCODE),
    destination_oblast_PCODE = ifelse(destination_oblast == "Abroad", "Abroad", destination_oblast_PCODE)
  )

# prepare stocks data
stocks_hromada_agesex <- flows_hromada_agesex |>
  as_tibble() |>
  group_by(t, a_name, s_name,
    hromada = destination_hromada, oblast = destination_oblast, raion = destination_raion,
    hromada_PCODE = destination_hromada_PCODE, raion_PCODE = destination_raion_PCODE,
    oblast_PCODE = destination_oblast_PCODE
  ) |>
  summarise(
    pop_estimated = sum(pop_estimated),
    .groups = "drop"
  )


# write output

write_csv(stocks_hromada_agesex, file.path(
  out_dir, "model", "deterministic", "deliverables", "202508",
  paste0(tolower(country), "_stocks_hromada_agesex", output_label, ".csv")
))

write_csv(flows_hromada_agesex, file.path(
  out_dir, "model", "deterministic", "deliverables", "202508",
  paste0(tolower(country), "_flows_hromada_agesex", output_label, ".csv")
))
