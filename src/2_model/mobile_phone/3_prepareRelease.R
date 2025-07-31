source(file.path(here::here(), "R_helpers/generic.R"))

# create output directory
dir.create(file.path(out_dir, "model", "deterministic", "deliverables", "202508 Deterministic Estimates"), recursive=T, showWarnings=F)


flows_hromada_agesex <- data.table::fread(file.path(out_dir, "model", "deterministic", "mobilePhone_deterministic_agesex_domestic.csv"))

flows_hromada_agesex <- flows_hromada_agesex |>
  filter(destination_oblast != "Abroad") |>
  mutate(
    origin_raion = ifelse(origin_raion == "Київ", "Kyiv", origin_raion),
    destination_raion = ifelse(destination_raion == "Київ", "Kyiv", destination_raion)
  )

stocks_hromada_agesex <- flows_hromada_agesex |>
  as_tibble() |>
  group_by(t, a_name, s_name, hromada = destination_hromada, oblast = destination_oblast, raion = destination_raion) |>
  summarise(
    pop_estimated = sum(monthlyFlow_hat_calibrated),
    .groups = "drop"
  )


# write output

write_csv(stocks_hromada_agesex, file.path(
  out_dir, "model", "deterministic", "deliverables", "202508 Deterministic Estimates",
  paste0(tolower(country), "_stocks_hromada_agesex", output_label, ".csv")
))

write_csv(flows_hromada_agesex, file.path(
  out_dir, "model", "deterministic", "deliverables", "202508 Deterministic Estimates",
  paste0(tolower(country), "_flows_hromada_agesex", output_label, ".csv")
))
