source(file.path(here::here(), "R_helpers/generic.R"))

# create output directory
dir.create(file.path(out_dir, "model", "deterministic", "deliverables", "202508"), recursive = T, showWarnings = F)


flows_hromada_agesex <- data.table::fread(file.path(out_dir, "model", "deterministic", "mobilePhone_deterministic_agesex.csv"))

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
  )


stocks_hromada_agesex <- flows_hromada_agesex |>
  as_tibble() |>
  group_by(t, a_name, s_name,
    hromada = destination_hromada, oblast = destination_oblast, raion = destination_raion,
    hromada_PCODE = destination_hromada_PCODE, raion_PCODE = destination_raion_PCODE
  ) |>
  summarise(
    pop_estimated = sum(monthlyFlow_hat_calibrated),
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
