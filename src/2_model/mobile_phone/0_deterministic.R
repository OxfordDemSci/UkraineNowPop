rm(list = ls())
gc()
library(sf)
library(tmap)
library(dtplyr)

tmap_options(component.autoscale = F)
options(scipen = 999)

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
hromada <- read_csv(file.path(in_dir, "KSE-Loc-Data-Hub", "full_dataset.csv"))
hromada_geo <- st_read(file.path(out_dir, "ua_master_hromada.gpkg"))
monthlyFlows <- read_csv(file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_monthlyFlows.csv"))
borderCrossing <- read_csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))


day1 <- monthlyFlows |> filter(t == "2021-12-01")

# Prepare baseline reference population
hromada_pop <- hromada |>
  select(hromada_code, total_popultaion_2022) |>
  full_join(hromada_geo |> st_drop_geometry()) |>
  mutate(total_popultaion_2022 = ifelse(hromada_code == "Kyiv", 2952301, total_popultaion_2022)) |>
  filter(oblast_name_en != "Autonomous Republic of Crimea") |>
  filter(hromada_code %in% unique(day1$destination_hromada))

national_pop <- borderCrossing |>
  bind_rows(
    tibble(
      date = monthlyFlows |> distinct(t) |> filter(t < min(borderCrossing$date)) |> pull(t),
      individuals = 0
    )
  ) |>
  rename(t = date) |>
  filter(day(t) == 1) |>
  mutate(
    national_total = sum(hromada_pop$total_popultaion_2022) - individuals
  )



# Monthly flows induced model ---------------------------------------

monthlyFlowstotals <- monthlyFlows |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    select(hromada_code) |>
    rename(
      origin_hromada = hromada_code
    )) |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    select(hromada_code) |>
    rename(
      destination_hromada = hromada_code
    )) |>
  group_by(t, origin_hromada, destination_hromada, origin_macroregion, destination_macroregion, origin_oblast, destination_oblast) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ungroup()

monthlyFlowstotals <- monthlyFlowstotals |>
  group_split(t)

penetration_rate <- list()
penetration_rate[[1]] <- monthlyFlowstotals[[1]] |>
  group_by(origin_hromada, origin_oblast, origin_macroregion) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
  ) |>
  left_join(
    hromada_pop |>
      group_by(hromada_code) |>
      summarise(total_popultaion_2022 = sum(total_popultaion_2022)) |>
      rename(
        origin_hromada = hromada_code
      )
  ) |>
  ungroup() |>
  mutate(
    p0 = subscribers_monthlyFlow / total_popultaion_2022,
    p0_imputed = case_when(
      is.na(p0) ~ sum(subscribers_monthlyFlow[!is.na(p0)], na.rm = T) / sum(total_popultaion_2022[!is.na(p0)], na.rm = T),
      TRUE ~ p0
    )
  ) |>
  select(origin_hromada, origin_oblast, origin_macroregion, origin_p = p0) |>
  mutate(t = min(day1$t))

monthlyFlows_totals_hat <- list()

for (idx in 1:n_distinct(monthlyFlows$t)) {
  print(monthlyFlowstotals[[idx]]$t[1])
  monthlyFlows_totals_hat[[idx]] <- monthlyFlowstotals[[idx]] |>
    full_join(penetration_rate[[idx]], by = c("t", "origin_hromada", "origin_oblast", "origin_macroregion")) |>
    left_join(
      penetration_rate[[idx]] |>
        select(t, origin_hromada, origin_p) |>
        rename(
          destination_hromada = origin_hromada,
          destination_p = origin_p
        ),
      by = c("t", "destination_hromada")
    ) |>
    mutate(
      p = case_when(
        origin_hromada == "Unknown" & destination_hromada == "abroad" ~ mean(origin_p, na.rm = T),
        origin_hromada == "Unknown" ~ destination_p,
        origin_hromada == "abroad" ~ mean(origin_p, na.rm = T),
        T ~ origin_p
      )
    ) |>
    group_by(origin_oblast) |>
    mutate(
      p = ifelse(is.na(p), mean(p, na.rm = T), p)
    ) |>
    ungroup() |>
    mutate(
      monthlyFlow_hat = subscribers_monthlyFlow / p
    ) |>
    select(-p, -origin_p, -destination_p)

  penetration_rate[[idx + 1]] <- monthlyFlows_totals_hat[[idx]] |>
    group_by(t, origin_hromada = destination_hromada, origin_oblast = destination_oblast, origin_macroregion = destination_macroregion) |>
    summarise(
      origin_p = sum(subscribers_monthlyFlow) / sum(monthlyFlow_hat),
      .groups = "drop"
    ) |>
    mutate(t = t + months(1))
}



# Age and sex modelling --------------------------------------------------

# Prepare national data

agesex <- read_csv(file.path(in_dir, "COD-PS", "ukr_admpop_adm1_2022.csv")) |>
  select(admin1Name_en, starts_with("F"), starts_with("M"), -ends_with("TL")) |>
  pivot_longer(c(starts_with("F"), starts_with("M")), names_to = "agesex_label", values_to = "pop") |>
  rowwise() |>
  mutate(
    s_name = ifelse(grepl("F", agesex_label), "F", "M"),
    a = str_split(agesex_label, "_")[[1]][2],
    a = as.integer(ifelse(a == "80Plus", 80, a)),
    count = ifelse(a == 15, 2, 1)
  ) |>
  uncount(count, .id = "id_dup") |>
  mutate(
    pop = case_when(
      a == 15 & id_dup == 1 ~ pop * 3 / 5,
      a == 15 & id_dup == 2 ~ pop * 2 / 5,
      T ~ pop
    ),
    a = ifelse(id_dup == 2, 18, a),
    a_name = case_when(
      a <= 15 ~ "0-17",
      a <= 20 ~ "18-24",
      a <= 30 ~ "25-34",
      a <= 40 ~ "35-44",
      a <= 50 ~ "45-54",
      a <= 60 ~ "55-64",
      a >= 65 ~ "65Plus"
    )
  ) |>
  group_by(admin1Name_en, s_name, a_name) |>
  summarise(
    pop = sum(pop),
    .groups = "drop"
  ) |>
  group_by(admin1Name_en) |>
  mutate(
    pi_0 = pop / sum(pop)
  ) |>
  rename(
    oblast_name_en = admin1Name_en
  ) |>
  right_join(
    hromada_pop |>
      distinct(oblast_name_en)
  )


monthlyFlows <- monthlyFlows |>
  group_split(t)

penetration_rate_agesex <- list()
penetration_rate_agesex[[1]] <- monthlyFlows[[1]] |>
  group_by(origin_hromada, origin_oblast, origin_macroregion, a_name, s_name) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
  ) |>
  left_join(
    agesex |>
      rename(
        origin_oblast = oblast_name_en
      )
  ) |>
  left_join(
    hromada_pop |>
      group_by(hromada_code) |>
      summarise(total_popultaion_2022 = sum(total_popultaion_2022)) |>
      rename(
        origin_hromada = hromada_code
      )
  ) |>
  ungroup() |>
  mutate(
    p0 = subscribers_monthlyFlow / (pi_0 * total_popultaion_2022),
    # p0_imputed = case_when(
    #   is.na(p0) ~ sum(pop[!is.na(p0)], na.rm = T) / sum(pop[!is.na(p0)], na.rm = T),
    #   TRUE ~ p0
    # )
  ) |>
  select(s_name, a_name, origin_hromada, origin_oblast, origin_macroregion, origin_p = p0) |>
  mutate(t = min(day1$t)) |>
  filter(origin_hromada != "Unknown" & origin_hromada != "abroad")

prop_agesex <- list()
prop_agesex[[1]] <- hromada_pop |>
  select(hromada_code, oblast_name_en, total_popultaion_2022) |>
  right_join(agesex |>
    select(-pop), relationship = "many-to-many") |>
  rename(
    origin_oblast = oblast_name_en,
    origin_hromada = hromada_code
  ) |>
  mutate(
    pop = pi_0 * total_popultaion_2022
  )


monthlyFlows_hat <- list()

for (idx in 1:n_distinct(monthlyFlows$t)) {
  print(monthlyFlows[[idx]]$t[1])
  temp <- monthlyFlows[[idx]] |>
    full_join(penetration_rate_agesex[[idx]], by = c("t", "s_name", "a_name", "origin_hromada", "origin_oblast", "origin_macroregion")) |>
    left_join(
      penetration_rate_agesex[[idx]] |>
        select(t, s_name, a_name, origin_hromada, origin_p) |>
        rename(
          destination_hromada = origin_hromada,
          destination_p = origin_p
        ),
      by = c("t", "s_name", "a_name", "destination_hromada")
    ) |>
    mutate(
      p = case_when(
        origin_hromada == "Unknown" & destination_hromada == "abroad" ~ mean(origin_p, na.rm = T),
        origin_hromada == "Unknown" ~ destination_p,
        origin_hromada == "abroad" ~ mean(origin_p, na.rm = T),
        T ~ origin_p
      )
    ) |>
    group_by(origin_oblast) |>
    mutate(
      p = ifelse(is.na(p), mean(p, na.rm = T), p)
    ) |>
    ungroup() |>
    mutate(
      monthlyFlow_hat = subscribers_monthlyFlow / p
    ) |>
    select(-p, -origin_p, -destination_p) |>
    group_by(origin_hromada, destination_hromada) |>
    mutate(
      pi = monthlyFlow_hat / sum(monthlyFlow_hat)
    ) |>
    ungroup() |>
    right_join(
      prop_agesex[[idx]] |>
        rename(origin_hromada = hromada_code)
    )


  left_join(monthlyFlows_totals_hat[[idx]] |>
    group_by(origin_hromada, destination_hromada) |>
    summarise(monthlyFlows_totals_hat = sum(monthlyFlow_hat)), by = c("origin_hromada", "destination_hromada")) |>
    group_by(origin_hromada, destination_hromada) |>
    mutate(
      monthlyFlows_hat_calibrated = monthlyFlows_totals_hat * monthlyFlow_hat / sum(monthlyFlow_hat)
    )

  penetration_rate_agesex[[idx + 1]] <- monthlyFlows_hat[[idx]] |>
    group_by(t, a_name, s_name, origin_hromada = destination_hromada, origin_oblast = destination_oblast, origin_macroregion = destination_macroregion) |>
    summarise(
      origin_p = sum(subscribers_monthlyFlow) / sum(monthlyFlows_hat_calibrated),
      .groups = "drop"
    ) |>
    mutate(t = t + months(1))
}

penetration_rate_df <- bind_rows(penetration_rate)
monthlyFlows_totals_hat_df <- bind_rows(monthlyFlows_totals_hat)
penetration_rate_agesex_df <- bind_rows(penetration_rate_agesex)
monthlyFlows_hat_df <- bind_rows(monthlyFlows_hat)

# National rescaling -----------------------------------------------------






# Evaluate

penetration_rate <- bind_rows(penetration_rate)
monthlyFlows_totals_hat <- bind_rows(monthlyFlows_totals_hat)


# 3.2 evaluate monthly flows induced model -------------------------------

# penetration rate

penetration_rate_df |>
  mutate(macroregion_oblast = paste0(origin_macroregion, " - ", origin_oblast)) |>
  ggplot(aes(x = t, y = origin_p, col = origin_hromada)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(macroregion_oblast ~ .) +
  theme(legend.position = "None") +
  geom_hline(yintercept = 1, color = "grey20") +
  labs(title = paste("Penetration rate at hromada level - Monthly flows"), x = "Time", y = "Subscribers/Pop")

# stocks total
monthlyFlows_totals_hat_national <- monthlyFlows_totals_hat_df |>
  filter(destination_hromada != "abroad") |>
  group_by(t) |>
  summarise(
    monthlyFlow_hat = sum(monthlyFlow_hat)
  ) |>
  left_join(national_pop) |>
  mutate(
    national_total = ifelse(t < min(national_pop$t), sum(day1_pop$total_popultaion_2022), national_total),
    monthlyFlow_hat_perc = (monthlyFlow_hat - national_total) / national_total * 100
  )



gg_flows_national <- ggplot(
  monthlyFlows_totals_hat_national |> select(-national_total) |> pivot_longer(c(monthlyFlow_hat, monthlyFlow_hat_perc), names_to = "type"),
  aes(x = t, y = value)
) +
  geom_line() +
  geom_hline(data = tibble(type = c("monthlyFlow_hat", "monthlyFlow_hat_perc"), value = c(sum(hromada_pop$total_popultaion_2022), 0)), aes(yintercept = value), color = "red", linetype = "dashed") +
  geom_line(data = national_pop |> mutate(type = "monthlyFlow_hat"), aes(x = t, y = national_total), color = "red", linetype = "dashed") +
  labs(title = "Monthly flows induced stocks: National level", ) +
  facet_wrap(~type, scales = "free_y") +
  theme_minimal()
gg_flows_national


monthlyFlowstotals_oblast <- monthlyFlows_totals_hat |>
  # filter(destination_hromada != "Abroad") |>
  group_by(t, destination_oblast) |>
  summarise(
    monthlyFlow_hat = sum(monthlyFlow_hat)
  )

ggplot(monthlyFlowstotals_oblast |>
  filter(destination_oblast != "Abroad"), aes(x = t, y = monthlyFlow_hat, col = destination_oblast)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Monthly flows induced stocks: Oblast level", x = "Time", y = "Subscribers/Pop at t0")

# Visualise penetration rate
penetration_rate_agesex_df |>
  group_by(t, a_name, s_name, origin_oblast) |>
  summarise(origin_p = mean(origin_p)) |>
  ggplot(aes(x = t, y = origin_p, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(origin_oblast ~ .) +
  geom_hline(yintercept = 1, color = "grey20") +
  labs(
    title = paste("Penetration rate at hromada level - Monthly flows"),
    x = "Time", y = "Penetration rate"
  )
