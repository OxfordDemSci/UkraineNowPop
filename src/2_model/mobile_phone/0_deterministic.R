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
refugee_agesex <- read_csv(file.path(out_dir, "population_proxy", "refugee", "eurostat_refugee_agesex.csv"))

day1 <- monthlyFlows |> filter(t == "2021-12-01")


# Prepare baseline reference population
hromada_pop <- hromada |>
  select(hromada_code, total_popultaion_2022) |>
  full_join(hromada_geo |> st_drop_geometry()) |>
  mutate(total_popultaion_2022 = ifelse(hromada_code == "Kyiv", 2952301, total_popultaion_2022)) |>
  filter(oblast_name_en != "Autonomous Republic of Crimea")

monthlyFlows <- monthlyFlows |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    select(hromada_code, raion_code) |>
    rename(
      origin_hromada = hromada_code,
      origin_raion = raion_code
    )) |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    select(hromada_code, raion_code) |>
    rename(
      destination_raion = raion_code,
      destination_hromada = hromada_code
    )) |>
  mutate(
    origin_raion = ifelse(origin_hromada == "Unknown", "Unknown", origin_raion),
    destination_raion = ifelse(destination_hromada == "Unknown", "Unknown", destination_raion),
    origin_raion = ifelse(origin_hromada == "abroad", "Abroad", origin_raion),
    destination_raion = ifelse(destination_hromada == "abroad", "Abroad", destination_raion),
  )

# Remove missing combination

# Dropping hromadas
dropping_hromadas <- monthlyFlows |>
  distinct(t, destination_hromada) |>
  group_by(destination_hromada) |>
  summarise(n_timesteps = length(unique(monthlyFlows$t)) - n()) |>
  filter(n_timesteps > 0)

# imputingHromada_names <- dropping_hromadas |>
#   filter(n_timesteps<=2) |>
#   pull(destination_hromada)

# dropping_hromadas <- dropping_hromadas |>
#   filter(n_timesteps>2) |>
#   pull(destination_hromada)


# # Apply deterministic model
# time_missing <- monthlyFlows_totals |>
#   filter(destination_hromada %in% imputingHromada_names) |>
#   complete(t, destination_hromada) |>
#   filter(is.na(subscribers_monthlyFlow)) |>
#   select(destination_hromada,t) |>
#   mutate(t_plus1 = t+months(1),
#         t_plus2 = t+months(2),
#          t_minus2 = t-months(2),
#          t_minus1 = t-months(1)) |>
#   pivot_longer(cols = -destination_hromada, names_to = "type", values_to = "t")

# monthlyFlows_totals_imputed <- monthlyFlows_totals |>
#   right_join(time_missing |>
#     distinct(destination_hromada, t)) |>
#   filter(!is.na(subscribers_monthlyFlow)) |>
#   group_by(destination_hromada) |>
#   complete(t, nesting(origin_hromada,  origin_macroregion, destination_macroregion, origin_oblast, destination_oblast), fill = list(subscribers_monthlyFlow = NA))


# remove missing hromadas
monthlyFlows <- monthlyFlows |>
  filter(!origin_hromada %in% dropping_hromadas$destination_hromada) |>
  filter(!destination_hromada %in% dropping_hromadas$destination_hromada)

agesex_geoCombination <- monthlyFlows |>
  group_by(t, a_name, s_name) |>
  summarise(
    n_hromada = n_distinct(origin_hromada, destination_hromada),
    n_raion = n_distinct(origin_raion, destination_raion),
    n_oblast = n_distinct(origin_oblast, destination_oblast),
    n_macroregion = n_distinct(origin_macroregion, destination_macroregion),
    .groups = "drop"
  ) |>
  bind_rows(monthlyFlows |>
    group_by(t) |>
    summarise(
      n_hromada = n_distinct(origin_hromada, destination_hromada),
      n_raion = n_distinct(origin_raion, destination_raion),
      n_oblast = n_distinct(origin_oblast, destination_oblast),
      n_macroregion = n_distinct(origin_macroregion, destination_macroregion),
      .groups = "drop"
    ) |>
    mutate(
      a_name = "All",
      s_name = "All"
    )) |>
  pivot_longer(c(n_hromada, n_raion, n_oblast, n_macroregion), names_to = "geo_level", values_to = "n_combinations")

agesex_geoCombination_full <- agesex_geoCombination |>
  ungroup() |>
  pivot_wider(names_from = geo_level, values_from = n_combinations) |>
  group_by(a_name, s_name) |>
  summarise(n_macroregion = mean(n_macroregion)) |>
  filter(n_macroregion == 56 & a_name != "All" & s_name != "All")

monthlyFlows <- monthlyFlows |>
  filter(a_name %in% agesex_geoCombination_full$a_name & s_name %in% agesex_geoCombination_full$s_name)


# Prepare age and sex reference data -------------------------------------

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

hromada_agesex <- hromada_pop |>
  filter(hromada_code %in% unique(monthlyFlows$destination_hromada)) |>
  left_join(agesex |>
    select(oblast_name_en, s_name, a_name, pi_0)) |>
  mutate(
    pop = total_popultaion_2022 * pi_0
  ) |>
  filter(a_name %in% agesex_geoCombination_full$a_name & s_name %in% agesex_geoCombination_full$s_name) |>
  select(hromada_code, s_name, a_name, pop)

# Prepare refugee age and sex data ---------------------------------------------------

agesex_national_d0 <- hromada_agesex |>
  group_by(s_name, a_name) |>
  summarise(
    pop = sum(pop),
    .groups = "drop"
  )

borderCrossing_monthly <- borderCrossing |>
  rename(
    outflows = individuals
  ) |>
  mutate(
    t = ceiling_date(date, unit = "month")
  ) |>
  group_by(t) |>
  summarise(
    outflows = mean(outflows)
  ) |>
  ungroup()

refugee_agesex_ <- bind_rows(
  refugee_agesex |>
    filter(t <= max(monthlyFlows$t)),
  refugee_agesex |>
    filter(t == "2022-04-01") |>
    mutate(
      t = as.Date("2022-03-01")
    )
)

refugee_agesex_ <- refugee_agesex_ |>
  mutate(pi_hat = percentage / 100) |>
  select(-refugee, -percentage) |>
  full_join(
    borderCrossing_monthly
  ) |>
  mutate(
    outflows = ifelse(is.na(outflows), 0, outflows),
    outflows = outflows * pi_hat
  ) |>
  filter(a_name %in% agesex_geoCombination_full$a_name & s_name %in% agesex_geoCombination_full$s_name)


national_pop <- bind_rows(
  agesex_national_d0 |>
    full_join(
      refugee_agesex_ |>
        select(t, a_name, s_name, outflows)
    ),
  bind_rows(
    lapply(seq(as.Date("2021-11-01"), min(refugee_agesex_$t) - months(1), by = "month"), function(t) {
      agesex_national_d0 |>
        mutate(t = t)
    })
  )
) |>
  mutate(
    outflows = ifelse(is.na(outflows), 0, outflows),
    pop_updated = pop - outflows
  ) |>
  ungroup()

# Monthly flows induced model ---------------------------------------

monthlyFlows_totals <- monthlyFlows |>
  group_by(t, origin_hromada, destination_hromada, origin_macroregion, destination_macroregion, origin_oblast, destination_oblast, origin_raion, destination_raion) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ungroup()

monthlyFlows_totals <- monthlyFlows_totals |>
  group_split(t)

# 1.1 Compute penetration rate day 1
penetration_rate <- list()
penetration_rate[[1]] <- monthlyFlows_totals[[1]] |>
  group_by(origin_hromada, origin_raion, origin_oblast, origin_macroregion) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
  ) |>
  left_join(
    hromada_agesex |>
      group_by(origin_hromada = hromada_code) |>
      summarise(pop = sum(pop))
  ) |>
  ungroup() |>
  mutate(
    p0 = subscribers_monthlyFlow / pop,
    p0_imputed = case_when(
      # is.na(p0) ~ sum(subscribers_monthlyFlow[!is.na(p0)], na.rm = T) / sum(total_popultaion_2022[!is.na(p0)], na.rm = T),
      TRUE ~ p0
    )
  ) |>
  select(origin_hromada, origin_raion, origin_oblast, origin_macroregion, origin_p = p0) |>
  mutate(t = min(monthlyFlows$t)) |>
  filter(origin_macroregion != "Unknown" & origin_macroregion != "Abroad")



# Compute age and sex penetration rate on day 1
monthlyFlows_agesex <- monthlyFlows |>
  group_split(t)

penetration_rate_agesexMacroregion <- list()
penetration_rate_agesexMacroregion[[1]] <- monthlyFlows_agesex[[1]] |>
  group_by(origin_hromada, origin_oblast, origin_macroregion, a_name, s_name) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
  ) |>
  left_join(
    hromada_agesex |>
      rename(
        origin_hromada = hromada_code
      )
  ) |>
  ungroup() |>
  group_by(s_name, a_name, origin_macroregion) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    monthlyFlow_hat_calibrated = sum(pop),
    origin_p = sum(subscribers_monthlyFlow) / sum(pop),
    # p0_imputed = case_when(
    #   is.na(p0) ~ sum(pop[!is.na(p0)], na.rm = T) / sum(pop[!is.na(p0)], na.rm = T),
    #   TRUE ~ p0
    # )
  ) |>
  mutate(t = min(monthlyFlows$t)) |>
  filter(origin_macroregion != "Unknown" & origin_macroregion != "Abroad") |>
  select(-subscribers_monthlyFlow, -monthlyFlow_hat_calibrated)


# 1.2 Update penetration rate and total flows

monthlyFlows_totals_hat <- list()
monthlyFlows_agesex_hat <- list()
monthlyFlows_agesex_domestic_hat <- list()

for (idx in 1:n_distinct(monthlyFlows$t)) {
  print(monthlyFlows_totals[[idx]]$t[1])

  # Compute intermediary population totals
  monthlyFlows_totals_hat[[idx]] <- monthlyFlows_totals[[idx]] |>
    full_join(penetration_rate[[idx]], by = c("t", "origin_hromada", "origin_raion", "origin_oblast", "origin_macroregion")) |>
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
      p_ = case_when(
        origin_hromada == "Unknown" & destination_hromada == "abroad" ~ mean(origin_p, na.rm = T),
        origin_hromada == "Unknown" ~ destination_p,
        origin_hromada == "abroad" ~ mean(origin_p, na.rm = T),
        T ~ origin_p
      )
    ) |>
    group_by(origin_oblast) |>
    mutate(
      p = ifelse(is.na(p_), mean(p_, na.rm = T), p_)
    ) |>
    ungroup() |>
    mutate(
      monthlyFlow_hat = subscribers_monthlyFlow / p
    ) |>
    select(-p, -origin_p, -destination_p, -p_)

  # Compute intermediary population by age and sex

  monthlyFlows_agesex_hat[[idx]] <- monthlyFlows_agesex[[idx]] |>
    full_join(penetration_rate_agesexMacroregion[[idx]], by = c("t", "s_name", "a_name", "origin_macroregion")) |>
    left_join(
      penetration_rate_agesexMacroregion[[idx]] |>
        select(t, s_name, a_name, origin_macroregion, origin_p) |>
        rename(
          destination_macroregion = origin_macroregion,
          destination_p = origin_p
        ),
      by = c("t", "destination_macroregion", "s_name", "a_name")
    ) |>
    mutate(
      p = case_when(
        origin_macroregion == "Unknown" & destination_macroregion == "Abroad" ~ mean(origin_p, na.rm = T),
        origin_macroregion == "Unknown" ~ destination_p,
        origin_macroregion == "Abroad" ~ mean(origin_p, na.rm = T),
        T ~ origin_p
      )
    ) |>
    ungroup() |>
    mutate(
      monthlyFlow_hat = subscribers_monthlyFlow / p
    ) |>
    select(-origin_p, -destination_p)

  agesexMacroregion_proportion[[idx]] <- monthlyFlows_agesex_hat[[idx]] |>
    group_by(t, s_name, a_name, origin_macroregion, destination_macroregion) |>
    mutate(
      monthlyFlow_hat_total = sum(monthlyFlow_hat),
      subscribers_monthlyFlow_total = sum(subscribers_monthlyFlow),
    ) |>
    ungroup() |>
    group_by(t, origin_macroregion, destination_macroregion) |>
    mutate(
      pi_hat = monthlyFlow_hat_total / sum(monthlyFlow_hat_total)
    ) |>
    ungroup()


  # Apply updated pyramid to monthly flows

  monthlyFlows_agesex_hat[[idx]] <- monthlyFlows_agesex_hat[[idx]] |>
    group_by(t, s_name, a_name, origin_macroregion, destination_macroregion) |>
    summarise(
      monthlyFlow_hat_total = sum(monthlyFlow_hat),
      .groups = "drop"
    ) |>
    group_by(t, origin_macroregion, destination_macroregion) |>
    mutate(
      pi_hat = monthlyFlow_hat_total / sum(monthlyFlow_hat_total)
    ) |>
    ungroup() |>
    select(-monthlyFlow_hat_total) |>
    right_join(
      monthlyFlows_totals_hat[[idx]] |> select(-subscribers_monthlyFlow),
      by = c("t", "origin_macroregion", "destination_macroregion"),
      relationship = "many-to-many"
    ) |>
    mutate(
      monthlyFlow_hat_agesex = monthlyFlow_hat * pi_hat
    ) |>
    select(-monthlyFlow_hat)

  print("Monthly flows before national rescaling by age and sex")
  print(sum(monthlyFlows_agesex_hat[[idx]]$monthlyFlow_hat_agesex))
  print(sum(monthlyFlows_totals_hat[[idx]]$monthlyFlow_hat))

  # Re-scale to national population
  monthlyFlows_agesex_domestic_hat[[idx]] <- monthlyFlows_agesex_hat[[idx]] |>
    filter(destination_macroregion != "Abroad") |>
    left_join(
      national_pop |>
        filter(t == monthlyFlows_agesex[[idx]]$t[1]) |>
        select(t, a_name, s_name, pop_updated),
      by = c("t", "a_name", "s_name")
    ) |>
    group_by(t, a_name, s_name) |>
    mutate(
      scaling_factor = pop_updated / sum(monthlyFlow_hat_agesex),
      monthlyFlow_hat_calibrated = monthlyFlow_hat_agesex * scaling_factor
    ) |>
    ungroup() |>
    select(-pop_updated)

  print("Monthly flows before national rescaling by age and sex (domestic)")
  print(sum(monthlyFlows_agesex_domestic_hat[[idx]]$monthlyFlow_hat_agesex))
  print(monthlyFlows_agesex_hat[[idx]] |>
    filter(destination_macroregion != "Abroad") |>
    summarise(s = sum(monthlyFlow_hat_agesex)) |>
    pull(s))

  print("Monthly flows after national rescaling by age and sex (domestic)")
  print(sum(monthlyFlows_agesex_domestic_hat[[idx]]$monthlyFlow_hat_calibrated))
  print(national_pop |>
    filter(t == monthlyFlows_agesex[[idx]]$t[1]) |>
    summarise(s = sum(pop_updated)) |>
    pull(s))

  # Update penetration rate
  penetration_rate_agesexMacroregion[[idx + 1]] <- monthlyFlows_agesex_domestic_hat[[idx]] |>
    left_join(
      monthlyFlows_agesex[[idx]],
      by = c(
        "t", "s_name", "a_name", "origin_macroregion", "destination_macroregion", "origin_hromada",
        "destination_hromada", "origin_oblast", "destination_oblast", "origin_raion", "destination_raion"
      )
    ) |>
    group_by(t, a_name, s_name, origin_macroregion = destination_macroregion) |>
    summarise(
      origin_p = sum(subscribers_monthlyFlow, na.rm = T) / sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    mutate(t = t + months(1))

  penetration_rate[[idx + 1]] <- monthlyFlows_agesex_domestic_hat[[idx]] |>
    group_by(t, origin_hromada = destination_hromada, origin_raion = destination_raion, origin_oblast = destination_oblast, origin_macroregion = destination_macroregion) |>
    summarise(
      monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    left_join(
      monthlyFlows_totals[[idx]] |>
        group_by(t, origin_hromada = destination_hromada, origin_raion = destination_raion, origin_oblast = destination_oblast, origin_macroregion = destination_macroregion) |>
        summarise(
          subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
          .groups = "drop"
        ),
      by = c("t", "origin_hromada", "origin_raion", "origin_oblast", "origin_macroregion")
    ) |>
    mutate(
      origin_p = subscribers_monthlyFlow / monthlyFlow_hat_calibrated,
    ) |>
    mutate(t = t + months(1)) |>
    select(-subscribers_monthlyFlow, -monthlyFlow_hat_calibrated)
}

# create df for monthly total flows

penetration_rate_df <- bind_rows(penetration_rate)
monthlyFlows_totals_hat_df <- bind_rows(monthlyFlows_totals_hat)

penetration_rate_agesexMacroregion_df <- bind_rows(penetration_rate_agesexMacroregion)
# monthlyFlows_agesex_hat_df <- bind_rows(monthlyFlows_agesex_hat)
monthlyFlows_agesex_domestic_hat_df <- bind_rows(monthlyFlows_agesex_domestic_hat)

monthlyFlows_agesex_df <- bind_rows(monthlyFlows_agesex)

# Compute estimated stocks


monthlyFlows_agesex_hat_df_stocks <- monthlyFlows_agesex_domestic_hat_df |>
  group_by(t, a_name, s_name, destination_macroregion, destination_oblast, destination_raion, destination_hromada) |>
  summarise(
    monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
    pi_hat = mean(pi_hat),
    .groups = "drop"
  )

monthlyFlows_agesex_hat_df_stocks_oblast <- monthlyFlows_agesex_hat_df_stocks |>
  group_by(t, a_name, s_name, destination_oblast, destination_macroregion) |>
  summarise(
    monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
    pi_hat = mean(pi_hat),
    .groups = "drop"
  )


# 3.2 evaluate monthly flows induced model -------------------------------
# visualise missing age-sex combination

ggplot(agesex_geoCombination, aes(x = t, y = n_combinations, col = paste(a_name, s_name))) +
  geom_line() +
  theme_minimal() +
  facet_wrap(. ~ geo_level, scales = "free_y") +
  labs(col = "Age and sex", title = "Number of origin-destination combinations by geographical level", x = "Time", y = "Number of combinations")

# visualise raw users count

monthlyFlows_agesex_df |>
  filter(destination_hromada != "abroad") |>
  group_by(t, a_name, s_name) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ggplot(aes(x = t, y = subscribers_monthlyFlow, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(s_name ~ .) +
  labs(title = paste("Raw domestic subscribers"), x = "Time", y = "Subscribers")

# visualise penetration rate for totals

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


monthlyFlows_totals_oblast <- monthlyFlows_totals_hat |>
  # filter(destination_hromada != "Abroad") |>
  group_by(t, destination_oblast) |>
  summarise(
    monthlyFlow_hat = sum(monthlyFlow_hat)
  )

ggplot(monthlyFlows_totals_oblast |>
  filter(destination_oblast != "Abroad"), aes(x = t, y = monthlyFlow_hat, col = destination_oblast)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Monthly flows induced stocks: Oblast level", x = "Time", y = "Subscribers/Pop at t0")

# Visualise penetration rate
penetration_rate_agesexMacroregion_df |>
  ggplot(aes(x = t, y = origin_p, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(origin_macroregion ~ .) +
  geom_hline(yintercept = 1, color = "grey20") +
  labs(
    title = paste("Penetration rate by age and sex at macroregion - Monthly flows"),
    x = "Time", y = "Subscribers/Pop"
  )

## visualise interim monthly flows

ggplot(
  monthlyFlows_agesex_hat_df_stocks,
  aes(x = t, y = monthlyFlow_hat, col = a_name, linetype = s_name)
) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~destination_macroregion)

# visualise age-sex proportion
ggplot(
  monthlyFlows_agesex_hat_df_stocks_oblast,
  aes(x = t, y = pi_hat, col = a_name)
) +
  geom_point() +
  theme_minimal() +
  facet_grid(destination_macroregion ~ s_name) +
  labs(x = "", y = "Oblast average age-sex proportion", title = "Oblast average age-sex estimated proportion per macroregion")

# Visualise age-sex profile of flows to abroad
abroad_profile <- monthlyFlows_agesexMacroregion_hat_df |>
  filter(destination_macroregion == "Abroad" & origin_macroregion != "Abroad") |>
  group_by(t, a_name, s_name) |>
  summarise(
    monthlyFlow_hat = sum(monthlyFlow_hat),
    .groups = "drop"
  ) |>
  group_by(t) |>
  mutate(
    pi_hat = monthlyFlow_hat / sum(monthlyFlow_hat)
  ) |>
  left_join(
    borderCrossing |>
      rename(
        t = date,
        outflows = individuals
      ) |>
      filter(day(t) == 1)
  ) |>
  mutate(
    outflows = ifelse(is.na(outflows), 0, outflows),
    outflows = outflows * pi_hat
  )

ggplot(abroad_profile, aes(x = t, y = pi_hat, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Flows to Abroad", x = "", y = "Proportion")

# Visualise national population with border crossing  by age and sex
ggplot(national_pop, aes(x = t, y = pop_updated, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  labs(title = "National population", x = "")


# Visualise scaling_factor

monthlyFlows_agesex_domestic_hat_df |>
  distinct(t, a_name, s_name, scaling_factor) |>
  mutate(inverse_scaling_factor = 1 / scaling_factor) |>
  ggplot(aes(x = t, y = inverse_scaling_factor, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = paste("Inverse of the scaling factor by age and sex"),
    x = "Time", y = "Inverse of scaling factor=sum(estimated pop)/(pop-border_crossing)"
  ) +
  facet_grid(. ~ s_name)


# Viualise final estimates

ggplot(
  monthlyFlows_agesex_hat_df_stocks_oblast,
  aes(x = t, y = monthlyFlow_hat_calibrated, col = a_name, linetype = s_name)
) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~destination_oblast, scales = "free_y") +
  labs(title = "Monthly flows induced stocks: Oblast level", x = "Time", y = "Nationally-rescaled population")
