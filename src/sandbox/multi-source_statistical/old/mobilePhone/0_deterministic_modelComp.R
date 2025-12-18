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
stocks <- read_csv(file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_stocks.csv"))
# baselineFlows <- read_csv(file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_baselineFlows.csv"))
monthlyFlows <- read_csv(file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_monthlyFlows.csv"))
borderCrossing <- read_csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))

stocks |>
  filter(t == as.Date("2021-11-01")) |>
  summarise(sum(subscribers_stock))


stocks_total <- stocks |>
  group_by(t, hromada_code, oblast_name_en, ADM1_PCODE, macroregion) |>
  summarise(subscribers_stock = sum(subscribers_stock)) |>
  ungroup()

# Prepare baseline reference population
hromada_pop <- hromada |>
  select(hromada_code, total_popultaion_2022) |>
  full_join(hromada_geo |> st_drop_geometry()) |>
  mutate(total_popultaion_2022 = ifelse(hromada_code == "Kyiv", 2952301, total_popultaion_2022)) |>
  filter(oblast_name_en != "Autonomous Republic of Crimea")

national_pop <- borderCrossing |>
  mutate(t = date) |>
  filter(day(t) == 1) |>
  mutate(
    national_total = sum(day1_pop$total_popultaion_2022) - individuals
  )


# 1. Stocks induced Stocks ------------------------------------------------------------------

# compute penetration rate -----------------------------------------------

day1_pop <- stocks_total |>
  filter(t == as.Date("2021-11-01") & hromada_code != "abroad") |>
  group_by(hromada_code, oblast_name_en, ADM1_PCODE, macroregion) |>
  summarise(
    subscribers_stock = sum(subscribers_stock)
  ) |>
  left_join(hromada_pop) |>
  mutate(
    p0 = subscribers_stock / total_popultaion_2022,
  ) |>
  ungroup()

sum(day1_pop |> select(total_popultaion_2022) |> pull())
sum(day1_pop$subscribers_stock)

# apply penetration rate -------------------------------------------------

stocks_total <- stocks_total |>
  left_join(day1_pop |> select(hromada_code, p0)) |>
  mutate(
    # Fill missing penetration rate
    p0_imputed = ifelse(is.na(p0), median(p0, na.rm = T), p0),
    stock_hat = subscribers_stock / p0_imputed
  )


# 1.2 Stocks induced Evaluation -------------------------------------------------------------

# evaluate penetration rate ----------------------------------------------

# Per oblast

day1_pop_oblast <- day1_pop |>
  group_by(macroregion, oblast_name_en, ADM1_PCODE) |>
  summarise(across(c(total_popultaion_2022, subscribers_stock), sum)) |>
  mutate(p0 = subscribers_stock / total_popultaion_2022)

sum(day1_pop_oblast$total_popultaion_2022)
sum(day1_pop_oblast$subscribers_stock)

gg_penRate_oblast <- ggplot(day1_pop_oblast, aes(y = oblast_name_en, x = p0)) +
  geom_col(fill = "grey40") +
  # lims(x = c(0, 5)) +
  geom_vline(xintercept = 1, color = "orange", linetype = "dashed") +
  theme_minimal() +
  facet_grid(macroregion ~ ., scales = "free_y", space = "free_y") +
  labs(title = paste("Penetration rate"), x = "Pop/Users at t0")
gg_penRate_oblast


ggplot(day1_pop, aes(y = oblast_name_en, x = p0)) +
  geom_boxplot(col = "grey50", outlier.shape = NA) +
  geom_point(col = "grey20") +
  geom_vline(xintercept = 1, color = "orange", linetype = "dashed") +
  theme_minimal() +
  facet_grid(macroregion ~ ., scales = "free_y", space = "free_y") +
  labs(title = paste("Penetration rate at hromada level"), x = "Subscribers/Pop at t0", y = "")


# Map
day1_pop_geo <- hromada_geo |>
  left_join(day1_pop)

tm_shape(day1_pop_geo) +
  tm_polygons(
    fill = "p0",
    fill.scale = tm_scale_intervals(n = 5, breaks = c(0, 0.1, 0.4, 0.6, 1, 2.5))
  )

# Distribution

day1_pop |>
  ggplot(aes(x = p0)) +
  geom_histogram(bins = 100) +
  theme_minimal()

day1_pop |>
  pull(p0) |>
  summary()

# evaluate stocks-induced totals ---------------------------------------

stocks_national <- stocks_total |>
  filter(hromada_code != "abroad") |>
  group_by(t) |>
  summarise(
    subscribers_stock = sum(subscribers_stock),
    stock_hat = sum(stock_hat)
  ) |>
  left_join(national_pop) |>
  mutate(
    national_total = ifelse(t < min(national_pop$t), sum(day1_pop$total_popultaion_2022), national_total),
    stock_hat_perc = (stock_hat - national_total) / national_total * 100
  )

type_label <- c(
  `stock_hat` = "Total",
  `stock_hat_perc` = "Percentage (%)"
)

gg_stocks_national <- ggplot(stocks_national |> select(-national_total) |> pivot_longer(c(stock_hat, stock_hat_perc), names_to = "type"), aes(x = t, y = value)) +
  geom_line() +
  theme_minimal() +
  geom_hline(data = tibble(type = c("stock_hat", "stock_hat_perc"), value = c(sum(day1_pop$total_popultaion_2022), 0)), aes(yintercept = value), color = "red", linetype = "dashed") +
  geom_line(data = national_pop |> mutate(type = "stock_hat"), aes(x = t, y = national_total), color = "red", linetype = "dashed") +
  labs(title = "Stocks induced stocks: National level", ) +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = type_label))
gg_stocks_national


stocks_oblast <- stocks_total |>
  group_by(t, oblast_name_en, macroregion) |>
  summarise(
    subscribers_stock = sum(subscribers_stock),
    stock_hat = sum(stock_hat)
  )
ggplot(stocks_oblast, aes(x = t, y = stock_hat)) +
  geom_line() +
  theme_minimal() +
  facet_grid(oblast_name_en ~ .) +
  theme(strip.text.y.right = element_text(angle = 0, vjust = 0.5, hjust = 1))


# 2. Baseline flows induced model --------------------------------------

baselineFlows_totals <- baselineFlows |>
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
  summarise(subscribers_baselineFlow = sum(subscribers_baselineFlow))

baselineFlows_totals <- lazy_dt(baselineFlows_totals)

baselineFlows_day1 <- baselineFlows_totals |>
  filter(t == "2021-12-01") |>
  as_tibble()

baselineFlows_day1 <- baselineFlows_day1 |>
  group_by(origin_hromada, origin_oblast, origin_macroregion) |>
  summarise(
    subscribers_baselineFlow = sum(subscribers_baselineFlow)
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
    p0 = subscribers_baselineFlow / total_popultaion_2022,
    p0_imputed = case_when(
      is.na(p0) ~ median(p0, na.rm = T),
      TRUE ~ p0
    )
  )

baselineFlows_estimated <- baselineFlows_totals |>
  left_join(baselineFlows_day1 |>
    select(origin_hromada, p0_imputed)) |>
  group_by(origin_oblast) |>
  mutate(
    baselineFlow_hat = subscribers_baselineFlow / p0_imputed
  ) |>
  as_tibble()

baselineFlows_stocks <- baselineFlows_estimated |>
  group_by(t, destination_hromada) |>
  summarise(
    baselineFlow_hat = sum(baselineFlow_hat)
  )


# 2.2 Evaluate baseline flows induced model ---------------------------------------------------------------

# Evaluate penetration rate

# Distribution

baselineFlows_day1 |>
  ggplot(aes(x = p0)) +
  geom_histogram(bins = 100) +
  theme_minimal()

baselineFlows_day1 |>
  pull(p0) |>
  summary()

ggplot(baselineFlows_day1, aes(y = origin_oblast, x = p0_imputed)) +
  geom_boxplot(col = "grey50", outlier.shape = NA) +
  geom_point(col = "grey20") +
  geom_vline(xintercept = 1, color = "orange", linetype = "dashed") +
  theme_minimal() +
  facet_grid(origin_macroregion ~ ., scales = "free_y", space = "free_y") +
  labs(title = paste("Penetration rate at hromada level - Baseline flows"), x = "Subscribers/Pop at t0", y = "")


# Evaluate flows-induced totals

baselineFlows_flows_national <- baselineFlows_stocks |>
  filter(destination_hromada != "Abroad") |>
  group_by(t) |>
  summarise(
    baselineFlow_hat = sum(baselineFlow_hat)
  ) |>
  left_join(national_pop) |>
  mutate(
    national_total = ifelse(t < min(national_pop$t), sum(day1_pop$total_popultaion_2022), national_total),
    baselineFlow_hat_perc = (baselineFlow_hat - national_total) / national_total * 100
  )

gg_flows_national <- ggplot(
  baselineFlows_flows_national |> select(-national_total) |> pivot_longer(c(baselineFlow_hat, baselineFlow_hat_perc), names_to = "type"),
  aes(x = t, y = value)
) +
  geom_line() +
  geom_hline(data = tibble(type = c("baselineFlow_hat", "baselineFlow_hat_perc"), value = c(sum(day1_pop$total_popultaion_2022), 0)), aes(yintercept = value), color = "red", linetype = "dashed") +
  geom_line(data = national_pop |> mutate(type = "baselineFlow_hat"), aes(x = t, y = national_total), color = "red", linetype = "dashed") +
  labs(title = "Flows induced stocks: National level", ) +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = type_label)) +
  theme_minimal()
gg_flows_national


# 3. Monthly flows induced model ---------------------------------------

monthlyFlows_totals <- monthlyFlows |>
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

monthlyFlows_totals <- monthlyFlows_totals |>
  group_split(t)

penetration_rate <- list()
penetration_rate[[1]] <- monthlyFlows_totals[[1]] |>
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
  mutate(t = min(stocks$t) + months(1))

monthlyFlows_totals_hat <- list()

for (idx in 1:n_distinct(monthlyFlows$t)) {
  print(monthlyFlows_totals[[idx]]$t[1])
  monthlyFlows_totals_hat[[idx]] <- monthlyFlows_totals[[idx]] |>
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

penetration_rate <- bind_rows(penetration_rate)
monthlyFlows_totals_hat <- bind_rows(monthlyFlows_totals_hat)

# Evaluate
# 3.2 evaluate monthly flows induced model -------------------------------

# penetration rate

penetration_rate |>
  mutate(macroregion_oblast = paste0(origin_macroregion, " - ", origin_oblast)) |>
  ggplot(aes(x = t, y = origin_p, col = origin_hromada)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(macroregion_oblast ~ .) +
  theme(legend.position = "None") +
  geom_hline(yintercept = 1, color = "grey20") +
  labs(title = paste("Penetration rate at hromada level - Monthly flows"), x = "Time", y = "Subscribers/Pop")

# stocks total
monthlyFlows_totals_hat_national <- monthlyFlows_totals_hat |>
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
  geom_hline(data = tibble(type = c("monthlyFlow_hat", "monthlyFlow_hat_perc"), value = c(sum(day1_pop$total_popultaion_2022), 0)), aes(yintercept = value), color = "red", linetype = "dashed") +
  geom_line(data = national_pop |> mutate(type = "monthlyFlow_hat"), aes(x = t, y = national_total), color = "red", linetype = "dashed") +
  labs(title = "Monthly flows induced stocks: National level", ) +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = type_label)) +
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


# Comparison of oblast level estimation

oblast_pop_hat <- stocks_total |>
  group_by(t, oblast_name_en, macroregion) |>
  summarise(
    stock_hat = sum(stock_hat)
  ) |>
  rename(
    destination_oblast = oblast_name_en,
    destination_macroregion = macroregion
  ) |>
  full_join(
    baselineFlows_estimated |>
      group_by(t, destination_oblast, destination_macroregion) |>
      summarise(
        baselineFlow_hat = sum(baselineFlow_hat)
      )
  ) |>
  full_join(
    monthlyFlows_totals_hat |>
      group_by(t, destination_oblast, destination_macroregion) |>
      summarise(
        monthlyFlow_hat = sum(monthlyFlow_hat)
      )
  ) |>
  pivot_longer(c(stock_hat, baselineFlow_hat, monthlyFlow_hat), names_to = "method")

ggplot(
  oblast_pop_hat |>
    filter(destination_oblast != "Abroad"),
  aes(x = t, y = value, col = method)
) +
  geom_line() +
  theme_minimal() +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  labs(title = "Comparison of oblast level estimated stocks", x = "Time", y = "Subscribers/Pop at t0")

ggplot(
  oblast_pop_hat |>
    filter(destination_oblast == "Abroad"),
  aes(x = t, y = value, col = method)
) +
  geom_line() +
  geom_line(data = national_pop, aes(x = date, y = individuals), color = "black") +
  theme_minimal()


# Comparison of penetration rate

penetration_rate_comp <- bind_rows(
  penetration_rate |>
    select(t, origin_hromada, origin_p) |>
    rename(p0 = origin_p) |>
    mutate(source = "Monthly flows"),
  day1_pop |>
    select(hromada_code, p0_imputed) |>
    rename(p0 = p0_imputed, ) |>
    mutate(source = "Stocks"),
  baselineFlows_day1 |>
    select(origin_hromada, p0_imputed) |>
    rename(p0 = p0_imputed) |>
    mutate(source = "Baseline flows")
)
