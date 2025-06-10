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
baselineFlows <- read_csv(file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_baselineFlows.csv"))
broderCrossing <- read_csv(file.path(out_dir, "population_proxy", "crossing_borders", "dat_refugees.csv"))

stocks |>
  filter(t == as.Date("2021-11-01") & hromada_code != "abroad") |>
  summarise(sum(subscribers_stock))

# Create custom_geo based on which spatial information is complete on day 1 for each oblast
day1_infoHromada <- c(
  "Kirovohradska", "Kyiv", "Lvivska", "Odeska", "Poltavska"
)

day1_infoRaion <- c(
  "Cherkaska", "Chernihivska", "Chernivetska", "Dnipropetrovska",
  "Ivano-Frankivska", "Kyivska",
  "Khmelnytska", "Mykolaivska", "Rivnenska", "Sumska", "Ternopilska",
  "Volynska", "Vinnytska", "Zakarpatska", "Zhytomyrska"
)

day1_infoOblast <- c(
  "Kharkivska", "Khersonska", "Zaporizka"
)

hromada_geo <- hromada_geo |>
  mutate(
    custom_geo = case_when(
      oblast_name_en %in% day1_infoHromada ~ hromada_code,
      oblast_name_en %in% day1_infoRaion ~ raion_code,
      oblast_name_en %in% day1_infoOblast ~ oblast_name_en,
      TRUE ~ NA
    ),
    custom_geo_source = case_when(
      oblast_name_en %in% day1_infoHromada ~ "hromada_code",
      oblast_name_en %in% day1_infoRaion ~ "raion_code",
      oblast_name_en %in% day1_infoOblast ~ "oblast_name_en",
      TRUE ~ NA
    )
  )

custom_geo <- hromada_geo |>
  group_by(custom_geo) |>
  summarise()

tm_shape(custom_geo) +
  tm_borders()

stocks_total <- stocks |>
  select(hromada_code, s_name, a_name, subscribers_stock, t, ADM1_PCODE, macroregion) |>
  filter(hromada_code != "abroad") |>
  left_join(hromada_geo |> st_drop_geometry()) |>
  group_by(t, hromada_code, custom_geo, custom_geo_source, oblast_name_en, ADM1_PCODE, macroregion) |>
  summarise(subscribers_stock = sum(subscribers_stock))

hromada_pop <- hromada |>
  select(hromada_code, total_popultaion_2022) |>
  full_join(hromada_geo |> st_drop_geometry()) |>
  mutate(total_popultaion_2022 = ifelse(hromada_code == "Kyiv", 2952301, total_popultaion_2022)) |>
  filter(oblast_name_en != "Autonomous Republic of Crimea") |>
  group_by(custom_geo) |>
  summarise(total_popultaion_2022 = sum(total_popultaion_2022))



# 1. Model ------------------------------------------------------------------

# 1.1 compute penetration rate -----------------------------------------------

day1_pop <- stocks_total |>
  filter(t == as.Date("2021-11-01")) |>
  group_by(custom_geo, custom_geo_source, oblast_name_en, ADM1_PCODE, macroregion) |>
  summarise(
    subscribers_stock = sum(subscribers_stock)
  ) |>
  left_join(hromada_pop) |>
  mutate(
    p0 = subscribers_stock / total_popultaion_2022,
  )

sum(day1_pop$total_popultaion_2022)
sum(day1_pop$subscribers_stock)

# 1.2 apply penetration rate -------------------------------------------------

stocks_total <- stocks_total |>
  left_join(day1_pop |> select(custom_geo, p0)) |>
  mutate(stock_hat = subscribers_stock / p0)



# 2. Evaluation -------------------------------------------------------------


# 2.1 Evaluate penetration rate ----------------------------------------------

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

# Map
day1_pop_geo <- custom_geo |>
  left_join(day1_pop)

tm_shape(day1_pop_geo) +
  tm_polygons(
    fill = "p0",
    fill.scale = tm_scale_intervals(n = 5, style = "quantile")
  )

# Distribution

day1_pop |>
  ggplot(aes(x = p0)) +
  geom_histogram(bins = 100) +
  theme_minimal()

day1_pop |>
  pull(p0) |>
  summary()

# 2.2 Evaluate stocks-induced totals ---------------------------------------
national_pop <- broderCrossing |>
  mutate(t = date) |>
  filter(day(t) == 1) |>
  mutate(
    national_total = sum(day1_pop$total_popultaion_2022) - individuals
  )

stocks_national <- stocks_total |>
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
    stock_hat = sum(stock_hat)
  ) |>
  ggplot(aes(x = t, y = stock_hat)) +
  geom_line() +
  theme_minimal() +
  facet_grid(oblast_name_en ~ .) +
  theme(strip.text.y.right = element_text(angle = 0, vjust = 0.5, hjust = 1))
stocks_oblast
# Baseline flows induced estimation --------------------------------------

baselineFlows_totals <- baselineFlows |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    select(hromada_code, custom_geo) |>
    rename(
      origin_hromada = hromada_code,
      origin_custom_geo = custom_geo
    )) |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    select(hromada_code, custom_geo) |>
    rename(
      destination_hromada = hromada_code,
      destination_custom_geo = custom_geo
    )) |>
  mutate(
    origin_custom_geo = case_when(
      origin_hromada == "abroad" ~ "Abroad",
      origin_hromada == "Unknown" ~ "Unknown",
      TRUE ~ origin_custom_geo
    ),
    destination_custom_geo = case_when(
      destination_hromada == "abroad" ~ "Abroad",
      destination_hromada == "Unknown" ~ "Unknown",
      TRUE ~ destination_custom_geo
    )
  ) |>
  group_by(t, origin_custom_geo, destination_custom_geo, origin_macroregion, destination_macroregion) |>
  summarise(subscribers_baselineFlow = sum(subscribers_baselineFlow))

baselineFlows_totals <- lazy_dt(baselineFlows_totals)

baselineFlows_day1 <- baselineFlows_totals |>
  filter(t == "2021-12-01") |>
  as_tibble()

baselineFlows_day1 <- baselineFlows_day1 |>
  group_by(origin_custom_geo) |>
  summarise(
    subscribers_baselineFlow = sum(subscribers_baselineFlow)
  ) |>
  left_join(
    hromada_pop |>
      group_by(custom_geo) |>
      summarise(total_popultaion_2022 = sum(total_popultaion_2022)) |>
      rename(
        origin_custom_geo = custom_geo
      )
  ) |>
  mutate(
    p0 = subscribers_baselineFlow / total_popultaion_2022,
    p0 = case_when(
      is.na(p0) ~ median(p0, na.rm = T),
      TRUE ~ p0
    )
  )

baselineFlows_estimated <- baselineFlows_totals |>
  left_join(baselineFlows_day1 |>
    select(origin_custom_geo, p0)) |>
  mutate(
    baselineFlow_hat = subscribers_baselineFlow / p0
  ) |>
  as_tibble()

baselineFlows_stocks <- baselineFlows_estimated |>
  group_by(t, destination_custom_geo) |>
  summarise(
    baselineFlow_hat = sum(baselineFlow_hat)
  )




# Evaluate ---------------------------------------------------------------

# Evaluate penetration rate


# Distribution

baselineFlows_day1 |>
  filter(origin_custom_geo != "Khersonska") |>
  ggplot(aes(x = p0)) +
  geom_histogram(bins = 100) +
  theme_minimal()



# Evaluate flows-induced totals

baselineFlows_stocks_national <- baselineFlows_stocks |>
  filter(destination_custom_geo != "Abroad") |>
  group_by(t) |>
  summarise(
    baselineFlow_hat = sum(baselineFlow_hat)
  ) |>
  left_join(national_pop) |>
  mutate(
    national_total = ifelse(t < min(national_pop$t), sum(day1_pop$total_popultaion_2022), national_total),
    baselineFlow_hat_perc = (baselineFlow_hat - national_total) / national_total * 100
  )

gg_stocks_national <- ggplot(
  baselineFlows_stocks_national |> select(-national_total) |> pivot_longer(c(baselineFlow_hat, baselineFlow_hat_perc), names_to = "type"),
  aes(x = t, y = value)
) +
  geom_line() +
  geom_hline(data = tibble(type = c("baselineFlow_hat", "baselineFlow_hat_perc"), value = c(sum(day1_pop$total_popultaion_2022), 0)), aes(yintercept = value), color = "red", linetype = "dashed") +
  geom_line(data = national_pop |> mutate(type = "baselineFlow_hat"), aes(x = t, y = national_total), color = "red", linetype = "dashed") +
  labs(title = "Flows induced stocks: National level", ) +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = type_label)) +
  theme_minimal()
gg_stocks_national
