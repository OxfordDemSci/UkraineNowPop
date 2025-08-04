rm(list = ls())
gc()
library(tmap)

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
hromada <- read_csv(file.path(in_dir, "KSE-Loc-Data-Hub", "full_dataset.csv"))
master_index <- read_csv(file.path(out_dir, "ua_master_index.csv"))
hromada_geo <- st_read(file.path(out_dir, "ua_master_hromada.gpkg"))

# create output directories
dir.create(file.path(out_dir, "population_proxy", "mobile_phone", "figs"), recursive = T, showWarnings = F)

# prepare inputs
oblast_geo <- hromada_geo |>
  group_by(oblast_name_en) |>
  summarise(n = n())

# Link Meta and Vodafone geolabelling
hromada_geo_names <- hromada |>
  select(
    hromada_name, raion_name, raion_code,
    oblast_name, oblast_code, oblast_name_en,
    hromada_code
  ) |>
  mutate(
    oblast_name_en = case_when(
      oblast_name == "Луганська" ~ "Luhanska",
      oblast_name == "Донецька" ~ "Donetska",
      oblast_name_en == "Vinnytsia" ~ "Vinnytska",
      oblast_name_en == "Driproptrovska" ~ "Dnipropetrovska",
      oblast_name_en == "Zhytomir" ~ "Zhytomyrska",
      oblast_name_en == "Ivano-Frankivsk" ~ "Ivano-Frankivska",
      oblast_name_en == "Kherson" ~ "Khersonska",
      oblast_name_en == "Kirovograd" ~ "Kirovohradska",
      oblast_name_en == "Kyiv-oblast" ~ "Kyivska",
      oblast_name_en == "Luhanska" ~ "Luhanska",
      oblast_name_en == "Lviv" ~ "Lvivska",
      oblast_name_en == "Mykolayiv" ~ "Mykolaivska",
      oblast_name_en == "Odesa" ~ "Odeska",
      oblast_name_en == "Poltava" ~ "Poltavska",
      oblast_name_en == "Rivenska" ~ "Rivnenska",
      oblast_name_en == "Vonyn" ~ "Volynska",
      oblast_name_en == "Kharkiv" ~ "Kharkivska",
      oblast_name_en == "Khmelnitsk" ~ "Khmelnytska",
      oblast_name_en == "Cherkassy" ~ "Cherkaska",
      oblast_name_en == "Cherniveska" ~ "Chernivetska",
      oblast_name_en == "Chernigiv" ~ "Chernihivska",
      TRUE ~ oblast_name_en
    ),
  ) |>
  full_join(master_index |> distinct(i, i_key, ADM1_PCODE, i_name, macroregion), by = c("oblast_name_en" = "i_name")) |>
  mutate(
    hromada_code = ifelse(oblast_name_en == "Kyiv", "Kyiv", hromada_code),
    raion_code = ifelse(oblast_name_en == "Kyiv", "Kyiv", raion_code),
    raion_name = ifelse(oblast_name_en == "Kyiv", "Kyiv", raion_name)
  ) |>
  filter(!is.na(hromada_code))


# Stocks data -------------------------------------------------------------

stocks <- read_csv2(file.path(in_dir, "Vodafone", "Stocks.csv")) |>
  rename(hromada_code = "Hromada") |>
  left_join(hromada_geo_names) |>
  mutate(t = as.Date(month, "%d.%m.%y") + months(3)) |>
  mutate(
    oblast_name_en = ifelse(hromada_code == "abroad", "Abroad", oblast_name_en),
    macroregion = ifelse(hromada_code == "abroad", "Abroad", macroregion),
    hromada_code = ifelse(hromada_code == "abroad", "Abroad", hromada_code),
    s_name = ifelse(sex == "female", "F", "M"),
    a_name = str_replace(age, "-", "_"),
    a_name = str_replace(age, "\\+", "Plus")
  ) |>
  rename(
    subscribers_stock = subscribers
  )

stocks <- stocks |>
  select(t, hromada_code, hromada_name, raion_code, raion_name, oblast_name_en, ADM1_PCODE, macroregion, s_name, a_name, subscribers_stock)

write.csv(stocks, file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_stocks.csv"), row.names = F)

# Baseline flows ---------------------------------------------------------

baselineFlows <- read_csv2(file.path(in_dir, "Vodafone", "Baseline Flows.csv")) |>
  rename(
    destination_hromada = `Current home hromada`,
    origin_hromada = `Home hromada pre-invasion`
  ) |>
  mutate(t = as.Date(month, "%d.%m.%y") + months(3)) |>
  left_join(hromada_geo_names |>
    select(hromada_code, oblast_name_en, macroregion) |>
    rename(
      destination_oblast = oblast_name_en,
      destination_hromada = hromada_code,
      destination_macroregion = macroregion
    )) |>
  left_join(hromada_geo_names |>
    select(hromada_code, oblast_name_en, macroregion) |>
    rename(origin_oblast = oblast_name_en, origin_hromada = hromada_code, origin_macroregion = macroregion)) |>
  mutate(
    # Replace NAs in origin/destination
    origin_hromada = if_else(is.na(origin_hromada), "Unknown", origin_hromada),
    destination_hromada = if_else(is.na(destination_hromada), "Unknown", destination_hromada),
    origin_oblast = case_when(
      origin_hromada == "abroad" ~ "Abroad",
      origin_hromada == "Unknown" ~ "Unknown",
      TRUE ~ origin_oblast
    ),
    destination_oblast = case_when(
      destination_hromada == "abroad" ~ "Abroad",
      destination_hromada == "Unknown" ~ "Unknown",
      TRUE ~ destination_oblast
    ),
    origin_macroregion = case_when(
      origin_hromada == "abroad" ~ "Abroad",
      origin_hromada == "Unknown" ~ "Unknown",
      TRUE ~ origin_macroregion
    ),
    destination_macroregion = case_when(
      destination_hromada == "abroad" ~ "Abroad",
      destination_hromada == "Unknown" ~ "Unknown",
      TRUE ~ destination_macroregion
    ),
    origin_hromada = if_else(origin_hromada == "abroad", "Abroad", origin_hromada),
    destination_hromada = if_else(destination_hromada == "abroad", "Abroad", destination_hromada),
    s_name = ifelse(sex == "female", "F", "M"),
    a_name = str_replace(age, "-", "_"),
    a_name = str_replace(age, "\\+", "Plus")
  ) |>
  rename(
    subscribers_baselineFlow = subscribers
  )

baselineFlows <- baselineFlows |>
  select(t, origin_hromada, origin_oblast, origin_macroregion, destination_hromada, destination_oblast, destination_macroregion, s_name, a_name, subscribers_baselineFlow)

write_csv(baselineFlows, file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_baselineFlows.csv"))


# Monthly flows ----------------------------------------------------------

monthlyFlows <- read_csv2(file.path(in_dir, "Vodafone", "Monthly Flows.csv"))

monthlyFlows <- monthlyFlows |>
  rename(
    destination_hromada = `Current home hromada`,
    origin_hromada = `Home hromada last month`
  ) |>
  mutate(t = as.Date(month, "%d.%m.%y") + months(3)) |>
  left_join(hromada_geo_names |>
    select(hromada_code, oblast_name_en, macroregion) |>
    rename(
      destination_oblast = oblast_name_en,
      destination_hromada = hromada_code,
      destination_macroregion = macroregion
    )) |>
  left_join(hromada_geo_names |>
    select(hromada_code, oblast_name_en, macroregion) |>
    rename(origin_oblast = oblast_name_en, origin_hromada = hromada_code, origin_macroregion = macroregion)) |>
  mutate(
    # Replace NAs in origin/destination
    origin_hromada = if_else(is.na(origin_hromada), "Unknown", origin_hromada),
    destination_hromada = if_else(is.na(destination_hromada), "Unknown", destination_hromada),
    origin_oblast = case_when(
      origin_hromada == "abroad" ~ "Abroad",
      origin_hromada == "Unknown" ~ "Unknown",
      TRUE ~ origin_oblast
    ),
    destination_oblast = case_when(
      destination_hromada == "abroad" ~ "Abroad",
      destination_hromada == "Unknown" ~ "Unknown",
      TRUE ~ destination_oblast
    ),
    origin_macroregion = case_when(
      origin_hromada == "abroad" ~ "Abroad",
      origin_hromada == "Unknown" ~ "Unknown",
      TRUE ~ origin_macroregion
    ),
    destination_macroregion = case_when(
      destination_hromada == "abroad" ~ "Abroad",
      destination_hromada == "Unknown" ~ "Unknown",
      TRUE ~ destination_macroregion
    ),
    origin_hromada = if_else(origin_hromada == "abroad", "Abroad", origin_hromada),
    destination_hromada = if_else(destination_hromada == "abroad", "Abroad", destination_hromada),
    s_name = ifelse(sex == "female", "F", "M"),
    a_name = str_replace(age, "-", "_"),
    a_name = str_replace(age, "\\+", "Plus")
  ) |>
  rename(
    subscribers_monthlyFlow = subscribers
  )

monthlyFlows <- monthlyFlows |>
  select(t, origin_hromada, origin_oblast, origin_macroregion, destination_hromada, destination_oblast, destination_macroregion, s_name, a_name, subscribers_monthlyFlow)

write_csv(monthlyFlows, file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_monthlyFlows.csv"))

# Data assessment -------------------------------------------------------


# Vodafone geographical coverage

n_distinct(stocks$hromada_code)
n_distinct(hromada$hromada_code)

hromada_list <- stocks |>
  distinct(hromada_code) |>
  mutate(source = "in_vodafone") |>
  full_join(hromada |> select(ends_with("_en"), ends_with("_name"), hromada_code))

# Data consistency checks

hromada_geo <- hromada_geo |>
  left_join(stocks |>
    filter(t == min(stocks$t)) |>
    distinct(hromada_code) |>
    mutate(with_users = T))


map_missing <- tm_shape(hromada_geo) + tm_polygons(fill = "with_users") +
  tm_shape(oblast_geo) +
  tm_borders(lwd = 3)

tmap_save(
  map_missing,
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "map_missing.png")
)

# Hromada through time
stocks |>
  group_by(t, oblast_name_en) |>
  summarise(n_hromada = n_distinct(hromada_code)) |>
  left_join(hromada_geo |>
    group_by(oblast_name_en) |>
    summarise(n_hromada_true = n_distinct(hromada_code))) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(oblast_name_en ~ ., scales = "free_y")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "hromada_through_time.png"),
  width = 8,
  height = 6
)

# Raion through time
stocks |>
  group_by(t, oblast_name_en) |>
  summarise(n_raion = n_distinct(raion_code)) |>
  left_join(hromada_geo |>
    group_by(oblast_name_en) |>
    summarise(n_raion_true = n_distinct(raion_code))) |>
  ggplot(aes(x = t, y = n_raion)) +
  geom_line() +
  geom_line(aes(y = n_raion_true), col = "red") +
  facet_wrap(oblast_name_en ~ ., scales = "free_y")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "raion_through_time.png"),
  width = 8,
  height = 6
)


# User evolution

stocks |>
  group_by(t, oblast_name_en) |>
  summarise(n_users = sum(subscribers_stock)) |>
  ggplot(aes(x = t, y = n_users)) +
  geom_line() +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  labs(title = "Subscribers evolution")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "subscribers_evolution.png"),
  width = 8,
  height = 6
)

# subscribers evolution by age and sex

stocks |>
  filter(s_name == "M") |>
  group_by(t, oblast_name_en, a_name, s_name) |>
  summarise(n_users = sum(subscribers_stock)) |>
  ggplot(aes(x = t, y = n_users, col = a_name)) +
  geom_line() +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  labs(title = "Male subscribers")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "subscribers_evolution_male_by_age.png"),
  width = 8,
  height = 6
)


stocks |>
  filter(s_name == "F") |>
  group_by(t, oblast_name_en, a_name, s_name) |>
  summarise(n_users = sum(subscribers_stock)) |>
  ggplot(aes(x = t, y = n_users, col = a_name)) +
  geom_line() +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  labs(title = "Female subscribers")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "subscribers_evolution_female_by_age.png"),
  width = 8,
  height = 6
)


# Baseline flows assessment -----------------------------------------------


baselineFlows_hromada <- baselineFlows |>
  distinct(origin_hromada, destination_hromada)

n_distinct(baselineFlows_hromada$origin_hromada)
n_distinct(baselineFlows_hromada$destination_hromada)

# Data availibility

# Origin
baselineFlows |>
  group_by(t, origin_oblast) |>
  summarise(n_hromada = n_distinct(origin_hromada)) |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    group_by(oblast_name_en) |>
    summarise(n_hromada_true = n_distinct(hromada_code)) |>
    rename(origin_oblast = oblast_name_en)) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(origin_oblast ~ ., scales = "free_y") +
  labs(title = "Baseline flows origin")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "baseline_flows_origin.png"),
  width = 8,
  height = 6
)


# Destination
baselineFlows |>
  group_by(t, destination_oblast) |>
  summarise(n_hromada = n_distinct(destination_hromada)) |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    group_by(oblast_name_en) |>
    summarise(n_hromada_true = n_distinct(hromada_code)) |>
    rename(destination_oblast = oblast_name_en)) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  labs(title = "Baseline flows destination")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "baseline_flows_destination.png"),
  width = 8,
  height = 6
)

# Users evolution

baselineFlows_hromada <- baselineFlows |>
  group_by(t, origin_oblast, origin_hromada, destination_oblast, destination_hromada) |>
  summarise(subscribers_baselineFlow = sum(subscribers_baselineFlow))

baselineFlows_oblast <- baselineFlows_hromada |>
  group_by(t, origin_oblast, destination_oblast) |>
  summarise(subscribers_baselineFlow = sum(subscribers_baselineFlow))


ggplot(baselineFlows_oblast |>
  filter(origin_oblast == "Kyiv"), aes(x = t, y = subscribers_baselineFlow)) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y") +
  labs(title = "Kyiv -> other oblasts")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "baseline_flows_from_kyiv.png"),
  width = 8,
  height = 6
)


ggplot(baselineFlows_oblast |>
  filter(destination_oblast == "Kyiv"), aes(x = t, y = subscribers_baselineFlow)) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y") +
  labs(title = "Other oblasts -> Kyiv")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "baseline_flows_to_kyiv.png"),
  width = 8,
  height = 6
)


ggplot(baselineFlows_oblast |>
  filter(origin_oblast == "Unknown"), aes(x = t, y = subscribers_baselineFlow)) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "baseline_flows_from_unknown.png"),
  width = 8,
  height = 6
)


# Monthly flows ----------------------------------------------------------

# Data consistency checks

# Origin
monthlyFlows |>
  group_by(t, origin_oblast) |>
  summarise(n_hromada = n_distinct(origin_hromada)) |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    group_by(oblast_name_en) |>
    summarise(n_hromada_true = n_distinct(hromada_code)) |>
    rename(origin_oblast = oblast_name_en)) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(origin_oblast ~ ., scales = "free_y") +
  labs(title = "Monthly flows origin")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "monthly_flows_origin.png"),
  width = 8,
  height = 6
)


# Destination
monthlyFlows |>
  group_by(t, destination_oblast) |>
  summarise(n_hromada = n_distinct(destination_hromada)) |>
  left_join(hromada_geo |>
    st_drop_geometry() |>
    group_by(oblast_name_en) |>
    summarise(n_hromada_true = n_distinct(hromada_code)) |>
    rename(destination_oblast = oblast_name_en)) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  labs(title = "Monthly flows destination")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "monthly_flows_destination.png"),
  width = 8,
  height = 6
)



# Check coherence stocks - baseline flows - monthly flows ----------------


coherence_df <- baselineFlows_oblast |>
  ungroup() |>
  group_by(t, destination_oblast) |>
  summarise(subscribers_baselineFlow = sum(subscribers_baselineFlow)) |>
  rename(oblast_name_en = destination_oblast) |>
  full_join(
    stocks |>
      ungroup() |>
      group_by(t, oblast_name_en) |>
      summarise(subscribers_stock = sum(subscribers_stock))
  ) |>
  full_join(
    monthlyFlows |>
      ungroup() |>
      group_by(t, destination_oblast) |>
      rename(oblast_name_en = destination_oblast) |>
      summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow))
  )

coherence_df |>
  pivot_longer(starts_with("subscriber"), names_to = "type", values_to = "subscribers") |>
  group_by(t, type) |>
  summarise(subscribers = sum(subscribers)) |>
  ggplot(aes(x = t, y = subscribers, col = type)) +
  geom_line() +
  labs(title = "Coherence stocks - baseline flows - monthly flows") +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "coherence_stocks_flows.png"),
  width = 8,
  height = 6
)


# Age and sex missingness ------------------------------------------------
# Hromada through time by age and sex

stocks |>
  group_by(t, oblast_name_en, a_name, s_name) |>
  summarise(n_hromada = n_distinct(hromada_code)) |>
  left_join(hromada_geo |>
    group_by(oblast_name_en) |>
    summarise(n_hromada_true = n_distinct(hromada_code))) |>
  ggplot(aes(x = t, y = n_hromada, col = a_name, linetype = s_name)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "grey20") +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "hromada_through_time_by_agesex.png"),
  width = 8,
  height = 6
)


# At hromada level
stocks |>
  group_by(s_name, a_name, hromada_code) |>
  summarise(n = n_distinct(t)) |>
  ungroup() |>
  group_by(a_name, s_name) |>
  summarise(complete = sum(n == 40) / n() * 100)

# At raion level
stocks |>
  group_by(s_name, a_name, macroregion) |>
  summarise(n = n_distinct(t)) |>
  ungroup() |>
  group_by(a_name, s_name) |>
  summarise(complete = sum(n == 40) / n() * 100)

# Focus on missing hromada

hromada_missing <- stocks |>
  group_by(hromada_code) |>
  mutate(n_t = 40 - n_distinct(t)) |>
  distinct(hromada_code, n_t) |>
  ungroup()

ggplot(stocks |>
  right_join(
    hromada_missing |>
      filter(n_t > 0) |>
      mutate(n_t_label = fct_reorder(paste("Missing timestep:", n_t), n_t)) |>
      arrange(n_t)
  ) |>
  group_by(t, hromada_code, n_t, n_t_label) |>
  summarise(n_subscribers = sum(subscribers_stock)), aes(x = t, y = n_subscribers, col = hromada_code)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(n_t_label ~ ., scales = "free_y") +
  theme(legend.position = "None") +
  labs(title = "Number of subscribers per hromada that have missing data through time")

ggsave(
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "missing_data_hromada_through_time.png"),
  width = 8,
  height = 6
)


hromada_geo_missing <- hromada_geo |>
  left_join(
    hromada_missing
  )

map_hromada_geo_missing <- tm_shape(hromada_geo_missing) +
  tm_fill(
    fill = "n_t",
    fill.scale = tm_scale_intervals(4, breaks = c(0, 1, 10, 20, 38), values = c("grey95", "yellowgreen", "gold1", "red4"), labels = c("0", "1-10", "11-20", "21-38")),
    fill.legend = tm_legend(title = "Number of timesteps missing")
  )

tmap_save(
  map_hromada_geo_missing,
  filename = file.path(out_dir, "population_proxy", "mobile_phone", "figs", "map_hromada_geo_missing.png")
)
