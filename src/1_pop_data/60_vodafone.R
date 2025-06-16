rm(list = ls())
gc()
library(tmap)

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
hromada <- read_csv(file.path(in_dir, "KSE-Loc-Data-Hub", "full_dataset.csv"))
master_index <- read_csv(file.path(out_dir, "ua_master_index.csv"))
hromada_geo <- st_read(file.path(out_dir, "ua_master_hromada.gpkg"))

oblast_geo <- hromada_geo |>
  group_by(oblast_name_en) |>
  summarise(n = n())
# Link Meta and Vodafone geolabelling
hromada_geo <- hromada |>
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
    raion_name = ifelse(oblast_name_en == "Kyiv", "Київ", raion_name)
  ) |>
  filter(!is.na(hromada_code))


# Stocks data -------------------------------------------------------------

stocks <- read_csv2(file.path(in_dir, "Vodafone", "Stocks.csv")) |>
  rename(hromada_code = "Hromada") |>
  left_join(hromada_geo) |>
  mutate(t = as.Date(month, "%d.%m.%y")) |>
  mutate(
    oblast_name_en = ifelse(hromada_code == "abroad", "Abroad", oblast_name_en),
    macroregion = ifelse(hromada_code == "abroad", "Abroad", macroregion),
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
  mutate(t = as.Date(month, "%d.%m.%y")) |>
  left_join(hromada_geo |>
    select(hromada_code, oblast_name_en, macroregion) |>
    rename(
      destination_oblast = oblast_name_en,
      destination_hromada = hromada_code,
      destination_macroregion = macroregion
    )) |>
  left_join(hromada_geo |>
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



# Data assessment -------------------------------------------------------


# Vodafone geographical coverage

n_distinct(stocks$hromada_code)
n_distinct(hromada$hromada_code)

hromada_list <- stocks |>
  distinct(hromada_code) |>
  mutate(source = "in_vodafone") |>
  full_join(hromada |> select(ends_with("_en"), ends_with("_name"), hromada_code))

hromada_list |>
  group_by(oblast_name, oblast_name_en) |>
  summarise(
    n_hromada = n(),
    n_hromada_inVodafone = sum(source == "in_vodafone", na.rm = T),
    n_hromada_missing = n_hromada - n_hromada_inVodafone
  ) |>
  filter(n_hromada_missing > 0) |>
  View()

# Data consistency checks

hromada_geo <- hromada_geo |>
  left_join(stocks |>
    filter(t == "2021-11-01") |>
    distinct(hromada_code) |>
    mutate(with_users = T))


tm_shape(hromada_geo) + tm_polygons(fill = "with_users") +
  tm_shape(oblast_geo) +
  tm_borders(lwd = 3)


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


# User evolution

stocks |>
  group_by(t, oblast_name_en) |>
  summarise(n_users = sum(subscribers)) |>
  ggplot(aes(x = t, y = n_users)) +
  geom_line() +
  facet_wrap(oblast_name_en ~ ., scales = "free_y")

# subscribers evolution by age and sex

stocks |>
  filter(sex == "male") |>
  group_by(t, oblast_name_en, age, sex) |>
  summarise(n_users = sum(subscribers)) |>
  ggplot(aes(x = t, y = n_users, col = age)) +
  geom_line() +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  labs(title = "Male subscribers")

stocks |>
  filter(sex == "female") |>
  group_by(t, oblast_name_en, age, sex) |>
  summarise(n_users = sum(subscribers)) |>
  ggplot(aes(x = t, y = n_users, col = age)) +
  geom_line() +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  labs(title = "Female subscribers")


# Baseline flows assessment -----------------------------------------------


baselineFlows_hromada <- baselineFlows |>
  distinct(origin, destination)

n_distinct(baselineFlows_hromada$origin)
n_distinct(baselineFlows_hromada$destination)

# Data availibility

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

# Users evolution
a <- baselineFlows |>
  mutate(t = as.Date(month, "%d.%m.%y"))
baselineFlows <- baselineFlows
baselineFlows_hromada <- baselineFlows |>
  group_by(t, origin_oblast, origin, destination_oblast, destination) |>
  summarise(subscribers = sum(subscribers))

baselineFlows_oblast <- baselineFlows_hromada |>
  group_by(t, origin_oblast, destination_oblast) |>
  summarise(subscribers = sum(subscribers))


ggplot(baselineFlows_oblast |>
  filter(origin_oblast == "Kyiv"), aes(x = t, y = subscribers)) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y") +
  labs(title = "Kyiv -> other oblasts")

ggplot(baselineFlows_oblast |>
  filter(destination_oblast == "Kyiv"), aes(x = t, y = subscribers)) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y") +
  labs(title = "Other oblasts -> Kyiv")


ggplot(baselineFlows_oblast |>
  filter(origin_oblast == "Unknown"), aes(x = t, y = subscribers)) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y")


# Check coherence flow-stock
baselineFlows_oblast_stocks <- baselineFlows_oblast |>
  ungroup() |>
  group_by(t, destination_oblast) |>
  summarise(subscribers_flows = sum(subscribers)) |>
  rename(oblast_name_en = destination_oblast) |>
  full_join(
    stocks |>
      ungroup() |>
      group_by(t, oblast_name_en) |>
      summarise(subscribers_stock = sum(subscribers))
  ) |>
  mutate(
    diff = subscribers_flows - subscribers_stock
  )

ggplot(
  baselineFlows_oblast_stocks |> group_by(t) |>
    summarise(subscribers = sum(subscribers_flows, na.rm = T)) |>
    mutate(type = "subscribers_flows") |>
    bind_rows(
      stocks |> group_by(t) |>
        summarise(subscribers = sum(subscribers)) |>
        mutate(type = "subscribers_stock")
    ), aes(x = t, y = subscribers, col = type)
) +
  geom_line() +
  geom_hline(yintercept = 38676840 / 3, col = "red", lty = 2)

ggplot(baselineFlows_oblast_stocks, aes(x = t, y = diff)) +
  geom_line() +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  geom_hline(yintercept = 0, col = "red", lty = 2)
