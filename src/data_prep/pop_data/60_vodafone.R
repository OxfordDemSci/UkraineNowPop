# This script prepares Vodafone mobile network data for Ukraine

rm(list = ls())
gc()
library(tmap)
tmap_options(component.autoscale = F)
library(data.table)

# Load required helpers
source(file.path(here::here(), "src", "helpers", "R_helpers", "generic.R"))

# load data
hromada <- read_csv(file.path(in_dir, "KSE-Loc-Data-Hub", "full_dataset.csv"))
master_index <- read_csv(file.path(out_dir, "ua_master_index.csv"))
hromada_geo <- st_read(file.path(out_dir, "ua_master_hromada.gpkg"))

# create output directories
dir.create(
  file.path(out_dir, "population_proxy", "mobile_phone", "figs"),
  recursive = T,
  showWarnings = F
)

# prepare inputs
oblast_geo <- hromada_geo |>
  group_by(oblast_name_en) |>
  summarise(n = n())

# Link Meta and Vodafone geolabelling
hromada_geo_names <- hromada |>
  select(
    hromada_name,
    raion_name,
    raion_code,
    oblast_name,
    oblast_code,
    oblast_name_en,
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
  full_join(
    master_index |> distinct(i, i_key, ADM1_PCODE, i_name, macroregion),
    by = c("oblast_name_en" = "i_name")
  ) |>
  mutate(
    hromada_code = ifelse(oblast_name_en == "Kyiv", "Kyiv", hromada_code),
    raion_code = ifelse(oblast_name_en == "Kyiv", "Kyiv", raion_code),
    raion_name = ifelse(oblast_name_en == "Kyiv", "Kyiv", raion_name)
  ) |>
  filter(!is.na(hromada_code))


# Stocks data -------------------------------------------------------------
stocks_list <- list.files(
  file.path(in_dir, "Vodafone"),
  pattern = "Stocks",
  full.names = T
)

names(stocks_list) <- c(
  "without_ngct1",
  "without_ngct2",
  "with_ngct",
  "with_ngct_postJune"
)

stocks <- lapply(stocks_list, function(x) {
  read_csv2(x) |>
    mutate(file = str_split(x, "/")[[1]][8]) |>
    rename(hromada_code = "Hromada") |>
    left_join(hromada_geo_names) |>
    mutate(t = as.Date(month, "%d.%m.%y") + months(3)) |>
    mutate(
      oblast_name_en = ifelse(
        hromada_code == "abroad",
        "Abroad",
        oblast_name_en
      ),
      macroregion = ifelse(hromada_code == "abroad", "Abroad", macroregion),
      hromada_code = ifelse(hromada_code == "abroad", "Abroad", hromada_code),
      s_name = ifelse(sex == "female", "F", "M"),
      a_name = str_replace(age, "-", "_"),
      a_name = str_replace(age, "\\+", "Plus")
    ) |>
    rename(
      subscribers_stock = subscribers
    )
})

stocks <- stocks[["without_ngct1"]] |>
  bind_rows(stocks[["without_ngct2"]]) |>
  full_join(
    stocks[["with_ngct"]] |>
      filter(
        !hromada_code %in% unique(stocks[["without_ngct1"]]$hromada_code)
      ) |>
      select(-file)
  ) |>
  filter(t < as.Date('2025-06-01')) |>
  bind_rows(
    stocks[["with_ngct_postJune"]] |>
      filter(t >= as.Date('2025-06-01'))
  ) |>
  mutate(
    territory = ifelse(is.na(file), "ngct", "gct")
  )

stocks <- stocks |>
  select(
    t,
    hromada_code,
    hromada_name,
    raion_code,
    raion_name,
    oblast_name_en,
    ADM1_PCODE,
    macroregion,
    territory,
    s_name,
    a_name,
    subscribers_stock
  )

ngct_mapping <- stocks |>
  distinct(
    hromada_code,
    hromada_name,
    raion_code,
    raion_name,
    oblast_name_en,
    ADM1_PCODE,
    macroregion,
    territory
  )

write.csv(
  ngct_mapping,
  file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "vodafone_ngct_mapping.csv"
  ),
  row.names = F
)
write.csv(
  stocks,
  file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_stocks.csv"),
  row.names = F
)

# Baseline flows ---------------------------------------------------------

baselineFlows_preJune <- read_csv2(file.path(
  in_dir,
  "Vodafone",
  "Baseline Flows_050925.csv"
))

baselineFlows_preJune <- baselineFlows_preJune |>
  mutate(t = as.Date(month, "%d.%m.%y") + months(3)) |>
  filter(t < as.Date('2025-06-01'))

baselineFlows_postJune <- read_csv2(file.path(
  in_dir,
  "Vodafone",
  "Baseline Flows_201125.csv"
)) |>
  mutate(t = as.Date(month, "%d.%m.%y") + months(3))

baselineFlows <- bind_rows(
  baselineFlows_preJune,
  baselineFlows_postJune
) |>
  rename(
    destination_hromada = `Current home hromada`,
    origin_hromada = `Home hromada pre-invasion`
  ) |>
  left_join(
    hromada_geo_names |>
      select(hromada_code, oblast_name_en, macroregion) |>
      rename(
        destination_oblast = oblast_name_en,
        destination_hromada = hromada_code,
        destination_macroregion = macroregion
      )
  ) |>
  left_join(
    hromada_geo_names |>
      select(hromada_code, oblast_name_en, macroregion) |>
      rename(
        origin_oblast = oblast_name_en,
        origin_hromada = hromada_code,
        origin_macroregion = macroregion
      )
  ) |>
  mutate(
    # Replace NAs in origin/destination
    origin_hromada = if_else(is.na(origin_hromada), "Unknown", origin_hromada),
    destination_hromada = if_else(
      is.na(destination_hromada),
      "Unknown",
      destination_hromada
    ),
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
    origin_hromada = if_else(
      origin_hromada == "abroad",
      "Abroad",
      origin_hromada
    ),
    destination_hromada = if_else(
      destination_hromada == "abroad",
      "Abroad",
      destination_hromada
    ),
    s_name = ifelse(sex == "female", "F", "M"),
    a_name = str_replace(age, "-", "_"),
    a_name = str_replace(age, "\\+", "Plus")
  ) |>
  rename(
    subscribers_baselineFlow = subscribers
  )

baselineFlows <- baselineFlows |>
  ungroup() |>
  left_join(
    ngct_mapping |>
      select(
        origin_hromada = hromada_code,
        origin_oblast = oblast_name_en,
        origin_territory = territory
      )
  ) |>
  left_join(
    ngct_mapping |>
      select(
        destination_hromada = hromada_code,
        destination_oblast = oblast_name_en,
        destination_territory = territory
      )
  )

baselineFlows <- baselineFlows |>
  select(
    t,
    origin_hromada,
    origin_oblast,
    origin_macroregion,
    origin_territory,
    destination_hromada,
    destination_oblast,
    destination_macroregion,
    destination_territory,
    s_name,
    a_name,
    subscribers_baselineFlow
  )

write_csv(
  baselineFlows,
  file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "vodafone_baselineFlows.csv"
  )
)

monthlyFlows_ngct <- baselineFlows |>
  filter(origin_territory == "ngct") |>
  group_by(t, origin_territory, destination_territory, s_name, a_name) |>
  summarise(
    subscribers_baselineFlow = sum(subscribers_baselineFlow),
    .groups = "drop"
  ) |>
  left_join(
    stocks |>
      filter(territory == "ngct") |>
      group_by(s_name, a_name) |>
      summarise(subscribers_stock = sum(subscribers_stock), .groups = "drop")
  ) |>
  mutate(
    subscribers_monthlyFlow = subscribers_stock - subscribers_baselineFlow,
    destination_raion = "ngct",
    destination_hromada = "ngct",
    destination_oblast = "ngct",
    destination_macroregion = "ngct",
    destination_territory = "ngct",
    origin_hromada = "ngct",
    origin_raion = "ngct",
    origin_oblast = "ngct",
    origin_macroregion = "ngct"
  ) |>
  select(
    t,
    origin_hromada,
    origin_raion,
    origin_oblast,
    origin_macroregion,
    origin_territory,
    destination_hromada,
    destination_raion,
    destination_oblast,
    destination_macroregion,
    destination_territory,
    s_name,
    a_name,
    subscribers_monthlyFlow
  )

write_csv(
  monthlyFlows_ngct,
  file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "vodafone_monthlyFlows_ngctToNgct.csv"
  )
)


# Monthly flows ----------------------------------------------------------
monthlyFlows_list <- list.files(
  file.path(in_dir, "Vodafone"),
  pattern = "Monthly",
  full.names = T
)

monthlyFlows <- lapply(monthlyFlows_list, read_csv2) |>
  bind_rows()

monthlyFlows <- monthlyFlows |>
  rename(
    destination_hromada = `Current home hromada`,
    origin_hromada = `Home hromada last month`
  ) |>
  mutate(t = as.Date(month, "%d.%m.%y") + months(3)) |>
  left_join(
    hromada_geo_names |>
      select(hromada_code, oblast_name_en, macroregion) |>
      rename(
        destination_oblast = oblast_name_en,
        destination_hromada = hromada_code,
        destination_macroregion = macroregion
      )
  ) |>
  left_join(
    hromada_geo_names |>
      select(hromada_code, oblast_name_en, macroregion) |>
      rename(
        origin_oblast = oblast_name_en,
        origin_hromada = hromada_code,
        origin_macroregion = macroregion
      )
  ) |>
  mutate(
    # Replace NAs in origin/destination
    origin_hromada = ifelse(is.na(origin_hromada), "Unknown", origin_hromada),
    destination_hromada = ifelse(
      is.na(destination_hromada),
      "Unknown",
      destination_hromada
    ),
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
    origin_hromada = ifelse(
      origin_hromada == "abroad",
      "Abroad",
      origin_hromada
    ),
    destination_hromada = ifelse(
      destination_hromada == "abroad",
      "Abroad",
      destination_hromada
    ),
    s_name = ifelse(sex == "female", "F", "M"),
    a_name = str_replace(age, "-", "_"),
    a_name = str_replace(age, "\\+", "Plus")
  ) |>
  rename(
    subscribers_monthlyFlow = subscribers
  )

monthlyFlows <- monthlyFlows |>
  select(
    t,
    origin_hromada,
    origin_oblast,
    origin_macroregion,
    destination_hromada,
    destination_oblast,
    destination_macroregion,
    s_name,
    a_name,
    subscribers_monthlyFlow
  )

write_csv(
  monthlyFlows,
  file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "vodafone_monthlyFlows.csv"
  )
)

# Remove missing combination (age, sex, hromada)

# compute missing hromadas
timesteps_total <- length(unique(monthlyFlows$t))
monthlyFlows <- data.table(monthlyFlows)
missing_hromadas <- monthlyFlows[,
  .(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)),
  by = .(t, destination_hromada)
][,
  .(n_timesteps = timesteps_total - .N),
  by = destination_hromada
][
  n_timesteps > 0
]


dropping_hromadas <- missing_hromadas |>
  filter(n_timesteps > 3)

# imputation
# select hromadas with only three missing timesteps
imputed_hromadas <- missing_hromadas |>
  filter(n_timesteps <= 3)

# check the minimum date without data
date_to_impute_hromadas_df <- monthlyFlows |>
  group_by(t, destination_hromada) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ungroup() |>
  filter(destination_hromada %in% imputed_hromadas$destination_hromada) |>
  complete(
    t = seq(
      min(monthlyFlows$t, na.rm = T),
      max(monthlyFlows$t),
      by = "1 month"
    ),
    nesting(destination_hromada)
  ) |>
  filter(is.na(subscribers_monthlyFlow))

imputed_hromadas_df <- missing_hromadas |>
  filter(n_timesteps < 4) |>
  left_join(
    date_to_impute_hromadas_df |>
      group_by(destination_hromada) |>
      summarise(t = min(t))
  )

# replicate last observed date
imputed_hromadas_df <- imputed_hromadas_df |>
  mutate(
    t = t - months(1)
  ) |>
  left_join(
    monthlyFlows
  ) |>
  uncount(n_timesteps, .id = "month") |>
  mutate(
    t = t + months(month)
  ) |>
  select(-month)

# combine imputed with available data
monthlyFlows_imputed <- bind_rows(
  monthlyFlows,
  imputed_hromadas_df
) |>
  filter(!origin_hromada %in% dropping_hromadas$destination_hromada) |>
  filter(!destination_hromada %in% dropping_hromadas$destination_hromada)


fwrite(
  monthlyFlows_imputed,
  file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "vodafone_monthlyFlows_imputed.csv"
  )
)

# Data assessment -------------------------------------------------------

# Monthly flows data availability ----------------------------------------------------------

# Destination
hromada_available <- monthlyFlows |>
  group_by(destination_hromada) |>
  summarise(n_timesteps = n_distinct(t))
n_timestep <- n_distinct(monthlyFlows$t)

n_hromada_available_total <- hromada_available |>
  filter(destination_hromada != "Abroad") |>
  filter(n_timesteps == n_distinct(monthlyFlows$t)) |>
  nrow()

hromada_oblast_available <- monthlyFlows |>
  group_by(t, destination_oblast) |>
  summarise(n_hromada = n_distinct(destination_hromada)) |>
  left_join(
    hromada_geo |>
      st_drop_geometry() |>
      group_by(oblast_name_en) |>
      summarise(n_hromada_true = n_distinct(hromada_code)) |>
      rename(destination_oblast = oblast_name_en)
  )

hromada_oblast_available |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  labs(
    title = paste0(
      "Hromada availibility: ",
      n_hromada_available_total,
      " present for the full period"
    )
  )

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "monthly_flows_hromada_available_timeline.png"
  ),
  width = 8,
  height = 6
)

# Missing timesteps through time
missing_hromadas <- hromada_available |>
  filter(n_timesteps < n_distinct(monthlyFlows$t))

missing_hromadas_df <- monthlyFlows |>
  group_by(
    t,
    destination_hromada,
    destination_oblast,
    destination_macroregion
  ) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  filter(destination_hromada %in% missing_hromadas$destination_hromada) |>
  ungroup() |>
  complete(
    t = seq(
      min(monthlyFlows$t, na.rm = T),
      max(monthlyFlows$t),
      by = "1 month"
    ),
    nesting(destination_hromada, destination_oblast, destination_macroregion)
  ) |>
  mutate(
    missing = ifelse(is.na(subscribers_monthlyFlow), T, F),
    subscribers_monthlyFlow = ifelse(
      is.na(subscribers_monthlyFlow),
      0,
      subscribers_monthlyFlow
    )
  ) |>
  left_join(
    missing_hromadas |>
      mutate(
        missing_label = paste0(n_timestep - n_timesteps, " timesteps missing\n")
      ),
    by = "destination_hromada"
  ) |>
  group_by(missing_label) |>
  mutate(
    missing_label = paste0(
      missing_label,
      n_distinct(destination_hromada),
      " hromadas"
    )
  )

gg_missing <- ggplot(
  missing_hromadas_df,
  aes(
    x = t,
    y = subscribers_monthlyFlow,
    col = destination_hromada,
    alpha = missing
  )
) +
  geom_point() +
  facet_wrap(fct_reorder(missing_label, n_timestep - n_timesteps) ~ .) +
  theme_minimal() +
  theme(legend.position = "None") +
  labs(title = "Timesteps missing for each hromada", x = "")
gg_missing

ggsave(
  file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "monthly_flows_hromada_missing.png"
  ),
  gg_missing,
  w = 8,
  height = 6
)

## Unique combination --------------------------------------------------------
monthlyFlows_wide <- monthlyFlows |>
  filter(a_name != "0-17") |>
  filter(a_name != "65Plus") |>
  pivot_wider(
    names_from = t,
    values_from = subscribers_monthlyFlow
  )

monthlyFlows_wide_s <- monthlyFlows_wide |>
  summarise(
    across(
      starts_with("202"),
      list(
        missing = ~ sum(is.na(.)),
        present = ~ sum(!is.na(.))
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("date", "type"),
    names_sep = "_"
  ) |>
  mutate(date = as.Date(date))

ggplot(monthlyFlows_wide_s, aes(x = date, y = value, fill = type)) +
  geom_col(position = "stack") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Evolution of combinations (origin hromada, destination hromada, age, sex)",
    subtitle = paste0(
      "There is ",
      scales::comma(nrow(monthlyFlows_wide)),
      " unique combinations in total"
    ),
    x = "",
    y = "Unique combinations",
    fill = "Combination"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(values = c("missing" = "grey", "present" = "darkgreen"))


# Monthly flows users evolution ----------------------------------------

monthlyFlows_imputed |>
  group_by(t) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    #subscribers_monthlyFlow_ = sum(subscribers_monthlyFlow_)
  ) |>
  ggplot(aes(x = t, y = subscribers_monthlyFlow)) +
  geom_line() +
  #geom_line(aes(y = subscribers_monthlyFlow_), col = "red") +
  theme_minimal() +
  labs(title = "Evolution of subscribers across time")

monthlyFlows |>
  group_by(t) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
  ) |>
  ggplot(aes(x = t, y = subscribers_monthlyFlow)) +
  geom_line() +
  geom_line(
    data = monthlyFlows_imputed |>
      group_by(t) |>
      summarise(
        subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
      ),
    aes(y = subscribers_monthlyFlow),
    col = "red"
  ) +
  theme_minimal() +
  labs(title = "Evolution of subscribers across time")

monthlyFlows_ngct |>
  group_by(t) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    #subscribers_monthlyFlow_ = sum(subscribers_monthlyFlow_)
  ) |>
  ggplot(aes(x = t, y = subscribers_monthlyFlow)) +
  geom_line() +
  #geom_line(aes(y = subscribers_monthlyFlow_), col = "red") +
  theme_minimal() +
  labs(title = "Evolution of subscribers ngct -> ngct across time")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "subscribers_evolution.png"
  ),
  width = 8,
  height = 6
)

monthlyFlows |>
  group_by(t, destination_oblast, destination_macroregion) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ggplot(aes(x = t, y = subscribers_monthlyFlow, col = destination_oblast)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(. ~ destination_macroregion, scales = "free") +
  labs(title = "Evolution of subscribers by oblast across time")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "subscribers_evolution_oblast.png"
  ),
  width = 8,
  height = 6
)

# Data availability

stocks |>
  filter(
    t == min(t)
  ) |>
  group_by(territory, oblast_name_en) |>
  summarise(n_hromada = n_distinct(hromada_code)) |>
  pivot_wider(names_from = territory, values_from = n_hromada) |>
  left_join(
    hromada_geo |>
      st_drop_geometry() |>
      group_by(oblast_name_en) |>
      summarise(n_hromada_true = n_distinct(hromada_code))
  ) |>
  mutate(
    ngct = ifelse(is.na(ngct), 0, ngct),
    gct = ifelse(is.na(gct), 0, gct),
    n_mising = n_hromada_true - gct - ngct
  ) |>
  select(-n_hromada_true) |>
  pivot_longer(
    cols = c(ngct, gct, n_mising),
    names_to = "territory",
    values_to = "n_hromada"
  ) |>
  ggplot(aes(y = oblast_name_en, x = n_hromada, fill = territory)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Data availability", y = "", x = "Number of hromada") +
  scale_fill_manual(
    values = c("gct" = "darksalmon", "ngct" = "darkgreen", "n_mising" = "grey")
  )

# map ngct/gct

df_map_ngct <- hromada_geo |>
  left_join(
    stocks |>
      filter(
        t == min(t)
      ) |>
      group_by(
        hromada_code,
        hromada_name,
        raion_code,
        raion_name,
        oblast_name_en,
        ADM1_PCODE,
        macroregion,
        territory
      ) |>
      summarise(subscribers_stock = sum(subscribers_stock)) |>
      ungroup()
  ) |>
  mutate(
    territory = ifelse(hromada_code == "Kyiv", "gct", territory),
    territory = ifelse(is.na(territory), "missing", territory)
  )

tmap_mode("view")
tm_shape(df_map_ngct) +
  tm_fill(
    col = "territory",
    palette = c("gct" = "darksalmon", "ngct" = "darkgreen", "missing" = "grey"),
    alpha = 0.4,
    title = "Territory"
  ) +
  tm_basemap("CartoDB.Positron")

ggplot(df_map_ngct, aes(fill = territory, geometry = geom)) +
  geom_sf() +
  theme_void() +
  scale_fill_manual(
    values = c("gct" = "darksalmon", "ngct" = "darkgreen", "missing" = "grey")
  )


# Hromada through time
stocks |>
  filter(territory == "gct") |>
  group_by(t, oblast_name_en) |>
  summarise(n_hromada = n_distinct(hromada_code)) |>
  left_join(
    hromada_geo |>
      group_by(oblast_name_en) |>
      summarise(n_hromada_true = n_distinct(hromada_code))
  ) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(oblast_name_en ~ ., scales = "free_y")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "hromada_through_time.png"
  ),
  width = 8,
  height = 6
)

# Raion through time
stocks |>
  group_by(t, oblast_name_en) |>
  summarise(n_raion = n_distinct(raion_code)) |>
  left_join(
    hromada_geo |>
      group_by(oblast_name_en) |>
      summarise(n_raion_true = n_distinct(raion_code))
  ) |>
  ggplot(aes(x = t, y = n_raion)) +
  geom_line() +
  geom_line(aes(y = n_raion_true), col = "red") +
  facet_wrap(oblast_name_en ~ ., scales = "free_y")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "raion_through_time.png"
  ),
  width = 8,
  height = 6
)


# User evolution

stocks |>
  group_by(t, territory) |>
  summarise(n_users = sum(subscribers_stock)) |>
  ggplot(aes(x = t, y = n_users)) +
  geom_line() +
  facet_wrap(territory ~ ., scales = "free_y") +
  labs(title = "Subscribers evolution")


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
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "subscribers_evolution_male_by_age.png"
  ),
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
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "subscribers_evolution_female_by_age.png"
  ),
  width = 8,
  height = 6
)


# Baseline flows assessment -----------------------------------------------
ngct_territory <- stocks |>
  filter(territory == "ngct") |>
  distinct(hromada_code) |>
  pull(hromada_code)

baselineFlows <- baselineFlows |>
  mutate(
    origin_territory = ifelse(
      origin_hromada %in% ngct_territory,
      "ngct",
      "gct"
    ),
    destination_territory = ifelse(
      destination_hromada %in% ngct_territory,
      "ngct",
      "gct"
    )
  )


baselineFlows_hromada <- baselineFlows |>
  distinct(origin_hromada, destination_hromada)

n_distinct(baselineFlows_hromada$origin_hromada)
n_distinct(baselineFlows_hromada$destination_hromada)

# Data availibility

# Origin
baselineFlows |>
  group_by(t, origin_oblast) |>
  summarise(n_hromada = n_distinct(origin_hromada)) |>
  left_join(
    hromada_geo |>
      st_drop_geometry() |>
      group_by(oblast_name_en) |>
      summarise(n_hromada_true = n_distinct(hromada_code)) |>
      rename(origin_oblast = oblast_name_en)
  ) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(origin_oblast ~ ., scales = "free_y") +
  labs(title = "Baseline flows origin")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "baseline_flows_origin.png"
  ),
  width = 8,
  height = 6
)


# Destination
baselineFlows |>
  group_by(t, destination_oblast) |>
  summarise(n_hromada = n_distinct(destination_hromada)) |>
  left_join(
    hromada_geo |>
      st_drop_geometry() |>
      group_by(oblast_name_en) |>
      summarise(n_hromada_true = n_distinct(hromada_code)) |>
      rename(destination_oblast = oblast_name_en)
  ) |>
  ggplot(aes(x = t, y = n_hromada)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "red") +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  labs(title = "Baseline flows destination")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "baseline_flows_destination.png"
  ),
  width = 8,
  height = 6
)

# Users evolution

baselineFlows_hromada <- baselineFlows |>
  group_by(
    t,
    origin_territory,
    origin_oblast,
    origin_hromada,
    destination_territory,
    destination_oblast,
    destination_hromada
  ) |>
  summarise(subscribers_baselineFlow = sum(subscribers_baselineFlow))

baselineFlows_oblast <- baselineFlows_hromada |>
  group_by(t, origin_oblast, destination_oblast) |>
  summarise(subscribers_baselineFlow = sum(subscribers_baselineFlow))


ggplot(
  baselineFlows_oblast |>
    filter(origin_oblast == "Kyiv"),
  aes(x = t, y = subscribers_baselineFlow)
) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y") +
  labs(title = "Kyiv -> other oblasts")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "baseline_flows_from_kyiv.png"
  ),
  width = 8,
  height = 6
)


ggplot(
  baselineFlows_oblast |>
    filter(destination_oblast == "Kyiv"),
  aes(x = t, y = subscribers_baselineFlow)
) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y") +
  labs(title = "Other oblasts -> Kyiv")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "baseline_flows_to_kyiv.png"
  ),
  width = 8,
  height = 6
)


ggplot(
  baselineFlows_oblast |>
    filter(origin_oblast == "Unknown"),
  aes(x = t, y = subscribers_baselineFlow)
) +
  geom_line() +
  facet_wrap(origin_oblast ~ destination_oblast, scales = "free_y")

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "baseline_flows_from_unknown.png"
  ),
  width = 8,
  height = 6
)

baselineFlows_territory <- baselineFlows_territory |>
  filter(destination_macroregion != "Abroad") |>
  filter(origin_macroregion != "Abroad") |>
  group_by(t, origin_territory, destination_territory) |>
  summarise(
    subscribers_baselineFlow_cumulative = sum(subscribers_monthlyFlow)
  ) |>
  ungroup() |>
  group_by(origin_territory, destination_territory) |>
  arrange(t) |>
  mutate(
    subscribers_baselineFlow = subscribers_baselineFlow_cumulative -
      lag(subscribers_baselineFlow_cumulative)
  )


ggplot(
  baselineFlows_territory |>
    pivot_longer(
      -c(t, origin_territory, destination_territory),
      names_to = "type",
      values_to = "subscribers_baselineFlow"
    ) |>
    mutate(
      type = fct_relevel(
        type,
        c("subscribers_baselineFlow_cumulative", "subscribers_baselineFlow")
      )
    ),
  aes(x = t, y = subscribers_baselineFlow, col = origin_territory)
) +
  geom_line() +
  facet_wrap(type ~ origin_territory, scales = "free_y") +
  scale_color_manual(values = c("gct" = "darksalmon", "ngct" = "darkgreen")) +
  labs(title = "Baseline flows to gct territories")

baselineFlows_originNgct <- baselineFlows |>
  filter(origin_territory == "ngct") |>
  group_by(t, destination_oblast) |>
  summarise(
    subscribers_baselineFlow_cumulative = sum(subscribers_baselineFlow)
  ) |>
  ungroup() |>
  group_by(destination_oblast) |>
  arrange(t) |>
  mutate(
    subscribers_baselineFlow = subscribers_baselineFlow_cumulative -
      lag(subscribers_baselineFlow_cumulative)
  )


ggplot(baselineFlows_originNgct, aes(x = t, y = subscribers_baselineFlow)) +
  geom_line(col = "darkgreen") +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  labs(title = "Baseline flows from ngct territory")

# Check coherence stocks - baseline flows - monthly flows ----------------
coherence_df_flows <- baselineFlows |>
  ungroup() |>
  full_join(
    monthlyFlows
  ) |>
  mutate(
    diff_flows = subscribers_baselineFlow - subscribers_monthlyFlow
  )

coherence_df_hromada <- baselineFlows_hromada |>
  ungroup() |>
  group_by(t, destination_territory, destination_oblast, destination_hromada) |>
  summarise(subscribers_baselineFlow_dest = sum(subscribers_baselineFlow)) |>
  rename(
    oblast_name_en = destination_oblast,
    hromada_code = destination_hromada,
    territory = destination_territory
  ) |>
  full_join(
    baselineFlows |>
      ungroup() |>
      group_by(t, origin_territory, origin_oblast, origin_hromada) |>
      summarise(
        subscribers_baselineFlow_ori = sum(subscribers_baselineFlow),
        .groups = "drop"
      ) |>
      mutate(t = t - months(1)) |>
      rename(
        oblast_name_en = origin_oblast,
        hromada_code = origin_hromada,
        territory = origin_territory
      )
  ) |>
  full_join(
    stocks |>
      ungroup() |>
      group_by(t, oblast_name_en, hromada_code) |>
      summarise(subscribers_stock = sum(subscribers_stock))
  ) |>
  full_join(
    monthlyFlows |>
      ungroup() |>
      group_by(t, destination_oblast, destination_hromada) |>
      rename(
        oblast_name_en = destination_oblast,
        hromada_code = destination_hromada
      ) |>
      summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow))
  ) |>
  mutate(
    diff_flows = subscribers_baselineFlow_dest - subscribers_monthlyFlow
  )

coherence_df <- baselineFlows_oblast |>
  ungroup() |>
  group_by(t, destination_oblast) |>
  summarise(subscribers_baselineFlow_dest = sum(subscribers_baselineFlow)) |>
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
  pivot_longer(
    starts_with("subscriber"),
    names_to = "type",
    values_to = "subscribers"
  ) |>
  group_by(t, type) |>
  summarise(subscribers = sum(subscribers, na.rm = T)) |>
  mutate(subscribers = ifelse(subscribers == 0, NA, subscribers)) |>
  ggplot(aes(x = t, y = subscribers, col = type)) +
  geom_line() +
  labs(title = "Coherence stocks - baseline flows - monthly flows") +
  theme_minimal()

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "coherence_stocks_flows.png"
  ),
  width = 8,
  height = 6
)


# Age and sex missingness ------------------------------------------------
# Hromada through time by age and sex

stocks |>
  group_by(t, oblast_name_en, a_name, s_name) |>
  summarise(n_hromada = n_distinct(hromada_code)) |>
  left_join(
    hromada_geo |>
      group_by(oblast_name_en) |>
      summarise(n_hromada_true = n_distinct(hromada_code))
  ) |>
  ggplot(aes(x = t, y = n_hromada, col = a_name, linetype = s_name)) +
  geom_line() +
  geom_line(aes(y = n_hromada_true), col = "grey20") +
  facet_wrap(oblast_name_en ~ ., scales = "free_y") +
  theme_minimal()

ggsave(
  filename = file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "figs",
    "hromada_through_time_by_agesex.png"
  ),
  width = 8,
  height = 6
)


# At hromada level
stocks |>
  group_by(s_name, hromada_code) |>
  summarise(n = n_distinct(t)) |>
  ungroup() |>
  group_by(s_name) |>
  summarise(complete = sum(n == n_distinct(stocks$t)) / n() * 100)

# At raion level
stocks |>
  group_by(s_name, a_name, macroregion) |>
  summarise(n = n_distinct(t)) |>
  ungroup() |>
  group_by(a_name, s_name) |>
  summarise(complete = sum(n == n_distinct(stocks$t)) / n() * 100)

stocks |>
  filter(territory == "gct") |>
  group_by(s_name, raion_code) |>
  summarise(n = n_distinct(t)) |>
  ungroup() |>
  group_by(s_name) |>
  summarise(complete = sum(n == n_distinct(stocks$t)) / n() * 100)

stocks |>
  filter(territory == "gct") |>
  group_by(a_name, raion_code) |>
  summarise(n = n_distinct(t)) |>
  ungroup() |>
  group_by(a_name) |>
  summarise(complete = sum(n == n_distinct(stocks$t)) / n() * 100)

# Investigate the dip in users in June 2025 ------------------------------------------------

stocks_arounddip <- monthlyFlows |>
  filter(t > as.Date('2025-03-01')) |>
  group_by(t, destination_oblast, a_name, s_name) |>
  summarise(
    n_users = sum(subscribers_monthlyFlow),
    n_hromada = n_distinct(destination_hromada)
  )

ggplot(
  stocks_arounddip,
  aes(x = t, y = n_users, col = a_name, linetype = s_name)
) +
  geom_line() +
  labs(title = "Subscribers evolution around June 2025") +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  theme_minimal()

ggplot(
  stocks_arounddip,
  aes(x = t, y = n_hromada, col = a_name, linetype = s_name)
) +
  geom_line() +
  labs(title = "Hromada availibility evolution around June 2025") +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  theme_minimal()

# compute the relative change in users from May to June 2025 at raion level
stocks_arounddip_raion <- monthlyFlows |>
  filter(t %in% as.Date(c('2025-05-01', '2025-06-01'))) |>
  group_by(t, destination_oblast, destination_raion, a_name, s_name) |>
  summarise(n_users = sum(subscribers_monthlyFlow)) |>
  pivot_wider(names_from = t, values_from = n_users) |>
  mutate(
    rel_change = (`2025-06-01` - `2025-05-01`) / `2025-05-01` * 100
  )

ggplot(stocks_arounddip_raion, aes(x = rel_change, y = a_name)) +
  geom_boxplot() +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  theme_minimal() +
  lims(x = c(-100, 1000))

# plot a couploe of hrmaoda by age and sex from monthlflows

sample_hromada <- sample(unique(monthlyFlows$destination_hromada), 10)
monthlyFlows |>
  filter(destination_hromada %in% sample_hromada) |>
  group_by(t, destination_hromada, a_name, s_name) |>
  summarise(n_users = sum(subscribers_monthlyFlow)) |>
  ggplot(aes(x = t, y = n_users, col = a_name, linetype = s_name)) +
  geom_line() +
  facet_wrap(destination_hromada ~ ., scales = "free_y") +
  theme_minimal()

monthlyFlows_imputed |>
  filter(destination_hromada %in% sample_hromada) |>
  group_by(t, destination_hromada) |>
  summarise(
    n_users = sum(subscribers_monthlyFlow),
    n_users_corrected = sum(subscribers_monthlyFlow_)
  ) |>
  ggplot(aes(x = t, y = n_users)) +
  geom_line() +
  geom_line(aes(y = n_users_corrected), col = "red") +
  facet_wrap(destination_hromada ~ ., scales = "free_y") +
  theme_minimal() +
  labs(title = "Destination hromadas. In red correction applied")

monthlyFlows_imputed |>
  filter(origin_hromada %in% sample_hromada) |>
  group_by(t, origin_hromada) |>
  summarise(
    n_users = sum(subscribers_monthlyFlow),
    n_users_corrected = sum(subscribers_monthlyFlow_)
  ) |>
  ggplot(aes(x = t, y = n_users)) +
  geom_line() +
  geom_line(aes(y = n_users_corrected), col = "red") +
  facet_wrap(origin_hromada ~ ., scales = "free_y") +
  theme_minimal() +
  labs(title = "Origin hromadas. In red correction applied")

movers <- data.table(monthlyFlows)[
  origin_oblast != destination_oblast,
  .(movers = sum(subscribers_monthlyFlow)),
  by = .(t, destination_oblast)
]

ggplot(
  movers,
  aes(x = t, y = movers, col = destination_oblast)
) +
  geom_line() +
  facet_wrap(. ~ destination_oblast, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Movers from the flows data", y = "count")

stocks <- data.table(monthlyFlows_imputed_)[,
  .(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    subscribers_monthlyFlow_ = sum(subscribers_monthlyFlow_)
  ),
  by = .(t, destination_oblast)
]

ggplot(
  stocks,
  aes(x = t, y = subscribers_monthlyFlow_, col = destination_oblast)
) +
  geom_line() +
  geom_line(aes(y = subscribers_monthlyFlow), linetype = 2) +
  facet_wrap(. ~ destination_oblast, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Stocks from the flows data", y = "count")


stocks_origin <- data.table(monthlyFlows)[,
  .(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)),
  by = .(t, origin_oblast)
]

ggplot(
  stocks_origin,
  aes(x = t, y = subscribers_monthlyFlow, col = origin_oblast)
) +
  geom_line() +
  facet_wrap(. ~ origin_oblast, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Stocks from the flows data - origin", y = "count")
