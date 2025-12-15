rm(list = ls())
gc()
library(sf)
library(tmap)
library(dtplyr)

tmap_options(component.autoscale = F)
options(scipen = 999)
print_check <- F

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))

# Load census data
hromada <- read_csv(file.path(in_dir, "KSE-Loc-Data-Hub", "full_dataset.csv"))
hromada_geo <- st_read(file.path(out_dir, "ua_master_hromada.gpkg")) |>
  mutate(
    raion_code = ifelse(oblast_name_en == "Kyiv", "Kyiv", raion_code)
  )

# Load Vodafone data
monthlyFlows <- data.table::fread(file.path(
  out_dir,
  "population_proxy",
  "mobile_phone",
  "vodafone_monthlyFlows_imputed.csv"
)) |>
  mutate(
    t = as.Date(t)
  )
monthlyFlows_ngct <- read_csv(file.path(
  out_dir,
  "population_proxy",
  "mobile_phone",
  "vodafone_monthlyFlows_ngctToNgct.csv"
))
ngct_mapping <- read_csv(file.path(
  out_dir,
  "population_proxy",
  "mobile_phone",
  "vodafone_ngct_mapping.csv"
))

# Load 2022 COD-PS
agesex <- read_csv(file.path(in_dir, "COD-PS", "ukr_admpop_adm1_2022.csv"))

# Load age-sex profile of Ukrainian refugees under the temporary protection scheme from Eurostat
refugee_agesex <- read_csv(file.path(
  out_dir,
  "population_proxy",
  "refugee",
  "eurostat_refugee_agesex.csv"
))

# Load border crossing data provided by border control services through UNHCR
borderCrossing_in <- read.csv(
  file.path(
    out_dir,
    "population_proxy",
    "crossing_borders",
    "refugees_in_daily.csv"
  ),
  stringsAsFactors = F,
  skip = 1,
  skipNul = T,
  sep = ";"
)
borderCrossing_out <- read.csv(
  file.path(
    out_dir,
    "population_proxy",
    "crossing_borders",
    "refugees_out_daily.csv"
  ),
  stringsAsFactors = F,
  skip = 1,
  skipNul = T,
  sep = ";"
)

# Border crossing after Jan
borderCrossing_in_2025 <- read_csv(file.path(
  in_dir,
  "UNHCR",
  "UNHCR_borderCrossing_in_monthly.csv"
))
borderCrossing_out_2025 <- read_csv(file.path(
  in_dir,
  "UNHCR",
  "UNHCR_borderCrossing_out_monthly.csv"
))

# Load sex profile of returnees from DTM surveys (IOM)
returnee_sex <- read_csv(file.path(
  out_dir,
  "population_proxy",
  "returnee",
  "dtm_returnees.csv"
))


#  Step 1: Prepare data ----------------------------------------------------------

# Prepare baseline reference population
hromada_pop <- hromada |>
  select(hromada_code, total_popultaion_2022) |>
  full_join(hromada_geo |> st_drop_geometry()) |>
  mutate(
    total_popultaion_2022 = ifelse(
      hromada_code == "Kyiv",
      2952301,
      total_popultaion_2022
    )
  ) |> # kyiv is not a hromada and was therefore not included in the referebce data
  filter(oblast_name_en != "Autonomous Republic of Crimea")

# Prepare monthly flows
monthlyFlows <- monthlyFlows |>
  left_join(
    hromada_geo |>
      st_drop_geometry() |>
      select(hromada_code, raion_code) |>
      rename(
        origin_hromada = hromada_code,
        origin_raion = raion_code
      )
  ) |>
  left_join(
    hromada_geo |>
      st_drop_geometry() |>
      select(hromada_code, raion_code) |>
      rename(
        destination_raion = raion_code,
        destination_hromada = hromada_code
      )
  ) |>
  mutate(
    origin_raion = ifelse(origin_hromada == "Unknown", "Unknown", origin_raion),
    destination_raion = ifelse(
      destination_hromada == "Unknown",
      "Unknown",
      destination_raion
    ),
    origin_raion = ifelse(origin_hromada == "Abroad", "Abroad", origin_raion),
    destination_raion = ifelse(
      destination_hromada == "Abroad",
      "Abroad",
      destination_raion
    ),
  )


# compute available age-sex combinations
agesex_geoCombination <- monthlyFlows |>
  group_by(t, a_name, s_name) |>
  summarise(
    n_hromada = n_distinct(origin_hromada, destination_hromada),
    n_raion = n_distinct(origin_raion, destination_raion),
    n_oblast = n_distinct(origin_oblast, destination_oblast),
    n_macroregion = n_distinct(origin_macroregion, destination_macroregion),
    .groups = "drop"
  ) |>
  bind_rows(
    monthlyFlows |>
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
      )
  ) |>
  pivot_longer(
    c(n_hromada, n_raion, n_oblast, n_macroregion),
    names_to = "geo_level",
    values_to = "n_combinations"
  )

agesex_geoCombination_full <- agesex_geoCombination |>
  ungroup() |>
  pivot_wider(names_from = geo_level, values_from = n_combinations) |>
  group_by(a_name, s_name) |>
  summarise(n_macroregion = mean(n_macroregion)) |>
  filter(n_macroregion == 56 & a_name != "All" & s_name != "All")

# remove missing age-sex combinations
monthlyFlows <- monthlyFlows |>
  filter(
    a_name %in%
      agesex_geoCombination_full$a_name &
      s_name %in% agesex_geoCombination_full$s_name
  )


# Prepare age and sex reference data

agesex <- agesex |>
  select(admin1Name_en, starts_with("F"), starts_with("M"), -ends_with("TL")) |>
  pivot_longer(
    c(starts_with("F"), starts_with("M")),
    names_to = "agesex_label",
    values_to = "pop"
  ) |>
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

# Prepare reference data by age and sex
hromada_agesex <- hromada_pop |>
  filter(
    hromada_code %in%
      c(
        unique(monthlyFlows$destination_hromada),
        ngct_mapping |> filter(territory == "ngct") |> pull(hromada_code)
      )
  ) |>
  left_join(
    agesex |>
      select(oblast_name_en, s_name, a_name, pi_0),
    relationship = "many-to-many"
  ) |>
  mutate(
    pop = total_popultaion_2022 * pi_0
  ) |>
  filter(
    a_name %in%
      agesex_geoCombination_full$a_name &
      s_name %in% agesex_geoCombination_full$s_name
  ) |>
  left_join(ngct_mapping |> select(hromada_code, territory)) |>
  mutate(
    hromada_code = ifelse(territory == "ngct", "ngct", hromada_code)
  ) |>
  group_by(hromada_code, s_name, a_name) |>
  summarise(
    pop = sum(pop),
    .groups = "drop"
  )

# Prepare border crossing outflows by age and sex

agesex_national_d0 <- hromada_agesex |>
  group_by(s_name, a_name) |>
  summarise(
    pop = sum(pop),
    .groups = "drop"
  )

borderCrossing_out_monthly <- borderCrossing_out |>
  filter(!is.na(individuals)) |>
  as_tibble() |>
  mutate(
    t = as.Date(data_date, format = "%Y-%m-%d"),
    t = ceiling_date(t, unit = "month")
  ) |>
  filter(t < "2025-02-01") |>
  arrange(t) |>
  mutate(
    outflows = individuals - lag(individuals),
    outflows = ifelse(is.na(outflows), individuals, outflows)
  ) |>
  group_by(t) |>
  summarise(
    outflows = sum(outflows),
    .groups = "drop"
  )


borderCrossing_out_monthly <- bind_rows(
  borderCrossing_out_monthly,
  borderCrossing_out_2025 |>
    mutate(
      t = ceiling_date(t, unit = "month")
    ) |>
    rename(outflows = count)
) |>
  filter(t <= max(monthlyFlows$t))

# fill missing dates of border crossing data
if (max(borderCrossing_out_monthly$t) < max(monthlyFlows$t)) {
  borderCrossing_out_monthly_filled <- lapply(
    (max(borderCrossing_out_monthly$t) + months(1)):max(monthlyFlows$t),
    function(t_) {
      borderCrossing_out_monthly |>
        filter(t == max(borderCrossing_out_monthly$t)) |>
        mutate(
          t = as.Date(t_)
        )
    }
  )
  borderCrossing_out_monthly <- bind_rows(
    borderCrossing_out_monthly,
    bind_rows(borderCrossing_out_monthly_filled)
  )
}

# fill missing dates of Eurostat data
refugee_agesex <- bind_rows(
  refugee_agesex |>
    filter(t <= max(monthlyFlows$t)),
  refugee_agesex |>
    filter(t == "2022-04-01") |>
    mutate(
      t = as.Date("2022-03-01")
    )
)

if (max(refugee_agesex$t) < max(monthlyFlows$t)) {
  refugee_agesex_filled <- lapply(
    (max(refugee_agesex$t) + months(1)):max(monthlyFlows$t),
    function(t_) {
      print(as.Date(t_))
      return(
        refugee_agesex |>
          filter(t == max(refugee_agesex$t)) |>
          mutate(
            t = as.Date(t_)
          )
      )
    }
  )

  refugee_agesex <- bind_rows(
    refugee_agesex,
    bind_rows(refugee_agesex_filled)
  )
}

borderCrossing_out_monthly_agesex <- refugee_agesex |>
  mutate(pi_hat = percentage / 100) |>
  select(-refugee, -percentage) |>
  full_join(
    borderCrossing_out_monthly
  ) |>
  mutate(
    outflows = ifelse(is.na(outflows), 0, outflows),
    outflows = outflows * pi_hat,
  ) |>
  filter(
    a_name %in%
      agesex_geoCombination_full$a_name &
      s_name %in% agesex_geoCombination_full$s_name
  )

# recreate totals by substracting missing age combination
borderCrossing_out_monthly <- borderCrossing_out_monthly_agesex |>
  group_by(t) |>
  summarise(outflows = sum(outflows))

# Prepare border crossing inflows by age and sex

borderCrossing_in_monthly <- borderCrossing_in |>
  filter(!is.na(individuals)) |>
  as_tibble() |>
  mutate(
    t = as.Date(data_date, format = "%Y-%m-%d"),
    t = ceiling_date(t, unit = "month")
  ) |>
  filter(t < "2025-02-01") |>
  arrange(t) |>
  mutate(
    inflows = individuals - lag(individuals),
    ,
    inflows = ifelse(is.na(inflows), individuals, inflows)
  ) |>
  group_by(t) |>
  summarise(
    inflows = sum(inflows),
    .groups = "drop"
  )

# Complete inflows post 2025

borderCrossing_in_monthly <- bind_rows(
  borderCrossing_in_monthly,
  borderCrossing_in_2025 |>
    mutate(
      t = ceiling_date(t, unit = "month")
    ) |>
    rename(inflows = count)
) |>
  filter(t <= max(monthlyFlows$t))

# fill missing dates of border crossing data
if (max(borderCrossing_in_monthly$t) < max(monthlyFlows$t)) {
  borderCrossing_in_monthly_filled <- lapply(
    (max(borderCrossing_in_monthly$t) + months(1)):max(monthlyFlows$t),
    function(t_) {
      print(as.Date(t_))
      borderCrossing_in_monthly |>
        filter(t == max(borderCrossing_in_monthly$t)) |>
        mutate(
          t = as.Date(t_)
        )
    }
  )
  borderCrossing_in_monthly <- bind_rows(
    borderCrossing_in_monthly,
    bind_rows(borderCrossing_in_monthly_filled)
  )
}

# Process age-sex profiles of returnees
returnee_agesex <- returnee_sex |>
  group_by(t) |>
  mutate(
    pi_returnee = count / sum(count)
  ) |>
  ungroup() |>
  complete(
    t = seq(
      min(borderCrossing_in_monthly$t, na.rm = T),
      max(monthlyFlows$t),
      by = "1 month"
    ),
    nesting(s_name, a)
  ) %>%
  arrange(s_name, a, t) %>%
  group_by(s_name, a) %>%
  fill(pi_returnee, .direction = "down") |>
  fill(pi_returnee, .direction = "up") |>
  ungroup() |>
  mutate(
    a_returnee = case_when(
      a == "0-4" ~ "0-17",
      a == "5-17" ~ "0-17",
      a == "60Plus" ~ "65Plus",
      T ~ a
    )
  ) |>
  left_join(
    refugee_agesex |>
      select(t, s_name, a_name, refugee) |>
      filter(a_name %in% c("18-24", "25-34", "35-44", "45-54", "55-64")) |>
      group_by(t, s_name) |>
      mutate(
        pi_refugee = refugee / sum(refugee),
        a_returnee = "18-59"
      ) |>
      select(-refugee)
  ) |>
  mutate(
    pi_refugee = ifelse(is.na(pi_refugee), 1, pi_refugee),
    pi = pi_returnee * pi_refugee
  )

borderCrossing_in_monthly_agesex <- borderCrossing_in_monthly |>
  left_join(
    returnee_agesex |>
      select(t, s_name, a_name, pi)
  ) |>
  rename(inflows_total = inflows) |>
  mutate(
    inflows = inflows_total * pi
  ) |>
  filter(
    a_name %in%
      agesex_geoCombination_full$a_name &
      s_name %in% agesex_geoCombination_full$s_name
  )

# recreate inflows totals by substracting missing age combination
borderCrossing_in_monthly <- borderCrossing_in_monthly_agesex |>
  group_by(t) |>
  summarise(inflows = sum(inflows))

# compute population totals by age and sex through time

national_pop_minusOutflows_plusInflows <- bind_rows(
  agesex_national_d0 |>
    full_join(
      borderCrossing_out_monthly_agesex |>
        select(t, a_name, s_name, outflows) |>
        full_join(
          borderCrossing_in_monthly_agesex |>
            select(t, a_name, s_name, inflows)
        )
    ),
  agesex_national_d0 |>
    mutate(
      t = as.Date("2022-02-01")
    )
) |>
  group_by(a_name, s_name) |>
  arrange(t) |>
  mutate(
    inflows = ifelse(is.na(inflows), 0, inflows),
    outflows = ifelse(is.na(outflows), 0, outflows),
    netoutflows = outflows - inflows,
    pop_updated = pop - cumsum(outflows) + cumsum(inflows),
    pop_minusInflows = pop_updated - inflows
  ) |>
  ungroup()

# estimate the number of people in NGCT area
penetration_rate_ngct <- monthlyFlows_ngct |>
  filter(t == min(monthlyFlows_ngct$t)) |>
  filter(
    a_name %in%
      agesex_geoCombination_full$a_name &
      s_name %in% agesex_geoCombination_full$s_name
  ) |>
  left_join(
    hromada_agesex |>
      rename(
        origin_hromada = hromada_code
      )
  ) |>
  mutate(
    p0 = subscribers_monthlyFlow / pop # t0 penetration rate
  ) |>
  select(-t)

ngct_hat <- monthlyFlows_ngct |>
  filter(
    a_name %in%
      agesex_geoCombination_full$a_name &
      s_name %in% agesex_geoCombination_full$s_name
  ) |>
  left_join(
    penetration_rate_ngct |>
      select(origin_hromada, a_name, s_name, p0)
  ) |>
  mutate(
    subscribers_monthlyFlow_hat = subscribers_monthlyFlow / p0
  )

# add NGCT pop estimates to the national population table
national_pop_minusOutflows_plusInflows_minusNGCT <- national_pop_minusOutflows_plusInflows |>
  left_join(
    ngct_hat |>
      select(t, a_name, s_name, ngct = subscribers_monthlyFlow_hat)
  )


# Step 2: The model ---------------------------------------

monthlyFlows_totals <- monthlyFlows |>
  group_by(
    t,
    origin_hromada,
    destination_hromada,
    origin_macroregion,
    destination_macroregion,
    origin_oblast,
    destination_oblast,
    origin_raion,
    destination_raion
  ) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ungroup()

monthlyFlows_totals <- monthlyFlows_totals |>
  group_split(t)

# Compute penetration rate day 1
penetration_rate <- list()
penetration_rate[[1]] <- monthlyFlows_totals[[1]] |>
  filter(origin_macroregion != "Unknown") |>
  filter(!(origin_hromada == "Abroad" & destination_hromada == "Abroad")) |>
  group_by(origin_hromada, origin_raion, origin_oblast, origin_macroregion) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
  ) |>
  left_join(
    bind_rows(
      hromada_agesex |>
        group_by(origin_hromada = hromada_code) |>
        summarise(pop = sum(pop)),
      borderCrossing_in_monthly |>
        filter(t == min(monthlyFlows_totals[[1]]$t)) |>
        select(pop = inflows) |>
        mutate(
          origin_hromada = "Abroad"
        )
    )
  ) |>
  ungroup() |>
  mutate(
    p0 = subscribers_monthlyFlow / pop,
    p0_imputed = case_when(
      # is.na(p0) ~ sum(subscribers_monthlyFlow[!is.na(p0)], na.rm = T) / sum(total_popultaion_2022[!is.na(p0)], na.rm = T),
      TRUE ~ p0
    )
  ) |>
  select(
    origin_hromada,
    origin_raion,
    origin_oblast,
    origin_macroregion,
    origin_p = p0
  ) |>
  mutate(t = min(monthlyFlows$t))


# Compute age and sex penetration rate on day 1
monthlyFlows_agesex <- monthlyFlows |>
  group_split(t)

penetration_rate_agesexMacroregion <- list()
penetration_rate_agesexMacroregion[[1]] <- monthlyFlows_agesex[[1]] |>
  filter(origin_macroregion != "Unknown") |>
  filter(!(origin_hromada == "Abroad" & destination_hromada == "Abroad")) |>
  group_by(origin_hromada, origin_oblast, origin_macroregion, a_name, s_name) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow)
  ) |>
  left_join(
    bind_rows(
      hromada_agesex |>
        rename(
          origin_hromada = hromada_code
        ),
      borderCrossing_in_monthly_agesex |>
        filter(t == min(monthlyFlows_totals[[1]]$t)) |>
        select(s_name, a_name, pop = inflows) |>
        mutate(
          origin_hromada = "Abroad"
        )
    )
  ) |>
  ungroup() |>
  group_by(s_name, a_name, origin_macroregion) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    monthlyFlow_hat_calibrated = sum(pop),
    origin_p = sum(subscribers_monthlyFlow) / sum(pop),
    .groups = "drop"
    # p0_imputed = case_when(
    #   is.na(p0) ~ sum(pop[!is.na(p0)], na.rm = T) / sum(pop[!is.na(p0)], na.rm = T),
    #   TRUE ~ p0
    # )
  ) |>
  mutate(t = min(monthlyFlows$t)) |>
  select(-subscribers_monthlyFlow, -monthlyFlow_hat_calibrated)


# Update penetration rate and total flows

monthlyFlows_totals_hat <- list()
monthlyFlows_agesex_hat <- list()
monthlyFlows_agesex_rescaled_hat <- list()

for (idx in 1:n_distinct(monthlyFlows$t)) {
  print(monthlyFlows_totals[[idx]]$t[1])

  # Compute intermediary population totals
  monthlyFlows_totals_hat[[idx]] <- monthlyFlows_totals[[idx]] |>
    filter(!(origin_hromada == "Abroad" & destination_hromada == "Abroad")) |> # we remove the abroad to abroad as we dont know their scaling factor
    full_join(
      penetration_rate[[idx]],
      by = c(
        "t",
        "origin_hromada",
        "origin_raion",
        "origin_oblast",
        "origin_macroregion"
      )
    ) |>
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
        origin_hromada == "Unknown" & destination_hromada == "Abroad" ~ mean(
          origin_p,
          na.rm = T
        ),
        origin_hromada == "Unknown" ~ destination_p,
        origin_hromada == "Abroad" & is.na(origin_p) ~ mean(
          origin_p,
          na.rm = T
        ), # if no border crossing data is available, revert to the mean penetration rate
        T ~ origin_p
      )
    ) |>
    group_by(origin_oblast) |>
    mutate(
      p = ifelse(is.na(p_), mean(p_, na.rm = T), p_) # for new hromadas. Not currently applied
    ) |>
    ungroup() |>
    mutate(
      monthlyFlow_hat = subscribers_monthlyFlow / p
    ) |>
    select(-p, -origin_p, -destination_p, -p_)

  # Compute intermediary population by age and sex
  monthlyFlows_agesex_hat[[idx]] <- monthlyFlows_agesex[[idx]] |>
    filter(!(origin_hromada == "Abroad" & destination_hromada == "Abroad")) |>
    full_join(
      penetration_rate_agesexMacroregion[[idx]],
      by = c("t", "s_name", "a_name", "origin_macroregion")
    ) |>
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
        origin_macroregion == "Unknown" &
          destination_macroregion == "Abroad" ~ mean(origin_p, na.rm = T),
        origin_macroregion == "Unknown" ~ destination_p,
        origin_macroregion == "Abroad" & is.na(origin_p) ~ mean(
          origin_p,
          na.rm = T
        ),
        T ~ origin_p
      )
    ) |>
    ungroup() |>
    mutate(
      monthlyFlow_hat = subscribers_monthlyFlow / p
    ) |>
    select(-origin_p, -destination_p)

  # Compute domestic estimates as a combination of rescaled domestic population + inflows from abroad

  monthlyFlows_agesex_rescaled_hat[[idx]] <- bind_rows(
    # Treat first internal (with scaling to GCT population derived as pop - NGCT)
    monthlyFlows_agesex_hat[[idx]] |>
      filter(origin_hromada != "Abroad") |>
      filter(destination_hromada != "Abroad") |>
      group_by(
        t,
        s_name,
        a_name,
        origin_macroregion,
        destination_macroregion
      ) |>
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
        monthlyFlows_totals_hat[[idx]] |>
          select(-subscribers_monthlyFlow) |>
          filter(origin_hromada != "Abroad") |>
          filter(destination_hromada != "Abroad"),
        by = c("t", "origin_macroregion", "destination_macroregion"),
        relationship = "many-to-many"
      ) |>
      mutate(
        monthlyFlow_hat_agesex = monthlyFlow_hat * pi_hat
      ) |>
      select(-monthlyFlow_hat) |>
      left_join(
        national_pop_minusOutflows_plusInflows_minusNGCT |>
          filter(t == monthlyFlows_agesex[[idx]]$t[1]) |>
          select(t, a_name, s_name, pop_updated, inflows, outflows, ngct),
        by = c("t", "a_name", "s_name")
      ) |>
      group_by(t, a_name, s_name) |>
      mutate(
        scaling_factor = (pop_updated - inflows - outflows - ngct) /
          sum(monthlyFlow_hat_agesex),
        monthlyFlow_hat_calibrated = monthlyFlow_hat_agesex * scaling_factor
      ) |>
      ungroup() |>
      select(-pop_updated, -inflows, -outflows, -ngct),

    # rescale the outflows the border crossing
    monthlyFlows_agesex_hat[[idx]] |>
      filter(origin_hromada != "Abroad") |>
      filter(destination_hromada == "Abroad") |>
      group_by(
        t,
        s_name,
        a_name,
        origin_macroregion,
        destination_macroregion
      ) |>
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
        monthlyFlows_totals_hat[[idx]] |>
          select(-subscribers_monthlyFlow) |>
          filter(origin_hromada != "Abroad") |>
          filter(destination_hromada == "Abroad"),
        by = c("t", "origin_macroregion", "destination_macroregion"),
        relationship = "many-to-many"
      ) |>
      mutate(
        monthlyFlow_hat_agesex = monthlyFlow_hat * pi_hat
      ) |>
      select(-monthlyFlow_hat) |>
      left_join(
        national_pop_minusOutflows_plusInflows |>
          filter(t == monthlyFlows_agesex[[idx]]$t[1]) |>
          select(t, a_name, s_name, outflows),
        by = c("t", "a_name", "s_name")
      ) |>
      group_by(t, a_name, s_name) |>
      mutate(
        scaling_factor = (outflows) / sum(monthlyFlow_hat_agesex),
        monthlyFlow_hat_calibrated = monthlyFlow_hat_agesex * scaling_factor
      ) |>
      ungroup() |>
      select(-outflows),

    # And add the actual inflows from abroad
    monthlyFlows_agesex_hat[[idx]] |>
      filter(origin_hromada == "Abroad" & destination_hromada != "Abroad") |>
      mutate(
        monthlyFlow_hat_calibrated = monthlyFlow_hat
      ) |>
      select(-monthlyFlow_hat, -subscribers_monthlyFlow, -p)
  )

  # Update penetration rate for next time steps

  penetration_rate_agesexMacroregion[[
    idx + 1
  ]] <- monthlyFlows_agesex_rescaled_hat[[idx]] |>
    filter(destination_macroregion != "Abroad") |>
    left_join(
      monthlyFlows_agesex[[idx]],
      by = c(
        "t",
        "s_name",
        "a_name",
        "origin_macroregion",
        "destination_macroregion",
        "origin_hromada",
        "destination_hromada",
        "origin_oblast",
        "destination_oblast",
        "origin_raion",
        "destination_raion"
      )
    ) |>
    group_by(t, a_name, s_name, origin_macroregion = destination_macroregion) |>
    summarise(
      origin_p = sum(subscribers_monthlyFlow, na.rm = T) /
        sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    mutate(t = t + months(1))

  penetration_rate[[idx + 1]] <- monthlyFlows_agesex_rescaled_hat[[idx]] |>
    filter(destination_macroregion != "Abroad") |>
    group_by(
      t,
      origin_hromada = destination_hromada,
      origin_raion = destination_raion,
      origin_oblast = destination_oblast,
      origin_macroregion = destination_macroregion
    ) |>
    summarise(
      monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    left_join(
      monthlyFlows_totals[[idx]] |>
        group_by(
          t,
          origin_hromada = destination_hromada,
          origin_raion = destination_raion,
          origin_oblast = destination_oblast,
          origin_macroregion = destination_macroregion
        ) |>
        summarise(
          subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
          .groups = "drop"
        ),
      by = c(
        "t",
        "origin_hromada",
        "origin_raion",
        "origin_oblast",
        "origin_macroregion"
      )
    ) |>
    mutate(
      origin_p = subscribers_monthlyFlow / monthlyFlow_hat_calibrated,
    ) |>
    mutate(t = t + months(1)) |>
    select(-subscribers_monthlyFlow, -monthlyFlow_hat_calibrated)

  # Account for border crossing specific penetration rate from abroad

  if (monthlyFlows_totals[[idx]]$t[1] < max(monthlyFlows$t)) {
    penetration_rate_agesexMacroregion_abroad <- monthlyFlows_agesex[[
      idx + 1
    ]] |>
      filter(
        origin_macroregion == "Abroad" & destination_macroregion != "Abroad"
      ) |>
      group_by(t, origin_macroregion, s_name, a_name) |>
      summarise(
        subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
        .groups = "drop"
      ) |>
      left_join(
        borderCrossing_in_monthly_agesex |>
          select(t, a_name, s_name, inflows),
        by = c("t", "a_name", "s_name")
      ) |>
      mutate(
        origin_p = subscribers_monthlyFlow / inflows
      )
    penetration_rate_abroad <- penetration_rate_agesexMacroregion_abroad |>
      group_by(t, origin_macroregion) |>
      summarise(
        origin_p = sum(subscribers_monthlyFlow) / sum(inflows),
        .groups = "drop"
      )

    penetration_rate_agesexMacroregion_abroad <- penetration_rate_agesexMacroregion_abroad |>
      select(-subscribers_monthlyFlow, -inflows)

    penetration_rate_agesexMacroregion[[idx + 1]] <- bind_rows(
      penetration_rate_agesexMacroregion[[idx + 1]],
      penetration_rate_agesexMacroregion_abroad
    )

    penetration_rate[[idx + 1]] <- bind_rows(
      penetration_rate[[idx + 1]],
      penetration_rate_abroad |>
        mutate(
          origin_hromada = "Abroad",
          origin_raion = "Abroad",
          origin_oblast = "Abroad",
        )
    )
  }
}


# Step 3: Post-processing of estimates -----------------------------------

# create df for the estimated quantities

penetration_rate_df <- bind_rows(penetration_rate)
monthlyFlows_totals_hat_df <- bind_rows(monthlyFlows_totals_hat)

penetration_rate_agesexMacroregion_df <- bind_rows(
  penetration_rate_agesexMacroregion
)

monthlyFlows_agesex_rescaled_hat_df <- bind_rows(
  monthlyFlows_agesex_rescaled_hat,
  ngct_hat |>
    rename(
      pi_hat = p0,
      monthlyFlow_hat_calibrated = subscribers_monthlyFlow_hat
    ) |>
    select(-subscribers_monthlyFlow)
)
monthlyFlows_agesex_df <- bind_rows(monthlyFlows_agesex)

# write output

dir.create(
  file.path(out_dir, "model", "deterministic"),
  showWarnings = F,
  recursive = T
)
fwrite(
  monthlyFlows_agesex_rescaled_hat_df,
  file.path(
    out_dir,
    "model",
    "deterministic",
    "mobilePhone_deterministic_agesex.csv"
  )
)

# Compute estimated stocks

monthlyFlows_agesex_hat_df_stocks <- monthlyFlows_agesex_rescaled_hat_df |>
  group_by(
    t,
    a_name,
    s_name,
    destination_macroregion,
    destination_oblast,
    destination_raion,
    destination_hromada
  ) |>
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

# check

if (print_check) {
  # check the population constraint
  monthlyFlows_agesex_hat_df_stocks_oblast |>
    group_by(t, a_name, s_name) |>
    summarise(
      monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    left_join(
      national_pop_minusOutflows_plusInflows |>
        select(-pop)
    ) |>
    mutate(
      diff = monthlyFlow_hat_calibrated - pop_updated
    ) |>
    View()

  monthlyFlows_agesex_hat_df_stocks_oblast |>
    filter(destination_macroregion != "Abroad") |>
    group_by(t, a_name, s_name) |>
    summarise(
      monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    left_join(
      national_pop_minusOutflows_plusInflows |>
        select(-pop)
    ) |>
    mutate(
      diff = monthlyFlow_hat_calibrated - (pop_updated - outflows)
    ) |>
    View()

  monthlyFlows_agesex_hat_df_stocks_oblast |>
    filter(destination_macroregion == "Abroad") |>
    group_by(t, a_name, s_name) |>
    summarise(
      monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    left_join(
      national_pop_minusOutflows_plusInflows |>
        select(-pop)
    ) |>
    mutate(
      diff = monthlyFlow_hat_calibrated - outflows
    ) |>
    View()

  monthlyFlows_agesex_hat_df_stocks_oblast |>
    filter(destination_macroregion != "Abroad") |>
    filter(destination_oblast != "ngct") |>
    group_by(t, a_name, s_name) |>
    summarise(
      monthlyFlow_hat_calibrated = sum(monthlyFlow_hat_calibrated),
      .groups = "drop"
    ) |>
    left_join(
      national_pop_minusOutflows_plusInflows_minusNGCT |>
        select(-pop)
    ) |>
    mutate(
      diff = monthlyFlow_hat_calibrated - (pop_updated - outflows - ngct)
    ) |>
    View()
}


# Step 4: Evaluate and visualise model -------------------------------

# visualise missing hromada

map_3_missing <-
  # visualise missing age-sex combination

  ggplot(
    agesex_geoCombination,
    aes(x = t, y = n_combinations, col = paste(a_name, s_name))
  ) +
  geom_line() +
  theme_minimal() +
  facet_wrap(. ~ geo_level, scales = "free_y") +
  labs(
    col = "Age and sex",
    title = "Number of origin-destination combinations by geographical level",
    x = "Time",
    y = "Number of combinations"
  )

# visualise raw users count

monthlyFlows_agesex_df |>
  filter(destination_hromada != "Abroad") |>
  group_by(t, a_name, s_name) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ggplot(aes(
    x = t,
    y = subscribers_monthlyFlow,
    col = a_name,
    linetype = s_name
  )) +
  geom_line() +
  theme_minimal() +
  facet_wrap(s_name ~ .) +
  labs(title = paste("Raw domestic subscribers"), x = "Time", y = "Subscribers")


monthlyFlows_agesex_df |>
  filter(destination_hromada != "Abroad") |>
  group_by(t, a_name, s_name, destination_oblast) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ggplot(aes(
    x = t,
    y = subscribers_monthlyFlow,
    col = a_name,
    linetype = s_name
  )) +
  geom_line() +
  theme_minimal() +
  facet_wrap(destination_oblast ~ ., scales = "free_y") +
  labs(title = paste("Raw domestic subscribers"), x = "Time", y = "Subscribers")

monthlyFlows_agesex_df |>
  filter(origin_hromada == "Abroad" & destination_hromada != "Abroad") |>
  group_by(t, a_name, s_name) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)) |>
  ggplot(aes(
    x = t,
    y = subscribers_monthlyFlow,
    col = a_name,
    linetype = s_name
  )) +
  geom_line() +
  theme_minimal() +
  labs(title = paste("Raw flows from abroad"), x = "Time", y = "Subscribers")

# visualise outflows
ggplot(borderCrossing_out_monthly, aes(x = t, y = outflows)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = paste("Border crossings outflows (Border Services)"),
    x = "Time",
    y = "People"
  )

ggplot(
  borderCrossing_out_monthly_agesex,
  aes(x = t, y = outflows, col = a_name, linetype = s_name)
) +
  geom_line() +
  theme_minimal() +
  labs(
    title = paste("Border crossings outflows (Border Services + Eurostat)"),
    x = "Time",
    y = "People"
  )

# Visualise inflows
ggplot(returnee_sex, aes(x = t, y = count, col = a, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  labs(title = paste("Inflows"), x = "Time", y = "People")

ggplot(
  borderCrossing_in_monthly_agesex,
  aes(x = t, y = inflows, col = a_name, linetype = s_name)
) +
  geom_line() +
  theme_minimal() +
  labs(title = paste("Inflows"), x = "Time", y = "People")

ggplot(borderCrossing_in_monthly, aes(x = t, y = inflows)) +
  geom_line() +
  theme_minimal() +
  labs(title = paste("Border crossings inflows"), x = "Time", y = "People")

# visualise penetration rate for totals

penetration_rate_df |>
  mutate(
    macroregion_oblast = paste0(origin_macroregion, " - ", origin_oblast)
  ) |>
  ggplot(aes(x = t, y = origin_p, col = origin_hromada)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(macroregion_oblast ~ .) +
  theme(legend.position = "None") +
  geom_hline(yintercept = 1, color = "grey20") +
  labs(
    title = paste("Penetration rate at hromada level - Monthly flows"),
    x = "Time",
    y = "Subscribers/Pop"
  )

# stocks total
national_pop_minusOutflows_plusInflows_totals <- national_pop_minusOutflows_plusInflows |>
  group_by(t) |>
  summarise(pop_updated = sum(pop_updated))


monthlyFlows_totals_hat_national <- monthlyFlows_totals_hat_df |>
  filter(destination_hromada != "Abroad") |>
  group_by(t) |>
  summarise(
    monthlyFlow_hat = sum(monthlyFlow_hat)
  ) |>
  left_join(national_pop_minusOutflows_plusInflows_totals) |>
  mutate(
    monthlyFlow_hat_perc = (monthlyFlow_hat - pop_updated) / pop_updated * 100
  )

gg_flows_national <- ggplot(
  monthlyFlows_totals_hat_national |>
    select(-pop_updated) |>
    pivot_longer(c(monthlyFlow_hat, monthlyFlow_hat_perc), names_to = "type"),
  aes(x = t, y = value)
) +
  geom_line() +
  geom_line(
    data = national_pop_minusOutflows_plusInflows_totals |>
      mutate(type = "monthlyFlow_hat"),
    aes(x = t, y = pop_updated),
    color = "red",
    linetype = "dashed"
  ) +
  labs(title = "Monthly flows induced stocks: National level", ) +
  facet_wrap(~type, scales = "free_y") +
  theme_minimal()
gg_flows_national


monthlyFlows_totals_oblast <- monthlyFlows_totals_hat_df |>
  # filter(destination_hromada != "Abroad") |>
  group_by(t, destination_oblast) |>
  summarise(
    monthlyFlow_hat = sum(monthlyFlow_hat)
  )

ggplot(
  monthlyFlows_totals_oblast |>
    filter(destination_oblast != "Abroad"),
  aes(x = t, y = monthlyFlow_hat, col = destination_oblast)
) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Monthly flows induced stocks: Oblast level",
    x = "Time",
    y = "Subscribers/Pop at t0"
  )

# Visualise penetration rate
penetration_rate_agesexMacroregion_df |>
  ggplot(aes(x = t, y = origin_p, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(origin_macroregion ~ .) +
  geom_hline(yintercept = 1, color = "grey20") +
  labs(
    title = paste(
      "Penetration rate by age and sex at macroregion - Monthly flows"
    ),
    x = "Time",
    y = "Subscribers/Pop"
  )

# visualise flows from abroad penetration rate

penetration_rate_agesexMacroregion_df |>
  filter(origin_macroregion == "Abroad") |>
  ggplot(aes(x = t, y = origin_p, col = a_name, linetype = s_name)) +
  geom_line() +
  theme_minimal() +
  geom_hline(yintercept = 1, color = "grey20") +
  labs(
    title = paste(
      "Penetration rate by age and sex of abroad as calibrated to border crossing"
    ),
    x = "Time",
    y = "Subscribers/Border crossing"
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
  labs(
    x = "",
    y = "Oblast average age-sex proportion",
    title = "Oblast average age-sex estimated proportion per macroregion"
  )

# Visualise age-sex profile of flows to abroad
abroad_profile <- monthlyFlows_agesexMacroregion_hat_df |>
  filter(
    destination_macroregion == "Abroad" & origin_macroregion != "Abroad"
  ) |>
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

ggplot(
  abroad_profile,
  aes(x = t, y = pi_hat, col = a_name, linetype = s_name)
) +
  geom_line() +
  theme_minimal() +
  labs(title = "Flows to Abroad", x = "", y = "Proportion")

# Visualise national population with border crossing  by age and sex
pop_names <- list(
  "inflows" = "Inflows from abroad\n(Borders+IOM DTM)",
  "outflows" = "Outflows to abroad\n(Borders+Eurostat)",
  "pop_updated" = "Updated population"
)
pop_labeller <- function(variable, value) {
  return(pop_names[value])
}


gg_national_agesex <- ggplot(
  national_pop_minusOutflows_plusInflows |>
    select(-pop, -ends_with("cum")) |>
    pivot_longer(c(outflows, inflows, pop_updated)),
  aes(x = t, y = value, col = a_name, linetype = s_name)
) +
  geom_line() +
  theme_minimal() +
  facet_grid(name ~ ., scales = "free_y", labeller = pop_labeller) +
  labs(
    title = "National totals by age and sex through time",
    x = "",
    linetype = "Gender",
    col = "Age group",
    y = ""
  )
gg_national_agesex

ggsave(
  file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    "timeline_national_agesex.png"
  ),
  gg_national,
  w = 8,
  height = 6
)

gg_national <- ggplot(
  national_pop_minusOutflows_plusInflows |>
    select(-pop, -ends_with("cum")) |>
    pivot_longer(c(outflows, inflows, pop_updated)) |>
    group_by(t, name) |>
    summarise(value = sum(value)),
  aes(x = t, y = value)
) +
  geom_line() +
  theme_minimal() +
  facet_grid(name ~ ., scales = "free_y", labeller = pop_labeller) +
  labs(title = "National totals through time", x = "", y = "") +
  scale_y_continuous(labels = scales::label_number())
gg_national

ggsave(
  file.path(out_dir, "model", "deterministic", "figs", "timeline_national.png"),
  gg_national,
  w = 8,
  height = 6
)

ggplot(
  national_pop_minusOutflows_plusInflows |>
    mutate(
      outflows_prop = outflows / pop,
      inflows_prop = inflows / pop
    ) |>
    select(s_name, a_name, t, ends_with("prop")) |>
    pivot_longer(c(ends_with("prop"))),
  aes(x = t, y = value, col = a_name, linetype = s_name)
) +
  geom_line() +
  theme_minimal() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Age sex profiles of flows", x = "")


# Visualise scaling_factor

monthlyFlows_agesex_rescaled_hat_df |>
  distinct(t, a_name, s_name, scaling_factor) |>
  mutate(inverse_scaling_factor = 1 / scaling_factor) |>
  ggplot(aes(
    x = t,
    y = inverse_scaling_factor,
    col = a_name,
    linetype = s_name
  )) +
  geom_line() +
  theme_minimal() +
  labs(
    title = paste("Inverse of the scaling factor by age and sex"),
    x = "Time",
    y = "Inverse of scaling factor=sum(estimated pop)/(pop-border_crossing)"
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
  labs(
    title = "Estimated population per oblast",
    x = "Time",
    y = "Nationally-rescaled population"
  )
