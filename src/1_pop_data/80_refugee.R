library(tidyverse)
source(file.path(here::here(), "R_helpers/generic.R"))

download_eurostat <- F

monthlyFlows <- read_csv(file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_monthlyFlows.csv"))


agesex_reference <- read_csv(file.path(in_dir, "COD-PS", "ukr_admpop_adm1_2022.csv"))

agesex_reference_processed <- agesex_reference |>
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
    a_ref = case_when(
      a == 18 ~ "18-24",
      a == 20 ~ "18-24",
      a == 25 ~ "25-34",
      a == 30 ~ "25-34",
      a == 35 ~ "35-44",
      a == 40 ~ "35-44",
      a == 45 ~ "45-54",
      a == 50 ~ "45-54",
      a == 55 ~ "55-59",
      a == 60 ~ "60-64",
      a >= 65 ~ "65Plus",
      T ~ NA
    ),
    a_unhcr = case_when(
      a_ref == "18-24" ~ "18-34",
      a_ref == "25-34" ~ "18-34",
      a_ref == "35-44" ~ "35-59",
      a_ref == "45-54" ~ "35-59",
      a_ref == "55-59" ~ "35-59",
      a_ref == "60-64" ~ "60Plus",
      a_ref == "65Plus" ~ "60Plus",
      T ~ NA
    ),
    a_eurostat = case_when(
      a_ref == "18-24" ~ "18-34",
      a_ref == "25-34" ~ "18-34",
      a_ref == "35-44" ~ "35-64",
      a_ref == "45-54" ~ "35-64",
      a_ref == "55-59" ~ "35-64",
      a_ref == "60-64" ~ "35-64",
      a_ref == "65Plus" ~ "65Plus",
      T ~ NA
    )
  ) |>
  filter(a >= 18) |>
  group_by(a_unhcr, a_eurostat, a_ref, s_name) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  group_by(a_unhcr, s_name) |>
  mutate(
    a_prop_unhcr = pop / sum(pop),
  ) |>
  ungroup() |>
  group_by(a_eurostat, s_name) |>
  mutate(
    a_prop_eurostat = pop / sum(pop)
  )

# UNHCR
refugee_agesex <- read_csv(file.path(in_dir, "UNHCR", "UNHCR_refugee_agesex.csv"))

refugee_agesex <- refugee_agesex |>
  mutate(
    t = as.Date(t, format = "%d/%m/%Y"),
    duplicate_row = case_when(
      a_unhcr == "18-34" ~ 2,
      a_unhcr == "35-59" ~ 3,
      a_unhcr == "60Plus" ~ 2,
      T ~ 1
    )
  ) |>
  uncount(duplicate_row, .id = "id_dup") |>
  mutate(
    a_ref = case_when(
      a_unhcr == "0-4" ~ "0-17",
      a_unhcr == "5-17" ~ "0-17",
      a_unhcr == "60Plus" & id_dup == 1 ~ "60-64",
      a_unhcr == "60Plus" & id_dup == 2 ~ "65Plus",
      a_unhcr == "18-34" & id_dup == 1 ~ "18-24",
      a_unhcr == "18-34" & id_dup == 2 ~ "25-34",
      a_unhcr == "35-59" & id_dup == 1 ~ "35-44",
      a_unhcr == "35-59" & id_dup == 2 ~ "45-54",
      a_unhcr == "35-59" & id_dup == 3 ~ "55-59",
      T ~ a_unhcr
    )
  ) |>
  left_join(
    agesex_reference_processed |>
      select(-pop)
  ) |>
  mutate(
    a_prop_unhcr = ifelse(is.na(a_prop_unhcr), 1, a_prop_unhcr),
    percentage = percentage * a_prop_unhcr,
    a_name = case_when(
      a_ref == "60-64" ~ "55-64",
      a_ref == "55-59" ~ "55-64",
      T ~ a_ref
    )
  ) |>
  group_by(t, a_name, s_name) |>
  summarise(
    percentage = sum(percentage)
  )

refugee_agesex_smoothed <- bind_rows(
  refugee_agesex,
  tibble(
    t = rep(seq.Date(
      max(refugee_agesex$t),
      max(monthlyFlows$t),
      by = "month"
    ), length(unique(refugee_agesex$s_name)) * length(unique(refugee_agesex$a_name))),
    a_name = rep(unique(refugee_agesex$a_name), length(seq.Date(max(refugee_agesex$t), max(monthlyFlows$t), by = "month")) * length(unique(refugee_agesex$s_name))),
    s_name = rep(unique(refugee_agesex$s_name), length(seq.Date(max(refugee_agesex$t), max(monthlyFlows$t), by = "month")) * length(unique(refugee_agesex$a_name))),
    percentage = NA
  )
)


# Eurostat
eurostat_refugee_file <- file.path(in_dir, "Eurostat", "Eurostat_refugee_agesex.csv")

if (download_eurostat) {
  dir.create(file.path(in_dir, "Eurostat"), showWarnings = FALSE)

  download.file(
    url = "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/migr_asytpfm/1.0?c[freq]=M&c[unit]=PER&c[citizen]=UA&c[sex]=M,F&c[age]=Y_LT14,Y14-17,Y_LT18,Y18-34,Y35-64,Y_GE65&c[geo]=BE,BG,CZ,DK,DE,EE,IE,EL,ES,FR,HR,IT,CY,LV,LT,LU,HU,MT,NL,AT,PL,PT,RO,SI,SK,FI,SE,IS,LI,NO,CH&c[TIME_PERIOD]=ge:2022-03&compress=false&format=csvdata&formatVersion=2.0&lang=en&labels=name",
    destfile = eurostat_refugee_file
  )
}

eurostat_refugee <- read_csv(eurostat_refugee_file)

eurostat_refugee_ <- eurostat_refugee |>
  select(sex, age, geo, TIME_PERIOD, OBS_VALUE) |>
  mutate(
    t = as.Date(paste(TIME_PERIOD, "-01", sep = "")) + months(1),
    a_eurostat = str_remove(age, "Y"),
    a_eurostat = case_when(
      a_eurostat == "14-17" ~ "0-17",
      a_eurostat == "_LT14" ~ "0-17",
      a_eurostat == "_LT18" ~ "0-17",
      a_eurostat == "_GE65" ~ "65Plus",
      T ~ a_eurostat
    )
  ) |>
  rename(
    s_name = sex
  ) |>
  group_by(
    t, a_eurostat, s_name
  ) |>
  summarise(
    refugee = sum(OBS_VALUE),
    .groups = "drop"
  ) |>
  mutate(
    duplicate_row = case_when(
      a_eurostat == "18-34" ~ 2,
      a_eurostat == "35-64" ~ 4,
      T ~ 1
    )
  ) |>
  uncount(duplicate_row, .id = "id_dup") |>
  mutate(
    a_ref = case_when(
      a_eurostat == "18-34" & id_dup == 1 ~ "18-24",
      a_eurostat == "18-34" & id_dup == 2 ~ "25-34",
      a_eurostat == "35-64" & id_dup == 1 ~ "35-44",
      a_eurostat == "35-64" & id_dup == 2 ~ "45-54",
      a_eurostat == "35-64" & id_dup == 3 ~ "55-59",
      a_eurostat == "35-64" & id_dup == 4 ~ "60-64",
      T ~ a_eurostat
    )
  ) |>
  left_join(
    agesex_reference_processed |>
      select(a_eurostat, a_ref, s_name, a_prop_eurostat)
  ) |>
  mutate(
    a_prop_eurostat = ifelse(is.na(a_prop_eurostat), 1, a_prop_eurostat),
    refugee = refugee * a_prop_eurostat,
    a_name = case_when(
      a_ref == "55-59" ~ "55-64",
      a_ref == "60-64" ~ "55-64",
      T ~ a_ref
    )
  ) |>
  group_by(t, a_name, s_name) |>
  summarise(
    refugee = sum(refugee),
    .groups = "drop"
  ) |>
  group_by(t) |>
  mutate(
    percentage = refugee / sum(refugee) * 100
  )

dir.create(file.path(out_dir, "population_proxy", "refugee"), showWarnings = F, recursive = T)
write_csv(eurostat_refugee, file.path(out_dir, "population_proxy", "refugee", "eurostat_refugee_agesex.csv"))

# Raw Vodafone

monthlyFlows_national_agesex <- monthlyFlows |>
  filter(destination_macroregion == "Abroad" & origin_macroregion != "Abroad") |>
  group_by(t, a_name, s_name) |>
  summarise(
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    .groups = "drop"
  ) |>
  group_by(t) |>
  mutate(
    percentage = subscribers_monthlyFlow / sum(subscribers_monthlyFlow) * 100
  )

# Compare age-sex profile
ggplot(agesex_reference |>
  select(admin1Name_en, starts_with("F"), starts_with("M"), -ends_with("TL")) |>
  pivot_longer(c(starts_with("F"), starts_with("M")), names_to = "agesex_label", values_to = "pop") |>
  rowwise() |>
  mutate(
    s_name = ifelse(grepl("F", agesex_label), "F", "M"),
    a = str_split(agesex_label, "_")[[1]][2],
    a = as.integer(ifelse(a == "80Plus", 80, a)),
    age_bin = cut(
      a,
      breaks = seq(0, 85, 5),
      right = FALSE,
      include.lowest = TRUE,
      labels = paste(seq(0, 80, 5), seq(4, 84, 5), sep = "-")
    )
  ), aes(x = age_bin, y = if_else(s_name == "M", -pop, pop), fill = s_name)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs) +
  coord_flip() +
  labs(
    x = "Age",
    y = "Population",
    fill = "Sex",
    title = "Age-Sex Pyramid in the COD-PS"
  ) +
  theme_minimal()

comp_agesex <- bind_rows(
  refugee_agesex |>
    mutate(
      source = "UNHCR"
    ),
  monthlyFlows_national_agesex |>
    mutate(
      source = " Vodafone"
    ),
  eurostat_refugee |>
    mutate(
      source = "Eurostat"
    )
)

ggplot(comp_agesex, aes(x = t, y = percentage, col = a_name)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  facet_grid(source ~ s_name) +
  labs(title = "Comparison of age-sex profile between: \nUNHCR survey, Eurostat settlement scheme and raw Vodafone monthly flows to abroad")
