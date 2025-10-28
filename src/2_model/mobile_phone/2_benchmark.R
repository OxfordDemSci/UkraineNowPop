# libraries
library(formattable)
library(ggforce)
library(NatParksPalettes)
library(jsonlite)
library(dtmapi)

source(file.path(here::here(), "R_helpers/generic.R"))
source(here::here(".env"), local = env)


# Create output directories ----
output_date <- "202510"

repo_dir <- env$repo_dir
src_dir <- file.path(repo_dir, "src", "2_model")
in_dir <- env$in_dir
out_dir <- file.path(
  env$out_dir,
  "model",
  "deterministic"
)
fig_dir <- file.path(out_dir, "comparison", output_date)

data_dir <- file.path(dirname(in_dir), "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# Load data ----
raion_geo <- st_read(file.path(
  in_dir,
  "COD-AB",
  "ukr_admbnda_sspe_20230201_SHP",
  "ukr_admbnda_adm2_sspe_20230201.shp"
))

stocks_hromada_agesex <- data.table::fread(file.path(
  out_dir,
  "deliverables",
  output_date,
  "ua_stocks_hromada_agesex.csv"
)) |>
  rename(ADM3_Minrehion_CODE = hromada)

flows_hromada_agesex <- data.table::fread(file.path(
  out_dir,
  "deliverables",
  output_date,
  "ua_flows_hromada_agesex.csv"
))

geo_linkADM3 <- readxl::read_excel(
  file.path(in_dir, "COD-PS", "2022", "ukr_gazetteer_v01.xlsx"),
  sheet = "ukr_admgz_adm3"
) %>%
  dplyr::select(ADM1_PCODE, ADM2_PCODE, ADM3_PCODE, Minrehion_CODE) %>%
  rename(ADM3_Minrehion_CODE = Minrehion_CODE)

geo_linkADM2 <- readxl::read_excel(
  file.path(in_dir, "COD-PS", "2022", "ukr_gazetteer_v01.xlsx"),
  sheet = "ukr_admgz_adm2"
) %>%
  dplyr::select(ADM1_PCODE, ADM2_PCODE, Minrehion_CODE) %>%
  rename(ADM2_Minrehion_CODE = Minrehion_CODE)

geo <- readxl::read_excel(
  file.path(in_dir, "COD-PS", "2022", "ukr_adminboundaries_tabulardata.xlsx"),
  sheet = "ADM3"
) %>%
  dplyr::select(
    ADM1_EN,
    ADM1_PCODE,
    ADM2_PCODE,
    ADM2_EN,
    ADM3_PCODE,
    ADM3_EN,
    AREA_SQKM
  )

codps23 <- read.csv(file.path(
  data_dir,
  "COD-PS",
  "2023",
  "DO_NOT_SHARE_UKR_ADM2_POP_2023.csv"
))

codps24 <- readxl::read_excel(
  file.path(
    data_dir,
    "COD-PS",
    "2024",
    "[restricted release] UKR_ADM2_POP_2024_Sept_27.xlsx"
  ),
  sheet = 2
)


# Standardise data format ----

date_start <- "2021-12-01"
date_end <- "2025-08-01"


key <- c("ADM3_Minrehion_CODE")

clean_key <- function(x) {
  x %>%
    str_trim() %>%
    str_squish() %>%
    str_replace_all("[[:punct:]]", "") %>%
    stringi::stri_trans_general("Latin-ASCII")
}

stocks_hromada_agesex <- stocks_hromada_agesex %>%
  mutate(ADM3_Minrehion_CODE = hromada) %>%
  mutate(!!key := clean_key(!!sym(key)))


time_index_expanded <- tibble(
  collection_date = seq(as.Date(date_start), as.Date(date_end), by = 1)
) |>
  mutate(
    t_name = floor_date(as.Date(collection_date), "week", week_start = 1),
    t_key = stringr::str_replace_all(as.character(t_name), "-", "") |>
      as.integer(),
    t = t_name |> as.character() |> haven::as_factor() |> as.integer()
  )

time_index <- time_index_expanded |>
  distinct(t_name, t_key, t) |>
  arrange(t)

geo <- geo |>
  rename(ADM3_AREA_SQKM = AREA_SQKM) %>%
  left_join(geo_linkADM3) %>%
  left_join(geo_linkADM2) %>%
  group_by(ADM2_PCODE, ADM2_EN) %>%
  mutate(
    ADM2_AREA_SQKM = sum(ADM3_AREA_SQKM, na.rm = TRUE),
    HRM_SHARE_RAION = ADM3_AREA_SQKM / ADM2_AREA_SQKM
  ) %>%
  ungroup() %>%
  group_by(ADM1_PCODE, ADM1_EN, .add = TRUE) %>%
  mutate(
    ADM1_AREA_SQKM = sum(ADM3_AREA_SQKM, na.rm = TRUE),
    RAION_SHARE_OBLAST = ADM2_AREA_SQKM / ADM1_AREA_SQKM,
    HRM_SHARE_OBLAST = ADM3_AREA_SQKM / ADM1_AREA_SQKM
  ) %>%
  ungroup() %>%
  mutate(
    !!key := clean_key(!!sym(key)),
    ADM3_Minrehion_CODE = ifelse(
      ADM1_EN == "Kyiv",
      "Kyiv",
      ADM3_Minrehion_CODE
    ),
    ADM2_Minrehion_CODE = ifelse(ADM1_EN == "Kyiv", "Kyiv", ADM2_Minrehion_CODE)
  )


codps23 <- codps23 %>%
  rename(ADM1_EN = ADM1_NAME, ADM2_EN = ADM2_NAME) %>%
  transmute(
    ADM1_PCODE = ADM1_PCODE,
    ADM1_EN = ADM1_EN,
    ADM2_PCODE = ADM2_PCODE,
    ADM2_EN = ADM2_EN,
    # F_18_24 = 0.4*F_15_19 + F_20_24,
    # M_18_24 = M_15_19 + M_20_24,
    F_25_34 = F_25_29 + F_30_34,
    M_25_34 = M_25_29 + M_30_34,
    F_35_44 = F_35_39 + F_40_44,
    M_35_44 = M_35_39 + M_40_44,
    F_45_54 = F_45_49 + F_50_54,
    M_45_54 = M_45_49 + M_50_54,
    F_55_64 = F_55_59 + F_60_64,
    M_55_64 = M_55_59 + M_60_64
  ) %>%
  pivot_longer(
    cols = 5:12,
    names_to = "demog",
    values_to = "pop1"
  ) %>%
  separate(demog, into = c("s_name", "age_min", "age_max"), sep = "_") %>%
  mutate(
    a_name = paste0(age_min, "-", age_max),
    ADM2_PCODE = ifelse(
      ADM1_PCODE == "UA01" | ADM1_PCODE == "UA85",
      ADM1_PCODE,
      ADM2_PCODE
    ),
    ADM2_EN = ifelse(
      ADM1_PCODE == "UA01" | ADM1_PCODE == "UA85",
      ADM1_EN,
      ADM2_EN
    )
  ) %>%
  dplyr::select(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, s_name, a_name, pop1)

codps24 <- codps24 %>%
  rename(ADM1_EN = ADM1_NAME, ADM2_EN = ADM2_NAME) %>%
  transmute(
    ADM1_PCODE = ADM1_PCODE,
    ADM1_EN = ADM1_EN,
    ADM2_PCODE = ADM2_PCODE,
    ADM2_EN = ADM2_EN,
    # F_18_24 = 0.4*F_15_19 + F_20_24,
    # M_18_24 = M_15_19 + M_20_24,
    F_25_34 = F_25_29 + F_30_34,
    M_25_34 = M_25_29 + M_30_34,
    F_35_44 = F_35_39 + F_40_44,
    M_35_44 = M_35_39 + M_40_44,
    F_45_54 = F_45_49 + F_50_54,
    M_45_54 = M_45_49 + M_50_54,
    F_55_64 = F_55_59 + F_60_64,
    M_55_64 = M_55_59 + M_60_64
  ) %>%
  pivot_longer(
    cols = 5:12,
    names_to = "demog",
    values_to = "pop2"
  ) %>%
  separate(demog, into = c("s_name", "age_min", "age_max"), sep = "_") %>%
  mutate(
    a_name = paste0(age_min, "-", age_max),
    ADM2_PCODE = ifelse(is.na(ADM2_PCODE), ADM1_PCODE, ADM2_PCODE),
    ADM2_EN = ifelse(is.na(ADM2_EN), ADM1_EN, ADM2_EN)
  ) %>%
  dplyr::select(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, s_name, a_name, pop2)


# Define geographical scope ----

oblast_hrom_total <- geo %>%
  distinct(ADM1_PCODE, ADM1_EN, ADM3_Minrehion_CODE) %>%
  group_by(ADM1_PCODE, ADM1_EN) %>%
  summarise(total_hromadas = n(), .groups = "drop")

comparison <- stocks_hromada_agesex %>%
  select(-raion, -oblast) %>%
  left_join(geo)

oblast_hrom_covered <- comparison %>%
  distinct(ADM1_PCODE, ADM3_Minrehion_CODE) %>%
  group_by(ADM1_PCODE) %>%
  summarise(covered_hromadas = n(), .groups = "drop")

coverage_check <- oblast_hrom_total %>%
  left_join(oblast_hrom_covered, by = "ADM1_PCODE") %>%
  mutate(
    covered_hromadas = replace_na(covered_hromadas, 0),
    full_coverage = covered_hromadas == total_hromadas,
    coverage_prop = covered_hromadas / total_hromadas
  )

oblasts_complete <- coverage_check %>%
  filter(full_coverage) %>%
  select(ADM1_PCODE, ADM1_EN, total_hromadas)


raion_hrom_total <- geo %>%
  distinct(ADM2_PCODE, ADM2_EN, ADM3_Minrehion_CODE) %>%
  group_by(ADM2_PCODE, ADM2_EN) %>%
  summarise(total_hromadas = n(), .groups = "drop")

raion_hrom_covered <- comparison %>%
  distinct(ADM2_PCODE, ADM3_Minrehion_CODE) %>%
  group_by(ADM2_PCODE) %>%
  summarise(covered_hromadas = n(), .groups = "drop")

raion_coverage <- raion_hrom_total %>%
  left_join(raion_hrom_covered, by = "ADM2_PCODE") %>%
  mutate(
    covered_hromadas = replace_na(covered_hromadas, 0),
    full_coverage = covered_hromadas == total_hromadas,
    coverage_prop = covered_hromadas / total_hromadas
  )

raions_complete <- raion_coverage %>%
  filter(full_coverage) %>%
  arrange(ADM2_EN) %>%
  select(ADM2_PCODE, ADM2_EN, total_hromadas)

raion_geo <- raion_geo |>
  mutate(
    complete = ifelse(
      ADM2_PCODE %in% raions_complete$ADM2_PCODE,
      "Complete",
      "Incomplete"
    )
  )

raion_geo |>
  ggplot(aes(fill = complete)) +
  geom_sf() +
  theme_void() +
  scale_fill_manual(values = c("Complete" = "grey", "Incomplete" = "white")) +
  labs(fill = "Raion coverage")

ggsave(
  filename = file.path(out_dir, "map_raionCoverage.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)


# Compare Vodafone and CODPS in 2023 ----

comparison23 <- stocks_hromada_agesex %>%
  select(-raion, -oblast) %>%
  left_join(geo) %>%
  filter(
    t == as.Date("2023-07-01"),
    ADM2_PCODE %in% raions_complete$ADM2_PCODE,
    a_name != "18_24"
  ) %>%
  group_by(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, a_name, s_name) %>%
  summarise(pop_estimated = sum(pop_estimated), .groups = "drop") %>%
  left_join(codps23) %>%
  filter(a_name != "18_24" & a_name != "18-24") |>
  mutate(
    pop_diff = pop1 - pop_estimated
  )


## Total comparison at National level ----
plot_nal <- comparison23 %>%
  group_by(a_name, s_name) %>%
  summarise(
    pop_estimated_nal = sum(pop_estimated, na.rm = T),
    pop1 = sum(pop1, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    pop_estimated_nal = ifelse(
      s_name == "M",
      pop_estimated_nal * (-1),
      pop_estimated_nal
    ),
    pop_bench_nal = ifelse(s_name == "M", pop1 * (-1), pop1)
  )

ggplot(plot_nal, aes(x = a_name)) +
  geom_col(
    aes(y = pop_estimated_nal, fill = s_name),
    width = 0.9,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = pop_bench_nal, colour = s_name, group = s_name),
    linewidth = 1.2
  ) +
  geom_point(aes(y = pop_bench_nal, colour = s_name), size = 2) +
  coord_flip() +
  scale_fill_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (Vodafone est.)", M = "Male (Vodafone est.)")
  ) +
  scale_colour_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (COD-PS 2023)", M = "Male (COD-PS 2023)")
  ) +
  scale_y_continuous(labels = function(x) formattable::comma(abs(x))) +
  labs(
    title = "Vodafone estimates vs. COD-PS 2023 population pyramid",
    caption = "Date: 1 July 2023 \nGeography: complete raions",
    x = NULL,
    y = "Population"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_pop_pyr_nal.jpeg"),
  plot = last_plot(),
  width = 25,
  height = 20,
  units = "cm",
  dpi = 300
)

## Total comparison at Oblast level ----

plot_obl <- comparison23 %>%
  group_by(ADM1_EN, a_name, s_name) %>%
  summarise(
    pop_estimated_obl = sum(pop_estimated),
    pop_bench_obl = sum(pop1)
  ) %>%
  ungroup() %>%
  mutate(
    pop_estimated_obl = ifelse(
      s_name == "M",
      -pop_estimated_obl,
      pop_estimated_obl
    ),
    pop_bench_obl = ifelse(s_name == "M", -pop_bench_obl, pop_bench_obl)
  )

ggplot(plot_obl, aes(x = a_name)) +
  geom_col(
    aes(y = pop_estimated_obl / 1000, fill = s_name),
    width = 0.9,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = pop_bench_obl / 1000, colour = s_name, group = s_name),
    linewidth = 1.2
  ) +
  geom_point(aes(y = pop_bench_obl / 1000, colour = s_name), size = 2) +
  coord_flip() +
  facet_wrap(~ADM1_EN, scales = "free_x") +
  scale_y_continuous(labels = function(x) round(abs(x), 1)) +
  scale_fill_manual(
    values = c(F = "#CF932C", M = "#8C86A0"),
    name = NULL,
    labels = c(F = "Female (Vodafone est.)", M = "Male (Vodafone est.)")
  ) +
  scale_colour_manual(
    values = c(F = "#CF932C", M = "#8C86A0"),
    name = NULL,
    labels = c(F = "Female (COD-PS 2023)", M = "Male (COD-PS 2023)")
  ) +
  labs(
    title = "Vodafone estimates vs. COD-PS 2023 population pyramid",
    x = NULL,
    caption = "Date: 1 July 2023\nGeographical scope: complete raions",
    y = "Population (in thousands)"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_pop_pyr_obl.jpeg"),
  plot = last_plot(),
  units = "cm",
  width = 35,
  height = 30
)

## Total comparison at raion level ----

plot_raion <- comparison23 %>%
  left_join(plot_obl) %>%
  group_by(ADM2_EN, a_name, s_name, pop_estimated, pop_bench_obl) %>%
  summarise(
    pop_estimated = sum(pop_estimated, na.rm = TRUE),
    pop1 = sum(pop1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    pop_estimated_raion = ifelse(s_name == "M", -pop_estimated, pop_estimated),
    pop_bench_raion = ifelse(s_name == "M", -pop1, pop1)
  )

map_raion <- raion_geo |>
  left_join(
    comparison23 |>
      group_by(ADM2_EN) %>%
      summarise(
        pop_estimated = sum(pop_estimated, na.rm = TRUE),
        pop1 = sum(pop1, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        pop_diff = pop1 - pop_estimated
      )
  )

tm_map_raion <- tm_shape(map_raion) +
  tm_polygons(
    fill = "pop_diff",
    fill.scale = tm_scale_intervals(
      values = c("#722d08ff", "white", "#75871B"),
      midpoint = 0,
      value.na = "grey"
    ),
    col = 'white',
    lwd = 0.1,
    fill.legend = tm_legend(
      title = "COD-PS 2023 - Vodafone",
      position = tm_pos_in("right", "bottom"),
      frame = F
    )
  ) +
  tm_layout(
    frame = FALSE,
    asp = 1.5
  ) +
  tm_title(
    text = "Population difference between Vodafone estimates and COD-PS 2023",
  ) +
  tm_credits(
    text = "Date: 1 July 2023 \nGeographical scope: complete raions",
    position = c("left", "bottom"),
    size = 0.7
  )
tm_map_raion
tmap_save(
  tm = tm_map_raion,
  filename = file.path(fig_dir, "vodafone_vs_codps_2023_pop_map_raion.jpeg"),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)

## Proportion comparison at raion level ----

plot_obl_prop <- comparison23 %>%
  group_by(ADM1_EN, a_name, s_name) %>%
  summarise(
    pop_estimated = sum(pop_estimated, na.rm = TRUE),
    pop1 = sum(pop1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ADM1_EN) %>%
  mutate(
    total_est_obl = sum(pop_estimated),
    total_bench_obl = sum(pop1),
    prop_bench_obl = pop1 / total_bench_obl,
    prop_bench_obl = ifelse(s_name == "M", -prop_bench_obl, prop_bench_obl)
  ) %>%
  ungroup() %>%
  select(ADM1_EN, a_name, s_name, prop_bench_obl)


plot_raion_prop <- comparison23 %>%
  left_join(plot_obl_prop) %>%
  group_by(ADM2_EN) %>%
  mutate(
    total_est = sum(pop_estimated, na.rm = TRUE),
    total_bench = sum(pop1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    prop_est = pop_estimated / total_est,
    prop_bench = pop1 / total_bench,
    prop_est = ifelse(s_name == "M", -prop_est, prop_est),
    prop_bench = ifelse(s_name == "M", -prop_bench, prop_bench)
  )

base_pyr <- ggplot(plot_raion_prop, aes(x = a_name)) +
  geom_col(aes(y = prop_est, fill = s_name), width = 0.9, alpha = 0.65) +
  geom_line(
    aes(y = prop_bench, colour = s_name, group = s_name),
    linewidth = 1.1
  ) +
  geom_line(
    aes(y = prop_bench_obl, colour = s_name, group = s_name),
    linewidth = 1.1,
    linetype = 2
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (Vodafone est.)", M = "Male (Vodafone est.)")
  ) +
  scale_colour_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (COD‑PS 2023)", M = "Male (COD‑PS 2023)")
  ) +
  scale_y_continuous(labels = function(x) round(abs(x), 2)) +
  labs(
    title = "Vodafone estimates vs. COD‑PS 2023 population pyramid by Raion (1 July 2023)",
    subtitle = "Solid lines: COD-PS 2023 - raion level; Dashed lines: COD-PS 2023 - oblast level ",
    x = NULL,
    y = "Population"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

n_col <- 3
n_row <- 3
base_pyr +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 1
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_pop_pyr_raion1.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)

base_pyr +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 2
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_pop_pyr_raion2.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)
base_pyr +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 2
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_pop_pyr_raion2.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)

base_pyr +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 3
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_pop_pyr_raion3.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)


base_plot <- ggplot(
  comparison23,
  aes(x = log(pop1), y = log(pop_estimated), colour = a_name, shape = s_name)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 3.5, stroke = 1.8, alpha = 0.65) +
  labs(
    title = "Vodafone estimates vs. COD‑PS 2023 - raion level",
    subtitle = "Reference date: 1 Jul 2023",
    x = "log(COD‑PS 2023)",
    y = "log(Vodafone estimate)",
    colour = "Age group",
    shape = "Sex"
  ) +
  scale_shape_manual(values = c(F = 3, M = 2)) +
  scale_color_viridis_d() +
  theme_minimal(base_size = 20)

n_col <- 3
n_row <- 2
base_plot +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 1
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_scatterplot_raion1.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)
base_plot +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 2
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_scatterplot_raion2.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)
base_plot +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 3
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2023_scatterplot_raion3.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)


# Compare Vodafone estimates vs. COD-PS 2024 ----

comparison24 <- stocks_hromada_agesex %>%
  select(-raion, -oblast) %>%
  left_join(geo) %>%
  filter(
    t == as.Date("2023-09-01"),
    ADM2_PCODE %in% raions_complete$ADM2_PCODE,
    a_name != "18_24"
  ) %>%
  group_by(ADM1_PCODE, ADM1_EN, ADM2_PCODE, ADM2_EN, a_name, s_name) %>%
  summarise(pop_estimated = sum(pop_estimated), .groups = "drop") %>%
  left_join(codps24) %>%
  filter(a_name != "18_24" & a_name != "18-24")

## Total comparison at National level ----

plot_nal <- comparison24 %>%
  group_by(a_name, s_name) %>%
  summarise(
    pop_estimated_nal = sum(pop_estimated, na.rm = T),
    pop2 = sum(pop2, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    pop_estimated_nal = ifelse(
      s_name == "M",
      pop_estimated_nal * (-1),
      pop_estimated_nal
    ),
    pop_bench_nal = ifelse(s_name == "M", pop2 * (-1), pop2)
  )

ggplot(plot_nal, aes(x = a_name)) +
  geom_col(
    aes(y = pop_estimated_nal, fill = s_name),
    width = 0.9,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = pop_bench_nal, colour = s_name, group = s_name),
    linewidth = 1.2
  ) +
  geom_point(aes(y = pop_bench_nal, colour = s_name), size = 2) +
  coord_flip() +
  scale_fill_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (Vodafone est.)", M = "Male (Vodafone est.)")
  ) +
  scale_colour_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (COD‑PS 2024)", M = "Male (COD‑PS 2024)")
  ) +
  scale_y_continuous(labels = function(x) formattable::comma(abs(x))) +
  labs(
    title = "Vodafone estimates vs. COD-PS 2024 population pyramid",
    caption = "Date: 1 July 2024 \nGeography: complete raions",
    x = NULL,
    y = "Population"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_pop_pyr_nal.jpeg"),
  width = 25,
  height = 20,
  units = "cm",
  dpi = 300
)

## Total comparison at Oblast level ----

plot_obl <- comparison24 %>%
  group_by(ADM1_EN, a_name, s_name) %>%
  summarise(
    pop_estimated_obl = sum(pop_estimated),
    pop_bench_obl = sum(pop2)
  ) %>%
  ungroup() %>%
  mutate(
    pop_estimated_obl = ifelse(
      s_name == "M",
      -pop_estimated_obl,
      pop_estimated_obl
    ),
    pop_bench_obl = ifelse(s_name == "M", -pop_bench_obl, pop_bench_obl)
  )

ggplot(plot_obl, aes(x = a_name)) +
  geom_col(
    aes(y = pop_estimated_obl / 1000, fill = s_name),
    width = 0.9,
    alpha = 0.6
  ) +
  geom_line(
    aes(y = pop_bench_obl / 1000, colour = s_name, group = s_name),
    linewidth = 1.2
  ) +
  geom_point(aes(y = pop_bench_obl / 1000, colour = s_name), size = 2) +
  coord_flip() +
  facet_wrap(~ADM1_EN, scales = "free_x") +
  scale_y_continuous(labels = function(x) round(abs(x), 1)) +
  scale_fill_manual(
    values = c(F = "#CF932C", M = "#8C86A0"),
    name = NULL,
    labels = c(F = "Female (Vodafone est.)", M = "Male (Vodafone est.)")
  ) +
  scale_colour_manual(
    values = c(F = "#CF932C", M = "#8C86A0"),
    name = NULL,
    labels = c(F = "Female (COD-PS 2024)", M = "Male (COD-PS 2024)")
  ) +
  labs(
    title = "Vodafone estimates vs. COD-PS 2024 population pyramid",
    x = NULL,
    caption = "Date: 1 July 2024\nGeographical scope: complete raions",
    y = "Population (in thousands)"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "bottom")

ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_pop_pyr_obl.jpeg"),
  units = "cm",
  width = 35,
  height = 30
)

## Total comparison at raion level ----

plot_raion <- comparison24 %>%
  left_join(plot_obl) %>%
  group_by(ADM2_EN, a_name, s_name, pop_estimated, pop_bench_obl) %>%
  summarise(
    pop_estimated = sum(pop_estimated, na.rm = TRUE),
    pop2 = sum(pop2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(
    pop_estimated_raion = ifelse(s_name == "M", -pop_estimated, pop_estimated),
    pop_bench_raion = ifelse(s_name == "M", -pop2, pop2)
  )
map_raion <- raion_geo |>
  left_join(
    comparison24 |>
      group_by(ADM2_EN) %>%
      summarise(
        pop_estimated = sum(pop_estimated, na.rm = TRUE),
        pop2 = sum(pop2, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(
        pop_diff = pop2 - pop_estimated
      )
  )
tm_map_raion_2024 <- tm_shape(map_raion) +
  tm_polygons(
    fill = "pop_diff",
    fill.scale = tm_scale_intervals(
      values = c("#722d08ff", "white", "#75871B"),
      midpoint = 0,
      value.na = "grey"
    ),
    col = 'white',
    lwd = 0.1,
    fill.legend = tm_legend(
      title = "COD-PS 2024 - Vodafone",
      position = tm_pos_in("right", "bottom"),
      frame = F
    )
  ) +
  tm_layout(
    frame = FALSE,
    asp = 1.5
  ) +
  tm_title(
    text = "Population difference between Vodafone estimates and COD-PS 2024",
  ) +
  tm_credits(
    text = "Date: 1 July 2024 \nGeographical scope: complete raions",
    position = c("left", "bottom"),
    size = 0.7
  )
tm_map_raion_2024
tmap_save(
  tm = tm_map_raion_2024,
  filename = file.path(fig_dir, "vodafone_vs_codps_2024_pop_map_raion.jpeg"),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)

## Proportion comparison at raion level ----

plot_obl_prop <- comparison24 %>%
  group_by(ADM1_EN, a_name, s_name) %>%
  summarise(
    pop_estimated = sum(pop_estimated, na.rm = TRUE),
    pop2 = sum(pop2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ADM1_EN) %>%
  mutate(
    total_est_obl = sum(pop_estimated),
    total_bench_obl = sum(pop2),
    prop_bench_obl = pop2 / total_bench_obl,
    prop_bench_obl = ifelse(s_name == "M", -prop_bench_obl, prop_bench_obl)
  ) %>%
  ungroup() %>%
  select(ADM1_EN, a_name, s_name, prop_bench_obl)


plot_raion_prop <- comparison24 %>%
  left_join(plot_obl_prop) %>%
  group_by(ADM2_EN) %>%
  mutate(
    total_est = sum(pop_estimated, na.rm = TRUE),
    total_bench = sum(pop2, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    prop_est = pop_estimated / total_est,
    prop_bench = pop2 / total_bench,
    prop_est = ifelse(s_name == "M", -prop_est, prop_est),
    prop_bench = ifelse(s_name == "M", -prop_bench, prop_bench)
  )

base_pyr <- ggplot(plot_raion_prop, aes(x = a_name)) +
  geom_col(aes(y = prop_est, fill = s_name), width = 0.9, alpha = 0.65) +
  geom_line(
    aes(y = prop_bench, colour = s_name, group = s_name),
    linewidth = 1.1
  ) +
  geom_line(
    aes(y = prop_bench_obl, colour = s_name, group = s_name),
    linewidth = 1.1,
    linetype = 2
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (Vodafone est.)", M = "Male (Vodafone est.)")
  ) +
  scale_colour_manual(
    values = c(M = "#8C86A0", F = "#CF932C"),
    name = NULL,
    labels = c(F = "Female (COD‑PS 2024)", M = "Male (COD‑PS 2024)")
  ) +
  scale_y_continuous(labels = function(x) round(abs(x), 2)) +
  labs(
    title = "Vodafone estimates vs. COD‑PS 2024 population pyramid by Raion (1 September 2024)",
    subtitle = "Solid lines: COD-PS 2024 - raion level; Dashed lines: COD-PS 2024 - oblast level ",
    x = NULL,
    y = "Population"
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom")

n_col <- 3
n_row <- 3
base_pyr +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 1
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_pop_pyr_raion1.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)

base_pyr +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 2
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_pop_pyr_raion2.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)

base_pyr +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 3
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_pop_pyr_raion3.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)


base_plot <- ggplot(
  comparison24,
  aes(x = log(pop2), y = log(pop_estimated), colour = a_name, shape = s_name)
) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 3.5, stroke = 1.8, alpha = 0.65) +
  labs(
    title = "Vodafone estimates vs. COD‑PS 2024 - raion level",
    subtitle = "Reference date: 1 Sep 2024",
    x = "log(COD‑PS 2024)",
    y = "log(Vodafone estimate)",
    colour = "Age group",
    shape = "Sex"
  ) +
  scale_shape_manual(values = c(F = 3, M = 2)) +
  scale_color_viridis_d() +
  theme_minimal(base_size = 20)

n_col <- 3
n_row <- 2
base_plot +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 1
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_scatterplot_raion1.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)
base_plot +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 2
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_scatterplot_raion2.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)
base_plot +
  ggforce::facet_wrap_paginate(
    ~ ADM1_EN * ADM2_EN,
    ncol = n_col,
    nrow = n_row,
    page = 3
  )
ggsave(
  file.path(fig_dir, "vodafone_vs_codps_2024_scatterplot_raion3.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)

# Compare Vodafone estimates vs Facebook estimates ----

parent_dir <- file.path(
  in_dir,
  "deliverables",
  "20240305 Deterministic Estimates/extract",
  "Ukraine_population_estimates_2203",
  "Ukraine_population_estimates_2023",
  "oblast_daily_population/model"
)

all_subdirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)
date_pattern <- "^\\d{4}-\\d{2}-\\d{2}$"

target_dates <- seq(as.Date("2021-12-01"), as.Date("2025-08-01"), by = "day")

date_subdirs <- all_subdirs[str_detect(basename(all_subdirs), date_pattern)]
filtered_subdirs <- date_subdirs[basename(date_subdirs) %in% target_dates]

target_filename <- "population_current.csv"

population_data_list <- vector("list", length(filtered_subdirs))
names(population_data_list) <- basename(filtered_subdirs)

for (s in seq_along(filtered_subdirs)) {
  fp <- file.path(filtered_subdirs[s], target_filename)
  if (file.exists(fp)) {
    population_data_list[[s]] <- read_csv(fp, show_col_types = FALSE) %>%
      mutate(date = as.Date(basename(filtered_subdirs[s])))
  }
}

combined_population_data_2023 <- bind_rows(population_data_list) %>%
  rename(t = date) %>%
  filter(day(t) == 1) %>%
  transmute(
    t, # t0,
    ADM1_PCODE,
    F_25_34 = f_25 + f_30,
    M_25_34 = m_25 + m_30,
    F_35_44 = f_35 + f_40,
    M_35_44 = m_35 + m_40,
    F_45_54 = f_45 + f_50,
    M_45_54 = m_45 + m_50,
    F_55_64 = f_55 + f_60,
    M_55_64 = m_55 + m_60
  ) %>%
  filter(ADM1_PCODE %in% oblasts_complete$ADM1_PCODE) %>%
  pivot_longer(
    -c(
      t, # t0,
      ADM1_PCODE
    ),
    names_to = "demog",
    values_to = "DL_estimate"
  ) %>%
  separate(demog, c("s_name", "age_min", "age_max"), sep = "_") %>%
  mutate(
    a_name = paste0(age_min, "-", age_max),
    a_name = str_replace_all(a_name, "[-_]", "-"),
    ADM1_PCODE = str_remove_all(ADM1_PCODE, "\\s+"),
    ADM1_PCODE = str_squish(str_to_upper(ADM1_PCODE))
  ) %>%
  select(
    t, # t0,
    ADM1_PCODE,
    s_name,
    a_name,
    DL_estimate
  )


estimates_adm1 <- stocks_hromada_agesex %>%
  select(-raion, -oblast) %>%
  left_join(geo, by = key) %>%
  filter(
    ADM1_PCODE %in% oblasts_complete$ADM1_PCODE,
    a_name != "18-24"
  ) %>%
  group_by(t, ADM1_PCODE, ADM1_EN, a_name, s_name) %>%
  summarise(pop_estimated = sum(pop_estimated), .groups = "drop") %>%
  mutate(
    t = as.Date(t),
    a_name = str_replace_all(a_name, "[-_]", "-"),
    ADM1_PCODE = str_remove_all(ADM1_PCODE, "\\s+"),
    ADM1_PCODE = str_squish(str_to_upper(ADM1_PCODE))
  )

comparison <- estimates_adm1 %>% right_join(combined_population_data_2023)

comparison_long <- comparison %>%
  pivot_longer(
    cols = c(DL_estimate, pop_estimated),
    names_to = "source",
    values_to = "value"
  )

ggplot(comparison_long, aes(x = t, y = value, color = source)) +
  geom_line(size = 0.8) +
  facet_wrap(ADM1_EN ~ a_name * s_name, scales = "free_y", nrow = 5) +
  labs(
    title = "Oblast (ADM1‑level) population over time",
    x = NULL,
    y = "Population",
    color = "Source"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  file.path(fig_dir, "vodafone_vs_facebookDL2023_time_series_obl.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 25,
  units = "cm",
  dpi = 300
)


pyramid_df <- comparison %>%
  mutate(
    pop_vf = ifelse(s_name == "M", -pop_estimated, pop_estimated),
    pop_bench = ifelse(s_name == "M", -DL_estimate, DL_estimate)
  )

for (adm in unique(pyramid_df$ADM1_EN)) {
  df_sub <- filter(pyramid_df, ADM1_EN == adm)
  p <- ggplot(df_sub, aes(x = a_name)) +
    geom_col(
      aes(y = pop_vf, fill = s_name),
      width = 0.8,
      alpha = 0.8,
      show.legend = FALSE
    ) +
    geom_line(aes(y = pop_bench, color = s_name, group = s_name), size = 1.1) +
    coord_flip() +
    facet_wrap(~t, ncol = 4, scales = "free_y") +
    scale_y_continuous(labels = abs, breaks = scales::pretty_breaks()) +
    scale_fill_manual(
      values = c(F = "#CF932C", M = "#8C86A0"),
      name = NULL,
      labels = c(F = "Female (Vodafone est.)", M = "Male (Vodafone est.)")
    ) +
    scale_colour_manual(
      values = c(F = "#CF932C", M = "#8C86A0"),
      name = NULL,
      labels = c(
        F = "Female (Facebook - Douglas et al. 2023)",
        M = "Male (Facebook - Douglas et al. 2023)"
      )
    ) +
    labs(
      x = "Age group",
      y = "Population",
      title = paste0("Vodafone estimates vs. Facebook estimates: ", adm)
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom"
    )

  fname <- paste0(
    "Vodafone_vs_Facebook_pop_pyr_obl_",
    gsub("\\s+", "_", adm),
    ".jpeg"
  )
  fpath <- file.path(out_dir, fname)

  ggsave(
    filename = fpath,
    plot = p,
    width = 35,
    height = 25,
    units = "cm",
    dpi = 300
  )
}

# Compare Vodafone estimates vs DTM IDP estimates ----

## Download IDP numbers from DTM ----
set_subscription_key(key = env$DTM_API_KEY)

idp <- get_idp_admin1_data(
  CountryName = "Ukraine",
  FromReportingDate = "2022-01-01",
  ToReportingDate = "2025-12-01"
)

## Compute movers from the flows data ----

library(data.table)

movers <- flows_hromada_agesex[
  origin_oblast != destination_oblast,
  .(movers = sum(pop_estimated)),
  by = .(t, admin1Pcode = str_sub(destination_raion, 1, 4), destination_oblast)
]

movers <- movers |>
  mutate(
    admin1Pcode = ifelse(admin1Pcode == "Kyiv", "UA80", admin1Pcode),
    t = as.Date(t)
  ) |>
  left_join(
    idp |>
      mutate(
        t = as.Date(
          paste0(
            "01",
            "-",
            month(as.Date(reportingDate)),
            "-",
            year(as.Date(reportingDate))
          ),
          format = "%d-%m-%Y"
        )
      ) |>
      group_by(admin1Pcode, t) |>
      summarise(idp = mean(numPresentIdpInd))
  ) |>
  mutate(
    diff = movers - idp,
    diff_perc = diff / idp
  )

movers |>
  filter(t > as.Date('2022-04-01')) |>
  filter(t < as.Date('2025-05-01')) |>
  pull(diff_perc) |>
  summary()

movers |>
  filter(t > as.Date('2025-05-01')) |>
  filter(t < as.Date('2025-07-01')) |>
  pull(diff_perc) |>
  summary()


## Plot comparison ----
ggplot(
  movers |> filter(!is.na(idp) & t > as.Date("2022-02-01")),
  aes(x = t, y = diff_perc, col = destination_oblast)
) +
  geom_line() +
  facet_wrap(. ~ destination_oblast) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Oblast-level comparison between IDP counts and estimated movers count",
    caption = "IDP counts from IOM Displacement Tracking Matrix\nMovers counts estimated from Vodafone flows data",
    y = "(Movers-IDP)/IDP",
    x = ''
  ) +
  paletteer::scale_color_paletteer_d("pals::stepped") +
  lims(y = c(-1, 10000))

ggsave(
  file.path(fig_dir, "vodafone_vs_dtm_idp_time_series_oblast.jpeg"),
  plot = last_plot(),
  width = 35,
  height = 30,
  units = "cm",
  dpi = 300
)
ggplot(
  movers |> filter(!is.na(idp) & t > as.Date("2022-03-01")),
  aes(x = t, y = diff_perc, col = destination_raion)
) +
  geom_line() +
  facet_wrap(. ~ destination_oblast, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "Difference between IDP and movers: after the start of the war",
    y = "(Movers-IDP)/IDP (%)"
  )

ggplot(
  movers |> filter(!is.na(idp)),
  aes(x = t, y = idp, col = destination_oblast)
) +
  geom_line() +
  facet_wrap(. ~ destination_oblast, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "IDP from the DTM data", y = "count")

ggplot(
  movers |> filter(!is.na(idp)),
  aes(x = t, y = movers, col = destination_oblast)
) +
  geom_line() +
  facet_wrap(. ~ destination_oblast, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Movers from the flows data", y = "count")
