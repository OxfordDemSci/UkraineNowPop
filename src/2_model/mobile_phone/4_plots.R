gc()
rm(list = ls())

source(file.path(here::here(), "R_helpers/generic.R"))
library(tmap)
library(data.table)
library(future.apply)

# parameters ----
output_date <- "202510"
output_label <- ""
dir.create(
  file.path(out_dir, "model", "deterministic", "figs", output_date),
  showWarnings = F
)
f_color <- "#CF932C"
m_color <- "#8C86A0"

# load data ----
hromada_geo <- st_read(file.path(out_dir, "ua_master_hromada.gpkg"))
flows_hromada_agesex <- fread(file.path(
  out_dir,
  "model",
  "deterministic",
  "deliverables",
  output_date,
  paste0(tolower(country), "_flows_hromada_agesex", output_label, ".csv")
))
flows_hromada_agesex_raw <- rbind(
  fread(file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "vodafone_monthlyFlows.csv"
  )),
  fread(file.path(
    out_dir,
    "population_proxy",
    "mobile_phone",
    "vodafone_monthlyFlows_ngctToNgct.csv"
  ))[, c("origin_territory ", "destination_territory") := NULL],
  fill = T
)

pcodes <- read_csv(file.path(here::here(
  "src/dashboard/api/app/data/db-data/global_pcodes.csv"
)))

pcodes <- pcodes |>
  filter(Location == "UKR") |>
  rename(pcode = `P-Code`)

ngct_mapping <- fread(file.path(
  out_dir,
  "population_proxy",
  "mobile_phone",
  "vodafone_ngct_mapping.csv"
))

# prepare data ----
stocks_hromada_agesex <- flows_hromada_agesex[,
  .(pop_estimated = sum(pop_estimated)),
  by = .(
    t,
    a_name,
    s_name,
    hromada = destination_hromada,
    oblast = destination_oblast,
    raion = destination_raion,
    hromada_PCODE = destination_hromada_PCODE,
    raion_PCODE = destination_raion_PCODE,
    macroregion = destination_macroregion
  )
]

flows_hromada_totals_last <- flows_hromada_agesex |>
  as_tibble() |>
  filter(t == max(stocks_hromada_agesex$t)) |>
  group_by(
    t,
    origin_macroregion,
    origin_oblast,
    origin_raion,
    origin_hromada,
    origin_hromada_PCODE,
    destination_macroregion,
    destination_oblast,
    destination_raion,
    destination_hromada,
    destination_hromada_PCODE
  ) |>
  summarise(
    pop_estimated = sum(pop_estimated)
  )

stocks_hromada_agesex_first <- stocks_hromada_agesex |>
  filter(t == min(stocks_hromada_agesex$t))

stocks_hromada_agesex_last <- stocks_hromada_agesex |>
  filter(t == max(stocks_hromada_agesex$t))

stocks_hromada_totals_last <- stocks_hromada_agesex_last |>
  group_by(t, hromada, oblast, raion) |>
  summarise(pop_estimated = sum(pop_estimated))

stocks_hromada_totals <- stocks_hromada_agesex |>
  group_by(t, hromada, oblast, raion) |>
  summarise(pop_estimated = sum(pop_estimated))

stocks_hromada_agesex_raw <- flows_hromada_agesex_raw[,
  .(subscribers_monthlyFlow = sum(subscribers_monthlyFlow)),
  by = .(t, s_name, a_name, hromada = destination_hromada)
]

stocks_hromada_agesex <- stocks_hromada_agesex |>
  left_join(stocks_hromada_agesex_raw) |>
  mutate(
    penetration_rate = subscribers_monthlyFlow / pop_estimated
  ) |>
  left_join(
    pcodes |>
      filter(`Admin Level` == 3) |>
      rename(hromada_PCODE = pcode, hromada_name = Name) |>
      select(hromada_PCODE, hromada_name)
  )

stocks_raion_agesex <- stocks_hromada_agesex |>
  group_by(t, raion, raion_PCODE, oblast, a_name, s_name) |>
  summarise(
    pop_estimated = sum(pop_estimated),
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    penetration_rate = sum(subscribers_monthlyFlow) / sum(pop_estimated),
    .groups = "drop"
  ) |>
  left_join(
    pcodes |>
      filter(`Admin Level` == 2) |>
      rename(raion_PCODE = pcode, raion_name = Name) |>
      select(raion_PCODE, raion_name)
  )

stocks_oblast_agesex <- stocks_hromada_agesex |>
  group_by(t, macroregion, oblast, a_name, s_name) |>
  summarise(
    pop_estimated = sum(pop_estimated),
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    penetration_rate = sum(subscribers_monthlyFlow) / sum(pop_estimated),
    .groups = "drop"
  )

stocks_oblast <- stocks_hromada_agesex |>
  group_by(t, macroregion, oblast) |>
  summarise(
    pop_estimated = sum(pop_estimated, na.rm = T),
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(penetration_rate = subscribers_monthlyFlow / pop_estimated)


stocks_national <- stocks_oblast |>
  group_by(t) |>
  summarise(
    pop_estimated = sum(pop_estimated),
    subscribers_monthlyFlow = sum(subscribers_monthlyFlow),
    penetration_rate = sum(subscribers_monthlyFlow) / sum(pop_estimated),
    .groups = "drop"
  )


# Assessment of data availibility -----

# See scripts 1_pop_data/60_vodafone.R. Section Monthly flows data availability
hromada_avail_geo <- hromada_geo |>
  left_join(
    ngct_mapping
  ) |>
  mutate(
    availability = case_when(
      territory == "gct" ~ "Complete (monthly flows)",
      territory == "ngct" ~ "Partial (flows before Feb 2022)",
      oblast_name_en == 'Kyiv' ~ "Complete (monthly flows)",
      TRUE ~ "No data"
    ),
    availability = factor(
      availability,
      levels = c(
        "Complete (monthly flows)",
        "Partial (flows before Feb 2022)",
        "No data"
      )
    )
  )

## Map of hromada data availability ----
tm_avail <- tm_shape(hromada_avail_geo) +
  tm_polygons(
    fill = "availability",
    fill.scale = tm_scale_categorical(
      values = c("#C68D61", "#4F6008", 'grey')
    ),

    fill.legend = tm_legend(
      title = "Availability of Vodafone data",
      position = tm_pos_in("right", "bottom")
    ),
    col = "white",
    lwd = 0.1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = FALSE,
    asp = 1.5
  ) +
  tm_title("Hromadas with Vodafone data available")

tm_avail
tmap_save(
  tm_avail,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_data_availability.png")
  ),
  width = 8,
  height = 6
)

## Bar plot of hromada data availability by oblast ----

hromada_avail_df <- hromada_avail_geo |>
  st_set_geometry(NULL) |>
  as_tibble() |>
  mutate(
    availability = case_when(
      availability == "Complete (monthly flows)" ~ "Complete",
      availability == "Partial (flows before Feb 2022)" ~ "Partial",
      TRUE ~ "No data"
    )
  ) |>
  group_by(Oblast = oblast_name_en, availability) |>
  summarise(n_hromadas = n(), .groups = "drop") |>
  pivot_wider(
    names_from = availability,
    values_from = n_hromadas,
    values_fill = 0
  ) |>
  pivot_longer(
    cols = c(
      `Complete`,
      `Partial`,
      `No data`
    ),
    names_to = "Availability",
    values_to = "Count"
  ) |>
  filter(!is.na(Oblast)) |>
  filter(!grepl('Crimea', Oblast)) |>
  mutate(
    Availability = factor(
      Availability,
      levels = c(
        "Complete",
        "Partial",
        "No data"
      )
    )
  )

# Calculate order and label
hromada_avail_df <- hromada_avail_df |>
  group_by(Oblast) |>
  mutate(
    complete_count = Count[Availability == "Complete"],
    no_data_count = Count[Availability == "No data"],
    partial_count = Count[Availability == "Partial"]
  ) |>
  ungroup()

# Set factor levels for Oblast by complete_count
hromada_avail_df <- hromada_avail_df |>
  mutate(
    Oblast = factor(
      Oblast,
      levels = hromada_avail_df |>
        distinct(Oblast, complete_count) |>
        arrange(complete_count) |>
        pull(Oblast)
    )
  )

# Plot with label for No data
ggplot(hromada_avail_df, aes(x = Count, y = Oblast, fill = Availability)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    data = hromada_avail_df |>
      filter(Availability == "No data") |>
      filter(Count > 0),
    aes(
      label = paste(no_data_count, 'hromada without data'),
      x = complete_count + no_data_count + partial_count
    ),
    hjust = -0.1,
    color = "grey20",
    size = 3
  ) +
  labs(
    title = "Hromada-level Vodafone data availability by oblast",
    y = "",
    x = "Number of hromadas",
    fill = "Availability"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#C68D61", "#4F6008", 'grey')) +
  lims(x = c(0, max(hromada_avail_df$Count) + 20))
ggsave(
  file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    "bar_data_availability_oblast.png"
  ),
  width = 8,
  height = 6
)

## Count of hromadas with data missing ----
hromada_avail_df |>
  filter(Oblast %in% c('Donetska', 'Luhanska')) |>
  filter(Availability != "Complete") |>
  summarise(
    n_hromadas = sum(Count),
  )

# Map of total population estimates at hromada level for most recent time ----
hromada_geo_pop <- hromada_geo |>
  left_join(
    stocks_hromada_totals_last |>
      rename(hromada_code = hromada)
  )

tm_pop_last <- tm_shape(hromada_geo_pop) +
  tm_polygons(
    fill = "pop_estimated",
    fill.scale = tm_scale_intervals(
      values = "carto.red_or",
      style = "fixed",
      breaks = c(
        0,
        5000,
        50000,
        500000,
        max(stocks_hromada_totals_last$pop_estimated)
      ),
      value.na = 'grey'
    ),
    fill.legend = tm_legend(
      title = "Population",
      position = tm_pos_in("right", "bottom"),
    ),
    col = "white",
    lwd = 0.1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = FALSE,
    asp = 1.5
  ) +
  tm_title(paste0(
    "18-64 years old population estimated on ",
    stocks_hromada_totals_last$t[1]
  )) +
  tm_credits(
    paste0(
      scales::comma(sum(stocks_hromada_totals_last$pop_estimated)),
      " people across ",
      nrow(stocks_hromada_totals_last) - 2,
      " hromadas"
    ),
    position = c("left", "bottom")
  )

tm_pop_last

tmap_save(
  tm_pop_last,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_pop_", stocks_hromada_totals_last$t[1], ".png")
  ),
  width = 8,
  height = 6
)

# Largest population change compared to last month -----

stocks_hromada_change <- stocks_hromada_agesex_last |>
  left_join(
    stocks_hromada_agesex |>
      filter(t == as.Date(max(stocks_hromada_agesex$t)) - months(1)) |>
      rename(
        pop_estimated_before = pop_estimated,
        t_first = t
      )
  ) |>
  group_by(hromada, oblast, raion) |>
  summarise(
    pop_estimated = sum(pop_estimated),
    pop_estimated_before = sum(pop_estimated_before),
    .groups = "drop"
  ) |>
  mutate(
    Absolute = pop_estimated - pop_estimated_before,
    Relative = (pop_estimated - pop_estimated_before) /
      pop_estimated_before *
      100
  ) |>
  pivot_longer(
    cols = c(Absolute, Relative),
    names_to = "change_type",
    values_to = "Change"
  )

tm_pop_change <- tm_shape(
  hromada_geo |>
    left_join(
      stocks_hromada_change |>
        rename(hromada_code = hromada)
    ) |>
    filter(!is.na(Change))
) +
  tm_polygons(
    fill = "Change",
    fill.scale = tm_scale_intervals(
      values = "carto.tropic",
      style = "jenks",
      value.na = 'grey',
      midpoint = 0
    ),
    fill.free = TRUE,
    col = "white",
    lwd = 0.1
  ) +
  tm_layout(
    panel.labels = c("Absolute difference (people)", "Relative difference (%)"),
    panel.label.bg.color = 'white',
    frame = FALSE,
    legend.frame = FALSE,
    asp = 1.7
  ) +
  tm_title(paste0(
    "Population change (18-64 years old) in ",
    stocks_hromada_totals_last$t[1],
    " compared to previous month"
  )) +
  tm_facets(by = "change_type", ncol = 2)
tm_pop_change

tmap_save(
  tm_pop_change,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_pop_change_", stocks_hromada_totals_last$t[1], ".png")
  ),
  width = 8,
  height = 5
)
# Time series of total pop -----

hromada_counts <- stocks_hromada_totals |>
  group_by(oblast) |>
  summarise(n_hromadas = n_distinct(hromada))

gg_timeserie <- stocks_hromada_totals |>
  filter(oblast != 'Abroad') |>
  group_by(t, oblast) |>
  summarise(pop_estimated = sum(pop_estimated), .groups = "drop")

# Create a named vector for facet labels
facet_labels <- setNames(
  paste0(hromada_counts$oblast, "\nn=", hromada_counts$n_hromadas),
  hromada_counts$oblast
)

ggplot(gg_timeserie, aes(x = t, y = pop_estimated)) +
  geom_line(col = '#dd686c') +
  geom_point(col = '#dd686c', size = 0.5) +
  labs(
    x = "",
    y = "Estimated population (18-64 years old)",
    title = "Estimated population (18-64 years old) through time",
    caption = "n is the number of hromadas estimated in each oblast",
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(
    . ~ oblast,
    scales = "free_y",
    labeller = labeller(oblast = facet_labels)
  )

ggsave(
  file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    "timeserie_pop_oblast.png"
  ),
  width = 9,
  height = 6
)


# Time series of total pop and in/out flows abroad (by age-sex) -----

# see 0_deterministic.R

# Top migration corridors  -----

top_corridor_last <- flows_hromada_agesex |>
  as_tibble() |>
  filter(t == max(stocks_hromada_agesex$t)) |>
  rename(pop_estimated = pop_estimated) |>
  ungroup() |>
  filter(origin_hromada != destination_hromada & origin_hromada != "Abroad") |>
  filter(origin_hromada != "Unknown") |>
  filter(destination_hromada != "Abroad") |>
  group_by(a_name, s_name) |>
  filter(rank(desc(pop_estimated)) <= 10) |>
  ungroup() |>
  left_join(
    pcodes |>
      filter(`Admin Level` == 3) |>
      rename(
        destination_hromada_PCODE = pcode,
        destination_hromada_name = Name
      ) |>
      select(destination_hromada_PCODE, destination_hromada_name)
  ) |>
  left_join(
    pcodes |>
      filter(`Admin Level` == 3) |>
      rename(origin_hromada_PCODE = pcode, origin_hromada_name = Name) |>
      select(origin_hromada_PCODE, origin_hromada_name)
  ) |>
  mutate(
    corridor = paste(origin_hromada_name, ">", destination_hromada_name),
  )

gg_corridor <- ggplot(
  top_corridor_last,
  aes(y = reorder(corridor, pop_estimated), x = pop_estimated, fill = s_name)
) +
  geom_col(position = position_dodge2(preserve = "single"), alpha = 0.8) +
  theme_minimal() +
  facet_grid(. ~ a_name, scales = "free") +
  labs(
    y = "",
    x = "Estimated population (18-64 years old)",
    fill = "Gender",
    title = paste0(
      "Top ten hromada-level flows corridor by age and sex"
    ),
    subtitle = paste0(
      "Between ",
      as.Date(max(stocks_hromada_agesex$t)) - months(1),
      " and ",
      max(stocks_hromada_agesex$t)
    )
  ) +
  scale_fill_manual(values = c(f_color, m_color))
gg_corridor

ggsave(
  file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    "corridor_agesex_top10.png"
  ),
  gg_corridor,
  w = 8,
  height = 6
)

# Population pyramid for a couple locations baseline vs current ----

stocks_hromada_agesex_pyramid <- stocks_hromada_agesex |>
  filter(t == max(t) | t == min(t)) |>
  left_join(
    pcodes |>
      filter(`Admin Level` == 3) |>
      rename(hromada_PCODE = pcode, hromada_name = Name)
  ) |>
  filter(
    hromada_name %in%
      c("Kyiv", "Lvivska", "Kharkivska", "Berehivska", "Sumska", "Dniprovska")
  )

gg_pyramid <- ggplot(
  stocks_hromada_agesex_pyramid,
  aes(
    x = a_name,
    y = if_else(s_name == "M", -pop_estimated, pop_estimated),
    fill = s_name,
    alpha = factor(t),
    group = factor(t)
  )
) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_alpha_discrete(
    range = c(0.4, 0.7)
  ) +
  coord_flip() +
  labs(
    x = "",
    y = "Population",
    fill = "Gender",
    alpha = "Month",
    title = "Evolution of age-sex population estimates in a sample of hromadas"
  ) +
  theme_minimal() +
  facet_wrap(. ~ hromada_name, scales = "free") +
  scale_y_continuous(labels = function(x) scales::comma(abs(x))) +
  scale_fill_manual(values = c(f_color, m_color))
gg_pyramid

ggsave(
  file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    "pyramid_hromada_firstLast.png"
  ),
  gg_pyramid,
  w = 8,
  height = 6
)

# Map of arrivers from abroad ----

abroad_arrivals_last <- flows_hromada_totals_last |>
  filter(origin_hromada == "Abroad")

hromada_geo_abroad_arrivals <- hromada_geo |>
  left_join(
    abroad_arrivals_last |>
      rename(hromada_code = destination_hromada)
  )

tm_abroad_arrivals_last <- tm_shape(hromada_geo_abroad_arrivals) +
  tm_polygons(
    fill = "pop_estimated",
    fill.scale = tm_scale_intervals(
      values = "carto.red_or",
      style = "fixed",
      breaks = c(0, 100, 500, 5000, max(abroad_arrivals_last$pop_estimated))
    ),
    fill.legend = tm_legend(
      title = "18-64 years old",
      position = tm_pos_in("right", "bottom")
    ),
    col = "white",
    lwd = 0.1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = FALSE
  ) +
  tm_title(
    paste0(
      "Monthly arrivals from abroad estimated on ",
      stocks_hromada_totals_last$t[1]
    )
  ) +
  tm_credits(
    paste0(
      scales::number(round(sum(abroad_arrivals_last$pop_estimated))),
      " people arrived from abroad"
    ),
    position = c("left", "bottom")
  )

tm_abroad_arrivals_last

tmap_save(
  tm_abroad_arrivals_last,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_abroad_arrivals_", stocks_hromada_totals_last$t[1], ".png")
  ),
)

# Map of leavers to abroad ----

abroad_leavers_last <- flows_hromada_totals_last |>
  filter(destination_hromada == "Abroad")

hromada_geo_abroad_leavers <- hromada_geo |>
  left_join(
    abroad_leavers_last |>
      rename(hromada_code = origin_hromada)
  )


tm_abroad_leavers_last <- tm_shape(hromada_geo_abroad_leavers) +
  tm_polygons(
    fill = "pop_estimated",
    fill.scale = tm_scale_intervals(
      values = "carto.red_or",
      style = "fixed",
      breaks = c(0, 100, 500, 5000, max(abroad_leavers_last$pop_estimated))
    ),
    fill.legend = tm_legend(
      title = "18-64 years old",
      position = tm_pos_in("right", "bottom")
    ),
    col = "white",
    lwd = 0.1
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = FALSE
  ) +
  tm_title(paste(
    "Monthly leavers to abroad estimated on",
    stocks_hromada_totals_last$t[1]
  )) +
  tm_credits(
    paste0(
      scales::number(round(sum(abroad_leavers_last$pop_estimated))),
      " people left to abroad"
    ),
    position = c("left", "bottom")
  )
tm_abroad_leavers_last
tmap_save(
  tm_abroad_leavers_last,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_abroad_leavers_", stocks_hromada_totals_last$t[1], ".png")
  ),
)

tm_abroad_combined <- tmap_arrange(
  tm_abroad_arrivals_last,
  tm_abroad_leavers_last,
  ncol = 2,
  asp = 1.5
)
tm_abroad_combined

tmap_save(
  tm_abroad_combined,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_abroad_combined_", stocks_hromada_totals_last$t[1], ".png")
  ),
  width = 12,
  height = 6
)

# Map of internal movement ----
## Leavers ----
leavers_last <- flows_hromada_totals_last |>
  filter(destination_hromada != "Abroad") |>
  filter(origin_hromada != "Abroad") |>
  filter(origin_hromada != destination_hromada) |>
  group_by(
    origin_hromada
  ) |>
  summarise(
    pop_estimated = sum(pop_estimated)
  )

hromada_geo_leavers <- hromada_geo |>
  left_join(
    leavers_last |>
      rename(hromada_code = origin_hromada)
  )


tm_leavers_last <- tm_shape(hromada_geo_leavers) +
  tm_polygons(
    fill = "pop_estimated",
    fill.scale = tm_scale_intervals(
      values = "brewer.reds",
      style = "fixed",
      breaks = c(0, 500, 1000, 10000, max(leavers_last$pop_estimated))
    ),
    fill.legend = tm_legend(
      title = "18-64 years old",
      position = tm_pos_in("right", "bottom")
    ),
    col = "white"
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = FALSE
  ) +
  tm_title(paste0(
    "Origin of monthly leavers to domestic estimated on ",
    stocks_hromada_totals_last$t[1],
    " [",
    round(sum(leavers_last$pop_estimated)),
    " people]"
  ))

tm_leavers_last

tmap_save(
  tm_leavers_last,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_internal_leavers_", stocks_hromada_totals_last$t[1], ".png")
  ),
)


## Arrivers ----
arrivers_last <- flows_hromada_totals_last |>
  filter(destination_hromada != "Abroad") |>
  filter(origin_hromada != "Abroad") |>
  filter(origin_hromada != destination_hromada) |>
  group_by(
    destination_hromada
  ) |>
  summarise(
    pop_estimated = sum(pop_estimated)
  )

hromada_geo_arrivers <- hromada_geo |>
  left_join(
    arrivers_last |>
      rename(hromada_code = destination_hromada)
  )


tm_arrivers_last <- tm_shape(hromada_geo_arrivers) +
  tm_polygons(
    fill = "pop_estimated",
    fill.scale = tm_scale_intervals(
      values = "brewer.reds",
      style = "fixed",
      breaks = c(0, 500, 1000, 10000, max(arrivers_last$pop_estimated))
    ),
    fill.legend = tm_legend(
      title = "18-64 years old",
      position = tm_pos_in("right", "bottom")
    ),
    col = "white"
  ) +
  tm_layout(
    frame = FALSE,
    legend.frame = FALSE
  ) +
  tm_title(paste0(
    "Destination of monthly arrivers from domestic estimated on ",
    stocks_hromada_totals_last$t[1],
    " [",
    round(sum(arrivers_last$pop_estimated)),
    " people]"
  ))

tm_arrivers_last

tmap_save(
  tm_arrivers_last,
  filename = file.path(
    out_dir,
    "model",
    "deterministic",
    "figs",
    output_date,
    paste0("map_internal_arrivers_", stocks_hromada_totals_last$t[1], ".png")
  ),
)


# [INTERNAL] Population and penetration rate through time ----

# --- helper: prepare melted table (do once for each dataset) ---
melt_for_plot <- function(df) {
  dt <- as.data.table(df)
  melted <- melt(
    dt,
    id.vars = intersect(
      names(dt),
      c(
        "t",
        "a_name",
        "s_name",
        "hromada",
        "hromada_name",
        "oblast",
        "raion",
        "raion_name",
        "macroregion",
        "territory"
      )
    ),
    measure.vars = c(
      "pop_estimated",
      "penetration_rate",
      "subscribers_monthlyFlow"
    ),
    variable.name = "name",
    value.name = "value",
    variable.factor = FALSE
  )
  # set factor levels once
  melted[,
    name := factor(
      name,
      levels = c("pop_estimated", "penetration_rate", "subscribers_monthlyFlow")
    )
  ]
  return(melted)
}

precreate_dirs <- function(
  melted_dt,
  level = c("Hromada", "Raion", "Oblast"),
  out_dir_base
) {
  level <- match.arg(level)
  if (level == "Hromada") {
    dirs <- unique(melted_dt[, .(oblast, raion)])
    for (r in seq_len(nrow(dirs))) {
      dir.create(
        file.path(
          out_dir_base,
          "Penetration rate",
          "Hromada",
          dirs$oblast[r],
          dirs$raion[r]
        ),
        recursive = TRUE
      )
    }
  } else if (level == "Raion") {
    dirs <- unique(melted_dt[, .(oblast)])
    for (r in seq_len(nrow(dirs))) {
      dir.create(
        file.path(out_dir_base, "Penetration rate", "Raion", dirs$oblast[r]),
        recursive = TRUE
      )
    }
  } else if (level == "Oblast") {
    dir.create(
      file.path(out_dir_base, "Penetration rate", "Oblast"),
      recursive = TRUE
    )
  }
}

plot_PoPpenRateSubs <- function(
  key,
  split_list,
  level = c("Hromada", "Raion", "Oblast"),
  out_dir_base
) {
  level <- match.arg(level)
  df <- split_list[[key]]
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Build plot (title uses name + oblast/raion info)
  if (level == "Hromada") {
    display_name <- df$hromada_name[1]
    oblast_name <- df$oblast[1]
    raion_name <- df$raion[1]
    outfile <- file.path(
      out_dir_base,
      "Penetration rate",
      "Hromada",
      oblast_name,
      raion_name,
      paste0("penRate_hromada_", key, "_", display_name, ".png")
    )
    title_txt <- paste0(
      "Hromada population and penetration rate through time in\n",
      display_name,
      " in oblast ",
      oblast_name
    )
  } else if (level == "Raion") {
    display_name <- df$raion_name[1]
    oblast_name <- df$oblast[1]
    outfile <- file.path(
      out_dir_base,
      "Penetration rate",
      "Raion",
      oblast_name,
      paste0("penRate_raion_", key, "_", display_name, ".png")
    )
    title_txt <- paste0(
      "Raion population and penetration rate through time in\n",
      display_name,
      " in oblast ",
      oblast_name
    )
  } else if (level == "Oblast") {
    outfile <- file.path(
      out_dir_base,
      "Penetration rate",
      "Oblast",
      paste0("penRate_oblast_", key, ".png")
    )
    title_txt <- paste0(
      "Oblast population and penetration rate through time in\n",
      key
    )
  }

  p <- ggplot(df, aes(x = t, y = value, colour = a_name, linetype = name)) +
    geom_line() +
    facet_grid(name ~ s_name, scales = "free_y") +
    theme_minimal() +
    labs(title = title_txt, x = "Time", y = "") +
    guides(linetype = "none") +
    scale_y_continuous(labels = scales::label_number())

  # Use ragg device for speed
  ggsave(p, filename = outfile, width = 8, height = 6)

  invisible(NULL)
}

# Process Hromadas in parallel

out_dir_base <- file.path(
  out_dir,
  "model",
  "deterministic",
  "figs",
  output_date
)
melted_h <- melt_for_plot(stocks_hromada_agesex)

# Pre-create directories (avoid repeated dir.create inside each worker)
precreate_dirs(melted_h, level = "Hromada", out_dir_base = out_dir_base)

# split into list by hromada (cheap reference copy)
h_list <- split(melted_h, by = "hromada", keep.by = TRUE)

# set up parallel plan (works on Windows too: multisession)
plan(multisession, workers = 4)
# dispatch jobs (future_lapply will export needed objects automatically)
future_lapply(
  names(h_list),
  function(hid) {
    plot_PoPpenRateSubs(
      hid,
      h_list,
      level = "Hromada",
      out_dir_base = out_dir_base
    )
  },
  future.seed = TRUE
)
plan(sequential)


# Process Raions in parallel

melted_r <- melt_for_plot(stocks_raion_agesex)

# Pre-create directories for raions
precreate_dirs(melted_r, level = "Raion", out_dir_base = out_dir_base)

# split list by raion
r_list <- split(melted_r, by = "raion", keep.by = TRUE)

plan(multisession, workers = 4)
future_lapply(
  names(r_list),
  function(rid) {
    plot_PoPpenRateSubs(
      rid,
      r_list,
      level = "Raion",
      out_dir_base = out_dir_base
    )
  },
  future.seed = TRUE
)
plan(sequential)


# Process Oblast

melted_o <- melt_for_plot(stocks_oblast_agesex)
precreate_dirs(melted_o, level = "Oblast", out_dir_base = out_dir_base)

# split list by oblast
o_list <- split(melted_o, by = "oblast", keep.by = TRUE)

lapply(names(o_list), function(oid) {
  plot_PoPpenRateSubs(
    oid,
    o_list,
    level = "Oblast",
    out_dir_base = out_dir_base
  )
})

# Process national
melted_o_tot <- melt_for_plot(stocks_oblast)

ggplot(melted_o_tot, aes(x = t, y = value, colour = oblast, linetype = name)) +
  geom_line() +
  ggh4x::facet_grid2(macroregion ~ name, scales = "free_y", independent = "y") +
  theme_minimal() +
  labs(title = "Oblast population and penetration rate", x = "Time", y = "") +
  guides(linetype = "none", colour = "none") +
  scale_y_continuous(labels = scales::label_number())

ggsave(
  filename = file.path(
    out_dir_base,
    "Penetration rate",
    "Oblast",
    paste0("penRate_oblast_total.png")
  ),
  width = 8,
  height = 6
)

melted_n <- melt_for_plot(stocks_national)
melted_t <- melt_for_plot(stocks_territory)

ggplot(
  bind_rows(melted_n |> mutate(territory = "total"), melted_t),
  aes(x = t, y = value, linetype = name, colour = territory)
) +
  geom_line() +
  theme_minimal() +
  labs(title = "National population and penetration rate", x = "Time", y = "") +
  guides(linetype = "none", colour = "none") +
  ggh4x::facet_grid2(name ~ territory, scales = "free_y", independent = "y") +
  scale_color_manual(
    values = c("total" = "black", "gct" = "darksalmon", "ngct" = "darkgreen")
  ) +
  scale_y_continuous(labels = scales::label_number())
