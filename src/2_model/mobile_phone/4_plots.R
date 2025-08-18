source(file.path(here::here(), "R_helpers/generic.R"))
library(tmap)
hromada_geo <- st_read(file.path(out_dir, "ua_master_hromada.gpkg"))
flows_hromada_agesex <- data.table::fread(file.path(
  out_dir, "model", "deterministic", "deliverables", "202508",
  paste0(tolower(country), "_flows_hromada_agesex", output_label, ".csv")
))
flows_hromada_agesex_raw <- data.table::fread(file.path(out_dir, "population_proxy", "mobile_phone", "vodafone_monthlyFlows.csv"))

pcodes <- read_csv(file.path(here::here("src/dashboard/api/app/data/db-data/global_pcodes.csv")))

pcodes <- pcodes |>
  filter(Location == "UKR") |>
  rename(pcode = `P-Code`)


dir.create(file.path(out_dir, "model", "deterministic", "figs"), showWarnings = F)

stocks_hromada_agesex <- flows_hromada_agesex |>
  as_tibble() |>
  group_by(t, a_name, s_name,
    hromada = destination_hromada, oblast = destination_oblast, raion = destination_raion,
    hromada_PCODE = destination_hromada_PCODE, raion_PCODE = destination_raion_PCODE
  ) |>
  summarise(
    pop_estimated = sum(pop_estimated),
    .groups = "drop"
  )

flows_hromada_totals_last <- flows_hromada_agesex |>
  as_tibble() |>
  filter(t == max(stocks_hromada_agesex$t)) |>
  group_by(
    t, origin_macroregion, origin_oblast, origin_raion, origin_hromada, origin_hromada_PCODE,
    destination_macroregion, destination_oblast, destination_raion, destination_hromada, destination_hromada_PCODE
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

stocks_hromada_agesex_raw <- flows_hromada_agesex_raw |>
  group_by(t, s_name, a_name, hromada = destination_hromada, ) |>
  summarise(subscribers_monthlyFlow = sum(subscribers_monthlyFlow))

stocks_hromada_agesex <- stocks_hromada_agesex |>
  left_join(stocks_hromada_agesex_raw) |>
  mutate(
    penetration_rate = subscribers_monthlyFlow / pop_estimated
  )

# Assessment of data availibility ----------------------------------------
# See scripts 1_pop_data/60_vodafone.R. Section Monthly flows data availability


# Map of total population estimates at hromada level for most recent time
hromada_geo_pop <- hromada_geo |>
  left_join(
    stocks_hromada_totals_last |>
      rename(hromada_code = hromada)
  )

tm_pop_last <- tm_shape(hromada_geo_pop) +
  tm_polygons(
    fill = "pop_estimated",
    fill.scale = tm_scale_intervals(
      values = "brewer.reds", style = "fixed", breaks = c(0, 5000, 50000, 500000, max(stocks_hromada_totals_last$pop_estimated))
    ),
    fill.legend = tm_legend(title = "18-64 years old", position = tm_pos_in("right", "bottom")),
    col = "white"
  ) +
  tm_layout(
    frame = FALSE, legend.frame = FALSE
  ) +
  tm_title(paste0("Population estimated on ", stocks_hromada_totals_last$t[1], " [", round(sum(stocks_hromada_totals_last$pop_estimated)), " people]"))

tm_pop_last
tmap_save(tm_pop_last,
  filename = file.path(out_dir, "model", "deterministic", "figs", paste0("map_pop_", stocks_hromada_totals_last$t[1], ".png")),
)

# Time series of total pop and in/out flows abroad (by age-sex)
# see 0_deterministic.R

# Hromada population and penetration rate through time


for (h in unique(stocks_hromada_agesex$hromada)) {
  # h='UA05020010000053508'
  df_hromada <- stocks_hromada_agesex |>
    filter(hromada == h) |>
    pivot_longer(cols = c(pop_estimated, penetration_rate, subscribers_monthlyFlow))
  ggplot(df_hromada |>
    mutate(name = factor(name, levels = c("pop_estimated", "penetration_rate", "subscribers_monthlyFlow"))), aes(x = t, y = value, colour = a_name, linetype = name)) +
    geom_line() +
    facet_grid(name ~ s_name, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste("Hromada population and penetration rate through time in\n", h, "in oblast", df_hromada$oblast[1]),
      x = "Time", y = ""
    )+
    guides(linetype="none")

  dir.create(file.path(out_dir, "model", "deterministic", "figs", "Penetration rate", df_hromada$oblast[1], df_hromada$raion[1]), showWarnings = F, recursive = T)
  ggsave(
    file.path(
      out_dir, "model", "deterministic", "figs", "Penetration rate", df_hromada$oblast[1], df_hromada$raion[1],
      paste0("penRate_hromada_", h, ".png")
    ),
    width = 8,
    height = 6
  )
}


# Top migration corridors

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
      rename(destination_hromada_PCODE = pcode, destination_hromada_name = Name) |>
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

gg_corridor <- ggplot(top_corridor_last, aes(y = reorder(corridor, pop_estimated), x = pop_estimated, fill = s_name)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  theme_minimal() +
  facet_grid(. ~ a_name, scales = "free") +
  labs(
    y = "", x = "Estimated population (18-64 years old)", fill = "Gender",
    title = paste0("Top ten flows corridor by age and sex\nbetween ", as.Date(max(stocks_hromada_agesex$t)) - months(1), " and ", max(stocks_hromada_agesex$t))
  )
gg_corridor

ggsave(file.path(out_dir, "model", "deterministic", "figs", "corridor_agesex_top10.png"), gg_corridor,
  w = 8, height = 6
)

# Population pyramid for a couple locations baseline vs current

stocks_hromada_agesex_pyramid <- stocks_hromada_agesex |>
  filter(t == max(t) | t == min(t)) |>
  left_join(
    pcodes |>
      filter(`Admin Level` == 3) |>
      rename(hromada_PCODE = pcode, hromada_name = Name)
  ) |>
  filter(hromada_name %in% c("Kyiv", "Lvivska", "Kharkivska", "Chernivetska", "Sumska", "Dniprovska"))


gg_pyramid <- ggplot(
  stocks_hromada_agesex_pyramid,
  aes(x = a_name, y = if_else(s_name == "M", -pop_estimated, pop_estimated), fill = s_name, alpha = factor(t), group = factor(t))
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
  facet_wrap(. ~ hromada_name, scales = "free")
gg_pyramid

ggsave(file.path(out_dir, "model", "deterministic", "figs", "pyramid_hromada_firstLast.png"), gg_pyramid,
  w = 8, height = 6
)

# Map of arrivers from abroad
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
      values = "brewer.reds", style = "fixed", breaks = c(0, 100, 500, 5000, max(abroad_arrivals_last$pop_estimated))
    ),
    fill.legend = tm_legend(title = "18-64 years old", position = tm_pos_in("right", "bottom")),
    col = "white"
  ) +
  tm_layout(
    frame = FALSE, legend.frame = FALSE
  ) +
  tm_title(
    paste0("Monthly arrivals from abroad estimated on ", stocks_hromada_totals_last$t[1], " [", round(sum(abroad_arrivals_last$pop_estimated)), " people]")
  )

tm_abroad_arrivals_last

tmap_save(tm_abroad_arrivals_last,
  filename = file.path(out_dir, "model", "deterministic", "figs", paste0("map_abroad_arrivals_", stocks_hromada_totals_last$t[1], ".png")),
)


# Map of leavers to abroad

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
      values = "brewer.reds", style = "fixed", breaks = c(0, 100, 500, 5000, max(abroad_leavers_last$pop_estimated))
    ),
    fill.legend = tm_legend(
      title = "18-64 years old", position = tm_pos_in("right", "bottom")
    ),
    col = "white"
  ) +
  tm_layout(
    frame = FALSE, legend.frame = FALSE
  ) +
  tm_title(paste("Monthly leavers to abroad estimated on", stocks_hromada_totals_last$t[1], " [", round(sum(abroad_leavers_last$pop_estimated)), " people]"))
tm_abroad_leavers_last

tmap_save(tm_abroad_leavers_last,
  filename = file.path(out_dir, "model", "deterministic", "figs", paste0("map_abroad_leavers_", stocks_hromada_totals_last$t[1], ".png")),
)

# Map of internal leavers

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
      values = "brewer.reds", style = "fixed", breaks = c(0, 500, 1000, 10000, max(leavers_last$pop_estimated))
    ),
    fill.legend = tm_legend(
      title = "18-64 years old", position = tm_pos_in("right", "bottom")
    ),
    col = "white"
  ) +
  tm_layout(
    frame = FALSE, legend.frame = FALSE
  ) +
  tm_title(paste0("Origin of monthly leavers to domestic estimated on ", stocks_hromada_totals_last$t[1], " [", round(sum(leavers_last$pop_estimated)), " people]"))

tm_leavers_last

tmap_save(tm_leavers_last,
  filename = file.path(out_dir, "model", "deterministic", "figs", paste0("map_internal_leavers_", stocks_hromada_totals_last$t[1], ".png")),
)


# Map of arrivers
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
      values = "brewer.reds", style = "fixed", breaks = c(0, 500, 1000, 10000, max(arrivers_last$pop_estimated))
    ),
    fill.legend = tm_legend(
      title = "18-64 years old", position = tm_pos_in("right", "bottom")
    ),
    col = "white"
  ) +
  tm_layout(
    frame = FALSE, legend.frame = FALSE
  ) +
  tm_title(paste0("Destination of monthly arrivers from domestic estimated on ", stocks_hromada_totals_last$t[1], " [", round(sum(arrivers_last$pop_estimated)), " people]"))

tm_arrivers_last

tmap_save(tm_arrivers_last,
  filename = file.path(out_dir, "model", "deterministic", "figs", paste0("map_internal_arrivers_", stocks_hromada_totals_last$t[1], ".png")),
)
