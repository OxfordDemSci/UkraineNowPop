# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
library(data.table)

output_date <- "20251209"
sample <- T

# Script parameter

# Load data
flows <- fread(
  file.path(
    out_dir,
    "model",
    "deterministic",
    "deliverables",
    output_date,
    paste0(tolower(country), "_flows_hromada_agesex", output_label, ".csv")
  ),
  # Read only what we actually use
  select = c(
    "t",
    "a_name",
    "s_name",
    "pop_estimated",
    "origin_hromada",
    "destination_hromada",
    "origin_hromada_PCODE",
    "origin_raion_PCODE",
    "origin_oblast_PCODE",
    "destination_hromada_PCODE",
    "destination_raion_PCODE",
    "destination_oblast_PCODE"
  )
)

sensitive <- flows[
  t == min(t),
  length(unique(destination_hromada_PCODE)),
  by = .(destination_oblast_PCODE)
][
  V1 < 3 & !(destination_oblast_PCODE %in% c('Abroad', 'UA80')),
  destination_oblast_PCODE
]

if (sample) {
  flows <- flows[t >= as.Date("2025-02-01") & t <= as.Date("2025-05-01")]
}

parse_age_sex <- function(DT) {
  # Split "a_name" like "0-4" or "65Plus"
  DT[, c("age_min0", "age_max0") := tstrsplit(a_name, "-", fixed = TRUE)]
  DT[,
    age_min := fifelse(
      grepl("Plus", a_name),
      as.integer(gsub("\\D", "", a_name)),
      as.integer(age_min0)
    )
  ]
  DT[, age_max := fifelse(grepl("Plus", a_name), 999L, as.integer(age_max0))]
  DT[, sex := fifelse(s_name == "M", 1L, 2L)]
  DT[, `:=`(country = "UKR", day = t)]
  DT[, c("age_min0", "age_max0", "s_name", "a_name", "t") := NULL]
  DT[]
}

agg_level <- function(DT, dest_col, level) {
  DT[pop == 0, pop := 1]
  # Summarize to one admin level; renames destination_* into `pcode`
  DT[,
    .(pop = sum(pop)),
    by = .(day, age_min, age_max, sex, country, pcode = get(dest_col))
  ][,
    `:=`(admin_level = level, pop = as.integer(round(pop)))
  ]
  # replace 0 by 1 in pop
}

agg_level_flows <- function(DT, dest_col, orig_col, level) {
  DT[count == 0, count := 1]
  DT[
    get(dest_col) != get(orig_col),
    .(count = sum(count)),
    by = .(
      day,
      age_min,
      age_max,
      sex,
      country,
      destination = get(dest_col),
      origin = get(orig_col)
    )
  ][,
    `:=`(admin_level = level, count = as.integer(round(count)))
  ]
}

# --- POP STOCKS --------------------------------------------------------------
# group once, then reshape to levels
stocks <- flows[
  destination_hromada_PCODE != "Abroad",
  .(pop = sum(pop_estimated)),
  by = .(
    t,
    a_name,
    s_name,
    destination_hromada_PCODE,
    destination_raion_PCODE,
    destination_oblast_PCODE
  )
]
stocks <- parse_age_sex(stocks)

stocks_lvl <- rbindlist(
  list(
    agg_level(stocks, "destination_hromada_PCODE", 3L),
    agg_level(stocks, "destination_raion_PCODE", 2L),
    agg_level(
      stocks[destination_oblast_PCODE != sensitive],
      "destination_oblast_PCODE",
      1L
    )
  ),
  use.names = TRUE
)

# Add uncertainty columns cheaply
stocks_lvl[, `:=`(pop_upper = pop, pop_lower = pop)]

# NOTE: Creating a 100-length posterior string per row is very costly.
stocks_lvl[,
  pop_posterior := {
    x <- as.integer(rnorm(2, mean = pop, sd = 1))
    paste0("[", paste(x, collapse = ", "), "]")
  },
  by = .(day, age_min, age_max, sex, country, pcode, admin_level)
]

setcolorder(
  stocks_lvl,
  c(
    "country",
    "admin_level",
    "pcode",
    "day",
    "age_min",
    "age_max",
    "sex",
    "pop",
    "pop_upper",
    "pop_lower"
  )
)

pop_name <- ifelse(sample, "pop.csv", "pop_full.csv")
fwrite(
  stocks_lvl,
  file = file.path(
    env$repo_dir,
    "src",
    "dashboard",
    "api",
    "app",
    "data",
    "db-data",
    pop_name
  )
)

# --- POP FLOWS ---------------------------------------------------------------
flows_dt <- flows[
  origin_hromada != "Unknown" &
    destination_hromada != "Abroad",
  .(count = sum(as.integer(pop_estimated))),
  by = .(
    t,
    a_name,
    s_name,
    origin_hromada_PCODE,
    origin_raion_PCODE,
    origin_oblast_PCODE,
    destination_hromada_PCODE,
    destination_raion_PCODE,
    destination_oblast_PCODE
  )
]

flows_dt <- parse_age_sex(flows_dt)

flows_lvl <- rbindlist(
  list(
    agg_level_flows(
      flows_dt,
      "destination_hromada_PCODE",
      "origin_hromada_PCODE",
      3L
    ),
    agg_level_flows(
      flows_dt,
      "destination_raion_PCODE",
      "origin_raion_PCODE",
      2L
    ),
    agg_level_flows(
      flows_dt[
        destination_oblast_PCODE != sensitive & origin_oblast_PCODE != sensitive
      ],
      "destination_oblast_PCODE",
      "origin_oblast_PCODE",
      1L
    )
  ),
  use.names = TRUE
)

# Probability within (day, age band, sex, admin_level, destination)
flows_lvl[,
  proportion := round(count / sum(count) * 100, 2),
  by = .(day, age_min, age_max, sex, admin_level, destination)
]

setcolorder(
  flows_lvl,
  c(
    "country",
    "admin_level",
    "day",
    "age_min",
    "age_max",
    "sex",
    "origin",
    "destination",
    "count",
    "proportion"
  )
)

flow_name <- ifelse(sample, "migration.csv", "migration_full.csv")
fwrite(
  flows_lvl,
  file = file.path(
    env$repo_dir,
    "src",
    "dashboard",
    "api",
    "app",
    "data",
    "db-data",
    flow_name
  )
)

# adapt geographies

library(sf)
geo <- st_read('./src/dashboard/api/app/data/db-data/GEODATA_full.gpkg')
pop <- read_csv('./src/dashboard/api/app/data/db-data/pop.csv') |>
  filter(admin_level == 3 & day == min(day)) |>
  distinct(pcode)

geo_3 <- geo |>
  filter(admin_level == 3) |>
  left_join(
    pop |>
      mutate(
        gct = T
      )
  ) |>
  mutate(
    pcode_1 = str_sub(pcode, 1, 4),
    pcode_2 = str_sub(pcode, 1, 6)
  )

geo_ <- bind_rows(
  geo_3 |> select(pcode, geom, gct) |> mutate(admin_level = 3),
  geo_3 |>
    group_by(
      pcode_1,
      gct
    ) |>
    summarise() |>
    select(pcode = pcode_1, gct, geom) |>
    mutate(admin_level = 1),
  geo_3 |>
    group_by(
      pcode_2,
      gct
    ) |>
    summarise(n = n()) |>
    select(pcode = pcode_2, gct, geom) |>
    mutate(admin_level = 2)
)

geo_t <- geo_ |>
  full_join(
    geo |>
      st_drop_geometry()
  )

geo_t <- st_buffer(geo_t, 0.0)

geo_t <- geo_t |>
  mutate(
    country = 'UKR',
    pcode = ifelse(is.na(gct), paste0('Missing ', pcode), pcode),
    name_en = ifelse(is.na(gct), paste0('Missing ', name_en), name_en)
  ) |>
  select(-gct)

st_write(
  geo_t,
  './src/dashboard/api/app/data/db-data/GEODATA.gpkg',
  append = FALSE,
  layer = 'UKR'
)

st_write(
  geo_t |> filter(admin_level == 1),
  './src/dashboard/www/public_html/data/admin_UKR_level_1.geojson'
)

st_write(
  geo_t |> filter(admin_level == 2),
  './src/dashboard/www/public_html/data/admin_UKR_level_2.geojson',
  append = F
)

st_write(
  geo_t |> filter(admin_level == 3),
  './src/dashboard/www/public_html/data/admin_UKR_level_3.geojson',
  append = FALSE
)

st_write(
  geo |> filter(admin_level == 1),
  './src/dashboard/www/public_html/data/admin_UKR_level_1_baseline.geojson',
)
tm_shape(geo_t |> filter(admin_level == 1)) +
  tm_polygons("name_en")
