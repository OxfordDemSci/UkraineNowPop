# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
library(data.table)

# Script parameter

# Load data
flows <- fread(
  file.path(
    out_dir, "model", "deterministic", "deliverables", "202508",
    paste0(tolower(country), "_flows_hromada_agesex", output_label, ".csv")
  ),
  # Read only what we actually use
  select = c(
    "t", "a_name", "s_name", "pop_estimated",
    "origin_hromada", "destination_hromada",
    "origin_hromada_PCODE", "origin_raion_PCODE", "origin_oblast_PCODE",
    "destination_hromada_PCODE", "destination_raion_PCODE", "destination_oblast_PCODE"
  )
)


parse_age_sex <- function(DT) {
  # Split "a_name" like "0-4" or "65Plus"
  DT[, c("age_min0", "age_max0") := tstrsplit(a_name, "-", fixed = TRUE)]
  DT[, age_min := fifelse(
    grepl("Plus", a_name),
    as.integer(gsub("\\D", "", a_name)),
    as.integer(age_min0)
  )]
  DT[, age_max := fifelse(grepl("Plus", a_name), 999L, as.integer(age_max0))]
  DT[, sex := fifelse(s_name == "M", 1L, 2L)]
  DT[, `:=`(country = "UKR", day = t)]
  DT[, c("age_min0", "age_max0", "s_name", "a_name", "t") := NULL]
  DT[]
}

agg_level <- function(DT, dest_col, level) {
  # Summarize to one admin level; renames destination_* into `pcode`
  DT[, .(pop = sum(pop)), by = .(day, age_min, age_max, sex, country, pcode = get(dest_col))][
    , `:=`(admin_level = level, pop = as.integer(round(pop)))
  ]
}

agg_level_flows <- function(DT, dest_col, orig_col, level) {
  DT[get(dest_col) != get(orig_col), .(count = sum(count)),
    by = .(day, age_min, age_max, sex, country,
      destination = get(dest_col), origin = get(orig_col)
    )
  ][
    , `:=`(admin_level = level, count = as.integer(round(count)))
  ]
}

# --- POP STOCKS --------------------------------------------------------------
# group once, then reshape to levels
stocks <- flows[destination_hromada_PCODE != "Abroad" & t %in% c("2025-05-01", "2025-06-01", "2025-07-01"),
  .(pop = sum(pop_estimated)),
  by = .(
    t, a_name, s_name,
    destination_hromada_PCODE, destination_raion_PCODE, destination_oblast_PCODE
  )
]
stocks <- parse_age_sex(stocks)

stocks_lvl <- rbindlist(list(
  agg_level(stocks, "destination_hromada_PCODE", 3L),
  agg_level(stocks, "destination_raion_PCODE", 2L),
  agg_level(stocks, "destination_oblast_PCODE", 1L)
), use.names = TRUE)

# Add uncertainty columns cheaply
stocks_lvl[, `:=`(pop_upper = pop, pop_lower = pop)]

# NOTE: Creating a 100-length posterior string per row is very costly.
stocks_lvl[, pop_posterior := {
  x <- as.integer(rnorm(50, mean = pop, sd = 1))
  paste0("[", paste(x, collapse = ", "), "]")
}, by = .(day, age_min, age_max, sex, country, pcode, admin_level)]

setcolorder(stocks_lvl, c("country", "admin_level", "pcode", "day", "age_min", "age_max", "sex", "pop", "pop_upper", "pop_lower"))

fwrite(
  stocks_lvl,
  file = file.path(env$repo_dir, "src", "dashboard", "api", "app", "data", "db-data", "pop.csv")
)

# --- POP FLOWS ---------------------------------------------------------------
flows_dt <- flows[
  origin_hromada != "Unknown" &
    destination_hromada != "Abroad" & t %in% c("2025-05-01", "2025-06-01", "2025-07-01"),
  .(count = sum(as.integer(pop_estimated))),
  by = .(
    t, a_name, s_name,
    origin_hromada_PCODE, origin_raion_PCODE, origin_oblast_PCODE,
    destination_hromada_PCODE, destination_raion_PCODE, destination_oblast_PCODE
  )
]

flows_dt <- parse_age_sex(flows_dt)

flows_lvl <- rbindlist(list(
  agg_level_flows(flows_dt, "destination_hromada_PCODE", "origin_hromada_PCODE", 3L),
  agg_level_flows(flows_dt, "destination_raion_PCODE", "origin_raion_PCODE", 2L),
  agg_level_flows(flows_dt, "destination_oblast_PCODE", "origin_oblast_PCODE", 1L)
), use.names = TRUE)

# Probability within (day, age band, sex, admin_level)
flows_lvl[, probability := count / sum(count),
  by = .(day, age_min, age_max, sex, admin_level)
]

setcolorder(flows_lvl, c("country", "admin_level", "day", "age_min", "age_max", "sex", "origin", "destination", "count", "probability"))

fwrite(
  flows_lvl,
  file = file.path(env$repo_dir, "src", "dashboard", "api", "app", "data", "db-data", "migration.csv")
)
