# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
source(file.path(here::here(), "R_helpers/data_querying.R"))

# Input parameters -------------------------------------------------------


# agesex demographic groups

agesex <- c("F_20_29", "F_30_39", "F_40_49", "F_50_59", "F_60Plus", 
            "M_20_29", "M_30_39", "M_40_49", "M_50_59", "M_60Plus")

# agesex <- c("F_13Plus", "F_18Plus", "F_20Plus", "F_13_19", "F_15_49", "F_15_64", "F_18_34", "F_20_29", "F_30_39", "F_40_49", "F_50_59", "F_60Plus", "F_65Plus",
#            "M_13Plus", "M_18Plus", "M_20Plus", "M_13_19", "M_15_49", "M_15_64", "M_18_34", "M_20_29", "M_30_39", "M_40_49", "M_50_59", "M_60Plus", "M_65Plus",
#            "T_13Plus", "T_18Plus", "T_20Plus", "T_13_19", "T_15_49", "T_15_64", "T_18_34", "T_20_29", "T_30_39", "T_40_49", "T_50_59", "T_60Plus", "T_65Plus")

# Function to create regular age groupings
# create_regular_group <- function(age_gap, gender = c("F", "M"), age_max=60) {
#     age_group <- c(paste(seq(from = 20, to = age_max-age_gap, by = age_gap),
#     seq(from = 20+age_gap-1, to = age_max, by = age_gap), sep = "_"), paste0(age_max, 'Plus'))
#   lapply(gender, function(g) paste(g, age_group, sep = "_")) |> unlist()
# }
# agesex <- create_regular_group(10)

# date
date_start <- "2022-02-25"
date_end <- "2023-02-24"

country <- "UA"

meta_keys <- read_csv(file.path(env$repo_dir, "data", 'meta', paste0(tolower(country), "_meta_keys.csv")))
pcodes <- read_csv(file = file.path(env$repo_dir, "data", "COD-PS", "population_baseline.csv"))

output_label <- ""

# create master_index ----------------------------------------------------

geo_index <- meta_keys |>
  rename(i_name = geo_name, i_key = geo_key) |>
  distinct(i_name, i_key) |>
  left_join(
    pcodes |>
      select(fb_key, ADM1_PCODE, ADM1_EN) |>
      rename(i_key = fb_key)
  ) |>
  arrange(i_key) |>
  mutate(
    i = 1:n(),
    i_key = as.integer(i_key)
  )

time_index_expanded <- tibble(
  collection_date = seq(as.Date(date_start), as.Date(date_end), by = 1)
) |>
  mutate(
    t_name = floor_date(as.Date(collection_date), "week", week_start = 1),
    t_key = str_replace_all(as.character(t_name), "-", "") |> as.integer(),
    t = t_name |> as.character() |> as_factor() |> as.numeric()
  )

time_index <- time_index_expanded |>
  distinct(t_name, t_key, t) |>
  arrange(t)

agesex_index <- lapply(agesex, function(agesex_) agesex_col_to_query_args(agesex_) |> as_tibble()) |>
  bind_rows() |>
  mutate(
    a_name = str_sub(agesex, 3),
    a = as.integer(factor(paste0(age_min, age_max))),
    a_key = paste0(age_min, str_pad(age_max, 3, pad = "0")) |> as.integer(),
    s_name = str_sub(agesex, 1, 1),
    s = ifelse(s_name == "F", 2, 1),  # Assign 2 for F and 1 for M
    s_key = s
  ) |>
  select(-age_min, -age_max, -gender)


master_index <- expand_grid(
  t = unique(time_index$t),
  i = geo_index$i,
  a = unique(agesex_index$a),
  s = unique(agesex_index$s)
) |>
  left_join(time_index, by = "t") |>
  left_join(geo_index, by = "i") |>
  left_join(agesex_index, by = c("a", "s")) |>
  mutate(parameter = row_number())
  arrange(t, i, a, s)


# Write output -----------------------------------------------------------

write_csv(master_index, file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
write_csv(time_index_expanded, file.path(out_dir, paste0(tolower(country), "_time_index", output_label, ".csv")))
