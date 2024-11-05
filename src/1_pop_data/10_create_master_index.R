# Load required helpers
source("R_helpers/generic.R")
source("R_helpers/data_querying.R")

# Access the environment variables

out_dir <- file.path(out_dir, "population_proxy","social_media_audience")
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)


# Input parameters -------------------------------------------------------


# agesex demographic groups

agesex <- c("T_13Plus")

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
date_start <- '2022-02-25'
date_end <- '2024-11-01'

country <- "UA"

meta_keys <- read_csv(file.path(out_dir, paste0(tolower(country), "_meta_keys.csv")))

output_label <- ''

# create master_index ----------------------------------------------------

geo_index <- meta_keys |> 
  rename(meta_name = geo_name, meta_key = geo_key) |>
  distinct(meta_key, meta_name) |> 
  arrange(meta_key) |> 
  mutate(i = 1:n())

time_index <- tibble(
  collection_date = seq(as.Date(date_start), as.Date(date_end), by = 1)
) |> 
  arrange(collection_date) |> 
  mutate(
    t = paste0(as.numeric(format(collection_date, "%y")) ,as.numeric(format(collection_date, "%W"))) |> 
      as_factor() |> 
      as.numeric()
  ) 

agesex_index <- lapply(agesex, function(agesex_) agesex_col_to_query_args(agesex_) |> as_tibble())|> 
  bind_rows() |> 
  arrange(age_min) |> 
  mutate(
    sa_label = agesex,
    s=gender,
    a = as.numeric(factor(paste0(age_min, age_max)))
  )

master_index <- expand_grid(
  t = unique(time_index$t),
  i = geo_index$i,
  a = unique(agesex_index$a),
  s = unique(agesex_index$s)
) |> 
  left_join(time_index |> 
    arrange(collection_date) |>
    group_by(t) |>
    filter(row_number()==1), by = "t") |> 
  left_join(geo_index, by = "i") |> 
  left_join(agesex_index, by = c("a", "s")) |> 
  arrange(t, i, a, s)


# Write output -----------------------------------------------------------


write_csv(master_index, file.path(out_dir, paste0(tolower(country), "_master_index",output_label,".csv")))
