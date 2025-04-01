# cleanup
rm(list = ls())
gc()

# Load required helpers
source(file.path(here::here(), "R_helpers/generic.R"))
source(file.path(here::here(), "R_helpers/data_querying.R"))

# Input parameters -------------------------------------------------------


# agesex demographic groups

agesex <- c(#"F_20_29", "F_30_39", "F_40_49", "F_50_59", 
  #"F_20_24", "F_25_29", "F_30_34", "F_35_39", "F_40_44", "F_45_49", "F_50_54", "F_55_59", "F_60_64", "F_65Plus",
  "F_18Plus",
 #"F_60Plus", "M_20_29", "M_30_39", "M_40_49", "M_50_59", "M_60Plus",
 #"M_20_24", "M_25_29", "M_30_34", "M_35_39", "M_40_44", "M_45_49", "M_50_54", "M_55_59", "M_60_64", "M_65Plus")
 "M_18Plus")

# Function to create regular age groupings
# create_regular_group <- function(age_gap, gender = c("F", "M"), age_max = 60) {
#   age_group <- c(paste(seq(from = 20, to = age_max - age_gap, by = age_gap),
#     seq(from = 20 + age_gap - 1, to = age_max, by = age_gap),
#     sep = "_"
#   ), paste0(age_max, "Plus"))
#   lapply(gender, function(g) paste(g, age_group, sep = "_")) |> unlist()
# }
# agesex <- create_regular_group(10)


# Define the reporting period and country
date_start <- "2022-02-25"
date_end   <- "2024-05-14"
country    <- "UA"

# Read metadata and population baseline files
meta_keys <- read_csv(file.path(env$repo_dir, "data", "meta", paste0(tolower(country), "_meta_keys.csv")))
pcodes    <- read_csv(file = file.path(env$repo_dir, "data", "cod-ps", "population_baseline.csv"))

output_label <- ""


agesex <- c(#"F_20_29", "F_30_39", "F_40_49", "F_50_59", 
  #"F_20_24", "F_25_29", "F_30_34", "F_35_39", "F_40_44", "F_45_49", "F_50_54", "F_55_59", "F_60_64", "F_65Plus",
  "F_18Plus",
 #"F_60Plus", "M_20_29", "M_30_39", "M_40_49", "M_50_59", "M_60Plus",
 #"M_20_24", "M_25_29", "M_30_34", "M_35_39", "M_40_44", "M_45_49", "M_50_54", "M_55_59", "M_60_64", "M_65Plus")
 "M_18Plus")


# create master_index ----------------------------------------------------

geo_index <- meta_keys |>
  rename(i_key = geo_key) |>
  distinct(i_key) |>
  left_join(
    pcodes |>
      mutate(macroregion = case_when(
           name=="Autonomous Republic of Crimea" ~ "Autonomous",
           name=="Cherkasy Oblast"                  ~ "Center",
           name=="Chernihiv Oblast"                  ~ "North",
           name=="Chernivtsi Oblast"                 ~ "West",
           name=="Dnipropetrovsk Oblast"             ~ "East",
           name=="Donetsk Oblast"                    ~ "East",
           name=="Ivano-Frankivsk Oblast"            ~ "West",
           name=="Kharkiv Oblast"                    ~ "East",
           name=="Kherson Oblast"                    ~ "South",
           name=="Khmelnytskyi Oblast"               ~ "West",
           name=="Kyiv Oblast"                       ~ "North",
           name=="Kirovohrad Oblast"                 ~ "Center",
           name=="Kyiv"                              ~ "City",
           name=="Luhansk Oblast"                    ~ "East",
           name=="Lviv Oblast"                       ~ "West",
           name=="Mykolaiv Oblast"                   ~ "South",
           name=="Odessa Oblast"                     ~ "South",
           name=="Poltava Oblast"                    ~ "Center",
           name=="Rivne Oblast"                      ~ "West",
           name=="Sevastopol"                        ~ "Autonomous",
           name=="Sumy Oblast"                       ~ "North",
           name=="Ternopil Oblast"                   ~ "West",
           name=="Vinnytsia Oblast"                  ~ "Center",
           name=="Volyn Oblast"                      ~ "West",
           name=="Zakarpattia Oblast"                ~ "West",
           name=="Zaporizhia Oblast"                 ~ "East",
           name=="Zhytomyr Oblast"                   ~ "North")) |> 
      select(fb_key, ADM1_PCODE, ADM1_EN, macroregion) |>
      rename(i_key = fb_key, i_name = ADM1_EN)
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
    t_key  = str_replace_all(as.character(t_name), "-", "") |> as.integer(),
    t      = t_name |> as.character() |> as_factor() |> as.integer()
  )

time_index <- time_index_expanded |>
  distinct(t_name, t_key, t) |>
  arrange(t)

agesex_index <- lapply(
  agesex, function(agesex_) {
    agesex_col_to_query_args(agesex_) |>
      as_tibble() |>
      mutate(agesex = agesex_)
  }
) |>
  bind_rows() |>
  mutate(
    a_name = str_sub(agesex, 3),
    a      = as.integer(factor(paste0(age_min, age_max))),
    a_key  = paste0(age_min, str_pad(age_max, 3, pad = "0")) |> as.integer(),
    s_name = str_sub(agesex, 1, 1),
    s      = ifelse(s_name == "F", 2, 1),  # 2 for F and 1 for M
    s_key  = s
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
  mutate(parameter = row_number()) |>
  arrange(t, i, a, s)


# Write output -----------------------------------------------------------

write_csv(master_index, file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")))
write_csv(time_index_expanded, file.path(out_dir, paste0(tolower(country), "_time_index", output_label, ".csv")))
