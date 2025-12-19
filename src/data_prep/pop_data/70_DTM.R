# This script obtains population displacement data from the 
# IOM Displacement Tracking Matrix (DTM) AOI

# Load required libraries
# https://dtm.iom.int/online-interactive-resources/ukraine-regional-response-dashboard/index.html?category=Demographic%20Profile&survey=crossings
library(jsonlite)
library(tidyverse)

# Define URLs
url_date <- "https://dtm.iom.int/online-interactive-resources/ukraine-regional-response-dashboard/chart-data/crossings/crossings_filter_columns.json"
url_age_sex_breakdown <- "https://dtm.iom.int/online-interactive-resources/ukraine-regional-response-dashboard/chart-data/crossings/travel_group_age_sex_breakdown.json"

# Download and parse JSON into tibbles
date_raw <- fromJSON(url_date, flatten = TRUE)
age_sex_breakdown_raw <- fromJSON(url_age_sex_breakdown, flatten = TRUE)

# Convert to tibbles
date_tbl <- as_tibble(date_raw[["v"]])
colnames(date_tbl) <- date_raw$c
age_sex_breakdown_tbl <- as_tibble(age_sex_breakdown_raw$v)
colnames(age_sex_breakdown_tbl) <- age_sex_breakdown_raw$c

# Prepare data

date_tbl <- date_tbl |>
  mutate(
    y = case_when(
      year == 0 ~ 2023,
      year == 1 ~ 2024,
      T ~ NA
    ),
    m = case_when(
      quarter == 0 ~ "01-03",
      quarter == 1 ~ "01-06",
      quarter == 2 ~ "01-09",
      quarter == 3 ~ "01-12",
      T ~ NA
    ),
    t = as.Date(paste0(m, "-", y), format = "%d-%m-%Y")
  )

age_sex_breakdown_tbl$t <- date_tbl$t

# clean data

age_sex_breakdown_tbl <- age_sex_breakdown_tbl |>
  select(-num_in_group_including_yourself) |>
  filter(rowSums(across(where(is.numeric))) != 0) |>
  pivot_longer(-t, names_to = "age_sex", values_to = "count") |>
  group_by(t, age_sex) |>
  summarise(count = sum(count)) |>
  # rowwise() |>
  mutate(
    s_name = case_when(
      str_detect(age_sex, "fe") ~ "F",
      str_detect(age_sex, "ma") ~ "M",
      T ~ NA
    ),
    a = case_when(
      str_detect(age_sex, "0_4") ~ "0-4",
      str_detect(age_sex, "5-17") ~ "5-17",
      str_detect(age_sex, "18_59") ~ "18-59",
      str_detect(age_sex, "60_plus") ~ "60Plus",
      T ~ NA
    )
  ) |>
  select(-age_sex)

# Write to file
dir.create(file.path(out_dir, "population_proxy", "returnee"), showWarnings = F, recursive = T)
write_csv(age_sex_breakdown_tbl, file.path(out_dir, "population_proxy", "returnee", "dtm_returnees.csv"))
