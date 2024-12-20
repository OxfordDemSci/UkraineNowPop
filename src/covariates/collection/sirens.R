# Load necessary libraries
library(git2r)
library(tidyverse)
library(fs)

# Load environment variables from .env file
env <- new.env()
source(here::here(".env"), local = env)
out_dir <- env$out_dir

country <- "UA"
output_label <- ""

# Clone repository
sirenRepo_dir <- file.path(out_dir, "covariates", "raw", "ukrainian-air-raid-sirens-dataset")
sirenRepo_url <- "https://github.com/Vadimkin/ukrainian-air-raid-sirens-dataset.git"

if (!dir.exists(sirenRepo_dir)) {
  git2r::clone(sirenRepo_url, sirenRepo_dir)
}

git2r::pull(sirenRepo_dir)

# Load data
time_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_time_index.csv")))
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index.csv")))
master_index <- master_index |>
  distinct(i_key, i_name, i, t, t_name, t_key, ADM1_PCODE)
sirens_official <- read_csv(file.path(sirenRepo_dir, "datasets", "official_data_en.csv"))
sirens_volunteered <- read_csv(file.path(sirenRepo_dir, "datasets", "volunteer_data_en.csv")) |>
  rename(oblast = region)

# Concatenate official and unoffical sources
sirens <- bind_rows(sirens_official, sirens_volunteered)

sirens <- sirens |>
  mutate(
    date_start = as.Date(started_at),
    date_end = as.Date(finished_at),
    diff = as.integer(date_end - date_start)
  )

# split observations if sirens lasted for several day
sirens <- sirens |>
  select(oblast, date_start, date_end, diff) |>
  rowwise() |>
  mutate(
    collection_date = list(seq(date_start, date_end, 1))
  ) |>
  select(oblast, collection_date) |>
  unnest_longer(col = collection_date)

# aggregate sirens per oblast
sirens_oblast_t <- sirens |>
  filter(collection_date <= max(time_index$collection_date)) |>
  filter(collection_date >= min(time_index$collection_date)) |>
  mutate(
    sirens = 1
  ) |>
  left_join(
    time_index
  ) |>
  rowwise() |>
  mutate(
    i_name = str_remove(oblast, " oblast"),
    i_name = str_remove(i_name, " City")

  ) |>
  group_by(i_name, t, t_name, t_key) |>
  summarise(sirens = sum(sirens)) |>
  filter(i_name %in% master_index$i_name)|>
  right_join(
    master_index
  ) |>
  ungroup() |>
  mutate(
    sirens = ifelse(is.na(sirens), 0, sirens)
  ) |>
  pivot_longer(sirens,
    names_to = "covariate",
    values_to = "value"
  )


# write output -----------------------------------------------------------

write_csv(
  sirens_oblast_t,
  file.path(out_dir, "covariates", "interim", paste0(tolower(country), "_sirens_oblast", output_label, ".csv"))
)


#  viz -------------------------------------------------------------------

ggplot(sirens_oblast_t, aes(x = t, y = value, col = i_name)) +
  geom_line() +
  theme(legend.position = "None")
