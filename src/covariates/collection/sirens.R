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
repo_dir <- file.path(out_dir, "covariates", "raw", "ukrainian-air-raid-sirens-dataset")
repo_url <- "https://github.com/Vadimkin/ukrainian-air-raid-sirens-dataset.git"

if (!dir.exists(repo_dir)) {
  git2r::clone(repo_url, repo_dir)
}

git2r::pull(repo_dir)

# Load data
time_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_time_index.csv")))
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index.csv")))
sirens_official <- read_csv(file.path(repo_dir, "datasets", "official_data_en.csv"))
sirens_volunteered <- read_csv(file.path(repo_dir, "datasets", "volunteer_data_en.csv")) |>
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
# Create a dictionary to map oblast names to i_name names
oblast_map <- c(
  "Cherkaska Oblast" = "Cherkasy Oblast",
  "Chernihivska Oblast" = "Chernihiv Oblast",
  "Chernivetska Oblast" = "Chernivtsi Oblast",
  "Dnipropetrovska Oblast" = "Dnipropetrovsk Oblast",
  "Donetska Oblast" = "Donetsk Oblast",
  "Ivano-Frankivska Oblast" = "Ivano-Frankivsk Oblast",
  "Kharkivska Oblast" = "Kharkiv Oblast",
  "Khersonska Oblast" = "Kherson Oblast",
  "Khmelnytska Oblast" = "Khmelnytska",
  "Kirovohradska Oblast" = "Kirovohrad Oblast",
  "Kyiv City" = "Kyiv",
  "Kyivska Oblast" = "Kiev Oblast",
  "Lubenskyi raion" = "Poltava Oblast",
  "Luhanska Oblast" = "Luhansk Oblast",
  "Lvivska Oblast" = "Lviv Oblast",
  "Mykolaivska Oblast" = "Mykolaiv Oblast",
  "Odeska Oblast" = "Odessa Oblast",
  "Poltavska Oblast" = "Poltava Oblast",
  "Rivnenska Oblast" = "Rivne Oblast",
  "Sumska Oblast" = "Sumy Oblast",
  "Ternopilska Oblast" = "Ternopil Oblast",
  "Vinnytska Oblast" = "Vinnytsia Oblast",
  "Volynska Oblast" = "Volyn Oblast",
  "Zakarpatska Oblast" = "Zakarpattia Oblast",
  "Zaporizka Oblast" = "Zaporizhia Oblast",
  "Zhytomyrska Oblast" = "Zhytomyr Oblast"
)

sirens_oblast_t <- sirens |>
  filter(collection_date <= max(time_index$collection_date)) |>
  mutate(
    sirens = 1
  ) |>
  left_join(
    time_index
  ) |>
  group_by(oblast, t, t_name, t_key) |>
  summarise(sirens = sum(sirens)) |>
  rowwise() |>
  mutate(
    i_name = str_replace_all(oblast, "oblast", "Oblast"),
    i_name = oblast_map[i_name]
  ) |>
  filter(i_name %in% master_index$i_name) |>
  left_join(
    master_index |> distinct(i_key, i_name, i)
  ) |>
  ungroup() |>
  select(-oblast)

sirens_oblast_t |>
  filter(if_any(everything(), ~ is.na(.)))
sirens_oblast_t

# write output -----------------------------------------------------------

write_csv(
  sirens_oblast_t,
  file.path(out_dir, "covariates", "interim", paste0(tolower(country), "_sirens_", output_label, ".csv"))
)


#  viz -------------------------------------------------------------------

ggplot(sirens_oblast_t, aes(x = t, y = sirens, col = i_name)) +
  geom_line() +
  theme(legend.position = "None")
