# Load necessary libraries
library(git2r)
library(tidyverse)
library(fs)

# Load environment variables from .env file
env <- new.env()
source(here::here('.env'), local=env)
out_dir <- env$out_dir

country <- 'UA'

# Clone repository
repo_url <- "https://github.com/Vadimkin/ukrainian-air-raid-sirens-dataset.git"
repo_dir <- file.path(out_dir, "covariates", "raw", "ukrainian-air-raid-sirens-dataset")
git2r::clone(repo_url, repo_dir)

# Load data
time_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_time_index.csv")))
sirens_official <- read_csv(file.path(repo_dir, "datasets", "official_data_en.csv"))
sirens_volunteered <- read_csv(file.path(repo_dir, "datasets", "volunteer_data_en.csv")) |> 
  rename(oblast = region)

# Concatenate data
sirens <- bind_rows(sirens_official, sirens_volunteered)

sirens <- sirens |> 
  mutate(
    date_start = as.Date(started_at),
    date_end = as.Date(finished_at),
    diff = as.integer(date_end - date_start) 
  )


sirens <- sirens |> 
  select(oblast, date_start, date_end, diff) |>
  rowwise() |>
  mutate(
    collection_date = list(seq(date_start,date_end,1))
  ) |> 
  select(oblast, collection_date) |> 
  unnest_longer(col = collection_date)

sirens_oblast_t <- sirens |>
  mutate(
    sirens = 1
  ) |> 
  left_join(
    time_index |> select(t, t_name), 
    by = c("collection_date" = "t_name")
  ) |> 
  group_by(oblast, t) |>
  summarise(sirens = sum(sirens)) 



# write output -----------------------------------------------------------

write_csv(sirens_oblast_t, 
  file.path(out_dir, "covariates","interim" ,paste0(tolower(country), "_sirens.csv")))


#  viz -------------------------------------------------------------------

ggplot(sirens_oblast_t, aes(x = t, y = sirens, col=oblast)) +
  geom_line()
