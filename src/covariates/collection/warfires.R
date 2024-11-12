# Load necessary libraries
library(git2r)
library(tidyverse)
library(sf)

# Load environment variables from .env file
env <- new.env()
source(here::here(".env"), local = env)
out_dir <- env$out_dir
in_dir <- env$in_dir

country <- "UA"

# Clone repository
repo_url <- "https://github.com/TheEconomist/the-economist-war-fire-model.git"
repo_dir <- file.path(out_dir, "covariates", "raw", "the-economist-war-fire-model")

if (!dir.exists(repo_dir)) {
  git2r::clone(repo_url, repo_dir)
}

git2r::pull(repo_dir)

# load data --------------------------------------------------------------
time_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_time_index.csv")))
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index.csv")))
master_index <- master_index |>
  distinct(i_key, i_name, i, t, t_name, t_key, ADM1_PCODE, ADM1_EN)
admin <- st_read(file.path(in_dir, "COD-AB", "ukr_admbnda_sspe_20230201_SHP/ukr_admbnda_adm1_sspe_20230201.shp"))


fires <- read_csv(file.path(repo_dir, "output-data", "ukraine_war_fires.csv"))
fires_sf <- st_as_sf(fires, coords = c("x", "y"), crs = "epsg:4326")



# pre-process fires ------------------------------------------------------


# add adm1 name
fires_sf
fires_sf <- st_intersection(fires_sf, admin)

fires_oblast <- fires_sf |>
  st_drop_geometry() |>
  rename(collection_date = date) |>
  group_by(ADM1_EN, ADM1_PCODE, collection_date) |>
  summarise(war_fires = n())

fires_oblast_t <- fires_sf |>
  st_drop_geometry() |>
  rename(collection_date = date) |>
  left_join(time_index) |>
  filter(!is.na(t)) |>
  filter(ADM1_PCODE %in% master_index$ADM1_PCODE) |>
  group_by(t, ADM1_EN, ADM1_PCODE) |>
  summarise(war_fires = n()) |>
  left_join(
    master_index
  )




# write output -----------------------------------------------------------

write_csv(
  fires_oblast_t,
  file.path(out_dir, "covariates", "interim", paste0(tolower(country), "_warfires_oblast.csv"))
)



# viz --------------------------------------------------------------------

ggplot(fires_oblast_t, aes(x = t, y = war_fires, col = ADM1_EN)) +
  geom_line() +
  theme(legend.position = "none")
