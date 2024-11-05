# Load necessary libraries
library(git2r)
library(tidyverse)

# Load environment variables from .env file
env <- new.env()
source(here::here('.env'), local=env)
out_dir <- env$out_dir

country <- 'UA'

# Clone repository
repo_url = "https://github.com/TheEconomist/the-economist-war-fire-model.git"
repo_dir <- file.path(out_dir, "covariates", "raw", "the-economist-war-fire-model")

git2r::clone(repo_url, repo_dir)


# load data --------------------------------------------------------------
time_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_time_index.csv")))

fires <- read_csv(file.path(repo_dir, "output_data", "war_fires_by_ADM3.csv"))

fires <- fires |> 
  group_by(ADM1_EN, ADM1_PCODE, date) |> 
  summarise(war_fires = n())

fires_oblast_t <- fires |>
  left_join(time_index |> select(t, t_name), by = c("date" = "t_name")) |> 
  group_by(t, ADM1_EN, ADM1_PCODE) |>
  summarise(war_fires = sum(war_fires))


# write output -----------------------------------------------------------

write_csv(fires_oblast_t, 
  file.path(out_dir, "covariates","interim" ,paste0(tolower(country), "_warfires.csv")))



# viz --------------------------------------------------------------------

ggplot(fires_oblast_t, aes(x = t, y = war_fires, col=ADM1_EN)) +
  geom_line()+
  theme(legend.position = "none")
