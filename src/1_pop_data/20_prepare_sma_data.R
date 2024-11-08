# cleanup
rm(list = ls())
gc()

# Load required helpers
source("R_helpers/generic.R")
source("R_helpers/data_querying.R")

# Set up output directory
out_dir_pop <- file.path(out_dir, "population_proxy", "social_media_audience")
dir.create(out_dir_pop, recursive = TRUE, showWarnings = FALSE)

output_label <- ""

audience_metric <- "dau"

# Load master index
country <- "UA"
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index", output_label, ".csv")),
  col_types = c(s_name = "c")
)
time_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_time_index", output_label, ".csv")))

# Retrieve parameters
date_start <- min(master_index$t_name)
date_end <- max(master_index$t_name) + 6
agesex <- unique(paste(master_index$s_name, master_index$a_name, sep = "_"))
meta_keys <- unique(master_index$i_key)

# Get Facebook and Instagram social media audience data
retrieve_sma_data <- function(platform) {
  lapply(meta_keys, function(meta_key_) {
    retrieve_data(
      country = country,
      date_start = date_start,
      date_end = date_end,
      geo_level = "regions",
      agesex = agesex,
      geo_key = meta_key_,
      platform = platform
    )
  }) |>
    bind_rows()
}

sma_facebook <- retrieve_sma_data("facebook")
sma_instagram <- retrieve_sma_data("instagram")

# Process social media audience data

master_index_without_t <- master_index |>
  rowwise() |>
  mutate(
    agesex = paste(s_name, a_name, sep = "_")
  )

process_sma_data <- function(sma_data, metric = audience_metric) {
  sma_data_ <- sma_data |>
    rename(value = all_of(metric), i_key = meta_key) |>
    left_join(time_index) |>
    left_join(
      master_index_without_t
    ) |>
    arrange(t_name) |>
    mutate(m = format(t_name, "%u")) |>
    select(t_name, t, m, i, a, s, value)

  return(sma_data_)
}

sma_facebook_processed <- process_sma_data(sma_facebook)
sma_instagram_processed <- process_sma_data(sma_instagram)

# Write processed data to CSV files
write_csv(sma_facebook_processed, file.path(out_dir_pop, paste0(tolower(country), "_facebook_audience", output_label, ".csv")))
write_csv(sma_instagram_processed, file.path(out_dir_pop, paste0(tolower(country), "_instagram_audience", output_label, ".csv")))
