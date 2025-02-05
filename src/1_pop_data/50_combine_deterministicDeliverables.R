library("tidyverse")
library("readr")

source("R_helpers/generic.R")
master_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_master_index.csv")))|>
  distinct(i_key, i_name, i, ADM1_PCODE, macroregion, t, t_key, t_name)
time_index <- read_csv(file.path(out_dir, paste0(tolower(country), "_time_index.csv")))
# parameters
parent_dir <- file.path(in_dir, "/Ukraine_population_estimates_2023/oblast_daily_population/model")
all_subdirs <- list.dirs(path = parent_dir, full.names = TRUE, recursive = FALSE)
target_filename <- "population_current.csv"

# extract daily estimates
population_data_list <- list()

for (subdir in all_subdirs) {
  
  file_path <- file.path(subdir, target_filename)
  if (file.exists(file_path)) {
    
    data <- tryCatch(
      {
        read_csv(file_path) 
      },
      error = function(e) {
        message(paste("Error reading file:", file_path))
        message("Error message:", e$message)
        return(NULL)
      }
    )
    
    if (!is.null(data)) {
      # Adding a column indicating the source folder/date
      data <- data %>%
        mutate(Source_Folder = basename(subdir))
      
      population_data_list[[basename(subdir)]] <- data
    }
    
  } else {
    message(paste("File not found:", file_path))
  }
}


# Combine all data frames into one
combined_population<- bind_rows(population_data_list, .id = "Source_Folder") %>%
  rename(collection_date = Source_Folder) %>%
  mutate(collection_date = as.Date(collection_date)) |> 
  select(ADM1_PCODE, collection_date, starts_with('m'),total,  starts_with('f')) |>
  left_join(time_index) |> 
  group_by(ADM1_PCODE, t) |> 
  summarise(across(c(starts_with('m'), starts_with('f'), total), ~mean(.x, na.rm = TRUE))) |> 
  left_join(master_index) 



# Write the combined data to a file
out_folder <- file.path(out_dir, "population_proxy", "deterministic_estimates")
dir.create(out_folder, showWarnings = FALSE)
write_csv(combined_population, file.path(out_folder, "ua_deterministic_estimates.csv"))



