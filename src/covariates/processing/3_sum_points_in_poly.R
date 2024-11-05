# Load required libraries
library(sf)
library(tidyverse)

# Load setup script

# Load boundaries
boundaries_oblast <- st_read(paste0(in_dir, "/COD-AB/ukr_admbnda_sspe_20230201_SHP/ukr_admbnda_adm1_sspe_20230201.shp")) %>%
  rename(pcode = ADM1_PCODE)

boundaries_raion <- st_read(paste0(in_dir, "/COD-AB/ukr_admbnda_sspe_20230201_SHP/ukr_admbnda_adm2_sspe_20230201.shp")) %>%
  rename(pcode = ADM1_PCODE)

# Define count_points_in_polygons function
count_points_in_polygons <- function(points, polygons, varname = "count", start_date = "2022-02-26", end_date = "2024-05-15") {
  # Perform spatial join to count points in each polygon
  points <- st_transform(points, st_crs(polygons))
  joined <- st_intersection(points, polygons[, c("pcode", "geometry")])
  
  # Count points in each polygon
  point_counts <- joined %>% 
    group_by(pcode, time) %>% 
    summarise(count = n())
  
  # Generate all combinations of pcode and time
  all_pcodes <- unique(polygons$pcode)
  all_dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  all_combinations <- expand.grid(pcode = all_pcodes, time = all_dates)
  
  # Reindex point_counts to include all combinations and fill missing with 0
  point_counts <- point_counts %>% 
    left_join(all_combinations, by = c("pcode", "time")) %>% 
    mutate(count = ifelse(is.na(count), 0, count))
  
  return(point_counts)
}

# Process ACLED
acled <- st_read(paste0(out_dir, "/covariates/raw/acled.gpkg")) %>%
  rename(time = event_date)

acled_oblast <- count_points_in_polygons(acled, boundaries_oblast, "acled_all")

# Define filtering conditions
conditions <- list(
  acled_withfatalities = acled$fatalities != "0",
  acled_disorderPolitical = acled$disorder_type == "Political violence",
  acled_disorderStrategic = acled$disorder_type == "Strategic developments",
  acled_disorderDemonstration = acled$disorder_type == "Demonstrations",
  acled_disorderPoliticalDemonstration = acled$disorder_type == "Political violence; Demonstrations",
  acled_eventExplosion = acled$event_type == "Explosions/Remote violence",
  acled_eventBattle = acled$event_type == "Battles",
  acled_eventStrategic = acled$event_type == "Strategic developments",
  acled_eventAgainstCivilians = acled$event_type == "Violence against civilians",
  acled_eventProtest = acled$event_type == "Protests",
  acled_eventRiot = acled$event_type == "Riots"
)

# Calculate counts for each condition
for (key in names(conditions)) {
  filtered_acled <- acled[conditions[[key]], ]
  acled_oblast[[key]] <- count_points_in_polygons(filtered_acled, boundaries_oblast)$count
}

# Save acled_oblast to CSV
write.csv(acled_oblast, paste0(out_dir, "/covariates/interim/acled_oblast.csv"), row.names = FALSE)

# Process Ecology
ecology <- st_read(paste0(out_dir, "/covariates/raw/conflictEcology_point.gpkg")) %>%
  rename(time = event_date)

ecology_oblast <- count_points_in_polygons(ecology, boundaries_oblast, "conflictEcology")

# Save ecology_oblast to CSV
write.csv(ecology_oblast, paste0(out_dir, "/covariates/interim/conflictEcology_oblast.csv"), row.names = FALSE)

# Combine all summaries in one output
covariates_file_paths <- c(
  paste0(out_dir, "/covariates/interim/acled_oblast.csv"),
  paste0(out_dir, "/covariates/interim/conflictEcology_oblast.csv")
)

covariates_list <- lapply(covariates_file_paths, function(file) {
  read_csv(file) %>% 
    pivot_longer(cols = -c(time, pcode), names_to = "covariate", values_to = "value")
})

covariates <- bind_rows(covariates_list)

write_csv(covariates, file.path(out_dir, 'covariates', 'final', 'covariates_locations_oblast.csv',
                  index=False))