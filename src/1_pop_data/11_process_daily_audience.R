# Convert the Facebook and Instagram daily audience data from a long format to a 3D array [week_num, geo_name, day_of_week]
# suitable for use with the N-mixture model.

# Libraries
library(tidyverse)

# Working directory
env <- new.env()
source(here::here('.env'), local=env)

#' Convert the daily audience data from a long format to a 3D array [week_num, geo_name, day_of_week]
#'
#' @param social_media a data frame containing the audience data.
#' @param metric the name of the column containing the audience values ('dau', 'mau_lower', 'mau_upper')
#' @export

convert_audience_to_md <- function(social_media, metric) {

  social_media <- social_media |>
    rename(audience=metric) |> 
    select(collection_date, audience, agesex, platform, geo_name)

  # compute the indices: day of week and week_num
  social_media$collection_date <- as.Date(social_media$collection_date)
  social_media$day_of_week <- format(social_media$collection_date, "%u")
  social_media$week <- paste0(as.numeric(format(social_media$collection_date, "%y")) ,as.numeric(format(social_media$collection_date, "%W")))
  social_media$week_num <- as.numeric((as_factor(social_media$week)))

  # fill with NAs the missing combinations of geo_name, day_of_week and week_num
  social_media <- social_media |>
    complete(geo_name, day_of_week, week_num)

  # reshape the audience data to a 3D array [week_num, geo_name, day_of_week]
  social_media_reshape <- social_media |> 
    select(geo_name, audience, day_of_week, week_num) |> 
    complete(geo_name, day_of_week, week_num)|> 
    pivot_wider(names_from = geo_name, values_from = audience) |> 
    arrange(week_num, day_of_week) |>
    select(-week_num) |> 
    group_split(day_of_week, .keep=F)

  social_media_reshape_array <- lapply(social_media_reshape, as.matrix)
  social_media_reshape_array <- array(unlist(social_media_reshape_array), 
                                      dim=c(n_distinct(social_media$week_num), n_distinct(social_media$geo_name), n_distinct(social_media$day_of_week)))
  
  # Compute the number of days per week per geo_name that have no audience 
  # and concatenate the corresponding day of the week indices

  n_obs_per_week <- social_media |> 
    filter(!is.na(audience)) |>
    group_by(week_num, geo_name) |> 
    summarise(
      n_obs = n_distinct(day_of_week),
      day_of_week_obs = paste(day_of_week, collapse = ",")) 

  saveRDS(social_media_reshape_array, paste0(env$out_dir,'population_proxy/', 'model_data/', platform, '_md.rds'))
  saveRDS(n_obs_per_week, paste0(env$out_dir,'population_proxy/', 'model_data/', platform, '_md_missing.rds'))

  return(list('md'=social_media_reshape_array, 'md_missing'=n_obs_per_week))
}


# Convert the audience data for each platform to a 3D array (md)
for (platform in c('facebook', 'instagram')) {
  platform_audience <- read_csv(paste0(env$out_dir,'population_proxy/', 'social_media_audience/', 'ua_', platform, '_audience.csv')) 
  out <- convert_audience_to_md(social_media=platform_audience, metric='dau')
}



