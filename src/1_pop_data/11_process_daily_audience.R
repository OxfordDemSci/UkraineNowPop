# Convert the Facebook and Instagram daily audience data from a long format to a 3D array  [week, oblast, day_of_week]
# suitable for use with the N-mixture model.

# Libraries
library(tidyverse)

# Working directory
env <- new.env()
source(here::here('.env'), local=env)

# directories
dir.create(file.path(env$out_dir,'population_proxy', 'social_media_audience'), showWarnings=F, recursive=T)
dir.create(file.path(env$out_dir,'population_proxy', 'model_data'), showWarnings=F, recursive=T)

#' Convert the daily audience data from a long format to a 3D array  [week, oblast, day_of_week]
#'
#' @param social_media a data frame containing the audience data.
#' @param metric the name of the column containing the audience values ('dau', 'mau_lower', 'mau_upper')
#' @export

convert_audience_to_md <- function(social_media, metric) {

  social_media <- social_media |>
    rename(audience=metric) |> 
    select(collection_date, audience, agesex, platform, meta_key)

  # compute the indices: day of week and week_num
  social_media$collection_date <- as.Date(social_media$collection_date)
  social_media$day_of_week <- format(social_media$collection_date, "%u")
  social_media$week <- paste0(as.numeric(format(social_media$collection_date, "%y")) ,as.numeric(format(social_media$collection_date, "%W")))
  social_media$week_num <- as.numeric((as_factor(social_media$week)))

  # fill with NAs the missing combinations of geo_name, day_of_week and week_num
  social_media <- social_media |>
    complete(meta_key, day_of_week, week_num)

  # reshape the audience data to a 3D array [week_num, geo_name, day_of_week]
  social_media_reshape <- social_media |> 
    select(meta_key, audience, day_of_week, week_num) |> 
    complete(meta_key, day_of_week, week_num)|> 
    pivot_wider(names_from = meta_key, values_from = audience) |> 
    arrange(week_num, day_of_week) |>
    select(-week_num) 
  
  social_media_reshape_array <- social_media_reshape |> 
    group_split(day_of_week, .keep=F)

  social_media_reshape_array <- lapply(social_media_reshape_array, as.matrix)
  social_media_reshape_array <- array(unlist(social_media_reshape_array), 
                                      dim=c(n_distinct(social_media$week_num), n_distinct(social_media$meta_key), n_distinct(social_media$day_of_week)))

  # Compute the number of days per week per geo_name that have no audience 
  # and concatenate the corresponding day of the week indices

  n_obs_per_week <- social_media |> 
    filter(!is.na(audience)) |>
    group_by(week_num, meta_key) |> 
    summarise(
      n_obs = n_distinct(day_of_week),
      day_of_week_obs = paste(day_of_week, collapse = ",")) |> 
    ungroup() |>
    complete(week_num, meta_key, fill = list(n_obs = 0, day_of_week_obs = NA))
  
  n_obs_per_week_matrix <- n_obs_per_week |> 
    ungroup() |>
    select(week_num, meta_key, n_obs) |> 
    pivot_wider(names_from = meta_key, values_from = n_obs) |> 
    arrange(week_num) |> 
    select(-week_num) |> 
    as.matrix()
    
    md <- list(
      y = social_media_reshape_array,
      T = n_distinct(social_media$week_num),
      I = n_distinct(social_media$meta_key),
      M = n_obs_per_week_matrix,
      locations = as.integer(colnames(social_media_reshape)[-1])
    )

  return(list('md'=md))
}


# Convert the audience data for each platform to a 3D array (md)
out <- list()
for (platform in c('facebook', 'instagram')) {
  # platform = 'facebook'
  platform_audience <- read_csv(paste0(env$out_dir,'population_proxy/', 'social_media_audience/', 'ua_', platform, '_audience.csv')) 
  out[platform] <- convert_audience_to_md(social_media=platform_audience, metric='dau')
}


elements <- names(out$facebook)
md <- list()
for (platform in c('facebook', 'instagram')) {
  # platform = 'facebook'
  platform_abb <- ifelse(platform == 'facebook', 'F', 'G')
  for (element in elements) {
    # element = 'y'
    md[[paste0(element, '_', platform_abb)]] <- out[[platform]][[element]]
  }
}

write_rds(md, paste0(env$out_dir, 'population_proxy/', 'model_data/', 'md.rds'))
