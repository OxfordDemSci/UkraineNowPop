library(jsonlite)
library(httr)
library(DBI)
library(RPostgres)

# Querying the social media audience database through SQL ----------------

# Define db_engine function
db_engine <- function(host = Sys.getenv("smaDB_host"),
                      port = Sys.getenv("smaDB_port"),
                      db = Sys.getenv("smaDB_dbname"),
                      user = Sys.getenv("smaDB_user"),
                      pw = Sys.getenv("smaDB_password")) {
  engine_string <- paste0("postgresql://", user, ":", pw, "@", host, ":", port, "/", db)
  con <- DBI::dbConnect(RPostgres::Postgres(), engine_string)
  return(con)
}

# Define query_sql function
query_sql <- function(con, sql, geom_col = NULL) {
  tryCatch(
    expr = {
      if (is.null(geom_col)) {
        result <- dbGetQuery(con, sql)
      } else {
        result <- sf::st_read(con, sql, geom_col = geom_col, crs = "EPSG:4326")
      }
    },
    error = function(e) {
      print(paste0("Error: ", e$message))
      result <- NULL
    }
  )
  return(result)
}


# Querying the social media audience database through API ----------------


# Define query_api function
query_api <- function(endpoint, args, max_attempts = 3, url = "http://18.135.72.18/api/v1/", token = env$sma_API_token) {
  args$token <- token
  
  # Submit query as GET request
  response <- httr::GET(paste0(url, endpoint), query = args)
  Sys.sleep(1)
  
  # API rate limit: sleep and retry
  attempts <- 1
  while (httr::status_code(response) == 429 & attempts <= max_attempts) {
    print(paste0("Too many API calls (attempt: ", attempts, "). Sleeping for 60 seconds before trying again..."))
    Sys.sleep(60)
    response <- httr::GET(paste0(url, endpoint), query = args)
    attempts <- attempts + 1
  }
  
  # Extract data as dataframe
  if (httr::status_code(response) != 200) {
    print(paste0("HTTP status: ", httr::status_code(response), ". Response: ", httr::content(response, "text")))
  } else {
    response <- jsonlite::fromJSON(httr::content(response, as='text', encode = 'UTF-8'))
    data <- map_if(jsonlite::fromJSON(response$data), is.data.frame, list)     |> 
      as_tibble()
    data <- unnest(data, cols = c(collection_name, collection_id, contributor_id,
      collection_date, timestamp, dau, mau, mau_lower, mau_upper, gender, age_min,
      age_max, country, geo_level, geo_key, geo_name, location_types,
      language_name, language_key))

    data$platform <- args$platform
  }

  return(data)
}

agesex_col_to_query_args <- function(col) {
  
  parms <- strsplit(col, "_")[[1]]
  
  if (grepl("Plus", parms[2])) {
    parms[2] <- gsub("Plus", "", parms[2])
    parms <- c(parms, "999")
  }
  
  agesex_key <- c("T", "M", "F")
  
  result <- list(
    gender = which(agesex_key == parms[1])-1,
    age_min = as.numeric(parms[2]),
    age_max = as.numeric(parms[3])
  )
  
  return(result)
}

retrieve_data <- function(country, date_start, date_end=Sys.Date(), geo_level, agesex, geo_key = NULL, platform = 'facebook'){
  
  results <- list()
  for (demgroup in agesex) {
    args <- list(
      country = country,
      date_start = date_start,
      date_end = date_end,
      geo_level = geo_level,
      location_types = '["recent"]',
      valid = T,
      language_name = c('all'),
      platform = platform
    )

    if(!is.null(geo_key)){
      args <- c(args, list(geo_key = geo_key))
    }
    args <- c(args, agesex_col_to_query_args(demgroup))

    result <- query_api(args, endpoint='query_clean') |> 
      as_tibble()
    if (nrow(result)>0){
      result <- result |> 
        mutate(agesex=demgroup)
      results <- bind_rows(results, result)
    }
  }
  if(!is_tibble(results)){
    print('no data found')
  } else {
    results <- results |> 
      mutate(
        collection_date = as.Date(collection_date),
        geo_key = as.integer(geo_key)) |> 
      rename(meta_key = geo_key,
            meta_name = geo_name)  |> 
      group_by(country, platform, collection_date, geo_level, meta_key, meta_name, location_types, gender, age_min, age_max, language_name) |> 
      filter(row_number(desc(dau))==1 ) |> # when several records are stored pick the highest dau
      ungroup()
  
  }

  return(results)
}
