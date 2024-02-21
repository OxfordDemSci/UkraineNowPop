requiredpackages <- c('ggplot2', 'ggthemes', 'tidyverse', 'here')

# load or install packages
install_load <- function(packages){
  for (p in packages) {
    if (p %in% rownames(installed.packages())) {
      library(p, character.only=TRUE)
    } else {
      install.packages(p)
      library(p,character.only = TRUE)
    }
  }
}

install_load(requiredpackages)
rm(requiredpackages)
rm(install_load)

# base path for script

basepath <- dirname(rstudioapi::getSourceEditorContext()$path)

# store credential locally in .env file (see ./example.env for example)
env <- new.env()
source(file.path(basepath, '.env'), local=env)

# extract data from API

#' Title
#'
#' @param args See args of the social media audience API - http://18.135.72.18/api/v1/
#' @param endpoint Name of endpoint. Currently works with `query_clean` or `data_overview`
#' @param url URL of API. Default 'http://18.135.72.18/'
#'
#' @return Tibble
#' @export
#'
#' @examples
query_API <-  function(args, endpoint, url = 'http://18.135.72.18/'){
  
  # check required params are provided for the API
  if(is.null(args$platform)){
    stop('Platform not provided')
  }
  if(is.null(args$token)){
    stop('Token not provided')
  }
  
  # Query API and parse response
  response <- request(paste0(url, 'api/v1/', endpoint)) |> 
    req_url_query(  
      !!!args)  |> 
    req_perform() |> 
    resp_body_json()
  
  print(response$message)
  
  if(response$status==206){
    print('Status is 206. The query hit the 100,000 rows limit.')
  }
  
  # Preapre names for tibble
  if(endpoint == 'query_clean'){
    cols_name = c('collection_name', 'collection_id', 'contributor_id', 'collection_date',
                  'timestamp', 'dau', 'mau', 'mau_lower', 'mau_upper', 
                  'gender', 'age_min', 'age_max', 
                  'country', 'geo_level', 'geo_key', 'geo_name', 'location_types',
             'language_name', 'language_key')
  } else if (endpoint == 'data_overview') {
    cols_name = c('collection_date', 'platform', 'country', 'location_types',
             'geo_level', 'language_name', 'nb_gender', 'nb_agegroup', 'nb_location',
             'nb_geometry')
  }
  
  # Extract data from response
  data <- map_if(jsonlite::fromJSON(response$data), is.data.frame, list) |> 
    as_tibble() |> 
    unnest(cols=all_of(cols_name))
  
  return(data)
}
