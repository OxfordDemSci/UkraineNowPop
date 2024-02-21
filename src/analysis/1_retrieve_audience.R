source(paste0(
  dirname(rstudioapi::getSourceEditorContext()$path),
  '/utils.R'
))

args <- list(
  country = 'UA',
  date_start = '2022-10-01',
  date_end = '2022-10-03',
  geo_level = 'countries',
  platform = 'facebook', # required
  token = env$db_API_token # required
)

# get individual data from the social media audience

data <- query_API(
  args, endpoint = 'query_clean'
  )

# get overview data

overview <- query_API(
  args,
  endpoint = 'data_overview'
)
