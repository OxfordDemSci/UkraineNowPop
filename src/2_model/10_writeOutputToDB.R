library(tidyverse)
library(dotenv)

load_dot_env('.env')

data_dir <- Sys.getenv('data_dir')

estimates <-  read_csv(
  file.path(
    data_dir,
    'model',
    'cleaning_model_output',
    'mod_output_pop.csv'
  )
) 
  

write_csv(estimates, './src/dashboard/api/app/data/db-data/pop.csv')
