import py_helpers
import os
from dotenv import load_dotenv
from pathlib import Path
import pandas as pd


# create directories
# Load the .env file
env_path = Path(".") / ".env"
load_dotenv(env_path)

# Access the environment variables
in_dir = Path(os.getenv("in_dir"))
out_dir = Path(os.getenv("out_dir"))
os.makedirs(out_dir, exist_ok=True)


# ---- input data ----#
country = "UA"
collection_name = "ukraine_regions"
date_start = "2022-02-26"
date_end = "2023-02-25"
gender = 0
age_min = 13
age_max = 999
geo_level = "regions"
location_type = '["recent"]'


# ---- meta key ----#
sql = (
    f"select distinct(geo_key), geo_name from facebook_clean where "
    f"country = '{country}' and "
    f"collection_name = '{collection_name}' and "
    f"collection_date >= '{date_start}' and "
    f"collection_date <= '{date_end}' and "
    f"gender = {gender} and "
    f"age_min = {age_min} and "
    f"age_max = {age_max} and "
    f"geo_level = '{geo_level}' and "
    f"location_types = '{location_type}';"
)

meta_key = pd.read_sql(sql=sql, con=py_helpers.db_engine())
meta_key.dropna(inplace=True)

outfile = os.path.join(in_dir, country.lower() + "_meta_keys.csv")
meta_key.to_csv(outfile, index=False)
