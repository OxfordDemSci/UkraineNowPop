# This script queries Meta's regional identifiers from Oxford's social media database

import os
import sys
from dotenv import load_dotenv
from pathlib import Path
import pandas as pd

# load the .env file
env_path = Path(".") / ".env"
load_dotenv(env_path)

# working directory and system path
os.chdir(os.getenv("repo_dir"))
sys.path.append(os.path.join(os.getenv("repo_dir"), "src", "helpers"))

# import local helper module
import py_helpers

# Access the environment variables
data_dir = os.path.join(Path(os.getenv("repo_dir")), "data")
os.makedirs(os.path.join(data_dir, "meta"), exist_ok=True)

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

outfile = os.path.join(data_dir, "meta", country.lower() + "_meta_keys.csv")
meta_key.to_csv(outfile, index=False)
