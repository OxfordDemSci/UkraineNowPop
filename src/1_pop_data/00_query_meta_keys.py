import pyidp
import os
from dotenv import load_dotenv
from pathlib import Path
import pandas as pd


# create directories
# Load the .env file
env_path = Path('.') / '.env'
load_dotenv(env_path)

# Access the environment variables
in_dir = Path(os.getenv('in_dir'))
out_dir = Path(os.getenv('out_dir'))
out_dir = os.path.join(out_dir, 'population_proxy', 'social_media_audience')
os.makedirs(out_dir, exist_ok=True)


#---- input data ----#

# agesex demographic groups
agesex = ['T_13Plus']
# \
# ['F_13Plus', 'F_18Plus', 'F_20Plus', 'F_13_19', 'F_15_49', 'F_15_64', 'F_18_34', 'F_20_29', 'F_30_39', 'F_40_49', 'F_50_59', 'F_60Plus', 'F_65Plus',
#     'M_13Plus', 'M_18Plus', 'M_20Plus', 'M_13_19', 'M_15_49', 'M_15_64', 'M_18_34', 'M_20_29', 'M_30_39', 'M_40_49', 'M_50_59', 'M_60Plus', 'M_65Plus',
#     'T_13Plus', 'T_18Plus', 'T_20Plus', 'T_13_19', 'T_15_49', 'T_15_64', 'T_18_34', 'T_20_29', 'T_30_39', 'T_40_49', 'T_50_59', 'T_60Plus', 'T_65Plus']


# end date
date_start = '2022-02-26'
date_end = '2023-02-25'

country = 'UA'


#---- meta key ----#
sql="select distinct geo_name, geo_key from facebook_clean where geo_level ='regions' and location_types = '" + '["recent"]' + "' and country = 'UA' limit 100;"

meta_key =pd.read_sql(sql=sql, con=pyidp.db_engine())

meta_key = meta_key[-meta_key['geo_name'].isna()]

outfile = os.path.join(out_dir, country.lower() + '_meta_keys.csv')
meta_key.to_csv(outfile, index=False)



