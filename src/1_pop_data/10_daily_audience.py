import pyidp
import os
from datetime import datetime
from datetime import timedelta
from dotenv import load_dotenv
from pathlib import Path


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
agesex = ['T_20Plus']
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

#---- daily audience ----#
for platform in ['facebook', 'instagram']:
    # platform = 'facebook'

    outfile = os.path.join(out_dir, country.lower() + '_'+platform+'_audience.csv')

    # daily audience
    audience = pyidp.daily_audience(
            date_start=date_start,
            date_end= date_end,
            platform=platform,
            country=country,
            geo_level='regions',
            geo_keys=meta_key['geo_key'].unique().tolist(),
            agesex= agesex,
            location_types='["recent"]',
            language_name='all'
        )

    audience = audience.rename(columns={'geo_key': 'meta_key'})


    # remove duplicate rows (if duplicates, keep row with greatest mau_lower)
    audience = (audience.
                sort_values('mau_lower', ascending=False).
                drop_duplicates(subset=['meta_key', 'collection_date', 'agesex'], keep='first'))

    # sort
    audience = audience.sort_values(['collection_date', 'meta_key', 'agesex'])

    # save
    audience.to_csv(outfile, index=False)



