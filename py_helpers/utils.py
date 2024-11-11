import logging
import os
import json
import time
import random
from math import ceil
import requests
import sqlalchemy, psycopg2
import pandas as pd
import geopandas as gpd
from plotly.express import colors as pxcolors
from dotenv import load_dotenv
from pathlib import Path
import numpy as np

# Load the .env file
env_path = Path(".") / ".env"
load_dotenv(env_path)
in_dir = Path(os.getenv("in_dir"))
out_dir = Path(os.getenv("out_dir"))

print("in_dir and out_dir set")

# logging
logging.basicConfig(
    format="%(asctime)s [%(levelname)s] %(message)s", datefmt="%Y-%m-%d %H:%M:%S"
)
logger = logging.getLogger("pyidpLogger")
logger.setLevel(logging.INFO)


def db_engine(
    host=os.environ.get("smaDB_host"),
    port=os.environ.get("smaDB_port"),
    db=os.environ.get("smaDB_dbname"),
    user=os.environ.get("smaDB_user"),
    pw=os.environ.get("smaDB_password"),
):

    engine_string = (
        "postgresql+psycopg2://" + user + ":" + pw + "@" + host + ":" + port + "/" + db
    )

    db = sqlalchemy.create_engine(engine_string, poolclass=sqlalchemy.pool.NullPool)

    return db


def query_sql(con, sql, geom_col=None):
    try:
        if geom_col is None:
            result = pd.read_sql(sql=sql, con=con)
        else:
            result = gpd.read_postgis(
                sql=sql, con=con, geom_col=geom_col, crs="EPSG:4326"
            )
    except Exception as e:
        exc = e.__dict__
        logger.error(str(exc.get("code")) + ": " + str(exc.get("orig")))
        result = None
    return result


def query_api(
    endpoint,
    args,
    max_attempts=3,
    url="http://18.135.72.18/api/v1/",
    token=os.getenv("sma_API_token"),
):

    args["token"] = token

    # submit query as GET request
    response = requests.get(url=url + endpoint, params=args)
    time.sleep(1)

    # API rate limit: sleep and retry
    attempts = 1
    while response.status_code == 429 and attempts <= max_attempts:
        logger.warning(
            f"Too many API calls (attempt: {attempts}). Sleeping for 60 seconds before trying again..."
        )
        time.sleep(60)
        response = requests.get(url=url + endpoint, params=args)
        attempts += 1

    # extract data as pandas dataframe
    df = None
    if response.status_code != 200:
        logger.error(f"http status: {response.status_code}. Response: {response.text}")
    else:

        # format response as dictionary
        response = response.json()

        # convert to data.frame
        df = pd.DataFrame(json.loads(response.get("data")))

        # add platform
        df["platform"] = args["platform"]

        if args.get("add_geometry"):
            # geodata.frame
            gdf = gpd.GeoDataFrame.from_features(json.loads(response.get("geodata")))

            # merge geometry with other ata
            gdf = gdf.merge(df, how="right", on="geo_key")
            df = gdf.reindex(columns=list(df.columns) + ["geometry"])

    return df


def config_to_api_args(config, date_start, date_end):
    args = {}
    for idx, row in config.iterrows():
        args_item = dict(
            row[
                [
                    "collection_name",
                    "country",
                    "geo_level",
                    "gender",
                    "age_min",
                    "age_max",
                    "location_types",
                    "language_name",
                ]
            ]
        )

        args_item.update(
            {
                "date_start": date_start,
                "date_end": date_end,
                "add_geometry": False,
                "token": os.environ["sma_API_token"],
            }
        )

        args[row["demographic"]] = args_item

    return args
