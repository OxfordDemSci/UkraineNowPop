import pandas as pd
import geopandas as gpd

import os
import sys
from dotenv import load_dotenv
from pathlib import Path
import numpy as np

# load the .env file
env_path = Path(".") / ".env"
load_dotenv(env_path)

# working directory and system path
os.chdir(os.getenv("repo_dir"))
sys.path.append(os.path.join(os.getenv("repo_dir"), "src", "helpers"))

# import local helper module
import py_helpers.covariates as prep



# Load the .env file
load_dotenv()
out_dir = Path(os.getenv("out_dir"))
repo_dir = Path(os.getenv("repo_dir"))
country = "ua"

raw_dir = out_dir / "covariates" / "raw" / "acled"
interim_dir = out_dir / "covariates" / "interim"

os.makedirs(raw_dir, exist_ok=True)
os.makedirs(interim_dir, exist_ok=True)

# data
boundaries_oblast = gpd.read_file(
    repo_dir
    / "data"
    / "cod-ab"
    / "ukr_admbnda_sspe_20230201_SHP"
    / "ukr_admbnda_adm1_sspe_20230201.shp"
)
master_index = pd.read_csv(out_dir / (country + "_master_index.csv"))
master_index = master_index[
    ["ADM1_PCODE", "i", "i_key", "i_name", "t", "t_key", "t_name"]
].drop_duplicates()

time_index = pd.read_csv(out_dir / (country + "_time_index.csv"))

# credentials
password = os.getenv("ACLED_PASSWORD")
email = os.getenv("ACLED_EMAIL")
country_name = "Ukraine"
start_date = min(time_index["collection_date"])
end_date = max(time_index["collection_date"])
crs = 4326

# get acled data
incident_gdf = prep.get_acled_data_from_api(
    password=password,
    email=email,
    country=country_name,
    start_date=start_date,
    end_date=end_date,
    crs=crs
)

# save acled data to disk
incident_gdf.to_csv(raw_dir / (country + "_acled.csv"), index=False)
incident_gdf.to_file(
    filename=raw_dir / (country + "_acled.gpkg"),
    driver="GPKG",
    append=False,
)

# aggregate events
acled = gpd.read_file(raw_dir / (country + "_acled.gpkg")).rename(
    columns={"event_date": "collection_date"}
)

acled = gpd.sjoin(
    left_df=acled,
    right_df=boundaries_oblast[["ADM1_PCODE", "geometry"]],
    how="inner",
    predicate="within",
)

# summarise events by type
acled_df = pd.DataFrame(acled)

acled_df = acled_df[
    acled["collection_date"] <= max(time_index["collection_date"])
].join(time_index.set_index("collection_date"), on="collection_date", how="left")


conditions = {
    "acled_withfatalities": acled_df["fatalities"] != "0",
    "acled_territoryUkraine": acled_df["sub_event_type"] == "Government regains territory",
    "acled_territoryRussia": acled_df["sub_event_type"] == "Non-state actor overtakes territory",
    "acled_eventExplosion": (acled_df["event_type"] == "Explosions/Remote violence") | (acled_df["sub_event_type"] == "Armed clash")
}

# Calculate counts for each condition
acled_oblast = (
    acled_df.groupby(["ADM1_PCODE", "t"])["event_id_cnty"]
    .count()
    .reset_index(name="acled_all")
)

for key, condition in conditions.items():
    filtered_acled = acled_df[condition]
    acled_oblast = acled_oblast.merge(
        filtered_acled.groupby(["ADM1_PCODE", "t"])["event_id_cnty"]
        .count()
        .reset_index(name=key),
        how="left",
        on=["ADM1_PCODE", "t"],
    )

acled_oblast = (
    acled_oblast[acled_oblast["ADM1_PCODE"].isin(master_index["ADM1_PCODE"])]
    .merge(
        master_index.set_index(["ADM1_PCODE", "t"]),
        how="right",
        on=["ADM1_PCODE", "t"],
    )
    .fillna(0)
)

acled_oblast = acled_oblast.melt(
    id_vars=["ADM1_PCODE", "t", "i", "i_key", "i_name", "t_key", "t_name"],
    var_name="covariate",
    value_name="value",
)

acled_oblast.to_csv(interim_dir / (country + "_acled_oblast.csv"), index=False)
