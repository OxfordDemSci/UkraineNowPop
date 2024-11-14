import pandas as pd
import geopandas as gpd
import net_friction.data_preparation as prep
import os
from dotenv import load_dotenv
from pathlib import Path
import numpy as np

# Load the .env file
env_path = Path(".") / ".env"
load_dotenv(env_path)
out_dir = Path(os.getenv("out_dir"))
repo_dir = Path(os.getenv("repo_dir"))
country = "ua"

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
key = os.getenv("ACLED_KEY")
email = os.getenv("ACLED_EMAIL")
country_name = "Ukraine"
start_date = min(time_index["collection_date"])
end_date = max(time_index["collection_date"])
crs = 4326
accept_acled_terms = True

incident_gdf = prep.get_acled_data_from_api(
    key, email, country_name, start_date, end_date, crs, accept_acled_terms
)

acled_path = Path(out_dir) / "covariates" / "raw" / (country + "_acled.csv")
incident_gdf.to_csv(acled_path, index=False)
incident_gdf.to_file(
    Path(out_dir) / "covariates" / "raw" / (country + "_acled.gpkg"),
    driver="GPKG",
    append=False,
)

# Aggregate events

acled = gpd.read_file(
    Path(out_dir) / "covariates" / "raw" / (country + "_acled.gpkg")
).rename(columns={"event_date": "collection_date"})

acled = gpd.sjoin(
    acled,
    boundaries_oblast[["ADM1_PCODE", "geometry"]],
    how="inner",
    predicate="within",
)

acled_df = pd.DataFrame(acled)

acled_df = acled_df[
    acled["collection_date"] <= max(time_index["collection_date"])
].join(time_index.set_index("collection_date"), on="collection_date", how="left")

conditions = {
    "acled_withfatalities": acled_df["fatalities"] != "0",
    "acled_disorderPolitical": acled_df["disorder_type"] == "Political violence",
    "acled_disorderStrategic": acled_df["disorder_type"] == "Strategic developments",
    "acled_disorderDemonstration": acled_df["disorder_type"] == "Demonstrations",
    "acled_disorderPoliticalDemonstration": acled_df["disorder_type"]
    == "Political violence; Demonstrations",
    "acled_eventExplosion": acled_df["event_type"] == "Explosions/Remote violence",
    "acled_eventBattle": acled_df["event_type"] == "Battles",
    "acled_eventStrategic": acled_df["event_type"] == "Strategic developments",
    "acled_eventAgainstCivilians": acled_df["event_type"]
    == "Violence against civilians",
    "acled_eventProtest": acled_df["event_type"] == "Protests",
    "acled_eventRiot": acled_df["event_type"] == "Riots",
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

acled_oblast = acled_oblast[
    acled_oblast["ADM1_PCODE"].isin(master_index["ADM1_PCODE"])
].merge(
    master_index.set_index(["ADM1_PCODE", "t"]), how="right", on=["ADM1_PCODE", "t"]
)


acled_oblast.to_csv(
    out_dir / "covariates" / "interim" / (country + "_acled_oblast.csv"), index=False
)


# Compute Incidents in Routes
def vary_buffer_in_subset(buffer_distance_in_meters):
    # Define the output filenames
    incidents_outfile_csv = (
        out_dir
        / "covariates"
        / "interim"
        / f"acled_edge_oblast_{buffer_distance_in_meters}.csv"
    )
    incidents_outfile_gpkg = (
        out_dir
        / "covariates"
        / "interim"
        / f"acled_edge_oblast_{buffer_distance_in_meters}.gpkg"
    )

    # Read and process the data
    incident_subset_gdf = prep.subset_incident_data_in_buffer(
        gpd.read_file(out_dir / "covariates" / "network_edges_oblast.gpkg"),
        Path(out_dir) / "covariates" / (country + "_acled_oblast.csv"),
        incidents_outfile_csv,
        buffer_distance_in_meters,
        6383,
        is_acled=True,
        index_col="event_id_cnty",  # Unique ID field in incidents table
    )

    # Save the results
    incident_subset_gdf.to_file(incidents_outfile_gpkg, driver="GPKG")


for buffer in [1000, 5000, 10000]:
    vary_buffer_in_subset(buffer)
