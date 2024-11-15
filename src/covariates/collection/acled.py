import pandas as pd
import geopandas as gpd

import os
from dotenv import load_dotenv
from pathlib import Path
import numpy as np

import net_friction.data_preparation as prep


def vary_buffer_in_subset(buffer_distance_in_meters, edges, incident_data, out_dir):
    """Compute Incidents in Routes"""

    # Define the output filenames
    incidents_outfile_csv = (
        out_dir / f"acled_edge_oblast_{buffer_distance_in_meters}.csv"
    )
    incidents_outfile_gpkg = (
        out_dir / f"acled_edge_oblast_{buffer_distance_in_meters}.gpkg"
    )

    # Read and process the data
    incident_subset_gdf = prep.subset_incident_data_in_buffer(
        edges=edges,
        incident_data=incident_data,
        incident_out_file=incidents_outfile_csv,
        buffer_distance=buffer_distance_in_meters,
        crs=6383,
        is_acled=True,
        index_col="event_id_cnty",  # Unique ID field in incidents table
    )

    # Save the results
    incident_subset_gdf.to_file(incidents_outfile_gpkg, driver="GPKG")


if __name__ == "__main__":

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
    key = os.getenv("ACLED_KEY")
    email = os.getenv("ACLED_EMAIL")
    country_name = "Ukraine"
    start_date = min(time_index["collection_date"])
    end_date = max(time_index["collection_date"])
    crs = 4326
    accept_acled_terms = True

    # get acled data
    incident_gdf = prep.get_acled_data_from_api(
        api_key=key,
        email=email,
        country=country_name,
        start_date=start_date,
        end_date=end_date,
        crs=crs,
        accept_acleddata_terms=accept_acled_terms,
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
        "acled_disorderPolitical": acled_df["disorder_type"] == "Political violence",
        "acled_disorderStrategic": acled_df["disorder_type"]
        == "Strategic developments",
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

    acled_oblast.to_csv(interim_dir / (country + "_acled_oblast.csv"), index=False)

    for buffer in [1000, 5000, 10000]:
        vary_buffer_in_subset(
            buffer_distance_in_meters=buffer,
            edges=out_dir / "covariates" / "network_edges_oblast.gpkg",
            incident_data=interim_dir / (country + "_acled_oblast.csv"),
            out_dir=out_dir,
        )
