from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv
import geopandas as gpd
import pandas as pd
import os

from areasofcontrol.get_data import get_history, filter_ids, get_geojson, filter_and_save
from net_friction.table_production import process_data
from net_friction.data_preparation import data_pre_processing, get_acled_data_from_api, subset_acled_data_in_buffer


load_dotenv()

BASE = Path(__file__).resolve().parent.parent.parent.joinpath("data", "covariates", "network_incidents")


def main(roads, subset_fields, subset_categories, crs, admin_levels, buffer_distance, start_date, end_date):
    areas_of_control_dir = BASE.joinpath("areas_of_control")
    if not areas_of_control_dir.exists():
        areas_of_control_dir.mkdir(parents=True, exist_ok=True)
    get_control_areas(areas_of_control_dir, start_date, end_date)
    raster = BASE.joinpath("ukr_ppp_2020_1km_Aggregated.tif")
    admin_boundaries = BASE.joinpath("GEODATA.gpkg")
    acled_data = BASE.joinpath(f"{start_date}-{end_date}-ACLED-UKRAINE.csv")
    if not acled_data.exists():
        acled_key = os.getenv("ACLED_KEY")
        acled_email = os.getenv("ACLED_EMAIL")
        acled_gdf = get_acled_data_from_api(acled_key,
                                            acled_email,
                                            "Ukraine",
                                            start_date,
                                            end_date,
                                            crs,
                                            accept_acleddata_terms=True,
                                            outfile=acled_data.parent.joinpath(f"{acled_data.stem}.gpkg"))
        pd.DataFrame(acled_gdf[[x for x in acled_gdf.columns if x != "geometry"]]).to_csv(acled_data, index=False)  
    for admin_level in admin_levels:
        BASE_OUT = BASE.joinpath(f"L{admin_level}", f"{start_date}-{end_date}")
        if not BASE_OUT.exists():
            BASE_OUT.mkdir(parents=True, exist_ok=True)
        edges = BASE.joinpath(f"L{admin_level}/edges_{admin_level}.gpkg")        
        centroids_file = BASE.joinpath(f"L{admin_level}/centroids_L{admin_level}.gpkg")
        acled_subset = BASE_OUT.joinpath("acled_subset.csv")
        distance_matrix = BASE_OUT.joinpath("distances.csv")
        incidents_in_routes = BASE_OUT.joinpath("incidents_in_routes.csv")
        incidents_in_routes_aggregated = BASE_OUT.joinpath("incidents_in_routes_aggregated.csv")
        areas_of_control_matrix = BASE_OUT.joinpath("areas_of_control.csv")
        if not edges.exists() or not centroids_file.exists():
            data_pre_processing(
                roads_data=roads,
                crs=crs,
                raster=raster,
                admin_boundaries=admin_boundaries,
                admin_level=admin_level,
                centroids_file=centroids_file,
                edges_file=edges,
                subset_fields=subset_fields,
                subset_categories=subset_categories
            )
        if not acled_subset.exists():
            edges_df = gpd.read_file(edges)
            subset_acled_data_in_buffer(edges_df, acled_data, acled_subset, buffer_distance, crs)
        process_data(
            roads_data=edges,
            crs=crs,
            raster=raster,
            admin_boundaries=admin_boundaries,
            control_areas_dir=control_areas_dir,
            aceld_data=acled_subset,
            date_start=start_date,
            date_end=end_date,
            distance_matrix=distance_matrix,
            incidents_in_routes_outfile=incidents_in_routes,
            incidents_in_routes_aggregated=incidents_in_routes_aggregated,
            areas_of_control_matrix=areas_of_control_matrix,
            admin_level=admin_level,
            buffer_distance=buffer_distance,
            centroids_file=centroids_file,
            roads_layer=None,
            fix_road_topology=False,
            subset_fields=None,
            subset_categories=None
        )


def get_control_areas(control_areas_dir, start_date, end_date):
    def get_geojson_data(id):
        geojson = get_geojson(id)
        if geojson is not None:
            filter_and_save(geojson, id, control_areas_dir)
        return id
    ids = get_history()
    filtered_timestamps = filter_ids(ids, start_date, end_date)
    with ThreadPoolExecutor(max_workers=15) as executor:
        futures = {executor.submit(get_geojson_data, id): id for id in filtered_timestamps}
        for future in as_completed(futures):
            id = futures[future]
            try:
                _ = future.result()
            except Exception as e:
                print(f"ID {id} generated an exception: {e}")



if __name__ == "__main__":
    import time
    start = time.time()
    # EDITH PLEASE EDIT THE PATH BELOW
    BASE_ROADS = Path(r"C:\Users\dkerr\Documents\GISRede\OXFORD_UNI_WORK\NET_FRICTION_DEBUGGING\data").resolve()
    roads = BASE_ROADS.joinpath("roads", "gis_osm_roads_free_1.shp")
    subset_fields = ["osm_id", "fclass"]
    subset_categories = ["motorway", "trunk", "primary", "secondary", "tertiary"]
    crs = 6383
    admin_levels = [1, 2]
    buffer_distance = 1000
    # EDITH PLEASE EDIT THE DATES BELOW - If only one range (i.e. 2024-02-01 - 2024-02-29), you only need one element in each list
    # for example start_dates = ["2024-02-01"] and end_dates = ["2024-02-29"]
    start_dates = ["2024-02-01", "2024-03-01"]
    end_dates = ["2024-02-29", "2024-03-31"]
    for start_date, end_date in zip(start_dates, end_dates):
        main(roads, subset_fields, subset_categories, crs, admin_levels, buffer_distance, start_date, end_date)
    end = time.time()
    print(f"Time taken: {(end - start) / 60} minutes")
