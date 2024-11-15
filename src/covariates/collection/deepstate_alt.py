#!/usr/bin/env python
# coding: utf-8

# # deepstatemap.live territorial gains scraping
#
# Extract polygons of occupied ukraine
#

import datetime
import json
import pathlib
import socket
import time
import re

from multiprocessing.pool import ThreadPool
from urllib import request
from urllib.error import HTTPError, URLError

import geopandas as gpd
import pandas as pd
from shapely.geometry import shape
from py_helpers.utils import *

import matplotlib.pyplot as plt

# scraper global variables
USER_AGENT = "deepstate-scraper/0.0.1"
TIMEOUT = 60
MAX_RETRIES = 10
COOLDOWN = 2  # 5 seconds between retries/after each download
PARALLEL_DOWNLOADS = 10
PARALLEL_PROCESSES = 16
HISTORY_URL = "https://deepstatemap.live/api/history/"
ITEMS_FOLDER = out_dir / "covariates" / "raw" / "deepstate"


def scrape_json(url: str):
    headers = {
        "Content-Type": "application/json",
        "User-Agent": USER_AGENT,
    }
    req = request.Request(
        url=url,
        headers=headers,
        method="GET",
    )

    def fetch():
        return json.loads(
            request.urlopen(
                req,
                timeout=TIMEOUT,
            )
            .read()
            .decode("utf-8")
        )

    r = 0
    while r < MAX_RETRIES:
        try:
            return fetch()
        except HTTPError as e:
            r += 1
            print(f"HTTPError, retrying. e={e}")
            time.sleep(60)
            continue
        except URLError as e:
            r += 1
            if isinstance(e.reason, socket.timeout):
                print("Socket timeout, retrying")
                continue
            print(f"URLError, retrying. e={e}")
            time.sleep(10)
    raise HTTPError


def scrape_items(items):

    ids = list(enumerate(filter(None, (str(p.get("id")) for p in items))))

    def scrape_content(args):
        idx, id = args
        url = HISTORY_URL + "/" + id + "/geojson"
        # https://stackoverflow.com/a/5291396/2193463
        print(f"(Downloading {idx}/{len(ids)}", end="\r")
        entry = scrape_json(url)
        save_to_file(entry, ITEMS_FOLDER.joinpath("json").joinpath(str(id) + ".json"))
        entry["id"] = id
        # Artificial throttling, otherwise we'll get an HTTP 429
        time.sleep(COOLDOWN)
        return entry

    print(f"Scraping all {len(ids)} items...")

    # https://www.markhneedham.com/blog/2018/07/15/python-parallel-download-files-requests/
    def dispatch(ids):
        return list(ThreadPool(PARALLEL_DOWNLOADS).imap_unordered(scrape_content, ids))

    results = dispatch(ids)
    return results


def save_to_file(items, filename):
    # print(f"Saving results to {filename}")
    with open(filename, "w", encoding="utf-8") as f:
        json.dump(items, f, ensure_ascii=False, indent=2)


def read_property(filename):
    """check manually the labels to define the regular expression"""

    with open(os.path.join(ITEMS_FOLDER, "json", filename), encoding="utf-8") as f:
        data = json.load(f)
    properties = []
    for feature in data["features"]:
        if re.search("Polygon", feature["geometry"]["type"].strip()):
            properties.append(feature["properties"]["name"])

    return properties


def extract_occupied(features):
    occupied = []
    for feature in features:
        if re.search(
            "уп|ОРДЛО|Крим", feature["properties"]["name"].strip(), flags=re.IGNORECASE
        ):
            occupied.append(feature)
    geom = [shape(i["geometry"]) for i in occupied]

    gdf = gpd.GeoDataFrame({"geometry": geom}, crs="EPSG:4326")

    gdf["property"] = [i["properties"]["name"].strip() for i in occupied]
    gdf["geometry"] = gdf.geometry.buffer(0)

    joined = gdf.dissolve(by=None)
    return joined


def process_item(args):
    idx, filename = args
    print(f"(Processing {idx}", end="\r")
    with open(os.path.join(ITEMS_FOLDER, "json", filename), encoding="utf-8") as f:
        data = json.load(f)
    id_ = filename.split(".json")[0]
    date = datetime.datetime.fromtimestamp(int(id_)).strftime("%Y-%m-%d")
    # https://gis.stackexchange.com/questions/329349/calculating-the-area-by-square-feet-with-geopandas
    polygons = extract_occupied(data["features"])
    polygons["date"] = date

    polygons.to_file(
        os.path.join(ITEMS_FOLDER, "gpkg", "occupied_" + date + ".gpkg"), driver="GPKG"
    )

    return [id_, polygons]


def dispatch(items):
    print(f"Processing all {len(items)} items...")
    return list(
        ThreadPool(PARALLEL_PROCESSES).imap_unordered(process_item, enumerate(items))
    )


if __name__ == "__main__":

    # settings
    country = "ua"

    # Create directories
    os.makedirs(ITEMS_FOLDER / "json", exist_ok=True)
    os.makedirs(ITEMS_FOLDER / "gpkg", exist_ok=True)
    os.makedirs(out_dir / "covariates" / "interim", exist_ok=True)

    # ---- load data ----#

    # cod-ab
    boundaries_oblast_path = (
        repo_dir
        / "data"
        / "cod-ab"
        / "ukr_admbnda_sspe_20230201_SHP"
        / "ukr_admbnda_adm1_sspe_20230201.shp"
    )
    boundaries_oblast = gpd.read_file(boundaries_oblast_path)
    boundaries_oblast = boundaries_oblast[["ADM1_PCODE", "geometry"]]

    # master index
    master_index = pd.read_csv(out_dir / (country + "_master_index.csv"))
    master_index = master_index[
        ["ADM1_PCODE", "t", "i", "t_key", "i_key", "t_name", "i_name"]
    ].drop_duplicates()

    # time index
    time_index = pd.read_csv(out_dir / (country + "_time_index.csv"))

    # ---- scraper ----#

    # scrape history
    history = scrape_json(HISTORY_URL)

    # XXX Beware, this will take some time as it has to download 530+ files
    scrape_items(history)

    # process file properties
    files = os.listdir(ITEMS_FOLDER / "json")
    files = [s for s in files if s.endswith(".json")]

    properties = [read_property(i) for i in files]
    properties = [item for sublist in properties for item in sublist]

    properties_table = pd.Series(properties).value_counts()
    properties_table = pd.DataFrame(
        {"text": properties_table.index, "Frequency": properties_table.values}
    )

    properties_table["matched"] = properties_table["text"].str.contains(
        "уп|ОРДЛО|Крим", case=False
    )

    # XXX This is slow, beware
    # Don't worry about weird text output below ("Processing ...")
    processed = dispatch(files)

    # combine occupied territory in one covariate
    occupied_list = os.listdir(ITEMS_FOLDER / "gpkg")
    occupied_list = [s for s in occupied_list if s.endswith(".gpkg")]

    boundaries_oblast_proj = boundaries_oblast.to_crs("EPSG:6381")
    intersections = pd.DataFrame()
    for occupied in occupied_list:
        # occupied = occupied_list[0]
        occupied_gdf = gpd.read_file(ITEMS_FOLDER / 'gpkg' / occupied).to_crs("EPSG:6381")
        intersection = gpd.overlay(
            occupied_gdf[["geometry", "date"]],
            boundaries_oblast_proj,
            how="intersection",
        )
        intersection["occupied"] = intersection.area
        intersection = intersection.rename(columns={"date": "collection_date"})
        intersection.drop(columns=["geometry"], inplace=True)
        intersections = pd.concat([intersections, intersection])

    intersections.groupby(["collection_date", "ADM1_PCODE"]).size()
    intersections = (
        intersections.merge(time_index, how="left", on="collection_date")
        .groupby(["ADM1_PCODE", "t", "t_key", "t_name"])["occupied"]
        .sum()
        .reset_index(name="occupied")
    )

    intersections = intersections.merge(master_index, how="right")

    # Write output
    intersections.to_csv(
        out_dir / "covariates" / "interim" / (country + "_occupied_oblast.csv"),
        index=False,
    )

    # ---- Visualise an example ----#
    if False:
        filtered_data = intersections[intersections["i"].isin([5, 7, 27, 143])]

        plt.figure(figsize=(12, 6))

        for i in filtered_data["i_name"].unique():
            i_data = filtered_data[filtered_data["i_name"] == i]
            i_key = i_data["i"].unique()[0] - 4
            plt.scatter(
                i_data["t"],
                i_data["occupied"],
                label=f"i={i} (t)",
                color=plt.cm.tab20(i_key),
            )

        plt.xlabel("t")
        plt.ylabel("occupied")
        plt.legend()
        plt.show()
