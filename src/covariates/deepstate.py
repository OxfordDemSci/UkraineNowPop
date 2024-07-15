#!/usr/bin/env python
# coding: utf-8

# # deepstatemap.live territorial gains scraping
# 
# Extract polygons of occupied ukraine
# 

import datetime
import json
import os
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

USER_AGENT = 'deepstate-scraper/0.0.1'
TIMEOUT = 60
MAX_RETRIES = 10
COOLDOWN = 2  # 5 seconds between retries/after each download
PARALLEL_DOWNLOADS = 10
PARALLEL_PROCESSES = 16

HISTORY_URL = 'https://deepstatemap.live/api/history/'
ITEMS_FOLDER = 'data/control/'

# Create directories
os.makedirs(ITEMS_FOLDER, exist_ok=True)
os.makedirs(ITEMS_FOLDER+'raw', exist_ok=True)

def scrape_json(url: str):
    headers = {
        'Content-Type': 'application/json',
        'User-Agent': USER_AGENT,
    }
    req = request.Request(
        url=url,
        headers=headers,
        method='GET',
    )

    def fetch():
        return json.loads(
            request.urlopen(
                req,
                timeout=TIMEOUT,
            ).read().decode('utf-8')
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

def save_to_file(items, filename):
    # print(f"Saving results to {filename}")
    with open(filename, 'w', encoding='utf-8') as f:
        json.dump(items, f, ensure_ascii=False, indent=2)


history = scrape_json(HISTORY_URL)


# Ensure 'items' dir exists
pathlib.Path(ITEMS_FOLDER).mkdir(parents=True, exist_ok=True)
def scrape_items(items):

    ids = list(enumerate(filter(None, (str(p.get('id')) for p in items))))

    def scrape_content(args):
        idx, id = args
        url = HISTORY_URL + '/' + id + '/geojson'
        # https://stackoverflow.com/a/5291396/2193463
        print(f"(Downloading {idx}/{len(ids)}", end='\r')
        entry = scrape_json(url)
        save_to_file(entry, ITEMS_FOLDER + '/' + id + '.json')
        entry['id'] = id
        # Artificial throttling, otherwise we'll get an HTTP 429
        time.sleep(COOLDOWN)
        return entry

    print(f"Scraping all {len(ids)} items...")

    # https://www.markhneedham.com/blog/2018/07/15/python-parallel-download-files-requests/
    def dispatch(ids):
        return list(ThreadPool(
            PARALLEL_DOWNLOADS).imap_unordered(scrape_content, ids))

    results = dispatch(ids)
    return results

# XXX Beware, this will take some time as it has to download 530+ files
scrape_items(history)

files = os.listdir(ITEMS_FOLDER+'/raw')

# check manually the labels to define the regular expression
def read_property(filename):
    with open(os.path.join(ITEMS_FOLDER, 'raw', filename), encoding='utf-8') as f:
        data = json.load(f)
    properties = []
    for feature in data['features']:
        if re.search('Polygon', feature['geometry']['type'].strip()):
            properties.append(feature['properties']['name'])

    return properties


properties = [read_property(i) for i in files]
properties = [item for sublist in properties for item in sublist]

properties_table = pd.Series(properties).value_counts()
properties_table = pd.DataFrame({'text': properties_table.index, 'Frequency': properties_table.values})

properties_table['matched'] = properties_table['text'].str.contains('уп|ОРДЛО|Крим', case=False)

def extract_occupied(features):
    occupied = []
    for feature in features:
        if re.search('уп|ОРДЛО|Крим', feature['properties']['name'].strip(), flags=re.IGNORECASE):
            occupied.append(feature)
    geom = [shape(i['geometry']) for i in occupied]

    gdf = gpd.GeoDataFrame({'geometry': geom}, crs='EPSG:4326')

    gdf['property'] = [i['properties']['name'].strip() for i in occupied]
    gdf['geometry'] = gdf.geometry.buffer(0)

    joined = gdf.dissolve(by=None)
    return joined
def process_item(args):
    idx, filename = args
    print(f"(Processing {idx}", end='\r')
    with open(os.path.join(ITEMS_FOLDER, 'raw', filename), encoding='utf-8') as f:
        data = json.load(f)
    id_ = filename.split('.json')[0]
    date = datetime.datetime.fromtimestamp(int(id_)).strftime("%Y-%m-%d")
    # https://gis.stackexchange.com/questions/329349/calculating-the-area-by-square-feet-with-geopandas
    polygons = extract_occupied(data['features'])
    polygons['date'] =date

    polygons.to_file(os.path.join(ITEMS_FOLDER, 'occupied_'+ date+ '.gpkg'), driver="GPKG")

    return [id_, polygons]


processed = []


def dispatch(items):
    print(f"Processing all {len(items)} items...")
    return list(ThreadPool(
        PARALLEL_PROCESSES).imap_unordered(process_item, enumerate(items)))

# XXX This is slow, beware
# Don't worry about weird text output below ("Processing ...")
processed = dispatch(files)
